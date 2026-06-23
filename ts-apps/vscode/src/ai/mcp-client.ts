import type { McpProxy } from '../mcp-proxy.js'
import type { AiLogger } from './logger.js'
import type { AiProxyTool } from './ai-proxy-client.js'

/**
 * Thin client over the local MCP proxy at `http://127.0.0.1:{port}/mcp`.
 *
 * The proxy forwards to the user's connected jl4-service on `/.mcp`,
 * so its `tools/list` returns whichever rules the user currently has
 * deployed. We translate those into OpenAI function-tool shape with a
 * `l4-rules__` prefix so they land in the same dispatcher path as the
 * built-in tools (`fs__read_file`, etc.).
 */

/** JSON-RPC id counter, unique per process. */
let rpcIdCounter = 1

/** Shape of a single MCP tool as returned by `tools/list`. */
interface McpTool {
  name: string
  description?: string
  inputSchema?: {
    type?: string
    properties?: Record<string, unknown>
    required?: string[]
    additionalProperties?: boolean
  }
}

interface McpListResult {
  tools?: McpTool[]
}

interface McpCallResult {
  content?: Array<{ type: string; text?: string }>
  isError?: boolean
}

export const MCP_L4_RULES_PREFIX = 'l4-rules__'

export class McpToolClient {
  /** Short in-process cache so a chat turn's tools/list call doesn't
   *  hammer the proxy if several turns are fired in quick succession. */
  private cache: { at: number; tools: AiProxyTool[] } | null = null
  /** sanitized-prefixed-name → original MCP rule name. Populated on
   *  every listTools(); consulted by callTool() to reverse the
   *  `[^a-zA-Z0-9_-]` sanitization we applied for OpenAI compliance. */
  private nameMap = new Map<string, string>()
  private initialized = false

  constructor(
    private readonly proxy: McpProxy,
    private readonly logger: AiLogger
  ) {}

  /**
   * Fetch the current tool list from the local MCP proxy, cached for a
   * few seconds. Returns an empty list if the proxy isn't running or
   * the user isn't connected (the proxy happily answers `tools/list`
   * with `[]` in that state — see its `handleLocally`).
   */
  async listTools(): Promise<AiProxyTool[]> {
    if (this.cache && Date.now() - this.cache.at < 3_000) {
      return this.cache.tools
    }
    const url = this.proxy.getLocalUrl()
    if (!url) return []
    try {
      if (!this.initialized) await this.initialize(url)
      const listed = await this.rpc<McpListResult>(url, 'tools/list', {})
      this.nameMap.clear()
      const tools = (listed?.tools ?? []).map((t) => {
        const tool = this.toAiProxyTool(t)
        this.nameMap.set(tool.function.name, t.name)
        return tool
      })
      this.cache = { at: Date.now(), tools }
      return tools
    } catch (err) {
      this.logger.warn(
        `mcp-client: tools/list failed: ${err instanceof Error ? err.message : String(err)}`
      )
      return []
    }
  }

  /**
   * Forward a `l4-rules__<name>` tool invocation to the MCP proxy as
   * `tools/call`. Returns a plain string the chat-service can ship
   * back to the LLM as the tool-result content.
   */
  async callTool(prefixedName: string, argsJson: string): Promise<string> {
    if (!prefixedName.startsWith(MCP_L4_RULES_PREFIX)) {
      throw new Error(
        `mcp-client: tool name must start with ${MCP_L4_RULES_PREFIX}: ${prefixedName}`
      )
    }
    // Prefer the reverse-mapped original name (undoes the sanitization
    // applied when we built the OpenAI tools list). Fall back to the
    // trimmed suffix if we've never listed or the map lost the entry —
    // covers the case where a rule name was already OpenAI-valid.
    const toolName =
      this.nameMap.get(prefixedName) ??
      prefixedName.slice(MCP_L4_RULES_PREFIX.length)
    const url = this.proxy.getLocalUrl()
    if (!url) {
      throw new Error(
        'L4 MCP proxy is not running — cannot invoke deployed-rule tools.'
      )
    }
    let parsed: Record<string, unknown>
    try {
      parsed = argsJson?.trim() ? JSON.parse(argsJson) : {}
    } catch (err) {
      throw new Error(
        `mcp-client: invalid JSON arguments for ${prefixedName}: ${err instanceof Error ? err.message : String(err)}`
      )
    }
    if (!this.initialized) await this.initialize(url)
    const res = await this.rpc<McpCallResult>(url, 'tools/call', {
      name: toolName,
      arguments: parsed,
    })
    if (res?.isError) {
      const text = (res.content ?? [])
        .map((c) => (c.type === 'text' && c.text ? c.text : ''))
        .filter(Boolean)
        .join('\n')
      throw new Error(text || `${prefixedName} failed`)
    }
    const text = (res?.content ?? [])
      .map((c) => (c.type === 'text' && c.text ? c.text : JSON.stringify(c)))
      .join('\n')
    return text || JSON.stringify(res ?? {})
  }

  /** Force a cache refresh on the next listTools() call. */
  invalidate(): void {
    this.cache = null
  }

  private async initialize(url: string): Promise<void> {
    try {
      await this.rpc(url, 'initialize', {
        protocolVersion: '2025-03-26',
        capabilities: { tools: {} },
        clientInfo: { name: 'legalese-ai-chat', version: '1.4.0' },
      })
      // Fire-and-forget the initialized notification.
      void this.rpc(url, 'notifications/initialized', {}).catch(() => undefined)
      this.initialized = true
    } catch (err) {
      this.logger.warn(
        `mcp-client: initialize failed: ${err instanceof Error ? err.message : String(err)}`
      )
      throw err
    }
  }

  private async rpc<T>(
    url: string,
    method: string,
    params: Record<string, unknown>
  ): Promise<T | null> {
    const id = rpcIdCounter++
    const res = await fetch(url, {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ jsonrpc: '2.0', id, method, params }),
    })
    if (!res.ok) {
      throw new Error(`${method} returned HTTP ${res.status}`)
    }
    const text = await res.text()
    if (!text) return null
    const payload = JSON.parse(text) as {
      result?: T
      error?: { code?: number; message?: string }
    }
    if (payload.error) {
      throw new Error(payload.error.message ?? `${method} failed`)
    }
    return payload.result ?? null
  }

  private toAiProxyTool(t: McpTool): AiProxyTool {
    // OpenAI function-tool schema wants `parameters` (not `inputSchema`).
    // MCP gives us a nearly-matching JSON schema; we shape it so the
    // provider accepts it even when a rule ships no schema at all.
    const parameters = t.inputSchema ?? { type: 'object', properties: {} }
    // OpenAI's function-tool schema requires `parameters.type === 'object'`;
    // some MCP tools omit that, which the provider rejects.
    const shaped = {
      type: 'object' as const,
      properties:
        (parameters as { properties?: Record<string, unknown> }).properties ??
        {},
      ...(Array.isArray((parameters as { required?: string[] }).required)
        ? { required: (parameters as { required: string[] }).required }
        : {}),
      additionalProperties:
        (parameters as { additionalProperties?: boolean })
          .additionalProperties ?? false,
    }
    return {
      type: 'function',
      function: {
        name: `${MCP_L4_RULES_PREFIX}${sanitizeToolName(t.name)}`,
        description:
          t.description ??
          `Deployed L4 rule: ${t.name}. Inputs come from the rule's function signature.`,
        parameters: shaped,
      },
    }
  }
}

/** OpenAI function names allow `[a-zA-Z0-9_-]{1,64}`; MCP rule names can
 *  contain spaces / dots / slashes. Map those to underscore so the
 *  combined name still round-trips safely. We key the dispatcher on
 *  the final prefixed name so the sanitization has to be deterministic. */
function sanitizeToolName(s: string): string {
  return s.replace(/[^a-zA-Z0-9_-]/g, '_').slice(0, 48)
}
