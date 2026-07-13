import * as fs from 'node:fs'
import * as path from 'node:path'
import * as vscode from 'vscode'
import type { AiLogger } from './logger.js'
import type { AiProxyTool } from './ai-proxy-client.js'

/**
 * MCP servers for the Legalese AI sidebar.
 *
 * The extension maintains its OWN connections to the http/sse servers
 * configured in VS Code's `mcp.json` (user + workspace level), instead
 * of piggybacking on VS Code's MCP lifecycle:
 *
 *  - Enabled servers auto-connect as soon as a usable Legalese AI
 *    session exists (login or extension boot) — no "start it in
 *    VS Code first" dance, and no dependence on VS Code's internal
 *    server naming (which follows the server's self-reported name and
 *    caused duplicate rows when it differed from the config key).
 *  - The per-server toggle starts/stops our connection; a per-tool
 *    deny-list refines which of a connected server's tools are
 *    advertised to the model.
 *  - Tool calls go straight to the server over JSON-RPC with the
 *    configured headers (e.g. an `Authorization: Bearer …` the user
 *    provided when adding the server).
 *
 * Servers we cannot connect to ourselves (stdio commands, or servers
 * another extension registered with VS Code) still surface through
 * `vscode.lm.tools` and execute via `vscode.lm.invokeTool`; live tool
 * groups that duplicate one of our direct connections are folded away.
 *
 * The built-in "L4 Rules" proxy is excluded everywhere — its tools
 * already reach the model via the `l4-rules__` path and it has its own
 * permission row.
 */

/** Wire-name prefix for MCP tools advertised to the ai-proxy,
 *  mirroring `l4-rules__` for the built-in server. Prefix-routing in
 *  the dispatcher and the permission gate key off it. */
export const VSCODE_MCP_PREFIX = 'vsmcp__'

/** Master switch: expose the user's MCP servers to Legalese AI at all.
 *  Shown as an "All MCP servers" toggle atop the settings list when
 *  more than one server exists. */
const MCP_ENABLED_SETTING = 'legaleseAi.mcp.enabled'

/** Servers the user switched off for Legalese AI (deny-list so new
 *  servers default to available; calls stay permission-gated). */
const DISABLED_SERVERS_SETTING = 'legaleseAi.mcp.disabledServers'

/** Individual tools the user switched off, as `serverId::toolName`. */
const DISABLED_TOOLS_SETTING = 'legaleseAi.mcp.disabledTools'

/** mcp.json keys owned by our own local proxy (see McpProxy). */
const BUILTIN_SERVER_KEYS = new Set(['L4 Rules', 'l4-rules', 'l4-tools'])

/**
 * Names the built-in L4 server may appear under in `vscode.lm.tools`
 * prefixes. VS Code derives the tool prefix from the server's
 * SELF-REPORTED name (`serverInfo.name` from `initialize`), not from
 * the mcp.json key — the jl4-service reports "L4 Tools", so its tools
 * arrive as `mcp_l4_tools_*` even though the config key is "L4 Rules".
 * The bare `l4` alias deliberately also swallows any other
 * unconfigured l4-prefixed group rather than risk advertising our own
 * deployed rules to the model twice.
 */
const BUILTIN_NAME_ALIASES = [
  'L4 Rules',
  'L4 Tools',
  'l4-rules',
  'l4-tools',
  'l4',
]

export type McpServerStatus =
  | 'connected'
  | 'connecting'
  | 'error'
  | 'stopped'
  /** Not connectable by us (stdio command / other-extension server) —
   *  its tools come and go with VS Code's own MCP lifecycle. */
  | 'external'

export interface VsCodeMcpToolInfo {
  name: string
  description?: string
  enabled: boolean
}

export interface VsCodeMcpServerInfo {
  id: string
  name: string
  source: 'user' | 'workspace' | 'discovered'
  transport?: string
  /** URL or command line — whatever identifies the server to a human. */
  detail?: string
  enabled: boolean
  status: McpServerStatus
  /** Human-readable connection failure, when status === 'error'. */
  error?: string
  tools: VsCodeMcpToolInfo[]
}

export interface AddServerInput {
  name: string
  transport: 'http' | 'sse' | 'stdio'
  url?: string
  /** For stdio servers: full command line, split on whitespace. */
  command?: string
  /** Optional bearer token for http/sse servers — stored as an
   *  `Authorization: Bearer …` header on the mcp.json entry. */
  bearerToken?: string
}

export type McpServerAction = 'start' | 'stop' | 'refresh' | 'remove'

interface McpJsonServerEntry {
  type?: string
  url?: string
  command?: string
  args?: string[]
  headers?: Record<string, string>
  [key: string]: unknown
}

interface ConfiguredServer {
  key: string
  source: 'user' | 'workspace' | 'builtin'
  entry: McpJsonServerEntry
}

interface McpToolDef {
  name: string
  description?: string
  inputSchema?: Record<string, unknown>
}

interface Connection {
  status: 'connected' | 'connecting' | 'error' | 'stopped'
  error?: string
  tools: McpToolDef[]
  /** `Mcp-Session-Id` echoed back per the streamable-HTTP spec. */
  sessionId?: string
}

/** JSON-RPC id counter, unique per process. */
let rpcIdCounter = 1

export class VsCodeMcpTools {
  /** Direct-connection state per configured server key. */
  private connections = new Map<string, Connection>()
  /** wire name → how to execute it. Rebuilt on every listTools(). */
  private wireMap = new Map<
    string,
    | { via: 'direct'; serverId: string; toolName: string }
    | { via: 'lm'; lmName: string }
  >()

  constructor(
    /** VS Code per-user data dir containing `mcp.json` (same value
     *  McpProxy uses). Undefined when it couldn't be derived. */
    private readonly userDataPath: string | undefined,
    private readonly logger: AiLogger,
    /** Gate for auto-connecting: true when the user has a usable
     *  Legalese AI session (Cloud login or API key). */
    private readonly isAiUsable: () => boolean
  ) {}

  // ── Lifecycle ─────────────────────────────────────────────────────

  /**
   * Connect every enabled http/sse server that isn't already connected.
   * Called on activation and whenever auth state changes; no-op until
   * a usable Legalese AI session exists.
   */
  /** Master switch state. */
  allEnabled(): boolean {
    return (
      vscode.workspace.getConfiguration().get<boolean>(MCP_ENABLED_SETTING) !==
      false
    )
  }

  /** Flip the master switch: off drops every connection, on reconnects
   *  the enabled servers (when auth allows). */
  async setAllEnabled(enabled: boolean): Promise<void> {
    await vscode.workspace
      .getConfiguration()
      .update(MCP_ENABLED_SETTING, enabled, vscode.ConfigurationTarget.Global)
    if (enabled) {
      void this.autoStart()
    } else {
      for (const id of [...this.connections.keys()]) this.stop(id)
    }
  }

  async autoStart(): Promise<void> {
    if (!this.isAiUsable()) return
    if (!this.allEnabled()) return
    const disabled = getDisabledServers()
    const jobs: Promise<unknown>[] = []
    for (const server of this.readConfiguredServers()) {
      if (server.source === 'builtin') continue
      if (disabled.has(server.key)) continue
      if (!isDirectlyConnectable(server.entry)) continue
      const current = this.connections.get(server.key)
      if (current?.status === 'connected' || current?.status === 'connecting')
        continue
      jobs.push(this.start(server.key))
    }
    await Promise.allSettled(jobs)
  }

  /** Connect one server (initialize + tools/list). */
  async start(id: string): Promise<{ ok: boolean; error?: string }> {
    const server = this.readConfiguredServers().find((s) => s.key === id)
    if (!server) return { ok: false, error: `Unknown server: ${id}` }
    if (!isDirectlyConnectable(server.entry)) {
      return {
        ok: false,
        error: 'Command (stdio) servers are managed by VS Code.',
      }
    }
    const url = server.entry.url as string
    const conn: Connection = { status: 'connecting', tools: [] }
    this.connections.set(id, conn)
    try {
      const init = await this.rpc<{ protocolVersion?: string }>(
        url,
        server.entry.headers,
        conn,
        'initialize',
        {
          protocolVersion: '2025-03-26',
          capabilities: { tools: {} },
          clientInfo: { name: 'legalese-ai-chat', version: '1.5.0' },
        }
      )
      void init
      void this.rpc(
        url,
        server.entry.headers,
        conn,
        'notifications/initialized',
        {}
      ).catch(() => undefined)
      const listed = await this.rpc<{ tools?: McpToolDef[] }>(
        url,
        server.entry.headers,
        conn,
        'tools/list',
        {}
      )
      conn.tools = listed?.tools ?? []
      conn.status = 'connected'
      conn.error = undefined
      this.logger.info(
        `vscode-mcp: connected to "${id}" (${conn.tools.length} tools)`
      )
      return { ok: true }
    } catch (err) {
      const msg = err instanceof Error ? err.message : String(err)
      conn.status = 'error'
      conn.error = msg
      conn.tools = []
      this.logger.warn(`vscode-mcp: connect "${id}" failed: ${msg}`)
      return { ok: false, error: msg }
    }
  }

  stop(id: string): void {
    this.connections.set(id, { status: 'stopped', tools: [] })
  }

  /** Dispatch a three-dot-menu action from the settings UI. */
  async runAction(
    id: string,
    action: McpServerAction
  ): Promise<{ ok: boolean; error?: string }> {
    switch (action) {
      case 'start':
      case 'refresh':
        return this.start(id)
      case 'stop':
        this.stop(id)
        return { ok: true }
      case 'remove':
        return this.removeServer(id)
    }
  }

  // ── Enablement ────────────────────────────────────────────────────

  /** Toggle a server for Legalese AI. Enabling connects it right away
   *  (when auth allows); disabling drops the connection. */
  async setEnabled(id: string, enabled: boolean): Promise<void> {
    const disabled = getDisabledServers()
    if (enabled) disabled.delete(id)
    else disabled.add(id)
    await vscode.workspace
      .getConfiguration()
      .update(
        DISABLED_SERVERS_SETTING,
        [...disabled].sort(),
        vscode.ConfigurationTarget.Global
      )
    if (enabled) {
      if (this.isAiUsable()) void this.start(id)
    } else {
      this.stop(id)
    }
  }

  async setToolEnabled(
    serverId: string,
    toolName: string,
    enabled: boolean
  ): Promise<void> {
    const key = toolKey(serverId, toolName)
    const disabled = getDisabledTools()
    if (enabled) disabled.delete(key)
    else disabled.add(key)
    await vscode.workspace
      .getConfiguration()
      .update(
        DISABLED_TOOLS_SETTING,
        [...disabled].sort(),
        vscode.ConfigurationTarget.Global
      )
  }

  // ── Server list for the settings UI ───────────────────────────────

  listServers(): VsCodeMcpServerInfo[] {
    const configured = this.readConfiguredServers()
    const disabledServers = getDisabledServers()
    const disabledTools = getDisabledTools()
    const lmGroups = this.groupLmToolsByServer(configured)

    const toolInfo = (
      serverId: string,
      name: string,
      description?: string
    ): VsCodeMcpToolInfo => ({
      name,
      description,
      enabled: !disabledTools.has(toolKey(serverId, name)),
    })

    const out: VsCodeMcpServerInfo[] = []
    for (const server of configured) {
      if (server.source === 'builtin') continue
      const direct = isDirectlyConnectable(server.entry)
      const conn = this.connections.get(server.key)
      let status: McpServerStatus
      let tools: VsCodeMcpToolInfo[]
      if (direct) {
        status = conn?.status ?? 'stopped'
        tools = (conn?.tools ?? []).map((t) =>
          toolInfo(server.key, t.name, t.description)
        )
      } else {
        status = 'external'
        tools = (lmGroups.get(server.key) ?? []).map((t) =>
          toolInfo(server.key, t.displayName, t.description)
        )
      }
      out.push({
        id: server.key,
        name: server.key,
        source: server.source,
        transport: server.entry.type,
        detail: describeEntry(server.entry),
        enabled: !disabledServers.has(server.key),
        status,
        error: conn?.error,
        tools,
      })
    }

    // Live lm.tools groups with no config entry (servers registered by
    // other extensions). Skip groups that duplicate one of our direct
    // connections — VS Code names servers after their self-reported
    // name, which need not match the mcp.json key, so key matching
    // alone can't catch these.
    for (const [id, tools] of lmGroups) {
      if (configured.some((s) => s.key === id)) continue
      if (this.isDuplicateOfDirectConnection(tools)) continue
      out.push({
        id,
        name: id.replace(/^discovered:/, ''),
        source: 'discovered',
        enabled: !disabledServers.has(id),
        status: 'external',
        tools: tools.map((t) => toolInfo(id, t.displayName, t.description)),
      })
    }
    return out
  }

  // ── Tool funnel for the chat loop ─────────────────────────────────

  /**
   * Advertise the enabled servers' enabled tools in OpenAI
   * function-tool shape. Direct connections win; lm-sourced tools are
   * added only for servers we can't connect to ourselves, with
   * duplicates of direct connections folded away.
   */
  listTools(): AiProxyTool[] {
    this.wireMap.clear()
    if (!this.allEnabled()) return []
    const configured = this.readConfiguredServers()
    const disabledServers = getDisabledServers()
    const disabledTools = getDisabledTools()
    const out: AiProxyTool[] = []

    // Direct connections first.
    for (const [serverId, conn] of this.connections) {
      if (conn.status !== 'connected') continue
      if (disabledServers.has(serverId)) continue
      for (const t of conn.tools) {
        if (disabledTools.has(toolKey(serverId, t.name))) continue
        const wireName = directWireName(serverId, t.name)
        if (this.wireMap.has(wireName)) continue
        this.wireMap.set(wireName, {
          via: 'direct',
          serverId,
          toolName: t.name,
        })
        out.push({
          type: 'function',
          function: {
            name: wireName,
            description: t.description || `MCP tool ${t.name} (${serverId})`,
            parameters: shapeParameters(t.inputSchema),
          },
        })
      }
    }

    // lm.tools for external servers (stdio / other extensions).
    const lmGroups = this.groupLmToolsByServer(configured)
    for (const [groupId, tools] of lmGroups) {
      if (disabledServers.has(groupId)) continue
      // Direct connections already cover this server.
      const cfg = configured.find((s) => s.key === groupId)
      if (cfg && isDirectlyConnectable(cfg.entry)) continue
      if (this.isDuplicateOfDirectConnection(tools)) continue
      for (const t of tools) {
        if (disabledTools.has(toolKey(groupId, t.displayName))) continue
        const wireName = lmWireName(t.lmName)
        if (this.wireMap.has(wireName)) continue
        this.wireMap.set(wireName, { via: 'lm', lmName: t.lmName })
        out.push({
          type: 'function',
          function: {
            name: wireName,
            description: t.description || `MCP tool ${t.lmName}`,
            parameters: shapeParameters(t.inputSchema),
          },
        })
      }
    }
    return out
  }

  /** True when the lm tool belongs to a server the user disabled (or
   *  to the built-in L4 server). Used by the @legalese chat participant
   *  so the sidebar toggles apply there too. */
  isLmToolDisabled(lmToolName: string): boolean {
    if (!lmToolName.startsWith('mcp_')) return false
    const owner = serverForLmTool(lmToolName, this.readConfiguredServers())
    if (owner.builtin) return false // participant may still use it via VS Code
    if (!this.allEnabled()) return true
    return getDisabledServers().has(owner.id)
  }

  /** Execute a `vsmcp__…` call. */
  async callTool(wireName: string, argsJson: string): Promise<string> {
    if (!wireName.startsWith(VSCODE_MCP_PREFIX)) {
      throw new Error(
        `vscode-mcp: tool name must start with ${VSCODE_MCP_PREFIX}: ${wireName}`
      )
    }
    let target = this.wireMap.get(wireName)
    if (!target) {
      // Map lost (e.g. extension host reload mid-conversation) —
      // rebuild from live state before giving up.
      this.listTools()
      target = this.wireMap.get(wireName)
    }
    if (!target) {
      throw new Error(
        `MCP tool ${wireName} is not available — its server may be stopped or disabled.`
      )
    }
    let parsed: unknown
    try {
      parsed = argsJson?.trim() ? JSON.parse(argsJson) : {}
    } catch (err) {
      throw new Error(
        `vscode-mcp: invalid JSON arguments for ${wireName}: ${err instanceof Error ? err.message : String(err)}`
      )
    }
    const args: Record<string, unknown> =
      parsed && typeof parsed === 'object'
        ? (parsed as Record<string, unknown>)
        : { value: parsed }

    if (target.via === 'lm') {
      const result = await vscode.lm.invokeTool(target.lmName, {
        input: args,
        toolInvocationToken: undefined,
      })
      const parts: string[] = []
      for (const p of result.content) {
        if (p instanceof vscode.LanguageModelTextPart) parts.push(p.value)
      }
      return parts.join('\n') || 'ok'
    }

    const server = this.readConfiguredServers().find(
      (s) => s.key === (target as { serverId: string }).serverId
    )
    const conn = this.connections.get(target.serverId)
    if (!server || !server.entry.url || conn?.status !== 'connected') {
      throw new Error(
        `MCP server ${target.serverId} is not connected — toggle it on in the Legalese AI settings.`
      )
    }
    const res = await this.rpc<{
      content?: Array<{ type: string; text?: string }>
      isError?: boolean
    }>(server.entry.url, server.entry.headers, conn, 'tools/call', {
      name: target.toolName,
      arguments: args,
    })
    const text = (res?.content ?? [])
      .map((c) => (c.type === 'text' && c.text ? c.text : JSON.stringify(c)))
      .join('\n')
    if (res?.isError) {
      throw new Error(text || `${wireName} failed`)
    }
    return text || JSON.stringify(res ?? {})
  }

  // ── mcp.json management ───────────────────────────────────────────

  /**
   * Add a server to the user-level `mcp.json` and (for http/sse)
   * connect it immediately. VS Code picks the entry up from the file
   * for its own features independently.
   */
  async addServer(
    input: AddServerInput
  ): Promise<{ ok: boolean; error?: string }> {
    const name = input.name.trim()
    if (!name) return { ok: false, error: 'Server name is required.' }
    if (BUILTIN_SERVER_KEYS.has(name)) {
      return {
        ok: false,
        error: `"${name}" is reserved for the built-in L4 server.`,
      }
    }
    let entry: McpJsonServerEntry
    if (input.transport === 'stdio') {
      const parts = (input.command ?? '').trim().split(/\s+/).filter(Boolean)
      if (parts.length === 0) {
        return { ok: false, error: 'A command is required for a stdio server.' }
      }
      entry = { type: 'stdio', command: parts[0], args: parts.slice(1) }
    } else {
      const url = (input.url ?? '').trim()
      if (!/^https?:\/\//.test(url)) {
        return { ok: false, error: 'A valid http(s) URL is required.' }
      }
      entry = { type: input.transport, url }
      const token = (input.bearerToken ?? '').trim()
      if (token) {
        entry.headers = { Authorization: `Bearer ${token}` }
      }
    }

    const written = this.mutateUserMcpJson((servers) => {
      if (servers[name]) {
        return `A server named "${name}" already exists.`
      }
      servers[name] = entry
      return null
    })
    if (!written.ok) return written

    this.logger.info(`vscode-mcp: added server "${name}"`)
    if (isDirectlyConnectable(entry) && this.isAiUsable()) {
      // Connect eagerly so the settings row shows real tools right away.
      return this.start(name)
    }
    return { ok: true }
  }

  private removeServer(id: string): { ok: boolean; error?: string } {
    const server = this.readConfiguredServers().find((s) => s.key === id)
    if (!server) return { ok: false, error: `Unknown server: ${id}` }
    if (server.source !== 'user') {
      return {
        ok: false,
        error:
          server.source === 'builtin'
            ? 'The built-in L4 server cannot be removed.'
            : 'Workspace servers live in .vscode/mcp.json — edit that file directly.',
      }
    }
    const res = this.mutateUserMcpJson((servers) => {
      if (!servers[id]) return `"${id}" is not in the user mcp.json.`
      delete servers[id]
      return null
    })
    if (res.ok) {
      this.connections.delete(id)
      this.logger.info(`vscode-mcp: removed server "${id}"`)
    }
    return res
  }

  /** Read-modify-write the user mcp.json's `servers` object. The
   *  mutator returns an error string to abort, or null to commit. */
  private mutateUserMcpJson(
    mutate: (servers: Record<string, McpJsonServerEntry>) => string | null
  ): { ok: boolean; error?: string } {
    if (!this.userDataPath) {
      return { ok: false, error: 'Could not locate the VS Code user mcp.json.' }
    }
    const mcpJsonPath = path.join(this.userDataPath, 'mcp.json')
    let config: { servers?: Record<string, McpJsonServerEntry> } = {}
    try {
      const raw = fs.readFileSync(mcpJsonPath, 'utf-8')
      if (raw.trim()) config = JSON.parse(raw)
    } catch (err) {
      if ((err as NodeJS.ErrnoException).code !== 'ENOENT') {
        // Same posture as McpProxy.updateVSCodeMcpJson: never risk
        // clobbering a JSONC file we can't faithfully parse.
        return {
          ok: false,
          error: `Could not parse ${mcpJsonPath} — if it contains comments, edit it manually.`,
        }
      }
    }
    if (!config || typeof config !== 'object') config = {}
    if (!config.servers || typeof config.servers !== 'object')
      config.servers = {}
    const abort = mutate(config.servers)
    if (abort) return { ok: false, error: abort }
    try {
      fs.mkdirSync(path.dirname(mcpJsonPath), { recursive: true })
      fs.writeFileSync(mcpJsonPath, JSON.stringify(config, null, 2) + '\n')
      return { ok: true }
    } catch (err) {
      return {
        ok: false,
        error: `Failed to write mcp.json: ${err instanceof Error ? err.message : String(err)}`,
      }
    }
  }

  private readConfiguredServers(): ConfiguredServer[] {
    const out: ConfiguredServer[] = []
    const seen = new Set<string>()
    const addFrom = (filePath: string, source: 'user' | 'workspace'): void => {
      const servers = readMcpJsonServers(filePath, this.logger)
      for (const [key, entry] of Object.entries(servers)) {
        if (seen.has(key)) continue
        seen.add(key)
        out.push({
          key,
          source: BUILTIN_SERVER_KEYS.has(key) ? 'builtin' : source,
          entry,
        })
      }
    }
    if (this.userDataPath) {
      addFrom(path.join(this.userDataPath, 'mcp.json'), 'user')
    }
    for (const folder of vscode.workspace.workspaceFolders ?? []) {
      addFrom(path.join(folder.uri.fsPath, '.vscode', 'mcp.json'), 'workspace')
    }
    return out
  }

  // ── lm.tools handling (external servers only) ─────────────────────

  /** Group live `mcp_*` lm tools by owning server id. Built-in tools
   *  are excluded — they never surface as a listable server. */
  private groupLmToolsByServer(configured: ConfiguredServer[]): Map<
    string,
    Array<{
      lmName: string
      displayName: string
      description?: string
      inputSchema?: Record<string, unknown>
    }>
  > {
    const groups = new Map<
      string,
      Array<{
        lmName: string
        displayName: string
        description?: string
        inputSchema?: Record<string, unknown>
      }>
    >()
    for (const t of vscode.lm.tools) {
      if (!t.name.startsWith('mcp_')) continue
      const owner = serverForLmTool(t.name, configured)
      if (owner.builtin) continue
      const list = groups.get(owner.id) ?? []
      list.push({
        lmName: t.name,
        displayName: displayToolName(t.name, owner.id),
        description: t.description,
        inputSchema: t.inputSchema as Record<string, unknown> | undefined,
      })
      groups.set(owner.id, list)
    }
    return groups
  }

  /**
   * A live lm group is a duplicate of one of our direct connections
   * when every one of its tools carries a connected server's tool name
   * as a suffix. Content-based, so it works no matter what name VS Code
   * generated for the server.
   */
  private isDuplicateOfDirectConnection(
    tools: Array<{ lmName: string }>
  ): boolean {
    if (tools.length === 0) return false
    for (const conn of this.connections.values()) {
      if (conn.status !== 'connected' || conn.tools.length === 0) continue
      const names = conn.tools.map((t) => sanitizeToolName(t.name))
      const allMatch = tools.every((t) => {
        const rest = t.lmName.slice('mcp_'.length)
        return names.some((n) => rest === n || rest.endsWith(`_${n}`))
      })
      if (allMatch) return true
    }
    return false
  }

  // ── JSON-RPC over (streamable) HTTP ───────────────────────────────

  private async rpc<T>(
    url: string,
    headers: Record<string, string> | undefined,
    conn: Connection,
    method: string,
    params: Record<string, unknown>
  ): Promise<T | null> {
    const id = rpcIdCounter++
    const res = await fetch(url, {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
        Accept: 'application/json, text/event-stream',
        ...(conn.sessionId ? { 'Mcp-Session-Id': conn.sessionId } : {}),
        ...(headers ?? {}),
      },
      body: JSON.stringify({ jsonrpc: '2.0', id, method, params }),
      signal: AbortSignal.timeout(20_000),
    })
    const sessionId = res.headers.get('Mcp-Session-Id')
    if (sessionId) conn.sessionId = sessionId
    if (!res.ok) {
      const preview = await res.text().catch(() => '')
      throw new Error(
        `${method} returned HTTP ${res.status}${preview ? `: ${preview.slice(0, 200)}` : ''}`
      )
    }
    const contentType = res.headers.get('Content-Type') ?? ''
    const text = await res.text()
    if (!text) return null
    const raw = contentType.includes('text/event-stream')
      ? extractSseJson(text, id)
      : text
    if (!raw) return null
    const payload = JSON.parse(raw) as {
      result?: T
      error?: { code?: number; message?: string }
    }
    if (payload.error) {
      throw new Error(payload.error.message ?? `${method} failed`)
    }
    return payload.result ?? null
  }
}

// ── helpers ───────────────────────────────────────────────────────────

function isDirectlyConnectable(entry: McpJsonServerEntry): boolean {
  return (
    typeof entry.url === 'string' &&
    (entry.type === 'http' || entry.type === 'sse' || entry.type === undefined)
  )
}

function toolKey(serverId: string, toolName: string): string {
  return `${serverId}::${toolName}`
}

function getDisabledServers(): Set<string> {
  return readStringSetSetting(DISABLED_SERVERS_SETTING)
}

function getDisabledTools(): Set<string> {
  return readStringSetSetting(DISABLED_TOOLS_SETTING)
}

function readStringSetSetting(setting: string): Set<string> {
  const raw = vscode.workspace.getConfiguration().get<string[]>(setting)
  return new Set(
    Array.isArray(raw) ? raw.filter((s) => typeof s === 'string') : []
  )
}

function readMcpJsonServers(
  filePath: string,
  logger: AiLogger
): Record<string, McpJsonServerEntry> {
  let raw: string
  try {
    raw = fs.readFileSync(filePath, 'utf-8')
  } catch {
    return {}
  }
  if (!raw.trim()) return {}
  try {
    const parsed = JSON.parse(raw) as {
      servers?: Record<string, McpJsonServerEntry>
    }
    if (parsed && typeof parsed === 'object' && parsed.servers) {
      return parsed.servers
    }
  } catch (err) {
    // JSONC with comments — readable by VS Code, not by us. Skip
    // rather than mis-report.
    logger.warn(
      `vscode-mcp: could not parse ${filePath}: ${err instanceof Error ? err.message : String(err)}`
    )
  }
  return {}
}

function describeEntry(entry: McpJsonServerEntry): string | undefined {
  if (typeof entry.url === 'string') return entry.url
  if (typeof entry.command === 'string') {
    return [
      entry.command,
      ...(Array.isArray(entry.args) ? entry.args : []),
    ].join(' ')
  }
  return undefined
}

/** Last `data:` payload of an SSE body that parses as the JSON-RPC
 *  response for `id` (falls back to the last data line). */
function extractSseJson(body: string, id: number): string | null {
  let last: string | null = null
  for (const line of body.split('\n')) {
    if (!line.startsWith('data:')) continue
    const data = line.slice('data:'.length).trim()
    if (!data) continue
    last = data
    try {
      const parsed = JSON.parse(data) as { id?: unknown }
      if (parsed && parsed.id === id) return data
    } catch {
      // keep scanning
    }
  }
  return last
}

/**
 * Replicate VS Code's MCP tool-prefix slug (verbatim from the workbench
 * bundle's prefix generator): lowercase, collapse runs of characters
 * outside `[a-z0-9_.-]` to `_`, truncate to 13 chars. When two servers
 * produce the same slug VS Code appends a numeric disambiguator —
 * hence the `\d*` allowance in {@link slugPrefixLength}.
 */
function vscodeMcpSlug(name: string): string {
  return name
    .trim()
    .toLowerCase()
    .replace(/[^a-z0-9_.-]+/g, '_')
    .slice(0, 13)
}

function escapeRegExp(s: string): string {
  return s.replace(/[.*+?^${}()|[\]\\]/g, '\\$&')
}

/** Length of the `<slug><n>_` prefix of `rest` that `serverName` owns,
 *  or 0 when it doesn't match. `rest` is the lm tool name minus its
 *  leading `mcp_`, lowercased. */
function slugPrefixLength(rest: string, serverName: string): number {
  const slug = vscodeMcpSlug(serverName)
  if (!slug) return 0
  const m = rest.match(new RegExp(`^${escapeRegExp(slug)}\\d*_`))
  if (m) return m[0].length
  if (slug.length >= 4) {
    const m2 = rest.match(
      new RegExp(`^${escapeRegExp(slug.slice(0, slug.length - 1))}\\d+_`)
    )
    if (m2) return m2[0].length
  }
  return 0
}

function serverForLmTool(
  lmToolName: string,
  configured: ConfiguredServer[]
): { id: string; builtin: boolean } {
  const rest = lmToolName.slice('mcp_'.length).toLowerCase()
  let best: { id: string; builtin: boolean; len: number } | null = null
  for (const server of configured) {
    const len = slugPrefixLength(rest, server.key)
    if (len && (!best || len > best.len)) {
      best = { id: server.key, builtin: server.source === 'builtin', len }
    }
  }
  for (const alias of BUILTIN_NAME_ALIASES) {
    const len = slugPrefixLength(rest, alias)
    if (len && (!best || len > best.len)) {
      best = { id: alias, builtin: true, len }
    }
  }
  if (best) return { id: best.id, builtin: best.builtin }
  const firstSegment = rest.split('_', 1)[0] || rest
  return { id: `discovered:${firstSegment}`, builtin: false }
}

/** Strip `mcp_<serverSlug>_` from a tool name for display, best-effort. */
function displayToolName(lmToolName: string, ownerId: string): string {
  const rest = lmToolName.slice('mcp_'.length)
  const len = slugPrefixLength(
    rest.toLowerCase(),
    ownerId.replace(/^discovered:/, '')
  )
  return len ? rest.slice(len) : rest
}

/** OpenAI function names allow `[a-zA-Z0-9_-]{1,64}`. */
function sanitizeToolName(s: string): string {
  return s.replace(/[^a-zA-Z0-9_-]/g, '_')
}

/** Wire name for a directly-connected server's tool. Uniqueness comes
 *  from the wireMap dedupe; the name only needs to be stable. */
function directWireName(serverId: string, toolName: string): string {
  const server = sanitizeToolName(serverId).slice(0, 16)
  const tool = sanitizeToolName(toolName).slice(0, 40)
  return `${VSCODE_MCP_PREFIX}${server}_${tool}`.slice(0, 64)
}

/** Wire name for an lm-sourced tool: drop the redundant `mcp_`. */
function lmWireName(lmToolName: string): string {
  const trimmed = lmToolName.startsWith('mcp_')
    ? lmToolName.slice('mcp_'.length)
    : lmToolName
  return `${VSCODE_MCP_PREFIX}${sanitizeToolName(trimmed)}`.slice(0, 64)
}

/** OpenAI function tools require `parameters.type === 'object'`; some
 *  MCP tools ship no schema (or a non-object one). */
function shapeParameters(
  schema: Record<string, unknown> | undefined
): Record<string, unknown> {
  if (!schema || typeof schema !== 'object') {
    return { type: 'object', properties: {} }
  }
  return {
    type: 'object',
    properties:
      (schema as { properties?: Record<string, unknown> }).properties ?? {},
    ...(Array.isArray((schema as { required?: string[] }).required)
      ? { required: (schema as { required: string[] }).required }
      : {}),
    additionalProperties:
      (schema as { additionalProperties?: boolean }).additionalProperties ??
      false,
  }
}
