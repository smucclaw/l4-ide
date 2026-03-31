import * as http from 'node:http'
import * as fs from 'node:fs'
import * as path from 'node:path'
import * as os from 'node:os'
import * as vscode from 'vscode'
import type { AuthManager } from './auth.js'

/**
 * Local MCP proxy server.
 *
 * Starts on extension activation and stays running for the lifetime of
 * the extension.  Registers with VS Code's MCP system so AI tools
 * (Copilot, Cursor, etc.) discover it automatically.
 *
 * When connected to a jl4-service, forwards MCP JSON-RPC requests with
 * the user's credentials. When disconnected or unauthenticated, returns
 * an empty tool list so the server stays registered but inert.
 */
const CLAUDE_SETUP_DISMISSED_KEY = 'l4.claudeCodeSetupDismissed'

export class McpProxy implements vscode.Disposable {
  private server: http.Server | null = null
  private port: number = 0
  private mcpRegistration: vscode.Disposable | undefined
  private outputChannel: vscode.OutputChannel
  private globalState: vscode.Memento | undefined

  constructor(
    private readonly auth: AuthManager,
    outputChannel: vscode.OutputChannel,
    globalState?: vscode.Memento
  ) {
    this.outputChannel = outputChannel
    this.globalState = globalState
  }

  /** Start the proxy and register with VS Code. Call once on activation. */
  async start(): Promise<void> {
    if (this.server) return

    this.server = http.createServer((req, res) => {
      this.handleRequest(req, res)
    })

    await new Promise<void>((resolve, reject) => {
      this.server!.listen(0, '127.0.0.1', () => {
        const addr = this.server!.address()
        if (addr && typeof addr === 'object') {
          this.port = addr.port
        }
        resolve()
      })
      this.server!.on('error', reject)
    })

    this.outputChannel.appendLine(
      `[mcp-proxy] Started on http://127.0.0.1:${this.port}/.mcp`
    )

    // Register with VS Code's MCP system so Copilot and other
    // AI extensions discover the server automatically.
    try {
      // eslint-disable-next-line @typescript-eslint/no-explicit-any
      const lm = vscode.lm as any
      if (typeof lm?.registerMcpServerDefinition === 'function') {
        this.mcpRegistration = lm.registerMcpServerDefinition({
          label: 'L4 Legal Rules',
          url: `http://127.0.0.1:${this.port}/.mcp`,
        })
        this.outputChannel.appendLine('[mcp-proxy] Registered with VS Code MCP')
      }
    } catch {
      // API not available in this VS Code version — proxy still works via URL
    }
  }

  /** The local MCP endpoint URL, or undefined if not running. */
  getLocalUrl(): string | undefined {
    if (!this.server || !this.port) return undefined
    return `http://127.0.0.1:${this.port}/.mcp`
  }

  /** Handle an incoming HTTP request. */
  private handleRequest(
    req: http.IncomingMessage,
    res: http.ServerResponse
  ): void {
    if (req.method !== 'POST' || !req.url?.startsWith('/.mcp')) {
      res.writeHead(404, { 'Content-Type': 'application/json' })
      res.end(JSON.stringify({ error: 'Not found' }))
      return
    }

    const chunks: Buffer[] = []
    req.on('data', (chunk: Buffer) => chunks.push(chunk))
    req.on('end', () => {
      const body = Buffer.concat(chunks).toString('utf-8')
      this.dispatch(body)
        .then((result) => {
          res.writeHead(200, { 'Content-Type': 'application/json' })
          res.end(result)
        })
        .catch((err) => {
          this.outputChannel.appendLine(
            `[mcp-proxy] Error: ${err instanceof Error ? err.message : String(err)}`
          )
          res.writeHead(200, { 'Content-Type': 'application/json' })
          res.end(
            JSON.stringify({
              jsonrpc: '2.0',
              error: { code: -32603, message: 'Internal error' },
            })
          )
        })
    })
  }

  /**
   * Dispatch a JSON-RPC request. If connected to a service, forward it.
   * Otherwise handle locally with empty/error responses.
   */
  private async dispatch(body: string): Promise<string> {
    const serviceUrl = this.auth.getEffectiveServiceUrl()

    // If connected, forward to the remote service
    if (serviceUrl) {
      try {
        const headers = await this.auth.getAuthHeaders()
        const resp = await fetch(`${serviceUrl}/.mcp`, {
          method: 'POST',
          headers: { 'Content-Type': 'application/json', ...headers },
          body,
        })
        return await resp.text()
      } catch {
        // Service unreachable — fall through to local handling
      }
    }

    // Not connected or service unreachable — handle locally
    return this.handleLocally(body)
  }

  /** Handle requests locally when no service is available. */
  private handleLocally(body: string): string {
    let parsed: { id?: unknown; method?: string }
    try {
      parsed = JSON.parse(body)
    } catch {
      return JSON.stringify({
        jsonrpc: '2.0',
        error: { code: -32700, message: 'Parse error' },
      })
    }

    const id = parsed.id ?? null

    switch (parsed.method) {
      case 'initialize':
        return JSON.stringify({
          jsonrpc: '2.0',
          id,
          result: {
            protocolVersion: '2025-03-26',
            serverInfo: { name: 'L4 Legal Rules', version: '1.0.0' },
            capabilities: { tools: {} },
          },
        })

      case 'notifications/initialized':
        return JSON.stringify({ jsonrpc: '2.0', id, result: {} })

      case 'tools/list':
        return JSON.stringify({ jsonrpc: '2.0', id, result: { tools: [] } })

      case 'tools/call':
        return JSON.stringify({
          jsonrpc: '2.0',
          id,
          error: {
            code: -32002,
            message: 'Not connected to L4 service',
          },
        })

      default:
        return JSON.stringify({
          jsonrpc: '2.0',
          id,
          error: { code: -32601, message: 'Method not found' },
        })
    }
  }

  /**
   * If Claude Code's config exists (~/.claude.json), offer to add the
   * local MCP proxy so Claude Code can access L4 tools automatically.
   */
  async offerClaudeCodeSetup(): Promise<void> {
    if (!this.port) return

    const claudeConfigPath = path.join(os.homedir(), '.claude.json')
    try {
      fs.accessSync(claudeConfigPath, fs.constants.R_OK)
    } catch {
      return // Claude Code not installed
    }

    // Check if already configured
    try {
      const raw = fs.readFileSync(claudeConfigPath, 'utf-8')
      const config = JSON.parse(raw)
      if (config?.mcpServers?.['l4-rules']) return // already set up
    } catch {
      return // can't parse config
    }

    // Check if user previously dismissed
    if (this.globalState?.get<boolean>(CLAUDE_SETUP_DISMISSED_KEY)) return

    const action = await vscode.window.showInformationMessage(
      'Add L4 Tools to Claude Code?',
      'Yes',
      'No',
      'Never'
    )
    if (action === 'Never') {
      this.globalState?.update(CLAUDE_SETUP_DISMISSED_KEY, true)
      return
    }
    if (action !== 'Yes') return

    try {
      const raw = fs.readFileSync(claudeConfigPath, 'utf-8')
      const config = JSON.parse(raw)
      if (!config.mcpServers) config.mcpServers = {}
      config.mcpServers['l4-rules'] = {
        type: 'http',
        url: `http://127.0.0.1:${this.port}/.mcp`,
      }
      fs.writeFileSync(claudeConfigPath, JSON.stringify(config, null, 2))
      this.outputChannel.appendLine(
        '[mcp-proxy] Added l4-rules to ~/.claude.json'
      )
      vscode.window.showInformationMessage(
        'L4 tools added to Claude Code. Restart Claude Code to pick up the change.'
      )
    } catch (err) {
      this.outputChannel.appendLine(
        `[mcp-proxy] Failed to update ~/.claude.json: ${err instanceof Error ? err.message : String(err)}`
      )
    }
  }

  dispose(): void {
    this.mcpRegistration?.dispose()
    this.mcpRegistration = undefined
    if (this.server) {
      this.server.close()
      this.server = null
      this.port = 0
      this.outputChannel.appendLine('[mcp-proxy] Stopped')
    }
  }
}
