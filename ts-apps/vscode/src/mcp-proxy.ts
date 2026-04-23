import * as http from 'node:http'
import * as fs from 'node:fs'
import * as path from 'node:path'
import * as os from 'node:os'
import * as vscode from 'vscode'
import type { AuthManager } from './auth.js'
import { showTimedWarningMessage } from './notifications.js'
import { installL4Cli } from './install-cli.js'

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
export class McpProxy implements vscode.Disposable {
  private server: http.Server | null = null
  private port: number = 0
  private mcpRegistration: vscode.Disposable | undefined
  private outputChannel: vscode.OutputChannel
  private extensionPath: string | undefined

  constructor(
    private readonly auth: AuthManager,
    outputChannel: vscode.OutputChannel,
    // Kept as an arg for call-site compatibility even though we no
    // longer persist any state — the auto-add startup flow (which used
    // `l4.claudeCodeSetupDismissed`) has been removed.
    _globalState?: vscode.Memento,
    extensionPath?: string
  ) {
    this.outputChannel = outputChannel
    this.extensionPath = extensionPath
  }

  /** Start the proxy and register with VS Code. Call once on activation. */
  async start(): Promise<void> {
    if (this.server) return

    this.server = http.createServer((req, res) => {
      this.handleRequest(req, res)
    })

    const port =
      vscode.workspace.getConfiguration('jl4').get<number>('mcpPort') ?? 19415

    try {
      await new Promise<void>((resolve, reject) => {
        this.server!.once('error', reject)
        this.server!.listen(port, '127.0.0.1', () => {
          this.server!.removeAllListeners('error')
          this.port = port
          resolve()
        })
      })
    } catch {
      this.server.close()
      this.server = null
      this.outputChannel.appendLine(
        `[mcp-proxy] Could not bind to port ${port}`
      )
      vscode.window.showErrorMessage(
        `L4 MCP server can't bind to localhost:${port}. Change the port in Settings → jl4.mcpPort.`
      )
      return
    }

    this.outputChannel.appendLine(
      `[mcp-proxy] Started on http://127.0.0.1:${this.port}/mcp`
    )

    // Register with VS Code's MCP system so Copilot and other
    // AI extensions discover the server automatically.
    this.registerWithVSCode()

    // If Claude Code already has l4-rules configured, update the port
    this.updateClaudeCodePort()
  }

  /**
   * Register the MCP server with VS Code's language model API.
   * Uses the registerMcpServerDefinitionProvider API (VS Code 1.99+).
   */
  private registerWithVSCode(): void {
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    const lm = vscode.lm as any
    if (!lm) {
      this.outputChannel.appendLine('[mcp-proxy] vscode.lm API not available')
      return
    }

    const mcpUrl = `http://127.0.0.1:${this.port}/mcp`

    // VS Code 1.99+ uses registerMcpServerDefinitionProvider
    // The provider ID must match the one declared in package.json contributes.mcpServerDefinitionProviders
    if (typeof lm.registerMcpServerDefinitionProvider === 'function') {
      try {
        // Check if McpHttpServerDefinition class exists
        // eslint-disable-next-line @typescript-eslint/no-explicit-any
        const McpHttpServerDefinition = (vscode as any).McpHttpServerDefinition
        if (McpHttpServerDefinition) {
          this.mcpRegistration = lm.registerMcpServerDefinitionProvider(
            'l4-tools',
            {
              provideMcpServerDefinitions: () => {
                return [
                  new McpHttpServerDefinition({
                    label: 'L4 Tools',
                    uri: mcpUrl,
                  }),
                ]
              },
            }
          )
          this.outputChannel.appendLine(
            '[mcp-proxy] Registered with VS Code MCP (McpHttpServerDefinition)'
          )
          return
        }

        // Fallback: try with plain object if class doesn't exist
        this.mcpRegistration = lm.registerMcpServerDefinitionProvider(
          'l4-tools',
          {
            provideMcpServerDefinitions: () => {
              return [
                {
                  label: 'L4 Tools',
                  type: 'http',
                  uri: mcpUrl,
                },
              ]
            },
          }
        )
        this.outputChannel.appendLine(
          '[mcp-proxy] Registered with VS Code MCP (plain object)'
        )
        return
      } catch (err) {
        this.outputChannel.appendLine(
          `[mcp-proxy] registerMcpServerDefinitionProvider failed: ${err instanceof Error ? err.message : String(err)}`
        )
      }
    }

    this.outputChannel.appendLine(
      `[mcp-proxy] VS Code MCP API not available - server accessible at ${mcpUrl}`
    )
  }

  /** The local MCP endpoint URL, or undefined if not running. */
  getLocalUrl(): string | undefined {
    if (!this.server || !this.port) return undefined
    return `http://127.0.0.1:${this.port}/mcp`
  }

  /** Handle an incoming HTTP request. */
  private handleRequest(
    req: http.IncomingMessage,
    res: http.ServerResponse
  ): void {
    const isMcpPath = req.url === '/mcp' || req.url?.startsWith('/mcp?')

    if (!isMcpPath) {
      res.writeHead(404, { 'Content-Type': 'application/json' })
      res.end(JSON.stringify({ error: 'Not found' }))
      return
    }

    if (req.method !== 'POST') {
      res.writeHead(405, { 'Content-Type': 'application/json' })
      res.end(JSON.stringify({ error: 'Method not allowed' }))
      return
    }

    // POST /mcp — MCP JSON-RPC
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

  /** Minimum interval between "sign in again" prompts. */
  private static readonly REAUTH_PROMPT_COOLDOWN_MS = 15 * 60 * 1000
  private lastReauthPromptAt = 0

  /**
   * Dispatch a JSON-RPC request. If a service URL is configured, forward
   * it; otherwise return a local stub so the server stays registered.
   *
   * On each successful forward, ride any `Set-Cookie: wos-session=…` the
   * auth proxy emits so our Bearer token rotates in lockstep with the
   * server's transparent refresh. On 401/403, re-verify connection state
   * (to clear a stale green dot) and prompt for re-login.
   */
  private async dispatch(body: string): Promise<string> {
    const serviceUrl = this.auth.getEffectiveServiceUrl()

    if (!serviceUrl) return this.handleLocally(body)

    let resp: Response
    try {
      const headers = await this.auth.getAuthHeaders()
      resp = await fetch(`${serviceUrl}/.mcp`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json', ...headers },
        body,
      })
    } catch (err) {
      this.outputChannel.appendLine(
        `[mcp-proxy] fetch ${serviceUrl}/.mcp failed: ${err instanceof Error ? err.message : String(err)}`
      )
      return this.errorResponse(
        body,
        -32002,
        `L4 service unreachable at ${serviceUrl}`
      )
    }

    await this.captureRefreshedSession(resp)

    if (resp.status === 401 || resp.status === 403) {
      const preview = await resp.text().catch(() => '')
      this.outputChannel.appendLine(
        `[mcp-proxy] ${serviceUrl}/.mcp responded ${resp.status}${preview ? `: ${preview.slice(0, 200)}` : ''}`
      )
      void this.auth.verifyConnection()
      this.maybePromptReauth()
      return this.errorResponse(
        body,
        -32001,
        'L4 authentication expired — sign in again'
      )
    }

    const text = await resp.text()
    if (resp.ok && text) return text

    this.outputChannel.appendLine(
      `[mcp-proxy] ${serviceUrl}/.mcp responded ${resp.status} (body ${text ? `${text.length}B` : 'empty'})`
    )
    return this.errorResponse(
      body,
      -32002,
      `L4 service error (HTTP ${resp.status})`
    )
  }

  /**
   * If the auth proxy rotated the sealed session, grab the new value from
   * Set-Cookie and persist it so the next request uses the refreshed token.
   */
  private async captureRefreshedSession(resp: Response): Promise<void> {
    // getSetCookie() returns one entry per Set-Cookie header (Node 20+);
    // fall back to the comma-joined get('set-cookie') on older runtimes.
    const headers = resp.headers as Headers & {
      getSetCookie?: () => string[]
    }
    const cookies = headers.getSetCookie
      ? headers.getSetCookie()
      : headers.get('set-cookie')
        ? [headers.get('set-cookie') as string]
        : []
    for (const c of cookies) {
      const m = c.match(/(?:^|;\s*)wos-session=([^;\s]+)/)
      if (m) {
        const current = await this.auth.getSessionToken()
        if (m[1] !== current) {
          await this.auth.setSessionToken(m[1])
          this.outputChannel.appendLine(
            '[mcp-proxy] Rotated session token from Set-Cookie'
          )
        }
        return
      }
    }
  }

  /** Build a JSON-RPC error reusing the inbound request id. */
  private errorResponse(body: string, code: number, message: string): string {
    let id: unknown = null
    try {
      id = (JSON.parse(body) as { id?: unknown }).id ?? null
    } catch {
      // leave id null
    }
    return JSON.stringify({ jsonrpc: '2.0', id, error: { code, message } })
  }

  private maybePromptReauth(): void {
    const now = Date.now()
    if (now - this.lastReauthPromptAt < McpProxy.REAUTH_PROMPT_COOLDOWN_MS) {
      return
    }
    this.lastReauthPromptAt = now
    void vscode.window
      .showWarningMessage(
        'L4 session expired. Sign in again to keep using L4 tools.',
        'Sign in'
      )
      .then((choice) => {
        if (choice === 'Sign in') void this.auth.login()
      })
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
            serverInfo: { name: 'L4 Tools', version: '1.3.0' },
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
   * If Claude Code's config has l4-rules, silently update the port
   * to match the current proxy. Called on every start().
   */
  private updateClaudeCodePort(): void {
    if (!this.port) return
    const claudeConfigPath = path.join(os.homedir(), '.claude.json')
    try {
      const raw = fs.readFileSync(claudeConfigPath, 'utf-8')
      const config = JSON.parse(raw)
      const existing = config?.mcpServers?.['l4-rules']
      if (!existing) return // not configured — don't touch
      const newUrl = `http://127.0.0.1:${this.port}/mcp`
      if (existing.url === newUrl) return // already correct
      config.mcpServers['l4-rules'] = { ...existing, url: newUrl }
      fs.writeFileSync(claudeConfigPath, JSON.stringify(config, null, 2))
      this.outputChannel.appendLine(
        `[mcp-proxy] Updated Claude Code port to ${this.port}`
      )
    } catch {
      // Config doesn't exist or isn't parseable — skip silently
    }
  }

  /**
   * Public entry point triggered from the sidebar menu.
   * Verifies/installs the L4 tools (MCP server + writing-l4-rules skill)
   * in Claude Code, without re-prompting the user.
   *
   * If ~/.claude.json is missing, shows a 4s warning and does nothing.
   */
  async addL4ToolsToClaudeCode(): Promise<void> {
    const claudeConfigPath = path.join(os.homedir(), '.claude.json')
    try {
      fs.accessSync(claudeConfigPath, fs.constants.R_OK)
    } catch {
      showTimedWarningMessage(
        'Claude Code .claude.json file could not be found in the user home directory'
      )
      return
    }
    this.installL4Tools(claudeConfigPath)
  }

  /**
   * Write (or refresh) the l4-rules MCP entry in ~/.claude.json, install
   * the bundled writing-l4-rules skill, and show a confirmation toast.
   *
   * Assumes claudeConfigPath exists and is readable.
   */
  private installL4Tools(claudeConfigPath: string): void {
    if (!this.port) return

    try {
      const raw = fs.readFileSync(claudeConfigPath, 'utf-8')
      const config = JSON.parse(raw)
      if (!config.mcpServers) config.mcpServers = {}
      const desiredUrl = `http://127.0.0.1:${this.port}/mcp`
      const existing = config.mcpServers['l4-rules']
      const alreadyCorrect =
        existing && existing.type === 'http' && existing.url === desiredUrl
      if (!alreadyCorrect) {
        config.mcpServers['l4-rules'] = {
          type: 'http',
          url: desiredUrl,
        }
        fs.writeFileSync(claudeConfigPath, JSON.stringify(config, null, 2))
        this.outputChannel.appendLine(
          existing
            ? '[mcp-proxy] Refreshed l4-rules entry in ~/.claude.json'
            : '[mcp-proxy] Added l4-rules to ~/.claude.json'
        )
      } else {
        this.outputChannel.appendLine(
          '[mcp-proxy] l4-rules entry in ~/.claude.json already up to date'
        )
      }
    } catch (err) {
      this.outputChannel.appendLine(
        `[mcp-proxy] Failed to update ~/.claude.json: ${err instanceof Error ? err.message : String(err)}`
      )
      return
    }

    this.installWritingL4RulesSkill()

    // The writing-l4-rules skill tells Claude to invoke the `l4` CLI for
    // validation, so installing the skill without installing the CLI
    // leaves the agent with instructions it can't follow. Install both
    // together, silently (we'll surface a single "done" toast below).
    if (this.extensionPath) {
      void installL4Cli(this.extensionPath, this.outputChannel, {
        silent: true,
      })
    }

    void vscode.window.showInformationMessage(
      'L4 tools added to Claude Code. Restart Claude Code to pick up the change.',
      'Okay'
    )
  }

  /**
   * Copy the bundled writing-l4-rules skill into ~/.claude/skills/.
   * Returns true on success, false otherwise (non-fatal).
   */
  private installWritingL4RulesSkill(): boolean {
    if (!this.extensionPath) {
      this.outputChannel.appendLine(
        '[mcp-proxy] Skill install skipped: extensionPath not set'
      )
      return false
    }
    const src = path.join(
      this.extensionPath,
      'static',
      'skills',
      'writing-l4-rules'
    )
    if (!fs.existsSync(src)) {
      this.outputChannel.appendLine(
        `[mcp-proxy] Skill install skipped: bundled skill not found at ${src}`
      )
      return false
    }
    const dest = path.join(
      os.homedir(),
      '.claude',
      'skills',
      'writing-l4-rules'
    )
    try {
      fs.mkdirSync(path.dirname(dest), { recursive: true })
      fs.cpSync(src, dest, { recursive: true, force: true })
      this.outputChannel.appendLine(
        `[mcp-proxy] Installed writing-l4-rules skill to ${dest}`
      )
      return true
    } catch (err) {
      this.outputChannel.appendLine(
        `[mcp-proxy] Failed to install writing-l4-rules skill: ${err instanceof Error ? err.message : String(err)}`
      )
      return false
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
