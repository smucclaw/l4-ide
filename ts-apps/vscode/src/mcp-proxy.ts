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
 * the extension. Announced to the editor in one of two ways:
 *
 *   - On hosts with the MCP server definition provider API (stable
 *     since VS Code 1.101): via
 *     {@link registerL4McpServerDefinitionProvider}, so the server gets
 *     proper lifecycle handling (no user-file edits, port changes
 *     propagate through the change event). Any entry we wrote to
 *     `mcp.json` in earlier versions is cleaned up.
 *   - On older hosts and forks without the API (e.g. Cursor): by
 *     writing itself into the user-level `mcp.json`, as before.
 *
 * When connected to a jl4-service, forwards MCP JSON-RPC requests with
 * the user's credentials. When disconnected or unauthenticated, returns
 * an empty tool list so the server stays registered but inert.
 */
export class McpProxy implements vscode.Disposable {
  private server: http.Server | null = null
  private port: number = 0
  private outputChannel: vscode.OutputChannel
  private extensionPath: string | undefined
  private userDataPath: string | undefined

  private readonly didChangeLocalUrl = new vscode.EventEmitter<void>()
  /** Fires whenever {@link getLocalUrl} changes (bind, rebind on a new
   *  port, shutdown). Wired into the MCP server definition provider's
   *  `onDidChangeMcpServerDefinitions` so the editor re-queries us. */
  readonly onDidChangeLocalUrl = this.didChangeLocalUrl.event

  constructor(
    private readonly auth: AuthManager,
    outputChannel: vscode.OutputChannel,
    // Kept as an arg for call-site compatibility even though we no
    // longer persist any state — the auto-add startup flow (which used
    // `l4.claudeCodeSetupDismissed`) has been removed.
    _globalState?: vscode.Memento,
    extensionPath?: string,
    // Path to VS Code's per-user data dir (the one containing `mcp.json`
    // and `globalStorage/`). Derived from `context.globalStorageUri` by
    // the caller so we transparently handle Insiders, VSCodium, portable
    // installs, remote SSH, etc.
    userDataPath?: string
  ) {
    this.outputChannel = outputChannel
    this.extensionPath = extensionPath
    this.userDataPath = userDataPath
  }

  /** Start the proxy and update VS Code's user-level `mcp.json`. Call once on activation. */
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

    // If Claude Code already has l4-rules configured, update the port
    this.updateClaudeCodePort()

    if (getMcpProviderApi()) {
      // The definition provider announces us — remove any entry a
      // previous extension version wrote to mcp.json so the server
      // doesn't show up twice.
      this.cleanupVSCodeMcpJson()
    } else {
      // No provider API on this host — register (or refresh) ourselves
      // in the user-level mcp.json so the server is reachable across
      // all workspaces. VS Code watches this file and prompts the user
      // to trust the server on first sight.
      this.updateVSCodeMcpJson()
    }

    this.didChangeLocalUrl.fire()
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
    const mcpUrl = this.auth.getEffectiveMcpUrl()

    if (!mcpUrl) return this.handleLocally(body)

    let resp: Response
    try {
      const headers = await this.auth.getAuthHeaders()
      resp = await fetch(mcpUrl, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json', ...headers },
        body,
      })
    } catch (err) {
      this.outputChannel.appendLine(
        `[mcp-proxy] fetch ${mcpUrl} failed: ${err instanceof Error ? err.message : String(err)}`
      )
      return this.errorResponse(
        body,
        -32002,
        `L4 service unreachable at ${mcpUrl}`
      )
    }

    await this.captureRefreshedSession(resp)

    if (resp.status === 401 || resp.status === 403) {
      const preview = await resp.text().catch(() => '')
      this.outputChannel.appendLine(
        `[mcp-proxy] ${mcpUrl} responded ${resp.status}${preview ? `: ${preview.slice(0, 200)}` : ''}`
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
      `[mcp-proxy] ${mcpUrl} responded ${resp.status} (body ${text ? `${text.length}B` : 'empty'})`
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
            serverInfo: { name: 'L4 Rules', version: '1.3.0' },
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

  /** Key our entry uses in VS Code's user-level `mcp.json`. VS Code
   *  surfaces this key verbatim in the chat picker, so we use the
   *  display-friendly form rather than a slug. */
  private static readonly VSCODE_MCP_KEY = 'L4 Rules'

  /** Older keys we may have written and want to migrate away from when
   *  we encounter them in a user's `mcp.json`. We only delete an entry
   *  whose URL points at our localhost port — anything else is
   *  user-owned and left alone. */
  private static readonly VSCODE_MCP_LEGACY_KEYS = ['l4-rules', 'l4-tools']

  /**
   * Write (or refresh) our entry in VS Code's user-level `mcp.json` so
   * the MCP server is available globally across all workspaces. Creates
   * the file if missing. Fallback for hosts without the MCP server
   * definition provider API; called on every {@link start} there so the
   * URL stays in sync if `jl4.mcpPort` changes (a port change triggers
   * a restart, which re-runs this).
   *
   * Note: `mcp.json` is JSONC (supports comments). We parse with plain
   * JSON; if the user has added comments we leave the file alone and
   * log — better to skip than corrupt their config.
   */
  private updateVSCodeMcpJson(): void {
    if (!this.port || !this.userDataPath) return

    const mcpJsonPath = path.join(this.userDataPath, 'mcp.json')
    const desiredUrl = `http://127.0.0.1:${this.port}/mcp`
    const desiredEntry = { type: 'http', url: desiredUrl }

    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    let config: any = {}
    try {
      const raw = fs.readFileSync(mcpJsonPath, 'utf-8')
      if (raw.trim()) {
        try {
          config = JSON.parse(raw)
        } catch (err) {
          this.outputChannel.appendLine(
            `[mcp-proxy] Skipped updating ${mcpJsonPath}: not valid JSON (${err instanceof Error ? err.message : String(err)}). If you have comments in the file, remove them or add the entry manually.`
          )
          return
        }
      }
    } catch (err) {
      // ENOENT is fine — we'll create the file. Anything else, give up.
      if ((err as NodeJS.ErrnoException).code !== 'ENOENT') {
        this.outputChannel.appendLine(
          `[mcp-proxy] Failed to read ${mcpJsonPath}: ${err instanceof Error ? err.message : String(err)}`
        )
        return
      }
    }

    if (!config || typeof config !== 'object') config = {}
    if (!config.servers || typeof config.servers !== 'object') {
      config.servers = {}
    }

    // Migrate away from any legacy key we previously wrote. Only delete
    // entries whose URL still matches our localhost port — anything
    // else is the user's own, possibly unrelated, entry.
    const localhostPrefix = `http://127.0.0.1:${this.port}/`
    let migratedFromLegacy = false
    for (const legacyKey of McpProxy.VSCODE_MCP_LEGACY_KEYS) {
      const legacy = config.servers[legacyKey]
      if (
        legacy &&
        typeof legacy.url === 'string' &&
        legacy.url.startsWith(localhostPrefix)
      ) {
        delete config.servers[legacyKey]
        migratedFromLegacy = true
      }
    }

    const existing = config.servers[McpProxy.VSCODE_MCP_KEY]
    if (
      !migratedFromLegacy &&
      existing &&
      existing.type === desiredEntry.type &&
      existing.url === desiredEntry.url
    ) {
      return // already correct
    }

    config.servers[McpProxy.VSCODE_MCP_KEY] = {
      ...(existing ?? {}),
      ...desiredEntry,
    }

    try {
      fs.mkdirSync(path.dirname(mcpJsonPath), { recursive: true })
      fs.writeFileSync(mcpJsonPath, JSON.stringify(config, null, 2) + '\n')
      this.outputChannel.appendLine(
        existing
          ? `[mcp-proxy] Refreshed ${McpProxy.VSCODE_MCP_KEY} in ${mcpJsonPath} → ${desiredUrl}`
          : `[mcp-proxy] Added ${McpProxy.VSCODE_MCP_KEY} to ${mcpJsonPath} → ${desiredUrl}`
      )
    } catch (err) {
      this.outputChannel.appendLine(
        `[mcp-proxy] Failed to write ${mcpJsonPath}: ${err instanceof Error ? err.message : String(err)}`
      )
    }
  }

  /**
   * Remove any entry a previous extension version wrote to the
   * user-level `mcp.json`. Only entries under our known keys whose URL
   * is a localhost `/mcp` endpoint are deleted — anything else is
   * user-owned and left alone. Runs on hosts where the definition
   * provider API announces the server instead.
   */
  private cleanupVSCodeMcpJson(): void {
    if (!this.userDataPath) return

    const mcpJsonPath = path.join(this.userDataPath, 'mcp.json')
    let raw: string
    try {
      raw = fs.readFileSync(mcpJsonPath, 'utf-8')
    } catch {
      return // no file, nothing to clean up
    }

    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    let config: any
    try {
      config = JSON.parse(raw)
    } catch {
      return // JSONC / hand-edited — leave alone rather than corrupt
    }
    if (!config?.servers || typeof config.servers !== 'object') return

    // Any port counts — the entry may have been written under an old
    // `jl4.mcpPort` value.
    const oursUrl = /^http:\/\/127\.0\.0\.1:\d+\/mcp$/
    let removed = false
    for (const key of [
      McpProxy.VSCODE_MCP_KEY,
      ...McpProxy.VSCODE_MCP_LEGACY_KEYS,
    ]) {
      const entry = config.servers[key]
      if (entry && typeof entry.url === 'string' && oursUrl.test(entry.url)) {
        delete config.servers[key]
        removed = true
      }
    }
    if (!removed) return

    try {
      fs.writeFileSync(mcpJsonPath, JSON.stringify(config, null, 2) + '\n')
      this.outputChannel.appendLine(
        `[mcp-proxy] Removed legacy entry from ${mcpJsonPath} (now announced via the MCP provider API)`
      )
    } catch (err) {
      this.outputChannel.appendLine(
        `[mcp-proxy] Failed to clean up ${mcpJsonPath}: ${err instanceof Error ? err.message : String(err)}`
      )
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

  /**
   * Tear down the listening server without destroying the McpProxy
   * instance. After a stop, {@link start} can be called again to bind
   * a (possibly different) port. Shared by {@link dispose} and
   * {@link restart}.
   */
  private async stop(): Promise<void> {
    if (this.server) {
      const server = this.server
      await new Promise<void>((resolve) => server.close(() => resolve()))
      this.server = null
      this.port = 0
      this.outputChannel.appendLine('[mcp-proxy] Stopped')
      this.didChangeLocalUrl.fire()
    }
  }

  /**
   * Restart the proxy on whatever port `jl4.mcpPort` currently
   * resolves to. Used when the user changes the port setting at
   * runtime — without restart the old socket stays bound and the
   * URL in `mcp.json` stays stale.
   */
  async restart(): Promise<void> {
    await this.stop()
    await this.start()
  }

  dispose(): void {
    // Fire-and-forget on dispose — VS Code doesn't await disposables
    // and a hanging close() shouldn't block shutdown.
    void this.stop()
    this.didChangeLocalUrl.dispose()
  }
}

/*
 * MCP server definition provider (VS Code >= 1.101).
 *
 * Our pinned @types/vscode (^1.95, matching the engines floor we keep
 * for fork compatibility) predates this API, so the shapes below are
 * minimal structural stand-ins and everything is feature-detected at
 * runtime. Once the engines floor moves past 1.101 these can be
 * replaced with the real `vscode.*` types.
 */

interface McpHttpServerDefinitionLike {
  readonly label: string
  readonly uri: vscode.Uri
}

type McpHttpServerDefinitionCtor = new (
  label: string,
  uri: vscode.Uri,
  headers?: Record<string, string>,
  version?: string
) => McpHttpServerDefinitionLike

interface McpServerDefinitionProviderLike {
  onDidChangeMcpServerDefinitions?: vscode.Event<void>
  provideMcpServerDefinitions(
    token: vscode.CancellationToken
  ): vscode.ProviderResult<McpHttpServerDefinitionLike[]>
}

interface McpProviderApi {
  register: (
    id: string,
    provider: McpServerDefinitionProviderLike
  ) => vscode.Disposable
  HttpDefinition: McpHttpServerDefinitionCtor
}

function getMcpProviderApi(): McpProviderApi | undefined {
  const lm = vscode.lm as
    | (typeof vscode.lm & {
        registerMcpServerDefinitionProvider?: McpProviderApi['register']
      })
    | undefined
  const ctor = (vscode as unknown as Record<string, unknown>)
    .McpHttpServerDefinition as McpHttpServerDefinitionCtor | undefined
  if (
    typeof lm?.registerMcpServerDefinitionProvider !== 'function' ||
    typeof ctor !== 'function'
  ) {
    return undefined
  }
  return {
    register: lm.registerMcpServerDefinitionProvider.bind(lm),
    HttpDefinition: ctor,
  }
}

/** Must match the `contributes.mcpServerDefinitionProviders` id in
 *  package.json — VS Code refuses the registration otherwise. */
const MCP_PROVIDER_ID = 'l4.mcp-servers'

/**
 * Announce the proxy to the editor through the MCP server definition
 * provider API, so Copilot agent mode (and the MCP Servers UI) picks it
 * up with proper lifecycle handling instead of a hand-written mcp.json
 * entry. Returns undefined on hosts without the API — the proxy's
 * mcp.json fallback covers those.
 */
export function registerL4McpServerDefinitionProvider(
  proxy: McpProxy,
  outputChannel: vscode.OutputChannel
): vscode.Disposable | undefined {
  const api = getMcpProviderApi()
  if (!api) {
    outputChannel.appendLine(
      '[mcp-proxy] MCP provider API unavailable on this host — using mcp.json fallback'
    )
    return undefined
  }
  const disposable = api.register(MCP_PROVIDER_ID, {
    onDidChangeMcpServerDefinitions: proxy.onDidChangeLocalUrl,
    provideMcpServerDefinitions: () => {
      const url = proxy.getLocalUrl()
      if (!url) return []
      return [new api.HttpDefinition('L4 Rules', vscode.Uri.parse(url))]
    },
  })
  outputChannel.appendLine(
    `[mcp-proxy] Registered MCP server definition provider (${MCP_PROVIDER_ID})`
  )
  return disposable
}
