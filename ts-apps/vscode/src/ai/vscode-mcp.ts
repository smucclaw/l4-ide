import * as fs from 'node:fs'
import * as os from 'node:os'
import * as path from 'node:path'
import { spawn, type ChildProcess } from 'node:child_process'
import * as vscode from 'vscode'
import type { AiLogger } from './logger.js'
import type { AiProxyTool } from './ai-proxy-client.js'
import type { McpOAuthManager } from './mcp-oauth.js'

/**
 * MCP servers for the Legalese AI sidebar.
 *
 * The extension keeps its OWN server list (in globalState) and runs
 * every enabled server itself — http/sse over fetch, stdio via a
 * spawned child process, and OAuth-protected servers through our own
 * authorization client ({@link McpOAuthManager}). We never control or
 * depend on VS Code's MCP runtime.
 *
 * VS Code's `mcp.json` (user + workspace) is only an import catalog:
 * its servers are listed, toggled off, below our own. Turning one on
 * for the first time copies its config into our list — from then on
 * it's ours (connection, toggles, removal), and it disappears from the
 * catalog. Removing a server removes it from OUR list only; the
 * mcp.json entry is untouched.
 *
 * Lifecycle: enabled servers auto-connect once a usable Legalese AI
 * session exists (boot or login). Interactive actions (toggle-on,
 * menu Start, import) may kick off a browser sign-in for OAuth
 * servers; background auto-connects never do — they surface a
 * "sign-in required" state instead.
 *
 * The built-in "L4 Rules" proxy is excluded from the catalog — its
 * tools already reach the model via the `l4-rules__` path and it has
 * its own permission row.
 */

/** Wire-name prefix for MCP tools advertised to the ai-proxy,
 *  mirroring `l4-rules__` for the built-in server. Prefix-routing in
 *  the dispatcher and the permission gate key off it. */
export const VSCODE_MCP_PREFIX = 'vsmcp__'

/** Master switch: expose MCP servers to Legalese AI at all. */
const MCP_ENABLED_SETTING = 'legaleseAi.mcp.enabled'

/** Servers the user switched off (deny-list so new servers default to
 *  available; calls stay permission-gated). */
const DISABLED_SERVERS_SETTING = 'legaleseAi.mcp.disabledServers'

/** Individual tools the user switched off, as `serverId::toolName`. */
const DISABLED_TOOLS_SETTING = 'legaleseAi.mcp.disabledTools'

/** globalState key holding OUR server list. */
const OWN_SERVERS_MEMENTO_KEY = 'legaleseAi.mcp.ownServers'

/** mcp.json keys owned by our own local L4 proxy (see McpProxy). */
const BUILTIN_SERVER_KEYS = new Set(['L4 Rules', 'l4-rules', 'l4-tools'])

export type McpServerStatus =
  | 'connected'
  | 'connecting'
  | 'error'
  | 'stopped'
  /** OAuth server without stored tokens — needs an interactive
   *  sign-in (menu → Start). */
  | 'unauthorized'

export interface VsCodeMcpToolInfo {
  name: string
  description?: string
  enabled: boolean
}

export interface VsCodeMcpServerInfo {
  id: string
  name: string
  transport?: string
  /** URL or command line — whatever identifies the server to a human. */
  detail?: string
  enabled: boolean
  status: McpServerStatus
  /** Human-readable connection failure, when status === 'error'. */
  error?: string
  tools: VsCodeMcpToolInfo[]
}

/** One mcp.json server offered for import (toggled off, not ours yet). */
export interface VsCodeMcpCandidateInfo {
  id: string
  name: string
  transport?: string
  detail?: string
}

export interface AddServerInput {
  name: string
  transport: 'http' | 'sse' | 'stdio'
  url?: string
  /** For stdio servers: full command line, split on whitespace. */
  command?: string
  /** Optional bearer token for http/sse servers — stored as an
   *  `Authorization: Bearer …` header on the entry. */
  bearerToken?: string
  /** Replace an existing entry with the same name instead of failing.
   *  Set by the UI after the user confirmed the override. */
  overwrite?: boolean
}

export type McpServerAction = 'start' | 'stop' | 'refresh' | 'remove'

/** Server config entry — same shape as VS Code's mcp.json entries so
 *  imports are verbatim copies. */
interface McpServerEntry {
  type?: string
  url?: string
  command?: string
  args?: string[]
  headers?: Record<string, string>
  [key: string]: unknown
}

interface McpToolDef {
  name: string
  description?: string
  inputSchema?: Record<string, unknown>
}

interface Connection {
  status: McpServerStatus
  error?: string
  tools: McpToolDef[]
  /** `Mcp-Session-Id` echoed back per the streamable-HTTP spec. */
  sessionId?: string
  /** OAuth access token in use (absent for static-header servers). */
  bearer?: string
  stdio?: StdioClient
}

/** JSON-RPC id counter, unique per process. */
let rpcIdCounter = 1

class HttpStatusError extends Error {
  constructor(
    message: string,
    readonly status: number,
    readonly wwwAuthenticate?: string
  ) {
    super(message)
  }
}

export class VsCodeMcpTools {
  private connections = new Map<string, Connection>()
  /** wire name → (server, tool). Rebuilt on every listTools(). */
  private wireMap = new Map<string, { serverId: string; toolName: string }>()

  constructor(
    /** VS Code per-user data dir containing `mcp.json` — the import
     *  catalog. Undefined when it couldn't be derived. */
    private readonly userDataPath: string | undefined,
    private readonly logger: AiLogger,
    /** Gate for auto-connecting: true when the user has a usable
     *  Legalese AI session (Cloud login or API key). */
    private readonly isAiUsable: () => boolean,
    /** Extension globalState — holds our server list. */
    private readonly memento: vscode.Memento,
    /** OAuth client for protected servers. */
    private readonly oauth?: McpOAuthManager
  ) {}

  dispose(): void {
    for (const id of [...this.connections.keys()]) this.stop(id)
  }

  // ── Our server list ───────────────────────────────────────────────

  private ownServers(): Record<string, McpServerEntry> {
    return (
      this.memento.get<Record<string, McpServerEntry>>(
        OWN_SERVERS_MEMENTO_KEY
      ) ?? {}
    )
  }

  private async saveOwnServers(
    next: Record<string, McpServerEntry>
  ): Promise<void> {
    await this.memento.update(OWN_SERVERS_MEMENTO_KEY, next)
  }

  // ── Master switch ─────────────────────────────────────────────────

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

  // ── Lifecycle ─────────────────────────────────────────────────────

  /**
   * Connect every enabled server that isn't already connected. Called
   * on activation and whenever auth state changes; never triggers an
   * interactive OAuth sign-in.
   */
  async autoStart(): Promise<void> {
    if (!this.isAiUsable()) return
    if (!this.allEnabled()) return
    const disabled = getDisabledServers()
    const jobs: Promise<unknown>[] = []
    for (const name of Object.keys(this.ownServers())) {
      if (disabled.has(name)) continue
      const current = this.connections.get(name)
      if (current?.status === 'connected' || current?.status === 'connecting')
        continue
      jobs.push(this.start(name, { interactive: false }))
    }
    await Promise.allSettled(jobs)
  }

  /**
   * Connect one of our servers. `interactive` allows a browser OAuth
   * sign-in when the server demands it; background starts report
   * `unauthorized` instead.
   */
  async start(
    id: string,
    opts: { interactive: boolean }
  ): Promise<{ ok: boolean; error?: string }> {
    const entry = this.ownServers()[id]
    if (!entry) return { ok: false, error: `Unknown server: ${id}` }
    this.stopStdio(id)
    const conn: Connection = { status: 'connecting', tools: [] }
    this.connections.set(id, conn)
    try {
      if (isHttpEntry(entry)) {
        await this.connectHttp(id, entry, conn, opts.interactive)
      } else if (typeof entry.command === 'string') {
        await this.connectStdio(id, entry, conn)
      } else {
        throw new Error('Entry has neither a url nor a command.')
      }
      conn.status = 'connected'
      conn.error = undefined
      this.logger.info(
        `vscode-mcp: connected to "${id}" (${conn.tools.length} tools)`
      )
      return { ok: true }
    } catch (err) {
      if (err instanceof SignInRequired) {
        conn.status = 'unauthorized'
        conn.error =
          'Sign-in required — choose Start in the server menu to authorize.'
        return { ok: false, error: conn.error }
      }
      const msg = err instanceof Error ? err.message : String(err)
      conn.status = 'error'
      conn.error = msg
      conn.tools = []
      this.stopStdio(id)
      this.logger.warn(`vscode-mcp: connect "${id}" failed: ${msg}`)
      return { ok: false, error: msg }
    }
  }

  stop(id: string): void {
    this.stopStdio(id)
    this.connections.set(id, { status: 'stopped', tools: [] })
  }

  private stopStdio(id: string): void {
    this.connections.get(id)?.stdio?.kill()
  }

  /** Dispatch a three-dot-menu action from the settings UI. */
  async runAction(
    id: string,
    action: McpServerAction
  ): Promise<{ ok: boolean; error?: string }> {
    switch (action) {
      case 'start':
      case 'refresh':
        return this.start(id, { interactive: true })
      case 'stop':
        this.stop(id)
        return { ok: true }
      case 'remove':
        return this.removeServer(id)
    }
  }

  // ── HTTP transport (with OAuth) ───────────────────────────────────

  private async connectHttp(
    id: string,
    entry: McpServerEntry,
    conn: Connection,
    interactive: boolean
  ): Promise<void> {
    const url = entry.url as string
    const hasStaticAuth = Boolean(entry.headers?.['Authorization'])
    if (!hasStaticAuth && this.oauth) {
      conn.bearer = (await this.oauth.getAccessToken(url)) ?? undefined
    }
    try {
      await this.httpHandshake(url, entry, conn)
      return
    } catch (err) {
      const unauthorized =
        err instanceof HttpStatusError &&
        (err.status === 401 || err.status === 403)
      if (!unauthorized || hasStaticAuth || !this.oauth) throw err
      if (!interactive) throw new SignInRequired()
      // Interactive path: run the full browser authorization, then
      // retry the handshake once with the fresh token.
      conn.bearer = await this.oauth.authorize(url, err.wwwAuthenticate)
      conn.sessionId = undefined
      await this.httpHandshake(url, entry, conn)
    }
  }

  private async httpHandshake(
    url: string,
    entry: McpServerEntry,
    conn: Connection
  ): Promise<void> {
    await this.rpc(url, entry, conn, 'initialize', {
      protocolVersion: '2025-03-26',
      capabilities: { tools: {} },
      clientInfo: { name: 'legalese-ai-chat', version: '1.6.0' },
    })
    void this.rpc(url, entry, conn, 'notifications/initialized', {}).catch(
      () => undefined
    )
    const listed = await this.rpc<{ tools?: McpToolDef[] }>(
      url,
      entry,
      conn,
      'tools/list',
      {}
    )
    conn.tools = listed?.tools ?? []
  }

  private async rpc<T>(
    url: string,
    entry: McpServerEntry,
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
        ...(conn.bearer ? { Authorization: `Bearer ${conn.bearer}` } : {}),
        ...(entry.headers ?? {}),
      },
      body: JSON.stringify({ jsonrpc: '2.0', id, method, params }),
      signal: AbortSignal.timeout(20_000),
    })
    const sessionId = res.headers.get('Mcp-Session-Id')
    if (sessionId) conn.sessionId = sessionId
    if (!res.ok) {
      const preview = await res.text().catch(() => '')
      throw new HttpStatusError(
        `${method} returned HTTP ${res.status}${preview ? `: ${preview.slice(0, 200)}` : ''}`,
        res.status,
        res.headers.get('WWW-Authenticate') ?? undefined
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

  // ── stdio transport ───────────────────────────────────────────────

  private async connectStdio(
    id: string,
    entry: McpServerEntry,
    conn: Connection
  ): Promise<void> {
    const client = new StdioClient(
      entry.command as string,
      Array.isArray(entry.args) ? entry.args.map(String) : [],
      this.logger,
      (reason) => {
        // Unexpected exit while we considered it connected.
        const current = this.connections.get(id)
        if (current === conn && conn.status === 'connected') {
          conn.status = 'error'
          conn.error = reason
          conn.tools = []
        }
      }
    )
    conn.stdio = client
    client.start()
    await client.request('initialize', {
      protocolVersion: '2025-03-26',
      capabilities: { tools: {} },
      clientInfo: { name: 'legalese-ai-chat', version: '1.6.0' },
    })
    client.notify('notifications/initialized', {})
    const listed = await client.request<{ tools?: McpToolDef[] }>(
      'tools/list',
      {}
    )
    conn.tools = listed?.tools ?? []
  }

  // ── Enablement ────────────────────────────────────────────────────

  /** Toggle a server. Enabling connects it right away (interactive —
   *  the user just asked for it); disabling drops the connection. */
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
      if (this.isAiUsable()) void this.start(id, { interactive: true })
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

  // ── Listing for the settings UI ───────────────────────────────────

  listServers(): VsCodeMcpServerInfo[] {
    const disabledServers = getDisabledServers()
    const disabledTools = getDisabledTools()
    const out: VsCodeMcpServerInfo[] = []
    for (const [name, entry] of Object.entries(this.ownServers())) {
      const conn = this.connections.get(name)
      out.push({
        id: name,
        name,
        transport: entry.type ?? (entry.command ? 'stdio' : 'http'),
        detail: describeEntry(entry),
        enabled: !disabledServers.has(name),
        status: conn?.status ?? 'stopped',
        error: conn?.error,
        tools: (conn?.tools ?? []).map((t) => ({
          name: t.name,
          description: t.description,
          enabled: !disabledTools.has(toolKey(name, t.name)),
        })),
      })
    }
    return out
  }

  /**
   * mcp.json servers not (yet) in our list — the import catalog shown
   * below our own servers. A candidate disappears once a same-named or
   * same-target server exists in our list.
   */
  listCandidates(): VsCodeMcpCandidateInfo[] {
    const own = this.ownServers()
    const ownTargets = new Set(
      Object.values(own)
        .map((e) => e.url ?? e.command)
        .filter(Boolean)
    )
    const out: VsCodeMcpCandidateInfo[] = []
    for (const [key, entry] of Object.entries(this.readVsCodeMcpJson())) {
      if (BUILTIN_SERVER_KEYS.has(key)) continue
      if (own[key]) continue
      const target = entry.url ?? entry.command
      if (target && ownTargets.has(target)) continue
      out.push({
        id: key,
        name: key,
        transport: entry.type ?? (entry.command ? 'stdio' : 'http'),
        detail: describeEntry(entry),
      })
    }
    return out
  }

  /** Copy an mcp.json server into our list and start it. The original
   *  entry is left untouched — from here on we run our own copy. */
  async importCandidate(id: string): Promise<{ ok: boolean; error?: string }> {
    const entry = this.readVsCodeMcpJson()[id]
    if (!entry) return { ok: false, error: `Unknown server: ${id}` }
    if (BUILTIN_SERVER_KEYS.has(id)) {
      return { ok: false, error: 'The built-in L4 server is always wired.' }
    }
    const own = this.ownServers()
    if (!own[id]) {
      await this.saveOwnServers({ ...own, [id]: entry })
      this.logger.info(`vscode-mcp: imported "${id}" from VS Code mcp.json`)
    }
    // The import gesture is an explicit enable.
    const disabled = getDisabledServers()
    if (disabled.has(id)) {
      disabled.delete(id)
      await vscode.workspace
        .getConfiguration()
        .update(
          DISABLED_SERVERS_SETTING,
          [...disabled].sort(),
          vscode.ConfigurationTarget.Global
        )
    }
    if (!this.isAiUsable()) return { ok: true }
    return this.start(id, { interactive: true })
  }

  // ── Tool funnel for the chat loop ─────────────────────────────────

  /** Advertise the enabled servers' enabled tools in OpenAI
   *  function-tool shape. */
  listTools(): AiProxyTool[] {
    this.wireMap.clear()
    if (!this.allEnabled()) return []
    const disabledServers = getDisabledServers()
    const disabledTools = getDisabledTools()
    const out: AiProxyTool[] = []
    for (const [serverId, conn] of this.connections) {
      if (conn.status !== 'connected') continue
      if (disabledServers.has(serverId)) continue
      for (const t of conn.tools) {
        if (disabledTools.has(toolKey(serverId, t.name))) continue
        const wireName = directWireName(serverId, t.name)
        if (this.wireMap.has(wireName)) continue
        this.wireMap.set(wireName, { serverId, toolName: t.name })
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
    return out
  }

  /** Execute a `vsmcp__…` call against the owning connection. */
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

    const { serverId, toolName } = target
    const entry = this.ownServers()[serverId]
    const conn = this.connections.get(serverId)
    if (!entry || conn?.status !== 'connected') {
      throw new Error(
        `MCP server ${serverId} is not connected — toggle it on in the Legalese AI settings.`
      )
    }

    let res: {
      content?: Array<{ type: string; text?: string }>
      isError?: boolean
    } | null
    if (conn.stdio) {
      res = await conn.stdio.request('tools/call', {
        name: toolName,
        arguments: args,
      })
    } else {
      const url = entry.url as string
      try {
        res = await this.rpc(url, entry, conn, 'tools/call', {
          name: toolName,
          arguments: args,
        })
      } catch (err) {
        // Token may have expired mid-session: refresh silently once.
        if (
          err instanceof HttpStatusError &&
          err.status === 401 &&
          conn.bearer &&
          this.oauth
        ) {
          const fresh = await this.oauth.getAccessToken(url)
          if (!fresh) {
            conn.status = 'unauthorized'
            throw new Error(
              `Sign-in to ${serverId} expired — restart it from the Legalese AI settings.`
            )
          }
          conn.bearer = fresh
          res = await this.rpc(url, entry, conn, 'tools/call', {
            name: toolName,
            arguments: args,
          })
        } else {
          throw err
        }
      }
    }
    const text = (res?.content ?? [])
      .map((c) => (c.type === 'text' && c.text ? c.text : JSON.stringify(c)))
      .join('\n')
    if (res?.isError) {
      throw new Error(text || `${wireName} failed`)
    }
    return text || JSON.stringify(res ?? {})
  }

  // ── Add / remove ──────────────────────────────────────────────────

  /**
   * Add a server to OUR list and connect it. A name collision returns
   * `exists: true` so the settings UI can offer to replace the entry
   * (confirmed resubmit carries `overwrite: true`).
   */
  async addServer(
    input: AddServerInput
  ): Promise<{ ok: boolean; error?: string; exists?: boolean }> {
    const name = input.name.trim()
    if (!name) return { ok: false, error: 'Server name is required.' }
    if (BUILTIN_SERVER_KEYS.has(name)) {
      return {
        ok: false,
        error: `"${name}" is reserved for the built-in L4 server.`,
      }
    }
    let entry: McpServerEntry
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

    const own = this.ownServers()
    if (own[name] && !input.overwrite) {
      return {
        ok: false,
        exists: true,
        error: `A server named "${name}" already exists.`,
      }
    }
    const replacing = Boolean(own[name])
    if (replacing) this.stop(name)
    await this.saveOwnServers({ ...own, [name]: entry })
    this.logger.info(
      `vscode-mcp: ${replacing ? 'replaced' : 'added'} server "${name}"`
    )
    if (!this.isAiUsable()) return { ok: true }
    return this.start(name, { interactive: true })
  }

  /** Remove a server from OUR list (mcp.json is never touched) and
   *  forget any OAuth tokens we hold for it. */
  private removeServer(id: string): { ok: boolean; error?: string } {
    const own = this.ownServers()
    const entry = own[id]
    if (!entry) return { ok: false, error: `Unknown server: ${id}` }
    this.stop(id)
    this.connections.delete(id)
    const next = { ...own }
    delete next[id]
    void this.saveOwnServers(next)
    if (isHttpEntry(entry) && this.oauth) {
      void this.oauth.clearAuth(entry.url as string)
    }
    this.logger.info(`vscode-mcp: removed server "${id}"`)
    return { ok: true }
  }

  // ── VS Code mcp.json (import catalog only) ────────────────────────

  private readVsCodeMcpJson(): Record<string, McpServerEntry> {
    const out: Record<string, McpServerEntry> = {}
    const addFrom = (filePath: string): void => {
      for (const [key, entry] of Object.entries(
        readMcpJsonServers(filePath, this.logger)
      )) {
        if (!(key in out)) out[key] = entry
      }
    }
    if (this.userDataPath) {
      addFrom(path.join(this.userDataPath, 'mcp.json'))
    }
    for (const folder of vscode.workspace.workspaceFolders ?? []) {
      addFrom(path.join(folder.uri.fsPath, '.vscode', 'mcp.json'))
    }
    return out
  }
}

/** Thrown internally when a background connect hits an OAuth wall. */
class SignInRequired extends Error {
  constructor() {
    super('Sign-in required')
  }
}

// ── stdio client ──────────────────────────────────────────────────────

/**
 * Minimal MCP stdio transport: newline-delimited JSON-RPC over a
 * spawned child process's stdin/stdout. stderr is logged.
 */
class StdioClient {
  private proc: ChildProcess | null = null
  private buffer = ''
  private pending = new Map<
    number,
    {
      resolve: (v: unknown) => void
      reject: (e: Error) => void
      timer: NodeJS.Timeout
    }
  >()

  constructor(
    private readonly command: string,
    private readonly args: string[],
    private readonly logger: AiLogger,
    private readonly onUnexpectedExit: (reason: string) => void
  ) {}

  start(): void {
    const cwd =
      vscode.workspace.workspaceFolders?.[0]?.uri.fsPath ?? os.homedir()
    this.proc = spawn(this.command, this.args, {
      cwd,
      env: process.env,
      stdio: ['pipe', 'pipe', 'pipe'],
      // Through a shell so PATH-resolved commands (`npx`, `uvx`, …)
      // behave the way they do in the user's terminal, incl. Windows.
      shell: true,
    })
    this.proc.stdout?.setEncoding('utf-8')
    this.proc.stdout?.on('data', (chunk: string) => this.onData(chunk))
    this.proc.stderr?.setEncoding('utf-8')
    this.proc.stderr?.on('data', (chunk: string) => {
      const line = chunk.trim()
      if (line) this.logger.info(`mcp-stdio[${this.command}]: ${line}`)
    })
    this.proc.on('exit', (code) => {
      const reason = `Server process exited (code ${code ?? 'unknown'}).`
      for (const [, p] of this.pending) {
        clearTimeout(p.timer)
        p.reject(new Error(reason))
      }
      this.pending.clear()
      this.onUnexpectedExit(reason)
    })
    this.proc.on('error', (err) => {
      const reason = `Failed to launch "${this.command}": ${err.message}`
      for (const [, p] of this.pending) {
        clearTimeout(p.timer)
        p.reject(new Error(reason))
      }
      this.pending.clear()
      this.onUnexpectedExit(reason)
    })
  }

  async request<T>(
    method: string,
    params: Record<string, unknown>,
    timeoutMs = 30_000
  ): Promise<T | null> {
    if (!this.proc?.stdin?.writable) {
      throw new Error('Server process is not running.')
    }
    const id = rpcIdCounter++
    const payload =
      JSON.stringify({ jsonrpc: '2.0', id, method, params }) + '\n'
    return new Promise<T | null>((resolve, reject) => {
      const timer = setTimeout(() => {
        this.pending.delete(id)
        reject(new Error(`${method} timed out after ${timeoutMs / 1000}s`))
      }, timeoutMs)
      this.pending.set(id, {
        resolve: resolve as (v: unknown) => void,
        reject,
        timer,
      })
      this.proc!.stdin!.write(payload)
    })
  }

  notify(method: string, params: Record<string, unknown>): void {
    if (!this.proc?.stdin?.writable) return
    this.proc.stdin.write(
      JSON.stringify({ jsonrpc: '2.0', method, params }) + '\n'
    )
  }

  kill(): void {
    if (!this.proc) return
    try {
      this.proc.removeAllListeners('exit')
      this.proc.kill()
    } catch {
      // already gone
    }
    this.proc = null
    for (const [, p] of this.pending) {
      clearTimeout(p.timer)
      p.reject(new Error('Server stopped.'))
    }
    this.pending.clear()
  }

  private onData(chunk: string): void {
    this.buffer += chunk
    let idx: number
    while ((idx = this.buffer.indexOf('\n')) >= 0) {
      const line = this.buffer.slice(0, idx).trim()
      this.buffer = this.buffer.slice(idx + 1)
      if (!line) continue
      try {
        const msg = JSON.parse(line) as {
          id?: number
          result?: unknown
          error?: { message?: string }
        }
        if (typeof msg.id !== 'number') continue // server notification
        const p = this.pending.get(msg.id)
        if (!p) continue
        this.pending.delete(msg.id)
        clearTimeout(p.timer)
        if (msg.error) {
          p.reject(new Error(msg.error.message ?? 'MCP request failed'))
        } else {
          p.resolve(msg.result ?? null)
        }
      } catch {
        // Non-JSON noise on stdout (some servers print banners) — skip.
      }
    }
  }
}

// ── helpers ───────────────────────────────────────────────────────────

function isHttpEntry(entry: McpServerEntry): boolean {
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
): Record<string, McpServerEntry> {
  let raw: string
  try {
    raw = fs.readFileSync(filePath, 'utf-8')
  } catch {
    return {}
  }
  if (!raw.trim()) return {}
  try {
    const parsed = JSON.parse(raw) as {
      servers?: Record<string, McpServerEntry>
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

function describeEntry(entry: McpServerEntry): string | undefined {
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

/** OpenAI function names allow `[a-zA-Z0-9_-]{1,64}`. */
function sanitizeToolName(s: string): string {
  return s.replace(/[^a-zA-Z0-9_-]/g, '_')
}

/** Wire name for a server's tool. Uniqueness comes from the wireMap
 *  dedupe; the name only needs to be stable. */
function directWireName(serverId: string, toolName: string): string {
  const server = sanitizeToolName(serverId).slice(0, 16)
  const tool = sanitizeToolName(toolName).slice(0, 40)
  return `${VSCODE_MCP_PREFIX}${server}_${tool}`.slice(0, 64)
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
