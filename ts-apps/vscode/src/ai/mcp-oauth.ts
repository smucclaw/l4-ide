import * as crypto from 'node:crypto'
import * as vscode from 'vscode'
import type { AiLogger } from './logger.js'

/**
 * OAuth client for MCP servers, per the MCP authorization spec:
 *
 *   1. A 401 from the server carries (or implies) an RFC 9728
 *      protected-resource-metadata URL → tells us the authorization
 *      server.
 *   2. RFC 8414 authorization-server metadata → endpoints.
 *   3. RFC 7591 dynamic client registration (public client, PKCE).
 *   4. Authorization-code flow in the external browser; the redirect
 *      comes back through the extension's `vscode://` URI handler.
 *   5. Tokens live in SecretStorage, refreshed silently on expiry.
 *
 * One instance serves all servers; tokens and client registrations are
 * keyed by the server URL.
 */

interface TokenSet {
  accessToken: string
  refreshToken?: string
  /** Epoch ms after which accessToken is stale (with safety margin). */
  expiresAt?: number
  clientId: string
  tokenEndpoint: string
  /** RFC 8707 resource indicator sent on token requests. */
  resource: string
}

interface AuthServerMetadata {
  authorization_endpoint?: string
  token_endpoint?: string
  registration_endpoint?: string
  code_challenge_methods_supported?: string[]
  scopes_supported?: string[]
}

interface PendingFlow {
  resolve: (tokens: TokenSet) => void
  reject: (err: Error) => void
  verifier: string
  clientId: string
  tokenEndpoint: string
  resource: string
  timeout: NodeJS.Timeout
}

const FLOW_TIMEOUT_MS = 5 * 60 * 1000

export class McpOAuthManager {
  /** state → in-flight browser flow. */
  private pending = new Map<string, PendingFlow>()

  constructor(
    private readonly secrets: vscode.SecretStorage,
    private readonly logger: AiLogger,
    /** Deep link the browser redirects back to, e.g.
     *  `vscode://legalese.l4-vscode/mcp-oauth`. */
    private readonly redirectUri: string
  ) {}

  // ── Public API ────────────────────────────────────────────────────

  /** True when we hold (possibly stale but refreshable) auth for the
   *  server — i.e. connecting silently is worth attempting. */
  async hasAuth(serverUrl: string): Promise<boolean> {
    return (await this.loadTokens(serverUrl)) !== null
  }

  /**
   * Valid access token for the server, refreshing silently when
   * expired. Null when there is no stored auth or the refresh fails
   * (→ an interactive {@link authorize} is needed).
   */
  async getAccessToken(serverUrl: string): Promise<string | null> {
    const tokens = await this.loadTokens(serverUrl)
    if (!tokens) return null
    if (!tokens.expiresAt || tokens.expiresAt > Date.now()) {
      return tokens.accessToken
    }
    return this.refresh(serverUrl, tokens)
  }

  /**
   * Interactive authorization: discovery → client registration →
   * browser consent → code exchange. Resolves with a fresh access
   * token; rejects on timeout/denial. `wwwAuthenticate` is the 401's
   * challenge header, used to locate the resource metadata.
   */
  async authorize(
    serverUrl: string,
    wwwAuthenticate?: string
  ): Promise<string> {
    const meta = await this.discover(serverUrl, wwwAuthenticate)
    if (!meta.authorization_endpoint || !meta.token_endpoint) {
      throw new Error(
        'The server requires sign-in but its authorization server publishes no authorization/token endpoints.'
      )
    }
    const clientId = await this.obtainClientId(serverUrl, meta)
    const verifier = base64url(crypto.randomBytes(32))
    const challenge = base64url(
      crypto.createHash('sha256').update(verifier).digest()
    )
    const state = base64url(crypto.randomBytes(16))
    const resource = canonicalResource(serverUrl)

    const authUrl = new URL(meta.authorization_endpoint)
    authUrl.searchParams.set('response_type', 'code')
    authUrl.searchParams.set('client_id', clientId)
    authUrl.searchParams.set('redirect_uri', this.redirectUri)
    authUrl.searchParams.set('code_challenge', challenge)
    authUrl.searchParams.set('code_challenge_method', 'S256')
    authUrl.searchParams.set('state', state)
    authUrl.searchParams.set('resource', resource)
    if (meta.scopes_supported?.length) {
      authUrl.searchParams.set('scope', meta.scopes_supported.join(' '))
    }

    const tokens = await new Promise<TokenSet>((resolve, reject) => {
      const timeout = setTimeout(() => {
        this.pending.delete(state)
        reject(new Error('Sign-in timed out — no browser callback received.'))
      }, FLOW_TIMEOUT_MS)
      this.pending.set(state, {
        resolve,
        reject,
        verifier,
        clientId,
        tokenEndpoint: meta.token_endpoint!,
        resource,
        timeout,
      })
      void vscode.env.openExternal(vscode.Uri.parse(authUrl.toString()))
      this.logger.info(`mcp-oauth: opened browser for ${serverUrl}`)
    })
    await this.storeTokens(serverUrl, tokens)
    return tokens.accessToken
  }

  /**
   * Route a `vscode://…/mcp-oauth?code=…&state=…` callback to its
   * pending flow. Returns true when the URI belonged to us.
   */
  handleCallback(uri: vscode.Uri): boolean {
    const params = new URLSearchParams(uri.query)
    const state = params.get('state')
    if (!state) return false
    const flow = this.pending.get(state)
    if (!flow) return false
    this.pending.delete(state)
    clearTimeout(flow.timeout)

    const error = params.get('error')
    const code = params.get('code')
    if (error || !code) {
      flow.reject(
        new Error(
          `Sign-in ${error ? `failed: ${error}` : 'returned no authorization code'}${
            params.get('error_description')
              ? ` (${params.get('error_description')})`
              : ''
          }`
        )
      )
      return true
    }
    void this.exchangeCode(flow, code).then(
      (tokens) => flow.resolve(tokens),
      (err) => flow.reject(err instanceof Error ? err : new Error(String(err)))
    )
    return true
  }

  /** Forget stored tokens + client registration for a server. */
  async clearAuth(serverUrl: string): Promise<void> {
    await this.secrets.delete(tokenKey(serverUrl))
    await this.secrets.delete(clientKey(serverUrl))
  }

  // ── Discovery ─────────────────────────────────────────────────────

  private async discover(
    serverUrl: string,
    wwwAuthenticate?: string
  ): Promise<AuthServerMetadata> {
    // RFC 9728: the challenge may name the metadata document; else the
    // well-known location (path-aware, then root) applies.
    const candidates: string[] = []
    const fromChallenge = wwwAuthenticate?.match(
      /resource_metadata="([^"]+)"/
    )?.[1]
    if (fromChallenge) candidates.push(fromChallenge)
    const u = new URL(serverUrl)
    if (u.pathname && u.pathname !== '/') {
      candidates.push(
        `${u.origin}/.well-known/oauth-protected-resource${u.pathname}`
      )
    }
    candidates.push(`${u.origin}/.well-known/oauth-protected-resource`)

    let issuer: string | undefined
    for (const url of candidates) {
      const prm = await fetchJson<{ authorization_servers?: string[] }>(url)
      if (prm?.authorization_servers?.length) {
        issuer = prm.authorization_servers[0]
        break
      }
    }
    // Some servers act as their own authorization server without
    // publishing resource metadata.
    if (!issuer) issuer = u.origin

    const i = new URL(issuer)
    const metaCandidates = [
      // RFC 8414 path-aware form, then root, then OIDC discovery.
      ...(i.pathname && i.pathname !== '/'
        ? [
            `${i.origin}/.well-known/oauth-authorization-server${i.pathname}`,
            `${i.origin}${i.pathname.replace(/\/$/, '')}/.well-known/openid-configuration`,
          ]
        : []),
      `${i.origin}/.well-known/oauth-authorization-server`,
      `${i.origin}/.well-known/openid-configuration`,
    ]
    for (const url of metaCandidates) {
      const meta = await fetchJson<AuthServerMetadata>(url)
      if (meta?.authorization_endpoint && meta.token_endpoint) return meta
    }
    throw new Error(
      `Could not discover the authorization server for ${serverUrl}.`
    )
  }

  private async obtainClientId(
    serverUrl: string,
    meta: AuthServerMetadata
  ): Promise<string> {
    const stored = await this.secrets.get(clientKey(serverUrl))
    if (stored) return stored
    if (!meta.registration_endpoint) {
      throw new Error(
        'The authorization server does not support dynamic client registration; a pre-registered client id would be required.'
      )
    }
    const res = await fetch(meta.registration_endpoint, {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({
        client_name: 'Legalese AI (VS Code)',
        redirect_uris: [this.redirectUri],
        grant_types: ['authorization_code', 'refresh_token'],
        response_types: ['code'],
        token_endpoint_auth_method: 'none',
      }),
      signal: AbortSignal.timeout(15_000),
    })
    if (!res.ok) {
      throw new Error(
        `Client registration failed (HTTP ${res.status}): ${(await res.text().catch(() => '')).slice(0, 200)}`
      )
    }
    const body = (await res.json()) as { client_id?: string }
    if (!body.client_id) {
      throw new Error('Client registration returned no client_id.')
    }
    await this.secrets.store(clientKey(serverUrl), body.client_id)
    return body.client_id
  }

  // ── Token handling ────────────────────────────────────────────────

  private async exchangeCode(
    flow: PendingFlow,
    code: string
  ): Promise<TokenSet> {
    const body = new URLSearchParams({
      grant_type: 'authorization_code',
      code,
      redirect_uri: this.redirectUri,
      client_id: flow.clientId,
      code_verifier: flow.verifier,
      resource: flow.resource,
    })
    const res = await fetch(flow.tokenEndpoint, {
      method: 'POST',
      headers: { 'Content-Type': 'application/x-www-form-urlencoded' },
      body: body.toString(),
      signal: AbortSignal.timeout(15_000),
    })
    if (!res.ok) {
      throw new Error(
        `Token exchange failed (HTTP ${res.status}): ${(await res.text().catch(() => '')).slice(0, 200)}`
      )
    }
    const t = (await res.json()) as {
      access_token?: string
      refresh_token?: string
      expires_in?: number
    }
    if (!t.access_token) throw new Error('Token exchange returned no token.')
    return {
      accessToken: t.access_token,
      refreshToken: t.refresh_token,
      expiresAt: expiryOf(t.expires_in),
      clientId: flow.clientId,
      tokenEndpoint: flow.tokenEndpoint,
      resource: flow.resource,
    }
  }

  private async refresh(
    serverUrl: string,
    tokens: TokenSet
  ): Promise<string | null> {
    if (!tokens.refreshToken) return null
    try {
      const body = new URLSearchParams({
        grant_type: 'refresh_token',
        refresh_token: tokens.refreshToken,
        client_id: tokens.clientId,
        resource: tokens.resource,
      })
      const res = await fetch(tokens.tokenEndpoint, {
        method: 'POST',
        headers: { 'Content-Type': 'application/x-www-form-urlencoded' },
        body: body.toString(),
        signal: AbortSignal.timeout(15_000),
      })
      if (!res.ok) {
        this.logger.warn(
          `mcp-oauth: refresh for ${serverUrl} failed (HTTP ${res.status})`
        )
        return null
      }
      const t = (await res.json()) as {
        access_token?: string
        refresh_token?: string
        expires_in?: number
      }
      if (!t.access_token) return null
      const next: TokenSet = {
        ...tokens,
        accessToken: t.access_token,
        refreshToken: t.refresh_token ?? tokens.refreshToken,
        expiresAt: expiryOf(t.expires_in),
      }
      await this.storeTokens(serverUrl, next)
      return next.accessToken
    } catch (err) {
      this.logger.warn(
        `mcp-oauth: refresh for ${serverUrl} failed: ${err instanceof Error ? err.message : String(err)}`
      )
      return null
    }
  }

  private async loadTokens(serverUrl: string): Promise<TokenSet | null> {
    const raw = await this.secrets.get(tokenKey(serverUrl))
    if (!raw) return null
    try {
      return JSON.parse(raw) as TokenSet
    } catch {
      return null
    }
  }

  private async storeTokens(
    serverUrl: string,
    tokens: TokenSet
  ): Promise<void> {
    await this.secrets.store(tokenKey(serverUrl), JSON.stringify(tokens))
  }
}

// ── helpers ───────────────────────────────────────────────────────────

function tokenKey(serverUrl: string): string {
  return `legaleseAi.mcpOAuth.tokens:${canonicalResource(serverUrl)}`
}

function clientKey(serverUrl: string): string {
  return `legaleseAi.mcpOAuth.client:${canonicalResource(serverUrl)}`
}

/** RFC 8707 canonical resource: scheme + host + path, no trailing `/`. */
function canonicalResource(serverUrl: string): string {
  const u = new URL(serverUrl)
  return `${u.origin}${u.pathname.replace(/\/$/, '')}`
}

function expiryOf(expiresIn: number | undefined): number | undefined {
  if (typeof expiresIn !== 'number' || expiresIn <= 0) return undefined
  // 60s safety margin so we refresh before the edge.
  return Date.now() + Math.max(0, expiresIn - 60) * 1000
}

function base64url(buf: Buffer): string {
  return buf.toString('base64url')
}

async function fetchJson<T>(url: string): Promise<T | null> {
  try {
    const res = await fetch(url, {
      headers: { Accept: 'application/json' },
      signal: AbortSignal.timeout(10_000),
    })
    if (!res.ok) return null
    return (await res.json()) as T
  } catch {
    return null
  }
}
