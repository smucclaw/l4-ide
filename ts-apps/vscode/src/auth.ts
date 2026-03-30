import * as vscode from 'vscode'

export type ConnectionStatus =
  | 'connected'
  | 'not-configured'
  | 'connecting'
  | 'error'

export interface ConnectionState {
  status: ConnectionStatus
  serviceUrl: string
  connected: boolean
  error?: string
}

const LEGALESE_CLOUD_DOMAIN = 'legalese.cloud'
const SECRET_KEY_SESSION = 'l4.sessionToken'

/**
 * Manages authentication state for connecting to jl4-service / legalese.cloud.
 *
 * Two modes:
 * 1. Self-hosted: User configures jl4.serviceUrl + optional jl4.serviceApiKey in settings
 * 2. Legalese Cloud: Browser-based login via WorkOS, token stored in SecretStorage
 */
export class AuthManager {
  private readonly onDidChangeEmitter =
    new vscode.EventEmitter<ConnectionState>()
  readonly onDidChange = this.onDidChangeEmitter.event

  private cachedState: ConnectionState | undefined
  private manuallyDisconnected = false
  private cloudOrgSlug: string | undefined
  private readonly disposables: vscode.Disposable[] = []

  constructor(
    private readonly secrets: vscode.SecretStorage,
    private readonly outputChannel: vscode.OutputChannel
  ) {
    this.disposables.push(
      vscode.workspace.onDidChangeConfiguration(async (e) => {
        if (
          e.affectsConfiguration('jl4.serviceUrl') ||
          e.affectsConfiguration('jl4.serviceApiKey')
        ) {
          this.outputChannel.appendLine(
            '[auth] Settings changed, disconnecting and refreshing connection state'
          )
          await this.secrets.delete(SECRET_KEY_SESSION)
          this.cloudOrgSlug = undefined
          this.manuallyDisconnected = false
          this.cachedState = undefined
          // If a service URL is now set, attempt to connect automatically
          if (this.getServiceUrl()) {
            await this.verifyConnection()
          } else {
            this.invalidateAndNotify()
          }
        }
      })
    )
  }

  getServiceUrl(): string {
    return (
      vscode.workspace.getConfiguration('jl4').get<string>('serviceUrl') ?? ''
    )
  }

  private getApiKeyFromSettings(): string {
    return (
      vscode.workspace.getConfiguration('jl4').get<string>('serviceApiKey') ??
      ''
    )
  }

  /**
   * Whether the user is in Legalese Cloud mode (no serviceUrl configured,
   * authenticated via browser login with a session token).
   * A configured serviceUrl (even *.legalese.cloud) is always "self-hosted" mode
   * and uses API key auth, not session auth.
   */
  isLegaleseCloudSession(): boolean {
    return !this.getServiceUrl()
  }

  async getAuthHeaders(): Promise<Record<string, string>> {
    // Use API key if explicitly configured in settings
    const apiKey = this.getApiKeyFromSettings()
    if (apiKey) return { Authorization: `Bearer ${apiKey}` }

    // Otherwise use session token (for Legalese Cloud browser login)
    const session = await this.secrets.get(SECRET_KEY_SESSION)
    if (session) return { Authorization: `Bearer ${session}` }

    return {}
  }

  /**
   * The effective service URL.
   * If serviceUrl is configured in settings, use that.
   * If in Legalese Cloud mode with an org slug, derive from the slug.
   */
  getEffectiveServiceUrl(): string {
    const configured = this.getServiceUrl()
    if (configured) return configured
    if (this.cloudOrgSlug) {
      return `https://${this.cloudOrgSlug}.${LEGALESE_CLOUD_DOMAIN}`
    }
    return ''
  }

  async getSessionToken(): Promise<string | undefined> {
    return this.secrets.get(SECRET_KEY_SESSION)
  }

  async setSessionToken(token: string): Promise<void> {
    await this.secrets.store(SECRET_KEY_SESSION, token)
    this.manuallyDisconnected = false
    this.cachedState = undefined
  }

  /**
   * Sign out (Legalese Cloud) or disconnect (configured service URL).
   * Clears session token. Never touches the serviceUrl setting.
   */
  async logout(): Promise<void> {
    await this.secrets.delete(SECRET_KEY_SESSION)
    this.cloudOrgSlug = undefined
    this.manuallyDisconnected = true
    this.cachedState = undefined
    const state = await this.getConnectionState()
    this.onDidChangeEmitter.fire(state)
  }

  /**
   * Connect or sign in.
   * If serviceUrl is configured: verify connectivity.
   * If no serviceUrl: open Legalese Cloud browser login.
   */
  async login(): Promise<void> {
    const serviceUrl = this.getServiceUrl()

    if (serviceUrl) {
      this.manuallyDisconnected = false
      this.cachedState = undefined
      await this.verifyConnection()
      return
    }

    // No service URL → Legalese Cloud browser login
    const callbackUri = await vscode.env.asExternalUri(
      vscode.Uri.parse(
        `${vscode.env.uriScheme}://legalese.l4-vscode/auth-callback`
      )
    )
    const loginUrl = `https://${LEGALESE_CLOUD_DOMAIN}/auth/login?return_to=${encodeURIComponent(callbackUri.toString())}`

    this.outputChannel.appendLine(`[auth] Opening login: ${loginUrl}`)
    await vscode.env.openExternal(vscode.Uri.parse(loginUrl))
  }

  async handleAuthCallback(uri: vscode.Uri): Promise<void> {
    const params = new URLSearchParams(uri.query)
    const token = params.get('token')

    if (!token) {
      this.outputChannel.appendLine(
        `[auth] Callback missing token: ${uri.toString()}`
      )
      vscode.window.showErrorMessage('Login failed: no token received.')
      return
    }

    this.outputChannel.appendLine('[auth] Received session token from callback')
    await this.setSessionToken(token)

    // Resolve the org slug, then verify the connection.
    // Success/failure is communicated via the sidebar status — no VS Code notifications.
    try {
      const resp = await fetch(
        `https://${LEGALESE_CLOUD_DOMAIN}/auth/session`,
        {
          headers: { Authorization: `Bearer ${token}` },
          signal: AbortSignal.timeout(10000),
        }
      )
      if (resp.ok) {
        const session = (await resp.json()) as {
          organization?: { slug: string; name: string }
        }
        if (session.organization) {
          this.cloudOrgSlug = session.organization.slug
          this.outputChannel.appendLine(
            `[auth] Resolved org: ${session.organization.name} (${session.organization.slug})`
          )
        }
      }
    } catch (err) {
      this.outputChannel.appendLine(
        `[auth] Failed to fetch session: ${err instanceof Error ? err.message : String(err)}`
      )
    }

    await this.verifyConnection()
  }

  async getConnectionState(): Promise<ConnectionState> {
    if (this.cachedState) return this.cachedState

    const serviceUrl = this.getEffectiveServiceUrl()

    this.outputChannel.appendLine(
      `[auth] getConnectionState: serviceUrl=${JSON.stringify(serviceUrl)} disconnected=${this.manuallyDisconnected}`
    )

    let state: ConnectionState

    if (this.manuallyDisconnected || !serviceUrl) {
      state = {
        status: 'not-configured',
        serviceUrl,
        connected: false,
      }
    } else {
      // Have a URL but haven't verified yet — report as connecting
      state = {
        status: 'connecting',
        serviceUrl,
        connected: false,
      }
    }

    this.cachedState = state
    return state
  }

  /**
   * Verify the connection by hitting /service/health.
   * Only returns 'connected' if that request succeeds with a 200.
   */
  async verifyConnection(): Promise<ConnectionState> {
    this.cachedState = undefined
    const serviceUrl = this.getEffectiveServiceUrl()

    if (this.manuallyDisconnected || !serviceUrl) {
      const state: ConnectionState = {
        status: 'not-configured',
        serviceUrl,
        connected: false,
      }
      this.cachedState = state
      this.onDidChangeEmitter.fire(state)
      return state
    }

    // Emit connecting state while we verify
    const connectingState: ConnectionState = {
      status: 'connecting',
      serviceUrl,
      connected: false,
    }
    this.cachedState = connectingState
    this.onDidChangeEmitter.fire(connectingState)

    // Run health check and a minimum delay in parallel so the
    // "Connecting..." state is visible for at least 1 second.
    const minDelay = new Promise((r) => setTimeout(r, 1000))

    let result: ConnectionState
    try {
      const headers = await this.getAuthHeaders()
      const [resp] = await Promise.all([
        fetch(`${serviceUrl.replace(/\/$/, '')}/service/health`, {
          headers,
          signal: AbortSignal.timeout(5000),
        }),
        minDelay,
      ])

      if (resp.ok) {
        result = { status: 'connected', serviceUrl, connected: true }
      } else if (resp.status === 401 || resp.status === 403) {
        result = {
          status: 'error',
          serviceUrl,
          connected: false,
          error: 'Authentication failed. Check your API key or re-login.',
        }
      } else {
        result = {
          status: 'error',
          serviceUrl,
          connected: false,
          error: `Service responded with ${resp.status}`,
        }
      }
    } catch {
      await minDelay
      result = {
        status: 'error',
        serviceUrl,
        connected: false,
        error: `Could not connect to ${serviceUrl}`,
      }
    }

    this.cachedState = result
    this.onDidChangeEmitter.fire(result)
    return result
  }

  /**
   * Auto-connect on startup.
   * If a service URL is configured, verify connectivity.
   * If in Legalese Cloud mode with a stored session token, resolve the org and verify.
   */
  async initialize(): Promise<void> {
    const serviceUrl = this.getServiceUrl()

    if (serviceUrl) {
      this.outputChannel.appendLine(
        '[auth] Service URL configured, attempting auto-connect'
      )
      await this.verifyConnection()
      return
    }

    // Legalese Cloud: check for existing session token
    const token = await this.secrets.get(SECRET_KEY_SESSION)
    if (token) {
      this.outputChannel.appendLine(
        '[auth] Found stored session token, verifying Legalese Cloud session'
      )
      try {
        const resp = await fetch(
          `https://${LEGALESE_CLOUD_DOMAIN}/auth/session`,
          {
            headers: { Authorization: `Bearer ${token}` },
            signal: AbortSignal.timeout(10000),
          }
        )
        if (resp.ok) {
          const session = (await resp.json()) as {
            organization?: { slug: string; name: string }
          }
          if (session.organization) {
            this.cloudOrgSlug = session.organization.slug
            this.outputChannel.appendLine(
              `[auth] Restored org: ${session.organization.name} (${session.organization.slug})`
            )
          }
        } else if (resp.status === 401 || resp.status === 403) {
          this.outputChannel.appendLine(
            '[auth] Stored session token is invalid, clearing'
          )
          await this.secrets.delete(SECRET_KEY_SESSION)
          this.invalidateAndNotify()
          return
        }
      } catch (err) {
        this.outputChannel.appendLine(
          `[auth] Failed to verify session: ${err instanceof Error ? err.message : String(err)}`
        )
      }

      // If we resolved an org slug, verify by fetching deployments
      if (this.cloudOrgSlug) {
        await this.verifyConnection()
      }
    }
  }

  private invalidateAndNotify() {
    this.cachedState = undefined
    this.getConnectionState().then((state) => {
      this.onDidChangeEmitter.fire(state)
    })
  }

  dispose() {
    this.onDidChangeEmitter.dispose()
    for (const d of this.disposables) d.dispose()
  }
}
