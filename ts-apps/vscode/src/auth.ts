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
  private readonly disposables: vscode.Disposable[] = []

  constructor(
    private readonly secrets: vscode.SecretStorage,
    private readonly outputChannel: vscode.OutputChannel
  ) {
    this.disposables.push(
      vscode.workspace.onDidChangeConfiguration((e) => {
        if (
          e.affectsConfiguration('jl4.serviceUrl') ||
          e.affectsConfiguration('jl4.serviceApiKey')
        ) {
          this.outputChannel.appendLine(
            '[auth] Settings changed, refreshing connection state'
          )
          this.manuallyDisconnected = false
          this.invalidateAndNotify()
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
   * Whether the user authenticated via Legalese Cloud browser login
   * (as opposed to a manually configured API key).
   */
  isLegaleseCloudSession(): boolean {
    if (this.getApiKeyFromSettings()) return false
    return true
  }

  async getAuthHeaders(): Promise<Record<string, string>> {
    const settingsKey = this.getApiKeyFromSettings()
    if (settingsKey) {
      return { Authorization: `Bearer ${settingsKey}` }
    }

    const session = await this.secrets.get(SECRET_KEY_SESSION)
    if (session) {
      return { Authorization: `Bearer ${session}` }
    }

    return {}
  }

  async getSessionToken(): Promise<string | undefined> {
    return this.secrets.get(SECRET_KEY_SESSION)
  }

  async setSessionToken(token: string): Promise<void> {
    await this.secrets.store(SECRET_KEY_SESSION, token)
    this.manuallyDisconnected = false
    this.invalidateAndNotify()
  }

  /**
   * Sign out / disconnect. Clears session token.
   * For legalese.cloud, also clears the auto-set serviceUrl.
   */
  async logout(): Promise<void> {
    const wasCloud = this.isLegaleseCloudSession()
    await this.secrets.delete(SECRET_KEY_SESSION)
    if (wasCloud) {
      await vscode.workspace
        .getConfiguration('jl4')
        .update('serviceUrl', undefined, vscode.ConfigurationTarget.Workspace)
    }
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
        if (session.organization?.slug) {
          const orgUrl = `https://${session.organization.slug}.${LEGALESE_CLOUD_DOMAIN}`
          this.outputChannel.appendLine(
            `[auth] Resolved org: ${session.organization.name} (${orgUrl})`
          )
          await vscode.workspace
            .getConfiguration('jl4')
            .update('serviceUrl', orgUrl, vscode.ConfigurationTarget.Workspace)
          this.invalidateAndNotify()
          vscode.window.showInformationMessage(
            `Signed in to ${session.organization.name} on Legalese Cloud.`
          )
          return
        }
      }
    } catch (err) {
      this.outputChannel.appendLine(
        `[auth] Failed to fetch session: ${err instanceof Error ? err.message : String(err)}`
      )
    }

    vscode.window.showInformationMessage(
      'Successfully signed in to Legalese Cloud.'
    )
  }

  async getConnectionState(): Promise<ConnectionState> {
    if (this.cachedState) return this.cachedState

    const serviceUrl = this.getServiceUrl()

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
      state = {
        status: 'connected',
        serviceUrl,
        connected: true,
      }
    }

    this.cachedState = state
    return state
  }

  async verifyConnection(): Promise<ConnectionState> {
    this.cachedState = undefined
    const state = await this.getConnectionState()
    if (!state.connected) return state

    try {
      const headers = await this.getAuthHeaders()
      const resp = await fetch(`${state.serviceUrl}/service/health`, {
        headers,
        signal: AbortSignal.timeout(5000),
      })

      if (resp.ok) {
        const verified: ConnectionState = { ...state, status: 'connected' }
        this.cachedState = verified
        this.onDidChangeEmitter.fire(verified)
        return verified
      }

      if (resp.status === 401 || resp.status === 403) {
        const errorState: ConnectionState = {
          ...state,
          status: 'error',
          connected: false,
          error: 'Authentication failed. Check your API key or re-login.',
        }
        this.cachedState = errorState
        this.onDidChangeEmitter.fire(errorState)
        return errorState
      }

      const errorState: ConnectionState = {
        ...state,
        status: 'error',
        connected: false,
        error: `Service responded with ${resp.status}`,
      }
      this.cachedState = errorState
      this.onDidChangeEmitter.fire(errorState)
      return errorState
    } catch (err) {
      const errorState: ConnectionState = {
        ...state,
        status: 'error',
        connected: false,
        error: err instanceof Error ? err.message : 'Connection failed',
      }
      this.cachedState = errorState
      this.onDidChangeEmitter.fire(errorState)
      return errorState
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
