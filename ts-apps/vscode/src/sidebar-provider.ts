import * as vscode from 'vscode'
import type { Messenger } from 'vscode-messenger'
import type { WebviewTypeMessageParticipant } from 'vscode-messenger-common'
import type { VSCodeL4LanguageClient } from './vscode-l4-language-client.js'
import { type AuthManager, LEGALESE_CLOUD_DOMAIN } from './auth.js'
import type { ServiceClient } from './service-client.js'
import { getWebviewContent } from './webview-panel.js'
import {
  GetExportedFunctionsRequestType,
  GetSidebarExportedFunctions,
  GetSidebarConnectionStatus,
  RequestSidebarLogin,
  RequestSidebarLogout,
  ListSidebarDeployments,
  RequestSidebarDeploy,
  RequestSidebarUndeploy,
  GetSidebarDeploymentOpenApi,
  GetSidebarDeploymentStatus,
  GetSidebarDocsContent,
  RequestNewL4File,
  RequestOpenUrl,
  RequestOpenServiceUrl,
  RequestOpenConsole,
  RequestOpenExtensionSettings,
  RequestCopySignInLink,
  RequestDisconnect,
  RequestRefreshDeployments,
  ShowNotification,
  RemoveInspectorResult,
  SidebarConnectionStatusChanged,
  type GetSidebarConnectionStatusResponse,
} from 'jl4-client-rpc'
import { getTokenColors } from './theme-colors.js'

export const SIDEBAR_WEBVIEW_TYPE = 'l4.deployView'

export const sidebarWebviewFrontend: WebviewTypeMessageParticipant = {
  type: 'webview',
  webviewType: SIDEBAR_WEBVIEW_TYPE,
}

export class SidebarProvider implements vscode.WebviewViewProvider {
  private view: vscode.WebviewView | undefined
  private ready = false
  private readyResolve: (() => void) | undefined
  private readyPromise: Promise<void>

  constructor(
    private readonly context: vscode.ExtensionContext,
    private readonly messenger: Messenger,
    private readonly auth: AuthManager,
    private readonly outputChannel: vscode.OutputChannel
  ) {
    this.readyPromise = new Promise((resolve) => {
      this.readyResolve = resolve
    })
    // Forward auth state changes to the sidebar webview
    auth.onDidChange((state) => {
      if (this.view) {
        const response: GetSidebarConnectionStatusResponse = {
          serviceUrl: state.serviceUrl,
          connected: state.connected,
          status: state.status,
          isLegaleseCloud: auth.isLegaleseCloudSession(),
          error: state.error,
        }
        this.messenger.sendNotification(
          SidebarConnectionStatusChanged,
          sidebarWebviewFrontend,
          response
        )
      }
    })
  }

  resolveWebviewView(
    webviewView: vscode.WebviewView,
    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    _context: vscode.WebviewViewResolveContext,
    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    _token: vscode.CancellationToken
  ): void {
    this.outputChannel.appendLine(
      `[sidebar] resolveWebviewView called (visible=${webviewView.visible})`
    )
    this.view = webviewView

    webviewView.webview.options = {
      enableScripts: true,
      localResourceRoots: [this.context.extensionUri],
    }

    // Clear then set HTML fresh — forces a full reload when restoring
    // from the secondary sidebar or after extension restart
    webviewView.webview.html = ''
    webviewView.webview.html = getWebviewContent(
      this.context,
      webviewView.webview,
      'sidebar'
    )

    this.messenger.registerWebviewView(webviewView)

    webviewView.onDidDispose(() => {
      this.outputChannel.appendLine('[sidebar] webview disposed')
      this.view = undefined
      this.ready = false
      this.readyPromise = new Promise((resolve) => {
        this.readyResolve = resolve
      })
    })
  }

  markReady() {
    this.ready = true
    this.readyResolve?.()
  }

  async waitUntilReady(): Promise<void> {
    return this.readyPromise
  }

  getView(): vscode.WebviewView | undefined {
    return this.view
  }

  refreshTokenColors() {
    if (!this.view) return
    const colors = getTokenColors()
    this.view.webview.postMessage({
      type: 'l4-token-colors',
      colors,
    })
  }

  notifyActiveFile(uri: string, version: number) {
    if (!this.view) return
    this.view.webview.postMessage({
      type: 'l4-sidebar-active-file',
      uri,
      version,
    })
  }

  clearActiveFile() {
    if (!this.view) return
    this.view.webview.postMessage({
      type: 'l4-sidebar-clear-file',
    })
  }

  async revealSidebar() {
    await vscode.commands.executeCommand(`${SIDEBAR_WEBVIEW_TYPE}.focus`)
  }

  switchToTab(tab: string) {
    if (!this.view) return
    this.view.webview.postMessage({
      type: 'l4-sidebar-switch-tab',
      tab,
    })
  }
}

/** Shape of a function entry in the org-wide /openapi.json response. */
interface DeploymentResponse {
  id: string
  status: string
  error?: string
  metadata?: {
    functions?: Array<{
      name: string
      description?: string
      parameters?: {
        type: 'object'
        properties: Record<
          string,
          {
            type: string
            enum: string[]
            description: string
            [key: string]: unknown
          }
        >
        required: string[]
      }
      returnType?: string
    }>
    files?: Array<{ path: string; exports: string[] }>
    version?: string
    createdAt?: string
  }
}

/**
 * Set up the messenger handlers for the sidebar <-> extension communication.
 */
export function initializeSidebarMessenger(
  messenger: Messenger,
  client: VSCodeL4LanguageClient,
  auth: AuthManager,
  serviceClient: ServiceClient,
  outputChannel: vscode.OutputChannel,
  onInspectorSectionRemoved?: (directiveId: string) => void
) {
  // Handle exported functions request from sidebar
  messenger.onRequest(GetSidebarExportedFunctions, async (params) => {
    outputChannel.appendLine(
      `[sidebar] Requesting exported functions for ${params.verDocId.uri}`
    )
    try {
      const response = await client.sendRequest(
        GetExportedFunctionsRequestType,
        params
      )
      return response
    } catch (err) {
      outputChannel.appendLine(
        `[sidebar] Error getting exported functions: ${err instanceof Error ? err.message : String(err)}`
      )
      return { functions: [] }
    }
  })

  // Handle connection status request
  messenger.onRequest(GetSidebarConnectionStatus, async () => {
    const state = await auth.getConnectionState()
    return {
      serviceUrl: state.serviceUrl,
      connected: state.connected,
      status: state.status,
      isLegaleseCloud: auth.isLegaleseCloudSession(),
      error: state.error,
    }
  })

  // Handle login request
  messenger.onNotification(RequestSidebarLogin, async () => {
    outputChannel.appendLine(`[sidebar] Login requested`)
    await auth.login()
  })

  // Handle logout request
  messenger.onNotification(RequestSidebarLogout, async () => {
    outputChannel.appendLine(`[sidebar] Logout requested`)
    await auth.logout()
  })

  // Handle list deployments request.
  // Uses GET /deployments?functions=full as the single source of truth.
  messenger.onRequest(ListSidebarDeployments, async () => {
    outputChannel.appendLine(
      `[sidebar] Listing deployments via /deployments?functions=full`
    )
    try {
      const deployments =
        (await serviceClient.getDeployments()) as DeploymentResponse[]

      // Empty array from 403 = insufficient permissions
      if (deployments.length === 0) {
        outputChannel.appendLine(
          `[sidebar] No deployments returned (may lack l4:rules permission)`
        )
      }

      return {
        deployments: deployments.map((dep) => ({
          deploymentId: dep.id,
          status: dep.status as 'pending' | 'compiling' | 'ready' | 'failed',
          error: dep.error,
          functions: (dep.metadata?.functions ?? []).map((fn) => ({
            name: fn.name,
            description: fn.description ?? '',
            isDefault: false,
            returnType: fn.returnType ?? '',
            isDeontic: false,
            parameters: fn.parameters ?? {
              type: 'object' as const,
              properties: {},
              required: [],
            },
          })),
        })),
      }
    } catch (err) {
      const message = err instanceof Error ? err.message : String(err)
      outputChannel.appendLine(
        `[sidebar] Error listing deployments: ${message}`
      )
      // Only logout on 401 (invalid credentials). 403 = valid session but
      // insufficient permissions — don't disconnect.
      if (message.includes(': 401 ')) {
        outputChannel.appendLine(
          `[sidebar] Disconnecting session due to authentication failure`
        )
        await auth.logout()
      }
      return { deployments: [] }
    }
  })

  // Handle deploy request
  messenger.onRequest(RequestSidebarDeploy, async (params) => {
    outputChannel.appendLine(
      `[sidebar] Deploy requested: ${params.deploymentId} from ${params.fileUri}`
    )
    try {
      // Ask the LSP for exported functions + transitive import URIs
      const docUri = vscode.Uri.parse(params.fileUri)
      const doc =
        vscode.workspace.textDocuments.find(
          (d) => d.uri.toString() === params.fileUri
        ) ?? (await vscode.workspace.openTextDocument(docUri))

      const lspResponse = await client.sendRequest(
        GetExportedFunctionsRequestType,
        { verDocId: { uri: params.fileUri, version: doc.version } }
      )
      const importUris = lspResponse.importedFiles ?? []

      // Collect the main file and all its LSP-resolved imports
      const files = await collectL4Sources(
        params.fileUri,
        importUris,
        outputChannel
      )
      outputChannel.appendLine(
        `[sidebar] Collected ${files.length} file(s) for deployment`
      )

      const zipBuffer = createZip(files)

      // Check if deployment already exists via its OpenAPI spec
      let isUpdate = false
      try {
        await serviceClient.getDeploymentOpenApi(params.deploymentId)
        isUpdate = true
      } catch {
        // Deployment doesn't exist — new deploy
      }

      const result = await serviceClient.deploy(
        params.deploymentId,
        zipBuffer,
        isUpdate
      )
      outputChannel.appendLine(
        `[sidebar] Deploy succeeded: ${result.id} (${result.status})`
      )
      return { success: true, deploymentId: result.id }
    } catch (err) {
      const msg = err instanceof Error ? err.message : String(err)
      outputChannel.appendLine(`[sidebar] Deploy failed: ${msg}`)
      return { success: false, error: msg }
    }
  })

  // Handle undeploy request
  messenger.onRequest(RequestSidebarUndeploy, async (params) => {
    outputChannel.appendLine(
      `[sidebar] Undeploy requested: ${params.deploymentId}`
    )
    try {
      await serviceClient.undeploy(params.deploymentId)
      return { success: true }
    } catch (err) {
      const msg = err instanceof Error ? err.message : String(err)
      outputChannel.appendLine(`[sidebar] Undeploy failed: ${msg}`)
      return { success: false, error: msg }
    }
  })

  // Handle deployment OpenAPI request (for breaking change detection)
  messenger.onRequest(GetSidebarDeploymentOpenApi, async (params) => {
    try {
      const openapi = await serviceClient.getDeploymentOpenApi(
        params.deploymentId
      )
      return { openapi }
    } catch (err) {
      outputChannel.appendLine(
        `[sidebar] Error fetching deployment OpenAPI: ${err instanceof Error ? err.message : String(err)}`
      )
      return { openapi: null }
    }
  })

  // Fetch markdown content for the docs tab
  messenger.onRequest(GetSidebarDocsContent, async (params) => {
    try {
      const resp = await fetch(params.url, {
        signal: AbortSignal.timeout(10000),
      })
      if (!resp.ok)
        return { markdown: `# Error\n\nFailed to load: ${resp.status}` }
      return { markdown: await resp.text() }
    } catch (err) {
      return {
        markdown: `# Error\n\nCould not load documentation: ${err instanceof Error ? err.message : String(err)}`,
      }
    }
  })

  // Poll deployment compilation status (lightweight, no schemas)
  messenger.onRequest(GetSidebarDeploymentStatus, async (params) => {
    try {
      const resp = await serviceClient.getDeploymentStatus(params.deploymentId)
      return {
        status: resp.status as 'pending' | 'compiling' | 'ready' | 'failed',
        error: resp.error,
      }
    } catch (err) {
      return {
        status: 'failed' as const,
        error: err instanceof Error ? err.message : String(err),
      }
    }
  })

  // Open L4 code as a new untitled file in the editor
  messenger.onNotification(RequestNewL4File, async (params) => {
    const doc = await vscode.workspace.openTextDocument({
      language: 'l4',
      content: params.content,
    })
    await vscode.window.showTextDocument(doc)
  })

  // Open an arbitrary URL in the browser
  messenger.onNotification(RequestOpenUrl, (params) => {
    vscode.env.openExternal(vscode.Uri.parse(params.url))
  })

  // Open the service URL in the browser
  // For Legalese Cloud sessions, route through /auth/redirect to set the cookie
  messenger.onNotification(RequestOpenServiceUrl, async () => {
    const url = auth.getEffectiveServiceUrl()
    if (!url) return

    if (auth.isLegaleseCloudSession()) {
      const session = await auth.getSessionToken()
      if (session) {
        const redirectUrl = `https://${LEGALESE_CLOUD_DOMAIN}/auth/redirect?token=${encodeURIComponent(session)}&redirect_to=${encodeURIComponent(url)}`
        vscode.env.openExternal(vscode.Uri.parse(redirectUrl))
        return
      }
    }

    vscode.env.openExternal(vscode.Uri.parse(url))
  })

  // Open Legalese Cloud Console with session token
  messenger.onNotification(RequestOpenConsole, async () => {
    const session = await auth.getSessionToken()
    const consoleUrl = session
      ? `https://legalese.com/console?token=${encodeURIComponent(session)}`
      : 'https://legalese.com/console'
    vscode.env.openExternal(vscode.Uri.parse(consoleUrl))
  })

  // Open the extension settings
  messenger.onNotification(RequestOpenExtensionSettings, () => {
    vscode.commands.executeCommand(
      'workbench.action.openSettings',
      '@ext:legalese.l4-vscode'
    )
  })

  // Copy Legalese Cloud sign-in link to clipboard.
  // Points to the console redirect page which shows who you're signed in as
  // and provides a "Return to VS Code" button. If not signed in, the console
  // handles the login flow and redirects back.
  messenger.onNotification(RequestCopySignInLink, async () => {
    const callbackUri = await vscode.env.asExternalUri(
      vscode.Uri.parse(
        `${vscode.env.uriScheme}://legalese.l4-vscode/auth-callback`
      )
    )
    const redirectUrl = `https://${LEGALESE_CLOUD_DOMAIN}/?redirect_to=${encodeURIComponent(callbackUri.toString())}`
    await vscode.env.clipboard.writeText(redirectUrl)
    vscode.window.showInformationMessage(
      'Legalese Cloud sign-in link copied to clipboard'
    )
  })

  // Disconnect: clear session token only, never touch settings
  messenger.onNotification(RequestDisconnect, async () => {
    outputChannel.appendLine(`[sidebar] Disconnect requested`)
    await auth.logout()
  })

  // Refresh deployments — handled on the sidebar side by re-requesting
  messenger.onNotification(RequestRefreshDeployments, () => {
    outputChannel.appendLine(`[sidebar] Refresh deployments requested`)
  })

  // Show VSCode notifications on behalf of the sidebar webview
  messenger.onNotification(ShowNotification, (params) => {
    switch (params.type) {
      case 'info':
        vscode.window.showInformationMessage(params.message)
        break
      case 'warning':
        vscode.window.showWarningMessage(params.message)
        break
      case 'error':
        vscode.window.showErrorMessage(params.message)
        break
    }
  })

  // Track when the webview removes an inspector result section
  messenger.onNotification(RemoveInspectorResult, (msg) => {
    onInspectorSectionRemoved?.(msg.directiveId)
  })
}

/**
 * Collect the main L4 file and all its transitive imports.
 * Import URIs are provided by the LSP via l4/getExportedFunctions.
 */
async function collectL4Sources(
  fileUri: string,
  importUris: string[],
  outputChannel: vscode.OutputChannel
): Promise<{ name: string; content: Uint8Array }[]> {
  const encoder = new TextEncoder()
  const collected = new Map<string, { name: string; content: Uint8Array }>()

  // Read a single file URI into the collection
  async function collectFile(uri: string) {
    if (collected.has(uri)) return
    try {
      const docUri = vscode.Uri.parse(uri)
      const existing = vscode.workspace.textDocuments.find(
        (d) => d.uri.toString() === uri
      )
      const doc = existing ?? (await vscode.workspace.openTextDocument(docUri))
      const text = doc.getText()
      const fileName = decodeURIComponent(
        docUri.path.split('/').pop() ?? 'main.l4'
      )
      collected.set(uri, { name: fileName, content: encoder.encode(text) })
      outputChannel.appendLine(`[sidebar] Collected: ${fileName}`)
    } catch (err) {
      outputChannel.appendLine(
        `[sidebar] Could not read import: ${uri} — ${err instanceof Error ? err.message : String(err)}`
      )
    }
  }

  // Collect main file first, then all imports
  await collectFile(fileUri)
  for (const importUri of importUris) {
    await collectFile(importUri)
  }

  return Array.from(collected.values())
}

// ---------------------------------------------------------------------------
// Minimal ZIP builder (store, no compression)
// ---------------------------------------------------------------------------

const crcTable = new Uint32Array(256)
for (let i = 0; i < 256; i++) {
  let c = i
  for (let j = 0; j < 8; j++) {
    c = c & 1 ? 0xedb88320 ^ (c >>> 1) : c >>> 1
  }
  crcTable[i] = c
}

function crc32(data: Uint8Array): number {
  let crc = 0xffffffff
  for (let i = 0; i < data.length; i++) {
    crc = crcTable[(crc ^ data[i]) & 0xff] ^ (crc >>> 8)
  }
  return (crc ^ 0xffffffff) >>> 0
}

function createZip(files: { name: string; content: Uint8Array }[]): Uint8Array {
  const encoder = new TextEncoder()

  // Pre-compute sizes
  type Entry = {
    nameBytes: Uint8Array
    content: Uint8Array
    crc: number
    localOffset: number
  }
  const entries: Entry[] = []
  let localSize = 0
  for (const file of files) {
    const nameBytes = encoder.encode(file.name)
    const crc = crc32(file.content)
    entries.push({
      nameBytes,
      content: file.content,
      crc,
      localOffset: localSize,
    })
    localSize += 30 + nameBytes.length + file.content.length
  }

  let centralSize = 0
  for (const e of entries) centralSize += 46 + e.nameBytes.length
  const endSize = 22
  const totalSize = localSize + centralSize + endSize

  const buf = new ArrayBuffer(totalSize)
  const view = new DataView(buf)
  const bytes = new Uint8Array(buf)
  let offset = 0

  // Local file headers + data
  for (const e of entries) {
    view.setUint32(offset, 0x04034b50, true)
    offset += 4
    view.setUint16(offset, 20, true)
    offset += 2
    view.setUint16(offset, 0, true)
    offset += 2
    view.setUint16(offset, 0, true)
    offset += 2
    view.setUint16(offset, 0, true)
    offset += 2
    view.setUint16(offset, 0, true)
    offset += 2
    view.setUint32(offset, e.crc, true)
    offset += 4
    view.setUint32(offset, e.content.length, true)
    offset += 4
    view.setUint32(offset, e.content.length, true)
    offset += 4
    view.setUint16(offset, e.nameBytes.length, true)
    offset += 2
    view.setUint16(offset, 0, true)
    offset += 2
    bytes.set(e.nameBytes, offset)
    offset += e.nameBytes.length
    bytes.set(e.content, offset)
    offset += e.content.length
  }

  const centralOffset = offset

  // Central directory headers
  for (const e of entries) {
    view.setUint32(offset, 0x02014b50, true)
    offset += 4
    view.setUint16(offset, 20, true)
    offset += 2
    view.setUint16(offset, 20, true)
    offset += 2
    view.setUint16(offset, 0, true)
    offset += 2
    view.setUint16(offset, 0, true)
    offset += 2
    view.setUint16(offset, 0, true)
    offset += 2
    view.setUint16(offset, 0, true)
    offset += 2
    view.setUint32(offset, e.crc, true)
    offset += 4
    view.setUint32(offset, e.content.length, true)
    offset += 4
    view.setUint32(offset, e.content.length, true)
    offset += 4
    view.setUint16(offset, e.nameBytes.length, true)
    offset += 2
    view.setUint16(offset, 0, true)
    offset += 2
    view.setUint16(offset, 0, true)
    offset += 2
    view.setUint16(offset, 0, true)
    offset += 2
    view.setUint16(offset, 0, true)
    offset += 2
    view.setUint32(offset, 0, true)
    offset += 4
    view.setUint32(offset, e.localOffset, true)
    offset += 4
    bytes.set(e.nameBytes, offset)
    offset += e.nameBytes.length
  }

  // End of central directory
  view.setUint32(offset, 0x06054b50, true)
  offset += 4
  view.setUint16(offset, 0, true)
  offset += 2
  view.setUint16(offset, 0, true)
  offset += 2
  view.setUint16(offset, entries.length, true)
  offset += 2
  view.setUint16(offset, entries.length, true)
  offset += 2
  view.setUint32(offset, centralSize, true)
  offset += 4
  view.setUint32(offset, centralOffset, true)
  offset += 4
  view.setUint16(offset, 0, true)

  return new Uint8Array(buf)
}
