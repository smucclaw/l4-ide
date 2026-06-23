import * as vscode from 'vscode'
import * as fs from 'node:fs'
import * as os from 'node:os'
import * as path from 'node:path'
import type { Messenger } from 'vscode-messenger'
import type { WebviewTypeMessageParticipant } from 'vscode-messenger-common'
import type { VSCodeL4LanguageClient } from './vscode-l4-language-client.js'
import { type AuthManager, LEGALESE_CLOUD_DOMAIN } from './auth.js'
import type { ServiceClient } from './service-client.js'
import { installDeploymentSkill } from './deployment-install.js'
import { installMarketplaceToHarness } from './harness-config.js'
import { getWebviewContent } from './webview-panel.js'
import {
  showTimedInformationMessage,
  showTimedWarningMessage,
} from './notifications.js'
import {
  GetExportedFunctionsRequestType,
  ExportDocumentRequestType,
  ExportPlanRequestType,
  RequestRenderPreview,
  RequestRenderInline,
  RequestRenderSave,
  GetSidebarImportedFiles,
  GetSidebarExportedFunctions,
  GetSidebarConnectionStatus,
  RequestSidebarLogin,
  RequestSidebarLogout,
  ListSidebarDeployments,
  RequestSidebarDeploy,
  RequestSidebarUndeploy,
  RequestSidebarDownloadDeployment,
  GetSidebarDeploymentSchemas,
  GetSidebarDeploymentStatus,
  GetSidebarUpdateStatus,
  GetSidebarDocsContent,
  RequestNewL4File,
  RequestOpenUrl,
  RequestOpenServiceUrl,
  RequestOpenConsole,
  RequestOpenExtensionSettings,
  RequestInstallMarketplace,
  RequestDownloadMarketplaceSkill,
  RequestInstallDeploymentSkill,
  RequestInstallL4Cli,
  RequestCopySignInLink,
  RequestDisconnect,
  RequestRevealLocation,
  RequestRefreshDeployments,
  ShowNotification,
  RemoveInspectorResult,
  SidebarConnectionStatusChanged,
  type GetSidebarConnectionStatusResponse,
  type RemoteFunctionSchema,
} from 'jl4-client-rpc'
import { getTokenColors } from './theme-colors.js'

export const SIDEBAR_WEBVIEW_TYPE = 'l4.deployView'

/** Read the MCP proxy port from settings, matching the default in
 *  mcp-proxy.ts and the package.json contribution. Surfaced in the
 *  sidebar's connection-status payload so the deployment panel can
 *  tell users which localhost port agents should connect to. */
function getMcpPort(): number {
  return (
    vscode.workspace.getConfiguration('jl4').get<number>('mcpPort') ?? 19415
  )
}

export const sidebarWebviewFrontend: WebviewTypeMessageParticipant = {
  type: 'webview',
  webviewType: SIDEBAR_WEBVIEW_TYPE,
}

/** Reused webview panel for the rendered HTML document, so successive
 *  previews update one panel rather than spawning a new one each time. */
let renderPanel: vscode.WebviewPanel | undefined

/** The L4 file most recently rendered. Lets "Generate preview" keep working
 *  when the focused tab is the rendered output (e.g. the HTML webview) and so
 *  has no sibling .l4 to derive the source from. */
let lastRenderedL4: vscode.Uri | undefined

const isL4Doc = (d: vscode.TextDocument): boolean =>
  d.languageId === 'l4' || d.languageId === 'jl4'

/** The file behind the active tab, even when it is a custom editor (PDF
 *  viewer) rather than a text editor. Webview tabs have no associated uri. */
function activeTabUri(): vscode.Uri | undefined {
  const input = vscode.window.tabGroups.activeTabGroup.activeTab?.input as
    | { uri?: vscode.Uri }
    | undefined
  return input?.uri instanceof vscode.Uri ? input.uri : undefined
}

/** `foo.html` / `foo.xml` / `foo.pdf` → sibling `foo.l4` in the same folder. */
function siblingL4(uri: vscode.Uri): vscode.Uri | undefined {
  const ext = path.extname(uri.fsPath).toLowerCase()
  if (ext === '.l4' || ext === '.jl4') return uri
  const base = path.basename(uri.fsPath, path.extname(uri.fsPath))
  return vscode.Uri.file(path.join(path.dirname(uri.fsPath), `${base}.l4`))
}

async function uriExists(uri: vscode.Uri): Promise<boolean> {
  try {
    await vscode.workspace.fs.stat(uri)
    return true
  } catch {
    return false
  }
}

/** Resolve the L4 document to render: the active L4 editor, else a same-named
 *  `.l4` sibling of the active (rendered) file, else the last-rendered L4. */
async function resolveL4Document(): Promise<vscode.TextDocument | undefined> {
  const active = vscode.window.activeTextEditor
  if (active && isL4Doc(active.document)) {
    lastRenderedL4 = active.document.uri
    return active.document
  }
  const candidate = active?.document.uri ?? activeTabUri()
  if (candidate && candidate.scheme === 'file') {
    const sib = siblingL4(candidate)
    if (sib && (await uriExists(sib))) {
      lastRenderedL4 = sib
      return vscode.workspace.openTextDocument(sib)
    }
  }
  if (lastRenderedL4 && (await uriExists(lastRenderedL4))) {
    return vscode.workspace.openTextDocument(lastRenderedL4)
  }
  return undefined
}

/** File extension for a render format. */
function extForFormat(format: string): string {
  switch (format) {
    case 'akn':
      return '.xml'
    case 'text':
      return '.txt'
    case 'json':
      return '.json'
    case 'plan':
      return '.plan.json'
    default:
      return '.html'
  }
}

/** Save-dialog file-type filters for a render format. */
function saveFiltersForFormat(format: string): Record<string, string[]> {
  switch (format) {
    case 'akn':
      return { 'Akoma Ntoso XML': ['xml'], 'All files': ['*'] }
    case 'text':
      return { 'Plain text': ['txt'], 'All files': ['*'] }
    case 'json':
    case 'plan':
      return { JSON: ['json'], 'All files': ['*'] }
    default:
      return { 'HTML document': ['html', 'htm'], 'All files': ['*'] }
  }
}

/** Render the rendered HTML in a (reused) webview panel in the active editor
 *  group — not a split — so it sits alongside the source as a tab. */
function showHtmlPreview(html: string, title: string): void {
  if (!renderPanel) {
    renderPanel = vscode.window.createWebviewPanel(
      'l4.renderPreview',
      title,
      vscode.ViewColumn.Active,
      { enableScripts: false, retainContextWhenHidden: true }
    )
    renderPanel.onDidDispose(() => {
      renderPanel = undefined
    })
  }
  renderPanel.title = title
  renderPanel.webview.html = html
  renderPanel.reveal(undefined, false)
}

export class SidebarProvider implements vscode.WebviewViewProvider {
  private view: vscode.WebviewView | undefined
  private ready = false
  private readyResolve: (() => void) | undefined
  private readyPromise: Promise<void>
  /** Fires with `true` when the webview becomes visible and `false`
   *  when it goes hidden (activity-bar switch, editor tab, layout
   *  change). Consumers that accumulate UI events for the webview
   *  drain on the true-transition so a chat streaming in the
   *  background catches up as soon as the user flips back. Fires
   *  before the event buffer drains, i.e. subscribers are free to
   *  check `isVisible()` directly. */
  private readonly visibilityEmitter = new vscode.EventEmitter<boolean>()
  readonly onDidChangeVisibility = this.visibilityEmitter.event

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
          orgSlug: auth.getCloudOrgSlug(),
          mcpPort: getMcpPort(),
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

  /** Current visibility. False when the view hasn't been resolved
   *  yet, so consumers that optimistically buffer before the first
   *  resolve don't drop frames on the floor. */
  isVisible(): boolean {
    return this.view?.visible ?? false
  }

  /**
   * Re-emit the connection-status notification with the current
   * snapshot. Called from outside the provider when something the
   * sidebar displays — but that isn't an auth state change — has
   * changed (e.g. the MCP port was reconfigured at runtime).
   */
  async refreshConnectionStatus(): Promise<void> {
    if (!this.view) return
    const state = await this.auth.getConnectionState()
    const response: GetSidebarConnectionStatusResponse = {
      serviceUrl: state.serviceUrl,
      connected: state.connected,
      status: state.status,
      isLegaleseCloud: this.auth.isLegaleseCloudSession(),
      orgSlug: this.auth.getCloudOrgSlug(),
      mcpPort: getMcpPort(),
      error: state.error,
    }
    this.messenger.sendNotification(
      SidebarConnectionStatusChanged,
      sidebarWebviewFrontend,
      response
    )
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

    // Fire the initial visibility state on resolve so downstream
    // buffers know whether to start flowing directly or to queue
    // events until the view actually paints. We also forward every
    // subsequent flip — the "hidden → visible" transition is the
    // cue to drain any events that piled up while the user was off
    // on a different activity-bar item.
    this.visibilityEmitter.fire(webviewView.visible)
    webviewView.onDidChangeVisibility(() => {
      this.outputChannel.appendLine(
        `[sidebar] visibility change (visible=${webviewView.visible})`
      )
      this.visibilityEmitter.fire(webviewView.visible)
    })

    webviewView.onDidDispose(() => {
      this.outputChannel.appendLine('[sidebar] webview disposed')
      this.view = undefined
      this.ready = false
      this.readyPromise = new Promise((resolve) => {
        this.readyResolve = resolve
      })
      // A disposed view is not visible; signal so buffers switch to
      // queue-mode and don't try to post into a dead webview. When
      // the user re-opens the sidebar, resolveWebviewView above fires
      // the visible-true event and the buffer drains.
      this.visibilityEmitter.fire(false)
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
    description?: string
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
  // VS Code's user-data path (the one containing `mcp.json` + `globalStorage/`).
  // Required by the install flows that write per-harness MCP entries. Derived
  // from `context.globalStorageUri` by the caller.
  userDataPath: string | undefined,
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

  // Handle Render-tab imported-files request: resolve the active L4 doc and
  // return the modules it imports (non-main), for the include/exclude checklist.
  messenger.onRequest(GetSidebarImportedFiles, async () => {
    const doc = await resolveL4Document()
    if (!doc) return { files: [] }
    try {
      const verDocId = { uri: doc.uri.toString(), version: doc.version }
      const plan = await client.sendRequest(ExportPlanRequestType, { verDocId })
      const files = (plan?.modules ?? [])
        .filter((m) => !m.isMain)
        .map((m) => ({ uri: m.uri, label: m.label }))
      return { files }
    } catch (err) {
      outputChannel.appendLine(
        `[sidebar] Error getting imported files: ${err instanceof Error ? err.message : String(err)}`
      )
      return { files: [] }
    }
  })

  // Handle Render-tab preview request: render the active L4 file via the LSP,
  // write the result next to the .l4 file, and open it in the active editor
  // group (HTML as a rendered webview; others as the saved file).
  messenger.onRequest(RequestRenderPreview, async (params) => {
    const doc = await resolveL4Document()
    if (!doc) {
      return {
        success: false,
        error: 'Open an L4 file (or a file beside one) to render.',
      }
    }
    try {
      const verDocId = { uri: doc.uri.toString(), version: doc.version }
      // Default to opening the result; the Render tab opts out when it
      // will refine the render through Legalese AI instead.
      const openInEditor = params.openInEditor ?? true

      // Compute the rendered content + the IR title.
      let content: string
      let title: string | undefined
      if (params.format === 'plan') {
        const plan = await client.sendRequest(ExportPlanRequestType, {
          verDocId,
        })
        content = JSON.stringify(plan ?? {}, null, 2)
      } else {
        const res = await client.sendRequest(ExportDocumentRequestType, {
          verDocId,
          format: params.format,
          includeUnused: params.includeUnused,
          numberSections: params.numberSections,
          numberClauses: params.numberClauses,
          toc: params.toc,
          excludeModules: params.excludeModules ?? [],
        })
        if (!res) {
          return {
            success: false,
            error: 'No response from the language server.',
          }
        }
        title =
          res.ir && typeof res.ir === 'object'
            ? (res.ir as { title?: string }).title
            : undefined
        content =
          params.format === 'json'
            ? JSON.stringify(res.ir ?? {}, null, 2)
            : res.content
      }

      // Write the rendered file next to the .l4 (file-scheme docs only).
      let savedPath: string | undefined
      if (doc.uri.scheme === 'file') {
        const dir = path.dirname(doc.uri.fsPath)
        const base = path.basename(doc.uri.fsPath, path.extname(doc.uri.fsPath))
        const outUri = vscode.Uri.file(
          path.join(dir, base + extForFormat(params.format))
        )
        await vscode.workspace.fs.writeFile(
          outUri,
          new TextEncoder().encode(content)
        )
        savedPath = outUri.fsPath

        // Open it in the active group (not a split) — unless the caller
        // asked us to skip it (Render tab handing off to Legalese AI
        // refinement, where the deterministic output is intermediate).
        if (openInEditor) {
          if (params.format === 'html') {
            showHtmlPreview(content, title ?? base)
          } else {
            const opened = await vscode.workspace.openTextDocument(outUri)
            await vscode.window.showTextDocument(opened, {
              viewColumn: vscode.ViewColumn.Active,
              preview: false,
            })
          }
        }
      } else if (openInEditor && params.format === 'html') {
        // Unsaved/remote doc: preview-only, no file written.
        showHtmlPreview(content, title ?? 'L4 Render')
      } else if (openInEditor) {
        const language =
          params.format === 'akn'
            ? 'xml'
            : params.format === 'json' || params.format === 'plan'
              ? 'json'
              : 'plaintext'
        const opened = await vscode.workspace.openTextDocument({
          content,
          language,
        })
        await vscode.window.showTextDocument(opened, {
          viewColumn: vscode.ViewColumn.Active,
          preview: false,
        })
      }

      return { success: true, title, savedPath }
    } catch (err) {
      const msg = err instanceof Error ? err.message : String(err)
      outputChannel.appendLine(`[sidebar] Render failed: ${msg}`)
      return { success: false, error: msg }
    }
  })

  // Render the active L4 document to a content string in the requested
  // format via the LSP. Returns the rendered content and the IR title.
  // Shared by the inline preview and the Save action. Throws on LSP failure.
  async function renderToContent(
    verDocId: { uri: string; version: number },
    params: {
      format: string
      includeUnused: boolean
      numberSections: boolean
      numberClauses: boolean
      toc: boolean
      excludeModules?: string[]
    }
  ): Promise<{ content: string; title: string | undefined }> {
    if (params.format === 'plan') {
      const plan = await client.sendRequest(ExportPlanRequestType, { verDocId })
      return { content: JSON.stringify(plan ?? {}, null, 2), title: undefined }
    }
    const res = await client.sendRequest(ExportDocumentRequestType, {
      verDocId,
      format: params.format,
      includeUnused: params.includeUnused,
      numberSections: params.numberSections,
      numberClauses: params.numberClauses,
      toc: params.toc,
      excludeModules: params.excludeModules ?? [],
    })
    if (!res) throw new Error('No response from the language server.')
    const title =
      res.ir && typeof res.ir === 'object'
        ? (res.ir as { title?: string }).title
        : undefined
    const content =
      params.format === 'json'
        ? JSON.stringify(res.ir ?? {}, null, 2)
        : res.content
    return { content, title }
  }

  // Handle Render-tab live inline preview: render the active file as HTML
  // and return it to the webview to display in an iframe. Never writes to
  // disk or opens a panel — reflects the in-memory (unsaved) buffer.
  messenger.onRequest(RequestRenderInline, async (params) => {
    const doc = await resolveL4Document()
    if (!doc) {
      return {
        success: false,
        error: 'Open an L4 file (or a file beside one) to render.',
      }
    }
    try {
      const verDocId = { uri: doc.uri.toString(), version: doc.version }
      const { content, title } = await renderToContent(verDocId, {
        format: 'html',
        includeUnused: params.includeUnused,
        numberSections: params.numberSections,
        numberClauses: params.numberClauses,
        toc: params.toc,
        excludeModules: params.excludeModules ?? [],
      })
      return { success: true, html: content, title }
    } catch (err) {
      const msg = err instanceof Error ? err.message : String(err)
      return { success: false, error: msg }
    }
  })

  // Handle Render-tab Save action: render the active file in the chosen
  // format, then prompt for a location with a native Save dialog and write
  // the file there.
  messenger.onRequest(RequestRenderSave, async (params) => {
    const doc = await resolveL4Document()
    if (!doc) {
      return {
        success: false,
        error: 'Open an L4 file (or a file beside one) to render.',
      }
    }
    try {
      const verDocId = { uri: doc.uri.toString(), version: doc.version }
      const { content } = await renderToContent(verDocId, params)

      // Default the Save dialog next to the .l4 source (when on disk),
      // with the format's natural extension and base name.
      const ext = extForFormat(params.format)
      let defaultUri: vscode.Uri | undefined
      if (doc.uri.scheme === 'file') {
        const dir = path.dirname(doc.uri.fsPath)
        const base = path.basename(doc.uri.fsPath, path.extname(doc.uri.fsPath))
        defaultUri = vscode.Uri.file(path.join(dir, base + ext))
      }
      const target = await vscode.window.showSaveDialog({
        defaultUri,
        filters: saveFiltersForFormat(params.format),
      })
      if (!target) return { success: false, canceled: true }

      await vscode.workspace.fs.writeFile(
        target,
        new TextEncoder().encode(content)
      )
      return { success: true, savedPath: target.fsPath }
    } catch (err) {
      const msg = err instanceof Error ? err.message : String(err)
      outputChannel.appendLine(`[sidebar] Render save failed: ${msg}`)
      return { success: false, error: msg }
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
      orgSlug: auth.getCloudOrgSlug(),
      mcpPort: getMcpPort(),
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
          description: dep.metadata?.description,
          // Backend omits `metadata.files` entirely when the read scope is
          // absent (proxy strips it via X-Include-Files: false). A non-empty
          // list means the user is allowed to read sources — gate the
          // sidebar's Download action on this.
          hasFiles: (dep.metadata?.files?.length ?? 0) > 0,
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

      // PUT (isUpdate) goes through the backwards-compatibility gate;
      // POST overwrites ungated. When the user has reviewed and
      // confirmed the breaking changes, force POST.
      let isUpdate = false
      if (!params.overwrite) {
        try {
          await serviceClient.getDeploymentOpenApi(params.deploymentId)
          isUpdate = true
        } catch {
          // Deployment doesn't exist — new deploy
        }
      }

      const result = await serviceClient.deploy(
        params.deploymentId,
        zipBuffer,
        isUpdate,
        params.mission
      )
      outputChannel.appendLine(
        `[sidebar] Deploy accepted: ${result.id} (${result.status}` +
          `${result.updateId ? `, job ${result.updateId}` : ''})`
      )
      return {
        success: true,
        deploymentId: result.id,
        updateId: result.updateId,
      }
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

  // Handle download deployment request: prompt for a folder, fetch all
  // source files from the service, and write `{folder}/{deploymentId}/*`.
  messenger.onRequest(RequestSidebarDownloadDeployment, async (params) => {
    const { deploymentId } = params
    outputChannel.appendLine(`[sidebar] Download requested: ${deploymentId}`)
    try {
      // Default to the active file's workspace folder (multi-root aware),
      // else the first workspace folder, else the user's home dir.
      const activeUri = vscode.window.activeTextEditor?.document.uri
      const activeFolder = activeUri
        ? vscode.workspace.getWorkspaceFolder(activeUri)
        : undefined
      const defaultUri =
        activeFolder?.uri ?? vscode.workspace.workspaceFolders?.[0]?.uri

      const picked = await vscode.window.showOpenDialog({
        canSelectFiles: false,
        canSelectFolders: true,
        canSelectMany: false,
        defaultUri,
        openLabel: 'Save here',
        title: `Download deployment "${deploymentId}"`,
      })
      if (!picked || picked.length === 0) {
        return { success: false, cancelled: true }
      }
      const parentUri = picked[0]

      // Sanitise the deployment id for use as a path segment. The backend
      // already restricts ids to [a-zA-Z0-9_-] (validateDeploymentId), but
      // strip path separators defensively so a malformed id can never
      // escape the chosen folder.
      const safeName = deploymentId.replace(/[/\\]/g, '_')
      const targetUri = vscode.Uri.joinPath(parentUri, safeName)

      // Conflict check: warn if the target folder already contains files,
      // give the user a chance to bail.
      let targetExists = false
      try {
        const entries = await vscode.workspace.fs.readDirectory(targetUri)
        targetExists = entries.length > 0
      } catch {
        // Folder doesn't exist yet — fine, we'll create it.
      }
      if (targetExists) {
        const choice = await vscode.window.showWarningMessage(
          `Folder "${safeName}" already exists in the chosen location. Overwrite files with the same name?`,
          { modal: true },
          'Overwrite'
        )
        if (choice !== 'Overwrite') {
          return { success: false, cancelled: true }
        }
      }

      // Single round-trip: GET /deployments/{id}/files returns every
      // .l4 source with its content inline.
      const { files } = await serviceClient.getDeploymentFiles(deploymentId)
      if (files.length === 0) {
        return {
          success: false,
          error:
            'No files returned for this deployment. You may not have permission to download its sources.',
        }
      }

      await vscode.workspace.fs.createDirectory(targetUri)

      // Path-traversal guard: each file path must be a clean relative
      // path. Reject anything containing ".." segments before joining.
      for (const file of files) {
        const segments = file.path.split(/[/\\]+/).filter((s) => s.length > 0)
        if (segments.some((s) => s === '..' || s === '.')) {
          throw new Error(
            `Refusing to write file with unsafe path: ${file.path}`
          )
        }
        const fileUri = vscode.Uri.joinPath(targetUri, ...segments)
        const parentDir = vscode.Uri.joinPath(fileUri, '..')
        await vscode.workspace.fs.createDirectory(parentDir)
        await vscode.workspace.fs.writeFile(
          fileUri,
          Buffer.from(file.content, 'utf8')
        )
      }

      outputChannel.appendLine(
        `[sidebar] Wrote ${files.length} file(s) to ${targetUri.fsPath}`
      )
      return {
        success: true,
        folderPath: targetUri.fsPath,
        fileCount: files.length,
      }
    } catch (err) {
      const msg = err instanceof Error ? err.message : String(err)
      outputChannel.appendLine(`[sidebar] Download failed: ${msg}`)
      return { success: false, error: msg }
    }
  })

  // Fetch the deployed functions' full schemas in one round-trip so the
  // sidebar can recursively diff them against the local interface.
  // Returns { functions: null } when the deployment does not exist yet
  // (or is unreachable) — a first deploy can't break anything.
  messenger.onRequest(GetSidebarDeploymentSchemas, async (params) => {
    try {
      const status = await serviceClient.getDeploymentStatus(
        params.deploymentId,
        'full'
      )
      const meta = status.metadata as
        | {
            functions?: Array<{
              name: string
              parameters?: RemoteFunctionSchema['parameters']
              returnType?: string
              returnSchema?: RemoteFunctionSchema['returnSchema']
            }>
          }
        | undefined
      const functions = (meta?.functions ?? []).map((f) => ({
        name: f.name,
        parameters: f.parameters,
        returnType: f.returnType,
        returnSchema: f.returnSchema,
      }))
      return { functions }
    } catch (err) {
      outputChannel.appendLine(
        `[sidebar] Error fetching deployment schemas: ${err instanceof Error ? err.message : String(err)}`
      )
      return { functions: null }
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

  // Poll an async deploy/update job (POST/PUT)
  messenger.onRequest(GetSidebarUpdateStatus, async (params) => {
    try {
      const resp = await serviceClient.getUpdateStatus(
        params.deploymentId,
        params.updateId
      )
      return {
        status: resp.status as 'compiling' | 'applied' | 'rejected',
        error: resp.error,
      }
    } catch (err) {
      return {
        status: 'rejected' as const,
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

  // Open a file at a specific line in the editor
  messenger.onNotification(RequestRevealLocation, async (params) => {
    const docUri = vscode.Uri.parse(params.uri)
    const doc =
      vscode.workspace.textDocuments.find(
        (d) => d.uri.toString() === params.uri
      ) ?? (await vscode.workspace.openTextDocument(docUri))
    const line = Math.max(0, params.line - 1) // Convert 1-based to 0-based
    const range = new vscode.Range(line, 0, line, 0)
    await vscode.window.showTextDocument(doc, {
      selection: range,
      preserveFocus: false,
    })
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

  // Install the global gateway "skills marketplace" (the org-wide rules MCP,
  // plus the rules@legalese-cloud plugin for Claude Code) into a harness.
  messenger.onNotification(RequestInstallMarketplace, async ({ harness }) => {
    outputChannel.appendLine(`[sidebar] Install marketplace → ${harness}`)
    try {
      await installMarketplaceToHarness(harness, {
        userDataPath,
        outputChannel,
      })
    } catch (err) {
      const msg = err instanceof Error ? err.message : String(err)
      outputChannel.appendLine(
        `[sidebar] Install marketplace → ${harness} failed: ${msg}`
      )
      void vscode.window.showErrorMessage(
        `Could not install the L4 Rules: ${msg}`
      )
    }
  })

  // Save the gateway "skills marketplace" plugin as a zip — the download
  // counterpart of the Install action. The plugin's single source of truth is
  // the public GitHub repo, so we fetch its archive rather than templating a
  // local copy that could drift from the served / GitHub skill.
  const MARKETPLACE_ZIP_URL =
    'https://github.com/legalese/cloud-rules/archive/refs/heads/main.zip'
  const MARKETPLACE_ZIP_NAME = 'cloud-rules.zip'
  messenger.onNotification(RequestDownloadMarketplaceSkill, async () => {
    outputChannel.appendLine('[sidebar] Download marketplace skill zip')
    try {
      const res = await fetch(MARKETPLACE_ZIP_URL)
      if (!res.ok) throw new Error(`HTTP ${res.status} ${res.statusText}`)
      const zip = Buffer.from(await res.arrayBuffer())
      const defaultUri = vscode.Uri.file(
        path.join(os.homedir(), 'Downloads', MARKETPLACE_ZIP_NAME)
      )
      const dest = await vscode.window.showSaveDialog({
        defaultUri,
        filters: { 'Zip Archive': ['zip'] },
        saveLabel: 'Save plugin',
        title: `Save ${MARKETPLACE_ZIP_NAME}`,
      })
      if (!dest) return // cancelled
      fs.mkdirSync(path.dirname(dest.fsPath), { recursive: true })
      fs.writeFileSync(dest.fsPath, zip)
      outputChannel.appendLine(
        `[sidebar] Saved ${zip.length}B marketplace skill zip to ${dest.fsPath}`
      )
      void vscode.window.showInformationMessage(
        `Saved ${MARKETPLACE_ZIP_NAME} to ${dest.fsPath}.`,
        'Okay'
      )
    } catch (err) {
      const msg = err instanceof Error ? err.message : String(err)
      outputChannel.appendLine(
        `[sidebar] Download marketplace skill failed: ${msg}`
      )
      void vscode.window.showErrorMessage(
        `Could not download the L4 Rules plugin zip: ${msg}`
      )
    }
  })

  // Install a per-deployment plugin bundle (SKILL.md + hosted MCP entry)
  // into either Claude Code or VS Code Chat. The bundle is downloaded
  // from the auth-proxy's .plugin endpoint with the user's session token.
  messenger.onNotification(
    RequestInstallDeploymentSkill,
    async ({ deploymentId, target }) => {
      outputChannel.appendLine(
        `[sidebar] Install deployment ${deploymentId} → ${target}`
      )
      try {
        await installDeploymentSkill({
          deploymentId,
          target,
          auth,
          serviceClient,
          outputChannel,
          userDataPath,
        })
      } catch (err) {
        const msg = err instanceof Error ? err.message : String(err)
        outputChannel.appendLine(
          `[sidebar] Install ${deploymentId} → ${target} failed: ${msg}`
        )
        void vscode.window.showErrorMessage(
          `Could not install ${deploymentId}: ${msg}`
        )
      }
    }
  )

  // Install the bundled l4 CLI on PATH.
  // The sidebar dropdown fires this separately from "Add L4 Tools …"
  // because a user may want the CLI without touching Claude Code
  // (e.g. to use it from their shell or editor).
  messenger.onNotification(RequestInstallL4Cli, async () => {
    await vscode.commands.executeCommand('l4.installCli')
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
    showTimedInformationMessage(
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
        showTimedInformationMessage(params.message)
        break
      case 'warning':
        showTimedWarningMessage(params.message)
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
