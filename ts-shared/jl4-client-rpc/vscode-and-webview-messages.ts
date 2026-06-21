import type {
  L4RpcRequestType,
  L4RpcNotificationType,
  FunctionParameter,
  FunctionParameters,
} from './custom-protocol.js'
import {
  makeL4RpcRequestType,
  makeL4RpcNotificationType,
} from './custom-protocol.js'
import { RenderAsLadderInfo } from '@repo/viz-expr'
import type { NotificationType, RequestType } from 'vscode-messenger-common'

/*************************************************************
              On webview frontend initialization
**************************************************************/

export interface WebviewFrontendIsReadyMessage {
  $type: 'webviewReady'
}

export const WebviewFrontendIsReadyNotification: NotificationType<WebviewFrontendIsReadyMessage> =
  {
    method: 'webviewFrontendIsReady',
  }

/*************************************************************
            Render FunDecl in Ladder Visualizer 
                Request and Response
                  for VSCode Webview
**************************************************************/

/** This is the 'please visualize this fun decl' request for the VSCode webview.
 * Using a request so that the extension can know whether the webview received it.
 * See also Wrapper / Protocol interfaces in viz-expr.ts
 */
export const RenderAsLadder: RequestType<
  RenderAsLadderInfo,
  RenderAsLadderResponse
> = {
  method: 'renderAsLadder',
}

export type RenderAsLadderResponse = { $type: 'ok' } | { $type: 'error' }

export function makeRenderAsLadderSuccessResponse(): RenderAsLadderResponse {
  return { $type: 'ok' }
}

export function makeRenderAsLadderFailureResponse(): RenderAsLadderResponse {
  return { $type: 'error' }
}

/*************************************************************
            Render Directive Result in Inspector Panel
                Request and Response
                  for VSCode Webview
**************************************************************/

import type { DirectiveResult, SrcPos } from './custom-protocol.js'

export interface AddInspectorResultMessage {
  directiveId: string
  srcPos: SrcPos
  result: DirectiveResult
  /** Full directive body (source-range slice joined by `\n`). Render
   *  sites collapse to the first non-blank line for one-line headers. */
  body: string
}

export const AddInspectorResult: RequestType<
  AddInspectorResultMessage,
  { $type: 'ok' | 'scrolled' }
> = {
  method: 'addInspectorResult',
}

export interface RemoveInspectorResultMessage {
  directiveId: string
}

export const RemoveInspectorResult: NotificationType<RemoveInspectorResultMessage> =
  {
    method: 'removeInspectorResult',
  }

export interface UpdateInspectorResultMessage {
  directiveId: string
  result: DirectiveResult
}

export const UpdateInspectorResult: NotificationType<UpdateInspectorResultMessage> =
  {
    method: 'updateInspectorResult',
  }

export interface SyncInspectorResultsMessage {
  /** When set, only sections belonging to this file are synced; others are left untouched. */
  uri?: string
  results: Array<{
    directiveId: string
    prettyText: string
    success: boolean | null
    /** Full directive body (source-range slice joined by `\n`). */
    body: string
  }>
}

export const SyncInspectorResults: NotificationType<SyncInspectorResultsMessage> =
  {
    method: 'syncInspectorResults',
  }

/*************************************************************
            Toggle Simplify for Ladder Visualization
**************************************************************/

export interface ToggleSimplifyMessage {
  shouldSimplify: boolean
}

export const ToggleSimplify: NotificationType<ToggleSimplifyMessage> = {
  method: 'toggleSimplify',
}

/*************************************************************
            For the vscode Webview to
            tell, via the Ladder Backend API,
            the VSCode Extension to
            forward a LSP client request
**************************************************************/

/** Returns the (vscode-webview-messenger) Request type for webview to get extension to forward a request to the language server. */
export const makeLspRelayRequestType = <P extends object, R>(): RequestType<
  ClientRequestParams<P, R>,
  R
> => ({
  method: 'sendClientRequest',
})

/** Payload for {@link makeLspRelayRequestType}:
 * contains all the info needed for the extension to send a request to the language server. */
export interface ClientRequestParams<P extends object, R> {
  requestType: L4RpcRequestType<P, R>
  params: P
}

/*************************************************************
            Deploy Sidebar Messages
**************************************************************/

import type {
  ExportedFunctionInfo,
  GetExportedFunctionsParams,
  GetExportedFunctionsResponse,
} from './custom-protocol.js'

/** Sidebar asks extension for exported functions from the active file */
export const GetSidebarExportedFunctions: RequestType<
  GetExportedFunctionsParams,
  GetExportedFunctionsResponse
> = {
  method: 'getSidebarExportedFunctions',
}

/** Render tab asks the extension to render the active file and open a preview.
 *  The extension resolves the active L4 document, calls the LSP
 *  `l4/exportDocument`, and opens the result (HTML in a webview panel; AKN/text
 *  in a new editor). */
export interface RenderPreviewParams {
  /** "html" (default) | "text" | "akn" */
  format: string
  includeUnused: boolean
  numberSections: boolean
  numberClauses: boolean
  toc: boolean
  /** Module URIs the user deselected in the imports checklist. */
  excludeModules?: string[]
  /** Whether to surface the rendered document in an editor/browser
   *  tab. Defaults to true. The Render tab sets this to false when it
   *  is about to hand the render off to Legalese AI for refinement —
   *  the deterministic output is an intermediate artifact there, so
   *  popping it open would just clutter the workspace. */
  openInEditor?: boolean
}

/** An imported module the active file pulls in, for the Render-tab
 *  include/exclude checklist. */
export interface SidebarImportedFile {
  /** Module URI — round-tripped as an `excludeModules` entry. */
  uri: string
  /** Display label (file name or section title). */
  label: string
}

/** Render tab asks the extension for the active file's imported modules.
 *  The extension resolves the active L4 document and calls the LSP
 *  `l4/exportPlan`, returning the non-main modules. */
export const GetSidebarImportedFiles: RequestType<
  void,
  { files: SidebarImportedFile[] }
> = {
  method: 'getSidebarImportedFiles',
}

export interface RenderPreviewResponse {
  success: boolean
  /** The document title, echoed for the sidebar status line. */
  title?: string
  /** Absolute path of the file written next to the .l4 (when on disk). */
  savedPath?: string
  error?: string
}

export const RequestRenderPreview: RequestType<
  RenderPreviewParams,
  RenderPreviewResponse
> = {
  method: 'requestRenderPreview',
}

/** Render tab asks the extension for a live in-tab HTML preview of the
 *  active file. Unlike {@link RequestRenderPreview}, this never writes a
 *  file to disk and never opens an editor/webview panel — the rendered
 *  HTML is returned to the sidebar webview, which displays it inline in an
 *  iframe (`srcdoc`). The render reflects the in-memory (unsaved) buffer,
 *  so the preview tracks edits as the user types. */
export interface RenderInlineParams {
  numberSections: boolean
  numberClauses: boolean
  toc: boolean
  includeUnused: boolean
  /** Module URIs the user deselected in the imports tray. */
  excludeModules?: string[]
}

export interface RenderInlineResponse {
  success: boolean
  /** Full HTML document, ready to drop into an iframe `srcdoc`. */
  html?: string
  /** The document title, echoed for the sidebar. */
  title?: string
  error?: string
}

export const RequestRenderInline: RequestType<
  RenderInlineParams,
  RenderInlineResponse
> = {
  method: 'requestRenderInline',
}

/** Render tab asks the extension to export the active file in a chosen
 *  format and persist it to a user-chosen location. The extension renders
 *  the content, shows a native Save dialog (defaulting next to the .l4
 *  source with the format's extension), and writes the file there. */
export interface RenderSaveParams {
  /** "html" | "akn" | "text" | "json" | "plan" */
  format: string
  numberSections: boolean
  numberClauses: boolean
  toc: boolean
  includeUnused: boolean
  excludeModules?: string[]
}

export interface RenderSaveResponse {
  success: boolean
  /** Absolute path the user saved to (omitted when canceled). */
  savedPath?: string
  /** True when the user dismissed the Save dialog. */
  canceled?: boolean
  error?: string
}

export const RequestRenderSave: RequestType<
  RenderSaveParams,
  RenderSaveResponse
> = {
  method: 'requestRenderSave',
}

/** Connection status response from extension to sidebar */
export interface GetSidebarConnectionStatusResponse {
  serviceUrl: string
  connected: boolean
  status: 'connected' | 'not-configured' | 'connecting' | 'error'
  isLegaleseCloud: boolean
  /** Verified Legalese Cloud org slug, when signed in to a cloud
   *  session. Undefined for self-hosted jl4-service / API-key-only.
   *  Drives the Deployment tab's deployment-scoped integration URLs. */
  orgSlug?: string
  /** Localhost port the extension's MCP proxy is listening on. Read
   *  from `jl4.mcpPort` (default 19415) at status time so the sidebar
   *  can surface the address agents should connect to. */
  mcpPort: number
  error?: string
}

/** Sidebar asks extension for current connection status */
export const GetSidebarConnectionStatus: RequestType<
  void,
  GetSidebarConnectionStatusResponse
> = {
  method: 'getSidebarConnectionStatus',
}

/** Sidebar requests login flow */
export const RequestSidebarLogin: NotificationType<void> = {
  method: 'requestSidebarLogin',
}

/** Sidebar requests logout */
export const RequestSidebarLogout: NotificationType<void> = {
  method: 'requestSidebarLogout',
}

/** Deployment info derived from /openapi.json */
export interface SidebarDeploymentInfo {
  deploymentId: string
  status?: 'pending' | 'compiling' | 'ready' | 'failed'
  error?: string
  /** Operator-supplied "Intended use" (metadata.description), so a
   *  redeploy can pre-populate the field instead of starting blank. */
  description?: string
  functions: ExportedFunctionInfo[]
  /** True when the backend returned a non-empty `metadata.files` list.
   * Empty/missing on a `ready` deployment indicates the proxy stripped
   * the file list (read scope absent), so the Download action should
   * be hidden from the deployment menu. */
  hasFiles?: boolean
}

/** Sidebar requests list of deployments */
export const ListSidebarDeployments: RequestType<
  void,
  { deployments: SidebarDeploymentInfo[] }
> = {
  method: 'listSidebarDeployments',
}

/** Sidebar requests deploy */
export interface SidebarDeployParams {
  deploymentId: string
  fileUri: string
  /** Operator-supplied "Intended use" for this deployment. */
  mission?: string
  /** Force an overwrite via POST (ungated), bypassing the PUT
   *  backwards-compatibility gate. Set after the user reviews and
   *  confirms the breaking changes. */
  overwrite?: boolean
}

export interface SidebarDeployResponse {
  success: boolean
  deploymentId?: string
  /** Async deploy/update job id to poll via {@link GetSidebarUpdateStatus}.
   *  Absent when the deploy resolved immediately (content-hash dedupe). */
  updateId?: string
  error?: string
}

export const RequestSidebarDeploy: RequestType<
  SidebarDeployParams,
  SidebarDeployResponse
> = {
  method: 'requestSidebarDeploy',
}

/** Sidebar requests undeploy */
export interface SidebarUndeployParams {
  deploymentId: string
}

export const RequestSidebarUndeploy: RequestType<
  SidebarUndeployParams,
  { success: boolean; error?: string }
> = {
  method: 'requestSidebarUndeploy',
}

/** Sidebar requests download of a deployment's sources to disk.
 * The extension owns the folder picker and disk writes; the webview
 * just sends the deployment id and surfaces the result. */
export interface SidebarDownloadDeploymentResponse {
  success: boolean
  /** Absolute path of the folder the files were written to. */
  folderPath?: string
  /** Number of files written on success. */
  fileCount?: number
  /** True when the user cancelled the folder picker or the
   * overwrite prompt — UI should stay quiet. */
  cancelled?: boolean
  error?: string
}

export const RequestSidebarDownloadDeployment: RequestType<
  { deploymentId: string },
  SidebarDownloadDeploymentResponse
> = {
  method: 'requestSidebarDownloadDeployment',
}

/** Sidebar polls deployment compilation status */
export interface SidebarDeploymentStatusResponse {
  status: 'pending' | 'compiling' | 'ready' | 'failed'
  error?: string
}

export const GetSidebarDeploymentStatus: RequestType<
  { deploymentId: string },
  SidebarDeploymentStatusResponse
> = {
  method: 'getSidebarDeploymentStatus',
}

/** Sidebar polls an async deploy/update job (POST/PUT). Distinct from
 *  the deployment's own status — the live version is unaffected until
 *  the job applies. */
export interface SidebarUpdateStatusResponse {
  status: 'compiling' | 'applied' | 'rejected'
  error?: string
}

export const GetSidebarUpdateStatus: RequestType<
  { deploymentId: string; updateId: string },
  SidebarUpdateStatusResponse
> = {
  method: 'getSidebarUpdateStatus',
}

/**
 * A deployed function's interface, normalized from jl4-service's
 * per-function schema endpoint (`GET /deployments/{id}/functions/{fn}`)
 * for recursive breaking-change detection.
 */
export interface RemoteFunctionSchema {
  name: string
  /** Full input schema (recursive: properties / items / required / enum). */
  parameters?: FunctionParameters
  /** Display name of the return type (e.g. "BOOLEAN", "DEONTIC"). */
  returnType?: string
  /** Structured schema of the return value, when the deployment exposes it. */
  returnSchema?: FunctionParameter
}

/**
 * Sidebar requests the deployed functions' full schemas for recursive
 * breaking-change detection. `functions` is `null` when the deployment
 * does not exist yet (a first deploy — nothing to break).
 */
export const GetSidebarDeploymentSchemas: RequestType<
  { deploymentId: string },
  { functions: RemoteFunctionSchema[] | null }
> = {
  method: 'getSidebarDeploymentSchemas',
}

/**
 * Sidebar asks the extension to draft an "Intended use" description for
 * the functions about to be deployed, using the summize model. The
 * extension itself surfaces the "not signed in" nudge as a VSCode
 * notification and returns `{ notSignedIn: true }` so the webview just
 * leaves the field untouched.
 */
export const GenerateSidebarIntendedUse: RequestType<
  { functions: ExportedFunctionInfo[] },
  { text: string } | { error: string } | { notSignedIn: true }
> = {
  method: 'generateSidebarIntendedUse',
}

/** Sidebar asks extension to open a URL in the browser */
export const RequestOpenUrl: NotificationType<{ url: string }> = {
  method: 'requestOpenUrl',
}

/** Sidebar asks extension to open the service URL in browser */
export const RequestOpenServiceUrl: NotificationType<void> = {
  method: 'requestOpenServiceUrl',
}

/** Sidebar asks extension to open Legalese Cloud Console */
export const RequestOpenConsole: NotificationType<void> = {
  method: 'requestOpenConsole',
}

/** Sidebar asks extension to open the extension settings */
export const RequestOpenExtensionSettings: NotificationType<void> = {
  method: 'requestOpenExtensionSettings',
}

/**
 * Local AI harnesses the extension can install the L4 Rules into. Each maps
 * to a config-file writer on the extension side (Claude Code also supports
 * the full plugin via its CLI). `download-zip` is a per-deployment-only
 * pseudo-target handled separately.
 */
export type Harness =
  | 'claude-code'
  | 'vscode'
  | 'cursor'
  | 'windsurf'
  | 'cline'
  | 'claude-desktop'

/** Display order + labels for the install dropdowns (rendered by the webview). */
export const HARNESSES: ReadonlyArray<{ id: Harness; label: string }> = [
  { id: 'claude-code', label: 'Claude Code' },
  { id: 'vscode', label: 'VS Code (Copilot)' },
  { id: 'cursor', label: 'Cursor' },
  { id: 'windsurf', label: 'Windsurf' },
  { id: 'cline', label: 'Cline' },
  { id: 'claude-desktop', label: 'Claude Desktop' },
]

/**
 * Sidebar asks the extension to install the global gateway "skills
 * marketplace" into a harness: the account-wide rules MCP server
 * (`mcp.legalese.cloud`, org resolved from auth) — and, for Claude Code, the
 * `rules@legalese-cloud` plugin (marketplace + skill) via its CLI when
 * available. Org scope comes from the user's sign-in, so no token is baked in.
 */
export const RequestInstallMarketplace: NotificationType<{ harness: Harness }> =
  {
    method: 'requestInstallMarketplace',
  }

/**
 * Sidebar asks the extension to install a specific deployment into a harness.
 * The extension downloads the plugin bundle from
 * `mcp.legalese.cloud/{slug}/{deploymentId}/.plugin` using the user's session
 * token, writes the agent skill (where the harness reads one) and the
 * per-deployment MCP server entry into that harness's config. `download-zip`
 * saves the raw bundle instead.
 *
 * Cloud mode only — every target references the hosted MCP URL.
 */
export const RequestInstallDeploymentSkill: NotificationType<{
  deploymentId: string
  target: Harness | 'download-zip'
}> = {
  method: 'requestInstallDeploymentSkill',
}

/** Sidebar asks extension to install the bundled `l4` CLI onto the user's PATH */
export const RequestInstallL4Cli: NotificationType<void> = {
  method: 'requestInstallL4Cli',
}

/** Sidebar asks extension to copy the Legalese Cloud sign-in link to clipboard */
export const RequestCopySignInLink: NotificationType<void> = {
  method: 'requestCopySignInLink',
}

/** Sidebar asks extension to disconnect (clear credentials + service URL) */
export const RequestDisconnect: NotificationType<void> = {
  method: 'requestDisconnect',
}

/** Sidebar asks extension to open a file at a specific line */
export const RequestRevealLocation: NotificationType<{
  uri: string
  line: number
}> = {
  method: 'requestRevealLocation',
}

/** Sidebar asks extension to open L4 code as a new untitled file */
export const RequestNewL4File: NotificationType<{ content: string }> = {
  method: 'requestNewL4File',
}

/** Sidebar requests markdown content for the docs tab */
export const GetSidebarDocsContent: RequestType<
  { url: string },
  { markdown: string }
> = {
  method: 'getSidebarDocsContent',
}

/** Sidebar asks extension to refresh deployments */
export const RequestRefreshDeployments: NotificationType<void> = {
  method: 'requestRefreshDeployments',
}

/** Sidebar asks extension to show a VSCode notification */
export interface ShowNotificationParams {
  type: 'info' | 'warning' | 'error'
  message: string
}

export const ShowNotification: NotificationType<ShowNotificationParams> = {
  method: 'showNotification',
}

/** Extension pushes connection status changes to sidebar */
export const SidebarConnectionStatusChanged: NotificationType<GetSidebarConnectionStatusResponse> =
  {
    method: 'sidebarConnectionStatusChanged',
  }

/*************************************************************
            Legalese AI chat tab messages
**************************************************************/

/** An OpenAI-shaped chat message. The server-side ai-proxy accepts the
 * standard OpenAI format including role "user" | "assistant" | "tool",
 * tool_calls on assistant, tool_call_id on tool. Phase 1 only exchanges
 * plain user/assistant text; later phases carry richer shapes.
 */
export interface AiChatMessage {
  role: 'system' | 'user' | 'assistant' | 'tool'
  /** String for plain-text turns (the common case). When the turn
   *  carries multimodal attachments, a user message instead ships an
   *  OpenAI-shaped array of `content parts` (see `AiChatContentPart`)
   *  that the ai-proxy translates to each provider's native block
   *  shape. */
  content: string | AiChatContentPart[] | null
  tool_calls?: Array<{
    id: string
    type: 'function'
    function: { name: string; arguments: string }
  }>
  tool_call_id?: string
  name?: string
  /** Client-only UI metadata (ignored by the server). */
  _meta?: Record<string, unknown>
}

/** OpenAI Chat Completions multimodal content parts. Kept narrow on
 *  purpose — only the two shapes we emit today. */
export type AiChatContentPart =
  | { type: 'text'; text: string }
  | { type: 'image_url'; image_url: { url: string } }
  | {
      type: 'file'
      file: { filename: string; file_data: string }
    }

/** Thin envelope around an OpenAI message list. Persists locally in the
 * extension's globalStorage and is replayed into the webview on reopen. */
export interface AiConversation {
  id: string
  orgId: string
  userId: string
  model: string
  title: string
  createdAt: string
  lastActiveAt: string
  messages: AiChatMessage[]
  /** L4 VSCode extension version that created this conversation.
   *  Stamped once at creation and never rewritten so support can trace
   *  a saved transcript back to the exact build that produced it. */
  extensionVersion?: string
  /** When set, this conversation is bound to a deployment ("Use in
   *  chat" from the Deployment tab). Stamped once at creation and
   *  never rewritten so follow-up turns — even after a webview reload
   *  or history reopen — keep routing to the same deployment endpoint
   *  rather than the default Legalese AI proxy. */
  deploymentId?: string
  /** Resolved deployment-scoped OpenAI-compatible base URL
   *  (`https://ai.legalese.cloud/{orgSlug}/{deploymentId}`). Paired
   *  with `deploymentId`; the chat-service appends `/v1/chat/completions`
   *  (and the reattach path) against this instead of `getAiEndpoint()`. */
  apiBaseUrl?: string
}

/** Lightweight row for the history overlay — avoids shipping full
 * message arrays for the listing. */
export interface AiConversationSummary {
  id: string
  title: string
  model: string
  createdAt: string
  lastActiveAt: string
  messageCount: number
  /** Present when the conversation is bound to a deployment. The
   *  history overlay renders it as a small subtitle under the title. */
  deploymentId?: string
}

/** Start (or continue) a streaming chat turn. Extension responds via
 * the AiChat* notifications below, keyed by conversationId. Omitting the
 * id starts a new conversation; the `AiChatStarted` event carries the
 * fresh id back. */
export interface AiChatStartParams {
  /** Server-assigned conversation id for follow-up turns. Omit to
   * start a new conversation; the `AiChatStarted` event carries the
   * fresh id back. */
  conversationId?: string
  /** Client-generated per-turn id, opaque to the server. Used as the
   * abort-correlation key so the webview can cancel a specific
   * in-flight request regardless of whether the server has assigned a
   * conversationId yet. */
  turnId: string
  text: string
  /** Resolved file or symbol mentions (already expanded to content
   * references client-side; extension just forwards). Phase 1 sends
   * empty list; `@` mention work arrives in a later commit. */
  mentions: Array<{ kind: 'file' | 'symbol' | 'selection'; label: string }>
  /** Multimodal attachments assembled by the webview via
   * `AiChatPickAttachment`. The extension ships these as OpenAI-shaped
   * `image_url` / `file` content parts on the user message; the
   * ai-proxy translates to the provider's native shape. `.docx` /
   * `.xlsx` are never shipped — they aren't native on either API, so
   * the picker nudges the user to save as PDF. */
  attachments: AiChatAttachment[]
  /** When false, the extension omits the per-turn `<editor-context>`
   * system message so the active file doesn't leak into context.
   * Defaults to true if unset. */
  includeActiveFile?: boolean
  /** Snapshot of the active-file chip the webview was showing at
   * send time. The extension uses this in
   * `buildEditorContextMessage` instead of re-querying
   * `vscode.window.activeTextEditor` at assemble time. Prevents
   * two divergence cases:
   *   1. Multi-window setups — each VSCode window has its own
   *      activeTextEditor, but the user reasons about "what the
   *      chip showed" in the window they last clicked on.
   *   2. Focus-change races — between the webview's last chip
   *      update and the extension assembling the request, the
   *      activeTextEditor can shift to a different file.
   *
   * Live cursor / selection / openFiles still come from the
   * extension's own window — they're inherently editor-scoped.
   * The extension only surfaces them when the snapshot's path
   * matches a visible editor (i.e. same window, same file);
   * otherwise it suppresses them so the system message is a
   * coherent point-in-time view rather than a Frankenstein. */
  activeFile?: {
    path: string
    name: string
  }
  /** Retry path: when true, the extension skips adding a user
   * message to the outgoing body and just asks the server to run
   * another turn against the conversation's existing on-disk
   * state. Used by the ErrorBubble "Retry" button so a
   * mid-stream failure doesn't double-insert the user's original
   * turn. The server-side conversation already has the user
   * message from the original turn (persisted on create), so the
   * model has what it needs. */
  continueTurn?: boolean
  /** Deployment binding for a "Use in chat" conversation. Sent by the
   *  webview only on the FIRST turn of a deployment chat; follow-up
   *  turns omit it and the extension re-resolves the binding from the
   *  persisted conversation doc (reload / history-reopen safe). When
   *  set, the extension routes this turn to `apiBaseUrl` as a plain
   *  passthrough (no local IDE context, tools, or summize title). */
  deploymentId?: string
  apiBaseUrl?: string
}

export interface AiChatAttachment {
  kind: 'image' | 'pdf'
  /** Display name (basename). */
  name: string
  /** IANA MIME type, e.g. `image/png` or `application/pdf`. */
  mediaType: string
  /** Base64-encoded file bytes (no data-URL prefix). */
  dataBase64: string
}

/** Webview asks the extension to preview a staged attachment in-editor.
 *  The extension writes the bytes to a temporary file under
 *  `globalStorage/ai/previews/` and tries to open it: for PDFs it
 *  attempts `vscode.open` (works if a PDF custom editor like
 *  `tomoki1207.pdf` is installed), falling back to
 *  `env.openExternal` (the OS default viewer). Images route straight
 *  through `vscode.open` — VSCode renders those natively. */
export const AiChatPreviewAttachment: NotificationType<{
  name: string
  mediaType: string
  dataBase64: string
}> = {
  method: 'aiChatPreviewAttachment',
}

/** Webview asks the extension to show a native file picker and return
 *  the selected file as an `AiChatAttachment`. The `accept` hint
 *  filters the picker dialog: `"text-or-pdf"` allows PDFs + common
 *  text MIME types; `"spreadsheet"` prompts the user to export as PDF
 *  first (returns null); `"any"` allows images + PDFs. Returns null on
 *  cancel. */
export const AiChatPickAttachment: RequestType<
  { accept: 'any' | 'text-or-pdf' | 'spreadsheet' },
  { attachment: AiChatAttachment | null; note?: string }
> = {
  method: 'aiChatPickAttachment',
}

export const AiChatStart: NotificationType<AiChatStartParams> = {
  method: 'aiChatStart',
}

/** Cancel an in-flight turn by its client-generated `turnId`. The
 * conversation id isn't used here because the server hasn't necessarily
 * assigned one yet when the user clicks Stop. */
export const AiChatAbort: NotificationType<{ turnId: string }> = {
  method: 'aiChatAbort',
}

/** Webview → extension: append a user message to an in-flight turn's
 * queue. The extension ends the current sub-turn at its next
 * intersection — either a natural finish (`stop`/`length`) or the
 * end of the in-flight tool round — and spawns a fresh sub-turn
 * (signalled via `AiChatTurnSpawn`) seeded with the drained user
 * text. When the prior sub-turn ended mid-tool-round, its leftover
 * `role:'tool'` results ride along on the new sub-turn's request so
 * the proxy commits a clean assistant(tool_calls) → tool(results)
 * → user sequence; otherwise the new sub-turn just carries the
 * editor-context + user delta.
 *
 * The webview is expected to render the user bubble immediately at
 * `send()` time for instant visual feedback; this notification just
 * tells the extension where to plug the text in. */
export interface AiChatInjectParams {
  /** Turn id of the in-flight turn this message attaches to. The
   *  extension uses this to look up the right queue. */
  turnId: string
  /** Webview-minted id for THIS specific injection. The extension
   *  echoes it back in `AiChatQueueConsumed` so the webview can
   *  remove the right entry from its pending-queue array (and
   *  unstyle the matching user bubble) instead of decrementing a
   *  raw counter. Lets a dropped/duplicated event fail loudly
   *  rather than silently miscount. */
  injectionId: string
  /** Conversation the in-flight turn belongs to. Used as a sanity
   *  check; the extension drops the inject if this doesn't match the
   *  active turn's conversation. */
  conversationId: string
  text: string
  mentions: Array<{ kind: 'file' | 'symbol' | 'selection'; label: string }>
  attachments: AiChatAttachment[]
  includeActiveFile?: boolean
  activeFile?: {
    path: string
    name: string
  }
}

export const AiChatInject: NotificationType<AiChatInjectParams> = {
  method: 'aiChatInject',
}

/** Extension → webview: a new sub-turn has spawned within an
 * existing turn's lifecycle because queued user messages remained
 * after the model finished naturally. The webview should mount a
 * fresh streaming assistant placeholder under `subTurnId` so abort,
 * text-deltas, and tool-call events route to it. The conversation id
 * is unchanged — only the per-turn abort key advances.
 *
 * Emitted BEFORE any `text-delta` of the new sub-turn lands so the
 * placeholder always exists by the time content arrives. */
export const AiChatTurnSpawn: NotificationType<{
  conversationId: string
  subTurnId: string
}> = {
  method: 'aiChatTurnSpawn',
}

/** Extension → webview: the listed `injectionIds` (minted by the
 * webview at send-time and echoed back here) have been drained into
 * a fresh sub-turn. The webview removes the matching entries from
 * its pending-queue array (so the user bubbles flip from greyed-out
 * to full opacity); an unack'd id stays in the array so a dropped
 * event surfaces as a stuck pipeline rather than a silent miscount. */
export const AiChatQueueConsumed: NotificationType<{
  conversationId: string
  injectionIds: string[]
}> = {
  method: 'aiChatQueueConsumed',
}

/** Extension → webview: turn started, id + model assigned. `turnId`
 * is the same client-generated key the webview shipped on
 * `AiChatStart` — echoed back so the webview can match this `started`
 * event to the exact pending buffer it belongs to. Without this, a
 * rapid sequence of new-chat submissions can land each one's
 * `started` against the WRONG local buffer (last-writer-wins on the
 * single pending slot), attaching one turn's bubbles to another
 * turn's server id. */
export const AiChatStarted: NotificationType<{
  conversationId: string
  turnId: string
  model: string
}> = {
  method: 'aiChatStarted',
}

/** Webview → extension: ask for the current active file + selection
 * synchronously. Called by the chat input at submit time so the
 * webview can render the user bubble's chip with a `:start-end`
 * range badge that matches what `<editor-context>` will end up
 * carrying — without needing a per-keystroke selection-change
 * subscription. Returns null fields when no editor is active. */
export const AiGetActiveFileSelection: RequestType<
  void,
  {
    uri: string | null
    name: string | null
    path: string | null
    inWorkspace: boolean
    selection?: { startLine: number; endLine: number }
  }
> = {
  method: 'aiGetActiveFileSelection',
}

/** Extension → webview: a client-side tool was invoked by the model.
 * Phase 2 payload carries everything the UI needs to render an inline
 * row (name, arguments) and reflect status updates without a second
 * round-trip. `status` may step through pending-approval → running →
 * done / error over the lifetime of a single callId. */
export const AiChatToolCall: NotificationType<{
  conversationId: string
  callId: string
  name: string
  argsJson: string
  status: 'pending-approval' | 'running' | 'done' | 'error'
  result?: string
  errorMessage?: string
  /** For `l4-rules__<sanitised>` calls only: the original (unsanitised)
   *  L4 function name as written in the user's source — parsed out of
   *  the MCP tool description trailer. Carries the spaces / mixed case
   *  the wire-level sanitised tool name had to drop to satisfy the
   *  `^[a-zA-Z0-9_-]{1,64}$` regex. Lets the row's display align with
   *  the server-side rule-activity card, which already shows the
   *  original. Absent for infra tools (`list_files`, etc.) and any
   *  non-rule call. */
  ruleFnName?: string
  /** Deployment id parsed from the same trailer. Lets the chat
   *  tool-call card resolve render-meta directly from the deployment
   *  without going through the IDE's sanitized MCP target map. */
  deploymentId?: string
}> = {
  method: 'aiChatToolCall',
}

/** Webview → extension: user made a decision on a pending-approval
 * tool call. `alwaysAllow` permanently bumps the matching permission
 * setting to `always`. */
export const AiChatApproveTool: NotificationType<{
  callId: string
  decision: 'allow' | 'deny' | 'alwaysAllow'
}> = {
  method: 'aiChatApproveTool',
}

export type AiPermissionCategory =
  | 'fs.read'
  | 'fs.create'
  | 'fs.edit'
  | 'fs.delete'
  | 'l4.evaluate'
  | 'l4.refactor'
  | 'mcp.l4Rules'
  | 'meta.askUser'

export type AiPermissionValue = 'never' | 'ask' | 'always'

/** Webview asks for the current permission values. */
export const AiPermissionsGet: RequestType<
  void,
  { values: Record<AiPermissionCategory, AiPermissionValue> }
> = {
  method: 'aiPermissionsGet',
}

/** Webview sets a single permission value. Extension persists to
 *  VSCode configuration under `legaleseAi.permissions.*`. */
export const AiPermissionsSet: NotificationType<{
  category: AiPermissionCategory
  value: AiPermissionValue
}> = {
  method: 'aiPermissionsSet',
}

/** Chat-panel UI preferences. Persisted on the extension side via
 *  `vscode.workspace.getConfiguration` under `legaleseAi.<key>`, so
 *  Settings Sync carries them across machines — same mechanism as
 *  permissions. New entries: add a field here, a default in the
 *  webview store, and a key mapping in the extension handler. */
export interface AiPreferences {
  showReasoning: boolean
  /** Free-text methodology the user wants the model to follow. When
   *  non-empty, it's injected as a `<methodology>` system message just
   *  before the first user prompt of a new (non-deployment)
   *  conversation. Empty string means "no methodology". */
  methodology: string
}

/** Webview asks for all current preference values. */
export const AiPreferencesGet: RequestType<void, { values: AiPreferences }> = {
  method: 'aiPreferencesGet',
}

/** Webview pushes a partial preference update. */
export const AiPreferencesSet: NotificationType<{
  values: Partial<AiPreferences>
}> = {
  method: 'aiPreferencesSet',
}

/** Webview asks the extension for the L4 render-meta of an MCP tool.
 *  The extension parses `[deployId/fnName]` from the cached MCP tool
 *  description, fetches `/deployments/{id}/functions/{fn}` (cached by
 *  deployment version), and returns the structured schemas with
 *  `x-l4-type` annotations recursively. Used by the chat tool-call
 *  card to render JSON arguments back into L4 syntax. Returns
 *  `{ kind: 'unavailable' }` if the tool isn't an l4-rules rule, or
 *  if the fetch fails — the caller falls back to plain JSON view. */
export const AiToolRenderMeta: RequestType<
  {
    toolName: string
    /** Server-side rule activities (deployment passthrough chats)
     *  carry the deployment + L4 function name directly. When set,
     *  the extension resolves the schema straight from
     *  `/deployments/{deploymentId}/functions/{fnName}` instead of
     *  the IDE's sanitized MCP target map (which need not cover the
     *  cloud deployment the chat is bound to). */
    deploymentId?: string
    fnName?: string
  },
  | {
      kind: 'meta'
      parameters: FunctionParameter
      returnSchema?: FunctionParameter
    }
  | { kind: 'unavailable' }
> = {
  method: 'aiToolRenderMeta',
}

/** Extension → webview: a server-side tool activity event (from the
 * ai-proxy's backend tools like `search_l4_docs`). Deduped client-side
 * by `tool`+`status` so long sequences collapse into a single row. */
export const AiChatToolActivity: NotificationType<{
  conversationId: string
  tool: string
  status: 'running' | 'done' | 'error'
  /** Bold action prefix the webview shows in front of `message`
   *  (e.g. "L4 Deployments", "Compacting...", "Legalesing...",
   *  "L4 Rule"). Set by the proxy so the webview doesn't need a
   *  per-tool name → label map. Absent on activities emitted by
   *  older proxy builds — webview falls back to a sane default. */
  label?: string
  message: string
  /** Verbatim model-supplied arguments — present only for L4 Rule
   *  activities (the proxy emits this only when its descriptor is
   *  `kind: "rule"`). Combined with `ruleId` (or `evaluate_rule`'s
   *  `input.function_name`), the webview renders this activity as an
   *  L4 Rule card identical to a client-side tool-call instead of the
   *  minimal status row. */
  input?: unknown
  /** Verbatim rule result (set on `done`). L4 Rule activities only. */
  output?: unknown
  /** Deployed L4 function name when the activity wraps a rule. */
  ruleId?: string
  /** Deployment the rule lives in, when scoped. */
  deploymentId?: string
  /** Error detail when status is `error`. */
  error?: string
  /** URL citations carried by a synthetic `web_search` activity — the
   *  upstream provider ran a native web search (Anthropic's
   *  `web_search_20250305` via OpenRouter) and the proxy aggregated
   *  the resulting `url_citation` annotations into this list. Empty /
   *  absent on every other activity; the webview renders these as the
   *  "Sources:" section of the turn's review card. */
  sources?: Array<{ url: string; title?: string }>
}> = {
  method: 'aiChatToolActivity',
}

/** Extension → webview: seed the chat input with text from an outside
 * entry point (e.g. the "Ask Legalese AI about this" editor code
 * action). The webview calls `setDraft` with this string. */
export const AiChatSeedDraft: NotificationType<{
  text: string
}> = {
  method: 'aiChatSeedDraft',
}

/** Extension → webview: the model invoked `meta__ask_user`. The webview
 * renders a question card above the in-progress assistant text in the
 * conversation the call belongs to. `conversationId` is required so
 * the store can route the card to the correct conversation even if the
 * user has since flipped to another one via the history panel. */
export const AiChatAskUser: NotificationType<{
  conversationId: string
  callId: string
  question: string
  /** Optional fixed set of choices; when empty the UI renders a
   *  free-form input. */
  choices?: string[]
}> = {
  method: 'aiChatAskUser',
}

/** Webview → extension: the user answered a meta__ask_user question.
 * Pass an empty string for `answer` to signal "skip / use your best
 * guess". */
export const AiChatAnswerUser: NotificationType<{
  callId: string
  answer: string
}> = {
  method: 'aiChatAnswerUser',
}

/** Webview → extension: open the target file of a tool call in a
 * regular editor tab. Used for `fs__read_file` and `fs__create_file`
 * when the user cmd+clicks the filename. */
export const AiFileOpen: NotificationType<{
  callId: string
}> = {
  method: 'aiFileOpen',
}

/** Webview → extension: open a VSCode diff editor showing the applied
 * delta of a `fs__edit_file` (or `fs__create_file`) call. The "before"
 * side is the pre-run snapshot the dispatcher captured, and the "after"
 * side is the current on-disk file, so the gutter paints red/green for
 * the changes that were actually written. */
export const AiFileOpenDiff: NotificationType<{
  callId: string
}> = {
  method: 'aiFileOpenDiff',
}

/** Extension → webview: the AI chat's view of the active editor file.
 * Pushed on activeTextEditor changes. `name` is the display basename;
 * `path` is the workspace-relative path (or absolute if the file is
 * outside every loaded workspace folder). `inWorkspace` flips to
 * false for outside-workspace files — the fs tools refuse those, and
 * the UI can surface that to the user. Selection range is NOT pushed
 * here; the webview fetches it on demand via
 * `AiGetActiveFileSelection` at submit time. */
export const AiActiveFile: NotificationType<{
  uri: string | null
  name: string | null
  path: string | null
  inWorkspace: boolean
}> = {
  method: 'aiActiveFile',
}

/** Extension → webview: incremental assistant text. */
export const AiChatTextDelta: NotificationType<{
  conversationId: string
  text: string
}> = {
  method: 'aiChatTextDelta',
}

/** Extension → webview: incremental reasoning / thinking text. Rendered
 * as a collapsed-by-default block that expands to show italic gray
 * text. Forwarded from the ai-proxy's `event: thinking_delta` SSE
 * frames. */
export const AiChatThinkingDelta: NotificationType<{
  conversationId: string
  text: string
}> = {
  method: 'aiChatThinkingDelta',
}

/** Extension → webview: turn finished. `finishReason` matches the
 * OpenAI set: 'stop' | 'tool_calls' | 'length' | 'content_filter' | 'error'.
 */
export const AiChatDone: NotificationType<{
  conversationId: string
  finishReason: string
  usage?: { promptTokens: number; completionTokens: number }
}> = {
  method: 'aiChatDone',
}

/** Extension → webview: something went wrong mid-turn. `code` is
 * optional and maps to http codes / OpenAI error codes for clients to
 * switch on (`daily_token_limit_exceeded`, `unauthenticated`, etc.). */
export const AiChatError: NotificationType<{
  conversationId: string
  message: string
  code?: string
}> = {
  method: 'aiChatError',
}

/** Webview asks for the list of locally-tracked conversations. */
export const AiConversationList: RequestType<
  void,
  { items: AiConversationSummary[] }
> = {
  method: 'aiConversationList',
}

/** Load a single conversation by id from local storage. */
export const AiConversationLoad: RequestType<
  { id: string },
  { conversation: AiConversation | null }
> = {
  method: 'aiConversationLoad',
}

/** Rename-delete locally + on the server. */
export const AiConversationDelete: RequestType<
  { id: string },
  { ok: boolean }
> = {
  method: 'aiConversationDelete',
}

/** Webview asks for a fresh conversation id (drops local state; starts
 * blank). Currently a notification because no response is needed —
 * state transitions happen in the webview's own store. */
export const AiConversationNew: NotificationType<void> = {
  method: 'aiConversationNew',
}

/** Push from extension: auth state changed (signed-in, cloud session
 * status). The AI tab uses this to enable/disable input. Separate from
 * `SidebarConnectionStatusChanged` because the AI tab cares about
 * cloud-specific auth, not generic service-url reachability. */
export const AiAuthStatus: NotificationType<{
  signedIn: boolean
  userId?: string
  orgSlug?: string
}> = {
  method: 'aiAuthStatus',
}

/** `@` mention autocomplete: returns matching files / workspace symbols.
 * Phase 1 populates this from the active editor + workspace file
 * search; later we augment with exported symbol names. */
export interface AiMentionCandidate {
  kind: 'file' | 'symbol' | 'selection'
  label: string
  /** URI for file candidates; export name for symbols; empty for selection. */
  target: string
}

export const AiMentionSearch: RequestType<
  { query: string },
  { items: AiMentionCandidate[] }
> = {
  method: 'aiMentionSearch',
}

/** Usage polling: webview tells the extension when to poll (tab
 * visible + conversation loaded → subscribe; otherwise unsubscribe). */
export const AiUsageSubscribe: NotificationType<void> = {
  method: 'aiUsageSubscribe',
}

export const AiUsageUnsubscribe: NotificationType<void> = {
  method: 'aiUsageUnsubscribe',
}

export const AiUsageUpdate: NotificationType<{
  used: number
  limit: number
  blockOnOverage: boolean
}> = {
  method: 'aiUsageUpdate',
}

/*******************************************************************************
 Convert between L4 RPC types and vscode-messenger's Request/Notification types
********************************************************************************/

export function toWebviewMessengerRequestType<P extends object, R>(
  requestType: L4RpcRequestType<P, R>
): RequestType<P, R> {
  return {
    method: requestType.method,
  }
}

export function fromWebviewMessengerRequestType<P extends object, R>(
  requestType: RequestType<P, R>
): L4RpcRequestType<P, R> {
  return makeL4RpcRequestType(requestType.method)
}

export function toWebviewMessengerNotificationType<P extends object>(
  notificationType: L4RpcNotificationType<P>
): NotificationType<P> {
  return {
    method: notificationType.method,
  }
}

export function fromWebviewMessengerNotificationType<P extends object>(
  notificationType: NotificationType<P>
): L4RpcNotificationType<P> {
  return makeL4RpcNotificationType(notificationType.method)
}
