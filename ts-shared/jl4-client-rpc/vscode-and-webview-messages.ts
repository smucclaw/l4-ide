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

/** Connection status response from extension to sidebar */
export interface GetSidebarConnectionStatusResponse {
  serviceUrl: string
  connected: boolean
  status: 'connected' | 'not-configured' | 'connecting' | 'error'
  isLegaleseCloud: boolean
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
}

export interface SidebarDeployResponse {
  success: boolean
  deploymentId?: string
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

/** Sidebar requests deployment OpenAPI spec (for breaking change detection) */
export const GetSidebarDeploymentOpenApi: RequestType<
  { deploymentId: string },
  { openapi: unknown }
> = {
  method: 'getSidebarDeploymentOpenApi',
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

/** Sidebar asks extension to add L4 tools (MCP server + skill) to Claude Code */
export const RequestAddL4ToolsToClaudeCode: NotificationType<void> = {
  method: 'requestAddL4ToolsToClaudeCode',
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

/** Webview asks the extension for the L4 render-meta of an MCP tool.
 *  The extension parses `[deployId/fnName]` from the cached MCP tool
 *  description, fetches `/deployments/{id}/functions/{fn}` (cached by
 *  deployment version), and returns the structured schemas with
 *  `x-l4-type` annotations recursively. Used by the chat tool-call
 *  card to render JSON arguments back into L4 syntax. Returns
 *  `{ kind: 'unavailable' }` if the tool isn't an l4-rules rule, or
 *  if the fetch fails — the caller falls back to plain JSON view. */
export const AiToolRenderMeta: RequestType<
  { toolName: string },
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
  message: string
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
