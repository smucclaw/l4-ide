import type { Messenger } from 'vscode-messenger-webview'
import { HOST_EXTENSION } from 'vscode-messenger-common'
import {
  AiChatAbort,
  AiChatAnswerUser,
  AiChatApproveTool,
  AiChatInject,
  AiChatPickAttachment,
  AiChatPreviewAttachment,
  AiChatStart,
  AiConversationDelete,
  AiConversationList,
  AiConversationLoad,
  AiFileOpen,
  AiFileOpenDiff,
  AiGetActiveFileSelection,
  AiMentionSearch,
  AiPermissionsGet,
  AiPermissionsSet,
  AiUsageSubscribe,
  AiUsageUnsubscribe,
  type AiChatAttachment,
  type AiConversation,
  type AiConversationSummary,
  type AiChatMessage,
  type AiChatStartParams,
  type AiMentionCandidate,
  type AiPermissionCategory,
  type AiPermissionValue,
} from 'jl4-client-rpc'
import { aiPrefs } from '$lib/stores/ai-prefs.svelte'

export interface PendingQuestion {
  callId: string
  question: string
  choices?: string[]
}

export interface ActiveFileInfo {
  /** Editor basename, e.g. "insurance-premium.l4". Null when no file is
   *  open. */
  name: string | null
  /** Workspace-relative path (or absolute if outside every workspace
   *  folder). Null when no file is open. */
  path: string | null
  inWorkspace: boolean
}

/**
 * A single turn that's either already stored in the conversation
 * history (role user/assistant/tool) or a transient "in-flight"
 * assistant bubble that the UI is currently streaming into.
 */
export interface RenderedTurn {
  id: string
  /** Client-generated per-turn id used for abort correlation. Present
   * on assistant turns so Stop can cancel the right in-flight request
   * even before the server assigns a conversationId. Absent on user
   * turns (nothing to cancel). */
  turnId?: string
  /** For user turns submitted DURING an in-flight pipeline (the
   *  inject path): the webview-minted id we shipped in the
   *  AiChatInject notification. The store keeps the matching entry
   *  in `conv.queuedInjections` until the extension echoes the id
   *  back via `queue-consumed`; the bubble renders greyed-out and
   *  is excluded from the scroll-sticky set while the entry is
   *  live. Absent on the initial user turn of a sub-turn (those go
   *  straight into the request, never queued) and on assistant
   *  turns. */
  injectionId?: string
  role: 'user' | 'assistant'
  /** Concatenated assistant text for retry / copy / persistence. For
   * display the UI iterates `blocks` instead, which preserves the
   * interleaving of text and tool calls as they arrived. */
  content: string
  /** True while tokens are still streaming into this bubble. */
  streaming?: boolean
  /** Populated on failure; the bubble shows it as an error footer. */
  error?: { message: string; code?: string }
  /** Chronological render log of what the model emitted on this turn.
   *  A text block grows as tokens arrive; a new tool-call block opens
   *  when the model invokes a tool; once the tool result lands, the
   *  next delta opens a fresh text block. Only populated on assistant
   *  turns. */
  blocks?: AssistantBlock[]
  /** Chips shown at the top of a user message — the attachments and
   *  (if enabled) the active file that went with this turn's request.
   *  Set once at submit time from the staged state in the chat input;
   *  never mutated afterwards. Only populated on user turns. */
  chips?: UserTurnChip[]
  /** Per-turn token totals from the server's terminal chunk, surfaced
   *  on the assistant bubble as a small badge so users can see
   *  expensive turns + catch quota drift before the 429. */
  usage?: { promptTokens: number; completionTokens: number }
}

/** A chip echoed at the top of a user message — mirrors what was
 *  staged in the chat input at submit time: PDF / image attachments
 *  and (when the active-file toggle was on) the active editor file.
 *  Kept minimal because these are UI-only badges, not re-usable
 *  payloads. The optional selection range on `active-file` is the
 *  range the chip was advertising at send time (1-based, inclusive). */
export type UserTurnChip =
  | { kind: 'image' | 'pdf'; name: string }
  | {
      kind: 'active-file'
      name: string
      path: string
      selectionStartLine?: number
      selectionEndLine?: number
    }

export type AssistantBlock =
  | { kind: 'text'; text: string }
  | { kind: 'tool-call'; call: RenderedToolCall }
  | { kind: 'tool-activity'; activity: RenderedToolActivity }
  | { kind: 'thinking'; text: string }

export interface RenderedToolActivity {
  /** Keyed tool name (server-side, e.g. `search_l4_docs`). */
  tool: string
  /** Most recent status across the merged run — `done`/`error` stick. */
  status: 'running' | 'done' | 'error'
  /** Bold action prefix the row shows in front of `message`. Stamped
   *  by the proxy ("L4 Deployments", "Compacting...", "Legalesing...").
   *  Absent for activities emitted by older proxy builds — the row
   *  falls back to a default label. */
  label?: string
  /** Most recent message; prior messages of the same run are dropped
   *  once the next one arrives. */
  message: string
  /** Verbatim model-supplied arguments — present only for inspectable
   *  server tools that wrap an L4 rule evaluation. Drives the L4 Rule
   *  card render (see {@link ruleKey}). */
  input?: unknown
  /** Verbatim tool result, set once the run reaches `done`. */
  output?: unknown
  /** Deployed L4 function name when the activity wraps a rule. */
  ruleId?: string
  /** Deployment the rule lives in, when scoped. */
  deploymentId?: string
  /** Error detail when status is `error`. */
  error?: string
  /** Identity of the rule run, used to merge a `running → done`
   *  burst into ONE mutating row (mirrors how tool-call rows update
   *  by callId). Set only for rule activities; `undefined` for plain
   *  status activities, which keep the legacy tool+message dedupe. */
  ruleKey?: string
  /** URL citations carried by a synthetic `web_search` activity. The
   *  message-assistant component aggregates these across the turn and
   *  renders them as the "Sources:" section of the review card. */
  sources?: Array<{ url: string; title?: string }>
}

/** Identify an L4-rule tool activity and compute its merge key + the
 *  deployed function name. Returns null for non-rule activities (plain
 *  status tickers like `search_l4_docs`), which keep the minimal row.
 *
 *  Two shapes carry a rule:
 *    - direct MCP rule descriptors → `ruleId` is set by the proxy.
 *    - the generic `evaluate_rule` tool → rule name lives in
 *      `input.function_name` (proxy can't know it statically).
 *  Both must also carry `input` so there's something to render. */
function ruleActivityIdentity(a: {
  tool: string
  ruleId?: string
  input?: unknown
}): { ruleKey: string; ruleName: string } | null {
  const fromInput =
    a.input && typeof a.input === 'object'
      ? (a.input as { function_name?: unknown }).function_name
      : undefined
  const ruleName =
    a.ruleId ?? (typeof fromInput === 'string' ? fromInput : undefined)
  if (!ruleName) return null
  return { ruleKey: `${a.tool}::${ruleName}`, ruleName }
}

export interface RenderedToolCall {
  callId: string
  name: string
  argsJson: string
  status: 'pending-approval' | 'running' | 'done' | 'error'
  result?: string
  error?: string
  /** Set when this row is projected from a server-side L4-rule
   *  activity: lets the render-meta lookup resolve the schema
   *  straight from the deployment instead of the IDE MCP map. */
  deploymentId?: string
  ruleFnName?: string
}

interface ConversationState {
  /** Server-assigned id once the `started` event lands; null before. */
  id: string | null
  /** Title from the last save (or the first user message until summize returns). */
  title: string | null
  /** Ordered list of message turns in the order they should render. */
  turns: RenderedTurn[]
  /** True while an in-flight stream is attached to this conversation. */
  streaming: boolean
  /** Active turnId for any in-flight stream. Drives the inject path:
   *  when the user submits while `streaming` is true, the new
   *  message rides on this id rather than starting a fresh
   *  `AiChatStart`. Null between turns. */
  activeTurnId: string | null
  /** Set when this conversation is bound to a deployment ("Use in
   *  chat"). Mirrors the persisted `AiConversation` fields so the
   *  banner survives a history reopen and follow-up turns keep the
   *  binding. */
  deploymentId?: string
  apiBaseUrl?: string
  /** The deployment's "Intended use" text (its metadata
   *  `description`). Not persisted to disk — only carried in-memory
   *  so a fresh deployment chat's empty-state can show it. Reopened
   *  conversations already have messages, so the empty-state (and
   *  thus this field) is irrelevant for them. */
  intendedUse?: string
  /** Pending user-message injections — one entry per `AiChatInject`
   *  the webview has dispatched but the extension has not yet
   *  echoed back via `queue-consumed`. Each entry carries the
   *  injection's id (the correlation key the extension echoes) and
   *  the corresponding user-bubble id (so we can grey out / unstyle
   *  the matching turn). Length drives the `pipelineActive` gate
   *  and the scroll-sticky exclusion set. The whole array clears on
   *  `abort()` and on `onError()` since both reasons halt the
   *  pipeline server-side. */
  queuedInjections: Array<{ injectionId: string; userTurnId: string }>
}

/** Parse `_meta.blocks` saved by chat-service into the webview's
 *  AssistantBlock shape. Returns null if the meta has no blocks. Shapes
 *  that don't match are dropped so a partially-written history entry
 *  still renders as text. */
function extractPersistedBlocks(meta: unknown): AssistantBlock[] | null {
  if (!meta || typeof meta !== 'object') return null
  const raw = (meta as { blocks?: unknown }).blocks
  if (!Array.isArray(raw) || raw.length === 0) return null
  const out: AssistantBlock[] = []
  for (const b of raw as Array<Record<string, unknown>>) {
    if (b.kind === 'text' && typeof b.text === 'string') {
      out.push({ kind: 'text', text: b.text })
    } else if (
      b.kind === 'tool-call' &&
      typeof b.callId === 'string' &&
      typeof b.name === 'string' &&
      typeof b.argsJson === 'string'
    ) {
      const status =
        b.status === 'done' || b.status === 'error' ? b.status : 'done'
      out.push({
        kind: 'tool-call',
        call: {
          callId: b.callId,
          name: b.name,
          argsJson: b.argsJson,
          status,
          result: typeof b.result === 'string' ? b.result : undefined,
          error: typeof b.error === 'string' ? b.error : undefined,
          ruleFnName:
            typeof b.ruleFnName === 'string' ? b.ruleFnName : undefined,
          deploymentId:
            typeof b.deploymentId === 'string' ? b.deploymentId : undefined,
        },
      })
    } else if (
      b.kind === 'tool-activity' &&
      typeof b.tool === 'string' &&
      typeof b.ruleId === 'string' &&
      typeof b.ruleKey === 'string'
    ) {
      // Persisted L4-rule activity. `ruleKey` being present is what
      // routes it back through <ToolCallRow> on render (same gate as
      // a live rule activity). Non-terminal statuses coerce to `done`
      // — same rule as tool-call blocks above; a Stop'd eval was
      // already rewritten to `error` at persist time
      // (markStoppedToolCalls), so anything still `running` here is a
      // crash-truncated transcript, best shown as a settled card
      // rather than a row that pulsates forever.
      const status =
        b.status === 'done' || b.status === 'error' ? b.status : 'done'
      out.push({
        kind: 'tool-activity',
        activity: {
          tool: b.tool,
          status,
          message: typeof b.message === 'string' ? b.message : '',
          input: b.input,
          output: b.output,
          ruleId: b.ruleId,
          deploymentId:
            typeof b.deploymentId === 'string' ? b.deploymentId : undefined,
          error: typeof b.error === 'string' ? b.error : undefined,
          ruleKey: b.ruleKey,
        },
      })
    } else if (b.kind === 'tool-activity' && b.tool === 'web_search') {
      // Persisted web-search activity. No rule fields — the only
      // payload that needs to survive a reload is the citations list;
      // the dot itself is rendered as the standard plain status row.
      const status =
        b.status === 'done' || b.status === 'error' ? b.status : 'done'
      const sources = Array.isArray(b.sources)
        ? (b.sources as Array<{ url?: unknown; title?: unknown }>)
            .filter(
              (s) =>
                s && typeof s.url === 'string' && (s.url as string).length > 0
            )
            .map((s) => ({
              url: s.url as string,
              ...(typeof s.title === 'string' ? { title: s.title } : {}),
            }))
        : undefined
      out.push({
        kind: 'tool-activity',
        activity: {
          tool: 'web_search',
          status,
          label: 'Web search',
          message: typeof b.message === 'string' ? b.message : '',
          sources,
        },
      })
    }
  }
  return out.length > 0 ? out : null
}

/**
 * Rune-backed store for the AI tab. Exposes reactive state (current
 * conversation, history list, token usage) and the actions needed to
 * drive the UI. One instance per webview.
 */
export function createAiChatStore(
  getMessenger: () => Messenger | null
): AiChatStore {
  // Server-assigned id of the conversation the user is currently
  // viewing, or null when they're composing a fresh chat that hasn't
  // received its id from the proxy yet. Only ever holds real server
  // ids — the not-yet-started state lives in `pendingConversation`
  // below.
  let currentId = $state<string | null>(null)
  // Plain-object bag (not Map). Svelte 5's `$state` deep-proxies object
  // properties, so nested mutations (`conv.streaming = false`) reliably
  // notify reactive reads. Reactive Map does not proxy values returned
  // from .get(), which made `$derived(store.current?.streaming)` go
  // stale after abort. Only keyed by real server ids — see
  // `pendingConversation` for the buffer used between local submit and
  // `AiChatStarted`.
  const conversations = $state<Record<string, ConversationState>>({})
  // Buffer for a conversation whose first turn is in flight but
  // hasn't been assigned a server id yet. Holds the user bubble and
  // the streaming assistant placeholder; `onStarted` promotes it into
  // `conversations[realId]` and clears the slot. At most one of these
  // can exist; `newConversation()` wipes it. Decoupling the
  // pre-started state from `conversations` keeps the latter clean of
  // sentinels and lets every event handler look up by real id without
  // a magic-string fallback.
  let pendingConversation = $state<ConversationState | null>(null)
  let history = $state<AiConversationSummary[]>([])
  let usedToday = $state<number>(0)
  let dailyLimit = $state<number>(0)
  let blockOnOverage = $state<boolean>(false)
  let signedIn = $state<boolean>(false)
  // Active deployment binding ("Use in chat" from the Deployment tab).
  // Single source of truth for the deployment banner and for stamping
  // the binding onto the first `AiChatStart` of the conversation.
  // Set by `startDeploymentChat` / restored by `loadConversation`;
  // cleared by `newConversation` (closing the banner opens a fresh
  // chat against the default endpoint again).
  let deploymentBinding = $state<{
    deploymentId: string
    apiBaseUrl: string
    /** Deployment "Intended use" text, shown in the empty-state of a
     *  fresh deployment chat. May be empty — the UI falls back to a
     *  generic prompt then. */
    intendedUse?: string
  } | null>(null)
  const draftsByConv = $state<Record<string, string>>({})
  // Draft for the not-yet-started conversation. Mirrors
  // `pendingConversation`'s lifecycle: lives while the user is
  // composing for a fresh chat, gets cleared by a successful submit
  // or by `newConversation()`. Keeping it out of `draftsByConv`
  // means the map only ever holds drafts keyed by real server ids.
  let pendingDraft = $state<string>('')
  // Version counter bumped whenever an *external* caller (Get Started
  // seed button, right-click "Ask Legalese AI about this" command, the
  // AiChatSeedDraft host notification) writes the draft via
  // `seedDraft`. Chat-input watches this alongside `currentId` to
  // decide when to resync its local `text` binding from the store.
  // Keeping the counter separate from `setDraft` (which the textarea's
  // own oninput calls) means keystrokes never retrigger the sync and
  // can't race with a stale getDraft() read.
  let draftSeedVersion = $state<number>(0)
  let activeFile = $state<ActiveFileInfo>({
    name: null,
    path: null,
    inWorkspace: false,
  })
  // Whether the active file's `<editor-context>` block should be
  // appended to the next send. Persists across conversations — a
  // user who toggles it off wants it off for subsequent prompts too,
  // until they toggle it back on.
  let includeActiveFile = $state<boolean>(true)
  // Pending meta__ask_user questions awaiting an answer, keyed by the
  // conversationId the call belongs to. Scoped per-conversation so
  // switching chats via the history panel doesn't surface another
  // conversation's card against the wrong turns. `currentId`'s entry
  // is surfaced via `pendingQuestion`.
  const pendingQuestionsByConv = $state<Record<string, PendingQuestion>>({})
  // Also track which conversationId originated a given callId so
  // answerQuestion / skip paths can find the right bucket even when
  // the user has navigated away from the originating chat.
  const pendingQuestionConvByCallId = $state<Record<string, string>>({})
  // Attachments the user has staged for the next send. Cleared after a
  // successful dispatch (mirrors the stagedMentions flow). PDFs and
  // images only; the extension refuses spreadsheets.
  let stagedAttachments = $state<AiChatAttachment[]>([])

  function ensureCurrent(): ConversationState {
    if (currentId) return conversations[currentId]!
    if (!pendingConversation) {
      pendingConversation = {
        id: null,
        title: null,
        turns: [],
        streaming: false,
        activeTurnId: null,
        queuedInjections: [],
        // Mirror the active deployment binding onto the in-memory
        // state at creation. Without this the binding lives only on
        // the persisted disk doc, so re-opening this conversation
        // from history while it's still cached (the common same-
        // session case) hits `syncDeploymentBinding(cached)` with
        // an unbound state and silently drops the banner + endpoint
        // routing. The object is promoted as-is into
        // `conversations[id]` by `onStarted`, so stamping it here
        // carries it through for the whole conversation lifetime.
        ...(deploymentBinding
          ? {
              deploymentId: deploymentBinding.deploymentId,
              apiBaseUrl: deploymentBinding.apiBaseUrl,
              ...(deploymentBinding.intendedUse
                ? { intendedUse: deploymentBinding.intendedUse }
                : {}),
            }
          : {}),
      }
    }
    return pendingConversation
  }

  function getConversation(): ConversationState | null {
    if (currentId) return conversations[currentId] ?? null
    return pendingConversation
  }

  /** Clear the draft for whichever conversation the user is currently
   *  composing in — established (keyed in draftsByConv) or pending
   *  (the standalone slot). Called after a successful submit. */
  function clearCurrentDraft(): void {
    if (currentId) delete draftsByConv[currentId]
    else pendingDraft = ''
  }

  async function refreshHistory(): Promise<void> {
    const m = getMessenger()
    if (!m) return
    try {
      const res = await m.sendRequest(
        AiConversationList,
        HOST_EXTENSION,
        undefined as never
      )
      history = res.items
    } catch {
      // ignore
    }
  }

  async function loadConversation(id: string): Promise<void> {
    const m = getMessenger()
    if (!m) return
    // If we already have an in-memory state for this conversation,
    // prefer it over whatever is on disk. Two reasons:
    //   1. A stream might still be attached — the in-flight text /
    //      tool-call deltas live in memory only; replacing with the
    //      disk snapshot would clobber the live turn and freeze the
    //      UI for the rest of the stream.
    //   2. Even after a turn completes, the disk snapshot isn't
    //      authoritative for the in-memory `blocks` shape (the chat
    //      service writes it via `persistAssistantTurn` at
    //      end-of-turn; the extension-host's event log is richer in
    //      the meantime). Using the in-memory state keeps the
    //      rendered history consistent with what the user saw while
    //      the turn was running.
    // Only when we have no in-memory state at all do we fetch from
    // disk — that covers app restarts and conversations authored on
    // another machine.
    const cached = conversations[id]
    if (cached) {
      currentId = id
      includeActiveFile = false
      syncDeploymentBinding(cached)
      return
    }
    try {
      const res = await m.sendRequest(AiConversationLoad, HOST_EXTENSION, {
        id,
      })
      const conv = res.conversation
      if (!conv) return
      const state: ConversationState = {
        id: conv.id,
        title: conv.title,
        turns: conv.messages
          .filter((m) => m.role === 'user' || m.role === 'assistant')
          .map((m, i) => {
            const content = typeof m.content === 'string' ? m.content : ''
            const turn: RenderedTurn = {
              id: `${conv.id}:${i}`,
              role: m.role as 'user' | 'assistant',
              content,
            }
            if (m.role === 'assistant') {
              const persisted = extractPersistedBlocks(m._meta)
              if (persisted) turn.blocks = persisted
              else if (content) turn.blocks = [{ kind: 'text', text: content }]
            }
            return turn
          }),
        streaming: false,
        activeTurnId: null,
        queuedInjections: [],
        ...(conv.deploymentId ? { deploymentId: conv.deploymentId } : {}),
        ...(conv.apiBaseUrl ? { apiBaseUrl: conv.apiBaseUrl } : {}),
      }
      conversations[conv.id] = state
      currentId = conv.id
      // Loading an existing conversation is not a fresh start — default
      // to off so a follow-up turn doesn't silently re-ship the file.
      includeActiveFile = false
      syncDeploymentBinding(state)
    } catch {
      // ignore
    }
  }

  async function deleteConversation(id: string): Promise<void> {
    const m = getMessenger()
    if (!m) return
    try {
      await m.sendRequest(AiConversationDelete, HOST_EXTENSION, { id })
    } catch {
      // ignore
    }
    delete conversations[id]
    if (currentId === id) currentId = null
    await refreshHistory()
  }

  /** Align the active deployment binding with the conversation the
   *  user just switched to: a deployment-bound conversation re-arms
   *  the banner + endpoint routing; a normal one clears it (so the
   *  next turn goes to the default endpoint). */
  function syncDeploymentBinding(state: ConversationState): void {
    deploymentBinding =
      state.deploymentId && state.apiBaseUrl
        ? {
            deploymentId: state.deploymentId,
            apiBaseUrl: state.apiBaseUrl,
            ...(state.intendedUse ? { intendedUse: state.intendedUse } : {}),
          }
        : null
  }

  /** Start a fresh conversation bound to a deployment. The binding is
   *  sent on the first `AiChatStart` and persisted on the conversation
   *  doc; closing the banner (`newConversation`) drops it.
   *  `intendedUse` is the deployment's metadata description — shown in
   *  the empty-state until the first message is sent. */
  function startDeploymentChat(
    deploymentId: string,
    apiBaseUrl: string,
    intendedUse?: string
  ): void {
    newConversation()
    deploymentBinding = {
      deploymentId,
      apiBaseUrl,
      ...(intendedUse ? { intendedUse } : {}),
    }
    // `newConversation()` defaults the active-file attach on; a
    // deployment passthrough chat can't use IDE context, so clear it
    // so no other UI reads a stale "attached" state.
    includeActiveFile = false
  }

  function newConversation(): void {
    currentId = null
    pendingConversation = null
    deploymentBinding = null
    // Intentionally leave `pendingDraft` alone — original behavior:
    // an unsent draft survives a "new chat" click (mirrors the way
    // `draftsByConv['__new__']` used to persist across deletions of
    // the conversations bucket). A successful submit clears it via
    // `clearCurrentDraft()`.
    // Fresh conversation → default to attaching the editor file with
    // the first message.
    includeActiveFile = true
  }

  async function send(
    text: string,
    mentions: AiChatStartParams['mentions'] = []
  ): Promise<void> {
    const m = getMessenger()
    if (!m || !text.trim()) return
    const conv = ensureCurrent()

    // A deployment-bound chat is a plain passthrough to the
    // deployment's model — it has no IDE tools and the extension
    // strips editor context for it anyway. Force the active-file
    // attachment off here so the user bubble shows no file chip and
    // nothing (chip, snapshot, or `includeActiveFile` flag) is sent.
    const wasIncludingActiveFile = includeActiveFile && !deploymentBinding

    // Fire a one-shot request to learn the freshest active-file
    // state (including the current selection range) so the user
    // bubble's chip can render `:start-end` if the user had text
    // selected at submit time. Falls back to the cached chip state
    // if the round-trip fails. Skipped entirely when the active-
    // file chip toggle is off this turn.
    let chipPath: string | null = null
    let chipName: string | null = null
    let chipSelection: { startLine: number; endLine: number } | undefined
    if (wasIncludingActiveFile) {
      try {
        const fresh = await m.sendRequest(
          AiGetActiveFileSelection,
          HOST_EXTENSION,
          undefined as never
        )
        chipPath = fresh.path
        chipName = fresh.name
        chipSelection = fresh.selection
      } catch {
        chipPath = activeFile.path
        chipName = activeFile.name
      }
    }

    // Snapshot the staged-context chips — attachments + (when the
    // chip was on) the active file — so the user message can echo
    // what actually went to the model. Plain objects to dodge the
    // $state-proxy postMessage clone issue elsewhere.
    const turnChips: UserTurnChip[] = []
    for (const att of stagedAttachments) {
      turnChips.push({ kind: att.kind, name: att.name })
    }
    if (chipName && chipPath) {
      turnChips.push({
        kind: 'active-file',
        name: chipName,
        path: chipPath,
        ...(chipSelection
          ? {
              selectionStartLine: chipSelection.startLine,
              selectionEndLine: chipSelection.endLine,
            }
          : {}),
      })
    }
    const mentionsPlain = mentions.map((m) => ({
      kind: m.kind,
      label: m.label,
    }))
    const attachmentsPlain: AiChatAttachment[] = stagedAttachments.map((a) => ({
      kind: a.kind,
      name: a.name,
      mediaType: a.mediaType,
      dataBase64: a.dataBase64,
    }))
    const activeFileSnapshot =
      chipName && chipPath ? { name: chipName, path: chipPath } : undefined

    // Drop any trailing errored assistant turns so a fresh user
    // submit doesn't get sandwiched under a stale error footer.
    // Forward progress (a new send) is the user's signal that
    // they've moved on from the failure; keeping the bubble would
    // just clutter the chat.
    dropTrailingErroredAssistantTurns(conv)

    // Always push the user bubble immediately for instant visual
    // feedback. The chips snapshot persists with the turn so
    // re-renders never drift from what the user submitted. We mint
    // the user-turn id up front so a queued submit can correlate
    // the bubble with its pending injection (greyed-out style +
    // sticky-scroll exclusion until the extension echoes consumption).
    const userTurnId = `user:${Date.now()}:${Math.random().toString(36).slice(2, 8)}`
    const isInject = conv.streaming && !!conv.activeTurnId
    const injectionId = isInject
      ? `inj_${Date.now()}_${Math.random().toString(36).slice(2, 10)}`
      : undefined
    conv.turns.push({
      id: userTurnId,
      role: 'user',
      content: text,
      ...(turnChips.length > 0 ? { chips: turnChips } : {}),
      ...(injectionId ? { injectionId } : {}),
    })

    // Mid-turn submit: we already have a streaming assistant bubble
    // attached to `activeTurnId`. Hand the new message off to the
    // extension's inject queue rather than starting a fresh turn.
    // The chat-service routes it as either follow-up `role:'user'`
    // after the next tool round or as the seed for a new sub-turn
    // once the model finishes naturally — see `ChatService.start()`.
    // No new assistant placeholder yet: if the extension folds the
    // message into a tool round, the existing bubble keeps growing;
    // if it spawns a sub-turn, the `turn-spawn` event will tell us
    // to mount one.
    if (isInject && injectionId) {
      const conversationIdForInject = conv.id ?? ''
      conv.queuedInjections.push({ injectionId, userTurnId })
      try {
        m.sendNotification(AiChatInject, HOST_EXTENSION, {
          turnId: conv.activeTurnId!,
          injectionId,
          conversationId: conversationIdForInject,
          text,
          mentions: mentionsPlain,
          attachments: attachmentsPlain,
          includeActiveFile: wasIncludingActiveFile,
          ...(activeFileSnapshot ? { activeFile: activeFileSnapshot } : {}),
        })
        if (wasIncludingActiveFile) includeActiveFile = false
        stagedAttachments = []
        console.log('[ai-chat] dispatched AiChatInject', {
          turnId: conv.activeTurnId,
          injectionId,
          textLen: text.length,
          queuedLen: conv.queuedInjections.length,
        })
      } catch (err) {
        // Failed dispatch: roll back the pending entry and clear
        // the bubble's injectionId so it renders in the normal
        // "consumed" style. The user can resubmit; the bubble
        // itself stays so they can copy from it.
        conv.queuedInjections = conv.queuedInjections.filter(
          (q) => q.injectionId !== injectionId
        )
        const tail = conv.turns[conv.turns.length - 1]
        if (tail && tail.id === userTurnId) tail.injectionId = undefined
        console.error('[ai-chat] sendNotification(AiChatInject) threw', err)
      }
      clearCurrentDraft()
      if (currentId) clearPendingQuestionFor(currentId)
      return
    }

    // Fresh turn (no active stream): freeze any pulsating tool-call
    // / tool-activity dots on prior turns of this conversation.
    // Usually the prior assistant turn has already settled, but if
    // the user fires a new turn before the previous stream's
    // terminal `done` reached us (disconnect + reattach window)
    // those rows would otherwise pulse forever next to the new
    // bubble.
    for (const t of conv.turns) cancelInflightToolCalls(t)
    const turnId = `turn_${Date.now()}_${Math.random().toString(36).slice(2, 10)}`
    conv.turns.push({
      id: `asst:${Date.now()}:${Math.random().toString(36).slice(2, 8)}`,
      turnId,
      role: 'assistant',
      content: '',
      streaming: true,
      blocks: [],
    })
    conv.streaming = true
    conv.activeTurnId = turnId

    const conversationId = conv.id ?? undefined
    try {
      m.sendNotification(AiChatStart, HOST_EXTENSION, {
        conversationId,
        turnId,
        text,
        mentions: mentionsPlain,
        attachments: attachmentsPlain,
        includeActiveFile: wasIncludingActiveFile,
        ...(activeFileSnapshot ? { activeFile: activeFileSnapshot } : {}),
        // Deployment binding: sent on every turn of a "Use in chat"
        // conversation. The extension also self-heals it from the
        // persisted doc on follow-ups, but sending it keeps a brand-
        // new (not-yet-persisted) conversation's first turn correct.
        ...(deploymentBinding
          ? {
              deploymentId: deploymentBinding.deploymentId,
              apiBaseUrl: deploymentBinding.apiBaseUrl,
            }
          : {}),
      })
      // Attaching the active file is one-shot: once this turn's
      // <editor-context> is in flight, flip the toggle off so the
      // next turn defaults to "no file" unless the user opts in
      // again. Matches Cursor / Claude Code "@file" semantics.
      if (wasIncludingActiveFile) includeActiveFile = false
      // Attachments are one-shot too — they were mine, they're yours now.
      stagedAttachments = []
      console.log('[ai-chat] dispatched AiChatStart', {
        turnId,
        conversationId,
        textLen: text.length,
      })
    } catch (err) {
      console.error('[ai-chat] sendNotification(AiChatStart) threw', err)
      const last = conv.turns[conv.turns.length - 1]
      if (last && last.role === 'assistant') {
        last.streaming = false
        last.error = {
          message: err instanceof Error ? err.message : String(err),
          code: 'send_failed',
        }
      }
      conv.streaming = false
      conv.activeTurnId = null
      return
    }

    clearCurrentDraft()
    // A brand-new turn supersedes any still-open meta__ask_user card
    // for *this* conversation. Cards on other conversations keep their
    // pending state — the user may flip back to answer them.
    if (currentId) clearPendingQuestionFor(currentId)
  }

  /** Drop any trailing assistant turns whose only outcome was an
   *  error. Called by `send()` and `continueTurn()` so forward
   *  progress (a new submit / a retry) clears the prior failure
   *  bubble instead of leaving it hanging above the new exchange.
   *  Conservative: stops as soon as we hit a non-errored or
   *  non-assistant turn so a successful response further back
   *  stays put. */
  function dropTrailingErroredAssistantTurns(conv: ConversationState): void {
    while (conv.turns.length > 0) {
      const tail = conv.turns[conv.turns.length - 1]!
      if (tail.role !== 'assistant') break
      if (!tail.error) break
      conv.turns.pop()
    }
  }

  /**
   * Retry the last turn *without* re-posting the user message. The
   * server persisted the user's original turn on first-request
   * create, so asking it to run another pass against that existing
   * history gets us a fresh assistant response without duplicating
   * the prompt. Used by the ErrorBubble "Retry" button.
   *
   * Caller should first strip any errored assistant bubbles from
   * `conv.turns` — we only append a new streaming placeholder and
   * dispatch AiChatStart with `continueTurn: true`.
   */
  function continueTurn(): void {
    const m = getMessenger()
    const conv = getConversation()
    if (!m || !conv) return

    // No server-assigned conversationId yet → the very first turn
    // failed before the proxy emitted its `metadata` SSE frame, so
    // there is no on-disk history to resume against. Fall back to
    // re-sending the last user message as a fresh request — same
    // effect as the user retyping it. Without this fallback Retry
    // silently no-ops, which is the bug the user hit when the proxy
    // crashed on their first prompt.
    if (!conv.id) {
      const lastUserTurn = [...conv.turns]
        .reverse()
        .find((t) => t.role === 'user')
      if (!lastUserTurn || !lastUserTurn.content.trim()) return
      // Drop trailing errored assistant bubble(s) so the resend lands
      // a fresh user→assistant pair instead of being sandwiched under
      // a stale error.
      while (
        conv.turns.length > 0 &&
        conv.turns[conv.turns.length - 1]!.role === 'assistant'
      ) {
        conv.turns.pop()
      }
      // Drop the user turn — send() will re-append it. Routing
      // through send() keeps the chips / mentions / attachments
      // path consistent and avoids duplicating the AiChatStart
      // construction here. Chips re-derive from current staging,
      // which is the closest we can get to the original send.
      const text = lastUserTurn.content
      conv.turns = conv.turns.filter((t) => t.id !== lastUserTurn.id)
      void send(text)
      return
    }

    // Freeze any pulsating tool blocks left over on prior turns
    // before pushing a fresh assistant bubble — same rationale as in
    // send(). Also drop trailing errored assistant turns so the
    // retry's new bubble doesn't sit under the stale failure.
    for (const t of conv.turns) cancelInflightToolCalls(t)
    dropTrailingErroredAssistantTurns(conv)
    const turnId = `turn_${Date.now()}_${Math.random().toString(36).slice(2, 10)}`
    conv.turns.push({
      id: `asst:${Date.now()}:${Math.random().toString(36).slice(2, 8)}`,
      turnId,
      role: 'assistant',
      content: '',
      streaming: true,
      blocks: [],
    })
    conv.streaming = true
    conv.activeTurnId = turnId
    try {
      m.sendNotification(AiChatStart, HOST_EXTENSION, {
        conversationId: conv.id,
        turnId,
        // Empty text + empty attachments / mentions — the extension
        // side's `continueTurn` branch in assembleMessages skips the
        // user role entirely so the server just runs another pass
        // against the conversation's on-disk history.
        text: '',
        mentions: [],
        attachments: [],
        continueTurn: true,
        ...(deploymentBinding
          ? {
              deploymentId: deploymentBinding.deploymentId,
              apiBaseUrl: deploymentBinding.apiBaseUrl,
            }
          : {}),
      })
    } catch (err) {
      const last = conv.turns[conv.turns.length - 1]
      if (last && last.role === 'assistant') {
        last.streaming = false
        last.error = {
          message: err instanceof Error ? err.message : String(err),
          code: 'send_failed',
        }
      }
      conv.streaming = false
      conv.activeTurnId = null
    }
    // A retry supersedes any pending meta__ask_user card attached
    // to this conversation — mirrors the `send()` path.
    if (currentId) clearPendingQuestionFor(currentId)
  }

  function abort(): void {
    const m = getMessenger()
    if (!m) return
    const conv = getConversation()
    if (!conv) return
    // Abort by the per-turn id the webview generated at send time.
    // Prefer the conv-level activeTurnId because mid-turn injects
    // can leave the streaming assistant turn somewhere other than
    // the tail of conv.turns (a queued user bubble lives after it).
    // Fall back to scanning for the streaming assistant if the
    // active id was never recorded — covers the rare race where
    // the user clicks Stop between send() and the activeTurnId
    // assignment.
    const turnId =
      conv.activeTurnId ??
      conv.turns.find((t) => t.role === 'assistant' && t.streaming)?.turnId ??
      null
    if (!turnId) return
    m.sendNotification(AiChatAbort, HOST_EXTENSION, { turnId })
    // Flip local streaming state right away. If the extension's
    // `done` event races or never arrives, the UI still unlocks.
    for (const t of conv.turns) {
      if (t.role !== 'assistant') continue
      if (!t.streaming) continue
      t.streaming = false
      cancelInflightToolCalls(t)
    }
    conv.streaming = false
    conv.activeTurnId = null
    // Stop also wipes any user messages queued in the extension's
    // inject pipeline — the chat-service drops them when its abort
    // signal trips. Clear the local pending list so the
    // pipeline-active gate releases immediately and any greyed-out
    // user bubbles pop back to their normal style.
    if (conv.queuedInjections.length > 0) {
      const cancelled = new Set(conv.queuedInjections.map((q) => q.userTurnId))
      for (const t of conv.turns) {
        if (t.role === 'user' && cancelled.has(t.id)) t.injectionId = undefined
      }
      conv.queuedInjections = []
    }
    if (currentId) clearPendingQuestionFor(currentId)
  }

  /** Mark any tool-call blocks on `turn` that are still in flight
   *  (`running` / `pending-approval`) as errored with a "Stopped"
   *  message. Called by abort() and by send() / retry when a new
   *  turn supersedes the prior one without a clean terminal `done`.
   *
   *  Why mutate status here (rather than gate the pulse with CSS the
   *  way `tool-activity` rows do): a tool-call that didn't resolve
   *  before the turn ended IS in a failure state semantically — the
   *  call never came back. Flipping to `error` swaps the row from
   *  the in-flight dot variant to the chevron card with a "Stopped"
   *  footer the user can read; the dot animation stops as a
   *  side-effect of the layout switch. tool-activity rows are
   *  inert ticker labels with no failure UX, so they freeze via
   *  CSS only (see `.assistant-bubble.is-streaming` rule in
   *  message-assistant.svelte) and keep their last status text. */
  function cancelInflightToolCalls(turn: RenderedTurn): void {
    if (!turn.blocks) return
    for (const b of turn.blocks) {
      if (b.kind !== 'tool-call') continue
      if (b.call.status === 'running' || b.call.status === 'pending-approval') {
        b.call.status = 'error'
        b.call.error = b.call.error ?? 'Stopped'
      }
    }
  }

  /** Drop any pending meta__ask_user card attached to `convId`.
   *  Used by abort / new-turn / answer paths that supersede the
   *  prior question. */
  function clearPendingQuestionFor(convId: string): void {
    const existing = pendingQuestionsByConv[convId]
    if (!existing) return
    delete pendingQuestionsByConv[convId]
    if (pendingQuestionConvByCallId[existing.callId] === convId) {
      delete pendingQuestionConvByCallId[existing.callId]
    }
  }

  // ── Event handlers, invoked by the webview's message pump.
  function onStarted(params: {
    conversationId: string
    turnId: string
    model: string
  }): void {
    // Promote the pre-started buffer into a real conversation, but
    // ONLY when the started event matches the buffer's turnId. This
    // is the load-bearing check that closes the rapid-fire race: if
    // the user submitted conv #1, hit new-chat, then submitted conv
    // #2, there are two distinct turnIds in flight. Conv #1's
    // `started` (carrying turnId_1) arrives while `pendingConversation`
    // already holds conv #2's buffer (activeTurnId === turnId_2) — the
    // mismatch sends conv #1's started through the else branch, where
    // it lands as a fresh empty entry (visible via history). Conv
    // #2's `started` later matches and migrates correctly.
    if (
      pendingConversation &&
      !pendingConversation.id &&
      pendingConversation.activeTurnId === params.turnId
    ) {
      // Only advance `currentId` if the user is still looking at the
      // fresh-chat view (currentId === null) — if they've switched
      // to a different chat while this one was resolving its id (e.g.
      // via the history panel), leave their view alone. Moving
      // currentId would yank their UI to the just-started
      // conversation mid-read.
      pendingConversation.id = params.conversationId
      conversations[params.conversationId] = pendingConversation
      if (currentId === null) currentId = params.conversationId
      pendingConversation = null
    } else if (!conversations[params.conversationId]) {
      conversations[params.conversationId] = {
        id: params.conversationId,
        title: null,
        turns: [],
        streaming: true,
        activeTurnId: null,
        queuedInjections: [],
        // Race fallback (started/turnId mismatch): the pending
        // buffer wasn't promoted, so seed the binding straight off
        // the active deployment state — same reason as
        // `ensureCurrent`, keeps the cached-reopen path bound.
        ...(deploymentBinding
          ? {
              deploymentId: deploymentBinding.deploymentId,
              apiBaseUrl: deploymentBinding.apiBaseUrl,
              ...(deploymentBinding.intendedUse
                ? { intendedUse: deploymentBinding.intendedUse }
                : {}),
            }
          : {}),
      }
    }
  }

  /** A queued user message has spawned a fresh sub-turn under the
   *  same conversation. Mount a new streaming assistant placeholder
   *  for the sub-turn so subsequent text-deltas / tool-calls route
   *  to it. The user bubble(s) for this sub-turn were already
   *  pushed into conv.turns at send-time; this just adds the
   *  assistant placeholder above which they sit.
   *
   *  Note: `conv.activeTurnId` deliberately stays pinned to the
   *  pipeline's root turnId — that's the abort handle the
   *  chat-service registered with its AbortController, and
   *  sub-turn ids are NOT in the extension's active map. The
   *  placeholder's own `turnId` field gets the sub-turn id for
   *  debugging only. */
  function onTurnSpawn(params: {
    conversationId: string
    subTurnId: string
  }): void {
    const conv = conversations[params.conversationId]
    if (!conv) return
    // Freeze any pulsating dots on prior assistant turns now that a
    // new sub-turn is taking over. The previous sub-turn already
    // received its own `done`, so its `streaming` flag is off — but
    // its trailing tool-activity rows would otherwise stay
    // animation-eligible until something appended after them.
    for (const t of conv.turns) {
      if (t.role === 'assistant') t.streaming = false
    }
    conv.turns.push({
      id: `asst:${Date.now()}:${Math.random().toString(36).slice(2, 8)}`,
      turnId: params.subTurnId,
      role: 'assistant',
      content: '',
      streaming: true,
      blocks: [],
    })
    conv.streaming = true
  }

  /** The extension has folded the listed `injectionIds` into the
   *  pipeline. Remove the matching entries from `queuedInjections`
   *  (so `pipelineActive` releases once everything has been
   *  consumed) and clear the `injectionId` field on each
   *  corresponding user bubble so it stops rendering greyed-out and
   *  becomes eligible for scroll-stickiness.
   *
   *  Stale ids (already removed by abort/error, or never enqueued —
   *  shouldn't happen but cheap to defend against) are silently
   *  ignored. */
  function onQueueConsumed(params: {
    conversationId: string
    injectionIds: string[]
  }): void {
    const conv = conversations[params.conversationId]
    if (!conv) return
    if (!params.injectionIds || params.injectionIds.length === 0) return
    const consumed = new Set(params.injectionIds)
    const consumedTurnIds = new Set<string>()
    conv.queuedInjections = conv.queuedInjections.filter((q) => {
      if (!consumed.has(q.injectionId)) return true
      consumedTurnIds.add(q.userTurnId)
      return false
    })
    if (consumedTurnIds.size > 0) {
      for (const t of conv.turns) {
        if (t.role === 'user' && consumedTurnIds.has(t.id)) {
          t.injectionId = undefined
        }
      }
    }
  }

  function onTextDelta(params: { conversationId: string; text: string }): void {
    const conv = conversations[params.conversationId]
    if (!conv) return
    // Target the most recent streaming assistant turn, not blindly
    // the tail — mid-turn injects leave queued user bubbles after
    // the in-flight assistant, and we need to keep streaming text
    // into the assistant turn that owns the active stream.
    const turn =
      findLastStreamingAssistant(conv) ??
      // Fallback: the tail-only logic still works for
      // disconnect+reattach paths where `streaming` got reset by
      // an early `done` but more text-deltas arrived afterward.
      (conv.turns[conv.turns.length - 1]?.role === 'assistant'
        ? conv.turns[conv.turns.length - 1]
        : null)
    if (!turn || turn.role !== 'assistant') return
    turn.content += params.text
    if (!turn.blocks) turn.blocks = []
    const lastBlock = turn.blocks[turn.blocks.length - 1]
    if (lastBlock && lastBlock.kind === 'text') {
      lastBlock.text += params.text
    } else {
      turn.blocks.push({ kind: 'text', text: params.text })
    }
  }

  function onDone(params: {
    conversationId: string
    finishReason: string
    usage?: { promptTokens: number; completionTokens: number }
  }): void {
    const conv = conversations[params.conversationId]
    if (!conv) return
    // Find the streaming assistant turn — usually the tail, but
    // mid-turn injects can leave queued user bubbles after it. Walk
    // backwards so we settle the most recent one in flight.
    const streamingTurn = findLastStreamingAssistant(conv)
    if (streamingTurn) {
      streamingTurn.streaming = false
      // Stash per-turn token totals on the assistant bubble so the
      // renderer can show a small "• 1.2k tokens" badge. Helps the
      // user see which turns are expensive and catches quota drift
      // before the 429. `usage` is undefined on tool-call pauses
      // (no terminal chunk yet) and on early errors. The badge
      // itself is gated on the conversation-level pipelineActive
      // flag in message-assistant.svelte so it stays hidden until
      // every queued follow-up has been consumed.
      if (params.usage) {
        streamingTurn.usage = params.usage
      }
    }
    // Pipeline `streaming` only flips off when there are no more
    // queued user messages waiting to spawn a follow-up sub-turn.
    // The chat-service's `turn-spawn` event will set it back to
    // true if more work is incoming.
    if (conv.queuedInjections.length === 0) {
      conv.streaming = false
      conv.activeTurnId = null
    }
    void refreshHistory()
  }

  function onError(params: {
    conversationId: string
    message: string
    code?: string
  }): void {
    // Route by real conversation id, falling back to the pre-started
    // buffer when the first turn errored before `AiChatStarted`
    // could land (the extension emits the turnId as a placeholder
    // conversationId in that race — neither key matches a real entry).
    const conv =
      conversations[params.conversationId] ?? pendingConversation ?? null
    if (!conv) return
    const streamingTurn = findLastStreamingAssistant(conv)
    if (streamingTurn) {
      streamingTurn.streaming = false
      streamingTurn.error = { message: params.message, code: params.code }
    }
    // An error halts the whole pipeline — the chat-service exits
    // start() without draining further queued messages — so clear
    // the pending list. The corresponding user bubbles stay on
    // screen (un-greyed, with their injectionId cleared) so the
    // user can copy from them or resubmit.
    conv.streaming = false
    conv.activeTurnId = null
    if (conv.queuedInjections.length > 0) {
      const cancelled = new Set(conv.queuedInjections.map((q) => q.userTurnId))
      for (const t of conv.turns) {
        if (t.role === 'user' && cancelled.has(t.id)) t.injectionId = undefined
      }
      conv.queuedInjections = []
    }
  }

  /** Find the most recent assistant turn that's still streaming.
   *  Walks back from the tail because mid-turn injects can leave
   *  queued user bubbles after the in-flight assistant. Returns
   *  null if nothing's currently streaming. */
  function findLastStreamingAssistant(
    conv: ConversationState
  ): RenderedTurn | null {
    for (let i = conv.turns.length - 1; i >= 0; i--) {
      const t = conv.turns[i]!
      if (t.role !== 'assistant') continue
      if (t.streaming) return t
    }
    return null
  }

  function onThinkingDelta(params: {
    conversationId: string
    text: string
  }): void {
    if (!params.conversationId) return
    const conv = conversations[params.conversationId]
    if (!conv) return
    const turn =
      findLastStreamingAssistant(conv) ??
      (conv.turns[conv.turns.length - 1]?.role === 'assistant'
        ? conv.turns[conv.turns.length - 1]
        : null)
    if (!turn || turn.role !== 'assistant') return
    if (!turn.blocks) turn.blocks = []
    // Merge consecutive thinking deltas into one block so the whole
    // reasoning stream reads as a single collapsible panel instead of
    // a row per token.
    const tail = turn.blocks[turn.blocks.length - 1]
    if (tail && tail.kind === 'thinking') {
      tail.text += params.text
      return
    }
    turn.blocks.push({ kind: 'thinking', text: params.text })
  }

  function onToolActivity(params: {
    conversationId: string
    tool: string
    status: 'running' | 'done' | 'error'
    label?: string
    message: string
    input?: unknown
    output?: unknown
    ruleId?: string
    deploymentId?: string
    error?: string
    sources?: Array<{ url: string; title?: string }>
  }): void {
    // Route strictly by conversationId. Falling back to `currentId`
    // crosstalks multiple concurrent streams into whichever chat the
    // user has open. The extension is now expected to always populate
    // `conversationId` — an empty value here means a bug upstream,
    // better to drop the event than misroute it.
    if (!params.conversationId) return
    const conv = conversations[params.conversationId]
    if (!conv) return
    const turn =
      findLastStreamingAssistant(conv) ??
      (conv.turns[conv.turns.length - 1]?.role === 'assistant'
        ? conv.turns[conv.turns.length - 1]
        : null)
    if (!turn || turn.role !== 'assistant') return
    if (!turn.blocks) turn.blocks = []

    // L4-rule activities (the proxy ran a deployed rule on the
    // server) merge `running → done` into a SINGLE mutating row so
    // they read exactly like a client-side rule tool-call: the row
    // appears pulsating the moment the call lands, then swaps to the
    // expandable INPUT/OUTPUT card in place when the result arrives.
    // Matched by rule identity rather than tool+message because the
    // running and done messages differ (describeStart vs
    // describeDone) — message-based dedupe would split them into two
    // rows. We scan all blocks (not just the tail): a rule eval can
    // interleave with text deltas, so the running row is rarely the
    // immediate predecessor of its own done event.
    const ruleIdentity = ruleActivityIdentity(params)
    if (ruleIdentity) {
      for (let i = turn.blocks.length - 1; i >= 0; i--) {
        const b = turn.blocks[i]
        if (
          b.kind === 'tool-activity' &&
          b.activity.ruleKey === ruleIdentity.ruleKey &&
          b.activity.status === 'running'
        ) {
          // Update in place. `input` may arrive on the running event
          // already; keep whichever is non-undefined so a done event
          // that omits it doesn't blank the card.
          b.activity.status = params.status
          b.activity.message = params.message
          if (params.label !== undefined) b.activity.label = params.label
          if (params.input !== undefined) b.activity.input = params.input
          if (params.output !== undefined) b.activity.output = params.output
          if (params.deploymentId) b.activity.deploymentId = params.deploymentId
          if (params.error !== undefined) b.activity.error = params.error
          return
        }
      }
      turn.blocks.push({
        kind: 'tool-activity',
        activity: {
          tool: params.tool,
          status: params.status,
          label: params.label,
          message: params.message,
          input: params.input,
          output: params.output,
          ruleId: ruleIdentity.ruleName,
          deploymentId: params.deploymentId,
          error: params.error,
          ruleKey: ruleIdentity.ruleKey,
        },
      })
      return
    }

    // Web-search activities merge by tool name (not by message) so
    // the initial empty-message running event and the final "N sources"
    // done event collapse into ONE row. Without this branch the
    // generic dedupe below would split them (different `message`),
    // showing two web-search rows for a single search burst.
    if (params.tool === 'web_search') {
      for (let i = turn.blocks.length - 1; i >= 0; i--) {
        const b = turn.blocks[i]
        if (b.kind === 'tool-activity' && b.activity.tool === 'web_search') {
          b.activity.status = params.status
          if (params.message) b.activity.message = params.message
          if (params.label !== undefined) b.activity.label = params.label
          if (params.sources !== undefined) b.activity.sources = params.sources
          if (params.error !== undefined) b.activity.error = params.error
          return
        }
      }
      turn.blocks.push({
        kind: 'tool-activity',
        activity: {
          tool: 'web_search',
          status: params.status,
          label: params.label,
          message: params.message,
          sources: params.sources,
          error: params.error,
        },
      })
      return
    }

    // Dedupe policy:
    //   - Same tool + same message as the tail → treat as a status
    //     update on the existing block (don't push a new row). This
    //     is what swallows a `running → running → done` burst that
    //     keeps re-emitting the same status text.
    //   - Same tool but a new message → the activity is reporting
    //     real progress, so push a new row even if the previous one
    //     is still "running" (it effectively terminates when the next
    //     message arrives).
    //
    // When the user has hidden reasoning ("Show model reasoning"
    // toggle off in chat settings), the dedupe scan walks past
    // intervening thinking blocks. Otherwise two identical activity
    // events sandwiching a hidden think look like duplicate rows.
    // When reasoning is visible, we want a thinking block to act
    // as a divider so the user can see "model thought, then status
    // re-emitted" — so the scan only looks at the immediate tail.
    let tailIdx = turn.blocks.length - 1
    if (!aiPrefs.showReasoning) {
      while (tailIdx >= 0 && turn.blocks[tailIdx]?.kind === 'thinking') {
        tailIdx--
      }
    }
    const tail = turn.blocks[tailIdx]
    if (
      tail &&
      tail.kind === 'tool-activity' &&
      tail.activity.tool === params.tool &&
      tail.activity.message === params.message
    ) {
      tail.activity.status = params.status
      return
    }
    turn.blocks.push({
      kind: 'tool-activity',
      activity: {
        tool: params.tool,
        status: params.status,
        label: params.label,
        message: params.message,
      },
    })
  }

  function onToolCall(params: {
    conversationId: string
    callId: string
    name: string
    argsJson: string
    status: 'pending-approval' | 'running' | 'done' | 'error'
    result?: string
    errorMessage?: string
    /** Original L4 function name (with spaces) for `l4-rules__*` calls;
     *  threaded through from the MCP target map so the row displays
     *  the unsanitised name. */
    ruleFnName?: string
    /** Deployment id parsed from the MCP description trailer. */
    deploymentId?: string
  }): void {
    // Status updates for an EXISTING tool call must merge into the
    // block wherever it already lives — not just the latest turn or
    // the named conversation. When the user submits a new message
    // while a tool was awaiting approval, the old assistant turn is
    // no longer the tail; when the dispatcher fires
    // `notifyStatus('error')` before the SSE `tool-call` event has
    // populated callArgs, the conversationId field is empty. Both
    // cases need to find the existing block by callId regardless of
    // its location, otherwise the row gets stuck at its initial
    // status (e.g. "running" → never flips to "error" → pulsating dot
    // never becomes the expandable error card).
    const updateBlock = (block: AssistantBlock): boolean => {
      if (block.kind !== 'tool-call') return false
      if (block.call.callId !== params.callId) return false
      const c = block.call
      c.status = params.status
      if (params.result !== undefined) c.result = params.result
      if (params.errorMessage !== undefined) c.error = params.errorMessage
      if (params.name) c.name = params.name
      if (params.argsJson) c.argsJson = params.argsJson
      if (params.ruleFnName !== undefined) c.ruleFnName = params.ruleFnName
      if (params.deploymentId !== undefined)
        c.deploymentId = params.deploymentId
      return true
    }

    // Fast path: the conversation we were told about. Scan all turns,
    // not just the tail.
    if (params.conversationId) {
      const conv = conversations[params.conversationId]
      if (conv) {
        for (const turn of conv.turns) {
          if (turn.role !== 'assistant' || !turn.blocks) continue
          if (turn.blocks.some(updateBlock)) return
        }
      }
    }

    // Fallback: scan every loaded conversation for the block. Picks
    // up late dispatcher status updates whose conversationId field
    // was empty (callArgs not yet populated when notifyStatus fired).
    for (const id in conversations) {
      const conv = conversations[id]
      for (const turn of conv.turns) {
        if (turn.role !== 'assistant' || !turn.blocks) continue
        if (turn.blocks.some(updateBlock)) return
      }
    }

    // No existing block — this is a brand-new tool call, so it must
    // attach to the streaming assistant turn of the named
    // conversation. Walk backwards to find it (mid-turn injects
    // can leave queued user bubbles after the in-flight assistant
    // so the tail isn't always the right target). Without a
    // conversationId or any streaming assistant, the event arrived
    // out of band; drop it rather than spawn an orphan row.
    if (!params.conversationId) return
    const conv = conversations[params.conversationId]
    if (!conv || !conv.turns.length) return
    const target =
      findLastStreamingAssistant(conv) ??
      (conv.turns[conv.turns.length - 1]?.role === 'assistant'
        ? conv.turns[conv.turns.length - 1]!
        : null)
    if (!target || target.role !== 'assistant') return
    if (!target.blocks) target.blocks = []
    target.blocks.push({
      kind: 'tool-call',
      call: {
        callId: params.callId,
        name: params.name,
        argsJson: params.argsJson,
        status: params.status,
        result: params.result,
        error: params.errorMessage,
        ...(params.ruleFnName !== undefined
          ? { ruleFnName: params.ruleFnName }
          : {}),
        ...(params.deploymentId !== undefined
          ? { deploymentId: params.deploymentId }
          : {}),
      },
    })
  }

  function approveTool(
    callId: string,
    decision: 'allow' | 'deny' | 'alwaysAllow'
  ): void {
    const m = getMessenger()
    m?.sendNotification(AiChatApproveTool, HOST_EXTENSION, {
      callId,
      decision,
    })
  }

  function openFileDiff(callId: string): void {
    const m = getMessenger()
    m?.sendNotification(AiFileOpenDiff, HOST_EXTENSION, { callId })
  }

  function openFile(callId: string): void {
    const m = getMessenger()
    m?.sendNotification(AiFileOpen, HOST_EXTENSION, { callId })
  }

  /** First tool call in the current conversation that's awaiting user
   *  approval, or null if none. The bottom action bar reads this so
   *  it can surface Accept/Reject buttons in place of the spinner. */
  function getPendingApproval(): RenderedToolCall | null {
    const conv = getConversation()
    if (!conv) return null
    for (const turn of conv.turns) {
      if (turn.role !== 'assistant' || !turn.blocks) continue
      for (const block of turn.blocks) {
        if (
          block.kind === 'tool-call' &&
          block.call.status === 'pending-approval'
        ) {
          return block.call
        }
      }
    }
    return null
  }

  function onUsageUpdate(params: {
    used: number
    limit: number
    blockOnOverage: boolean
  }): void {
    usedToday = params.used
    dailyLimit = params.limit
    blockOnOverage = params.blockOnOverage
  }

  function onAuthStatus(params: { signedIn: boolean }): void {
    const prevSignedIn = signedIn
    signedIn = params.signedIn
    // Auth flips (sign in, sign out, user swap) invalidate the cached
    // history list — it was scoped to the previous user's on-disk
    // folder. Drop any currently-loaded conversation too: if it
    // belonged to the previous user it would now 404 on reload, and
    // leaving its rendered messages on screen under a different
    // identity is exactly the cross-user leak we're guarding against.
    //
    // Reset to a fresh-chat view explicitly — clearing only
    // `currentId` would leave a stale `pendingConversation` buffer or
    // an active `deploymentBinding` ("Use in chat") in place, so the
    // panel would still render the previous session's deployment
    // banner / empty-state after signing back in. Also wipe the
    // in-memory conversation cache so a stale id never resurfaces.
    if (prevSignedIn !== params.signedIn) {
      newConversation()
      for (const k of Object.keys(conversations)) delete conversations[k]
      for (const k of Object.keys(pendingQuestionsByConv))
        delete pendingQuestionsByConv[k]
      for (const k of Object.keys(pendingQuestionConvByCallId))
        delete pendingQuestionConvByCallId[k]
      void refreshHistory()
    }
  }

  function onActiveFile(params: {
    uri: string | null
    name: string | null
    path: string | null
    inWorkspace: boolean
  }): void {
    activeFile = {
      name: params.name,
      path: params.path,
      inWorkspace: params.inWorkspace,
    }
  }

  function toggleIncludeActiveFile(): void {
    includeActiveFile = !includeActiveFile
  }

  /** Force the "attach active file" chip on or off. Used by the Get
   *  Started seed flows to switch the chip *off* once a document
   *  attachment lands — otherwise the user accidentally ships both
   *  their current editor file AND the seed document, doubling up
   *  the context budget for nearly the same information. */
  function setIncludeActiveFile(value: boolean): void {
    includeActiveFile = value
  }

  function onAskUser(params: {
    conversationId: string
    callId: string
    question: string
    choices?: string[]
  }): void {
    // Route strictly by the conversation the call belongs to. The
    // extension populates `conversationId` from its callArgs map, so
    // an empty value signals a bug upstream — drop the event rather
    // than attach the card to an unrelated chat. If the user has
    // flipped conversations, the card will materialize as soon as
    // they navigate back.
    if (!params.conversationId) return
    pendingQuestionsByConv[params.conversationId] = {
      callId: params.callId,
      question: params.question,
      choices: params.choices,
    }
    pendingQuestionConvByCallId[params.callId] = params.conversationId
  }

  function answerQuestion(answer: string): void {
    // Answer the card attached to the *currently-viewed* conversation
    // — the same one `pendingQuestion` surfaces to MessageList. If
    // the user is looking at a different chat, there's no card on
    // screen, so answering via this path is a no-op.
    if (!currentId) return
    const q = pendingQuestionsByConv[currentId]
    if (!q) return
    clearPendingQuestionFor(currentId)
    const m = getMessenger()
    m?.sendNotification(AiChatAnswerUser, HOST_EXTENSION, {
      callId: q.callId,
      answer,
    })
  }

  function usageSubscribe(): void {
    const m = getMessenger()
    m?.sendNotification(AiUsageSubscribe, HOST_EXTENSION, undefined as never)
  }

  function usageUnsubscribe(): void {
    const m = getMessenger()
    m?.sendNotification(AiUsageUnsubscribe, HOST_EXTENSION, undefined as never)
  }

  function setDraft(text: string): void {
    if (currentId) draftsByConv[currentId] = text
    else pendingDraft = text
  }

  /**
   * Write the draft from an external source (seed button, right-click
   * command, host notification) and bump `draftSeedVersion` so the
   * chat-input effect picks it up on the next tick. Use this instead
   * of `setDraft` whenever the caller is NOT the textarea's own
   * oninput — `setDraft` skips the version bump specifically to keep
   * keystrokes off the sync path.
   */
  function seedDraft(text: string): void {
    if (currentId) draftsByConv[currentId] = text
    else pendingDraft = text
    draftSeedVersion++
  }

  function getDraft(): string {
    if (currentId) return draftsByConv[currentId] ?? ''
    return pendingDraft
  }

  async function getPermissions(): Promise<
    Record<AiPermissionCategory, AiPermissionValue>
  > {
    const m = getMessenger()
    const empty = {
      'fs.read': 'always',
      'fs.create': 'always',
      'fs.edit': 'always',
      'fs.delete': 'always',
      'l4.evaluate': 'always',
      'l4.refactor': 'always',
      'mcp.l4Rules': 'always',
      'meta.askUser': 'always',
    } as Record<AiPermissionCategory, AiPermissionValue>
    if (!m) return empty
    try {
      const res = await m.sendRequest(
        AiPermissionsGet,
        HOST_EXTENSION,
        undefined as never
      )
      return res.values
    } catch {
      return empty
    }
  }

  function setPermission(
    category: AiPermissionCategory,
    value: AiPermissionValue
  ): void {
    const m = getMessenger()
    m?.sendNotification(AiPermissionsSet, HOST_EXTENSION, { category, value })
  }

  /** Pop the extension's native file picker for an attachment. Returns
   *  `{ ok: true }` when the staged list gained a file, or `{ ok: false,
   *   note }` when the user cancelled / the file was rejected (too big,
   *  unsupported type, etc.) so callers can surface a friendly note. */
  async function pickAttachment(
    accept: 'any' | 'text-or-pdf' | 'spreadsheet'
  ): Promise<{ ok: boolean; note?: string }> {
    const m = getMessenger()
    if (!m) return { ok: false }
    try {
      const res = await m.sendRequest(AiChatPickAttachment, HOST_EXTENSION, {
        accept,
      })
      if (res.attachment) {
        stagedAttachments = [...stagedAttachments, res.attachment]
        return { ok: true }
      }
      return { ok: false, note: res.note }
    } catch (err) {
      return {
        ok: false,
        note: err instanceof Error ? err.message : String(err),
      }
    }
  }

  function removeAttachment(index: number): void {
    stagedAttachments = stagedAttachments.filter((_, i) => i !== index)
  }

  function previewAttachment(att: AiChatAttachment): void {
    const m = getMessenger()
    m?.sendNotification(AiChatPreviewAttachment, HOST_EXTENSION, {
      name: att.name,
      mediaType: att.mediaType,
      dataBase64: att.dataBase64,
    })
  }

  async function searchMentions(query: string): Promise<AiMentionCandidate[]> {
    const m = getMessenger()
    if (!m) return []
    try {
      const res = await m.sendRequest(AiMentionSearch, HOST_EXTENSION, {
        query,
      })
      return res.items
    } catch {
      return []
    }
  }

  return {
    // Reactive state exposed for components.
    get currentId() {
      return currentId
    },
    get current() {
      return getConversation()
    },
    get history() {
      return history
    },
    get usedToday() {
      return usedToday
    },
    get dailyLimit() {
      return dailyLimit
    },
    get blockOnOverage() {
      return blockOnOverage
    },
    get signedIn() {
      return signedIn
    },
    get deploymentBinding() {
      return deploymentBinding
    },
    get activeFile() {
      return activeFile
    },
    get includeActiveFile() {
      return includeActiveFile
    },
    get pendingQuestion() {
      // Surface only the card for the *current* conversation so the
      // user never sees a question belonging to a different chat they
      // flipped through. Other conversations' cards stay in the
      // bucket and re-appear when the user navigates back.
      if (!currentId) return null
      return pendingQuestionsByConv[currentId] ?? null
    },
    /** True while the current conversation has either an in-flight
     *  stream OR queued user messages still waiting to be folded
     *  into the model request. Drives the per-turn usage badge and
     *  the "Files changed" review card on completed assistant
     *  bubbles — both stay hidden until the whole pipeline settles
     *  so the user doesn't see a "turn complete" badge in the
     *  middle of a multi-message exchange. */
    get pipelineActive() {
      const conv = getConversation()
      if (!conv) return false
      return conv.streaming || conv.queuedInjections.length > 0
    },
    /** Ids of conversations with an in-flight stream. The history
     *  panel renders a spinner on these rows so the user can see at
     *  a glance that a chat is still burning tokens even when it
     *  isn't the currently-focused one — defence against silent
     *  "ghost session" token spend. */
    get streamingConversationIds() {
      const ids: string[] = []
      for (const key of Object.keys(conversations)) {
        const conv = conversations[key]
        if (!conv) continue
        if (conv.streaming && conv.id) ids.push(conv.id)
      }
      return ids
    },
    get stagedAttachments() {
      return stagedAttachments
    },
    pickAttachment,
    removeAttachment,
    previewAttachment,
    toggleIncludeActiveFile,
    setIncludeActiveFile,
    answerQuestion,
    getPermissions,
    setPermission,
    // Actions.
    refreshHistory,
    loadConversation,
    deleteConversation,
    newConversation,
    startDeploymentChat,
    send,
    continueTurn,
    abort,
    usageSubscribe,
    usageUnsubscribe,
    setDraft,
    seedDraft,
    getDraft,
    get draftSeedVersion() {
      return draftSeedVersion
    },
    searchMentions,
    approveTool,
    openFileDiff,
    openFile,
    get pendingApproval() {
      return getPendingApproval()
    },
    // Event handlers — wired into the messenger from the top-level panel.
    onStarted,
    onTextDelta,
    onThinkingDelta,
    onDone,
    onError,
    onToolCall,
    onToolActivity,
    onTurnSpawn,
    onQueueConsumed,
    onUsageUpdate,
    onAuthStatus,
    onActiveFile,
    onAskUser,
  }
}

export type AiChatStore = {
  readonly currentId: string | null
  readonly current: ConversationState | null
  readonly history: AiConversationSummary[]
  readonly usedToday: number
  readonly dailyLimit: number
  readonly blockOnOverage: boolean
  readonly signedIn: boolean
  readonly deploymentBinding: {
    deploymentId: string
    apiBaseUrl: string
    intendedUse?: string
  } | null
  readonly activeFile: ActiveFileInfo
  readonly includeActiveFile: boolean
  readonly pendingQuestion: PendingQuestion | null
  readonly pipelineActive: boolean
  readonly streamingConversationIds: string[]
  toggleIncludeActiveFile: () => void
  setIncludeActiveFile: (value: boolean) => void
  answerQuestion: (answer: string) => void
  readonly stagedAttachments: AiChatAttachment[]
  pickAttachment: (
    accept: 'any' | 'text-or-pdf' | 'spreadsheet'
  ) => Promise<{ ok: boolean; note?: string }>
  removeAttachment: (index: number) => void
  previewAttachment: (att: AiChatAttachment) => void
  getPermissions: () => Promise<Record<AiPermissionCategory, AiPermissionValue>>
  setPermission: (
    category: AiPermissionCategory,
    value: AiPermissionValue
  ) => void
  refreshHistory: () => Promise<void>
  loadConversation: (id: string) => Promise<void>
  deleteConversation: (id: string) => Promise<void>
  newConversation: () => void
  startDeploymentChat: (
    deploymentId: string,
    apiBaseUrl: string,
    intendedUse?: string
  ) => void
  send: (
    text: string,
    mentions?: AiChatStartParams['mentions']
  ) => Promise<void>
  continueTurn: () => void
  abort: () => void
  usageSubscribe: () => void
  usageUnsubscribe: () => void
  setDraft: (text: string) => void
  seedDraft: (text: string) => void
  getDraft: () => string
  readonly draftSeedVersion: number
  searchMentions: (query: string) => Promise<AiMentionCandidate[]>
  approveTool: (
    callId: string,
    decision: 'allow' | 'deny' | 'alwaysAllow'
  ) => void
  openFileDiff: (callId: string) => void
  openFile: (callId: string) => void
  readonly pendingApproval: RenderedToolCall | null
  onStarted: (params: {
    conversationId: string
    turnId: string
    model: string
  }) => void
  onTextDelta: (params: { conversationId: string; text: string }) => void
  onThinkingDelta: (params: { conversationId: string; text: string }) => void
  onDone: (params: {
    conversationId: string
    finishReason: string
    usage?: { promptTokens: number; completionTokens: number }
  }) => void
  onError: (params: {
    conversationId: string
    message: string
    code?: string
  }) => void
  onToolCall: (params: {
    conversationId: string
    callId: string
    name: string
    argsJson: string
    status: 'pending-approval' | 'running' | 'done' | 'error'
    result?: string
    errorMessage?: string
    ruleFnName?: string
    deploymentId?: string
  }) => void
  onToolActivity: (params: {
    conversationId: string
    tool: string
    status: 'running' | 'done' | 'error'
    label?: string
    message: string
    input?: unknown
    output?: unknown
    ruleId?: string
    deploymentId?: string
    error?: string
  }) => void
  onTurnSpawn: (params: { conversationId: string; subTurnId: string }) => void
  onQueueConsumed: (params: {
    conversationId: string
    injectionIds: string[]
  }) => void
  onUsageUpdate: (params: {
    used: number
    limit: number
    blockOnOverage: boolean
  }) => void
  onAuthStatus: (params: { signedIn: boolean }) => void
  onActiveFile: (params: {
    uri: string | null
    name: string | null
    path: string | null
    inWorkspace: boolean
  }) => void
  onAskUser: (params: {
    conversationId: string
    callId: string
    question: string
    choices?: string[]
  }) => void
}

// Satisfy the TypeScript import of the standard-library shape we
// construct above.
export type { AiConversation, AiChatMessage }
