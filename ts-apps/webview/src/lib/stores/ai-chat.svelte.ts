import type { Messenger } from 'vscode-messenger-webview'
import { HOST_EXTENSION } from 'vscode-messenger-common'
import {
  AiChatAbort,
  AiChatApproveTool,
  AiChatStart,
  AiConversationDelete,
  AiConversationList,
  AiConversationLoad,
  AiFileOpen,
  AiFileOpenDiff,
  AiMentionSearch,
  AiUsageSubscribe,
  AiUsageUnsubscribe,
  type AiConversation,
  type AiConversationSummary,
  type AiChatMessage,
  type AiChatStartParams,
  type AiMentionCandidate,
} from 'jl4-client-rpc'

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
}

export type AssistantBlock =
  | { kind: 'text'; text: string }
  | { kind: 'tool-call'; call: RenderedToolCall }

export interface RenderedToolCall {
  callId: string
  name: string
  argsJson: string
  status: 'pending-approval' | 'running' | 'done' | 'error'
  result?: string
  error?: string
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
  let currentId = $state<string | null>(null)
  // Plain-object bag (not Map). Svelte 5's `$state` deep-proxies object
  // properties, so nested mutations (`conv.streaming = false`) reliably
  // notify reactive reads. Reactive Map does not proxy values returned
  // from .get(), which made `$derived(store.current?.streaming)` go
  // stale after abort.
  const conversations = $state<Record<string, ConversationState>>({})
  let history = $state<AiConversationSummary[]>([])
  let usedToday = $state<number>(0)
  let dailyLimit = $state<number>(0)
  let blockOnOverage = $state<boolean>(false)
  let signedIn = $state<boolean>(false)
  const draftsByConv = $state<Record<string, string>>({})
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

  function ensureCurrent(): ConversationState {
    if (!currentId) {
      const localKey = '__new__'
      if (!conversations[localKey]) {
        conversations[localKey] = {
          id: null,
          title: null,
          turns: [],
          streaming: false,
        }
      }
      currentId = localKey
    }
    return conversations[currentId!]!
  }

  function getConversation(): ConversationState | null {
    if (!currentId) return null
    return conversations[currentId] ?? null
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
      }
      conversations[conv.id] = state
      currentId = conv.id
      // Loading an existing conversation is not a fresh start — default
      // to off so a follow-up turn doesn't silently re-ship the file.
      includeActiveFile = false
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

  function newConversation(): void {
    currentId = null
    delete conversations['__new__']
    // Fresh conversation → default to attaching the editor file with
    // the first message.
    includeActiveFile = true
  }

  function send(
    text: string,
    mentions: AiChatStartParams['mentions'] = []
  ): void {
    const m = getMessenger()
    if (!m || !text.trim()) return
    const conv = ensureCurrent()
    const turnId = `turn_${Date.now()}_${Math.random().toString(36).slice(2, 10)}`
    conv.turns.push({
      id: `user:${Date.now()}:${Math.random().toString(36).slice(2, 8)}`,
      role: 'user',
      content: text,
    })
    conv.turns.push({
      id: `asst:${Date.now()}:${Math.random().toString(36).slice(2, 8)}`,
      turnId,
      role: 'assistant',
      content: '',
      streaming: true,
      blocks: [],
    })
    conv.streaming = true

    const conversationId = conv.id ?? undefined
    // The `mentions` array from the caller is usually a Svelte $state
    // proxy. postMessage() can't structured-clone proxies across the
    // webview → extension bridge ("DataCloneError: [object Array]
    // could not be cloned"), so we hand over a plain-object copy.
    const mentionsPlain = mentions.map((m) => ({
      kind: m.kind,
      label: m.label,
    }))
    const wasIncludingActiveFile = includeActiveFile
    try {
      m.sendNotification(AiChatStart, HOST_EXTENSION, {
        conversationId,
        turnId,
        text,
        mentions: mentionsPlain,
        attachments: [],
        includeActiveFile: wasIncludingActiveFile,
      })
      // Attaching the active file is one-shot: once this turn's
      // <editor-context> is in flight, flip the toggle off so the
      // next turn defaults to "no file" unless the user opts in
      // again. Matches Cursor / Claude Code "@file" semantics.
      if (wasIncludingActiveFile) includeActiveFile = false
      // eslint-disable-next-line no-console
      console.log('[ai-chat] dispatched AiChatStart', {
        turnId,
        conversationId,
        textLen: text.length,
      })
    } catch (err) {
      // eslint-disable-next-line no-console
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
      return
    }

    if (currentId) delete draftsByConv[currentId]
  }

  function abort(): void {
    const m = getMessenger()
    if (!m) return
    const conv = getConversation()
    if (!conv) return
    // Abort by the per-turn id the webview generated at send time.
    // The server's conversationId isn't needed here — we're cancelling
    // our own HTTP request, and turnId is the canonical handle for
    // that request on both sides.
    const last = conv.turns[conv.turns.length - 1]
    if (!last || !last.turnId || !last.streaming) return
    m.sendNotification(AiChatAbort, HOST_EXTENSION, { turnId: last.turnId })
    // Flip local streaming state right away. If the extension's 'done'
    // event races or never arrives, the UI still unlocks.
    last.streaming = false
    conv.streaming = false
  }

  // ── Event handlers, invoked by the webview's message pump.
  function onStarted(params: { conversationId: string; model: string }): void {
    const newKey = '__new__'
    const pending = conversations[newKey]
    if (pending && !pending.id) {
      pending.id = params.conversationId
      conversations[params.conversationId] = pending
      delete conversations[newKey]
      currentId = params.conversationId
    } else if (!conversations[params.conversationId]) {
      conversations[params.conversationId] = {
        id: params.conversationId,
        title: null,
        turns: [],
        streaming: true,
      }
    }
  }

  function onTextDelta(params: { conversationId: string; text: string }): void {
    const conv = conversations[params.conversationId]
    if (!conv) return
    const turn = conv.turns[conv.turns.length - 1]
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
  }): void {
    const conv = conversations[params.conversationId]
    if (!conv) return
    const last = conv.turns[conv.turns.length - 1]
    if (last) last.streaming = false
    conv.streaming = false
    void refreshHistory()
  }

  function onError(params: {
    conversationId: string
    message: string
    code?: string
  }): void {
    const conv =
      conversations[params.conversationId] ?? conversations['__new__']
    if (!conv) return
    const last = conv.turns[conv.turns.length - 1]
    if (last && last.role === 'assistant') {
      last.streaming = false
      last.error = { message: params.message, code: params.code }
    }
    conv.streaming = false
  }

  function onToolCall(params: {
    conversationId: string
    callId: string
    name: string
    argsJson: string
    status: 'pending-approval' | 'running' | 'done' | 'error'
    result?: string
    errorMessage?: string
  }): void {
    // conversationId may be empty when the dispatcher fires a status
    // update for a callId that's already been placed in a bucket; fall
    // back to the current conversation in that case.
    const convId = params.conversationId || currentId
    if (!convId) return
    const conv = conversations[convId] ?? conversations['__new__']
    if (!conv) return
    const turn = conv.turns[conv.turns.length - 1]
    if (!turn || turn.role !== 'assistant') return
    if (!turn.blocks) turn.blocks = []
    // Merge a status update into an existing tool-call block; otherwise
    // append a new block in chronological position so the row sits where
    // the model actually invoked the tool.
    const existing = turn.blocks.find(
      (b) => b.kind === 'tool-call' && b.call.callId === params.callId
    )
    if (existing && existing.kind === 'tool-call') {
      const c = existing.call
      c.status = params.status
      if (params.result !== undefined) c.result = params.result
      if (params.errorMessage !== undefined) c.error = params.errorMessage
      if (params.name) c.name = params.name
      if (params.argsJson) c.argsJson = params.argsJson
      return
    }
    turn.blocks.push({
      kind: 'tool-call',
      call: {
        callId: params.callId,
        name: params.name,
        argsJson: params.argsJson,
        status: params.status,
        result: params.result,
        error: params.errorMessage,
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
    signedIn = params.signedIn
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

  function usageSubscribe(): void {
    const m = getMessenger()
    m?.sendNotification(AiUsageSubscribe, HOST_EXTENSION, undefined as never)
  }

  function usageUnsubscribe(): void {
    const m = getMessenger()
    m?.sendNotification(AiUsageUnsubscribe, HOST_EXTENSION, undefined as never)
  }

  function setDraft(text: string): void {
    const key = currentId ?? '__new__'
    draftsByConv[key] = text
  }

  function getDraft(): string {
    const key = currentId ?? '__new__'
    return draftsByConv[key] ?? ''
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
    get activeFile() {
      return activeFile
    },
    get includeActiveFile() {
      return includeActiveFile
    },
    toggleIncludeActiveFile,
    // Actions.
    refreshHistory,
    loadConversation,
    deleteConversation,
    newConversation,
    send,
    abort,
    usageSubscribe,
    usageUnsubscribe,
    setDraft,
    getDraft,
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
    onDone,
    onError,
    onToolCall,
    onUsageUpdate,
    onAuthStatus,
    onActiveFile,
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
  readonly activeFile: ActiveFileInfo
  readonly includeActiveFile: boolean
  toggleIncludeActiveFile: () => void
  refreshHistory: () => Promise<void>
  loadConversation: (id: string) => Promise<void>
  deleteConversation: (id: string) => Promise<void>
  newConversation: () => void
  send: (text: string, mentions?: AiChatStartParams['mentions']) => void
  abort: () => void
  usageSubscribe: () => void
  usageUnsubscribe: () => void
  setDraft: (text: string) => void
  getDraft: () => string
  searchMentions: (query: string) => Promise<AiMentionCandidate[]>
  approveTool: (
    callId: string,
    decision: 'allow' | 'deny' | 'alwaysAllow'
  ) => void
  openFileDiff: (callId: string) => void
  openFile: (callId: string) => void
  readonly pendingApproval: RenderedToolCall | null
  onStarted: (params: { conversationId: string; model: string }) => void
  onTextDelta: (params: { conversationId: string; text: string }) => void
  onDone: (params: { conversationId: string; finishReason: string }) => void
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
}

// Satisfy the TypeScript import of the standard-library shape we
// construct above.
export type { AiConversation, AiChatMessage }
