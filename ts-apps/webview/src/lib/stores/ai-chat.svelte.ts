import type { Messenger } from 'vscode-messenger-webview'
import { HOST_EXTENSION } from 'vscode-messenger-common'
import {
  AiChatAbort,
  AiChatApproveTool,
  AiChatStart,
  AiConversationDelete,
  AiConversationList,
  AiConversationLoad,
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
  content: string
  /** True while tokens are still streaming into this bubble. */
  streaming?: boolean
  /** Populated on failure; the bubble shows it as an error footer. */
  error?: { message: string; code?: string }
  /** Tool calls the model invoked during this assistant turn, in the
   *  order they were emitted. Rendered inline in the bubble below the
   *  text. Each is keyed by `callId` so status updates can update the
   *  right row as the extension host executes the call. */
  toolCalls?: RenderedToolCall[]
}

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
          .map((m, i) => ({
            id: `${conv.id}:${i}`,
            role: m.role as 'user' | 'assistant',
            content: typeof m.content === 'string' ? m.content : '',
          })),
        streaming: false,
      }
      conversations[conv.id] = state
      currentId = conv.id
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
    try {
      m.sendNotification(AiChatStart, HOST_EXTENSION, {
        conversationId,
        turnId,
        text,
        mentions: mentionsPlain,
        attachments: [],
      })
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
    const last = conv.turns[conv.turns.length - 1]
    if (!last || last.role !== 'assistant') return
    last.content += params.text
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
    const last = conv.turns[conv.turns.length - 1]
    if (!last || last.role !== 'assistant') return
    if (!last.toolCalls) last.toolCalls = []
    const existing = last.toolCalls.find((tc) => tc.callId === params.callId)
    if (existing) {
      existing.status = params.status
      if (params.result !== undefined) existing.result = params.result
      if (params.errorMessage !== undefined)
        existing.error = params.errorMessage
      // Refresh name/args if the status update has better data.
      if (params.name) existing.name = params.name
      if (params.argsJson) existing.argsJson = params.argsJson
    } else {
      last.toolCalls.push({
        callId: params.callId,
        name: params.name,
        argsJson: params.argsJson,
        status: params.status,
        result: params.result,
        error: params.errorMessage,
      })
    }
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
    // Event handlers — wired into the messenger from the top-level panel.
    onStarted,
    onTextDelta,
    onDone,
    onError,
    onToolCall,
    onUsageUpdate,
    onAuthStatus,
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
}

// Satisfy the TypeScript import of the standard-library shape we
// construct above.
export type { AiConversation, AiChatMessage }
