import type { Messenger } from 'vscode-messenger-webview'
import { HOST_EXTENSION } from 'vscode-messenger-common'
import {
  AiChatAbort,
  AiChatStart,
  AiConversationDelete,
  AiConversationList,
  AiConversationLoad,
  AiUsageSubscribe,
  AiUsageUnsubscribe,
  type AiConversation,
  type AiConversationSummary,
  type AiChatMessage,
  type AiChatStartParams,
} from 'jl4-client-rpc'

/**
 * A single turn that's either already stored in the conversation
 * history (role user/assistant/tool) or a transient "in-flight"
 * assistant bubble that the UI is currently streaming into.
 */
export interface RenderedTurn {
  id: string
  role: 'user' | 'assistant'
  content: string
  /** True while tokens are still streaming into this bubble. */
  streaming?: boolean
  /** Populated on failure; the bubble shows it as an error footer. */
  error?: { message: string; code?: string }
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
  const conversations = $state(new Map<string, ConversationState>())
  let history = $state<AiConversationSummary[]>([])
  let usedToday = $state<number>(0)
  let dailyLimit = $state<number>(0)
  let blockOnOverage = $state<boolean>(false)
  let signedIn = $state<boolean>(false)
  let draftsByConv = $state(new Map<string, string>())

  function ensureCurrent(): ConversationState {
    if (!currentId) {
      // Use a stable local key so multiple starts before the server
      // assigns an id don't spawn multiple bubbles.
      const localKey = '__new__'
      let conv = conversations.get(localKey)
      if (!conv) {
        conv = { id: null, title: null, turns: [], streaming: false }
        conversations.set(localKey, conv)
      }
      currentId = localKey
    }
    return conversations.get(currentId!)!
  }

  function getConversation(): ConversationState | null {
    if (!currentId) return null
    return conversations.get(currentId) ?? null
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
      conversations.set(conv.id, state)
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
    conversations.delete(id)
    if (currentId === id) currentId = null
    await refreshHistory()
  }

  function newConversation(): void {
    currentId = null
    // Clean up any prior "__new__" state so the empty-state seed
    // prompts render instead of an old half-written turn.
    conversations.delete('__new__')
  }

  function send(
    text: string,
    mentions: AiChatStartParams['mentions'] = []
  ): void {
    const m = getMessenger()
    if (!m || !text.trim()) return
    const conv = ensureCurrent()
    conv.turns.push({
      id: `user:${Date.now()}:${Math.random().toString(36).slice(2, 8)}`,
      role: 'user',
      content: text,
    })
    conv.turns.push({
      id: `asst:${Date.now()}:${Math.random().toString(36).slice(2, 8)}`,
      role: 'assistant',
      content: '',
      streaming: true,
    })
    conv.streaming = true

    const conversationId = conv.id ?? undefined
    m.sendNotification(AiChatStart, HOST_EXTENSION, {
      conversationId,
      text,
      mentions,
      attachments: [],
    })

    if (currentId) draftsByConv.delete(currentId)
  }

  function abort(): void {
    const m = getMessenger()
    if (!m) return
    const conv = getConversation()
    if (!conv?.id) return
    m.sendNotification(AiChatAbort, HOST_EXTENSION, {
      conversationId: conv.id,
    })
  }

  // ── Event handlers, invoked by the webview's message pump.
  function onStarted(params: { conversationId: string; model: string }): void {
    // If the active bucket is still '__new__', migrate it to the real id.
    const newKey = '__new__'
    const pending = conversations.get(newKey)
    if (pending && !pending.id) {
      pending.id = params.conversationId
      conversations.delete(newKey)
      conversations.set(params.conversationId, pending)
      currentId = params.conversationId
    } else if (!conversations.has(params.conversationId)) {
      conversations.set(params.conversationId, {
        id: params.conversationId,
        title: null,
        turns: [],
        streaming: true,
      })
    }
  }

  function onTextDelta(params: { conversationId: string; text: string }): void {
    const conv = conversations.get(params.conversationId)
    if (!conv) return
    const last = conv.turns[conv.turns.length - 1]
    if (!last || last.role !== 'assistant') return
    last.content += params.text
  }

  function onDone(params: {
    conversationId: string
    finishReason: string
  }): void {
    const conv = conversations.get(params.conversationId)
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
    // Error before a server id was assigned — fall through to '__new__'.
    const conv =
      conversations.get(params.conversationId) ?? conversations.get('__new__')
    if (!conv) return
    const last = conv.turns[conv.turns.length - 1]
    if (last && last.role === 'assistant') {
      last.streaming = false
      last.error = { message: params.message, code: params.code }
    }
    conv.streaming = false
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
    draftsByConv.set(key, text)
  }

  function getDraft(): string {
    const key = currentId ?? '__new__'
    return draftsByConv.get(key) ?? ''
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
    // Event handlers — wired into the messenger from the top-level panel.
    onStarted,
    onTextDelta,
    onDone,
    onError,
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
  onStarted: (params: { conversationId: string; model: string }) => void
  onTextDelta: (params: { conversationId: string; text: string }) => void
  onDone: (params: { conversationId: string; finishReason: string }) => void
  onError: (params: {
    conversationId: string
    message: string
    code?: string
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
