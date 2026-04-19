import { randomUUID } from 'crypto'
import type {
  AiChatMessage,
  AiChatStartParams,
  AiConversation,
} from 'jl4-client-rpc'
import type { AuthManager } from '../auth.js'
import type { VSCodeL4LanguageClient } from '../vscode-l4-language-client.js'
import { AiProxyClient, AiProxyError } from './ai-proxy-client.js'
import type { ConversationStore } from './conversation-store.js'
import type { AiLogger } from './logger.js'
import { buildEditorContextMessage } from './editor-context.js'
import { buildWorkspaceBootstrapMessage } from './workspace-bootstrap.js'

/**
 * Events the chat service emits while running a turn. The sidebar
 * provider forwards each to the webview as the matching `AiChat*`
 * notification.
 */
export type ChatServiceEvent =
  | { kind: 'started'; conversationId: string; model: string }
  | { kind: 'text-delta'; conversationId: string; text: string }
  | {
      kind: 'tool-activity'
      conversationId: string
      tool: string
      status: 'running' | 'done' | 'error'
      message: string
    }
  | {
      kind: 'done'
      conversationId: string
      finishReason: string
      usage?: { promptTokens: number; completionTokens: number }
    }
  | {
      kind: 'error'
      conversationId: string
      message: string
      code?: string
    }

export type ChatServiceEmitter = (event: ChatServiceEvent) => void

export interface ChatServiceOptions {
  auth: AuthManager
  client: VSCodeL4LanguageClient
  store: ConversationStore
  proxy: AiProxyClient
  logger: AiLogger
}

/**
 * Orchestrates a single user → assistant turn end-to-end. Holds one
 * active stream per conversation id so abort requests can cancel the
 * right one.
 *
 * Emitter is set after construction (via `setEmitter`) because the
 * sidebar messenger — which defines where events are routed — is
 * usually initialized after the service itself.
 */
export class ChatService {
  private readonly active = new Map<string, AbortController>()
  private emitter: ChatServiceEmitter = () => undefined

  constructor(private readonly opts: ChatServiceOptions) {}

  setEmitter(emitter: ChatServiceEmitter): void {
    this.emitter = emitter
  }

  private emit(event: ChatServiceEvent): void {
    this.emitter(event)
  }

  /**
   * Start a turn. Runs entirely in the background — awaiting this
   * promise waits for the whole turn to complete (including persistence
   * + title generation). Callers typically fire-and-forget since
   * progress flows through the event emitter.
   */
  async start(params: AiChatStartParams): Promise<void> {
    const isNew = !params.conversationId
    // Local provisional id — replaced by the server's id once metadata
    // arrives. The webview reconciles on the `started` event.
    const localId = params.conversationId ?? provisionalId()

    const abortController = new AbortController()
    this.active.set(localId, abortController)

    try {
      const messages = await this.assembleMessages(params, isNew)
      const existing = await this.opts.store.load(localId)
      let serverConversationId: string | undefined = params.conversationId
      let serverModel: string | undefined = existing?.model

      let assistantText = ''
      let finished = false

      try {
        for await (const ev of this.opts.proxy.stream(
          {
            messages,
            conversationId: params.conversationId,
            stream: true,
          },
          abortController.signal
        )) {
          if (ev.kind === 'metadata') {
            serverConversationId = ev.conversationId
            serverModel = ev.model
            this.emit({
              kind: 'started',
              conversationId: ev.conversationId,
              model: ev.model,
            })
            // Seed the local file so a disconnect doesn't lose the user turn.
            await this.opts.store
              .appendMessages(
                ev.conversationId,
                // orgId/userId not known here from the webview's side; the
                // proxy owns the truth. We store a placeholder that the
                // user-facing UI doesn't read.
                '',
                '',
                ev.model,
                titleFromUserMessage(params.text),
                messages.filter((m) => m.role === 'user')
              )
              .catch((err) =>
                this.opts.logger.warn(
                  `chat-service: initial user-turn save failed: ${err instanceof Error ? err.message : String(err)}`
                )
              )
          } else if (ev.kind === 'text-delta') {
            assistantText += ev.text
            this.emit({
              kind: 'text-delta',
              conversationId: serverConversationId ?? localId,
              text: ev.text,
            })
          } else if (ev.kind === 'tool-activity') {
            this.emit({
              kind: 'tool-activity',
              conversationId: serverConversationId ?? localId,
              tool: ev.tool,
              status: ev.status,
              message: ev.message,
            })
          } else if (ev.kind === 'done') {
            finished = true
            if (serverConversationId) {
              await this.persistAssistantTurn(
                serverConversationId,
                assistantText
              )
            }
            this.emit({
              kind: 'done',
              conversationId: serverConversationId ?? localId,
              finishReason: ev.finishReason,
              usage: ev.usage,
            })
            if (isNew && serverConversationId) {
              this.generateTitleInBackground(serverConversationId, params.text)
            }
          } else if (ev.kind === 'error') {
            this.emit({
              kind: 'error',
              conversationId: serverConversationId ?? localId,
              message: ev.message,
              code: ev.code,
            })
            finished = true
          }
        }

        if (!finished) {
          // Stream ended without a done frame — treat as a completion
          // so the UI unsticks. Partial text (if any) is already persisted
          // per delta and will be saved below.
          if (serverConversationId) {
            await this.persistAssistantTurn(serverConversationId, assistantText)
          }
          this.emit({
            kind: 'done',
            conversationId: serverConversationId ?? localId,
            finishReason: 'stop',
          })
        }
      } catch (err) {
        if (abortController.signal.aborted) {
          // Abort is a user action, not an error.
          if (serverConversationId && assistantText) {
            await this.persistAssistantTurn(
              serverConversationId,
              assistantText,
              { aborted: true }
            )
          }
          this.emit({
            kind: 'done',
            conversationId: serverConversationId ?? localId,
            finishReason: 'aborted',
          })
        } else if (err instanceof AiProxyError) {
          this.emit({
            kind: 'error',
            conversationId: serverConversationId ?? localId,
            message: err.message,
            code: err.code,
          })
        } else {
          this.opts.logger.error('chat-service: unhandled stream error', err)
          this.emit({
            kind: 'error',
            conversationId: serverConversationId ?? localId,
            message: err instanceof Error ? err.message : String(err),
            code: 'internal_error',
          })
        }
      }
    } finally {
      this.active.delete(localId)
    }
  }

  abort(conversationId: string): void {
    const ctrl = this.active.get(conversationId)
    if (ctrl && !ctrl.signal.aborted) {
      this.opts.logger.info(`abort requested for ${conversationId}`)
      ctrl.abort()
    }
  }

  /**
   * Assemble the message array we POST to the proxy: editor context +
   * (on first turn) workspace exports + prior local history + current
   * user turn. The proxy handles compaction server-side, so for
   * follow-up turns we rely on its `conversationId` state and send
   * just the delta (editor context + new user text).
   */
  private async assembleMessages(
    params: AiChatStartParams,
    isNew: boolean
  ): Promise<AiChatMessage[]> {
    const messages: AiChatMessage[] = []

    const editorCtx = buildEditorContextMessage()
    if (editorCtx) messages.push(editorCtx)

    if (isNew) {
      const bootstrap = await buildWorkspaceBootstrapMessage(this.opts.client)
      if (bootstrap) messages.push(bootstrap)
    }

    messages.push({
      role: 'user',
      content: params.text,
    })
    return messages
  }

  private async persistAssistantTurn(
    conversationId: string,
    text: string,
    meta?: Record<string, unknown>
  ): Promise<void> {
    if (!text) return
    const existing = await this.opts.store.load(conversationId)
    if (!existing) return
    existing.messages.push({
      role: 'assistant',
      content: text,
      ...(meta ? { _meta: meta } : {}),
    })
    existing.lastActiveAt = new Date().toISOString()
    await this.opts.store
      .save(existing)
      .catch((err) =>
        this.opts.logger.warn(
          `chat-service: assistant-turn save failed: ${err instanceof Error ? err.message : String(err)}`
        )
      )
  }

  private generateTitleInBackground(
    conversationId: string,
    firstUserMessage: string
  ): void {
    void this.opts.proxy
      .summarizeTitle(firstUserMessage)
      .then(async (title) => {
        if (!title) return
        await this.opts.store.setTitle(conversationId, title)
      })
      .catch(() => undefined)
  }
}

function provisionalId(): string {
  return `conv_local_${randomUUID().replace(/-/g, '').slice(0, 24)}`
}

function titleFromUserMessage(text: string): string {
  return text.trim().slice(0, 80) || 'New conversation'
}

// Re-export so register.ts can type-check emitted events.
export type { AiConversation }
