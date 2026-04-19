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
import { BUILTIN_TOOLS } from './tool-registry.js'
import type { ToolDispatcher } from './tool-dispatcher.js'

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
      kind: 'tool-call'
      conversationId: string
      callId: string
      name: string
      argsJson: string
      status: 'pending-approval' | 'running' | 'done' | 'error'
      result?: string
      error?: string
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
  dispatcher: ToolDispatcher
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
    const { turnId } = params

    const abortController = new AbortController()
    this.active.set(turnId, abortController)
    this.opts.logger.info(
      `turn start (turnId=${turnId}, conv=${params.conversationId ?? '<new>'}, isNew=${isNew}, textLen=${params.text.length})`
    )

    // Outer loop state: tracks the server conversation id once metadata
    // arrives, the running total of assistant text emitted this turn,
    // and whether we've already emitted a `done` for the UI.
    let serverConversationId: string | undefined = params.conversationId
    let totalAssistantText = ''

    // Initial POST body: editor/workspace context + the new user turn.
    let nextMessages = await this.assembleMessages(params, isNew).catch(
      (err) => {
        this.opts.logger.error('chat-service: assembleMessages failed', err)
        return null
      }
    )
    if (nextMessages === null) {
      this.emit({
        kind: 'error',
        conversationId: turnId,
        message: 'Failed to assemble request context',
        code: 'internal_error',
      })
      this.active.delete(turnId)
      return
    }
    this.opts.logger.info(
      `assembled ${nextMessages.length} messages; opening stream`
    )

    try {
      // Tool-call loop: each iteration runs one streaming request. If
      // the model finishes with `tool_calls`, we dispatch the calls
      // locally, build a follow-up body with `role:"tool"` results,
      // and run the stream again. Loop exits on `stop`, `length`,
      // `content_filter`, `aborted`, or an error.
      for (let iteration = 0; ; iteration++) {
        const { finishReason, pendingCalls, assistantText } =
          await this.runStreamIteration({
            turnId,
            iteration,
            messages: nextMessages,
            // After the first iteration we let the proxy's
            // conversation state own history; we pass the server id
            // back and send only the delta.
            conversationId: serverConversationId ?? params.conversationId,
            isNew,
            userText: params.text,
            abortSignal: abortController.signal,
            onMetadata: (md) => {
              serverConversationId = md.conversationId
            },
          })
        totalAssistantText += assistantText

        if (finishReason !== 'tool_calls') {
          // Natural terminal state: stop, length, content_filter, error, aborted.
          if (serverConversationId) {
            await this.persistAssistantTurn(
              serverConversationId,
              totalAssistantText,
              finishReason === 'aborted' ? { aborted: true } : undefined
            )
          }
          this.emit({
            kind: 'done',
            conversationId: serverConversationId ?? turnId,
            finishReason,
          })
          if (isNew && serverConversationId && finishReason === 'stop') {
            this.generateTitleInBackground(serverConversationId, params.text)
          }
          break
        }

        if (pendingCalls.length === 0) {
          // Defensive: the proxy said tool_calls but shipped none.
          // Treat as stop to avoid an infinite loop.
          this.opts.logger.warn(
            'finish_reason=tool_calls but no tool-call events received; stopping'
          )
          this.emit({
            kind: 'done',
            conversationId: serverConversationId ?? turnId,
            finishReason: 'stop',
          })
          break
        }

        // Execute each pending call. The dispatcher handles permissions,
        // approval prompts, and execution. Failures come back as
        // `{ok:false,error}` and still flow back to the proxy as a
        // tool-result so the model can react to them.
        const toolMessages: AiChatMessage[] = []
        for (const call of pendingCalls) {
          const result = await this.opts.dispatcher.run({
            callId: call.callId,
            name: call.name,
            argsJson: call.argsJson,
          })
          const content = result.ok
            ? result.output
            : JSON.stringify({
                error: (result as { error: string }).error,
                code: (result as { code?: string }).code,
              })
          toolMessages.push({
            role: 'tool',
            content,
            tool_call_id: call.callId,
            name: call.name,
          })
        }

        // Next iteration sends only the tool results; the proxy's
        // conversation state already has the assistant tool_calls saved.
        nextMessages = toolMessages
      }
    } catch (err) {
      if (abortController.signal.aborted) {
        if (serverConversationId && totalAssistantText) {
          await this.persistAssistantTurn(
            serverConversationId,
            totalAssistantText,
            { aborted: true }
          )
        }
        this.emit({
          kind: 'done',
          conversationId: serverConversationId ?? turnId,
          finishReason: 'aborted',
        })
      } else if (err instanceof AiProxyError) {
        this.emit({
          kind: 'error',
          conversationId: serverConversationId ?? turnId,
          message: err.message,
          code: err.code,
        })
      } else {
        this.opts.logger.error('chat-service: unhandled loop error', err)
        this.emit({
          kind: 'error',
          conversationId: serverConversationId ?? turnId,
          message: err instanceof Error ? err.message : String(err),
          code: 'internal_error',
        })
      }
    } finally {
      this.active.delete(turnId)
    }
  }

  /**
   * Run a single HTTP round-trip against the proxy and emit every SSE
   * event to the webview. Returns the terminal state:
   *  - `finishReason`: whatever the stream ended on; `tool_calls`
   *    means the caller should dispatch `pendingCalls` and loop again.
   *  - `pendingCalls`: any client-tool invocations the model asked for.
   *  - `assistantText`: the text streamed in this iteration (for
   *    persistence on the terminal iteration).
   */
  private async runStreamIteration(opts: {
    turnId: string
    iteration: number
    messages: AiChatMessage[]
    conversationId: string | undefined
    isNew: boolean
    userText: string
    abortSignal: AbortSignal
    onMetadata: (md: { conversationId: string; model: string }) => void
  }): Promise<{
    finishReason: string
    pendingCalls: Array<{ callId: string; name: string; argsJson: string }>
    assistantText: string
  }> {
    const {
      turnId,
      iteration,
      messages,
      conversationId,
      abortSignal,
      onMetadata,
    } = opts
    const pendingCalls: Array<{
      callId: string
      name: string
      argsJson: string
    }> = []
    let assistantText = ''
    let finishReason = 'stop'
    let local: string | undefined = conversationId

    for await (const ev of this.opts.proxy.stream(
      {
        messages,
        conversationId,
        // Client-declared tools every request; harmless when the model
        // doesn't call any.
        tools: BUILTIN_TOOLS,
        stream: true,
      },
      abortSignal
    )) {
      if (ev.kind === 'metadata') {
        local = ev.conversationId
        onMetadata({ conversationId: ev.conversationId, model: ev.model })
        if (iteration === 0) {
          this.emit({
            kind: 'started',
            conversationId: ev.conversationId,
            model: ev.model,
          })
          // Seed the local file with the user turn so a crash mid-stream
          // doesn't lose it.
          await this.opts.store
            .appendMessages(
              ev.conversationId,
              '',
              '',
              ev.model,
              titleFromUserMessage(opts.userText),
              messages.filter((m) => m.role === 'user')
            )
            .catch((err) =>
              this.opts.logger.warn(
                `chat-service: initial user-turn save failed: ${err instanceof Error ? err.message : String(err)}`
              )
            )
        }
      } else if (ev.kind === 'text-delta') {
        assistantText += ev.text
        this.emit({
          kind: 'text-delta',
          conversationId: local ?? turnId,
          text: ev.text,
        })
      } else if (ev.kind === 'tool-activity') {
        this.emit({
          kind: 'tool-activity',
          conversationId: local ?? turnId,
          tool: ev.tool,
          status: ev.status,
          message: ev.message,
        })
      } else if (ev.kind === 'tool-call') {
        pendingCalls.push({
          callId: ev.callId,
          name: ev.name,
          argsJson: ev.argsJson,
        })
        // Surface the call to the UI as "running" (actual permission
        // + execution happens in the dispatcher below). The webview
        // shows the tool row immediately for responsive feel.
        this.emit({
          kind: 'tool-call',
          conversationId: local ?? turnId,
          callId: ev.callId,
          name: ev.name,
          argsJson: ev.argsJson,
          status: 'running',
        })
      } else if (ev.kind === 'done') {
        finishReason = ev.finishReason
      } else if (ev.kind === 'error') {
        this.emit({
          kind: 'error',
          conversationId: local ?? turnId,
          message: ev.message,
          code: ev.code,
        })
        // Treat the inner error as a terminal state for this turn.
        return { finishReason: 'error', pendingCalls: [], assistantText }
      }
    }

    return { finishReason, pendingCalls, assistantText }
  }

  abort(turnId: string): void {
    const ctrl = this.active.get(turnId)
    if (ctrl && !ctrl.signal.aborted) {
      this.opts.logger.info(`abort requested for turn ${turnId}`)
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

function titleFromUserMessage(text: string): string {
  return text.trim().slice(0, 80) || 'New conversation'
}

// Re-export so register.ts can type-check emitted events.
export type { AiConversation }
