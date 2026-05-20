import type {
  AiChatAttachment,
  AiChatInjectParams,
  AiChatMessage,
  AiChatStartParams,
  AiConversation,
} from 'jl4-client-rpc'
import type { AuthManager } from '../auth.js'
import type { VSCodeL4LanguageClient } from '../vscode-l4-language-client.js'
import { AiProxyClient, AiProxyError } from './ai-proxy-client.js'
import type { ConversationStore } from './conversation-store.js'
import type { AiLogger } from './logger.js'
import {
  buildEditorContextMessage,
  buildMentionContextMessage,
  buildSessionContextMessage,
} from './editor-context.js'
import { buildWorkspaceBootstrapMessage } from './workspace-bootstrap.js'
import { BUILTIN_TOOLS } from './tool-registry.js'
import type { ToolDispatcher } from './tool-dispatcher.js'
import { categoryForTool, getPermission } from './permissions.js'
import type { McpToolClient } from './mcp-client.js'

/**
 * Events the chat service emits while running a turn. The sidebar
 * provider forwards each to the webview as the matching `AiChat*`
 * notification.
 */
export type ChatServiceEvent =
  | { kind: 'started'; conversationId: string; turnId: string; model: string }
  | { kind: 'text-delta'; conversationId: string; text: string }
  | { kind: 'thinking-delta'; conversationId: string; text: string }
  | {
      kind: 'tool-activity'
      conversationId: string
      tool: string
      status: 'running' | 'done' | 'error'
      message: string
      input?: unknown
      output?: unknown
      ruleId?: string
      deploymentId?: string
      error?: string
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
      /** For `l4-rules__<sanitised>` calls: original L4 function name
       *  + deployment id parsed from the MCP description trailer.
       *  Threaded through to the webview so the tool-call row shows
       *  the unsanitised name (matching the server-side rule-activity
       *  card) instead of the wire-level slug with dashes. */
      ruleFnName?: string
      deploymentId?: string
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
  /** Fired when a queued user message (sent during an in-flight turn
   *  via `AiChatInject`) triggers a brand-new sub-turn under the same
   *  conversation. The webview mounts a fresh streaming assistant
   *  bubble keyed off `subTurnId` so subsequent text-delta /
   *  tool-call events route to it. */
  | { kind: 'turn-spawn'; conversationId: string; subTurnId: string }
  /** Fired each time the chat service drains queued user messages
   *  into a fresh sub-turn. `injectionIds` lists the webview-minted
   *  ids of the messages just consumed — the webview removes the
   *  matching entries from its pending-queue array. An unack'd id
   *  stays in the array so a dropped event surfaces as a stuck
   *  pipeline rather than a silent miscount. */
  | {
      kind: 'queue-consumed'
      conversationId: string
      injectionIds: string[]
    }

export type ChatServiceEmitter = (event: ChatServiceEvent) => void

/** Chronological record of what happened inside an assistant turn, saved
 *  as `_meta.blocks` on the assistant message so the webview can
 *  reconstruct the original text + tool-call row layout on reload. */
export type PersistedBlock =
  | { kind: 'text'; text: string }
  | {
      kind: 'tool-call'
      callId: string
      name: string
      argsJson: string
      status: 'running' | 'done' | 'error'
      result?: string
      error?: string
      /** Original (unsanitised) L4 function name for `l4-rules__*`
       *  rule calls — preserved so a reloaded transcript shows the
       *  same row label the user saw live. */
      ruleFnName?: string
      /** Deployment id parsed from the MCP description trailer. */
      deploymentId?: string
    }
  // Only L4-rule server activities are persisted (the proxy ran a
  // deployed rule). Plain status tickers (doc search etc.) are
  // intentionally not recorded — they're ephemeral progress noise
  // with nothing to reconstruct on reload. `ruleKey` mirrors the
  // webview store's merge key so a `running → done` burst persists
  // as ONE block.
  | {
      kind: 'tool-activity'
      tool: string
      ruleId: string
      ruleKey: string
      status: 'running' | 'done' | 'error'
      message: string
      input?: unknown
      output?: unknown
      deploymentId?: string
      error?: string
    }

export interface ChatServiceOptions {
  auth: AuthManager
  client: VSCodeL4LanguageClient
  store: ConversationStore
  proxy: AiProxyClient
  logger: AiLogger
  dispatcher: ToolDispatcher
  mcp: McpToolClient
  /** Extension version string (e.g. "1.4.0"). Injected into the
   *  first-turn `<session-context>` system message and stamped on
   *  locally-persisted conversations so support can correlate
   *  transcripts with a specific extension build. */
  extensionVersion: string
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
/** A user message queued via `inject()` while a turn is still
 *  in-flight. Mirrors the shape of `AiChatStartParams` but only the
 *  fields the chat-service needs to assemble a follow-up user
 *  message (text + optional content parts). The `injectionId` is the
 *  webview's correlation key — echoed back in `queue-consumed` so
 *  the matching pending-queue entry is removed (and its user bubble
 *  un-greyed) instead of decrementing a raw counter. */
interface QueuedUserMessage {
  injectionId: string
  text: string
  mentions: AiChatStartParams['mentions']
  attachments: AiChatAttachment[]
  includeActiveFile?: boolean
  activeFile?: { path: string; name: string }
}

export class ChatService {
  private readonly active = new Map<string, AbortController>()
  /** FIFO queue of user messages submitted via the webview's "send
   *  during streaming" path, keyed by the in-flight turn's `turnId`.
   *  Drained at the next sub-turn boundary — either after the model
   *  finishes naturally, or after the current tool round completes
   *  (whichever comes first). The drained messages seed a fresh
   *  sub-turn whose request body carries the leftover tool results
   *  alongside the new user delta so the proxy commits a clean
   *  assistant→tool→user sequence. Cleared on abort and on the
   *  turn's ultimate exit from `start()`. */
  private readonly injections = new Map<string, QueuedUserMessage[]>()
  private emitter: ChatServiceEmitter = () => undefined

  constructor(private readonly opts: ChatServiceOptions) {}

  setEmitter(emitter: ChatServiceEmitter): void {
    this.emitter = emitter
  }

  private emit(event: ChatServiceEvent): void {
    this.emitter(event)
  }

  /** Webview → chat-service: append a user message to an in-flight
   *  turn's queue. No-op if the turn is no longer active (the user
   *  raced a `done`/`error` arrival). */
  inject(params: AiChatInjectParams): void {
    if (!this.active.has(params.turnId)) {
      this.opts.logger.warn(
        `inject: no active turn ${params.turnId}; dropping queued message (textLen=${params.text.length})`
      )
      return
    }
    let queue = this.injections.get(params.turnId)
    if (!queue) {
      queue = []
      this.injections.set(params.turnId, queue)
    }
    queue.push({
      injectionId: params.injectionId,
      text: params.text,
      mentions: params.mentions,
      attachments: params.attachments,
      includeActiveFile: params.includeActiveFile,
      activeFile: params.activeFile,
    })
    this.opts.logger.info(
      `inject: queued message on turn ${params.turnId} (queueLen=${queue.length}, injectionId=${params.injectionId}, textLen=${params.text.length})`
    )
  }

  /**
   * Start a turn. Runs entirely in the background — awaiting this
   * promise waits for the whole pipeline to complete (including
   * persistence + title generation + draining any user messages
   * queued mid-stream via `inject()`). Callers typically
   * fire-and-forget since progress flows through the event emitter.
   *
   * Sub-turn loop: a single `start()` call may produce multiple
   * assistant turns when the user keeps submitting while the model
   * is working. Each iteration of the outer loop runs one
   * tool-call-aware sub-turn against the proxy; whenever the queue
   * has at least one pending user message, the current sub-turn
   * ends at its next intersection — either a natural finish or a
   * tool-round boundary — and the loop spawns a follow-up sub-turn
   * (signalled to the webview via `turn-spawn`) seeded with the
   * drained user text plus any leftover tool results. The original
   * `turnId` remains the stable abort key for the entire pipeline.
   */
  async start(params: AiChatStartParams): Promise<void> {
    const isNewConversation = !params.conversationId
    const { turnId } = params

    // Re-resolve the deployment binding for a follow-up turn. The
    // webview only sends `apiBaseUrl`/`deploymentId` on the FIRST turn
    // of a "Use in chat" conversation; subsequent turns (and any turn
    // after a webview reload or history-reopen) arrive without it, so
    // we recover it from the persisted conversation doc. This keeps a
    // deployment chat pinned to its endpoint for its whole lifetime.
    if (!params.apiBaseUrl && params.conversationId) {
      const stored = await this.opts.store
        .load(params.conversationId)
        .catch(() => null)
      if (stored?.apiBaseUrl) {
        params = {
          ...params,
          apiBaseUrl: stored.apiBaseUrl,
          deploymentId: stored.deploymentId,
        }
      }
    }

    const abortController = new AbortController()
    this.active.set(turnId, abortController)
    this.injections.set(turnId, [])
    this.opts.logger.info(
      `turn start (turnId=${turnId}, conv=${params.conversationId ?? '<new>'}, isNew=${isNewConversation}, textLen=${params.text.length})`
    )

    // Pipeline-wide state. `serverConversationId` flips from
    // undefined → real id once the first sub-turn's metadata frame
    // arrives; subsequent sub-turns reuse it. `subTurnIndex` ticks up
    // with every new sub-turn and feeds into the per-sub-turn ids
    // emitted via `turn-spawn`.
    let serverConversationId: string | undefined = params.conversationId
    let subTurnIndex = 0
    // The very first sub-turn uses the original params; subsequent
    // sub-turns get their params synthesized from the drained queue.
    let activeParams: AiChatStartParams = params
    // Tool results carried over from the prior sub-turn when it ended
    // at a tool-round boundary because of mid-turn user input. We
    // prepend these to the next sub-turn's request body so the proxy
    // sees a clean assistant(tool_calls) → tool(results) → user
    // sequence and commits the tool_use/tool_result pair to history
    // before the model's next response. Empty (or unset) on the first
    // sub-turn and after a natural-finish drain.
    let prependedToolResults: AiChatMessage[] | undefined

    try {
      while (true) {
        const isFirstSubTurn = subTurnIndex === 0
        // The `isNew` flag (which controls workspace bootstrap +
        // session-context injection) is only honoured on the FIRST
        // sub-turn of the pipeline — after that the conversation
        // exists server-side and we just want the editor-context +
        // user delta.
        const isNewForAssemble = isFirstSubTurn && isNewConversation
        let nextMessages: AiChatMessage[] | null
        try {
          nextMessages = await this.assembleMessages(
            activeParams,
            isNewForAssemble
          )
        } catch (err) {
          this.opts.logger.error('chat-service: assembleMessages failed', err)
          this.emit({
            kind: 'error',
            conversationId: serverConversationId ?? turnId,
            message: 'Failed to assemble request context',
            code: 'internal_error',
          })
          return
        }
        if (prependedToolResults && prependedToolResults.length > 0) {
          nextMessages = [...prependedToolResults, ...nextMessages]
          prependedToolResults = undefined
        }

        if (!isFirstSubTurn) {
          // Spawn a new assistant placeholder client-side BEFORE any
          // text-delta of this sub-turn lands.
          const subTurnId = `${turnId}_q${subTurnIndex}`
          this.emit({
            kind: 'turn-spawn',
            conversationId: serverConversationId ?? turnId,
            subTurnId,
          })
          // Persist the queued user message locally so reload sees
          // the same transcript the live UI showed. The user-only
          // filter mirrors the iteration-0 save in
          // `runStreamIteration` — the proxy still owns the
          // authoritative conversation state, this is just for
          // crash-recovery / offline replay.
          if (serverConversationId) {
            const userMessages = nextMessages.filter((m) => m.role === 'user')
            if (userMessages.length > 0) {
              await this.opts.store
                .appendMessages(
                  serverConversationId,
                  '',
                  '',
                  '',
                  titleFromUserMessage(activeParams.text),
                  userMessages,
                  this.opts.extensionVersion,
                  activeParams.deploymentId,
                  activeParams.apiBaseUrl
                )
                .catch((err) =>
                  this.opts.logger.warn(
                    `chat-service: sub-turn user save failed: ${err instanceof Error ? err.message : String(err)}`
                  )
                )
            }
          }
        }

        this.opts.logger.info(
          `sub-turn ${subTurnIndex}: assembled ${nextMessages.length} messages; opening stream`
        )

        const subTurnResult = await this.runSubTurn({
          turnId,
          activeParams,
          isNewForAssemble,
          isFirstSubTurn,
          serverConversationId,
          nextMessages,
          abortController,
          onMetadata: (md) => {
            serverConversationId = md.conversationId
          },
        })

        // Update conversationId from the sub-turn (in case it landed
        // for the first time during this sub-turn).
        if (subTurnResult.serverConversationId) {
          serverConversationId = subTurnResult.serverConversationId
        }

        // Pipeline exits unless this sub-turn ended at a queue
        // boundary (natural finish OR tool-round-boundary-with-queued-
        // input) AND the queue still has at least one message to
        // drive a follow-up sub-turn. Anything else (error, abort,
        // length cap) is a hard stop.
        if (subTurnResult.terminate) break
        const queue = this.injections.get(turnId) ?? []
        if (queue.length === 0) break

        // Drain queue → seed a follow-up sub-turn. Multiple queued
        // messages get joined with blank lines so the model treats
        // them as one thought; mentions and attachments merge
        // (de-duped on label / name to avoid double-context).
        const drained = queue.splice(0, queue.length)
        this.emit({
          kind: 'queue-consumed',
          conversationId: serverConversationId ?? turnId,
          injectionIds: drained.map((m) => m.injectionId),
        })
        activeParams = mergeQueuedAsParams(activeParams, drained)
        // Carry over any tool results the prior sub-turn produced
        // mid-round so the next sub-turn's request body commits
        // them to the proxy's history before the model's fresh
        // response. Undefined when the prior sub-turn finished
        // naturally (no leftover tool round to wrap up).
        prependedToolResults = subTurnResult.leftoverToolMessages
        subTurnIndex++
      }
    } catch (err) {
      // Catch-all for anything not handled by `runSubTurn`. The
      // sub-turn helper already maps abort + AiProxyError into
      // proper events; this branch only fires on truly unexpected
      // throws (e.g. a bug in the persistence path).
      this.opts.logger.error('chat-service: unhandled pipeline error', err)
      this.emit({
        kind: 'error',
        conversationId: serverConversationId ?? turnId,
        message: err instanceof Error ? err.message : String(err),
        code: 'internal_error',
      })
    } finally {
      this.active.delete(turnId)
      this.injections.delete(turnId)
    }
  }

  /**
   * Run one sub-turn end-to-end: open the proxy stream, drive the
   * tool-call loop, persist the assistant text, and emit `done`.
   * Returns a `terminate` flag the outer pipeline uses to decide
   * whether another sub-turn should be spawned from the queue or the
   * whole pipeline should exit.
   *
   * Mid-turn user input (queued via `inject()` while the model is
   * working) ends this sub-turn at the next tool-round boundary —
   * the leftover `toolMessages` come back as `leftoverToolMessages`
   * so the outer loop can prepend them to the next sub-turn's
   * request and the proxy still sees a complete
   * assistant(tool_calls) → tool(results) → user sequence.
   */
  private async runSubTurn(opts: {
    turnId: string
    activeParams: AiChatStartParams
    isNewForAssemble: boolean
    isFirstSubTurn: boolean
    serverConversationId: string | undefined
    nextMessages: AiChatMessage[]
    abortController: AbortController
    onMetadata: (md: { conversationId: string; model: string }) => void
  }): Promise<{
    terminate: boolean
    serverConversationId?: string
    /** Tool results produced in this sub-turn's last tool round but
     *  not yet handed to the model — set when the sub-turn ended at
     *  a tool-round boundary because of mid-turn user input. The
     *  outer pipeline prepends them to the next sub-turn's request
     *  body. Undefined on natural finish / error / abort. */
    leftoverToolMessages?: AiChatMessage[]
  }> {
    const {
      turnId,
      activeParams,
      isNewForAssemble,
      isFirstSubTurn,
      abortController,
      onMetadata,
    } = opts
    let { serverConversationId, nextMessages } = opts

    let totalAssistantText = ''
    const turnBlocks: PersistedBlock[] = []

    try {
      for (let iteration = 0; ; iteration++) {
        const { finishReason, pendingCalls, assistantText, usage } =
          await this.runStreamIteration({
            turnId,
            iteration,
            messages: nextMessages,
            conversationId: serverConversationId ?? activeParams.conversationId,
            isNew: isNewForAssemble,
            userText: activeParams.text,
            abortSignal: abortController.signal,
            blocks: turnBlocks,
            apiBaseUrl: activeParams.apiBaseUrl,
            deploymentId: activeParams.deploymentId,
            // `continueTurn` only applies to the FIRST iteration of
            // the FIRST sub-turn — that's the retry-from-error-
            // bubble path where the caller wants the server to run
            // against its stored history without accepting any
            // delta. Tool-round follow-ups and queued sub-turns
            // always carry real user/tool delta, so continueTurn
            // must be false there or the messages would be dropped.
            continueTurn:
              isFirstSubTurn && iteration === 0 && !!activeParams.continueTurn,
            // Emit `started` only once per pipeline. Sub-turns past
            // the first reuse the same conversationId — emitting
            // `started` again would make the webview reset its
            // per-turn flags (seenToolActivity, etc.) and cause
            // visual jitter.
            suppressStartedEmit: !isFirstSubTurn,
            onMetadata: (md) => {
              serverConversationId = md.conversationId
              onMetadata(md)
            },
          })
        totalAssistantText += assistantText

        if (finishReason !== 'tool_calls') {
          // Natural terminal state: stop, length, content_filter,
          // error, aborted. Persist the assistant turn locally
          // (proxy already owns the authoritative copy server-side)
          // and emit `done` for this sub-turn.
          if (serverConversationId) {
            await this.persistAssistantTurn(
              serverConversationId,
              totalAssistantText,
              {
                blocks: turnBlocks,
                ...(finishReason === 'aborted' ? { aborted: true } : {}),
              }
            )
          }
          this.emit({
            kind: 'done',
            conversationId: serverConversationId ?? turnId,
            finishReason,
            usage,
          })
          if (
            isFirstSubTurn &&
            isNewForAssemble &&
            serverConversationId &&
            finishReason === 'stop'
          ) {
            // Title generation goes through the default Legalese AI
            // proxy's stateless summize pipeline regardless of where
            // the turn itself ran — deployment ("Use in chat") chats
            // included. Anyone in an AI chat already has AI access
            // (the panel is gated on it), and the deployment's own
            // endpoint only serves its comply model, not summize, so
            // the default proxy is the correct target here.
            this.generateTitleInBackground(
              serverConversationId,
              activeParams.text
            )
          }
          // Only `stop` (clean finish) lets the pipeline drain
          // queued messages into another sub-turn. Length caps and
          // errors halt the whole pipeline so we don't burn tokens
          // re-running into the same wall.
          const terminate = finishReason !== 'stop'
          return { terminate, serverConversationId }
        }

        if (pendingCalls.length === 0) {
          this.opts.logger.warn(
            'finish_reason=tool_calls but no tool-call events received; stopping'
          )
          this.emit({
            kind: 'done',
            conversationId: serverConversationId ?? turnId,
            finishReason: 'stop',
          })
          return { terminate: true, serverConversationId }
        }

        // Execute each pending tool call.
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
          const block = turnBlocks.find(
            (b) => b.kind === 'tool-call' && b.callId === call.callId
          )
          if (block && block.kind === 'tool-call') {
            block.status = result.ok ? 'done' : 'error'
            if (result.ok) block.result = result.output
            else block.error = (result as { error: string }).error
          }
          toolMessages.push({
            role: 'tool',
            content,
            tool_call_id: call.callId,
            name: call.name,
          })
        }

        if (abortController.signal.aborted) {
          markStoppedToolCalls(turnBlocks)
          if (serverConversationId) {
            await this.persistAssistantTurn(
              serverConversationId,
              totalAssistantText,
              { aborted: true, blocks: turnBlocks }
            )
          }
          this.emit({
            kind: 'done',
            conversationId: serverConversationId ?? turnId,
            finishReason: 'aborted',
          })
          return { terminate: true, serverConversationId }
        }

        // Mid-turn user input: if anything was queued via inject()
        // during this tool round, end the sub-turn here instead of
        // looping back into another assistant call. The outer
        // pipeline drains the queue, emits `queue-consumed` +
        // `turn-spawn`, and seeds the next sub-turn with these
        // tool results prepended to the new user delta — the
        // proxy gets a clean assistant(tool_calls) → tool(results)
        // → user sequence in one request. Folding the feedback
        // into the tool result's `content` (the previous design)
        // mis-encoded a user redirect as tool output and confused
        // the model.
        const queuedDuringTool = this.injections.get(turnId) ?? []
        if (queuedDuringTool.length > 0) {
          if (serverConversationId) {
            await this.persistAssistantTurn(
              serverConversationId,
              totalAssistantText,
              { blocks: turnBlocks }
            )
          }
          this.emit({
            kind: 'done',
            conversationId: serverConversationId ?? turnId,
            finishReason: 'stop',
          })
          return {
            terminate: false,
            serverConversationId,
            leftoverToolMessages: toolMessages,
          }
        }

        // No queued user input — continue the inner loop with the
        // tool results as the next request body. The proxy's stored
        // history already includes the assistant tool_calls so we
        // don't re-send them.
        nextMessages = [...toolMessages]
      }
    } catch (err) {
      if (abortController.signal.aborted) {
        markStoppedToolCalls(turnBlocks)
        if (serverConversationId && totalAssistantText) {
          await this.persistAssistantTurn(
            serverConversationId,
            totalAssistantText,
            { aborted: true, blocks: turnBlocks }
          )
        }
        this.emit({
          kind: 'done',
          conversationId: serverConversationId ?? turnId,
          finishReason: 'aborted',
        })
        return { terminate: true, serverConversationId }
      }
      if (err instanceof AiProxyError) {
        this.emit({
          kind: 'error',
          conversationId: serverConversationId ?? turnId,
          message: err.message,
          code: err.code,
        })
        return { terminate: true, serverConversationId }
      }
      this.opts.logger.error('chat-service: unhandled sub-turn error', err)
      this.emit({
        kind: 'error',
        conversationId: serverConversationId ?? turnId,
        message: err instanceof Error ? err.message : String(err),
        code: 'internal_error',
      })
      return { terminate: true, serverConversationId }
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
    /** Chronological record the outer loop persists as `_meta.blocks`
     *  so the webview can rebuild rows on reload. Mutated in place. */
    blocks: PersistedBlock[]
    /** Deployment binding (plain-passthrough mode). When `apiBaseUrl`
     *  is set the request is POSTed there instead of the default
     *  proxy, no client tools are advertised, and `deploymentId` is
     *  stamped on the persisted conversation doc. */
    apiBaseUrl?: string
    deploymentId?: string
    /** Pass `continueTurn: true` through to the server so it skips
     *  extractDelta + appendMessages and runs another pass against
     *  the stored history. Only meaningful on iteration 0. */
    continueTurn: boolean
    /** Set on every sub-turn past the first within a single
     *  pipeline. The conversationId already exists from the first
     *  sub-turn's metadata frame, so re-emitting `started` would
     *  reset the webview's per-turn state (seenToolActivity,
     *  spinner gates) and cause flicker. */
    suppressStartedEmit?: boolean
    onMetadata: (md: { conversationId: string; model: string }) => void
  }): Promise<{
    finishReason: string
    pendingCalls: Array<{ callId: string; name: string; argsJson: string }>
    assistantText: string
    /** Cumulative prompt + completion tokens reported by the
     *  server on the terminal chunk, if any. Absent for
     *  tool_calls pauses (no terminal chunk yet). */
    usage?: { promptTokens: number; completionTokens: number }
  }> {
    const {
      turnId,
      iteration,
      messages,
      conversationId,
      abortSignal,
      blocks,
      continueTurn,
      suppressStartedEmit,
      onMetadata,
      apiBaseUrl,
    } = opts
    const deploymentMode = !!apiBaseUrl
    const pendingCalls: Array<{
      callId: string
      name: string
      argsJson: string
    }> = []
    let assistantText = ''
    let finishReason = 'stop'
    let usage: { promptTokens: number; completionTokens: number } | undefined
    let local: string | undefined = conversationId

    // Merge the current MCP server's deployed rules into the tool list
    // alongside our built-in fs/lsp/l4/meta tools. The MCP list is
    // cached in-process for a few seconds so multi-iteration turns
    // don't hit the local proxy every round.
    //
    // Deployment ("Use in chat") mode is a PLAIN PASSTHROUGH: the
    // deployment's own model owns rule evaluation, so we advertise no
    // client tools — shipping our local fs/lsp tools would let that
    // model try to read the operator's filesystem, which is both
    // wrong and confusing for a deployment-scoped chat.
    const mcpTools = deploymentMode
      ? []
      : await this.opts.mcp.listTools().catch((err) => {
          this.opts.logger.warn(
            `chat-service: mcp tools/list failed: ${err instanceof Error ? err.message : String(err)}`
          )
          return []
        })
    const tools = deploymentMode ? [] : [...BUILTIN_TOOLS, ...mcpTools]

    for await (const ev of this.opts.proxy.streamResilient(
      {
        messages,
        conversationId,
        // Client-declared tools every request; harmless when the model
        // doesn't call any.
        tools,
        stream: true,
        // Explicit retry verb; see outer loop for why only iteration
        // 0 may carry this. The proxy skips extractDelta on the
        // server so no duplicate user message lands in the stored
        // transcript.
        ...(continueTurn ? { continueTurn: true } : {}),
        // Deployment-scoped passthrough: POST against
        // `{apiBaseUrl}/v1/chat/completions` (and reattach there)
        // instead of the default Legalese AI proxy.
        ...(apiBaseUrl ? { apiBaseUrl } : {}),
        // Register this turn with the proxy's TurnRegistry so a
        // mid-stream transport drop auto-reattaches (since-cursor
        // protocol) and picks up the buffered frames without
        // re-running the turn.
        turnId: opts.turnId,
      },
      abortSignal,
      opts.turnId
    )) {
      if (ev.kind === 'metadata') {
        local = ev.conversationId
        onMetadata({ conversationId: ev.conversationId, model: ev.model })
        if (iteration === 0 && !suppressStartedEmit) {
          this.emit({
            kind: 'started',
            conversationId: ev.conversationId,
            turnId: opts.turnId,
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
              messages.filter((m) => m.role === 'user'),
              this.opts.extensionVersion,
              opts.deploymentId,
              apiBaseUrl
            )
            .catch((err) =>
              this.opts.logger.warn(
                `chat-service: initial user-turn save failed: ${err instanceof Error ? err.message : String(err)}`
              )
            )
        }
      } else if (ev.kind === 'text-delta') {
        assistantText += ev.text
        const tail = blocks[blocks.length - 1]
        if (tail && tail.kind === 'text') tail.text += ev.text
        else blocks.push({ kind: 'text', text: ev.text })
        this.emit({
          kind: 'text-delta',
          conversationId: local ?? turnId,
          text: ev.text,
        })
      } else if (ev.kind === 'thinking-delta') {
        this.emit({
          kind: 'thinking-delta',
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
          input: ev.input,
          output: ev.output,
          ruleId: ev.ruleId,
          deploymentId: ev.deploymentId,
          error: ev.error,
        })
        // Persist L4-rule activities so they survive a history
        // reload (the user explicitly wants server rule calls kept;
        // plain status tickers don't need to be). Identity + merge
        // mirror the webview store's `ruleActivityIdentity` /
        // `onToolActivity` exactly so the persisted block and the
        // live block collapse a `running → done` burst into ONE row
        // the same way.
        {
          const fromInput =
            ev.input && typeof ev.input === 'object'
              ? (ev.input as { function_name?: unknown }).function_name
              : undefined
          const ruleName =
            ev.ruleId ?? (typeof fromInput === 'string' ? fromInput : undefined)
          if (ruleName) {
            const ruleKey = `${ev.tool}::${ruleName}`
            const existing = blocks.find(
              (b) =>
                b.kind === 'tool-activity' &&
                b.ruleKey === ruleKey &&
                b.status === 'running'
            )
            if (existing && existing.kind === 'tool-activity') {
              existing.status = ev.status
              existing.message = ev.message
              if (ev.input !== undefined) existing.input = ev.input
              if (ev.output !== undefined) existing.output = ev.output
              if (ev.deploymentId) existing.deploymentId = ev.deploymentId
              if (ev.error !== undefined) existing.error = ev.error
            } else {
              blocks.push({
                kind: 'tool-activity',
                tool: ev.tool,
                ruleId: ruleName,
                ruleKey,
                status: ev.status,
                message: ev.message,
                input: ev.input,
                output: ev.output,
                deploymentId: ev.deploymentId,
                error: ev.error,
              })
            }
          }
        }
      } else if (ev.kind === 'tool-call') {
        // Status updates render as plain assistant prose, not as a
        // tool-call card. The model still goes through the normal
        // tool-result loop (so it can stop after the update), but the
        // user-visible side is a streamed text fragment merged into
        // the current assistant bubble. The dispatcher's executor for
        // this tool is a no-op that just returns "ok".
        if (ev.name === 'meta__post_status_update') {
          // Same dedupe story as the regular tool-call branch below:
          // the proxy emits two SSE frames per call (early
          // `tool-input-start` + final `tool-call`). Without this
          // guard we'd push two pendingCalls with the same callId,
          // POST two `role:"tool"` results to the proxy on the next
          // turn, and Anthropic would reject with "each tool_use
          // must have a single result". Also dedupe the inline-text
          // emission so the user doesn't see the status sentence
          // twice.
          const existingCall = pendingCalls.find((c) => c.callId === ev.callId)
          if (existingCall) {
            if (ev.argsJson) existingCall.argsJson = ev.argsJson
            continue
          }
          const text = extractStatusUpdateText(ev.argsJson)
          if (text) {
            assistantText += text
            const tail = blocks[blocks.length - 1]
            if (tail && tail.kind === 'text') tail.text += text
            else blocks.push({ kind: 'text', text })
            this.emit({
              kind: 'text-delta',
              conversationId: local ?? turnId,
              text,
            })
          }
          pendingCalls.push({
            callId: ev.callId,
            name: ev.name,
            argsJson: ev.argsJson,
          })
          continue
        }
        // Dedupe by callId: the proxy emits TWO tool_calls SSE chunks
        // per call now (an early `tool-input-start` frame with empty
        // args so the webview can pop a pulsating-dot row immediately,
        // followed by a final `tool-call` frame with the parsed args).
        // Without dedup we'd end up running the tool twice — and the
        // dispatcher would POST two tool_result messages back to the
        // proxy for the same toolCallId, which Anthropic rejects on
        // the next turn ("each tool_use must have a single result").
        // Update the existing pendingCall / block in place when the
        // callId is already known; otherwise push a new entry as
        // before.
        const existingCall = pendingCalls.find((c) => c.callId === ev.callId)
        if (existingCall) {
          if (ev.argsJson) existingCall.argsJson = ev.argsJson
          if (ev.name) existingCall.name = ev.name
        } else {
          pendingCalls.push({
            callId: ev.callId,
            name: ev.name,
            argsJson: ev.argsJson,
          })
        }
        // For `l4-rules__*` calls, resolve the original L4 function
        // name + deployment from the MCP target map so the row can
        // display the unsanitised name instead of the wire-level
        // sanitised slug. listTools() ran earlier in this turn (the
        // proxy received the tools array), so the map is populated.
        // For infra tools (list_files / read_file / etc.) and any
        // non-`l4-rules__` call this returns null and we leave the
        // fields undefined.
        const ruleTarget = this.opts.mcp.getToolTarget(ev.name)
        const existingBlock = blocks.find(
          (b) => b.kind === 'tool-call' && b.callId === ev.callId
        )
        if (existingBlock && existingBlock.kind === 'tool-call') {
          if (ev.argsJson) existingBlock.argsJson = ev.argsJson
          if (ev.name) existingBlock.name = ev.name
        } else {
          blocks.push({
            kind: 'tool-call',
            callId: ev.callId,
            name: ev.name,
            argsJson: ev.argsJson,
            status: 'running',
            ...(ruleTarget
              ? {
                  ruleFnName: ruleTarget.fnName,
                  deploymentId: ruleTarget.deployId,
                }
              : {}),
          })
        }
        // Pre-compute the initial status based on the user's permission
        // setting so the tool row renders in its real state from the
        // first frame (no merge race between the initial emit and the
        // dispatcher's later notifyStatus). `ask` → pending-approval so
        // the bottom Accept/Reject bar shows immediately.
        const category = categoryForTool(ev.name)
        const permission = category ? getPermission(category) : null
        const initialStatus: 'pending-approval' | 'running' =
          permission === 'ask' ? 'pending-approval' : 'running'
        // Re-emit on every frame — the webview's onToolCall merges by
        // callId, so the early frame creates the row and subsequent
        // frames update it (notably argsJson goes from "" → real args
        // when the final tool-call lands).
        this.emit({
          kind: 'tool-call',
          conversationId: local ?? turnId,
          callId: ev.callId,
          name: ev.name,
          argsJson: ev.argsJson,
          status: initialStatus,
          ...(ruleTarget
            ? {
                ruleFnName: ruleTarget.fnName,
                deploymentId: ruleTarget.deployId,
              }
            : {}),
        })
      } else if (ev.kind === 'done') {
        finishReason = ev.finishReason
        if (ev.usage) usage = ev.usage
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

    return { finishReason, pendingCalls, assistantText, usage }
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

    // Deployment ("Use in chat") mode is a plain passthrough to the
    // deployment's own model: no editor context, @-mention context,
    // session-context system message, or workspace bootstrap. Only the
    // user's message (plus any explicit attachments, assembled by the
    // shared tail below) goes through.
    const deploymentMode = !!params.apiBaseUrl

    // `includeActiveFile !== false` honors the default (send it) and
    // an explicit `true`; only `false` suppresses. The chat-input UI
    // exposes a per-send toggle for this.
    if (!deploymentMode && params.includeActiveFile !== false) {
      // Prefer the chip snapshot the webview captured at send time
      // (immune to multi-window / focus-race drift). Fall back to
      // live activeTextEditor when the webview didn't send one
      // (older builds, or "include active file" toggled on without a
      // file open at the moment of the send).
      const editorCtx = buildEditorContextMessage(params.activeFile)
      if (editorCtx) messages.push(editorCtx)
    }

    // @-mention context: the user's text already contains the literal
    // `@<path>` token, but on its own that's just a string the model
    // could read as ASCII art. Mirror the editor-context shape and tell
    // the model these tokens resolve to real workspace paths it can
    // open with fs__read_file. Without this hint, follow-up turns where
    // the user attaches an extra file via @ never see the body — the
    // model has no signal that the @-token is anything actionable.
    if (!deploymentMode) {
      const mentionCtx = buildMentionContextMessage(params.mentions)
      if (mentionCtx) messages.push(mentionCtx)
    }

    if (isNew && !deploymentMode) {
      const session = buildSessionContextMessage(
        this.opts.auth,
        this.opts.extensionVersion
      )
      if (session) messages.push(session)
      const bootstrap = await buildWorkspaceBootstrapMessage(this.opts.client)
      if (bootstrap) messages.push(bootstrap)
    }

    // Retry path: skip the user message entirely. The server's
    // explicit `continueTurn: true` flag (set in runStreamIteration
    // for iteration 0 when the outer loop was invoked via Retry)
    // tells the proxy to bypass extractDelta and run another pass
    // against the stored history. body.messages can be empty — the
    // proxy allows it when continueTurn is set, so we don't need
    // the old "continue" system-hint workaround.
    if (params.continueTurn) return messages

    // Multimodal user content: when the webview has staged attachments,
    // ship them as OpenAI-shaped content parts alongside the text so
    // the ai-proxy can pass them through to OpenAI as-is or translate
    // to Anthropic's image/document block shape.
    //
    // Anthropic's ai-sdk adapter only accepts PDFs in `file` parts
    // ("'Non-PDF files in user messages' functionality not
    // supported."), so plain text / markdown attachments can't use
    // the file block. Inline them as an additional `text` content
    // part wrapped with a filename marker instead — works across
    // every provider and keeps the source attribution visible to the
    // model. PDFs still go through as file blocks; images still use
    // image_url parts.
    if (params.attachments && params.attachments.length > 0) {
      const parts: Array<
        | { type: 'text'; text: string }
        | { type: 'image_url'; image_url: { url: string } }
        | { type: 'file'; file: { filename: string; file_data: string } }
      > = [{ type: 'text', text: params.text }]
      for (const att of params.attachments) {
        if (att.kind === 'image') {
          parts.push({
            type: 'image_url',
            image_url: {
              url: `data:${att.mediaType};base64,${att.dataBase64}`,
            },
          })
        } else if (isTextLikeMediaType(att.mediaType)) {
          // Decode utf-8 and inline. The wrapper tag mirrors the
          // `<editor-context>` / `<mention-context>` shape the model
          // already sees for filesystem content, so it treats the
          // body as an attached document rather than an ambiguous
          // second prompt.
          let body: string
          try {
            body = Buffer.from(att.dataBase64, 'base64').toString('utf-8')
          } catch (err) {
            this.opts.logger.warn(
              `assembleMessages: failed to decode text attachment ${att.name}: ${err instanceof Error ? err.message : String(err)}`
            )
            continue
          }
          parts.push({
            type: 'text',
            text: `<attached-file name="${att.name}" mediaType="${att.mediaType}">\n${body}\n</attached-file>`,
          })
        } else {
          parts.push({
            type: 'file',
            file: {
              filename: att.name,
              file_data: `data:${att.mediaType};base64,${att.dataBase64}`,
            },
          })
        }
      }
      messages.push({ role: 'user', content: parts })
    } else {
      messages.push({ role: 'user', content: params.text })
    }
    return messages
  }

  private async persistAssistantTurn(
    conversationId: string,
    text: string,
    meta?: Record<string, unknown>
  ): Promise<void> {
    // A sub-turn ending at a tool-round boundary may have produced
    // tool-call rows but no streamed text yet — still worth saving so
    // reload doesn't lose the tool-call history.
    const blocks = (meta as { blocks?: PersistedBlock[] } | undefined)?.blocks
    const hasBlocks = Array.isArray(blocks) && blocks.length > 0
    if (!text && !hasBlocks) return
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

/**
 * Synthesize an `AiChatStartParams` for a follow-up sub-turn from
 * the user messages queued during the previous sub-turn. Multiple
 * texts get joined with blank lines so the model treats them as one
 * thought; mentions and attachments dedupe on label / name to avoid
 * double-shipping the same context. The base params (conversationId,
 * turnId, includeActiveFile, activeFile) come from the original
 * `start()` call so abort + activity-file context stay consistent.
 */
function mergeQueuedAsParams(
  base: AiChatStartParams,
  drained: QueuedUserMessage[]
): AiChatStartParams {
  const text = drained
    .map((m) => m.text)
    .filter((t) => t.length > 0)
    .join('\n\n')
  const mentions: AiChatStartParams['mentions'] = []
  const seenMentions = new Set<string>()
  for (const m of drained) {
    for (const mention of m.mentions ?? []) {
      const key = `${mention.kind}:${mention.label}`
      if (seenMentions.has(key)) continue
      seenMentions.add(key)
      mentions.push(mention)
    }
  }
  const attachments: AiChatAttachment[] = []
  const seenAttachments = new Set<string>()
  for (const m of drained) {
    for (const att of m.attachments ?? []) {
      const key = `${att.kind}:${att.name}:${att.dataBase64.length}`
      if (seenAttachments.has(key)) continue
      seenAttachments.add(key)
      attachments.push(att)
    }
  }
  // Sub-turns reuse the base's includeActiveFile decision so a chat
  // that started "with editor context" keeps shipping it on each
  // follow-up. The webview's per-send chip toggle is only consulted
  // for the initial submit; after that the user has clearly opted
  // into a multi-message conversation and re-prompting them per
  // queued message would be hostile.
  return {
    ...base,
    text,
    mentions,
    attachments,
    // Ensure the retry flag never leaks past the first sub-turn.
    continueTurn: false,
  }
}

/**
 * Mark any in-flight tool-call blocks as `error: 'Stopped'` before the
 * turn gets persisted. Mirrors `cancelInflightToolCalls` on the
 * webview side — keeps the saved `_meta.blocks` honest so a Stop'd
 * tool call doesn't quietly turn into a `'done'` row when the user
 * reloads from history (extractPersistedBlocks defaults non-terminal
 * statuses to `'done'`, which would otherwise erase the Stop signal).
 */
function markStoppedToolCalls(blocks: PersistedBlock[]): void {
  for (const b of blocks) {
    // Rule-activity blocks pulsate while `running` (ToolCallRow's own
    // dot animation, independent of stream state), so a Stop'd eval
    // left at `running` would pulse forever after a reload. Flip it
    // to error like tool-call rows.
    if (b.kind !== 'tool-call' && b.kind !== 'tool-activity') continue
    if (b.status === 'running') {
      b.status = 'error'
      b.error = b.error ?? 'Stopped'
    }
  }
}

/**
 * Pull the user-facing text out of a `meta__post_status_update` call.
 * Returns `null` if the args don't parse or the `text` field is missing
 * — the caller treats that as "render nothing" and still acks the model.
 */
function extractStatusUpdateText(argsJson: string): string | null {
  if (!argsJson || !argsJson.trim()) return null
  let parsed: unknown
  try {
    parsed = JSON.parse(argsJson)
  } catch {
    return null
  }
  if (!parsed || typeof parsed !== 'object') return null
  const text = (parsed as { text?: unknown }).text
  if (typeof text !== 'string' || text.length === 0) return null
  return text
}

/**
 * Media types we inline as a text content part instead of shipping
 * as a `file` block. Anthropic rejects non-PDF file blocks
 * ("'Non-PDF files in user messages' functionality not supported"),
 * so the only way to pass e.g. `.json` / `.yaml` / `.csv` through is
 * to decode as UTF-8 and wrap with `<attached-file>` markers in a
 * text content part. Covers `text/*` plus the `application/*` types
 * whose payloads are conventionally UTF-8 documents.
 */
function isTextLikeMediaType(mediaType: string): boolean {
  if (mediaType.startsWith('text/')) return true
  return (
    mediaType === 'application/json' ||
    mediaType === 'application/xml' ||
    mediaType === 'application/x-yaml' ||
    mediaType === 'application/yaml'
  )
}

// Re-export so register.ts can type-check emitted events.
export type { AiConversation }
