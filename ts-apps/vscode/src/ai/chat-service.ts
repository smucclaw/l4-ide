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
  | { kind: 'started'; conversationId: string; model: string }
  | { kind: 'text-delta'; conversationId: string; text: string }
  | { kind: 'thinking-delta'; conversationId: string; text: string }
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
    // and the chronological block list (for persistence + replay on
    // reload).
    let serverConversationId: string | undefined = params.conversationId
    let totalAssistantText = ''
    const turnBlocks: PersistedBlock[] = []

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
        const { finishReason, pendingCalls, assistantText, usage } =
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
            blocks: turnBlocks,
            // `continueTurn` only applies to iteration 0 — that's
            // the retry-from-error-bubble path where the caller
            // wants the server to run against its stored history
            // without accepting any delta. Subsequent iterations
            // are tool-round follow-ups and always carry real
            // tool-result delta, so continueTurn must be false
            // there or the results would be dropped.
            continueTurn: iteration === 0 && !!params.continueTurn,
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
          // Update the persisted block with the final outcome so the
          // replayed row shows the right status after reload.
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

        // If the user hit Stop (or sent a new message) while we were
        // awaiting approval, the abort signal is now set and the
        // approvals were auto-denied. Don't start another proxy
        // round-trip — finalize as aborted.
        if (abortController.signal.aborted) {
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
          break
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
            { aborted: true, blocks: turnBlocks }
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
    /** Chronological record the outer loop persists as `_meta.blocks`
     *  so the webview can rebuild rows on reload. Mutated in place. */
    blocks: PersistedBlock[]
    /** Pass `continueTurn: true` through to the server so it skips
     *  extractDelta + appendMessages and runs another pass against
     *  the stored history. Only meaningful on iteration 0. */
    continueTurn: boolean
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
      onMetadata,
    } = opts
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
    const mcpTools = await this.opts.mcp.listTools().catch((err) => {
      this.opts.logger.warn(
        `chat-service: mcp tools/list failed: ${err instanceof Error ? err.message : String(err)}`
      )
      return []
    })
    const tools = [...BUILTIN_TOOLS, ...mcpTools]

    for await (const ev of this.opts.proxy.stream(
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
        // Register this turn with the proxy's TurnRegistry so a
        // reattach call (future: manual, or auto on stream error
        // once we've got since-cursor protocol) can pick up the
        // buffered frames after a disconnect.
        turnId: opts.turnId,
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
              messages.filter((m) => m.role === 'user'),
              this.opts.extensionVersion
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
        })
      } else if (ev.kind === 'tool-call') {
        // Status updates render as plain assistant prose, not as a
        // tool-call card. The model still goes through the normal
        // tool-result loop (so it can stop after the update), but the
        // user-visible side is a streamed text fragment merged into
        // the current assistant bubble. The dispatcher's executor for
        // this tool is a no-op that just returns "ok".
        if (ev.name === 'meta__post_status_update') {
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
        pendingCalls.push({
          callId: ev.callId,
          name: ev.name,
          argsJson: ev.argsJson,
        })
        // Pre-compute the initial status based on the user's permission
        // setting so the tool row renders in its real state from the
        // first frame (no merge race between the initial emit and the
        // dispatcher's later notifyStatus). `ask` → pending-approval so
        // the bottom Accept/Reject bar shows immediately.
        const category = categoryForTool(ev.name)
        const permission = category ? getPermission(category) : null
        const initialStatus: 'pending-approval' | 'running' =
          permission === 'ask' ? 'pending-approval' : 'running'
        blocks.push({
          kind: 'tool-call',
          callId: ev.callId,
          name: ev.name,
          argsJson: ev.argsJson,
          status: 'running',
        })
        this.emit({
          kind: 'tool-call',
          conversationId: local ?? turnId,
          callId: ev.callId,
          name: ev.name,
          argsJson: ev.argsJson,
          status: initialStatus,
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

    // `includeActiveFile !== false` honors the default (send it) and
    // an explicit `true`; only `false` suppresses. The chat-input UI
    // exposes a per-send toggle for this.
    if (params.includeActiveFile !== false) {
      const editorCtx = buildEditorContextMessage()
      if (editorCtx) messages.push(editorCtx)
    }

    // @-mention context: the user's text already contains the literal
    // `@<path>` token, but on its own that's just a string the model
    // could read as ASCII art. Mirror the editor-context shape and tell
    // the model these tokens resolve to real workspace paths it can
    // open with fs__read_file. Without this hint, follow-up turns where
    // the user attaches an extra file via @ never see the body — the
    // model has no signal that the @-token is anything actionable.
    const mentionCtx = buildMentionContextMessage(params.mentions)
    if (mentionCtx) messages.push(mentionCtx)

    if (isNew) {
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
