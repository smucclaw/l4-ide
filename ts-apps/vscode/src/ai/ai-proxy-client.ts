import type { AiChatMessage } from 'jl4-client-rpc'
import type { AuthManager } from '../auth.js'
import type { AiLogger } from './logger.js'
import { buildCurrentTimeBlock } from './editor-context.js'

/**
 * Client-declared tool definition in the OpenAI function-tool shape the
 * ai-proxy expects. Phase 1 sends no tools; the field exists so the
 * chat-service API is stable.
 */
export interface AiProxyTool {
  type: 'function'
  function: {
    name: string
    description?: string
    parameters: Record<string, unknown>
  }
}

export interface AiProxyChatRequest {
  messages: AiChatMessage[]
  tools?: AiProxyTool[]
  conversationId?: string
  model?: string
  stream: true
  /** Explicit retry/resume verb. When true the ai-proxy skips
   *  extractDelta and runs another turn against the stored
   *  conversation's existing history. Empty `messages` is allowed
   *  in this mode. */
  continueTurn?: boolean
  /** Client-generated turn id — registered server-side so we can
   *  reattach via `GET /v1/chat/turns/{turnId}/stream` after a
   *  disconnect and pick up missed deltas. Required for reattach
   *  to work (the server falls back to a UUID when absent, but
   *  the client can't reconstruct that id). */
  turnId?: string
  /** Deployment-scoped base URL override
   *  (`https://ai.legalese.cloud/{orgSlug}/{deploymentId}`). When set,
   *  `stream()` / `reattach()` POST/GET against this instead of
   *  `getAiEndpoint()`. The deployment endpoint is the same ai-proxy
   *  stack, path-scoped, so the SSE `metadata` frame and turn-reattach
   *  protocol work identically. Local-mode is ignored when this is
   *  set (a deployment chat always needs real cloud auth). */
  apiBaseUrl?: string
}

/** Event union emitted by {@link streamChatCompletions} while parsing SSE. */
export type AiProxyStreamEvent =
  | {
      kind: 'metadata'
      conversationId: string
      model: string
      version?: string
    }
  | { kind: 'text-delta'; text: string }
  | { kind: 'thinking-delta'; text: string }
  | {
      kind: 'tool-activity'
      tool: string
      status: 'running' | 'done' | 'error'
      /** Bold action prefix the webview shows in front of `message`.
       *  Stamped by the proxy so the client doesn't need a per-tool
       *  name → label mapping. May be absent on older proxy events;
       *  the webview falls back to a sane default in that case. */
      label?: string
      message: string
      /** Verbatim model-supplied arguments. Present only for L4 Rule
       *  activities (the proxy emits the structured payload only when
       *  `kind === "rule"`); the webview uses this to render an L4
       *  Rule card identical to a client-side tool-call. */
      input?: unknown
      /** Verbatim tool result (set on `done`). L4 Rule activities only. */
      output?: unknown
      /** Deployed L4 function name when the activity wraps a rule. */
      ruleId?: string
      /** Deployment the rule lives in, when scoped. */
      deploymentId?: string
      /** Error detail when status is `error`. */
      error?: string
    }
  | {
      kind: 'tool-call'
      callId: string
      name: string
      argsJson: string
    }
  | {
      kind: 'done'
      finishReason: string
      usage?: { promptTokens: number; completionTokens: number }
    }
  | { kind: 'error'; message: string; code?: string }

/**
 * Mutable resume cursor shared with {@link parseSse}. Tracks the SSE
 * `id:` of the last frame fully delivered to the consumer so a
 * reattach after a mid-stream transport drop can ask the proxy to
 * replay strictly *after* it — no duplicate deltas. 0 = nothing
 * consumed yet (full replay on reattach).
 */
export interface SseCursor {
  lastEventId: number
}

/** Bounded reattach policy for {@link AiProxyClient.streamResilient}.
 *  A handful of quick retries covers laptop-lid / Wi-Fi-roam / ALB
 *  RST blips without spinning forever on a genuinely dead route. */
const MAX_REATTACHES = 5
const REATTACH_BACKOFF_MS = [500, 1000, 2000, 4000, 8000]

/** Abortable sleep — rejects immediately if the signal trips so a
 *  user "stop" during backoff doesn't wait out the timer. */
function delay(ms: number, signal: AbortSignal): Promise<void> {
  return new Promise((resolve, reject) => {
    if (signal.aborted) return reject(new DOMException('Aborted', 'AbortError'))
    const t = setTimeout(() => {
      signal.removeEventListener('abort', onAbort)
      resolve()
    }, ms)
    const onAbort = (): void => {
      clearTimeout(t)
      reject(new DOMException('Aborted', 'AbortError'))
    }
    signal.addEventListener('abort', onAbort, { once: true })
  })
}

/**
 * Production ai-proxy URL. The `LEGALESE_AI_ENDPOINT` env var stays as
 * an undocumented escape hatch for developers running on a non-default
 * port. End users flip the `legaleseAi.localMode` setting instead —
 * see `getAiEndpoint` below.
 */
const PROD_AI_ENDPOINT = (
  process.env.LEGALESE_AI_ENDPOINT ?? 'https://ai.legalese.cloud'
).replace(/\/$/, '')

/** Local ai-proxy URL used when `legaleseAi.localMode` is enabled. */
const LOCAL_AI_ENDPOINT = 'http://127.0.0.1:3000'

/**
 * True when the user has flipped `legaleseAi.localMode` on. Read fresh
 * each request so toggling the setting takes effect without a reload.
 */
export function isLocalMode(): boolean {
  try {
    // Lazy import: this module is also pulled into non-extension
    // contexts (tests) where `vscode` isn't on the runtime path.
    // Falling back to false there is the right default.
    // eslint-disable-next-line @typescript-eslint/no-require-imports
    const vscode = require('vscode') as typeof import('vscode')
    return (
      vscode.workspace
        .getConfiguration()
        .get<boolean>('legaleseAi.localMode') === true
    )
  } catch {
    return false
  }
}

/**
 * Resolve the active ai-proxy base URL. Re-evaluated per call so the
 * setting toggle applies immediately to subsequent requests.
 */
export function getAiEndpoint(): string {
  return isLocalMode() ? LOCAL_AI_ENDPOINT : PROD_AI_ENDPOINT
}

/** Back-compat: callers that want a snapshot of the production URL. */
export const AI_ENDPOINT = PROD_AI_ENDPOINT

export interface AiProxyClientOptions {
  auth: AuthManager
  logger: AiLogger
}

export class AiProxyClient {
  constructor(private readonly opts: AiProxyClientOptions) {}

  /**
   * POST /v1/chat/completions and stream SSE events as an async iterator.
   * Throws on network / 4xx / 5xx; those are the caller's cue to emit a
   * terminal error event to the webview. Aborting the returned signal
   * cancels the upstream request mid-stream.
   */
  async *stream(
    request: AiProxyChatRequest,
    abortSignal: AbortSignal,
    cursor?: SseCursor
  ): AsyncGenerator<AiProxyStreamEvent> {
    const endpoint = request.apiBaseUrl ?? getAiEndpoint()
    const local = request.apiBaseUrl ? false : isLocalMode()
    const url = `${endpoint}/v1/chat/completions`
    const headers = await this.opts.auth.getAiAuthHeaders()
    let hasAuth = !!headers.Authorization
    if (!hasAuth && local) {
      // Local proxy in dev mode ignores Authorization but still
      // requires the header to exist. Stamp a synthetic value so the
      // request goes through without forcing a real sign-in.
      headers.Authorization = 'Bearer dev-local'
      hasAuth = true
    }
    this.opts.logger.info(
      `POST ${url} (conversationId=${request.conversationId ?? '<new>'}, auth=${hasAuth ? 'bearer' : 'none'})`
    )
    if (!hasAuth) {
      // Fail fast instead of hitting the server with empty
      // credentials — that would just return 401, but surfacing it
      // here with a clearer code/message is friendlier.
      throw new AiProxyError(
        'No Legalese Cloud session found. Sign in from the sidebar to use AI chat.',
        'unauthenticated',
        401
      )
    }
    // Inline the current-time tag into the trailing user message
    // (non-mutating clone) so the proxy sees wall-clock context on
    // every turn. The local conversation store keeps the user's
    // original text — what the chat bubble shows on reload — while
    // the wire payload carries the timestamp. role:"system" wouldn't
    // work here because the proxy's extractDelta filters system
    // messages out of follow-up-turn deltas.
    const wireMessages = withCurrentTimeOnLastUser(request.messages)
    const res = await fetch(url, {
      method: 'POST',
      headers: {
        ...headers,
        'Content-Type': 'application/json',
        Accept: 'text/event-stream',
      },
      body: JSON.stringify({
        ...request,
        messages: wireMessages,
        stream: true,
      }),
      signal: abortSignal,
    })
    this.opts.logger.info(
      `response ${res.status} ${res.statusText} (content-type=${res.headers.get('content-type') ?? '?'})`
    )
    if (!res.ok || !res.body) {
      let body = ''
      try {
        body = await res.text()
      } catch {
        // ignore
      }
      const message = extractErrorMessage(body) ?? `HTTP ${res.status}`
      const code = extractErrorCode(body) ?? httpStatusToCode(res.status)
      throw new AiProxyError(message, code, res.status)
    }
    yield* parseSse(res.body, this.opts.logger, cursor)
  }

  /**
   * GET /v1/chat/turns/{turnId}/stream. Rebinds to an existing
   * server-side turn after a client disconnect and replays + tails
   * the SSE buffer. `sinceId` (the last fully-consumed SSE id) is
   * sent as `Last-Event-ID` so the proxy replays strictly after it —
   * no duplicate deltas. Throws AiProxyError on 404 (turn unknown or
   * reaped) so the caller can fall back to a fresh request.
   * Same SSE parse path as `stream()`.
   */
  async *reattach(
    turnId: string,
    abortSignal: AbortSignal,
    cursor?: SseCursor,
    apiBaseUrl?: string
  ): AsyncGenerator<AiProxyStreamEvent> {
    const endpoint = apiBaseUrl ?? getAiEndpoint()
    const sinceId = cursor?.lastEventId ?? 0
    const url =
      `${endpoint}/v1/chat/turns/${encodeURIComponent(turnId)}/stream` +
      (sinceId > 0 ? `?since=${sinceId}` : '')
    const headers = await this.opts.auth.getAiAuthHeaders()
    if (!headers.Authorization && !apiBaseUrl && isLocalMode()) {
      headers.Authorization = 'Bearer dev-local'
    }
    if (!headers.Authorization) {
      throw new AiProxyError(
        'No Legalese Cloud session found. Sign in from the sidebar to use AI chat.',
        'unauthenticated',
        401
      )
    }
    this.opts.logger.info(`GET ${url} (reattach, since=${sinceId})`)
    const res = await fetch(url, {
      method: 'GET',
      headers: {
        ...headers,
        Accept: 'text/event-stream',
        // Standard SSE resume header; the proxy also accepts the
        // `?since=` query above for belt-and-braces.
        ...(sinceId > 0 ? { 'Last-Event-ID': String(sinceId) } : {}),
      },
      signal: abortSignal,
    })
    this.opts.logger.info(`reattach response ${res.status} ${res.statusText}`)
    if (res.status === 404) {
      throw new AiProxyError('Turn not found or expired', 'not_found', 404)
    }
    if (!res.ok || !res.body) {
      let body = ''
      try {
        body = await res.text()
      } catch {
        // ignore
      }
      const message = extractErrorMessage(body) ?? `HTTP ${res.status}`
      const code = extractErrorCode(body) ?? httpStatusToCode(res.status)
      throw new AiProxyError(message, code, res.status)
    }
    yield* parseSse(res.body, this.opts.logger, cursor)
  }

  /**
   * `stream()` with automatic reattach across transient transport
   * drops (Wi-Fi roam, laptop lid, ALB RST during a long
   * think/tool-execute gap). The proxy keeps running the turn and
   * buffers every frame; on a network-level failure we GET the
   * reattach endpoint with the resume cursor and keep yielding into
   * the *same* consumer, so the caller sees one continuous event
   * stream and never has to re-run the turn.
   *
   * NOT retried: user abort, and `AiProxyError` (a real HTTP/protocol
   * outcome — 4xx/5xx, or the proxy's typed mid-stream `error` event
   * which is yielded, not thrown). Those are terminal by design;
   * replaying the buffer would just hit the same wall.
   */
  async *streamResilient(
    request: AiProxyChatRequest,
    abortSignal: AbortSignal,
    turnId: string
  ): AsyncGenerator<AiProxyStreamEvent> {
    const cursor: SseCursor = { lastEventId: 0 }
    let gen = this.stream(request, abortSignal, cursor)
    let reattaches = 0
    for (;;) {
      try {
        yield* gen
        return
      } catch (err) {
        if (abortSignal.aborted) throw err
        if (err instanceof AiProxyError) throw err
        if (reattaches >= MAX_REATTACHES) {
          this.opts.logger.warn(
            `stream interrupted and reattach budget exhausted (${reattaches}); surfacing error`
          )
          throw err
        }
        const backoff =
          REATTACH_BACKOFF_MS[
            Math.min(reattaches, REATTACH_BACKOFF_MS.length - 1)
          ]
        reattaches++
        this.opts.logger.warn(
          `stream interrupted (${err instanceof Error ? err.message : String(err)}); ` +
            `reattach ${reattaches}/${MAX_REATTACHES} after ${backoff}ms (since=${cursor.lastEventId})`
        )
        try {
          await delay(backoff, abortSignal)
        } catch {
          throw err // aborted during backoff
        }
        try {
          gen = this.reattach(turnId, abortSignal, cursor, request.apiBaseUrl)
        } catch (re) {
          // 404 (turn reaped / wrong task) or auth — can't resume;
          // surface the ORIGINAL transport error so the UI shows a
          // retry affordance rather than a confusing "not found".
          this.opts.logger.warn(
            `reattach failed (${re instanceof Error ? re.message : String(re)}); surfacing original error`
          )
          throw err
        }
      }
    }
  }

  /**
   * Fire-and-forget title generation using the summize pipeline. Keeps
   * the call stateless (no conversationId) so it doesn't create junk
   * conversation files on the server.
   */
  async summarizeTitle(firstUserMessage: string): Promise<string | null> {
    const url = `${getAiEndpoint()}/v1/chat/completions`
    const headers = await this.opts.auth.getAiAuthHeaders()
    if (!headers.Authorization && isLocalMode()) {
      headers.Authorization = 'Bearer dev-local'
    }
    const body = {
      model: 'legalese-summize-4',
      messages: [
        {
          role: 'user' as const,
          content: `Generate a 4-6 word title for a chat that begins: "${firstUserMessage.slice(
            0,
            500
          )}". Respond with just the title, no quotes, no trailing period.`,
        },
      ],
    }
    try {
      const res = await fetch(url, {
        method: 'POST',
        headers: {
          ...headers,
          'Content-Type': 'application/json',
          Accept: 'text/event-stream',
        },
        body: JSON.stringify(body),
      })
      if (!res.ok || !res.body) return null
      let text = ''
      for await (const ev of parseSse(res.body)) {
        if (ev.kind === 'text-delta') text += ev.text
        if (ev.kind === 'done') break
      }
      text = text.trim().replace(/^["']|["']$/g, '')
      return text.length > 0 ? text : null
    } catch (err) {
      this.opts.logger.warn(
        `Title generation failed: ${err instanceof Error ? err.message : String(err)}`
      )
      return null
    }
  }

  /**
   * One-shot "intended use" description for a set of about-to-be-deployed
   * functions, using the same stateless summize pipeline as
   * {@link summarizeTitle} (no conversationId, so no junk conversation
   * files server-side). Returns null on any failure — the caller
   * surfaces a friendly message.
   */
  async describeIntendedUse(functionsJson: string): Promise<string | null> {
    const url = `${getAiEndpoint()}/v1/chat/completions`
    const headers = await this.opts.auth.getAiAuthHeaders()
    if (!headers.Authorization && isLocalMode()) {
      headers.Authorization = 'Bearer dev-local'
    }
    const body = {
      model: 'legalese-summize-4',
      messages: [
        {
          role: 'system' as const,
          content:
            `You write the "Intended use" blurb for a deployment of L4 ` +
            `legal rules. You are given the JSON schemas of the ` +
            `deployed rules purely as evidence of the subject matter — ` +
            `they are rules, never "functions", and the schema itself ` +
            `is an implementation detail the reader does not care ` +
            `about.\n\n` +
            `Describe WHY someone would deploy these rules and WHEN ` +
            `they apply: the real-world legal or business situation ` +
            `they govern, who relies on them, and the circumstances or ` +
            `decisions in which they come into play. Do NOT explain ` +
            `what the rules compute, how they work, their inputs or ` +
            `outputs, or list them one by one. Think "what this is for", ` +
            `not "what this does".\n\n` +
            `Write 2-4 plain-English sentences for a legal or business ` +
            `operator, not a programmer. Stay under 1500 characters ` +
            `total. No markdown, no preamble — respond with the blurb ` +
            `text only.`,
        },
        {
          role: 'user' as const,
          content: `Deployed rule schemas:\n\n${functionsJson.slice(0, 12000)}`,
        },
      ],
    }
    try {
      const res = await fetch(url, {
        method: 'POST',
        headers: {
          ...headers,
          'Content-Type': 'application/json',
          Accept: 'text/event-stream',
        },
        body: JSON.stringify(body),
      })
      if (!res.ok || !res.body) return null
      let text = ''
      for await (const ev of parseSse(res.body)) {
        if (ev.kind === 'text-delta') text += ev.text
        if (ev.kind === 'done') break
      }
      text = text.trim()
      // Mirror the server-side cap (`maxDescriptionLength` in
      // jl4-service/src/ControlPlane.hs) and the textarea's maxlength so
      // the drafted text is never longer than the field can hold.
      if (text.length > 1500) text = text.slice(0, 1500)
      return text.length > 0 ? text : null
    } catch (err) {
      this.opts.logger.warn(
        `Intended-use generation failed: ${err instanceof Error ? err.message : String(err)}`
      )
      return null
    }
  }
}

/**
 * Error from the ai-proxy that carries a structured code / status so
 * the UI can branch on it (`unauthenticated`, `daily_token_limit_exceeded`,
 * etc.).
 */
export class AiProxyError extends Error {
  constructor(
    message: string,
    public readonly code: string,
    public readonly status?: number
  ) {
    super(message)
    this.name = 'AiProxyError'
  }
}

function httpStatusToCode(status: number): string {
  if (status === 401) return 'unauthenticated'
  if (status === 403) return 'forbidden'
  if (status === 404) return 'not_found'
  if (status === 429) return 'rate_limited'
  if (status >= 500) return 'upstream_error'
  return 'request_failed'
}

/**
 * Return a NEW messages array with the {@link buildCurrentTimeBlock}
 * prepended to the trailing `role:"user"` message's content. Pure —
 * the input array and its messages aren't mutated, so the caller's
 * local-store copy stays clean of wire-only metadata. When there's no
 * user message in the body (tool-result follow-ups, `continueTurn`)
 * the original array is returned unchanged.
 */
function withCurrentTimeOnLastUser(messages: AiChatMessage[]): AiChatMessage[] {
  let lastUserIdx = -1
  for (let i = messages.length - 1; i >= 0; i--) {
    if (messages[i]!.role === 'user') {
      lastUserIdx = i
      break
    }
  }
  if (lastUserIdx < 0) return messages
  const timeBlock = buildCurrentTimeBlock()
  const original = messages[lastUserIdx]!
  const content = original.content
  let augmented: AiChatMessage['content']
  if (typeof content === 'string') {
    augmented = `${timeBlock}\n\n${content}`
  } else if (Array.isArray(content)) {
    augmented = [{ type: 'text', text: timeBlock }, ...content]
  } else {
    // null content + role:"user" is degenerate; leave it alone.
    return messages
  }
  const next = messages.slice()
  next[lastUserIdx] = { ...original, content: augmented }
  return next
}

function extractErrorMessage(body: string): string | undefined {
  try {
    const parsed = JSON.parse(body) as {
      error?: { message?: string }
    }
    return parsed.error?.message
  } catch {
    return body.slice(0, 200) || undefined
  }
}

function extractErrorCode(body: string): string | undefined {
  try {
    const parsed = JSON.parse(body) as {
      error?: { code?: string }
    }
    return parsed.error?.code
  } catch {
    return undefined
  }
}

/**
 * SSE parser. Reads the response body stream, splits on `\n\n` frame
 * boundaries, and emits typed events. Handles both `event:`-prefixed
 * frames (our `metadata` / `tool_activity`) and the OpenAI-compat
 * `chat.completion.chunk` data-only frames.
 */
async function* parseSse(
  body: ReadableStream<Uint8Array>,
  logger?: AiLogger,
  cursor?: SseCursor
): AsyncGenerator<AiProxyStreamEvent> {
  const reader = body.getReader()
  const decoder = new TextDecoder('utf-8')
  let buffer = ''
  let totalBytes = 0
  let totalFrames = 0
  let firstByteLogged = false
  // Accept either \n\n (standard SSE) or \r\n\r\n (some edge proxies).
  const FRAME_BOUNDARY = /\r?\n\r?\n/
  while (true) {
    const { value, done } = await reader.read()
    if (done) {
      logger?.info(`stream closed (bytes=${totalBytes}, frames=${totalFrames})`)
      break
    }
    totalBytes += value.byteLength
    if (!firstByteLogged) {
      firstByteLogged = true
      logger?.info(`first bytes received (${value.byteLength})`)
    }
    buffer += decoder.decode(value, { stream: true })
    let match: RegExpExecArray | null
    while ((match = FRAME_BOUNDARY.exec(buffer)) !== null) {
      const rawFrame = buffer.slice(0, match.index)
      buffer = buffer.slice(match.index + match[0].length)
      const frame = parseFrame(rawFrame)
      if (!frame) continue
      totalFrames++
      yield* interpretFrame(frame, logger)
      // Advance the resume cursor only AFTER the consumer has pulled
      // every event this frame produced — so a reattach asks the
      // proxy to replay strictly after the last frame we fully
      // handled, never re-delivering a partially-applied one.
      if (cursor && frame.id !== undefined) cursor.lastEventId = frame.id
    }
  }
  if (buffer.trim().length > 0) {
    const frame = parseFrame(buffer)
    if (frame) {
      yield* interpretFrame(frame, logger)
      if (cursor && frame.id !== undefined) cursor.lastEventId = frame.id
    }
  }
}

interface SseFrame {
  event?: string
  data: string
  /** SSE `id:` field — monotonic per-turn sequence the proxy stamps
   *  on every event/data frame. Absent on keepalive comments. */
  id?: number
}

function parseFrame(raw: string): SseFrame | null {
  const lines = raw.split('\n')
  let event: string | undefined
  let id: number | undefined
  const dataLines: string[] = []
  for (const line of lines) {
    if (line.startsWith('event:')) {
      event = line.slice('event:'.length).trim()
    } else if (line.startsWith('data:')) {
      dataLines.push(line.slice('data:'.length).trimStart())
    } else if (line.startsWith('id:')) {
      const n = Number.parseInt(line.slice('id:'.length).trim(), 10)
      if (Number.isFinite(n)) id = n
    }
  }
  if (dataLines.length === 0) return null
  return { event, data: dataLines.join('\n'), id }
}

function* interpretFrame(
  frame: SseFrame,
  logger?: AiLogger
): Generator<AiProxyStreamEvent> {
  logger?.debug(
    `frame event=${frame.event ?? '<default>'} data[0..80]=${frame.data.slice(0, 80)}`
  )
  if (frame.data === '[DONE]') return
  if (frame.event === 'metadata') {
    try {
      const payload = JSON.parse(frame.data) as {
        conversationId?: string
        model?: string
        version?: string
      }
      if (payload.conversationId && payload.model) {
        yield {
          kind: 'metadata',
          conversationId: payload.conversationId,
          model: payload.model,
          version: payload.version,
        }
      }
    } catch {
      // ignore malformed metadata
    }
    return
  }
  if (frame.event === 'thinking_delta') {
    try {
      const payload = JSON.parse(frame.data) as { text?: string }
      if (payload.text) {
        yield { kind: 'thinking-delta', text: payload.text }
      }
    } catch {
      // ignore malformed thinking_delta
    }
    return
  }
  if (frame.event === 'error') {
    // The ai-proxy emits this after an SSE stream has already
    // started (headers sent) but the generate/stream pass threw —
    // e.g. Anthropic returning a non-retryable error mid-turn.
    // The webview's store handler maps this to `turn.error` and
    // the ErrorBubble component renders the user-facing message +
    // Retry button.
    try {
      const payload = JSON.parse(frame.data) as {
        message?: string
        code?: string
      }
      yield {
        kind: 'error',
        message: payload.message ?? 'Upstream LLM call failed',
        code: payload.code,
      }
    } catch {
      yield { kind: 'error', message: 'Upstream LLM call failed' }
    }
    return
  }
  if (frame.event === 'tool_activity') {
    try {
      const payload = JSON.parse(frame.data) as {
        activities?: Array<{
          tool: string
          status: 'running' | 'done' | 'error'
          label?: string
          message: string
          input?: unknown
          output?: unknown
          ruleId?: string
          deploymentId?: string
          error?: string
        }>
      }
      for (const a of payload.activities ?? []) {
        yield {
          kind: 'tool-activity',
          tool: a.tool,
          status: a.status,
          label: a.label,
          message: a.message,
          input: a.input,
          output: a.output,
          ruleId: a.ruleId,
          deploymentId: a.deploymentId,
          error: a.error,
        }
      }
    } catch {
      // ignore
    }
    return
  }
  // Default: OpenAI-compat chat.completion.chunk
  try {
    const chunk = JSON.parse(frame.data) as {
      choices?: Array<{
        delta?: {
          content?: string | null
          tool_calls?: Array<{
            index: number
            id?: string
            type?: string
            function?: { name?: string; arguments?: string }
          }>
        }
        finish_reason?: string | null
      }>
      usage?: { prompt_tokens?: number; completion_tokens?: number }
    }
    const choice = chunk.choices?.[0]
    if (!choice) return
    if (choice.delta?.content) {
      yield { kind: 'text-delta', text: choice.delta.content }
    }
    if (choice.delta?.tool_calls) {
      for (const tc of choice.delta.tool_calls) {
        if (tc.id && tc.function?.name) {
          yield {
            kind: 'tool-call',
            callId: tc.id,
            name: tc.function.name,
            argsJson: tc.function.arguments ?? '{}',
          }
        }
      }
    }
    if (choice.finish_reason) {
      const usage = chunk.usage
        ? {
            promptTokens: chunk.usage.prompt_tokens ?? 0,
            completionTokens: chunk.usage.completion_tokens ?? 0,
          }
        : undefined
      yield { kind: 'done', finishReason: choice.finish_reason, usage }
    }
  } catch {
    // malformed chunk; skip
  }
}
