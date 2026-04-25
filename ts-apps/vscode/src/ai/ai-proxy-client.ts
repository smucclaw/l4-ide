import type { AiChatMessage } from 'jl4-client-rpc'
import type { AuthManager } from '../auth.js'
import type { AiLogger } from './logger.js'

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
      message: string
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
    abortSignal: AbortSignal
  ): AsyncGenerator<AiProxyStreamEvent> {
    const endpoint = getAiEndpoint()
    const local = isLocalMode()
    const url = `${endpoint}/v1/chat/completions`
    const headers = await this.opts.auth.getAuthHeaders()
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
    const res = await fetch(url, {
      method: 'POST',
      headers: {
        ...headers,
        'Content-Type': 'application/json',
        Accept: 'text/event-stream',
      },
      body: JSON.stringify({ ...request, stream: true }),
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
    yield* parseSse(res.body, this.opts.logger)
  }

  /**
   * GET /v1/chat/turns/{turnId}/stream. Rebinds to an existing
   * server-side turn after a client disconnect and replays + tails
   * the SSE buffer. Throws AiProxyError on 404 (turn unknown or
   * reaped) so the caller can fall back to a fresh request.
   * Same SSE parse path as `stream()`.
   */
  async *reattach(
    turnId: string,
    abortSignal: AbortSignal
  ): AsyncGenerator<AiProxyStreamEvent> {
    const endpoint = getAiEndpoint()
    const url = `${endpoint}/v1/chat/turns/${encodeURIComponent(turnId)}/stream`
    const headers = await this.opts.auth.getAuthHeaders()
    if (!headers.Authorization && isLocalMode()) {
      headers.Authorization = 'Bearer dev-local'
    }
    if (!headers.Authorization) {
      throw new AiProxyError(
        'No Legalese Cloud session found. Sign in from the sidebar to use AI chat.',
        'unauthenticated',
        401
      )
    }
    this.opts.logger.info(`GET ${url} (reattach)`)
    const res = await fetch(url, {
      method: 'GET',
      headers: { ...headers, Accept: 'text/event-stream' },
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
    yield* parseSse(res.body, this.opts.logger)
  }

  /**
   * Fire-and-forget title generation using the summize pipeline. Keeps
   * the call stateless (no conversationId) so it doesn't create junk
   * conversation files on the server.
   */
  async summarizeTitle(firstUserMessage: string): Promise<string | null> {
    const url = `${getAiEndpoint()}/v1/chat/completions`
    const headers = await this.opts.auth.getAuthHeaders()
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
  logger?: AiLogger
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
    }
  }
  if (buffer.trim().length > 0) {
    const frame = parseFrame(buffer)
    if (frame) yield* interpretFrame(frame, logger)
  }
}

interface SseFrame {
  event?: string
  data: string
}

function parseFrame(raw: string): SseFrame | null {
  const lines = raw.split('\n')
  let event: string | undefined
  const dataLines: string[] = []
  for (const line of lines) {
    if (line.startsWith('event:')) {
      event = line.slice('event:'.length).trim()
    } else if (line.startsWith('data:')) {
      dataLines.push(line.slice('data:'.length).trimStart())
    }
  }
  if (dataLines.length === 0) return null
  return { event, data: dataLines.join('\n') }
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
          message: string
        }>
      }
      for (const a of payload.activities ?? []) {
        yield {
          kind: 'tool-activity',
          tool: a.tool,
          status: a.status,
          message: a.message,
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
