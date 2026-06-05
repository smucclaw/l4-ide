// Browser transport bridge.
//
// The copied store + panel were written to talk to the VSCode extension host
// over `vscode-messenger`: they SEND `AiChatStart`/`AiChatAbort`/… and expect
// the host to push back `AiChatStarted`/`AiChatTextDelta`/… notifications. This
// bridge implements just enough of that messenger surface to run a
// DEPLOYMENT-SCOPED chat directly from the browser: on `AiChatStart` it streams
// `POST {apiBaseUrl}/v1/chat/completions` (SSE) and translates each frame into
// the same notification the host would have sent. Conversation history is the
// browser's responsibility (the extension's job in the IDE), so the bridge also
// owns localStorage persistence and assembles the full message array each turn.
//
// Only the deployment path is implemented — server-side tools (the `comply`
// pipeline) run in the proxy, so there is no client tool dispatch, MCP, editor
// context, mentions, or permissions here.

import {
  // inbound (host → webview) — we emit these into the store's handlers
  AiActiveFile,
  AiAuthStatus,
  AiChatDone,
  AiChatError,
  AiChatStarted,
  AiChatTextDelta,
  AiChatThinkingDelta,
  AiChatToolActivity,
  AiChatToolCall,
  AiUsageUpdate,
  // outbound (webview → host) — matched by identity in sendNotification/Request
  AiChatAbort,
  AiChatPickAttachment,
  AiChatStart,
  AiConversationDelete,
  AiConversationList,
  AiConversationLoad,
  AiMentionSearch,
  AiUsageSubscribe,
  AiUsageUnsubscribe,
  type AiChatAttachment,
  type AiChatContentPart,
  type AiChatMessage,
  type AiChatStartParams,
  type AiConversation,
} from 'jl4-client-rpc'
import { authHeaders } from '$lib/auth/session.svelte'
import {
  deleteConversation,
  deriveTitle,
  listSummaries,
  loadConversation,
  renameConversation,
  saveConversation,
} from './history-store'

type Handler = (params: unknown) => void
type TypeRef = { method: string }

export interface BridgeConfig {
  /** `https://ai.legalese.cloud/{orgSlug}/{deploymentId}` */
  apiBaseUrl: string
  deploymentId: string
  orgSlug: string
}

const USAGE_POLL_MS = 30_000

function nowIso(): string {
  return new Date().toISOString()
}

function newId(): string {
  return crypto.randomUUID()
}

/** Strip client-only `_meta` before sending a message on the wire. */
function toWire(m: AiChatMessage): AiChatMessage {
  const { _meta, ...rest } = m
  void _meta
  return rest
}

function buildUserContent(
  text: string,
  attachments: AiChatAttachment[]
): string | AiChatContentPart[] {
  if (!attachments.length) return text
  const parts: AiChatContentPart[] = [{ type: 'text', text }]
  for (const a of attachments) {
    const url = `data:${a.mediaType};base64,${a.dataBase64}`
    if (a.kind === 'image') {
      parts.push({ type: 'image_url', image_url: { url } })
    } else {
      parts.push({ type: 'file', file: { filename: a.name, file_data: url } })
    }
  }
  return parts
}

export class AiBridge {
  private handlers = new Map<string, Handler>()
  private aborts = new Map<string, AbortController>()
  private usageTimer: ReturnType<typeof setInterval> | null = null

  constructor(private cfg: BridgeConfig) {}

  // ── Messenger surface ────────────────────────────────────────────────

  onNotification(type: TypeRef, handler: Handler): this {
    this.handlers.set(type.method, handler)
    return this
  }

  sendNotification(type: TypeRef, _recipient: unknown, params: unknown): void {
    switch (type) {
      case AiChatStart:
        void this.runTurn(params as AiChatStartParams)
        break
      case AiChatAbort: {
        const { turnId } = params as { turnId: string }
        this.aborts.get(turnId)?.abort()
        this.aborts.delete(turnId)
        break
      }
      case AiUsageSubscribe:
        this.startUsage()
        break
      case AiUsageUnsubscribe:
        this.stopUsage()
        break
      default:
        // AiChatInject / AiChatAnswerUser / AiChatApproveTool / AiFileOpen /
        // AiFileOpenDiff / AiPermissionsSet / AiChatPreviewAttachment — not
        // applicable to a deployment chat in the browser. No-op.
        break
    }
  }

  async sendRequest(
    type: TypeRef,
    _recipient: unknown,
    params: unknown
  ): Promise<unknown> {
    switch (type) {
      case AiConversationList:
        return { items: listSummaries(this.cfg.deploymentId) }
      case AiConversationLoad:
        return { conversation: loadConversation((params as { id: string }).id) }
      case AiConversationDelete:
        deleteConversation((params as { id: string }).id)
        return {}
      case AiChatPickAttachment:
        return pickAttachmentFile()
      case AiMentionSearch:
        // No @-mentions in the standalone web chat.
        return { items: [] }
      default:
        // Unknown request (e.g. tool render-meta lookups) — fail soft so the
        // copied components fall back to their default rendering.
        return null
    }
  }

  // Some callers invoke start(); nothing to do here.
  start(): this {
    return this
  }

  // ── Emit helpers ─────────────────────────────────────────────────────

  private emit(type: TypeRef, payload: unknown): void {
    this.handlers.get(type.method)?.(payload)
  }

  /** Called by the panel once handlers are wired: we are signed-in (the route
   *  guard already validated the session) and there is no editor active file. */
  signalReady(): void {
    this.emit(AiAuthStatus, { signedIn: true })
    this.emit(AiActiveFile, {
      uri: null,
      name: null,
      path: null,
      inWorkspace: false,
    })
  }

  dispose(): void {
    this.stopUsage()
    for (const ac of this.aborts.values()) ac.abort()
    this.aborts.clear()
  }

  // ── Turn execution ───────────────────────────────────────────────────

  private async runTurn(p: AiChatStartParams): Promise<void> {
    const ac = new AbortController()
    this.aborts.set(p.turnId, ac)

    // Resolve / create the conversation. On a brand-new chat we hold a local
    // id until the server's `metadata` frame assigns the canonical one.
    let convId = p.conversationId ?? newId()
    const isNew = !p.conversationId
    let conv: AiConversation =
      (p.conversationId && loadConversation(p.conversationId)) ||
      ({
        id: convId,
        orgId: this.cfg.orgSlug,
        userId: '',
        model: '',
        title: '',
        createdAt: nowIso(),
        lastActiveAt: nowIso(),
        messages: [],
        deploymentId: this.cfg.deploymentId,
        apiBaseUrl: this.cfg.apiBaseUrl,
      } satisfies AiConversation)

    // Retry (continueTurn) runs another pass against existing history without
    // appending a new user message.
    if (!p.continueTurn) {
      conv.messages.push({
        role: 'user',
        content: buildUserContent(p.text, p.attachments ?? []),
      })
      if (!conv.title) conv.title = deriveTitle(conv.messages)
    }
    // Persist the user turn for follow-ups; new chats persist after we know the
    // server id (on metadata).
    if (!isNew) saveConversation(conv)

    const body = JSON.stringify({
      messages: conv.messages.map(toWire),
      tools: [],
      stream: true,
      turnId: p.turnId,
      ...(p.conversationId ? { conversationId: p.conversationId } : {}),
      ...(p.continueTurn ? { continueTurn: true } : {}),
    })

    let res: Response
    try {
      res = await fetch(`${this.cfg.apiBaseUrl}/v1/chat/completions`, {
        method: 'POST',
        headers: {
          ...authHeaders(),
          'Content-Type': 'application/json',
          Accept: 'text/event-stream',
        },
        body,
        signal: ac.signal,
      })
    } catch (err) {
      this.aborts.delete(p.turnId)
      if (ac.signal.aborted) return
      this.emit(AiChatError, {
        conversationId: convId,
        message: err instanceof Error ? err.message : 'Network error',
        code: 'network',
      })
      return
    }

    if (!res.ok || !res.body) {
      const { message, code } = await readError(res)
      this.emit(AiChatError, { conversationId: convId, message, code })
      this.aborts.delete(p.turnId)
      return
    }

    // ── Stream ──
    let assistantText = ''
    const blocks: PersistBlock[] = []
    let usage: { promptTokens: number; completionTokens: number } | undefined
    let finishReason = 'stop'
    let started = false

    const ensureTextBlock = (): { kind: 'text'; text: string } => {
      const last = blocks[blocks.length - 1]
      if (last && last.kind === 'text') return last
      const b = { kind: 'text', text: '' } as const
      blocks.push(b)
      return b
    }

    try {
      for await (const frame of parseSse(res.body, ac.signal)) {
        const { event, data } = frame
        if (data === '[DONE]') break
        let json: Record<string, unknown>
        try {
          json = JSON.parse(data)
        } catch {
          continue
        }

        if (event === 'metadata') {
          const serverId = (json.conversationId as string) || convId
          if (isNew && serverId !== convId) {
            conv.id = serverId
            renameConversation(convId, serverId)
          }
          convId = serverId
          conv = loadConversation(serverId) ?? conv
          conv.id = serverId
          if (!isNew) {
            // already persisted
          } else {
            saveConversation(conv)
          }
          const model = (json.model as string) || conv.model || ''
          conv.model = model
          started = true
          this.emit(AiChatStarted, {
            conversationId: serverId,
            turnId: p.turnId,
            model,
          })
        } else if (event === 'error') {
          this.emit(AiChatError, {
            conversationId: convId,
            message: (json.message as string) || 'Upstream error',
            code: json.code as string | undefined,
          })
          this.aborts.delete(p.turnId)
          return
        } else if (event === 'thinking_delta') {
          this.emit(AiChatThinkingDelta, {
            conversationId: convId,
            text: (json.text as string) || '',
          })
        } else if (event === 'tool_activity') {
          const activities = (json.activities as ToolActivityWire[]) ?? []
          for (const a of activities) {
            this.emit(AiChatToolActivity, { conversationId: convId, ...a })
            blocks.push({
              kind: 'tool-activity',
              tool: a.tool,
              status: a.status,
              message: a.message ?? '',
              ...(a.label ? { label: a.label } : {}),
              ...(a.input !== undefined ? { input: a.input } : {}),
              ...(a.output !== undefined ? { output: a.output } : {}),
              ...(a.ruleId ? { ruleId: a.ruleId } : {}),
              ...(a.deploymentId ? { deploymentId: a.deploymentId } : {}),
              ...(a.error ? { error: a.error } : {}),
              ...(a.sources ? { sources: a.sources } : {}),
            })
          }
        } else {
          // OpenAI chat.completion.chunk
          const choice = (json.choices as ChunkChoice[] | undefined)?.[0]
          const delta = choice?.delta
          if (delta?.content) {
            assistantText += delta.content
            ensureTextBlock().text += delta.content
            this.emit(AiChatTextDelta, {
              conversationId: convId,
              text: delta.content,
            })
          }
          if (delta?.tool_calls) {
            for (const tc of delta.tool_calls) {
              this.emit(AiChatToolCall, {
                conversationId: convId,
                callId: tc.id ?? `call_${tc.index}`,
                name: tc.function?.name ?? '',
                argsJson: tc.function?.arguments ?? '',
                status: 'running',
              })
            }
          }
          if (choice?.finish_reason) finishReason = choice.finish_reason
          const u = json.usage as
            | { prompt_tokens?: number; completion_tokens?: number }
            | undefined
          if (u) {
            usage = {
              promptTokens: u.prompt_tokens ?? 0,
              completionTokens: u.completion_tokens ?? 0,
            }
          }
        }
      }
    } catch (err) {
      if (ac.signal.aborted) {
        this.aborts.delete(p.turnId)
        return
      }
      this.emit(AiChatError, {
        conversationId: convId,
        message: err instanceof Error ? err.message : 'Stream error',
        code: 'stream',
      })
      this.aborts.delete(p.turnId)
      return
    }

    // Persist the assistant turn + finalize.
    if (started || assistantText) {
      conv.messages.push({
        role: 'assistant',
        content: assistantText,
        _meta: { blocks },
      })
      conv.lastActiveAt = nowIso()
      saveConversation(conv)
    }
    this.emit(AiChatDone, {
      conversationId: convId,
      finishReason,
      ...(usage ? { usage } : {}),
    })
    this.aborts.delete(p.turnId)
  }

  // ── Usage polling ────────────────────────────────────────────────────

  private startUsage(): void {
    if (this.usageTimer) return
    void this.pollUsage()
    this.usageTimer = setInterval(() => void this.pollUsage(), USAGE_POLL_MS)
  }

  private stopUsage(): void {
    if (this.usageTimer) {
      clearInterval(this.usageTimer)
      this.usageTimer = null
    }
  }

  private async pollUsage(): Promise<void> {
    try {
      const res = await fetch(`${this.cfg.apiBaseUrl}/v1/usage`, {
        headers: authHeaders(),
      })
      if (!res.ok) return
      const u = (await res.json()) as {
        used: number
        limit: number
        blockOnOverage: boolean
      }
      this.emit(AiUsageUpdate, u)
    } catch {
      // transient — keep last known usage
    }
  }
}

// ── SSE parsing ──────────────────────────────────────────────────────────

interface SseFrame {
  event?: string
  data: string
}

async function* parseSse(
  body: ReadableStream<Uint8Array>,
  signal: AbortSignal
): AsyncGenerator<SseFrame> {
  const reader = body.getReader()
  const decoder = new TextDecoder()
  let buf = ''
  try {
    while (!signal.aborted) {
      const { done, value } = await reader.read()
      if (done) break
      buf += decoder.decode(value, { stream: true })
      let sep: number
      // Frames are separated by a blank line.
      while ((sep = buf.indexOf('\n\n')) !== -1) {
        const rawFrame = buf.slice(0, sep)
        buf = buf.slice(sep + 2)
        const frame = parseFrame(rawFrame)
        if (frame) yield frame
      }
    }
  } finally {
    reader.cancel().catch(() => {})
  }
}

function parseFrame(raw: string): SseFrame | null {
  let event: string | undefined
  const dataLines: string[] = []
  for (const line of raw.split('\n')) {
    if (!line || line.startsWith(':')) continue
    if (line.startsWith('event:')) event = line.slice(6).trim()
    else if (line.startsWith('data:')) dataLines.push(line.slice(5).trimStart())
  }
  if (!dataLines.length) return null
  return { event, data: dataLines.join('\n') }
}

// ── Attachment picker ─────────────────────────────────────────────────────

const ATTACH_ACCEPT =
  'image/png,image/jpeg,image/gif,image/webp,application/pdf'
const MAX_ATTACH_BYTES = 25 * 1024 * 1024 // 25 MB — provider-side ceiling

/** Open the browser file dialog and return an `AiChatAttachment` the store can
 *  stage. Mirrors the extension's `AiChatPickAttachment` response shape
 *  (`{ attachment }` on success, `{ note }` otherwise). Images and PDFs only —
 *  the two formats the providers read natively. */
async function pickAttachmentFile(): Promise<{
  attachment?: AiChatAttachment
  note?: string
}> {
  const file = await openFileDialog(ATTACH_ACCEPT)
  if (!file) return {}
  const isImage = file.type.startsWith('image/')
  const isPdf = file.type === 'application/pdf'
  if (!isImage && !isPdf) {
    return { note: 'Only images and PDFs can be attached.' }
  }
  if (file.size > MAX_ATTACH_BYTES) {
    return { note: 'File is too large (max 25 MB).' }
  }
  const dataBase64 = toBase64(await file.arrayBuffer())
  return {
    attachment: {
      kind: isImage ? 'image' : 'pdf',
      name: file.name,
      mediaType: file.type,
      dataBase64,
    },
  }
}

function openFileDialog(accept: string): Promise<File | null> {
  return new Promise((resolve) => {
    const input = document.createElement('input')
    input.type = 'file'
    input.accept = accept
    input.style.display = 'none'
    let settled = false
    const done = (f: File | null) => {
      if (settled) return
      settled = true
      input.remove()
      resolve(f)
    }
    input.addEventListener('change', () => done(input.files?.[0] ?? null))
    // If the dialog is dismissed, most browsers fire no event; resolve null
    // once focus returns and no file was chosen.
    window.addEventListener('focus', () => setTimeout(() => done(null), 300), {
      once: true,
    })
    document.body.appendChild(input)
    input.click()
  })
}

function toBase64(buf: ArrayBuffer): string {
  const bytes = new Uint8Array(buf)
  let binary = ''
  const chunk = 0x8000
  for (let i = 0; i < bytes.length; i += chunk) {
    binary += String.fromCharCode(...bytes.subarray(i, i + chunk))
  }
  return btoa(binary)
}

async function readError(
  res: Response
): Promise<{ message: string; code?: string }> {
  if (res.status === 401)
    return { message: 'Not signed in', code: 'unauthenticated' }
  if (res.status === 429)
    return {
      message: 'Daily usage limit reached',
      code: 'daily_token_limit_exceeded',
    }
  try {
    const j = (await res.json()) as {
      message?: string
      code?: string
      error?: string
    }
    return {
      message: j.message || j.error || `Request failed (${res.status})`,
      code: j.code,
    }
  } catch {
    return { message: `Request failed (${res.status})` }
  }
}

// ── Wire shapes (narrow, local) ───────────────────────────────────────────

interface ToolActivityWire {
  tool: string
  status: 'running' | 'done' | 'error'
  label?: string
  message?: string
  input?: unknown
  output?: unknown
  ruleId?: string
  deploymentId?: string
  error?: string
  sources?: Array<{ url: string; title?: string }>
}

interface ChunkChoice {
  delta?: {
    content?: string
    tool_calls?: Array<{
      index: number
      id?: string
      function?: { name?: string; arguments?: string }
    }>
  }
  finish_reason?: string
}

type PersistBlock =
  | { kind: 'text'; text: string }
  | ({ kind: 'tool-activity' } & ToolActivityWire & { message: string })
