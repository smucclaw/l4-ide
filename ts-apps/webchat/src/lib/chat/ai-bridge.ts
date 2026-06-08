// Browser transport bridge.
//
// The copied store + panel were written to talk to the VSCode extension host
// over `vscode-messenger`: they SEND `AiChatStart`/`AiChatAbort`/… and expect
// the host to push back `AiChatStarted`/`AiChatTextDelta`/… notifications. This
// bridge implements just enough of that messenger surface to run a
// DEPLOYMENT-SCOPED chat directly from the browser: on `AiChatStart` it streams
// `POST {apiBaseUrl}/v1/chat/completions` (SSE) and translates each frame into
// the same notification the host would have sent.
//
// The server owns conversation history: it persists every turn keyed by
// conversationId and resumes its own copy on follow-ups (the request body only
// carries the new user turn). The browser keeps just a lightweight {id, title,
// …} index in localStorage so the sidebar can list history offline; the full
// transcript — including reconstructed L4 rule cards — is fetched on demand via
// `GET {apiBaseUrl}/v1/conversations/{id}` when a conversation is reopened.
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
  AiToolRenderMeta,
  AiUsageSubscribe,
  AiUsageUnsubscribe,
  type AiChatAttachment,
  type AiChatContentPart,
  type AiChatMessage,
  type AiChatStartParams,
  type AiConversation,
  type FunctionParameter,
} from 'jl4-client-rpc'
import { authHeaders } from '$lib/auth/session.svelte'
import {
  deleteIndexEntry,
  deriveTitle,
  listSummaries,
  loadIndexEntry,
  renameIndexEntry,
  saveIndexEntry,
  updateIndexEntry,
} from './history-store'

type Handler = (params: unknown) => void
type TypeRef = { method: string }

export interface BridgeConfig {
  /** `https://ai.legalese.cloud/{orgSlug}/{deploymentId}` */
  apiBaseUrl: string
  /** Deployment API root for this org — `{API_BASE}/{orgSlug}`. The
   *  per-function schema endpoint (`/deployments/{id}/functions/{fn}`,
   *  used for L4-syntax rendering of rule tool-calls) lives here, NOT
   *  on the ai-proxy `apiBaseUrl`. */
  serviceBaseUrl: string
  deploymentId: string
  orgSlug: string
}

/** Response shape of `GET /deployments/{id}/functions/{fn}` — only the
 *  two schema fields we render with are typed. */
interface FunctionSchemaResponse {
  parameters?: FunctionParameter
  returnSchema?: FunctionParameter
}

const USAGE_POLL_MS = 30_000

function nowIso(): string {
  return new Date().toISOString()
}

/** A `<current-time>` block carrying the user's local time and timezone, so
 *  the model can reason about "today"/"now" on every turn. Mirrors the VSCode
 *  extension's `buildCurrentTimeBlock`. */
function buildCurrentTimeBlock(): string {
  const now = new Date()
  const tz = Intl.DateTimeFormat().resolvedOptions().timeZone || 'UTC'
  return [
    '<current-time>',
    `localTime: ${now.toLocaleString(undefined, { timeZone: tz, timeZoneName: 'short' })}`,
    `timezone: ${tz}`,
    '</current-time>',
  ].join('\n')
}

/** Prepend the current-time block to the last user message. Inlined as a text
 *  part (not a `role:"system"` message) because the proxy's `extractDelta`
 *  filters system messages out of follow-up-turn deltas, which would drop the
 *  timestamp. Pure: returns a new array, leaving the locally-stored copy clean
 *  of this wire-only metadata. Mirrors the extension's `withCurrentTimeOnLastUser`. */
function withCurrentTimeOnLastUser(
  messages: AiChatMessage[]
): AiChatMessage[] {
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
    return messages
  }
  const next = messages.slice()
  next[lastUserIdx] = { ...original, content: augmented }
  return next
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
  // Render-meta (function schema) cache, keyed by `${deploymentId}/${fnName}`.
  // Mirrors the extension's cache so the many tool-call rows in a long
  // conversation share a single round-trip per distinct rule. We store the
  // in-flight promise so concurrent rows for the same rule de-dupe too.
  private renderMeta = new Map<string, Promise<unknown>>()

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
        return {
          conversation: await this.loadConversationFromServer(
            (params as { id: string }).id
          ),
        }
      case AiConversationDelete:
        await this.deleteConversation((params as { id: string }).id)
        return {}
      case AiChatPickAttachment:
        return pickAttachmentFile()
      case AiMentionSearch:
        // No @-mentions in the standalone web chat.
        return { items: [] }
      case AiToolRenderMeta:
        return this.resolveRenderMeta(
          params as { toolName: string; deploymentId?: string; fnName?: string }
        )
      default:
        // Unknown request — fail soft so the copied components fall back to
        // their default rendering.
        return null
    }
  }

  /** Fetch a conversation's full transcript from the server and assemble
   *  it into the `AiConversation` shape the store expects. The server
   *  reconstructs L4 rule tool-calls into `_meta.blocks` (with the
   *  `ruleId`/`ruleKey` the store's `extractPersistedBlocks` requires);
   *  we stitch in the title from the local index and the deployment
   *  binding from cfg so reopening rebinds the banner + endpoint. Returns
   *  null on any failure so the store surfaces an empty/known-error state
   *  rather than a stale local copy. */
  private async loadConversationFromServer(
    id: string
  ): Promise<AiConversation | null> {
    try {
      const res = await fetch(
        `${this.cfg.apiBaseUrl}/v1/conversations/${encodeURIComponent(id)}`,
        { headers: authHeaders() }
      )
      if (!res.ok) return null
      const data = (await res.json()) as {
        id: string
        orgId?: string
        model?: string
        messages?: AiChatMessage[]
        createdAt?: string
        lastActiveAt?: string
      }
      const index = loadIndexEntry(id)
      return {
        id: data.id,
        orgId: data.orgId ?? this.cfg.orgSlug,
        userId: '',
        model: data.model ?? index?.model ?? '',
        title: index?.title ?? '',
        createdAt: data.createdAt ?? nowIso(),
        lastActiveAt: data.lastActiveAt ?? nowIso(),
        messages: data.messages ?? [],
        deploymentId: this.cfg.deploymentId,
        apiBaseUrl: this.cfg.apiBaseUrl,
      }
    } catch {
      return null
    }
  }

  /** "Delete" a conversation: a SOFT delete server-side and a hard drop
   *  of the local index row. The server's DELETE endpoint only renames
   *  the transcript file (`…json` → `…deleted-<ts>.json`,
   *  ConversationStore.softDeleteByRename) — nothing is unlinked, so the
   *  history stays recoverable. The server copy is the source of truth,
   *  so we always attempt the remote call; a network failure still
   *  clears the local index so the sidebar reflects the user's intent. */
  private async deleteConversation(id: string): Promise<void> {
    try {
      await fetch(
        `${this.cfg.apiBaseUrl}/v1/conversations/${encodeURIComponent(id)}`,
        { method: 'DELETE', headers: authHeaders() }
      )
    } catch {
      // ignore — local index is cleared regardless below
    }
    deleteIndexEntry(id)
  }

  /** Fetch a deployed rule's function schema (parameters + returnSchema,
   *  carrying `x-l4-type` annotations) so the tool-call row can render the
   *  call's arguments and result as L4 syntax instead of raw JSON — the
   *  same metadata the VSCode extension serves over `AiToolRenderMeta`.
   *
   *  The L4 function name comes through on the rule activity (`fnName`);
   *  we fall back to the sanitised tool-name slug when it's absent. On any
   *  failure we return `unavailable`, which makes the row keep its JSON
   *  fallback — so a missing / not-yet-compiled schema degrades
   *  gracefully. */
  private async resolveRenderMeta(params: {
    toolName: string
    deploymentId?: string
    fnName?: string
  }): Promise<
    | {
        kind: 'meta'
        parameters: FunctionParameter
        returnSchema?: FunctionParameter
      }
    | { kind: 'unavailable' }
  > {
    const deploymentId = params.deploymentId ?? this.cfg.deploymentId
    const fnName =
      params.fnName ??
      (params.toolName.startsWith('l4-rules__')
        ? params.toolName.slice('l4-rules__'.length)
        : params.toolName)
    if (!deploymentId || !fnName) return { kind: 'unavailable' }

    const cacheKey = `${deploymentId}/${fnName}`
    let work = this.renderMeta.get(cacheKey) as
      | Promise<
          | {
              kind: 'meta'
              parameters: FunctionParameter
              returnSchema?: FunctionParameter
            }
          | { kind: 'unavailable' }
        >
      | undefined
    if (!work) {
      work = (async () => {
        try {
          // The consolidated api.legalese.cloud/{slug} host uses a
          // path-slug scheme (jl4-auth-proxy parseApiHostPath): the
          // canonical jl4-service path /deployments/{id}/functions/{fn}
          // is exposed here as /{id}/fn/{fn} (no `/deployments` prefix,
          // `functions` shortened to `fn`). serviceBaseUrl already
          // carries the /{slug}.
          const url =
            `${this.cfg.serviceBaseUrl}/` +
            `${encodeURIComponent(deploymentId)}/fn/` +
            `${encodeURIComponent(fnName)}`
          const res = await fetch(url, { headers: authHeaders() })
          if (!res.ok) return { kind: 'unavailable' as const }
          const schema = (await res.json()) as FunctionSchemaResponse
          if (!schema.parameters) return { kind: 'unavailable' as const }
          return {
            kind: 'meta' as const,
            parameters: schema.parameters,
            ...(schema.returnSchema
              ? { returnSchema: schema.returnSchema }
              : {}),
          }
        } catch {
          // Network / parse hiccup — drop the cached rejection below so a
          // later row can retry.
          return { kind: 'unavailable' as const }
        }
      })()
      this.renderMeta.set(cacheKey, work)
    }
    const result = await work
    // Don't cache misses: a rule that wasn't compiled yet when the first
    // row asked may resolve on a later turn. Successful metadata stays
    // cached for the bridge's lifetime (schemas are stable per deploy).
    if (result.kind === 'unavailable') this.renderMeta.delete(cacheKey)
    return result
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

    // Working id: server-assigned for follow-ups, a local placeholder for
    // a brand-new chat until the `metadata` frame assigns the canonical id.
    let convId = p.conversationId ?? newId()
    const isNew = !p.conversationId

    // Title: derive from this turn's user text on a new chat; keep the
    // existing index title on follow-ups.
    const title = isNew
      ? deriveTitle([{ role: 'user', content: p.text }])
      : (loadIndexEntry(convId)?.title ?? '')

    // Outgoing messages. The server owns history: with a conversationId it
    // resumes its own copy and `extractDelta` keeps only the trailing user
    // turn, so we send just the new message (or nothing on continueTurn).
    const outgoing: AiChatMessage[] = p.continueTurn
      ? []
      : [
          {
            role: 'user',
            content: buildUserContent(p.text, p.attachments ?? []),
          },
        ]

    // Seed the local index row immediately so the sidebar shows a new chat
    // even before the server responds. Keyed on the placeholder id here;
    // re-keyed to the server id on the `metadata` frame below.
    if (isNew) {
      saveIndexEntry({
        id: convId,
        title,
        model: '',
        createdAt: nowIso(),
        lastActiveAt: nowIso(),
        messageCount: 0,
        deploymentId: this.cfg.deploymentId,
      })
    }

    const body = JSON.stringify({
      messages: withCurrentTimeOnLastUser(outgoing).map(toWire),
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
    // No client-side transcript persistence: the server stores every turn
    // and the webchat refetches it on reopen. We only relay SSE frames as
    // store notifications and maintain the lightweight index row.
    let model = ''
    let usage: { promptTokens: number; completionTokens: number } | undefined
    let finishReason = 'stop'

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
            renameIndexEntry(convId, serverId)
          }
          convId = serverId
          model = (json.model as string) || model
          updateIndexEntry(convId, { model, lastActiveAt: nowIso() })
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
          }
        } else {
          // OpenAI chat.completion.chunk
          const choice = (json.choices as ChunkChoice[] | undefined)?.[0]
          const delta = choice?.delta
          if (delta?.content) {
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

    // Finalize: bump the index row (user + assistant turns just landed
    // server-side). The transcript itself is fetched from the server on
    // reopen, so there is nothing else to persist locally.
    const prevCount = loadIndexEntry(convId)?.messageCount ?? 0
    updateIndexEntry(convId, {
      lastActiveAt: nowIso(),
      messageCount: prevCount + outgoing.length + 1,
      ...(model ? { model } : {}),
    })
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
