import * as vscode from 'vscode'
import { randomUUID } from 'node:crypto'
import type { AuthManager } from './auth.js'
import type { AiProxyClient, AiProxyTool } from './ai/ai-proxy-client.js'
import type { AiLogger } from './ai/logger.js'
import { BUILTIN_TOOLS } from './ai/tool-registry.js'
import {
  fsCreateFile,
  fsDeleteFile,
  fsEditFile,
  fsReadFile,
} from './ai/tools/fs.js'
import { l4Evaluate } from './ai/tools/l4-evaluate.js'
import { categoryForTool, getPermission } from './ai/permissions.js'
import type { AiChatMessage } from 'jl4-client-rpc'

/**
 * `@legalese` chat participant for Copilot Chat.
 *
 * Streams responses from the Legalese Cloud AI proxy (same backend the
 * sidebar webview uses). Stateless per turn — VS Code's chat panel owns
 * the visible history; we translate it back into the proxy's message
 * format on every request rather than maintaining a server-side
 * conversation id. This keeps the participant a thin shim over
 * `AiProxyClient` and avoids any cross-talk with the sidebar's
 * `ChatService` / persistence layer.
 *
 * Exposes a unified tool list to the model:
 *   - the same {@link BUILTIN_TOOLS} the sidebar ships (fs / l4 / meta);
 *   - every tool VS Code knows about via {@link vscode.lm.tools} — which
 *     by virtue of our mcp.json registration already includes the L4
 *     Rules MCP server, plus any other MCP servers / language-model
 *     tools the user has installed.
 *
 * When the model issues a `tool_calls` finish we dispatch each call:
 *   - built-in names → execute the same handler the sidebar uses (with
 *     the user's category permission honored — `never` blocks; for v1
 *     `ask` is treated as `always` because we don't have an inline
 *     approval surface in the chat panel);
 *   - everything else → `vscode.lm.invokeTool` (which routes to MCP
 *     servers and other extension-registered tools, with VS Code
 *     handling its own confirmation prompts).
 *
 * Capped at {@link MAX_TOOL_ROUNDS} iterations as an infinite-loop guard.
 *
 * Tool activity emitted by the proxy (rule evaluations, doc search,
 * compaction status) is surfaced inline: live status via `stream.progress`
 * while running, plus a persistent `> Label: message` line on done so the
 * final transcript still shows what happened.
 */
export function registerChatParticipant(deps: {
  auth: AuthManager
  proxy: AiProxyClient
  logger: AiLogger
  iconPath: vscode.Uri
}): vscode.Disposable {
  const handler: vscode.ChatRequestHandler = async (
    request,
    context,
    stream,
    token
  ) => {
    // Pre-flight auth check so we render a sign-in button instead of
    // bubbling a 401 from the proxy. `isAiUsable()` is true when a
    // verified Cloud session exists OR a `legaleseAi.apiKey` setting
    // is configured — matching the sidebar chat's gate.
    if (!deps.auth.isAiUsable()) {
      stream.markdown('Sign in to Legalese Cloud to use @legalese.\n\n')
      stream.button({
        command: 'l4.login',
        title: 'Sign in to Legalese Cloud',
      })
      return {}
    }

    const messages = await buildMessages(
      context.history,
      request.prompt,
      request.references
    )

    // Build the unified tool list once per user turn. `vscode.lm.tools`
    // is read once (a snapshot — new tools registered mid-turn won't be
    // visible to the model until the next user message).
    const { tools, lmByWireName } = collectTools()

    const abort = new AbortController()
    const cancelSub = token.onCancellationRequested(() => abort.abort())
    const turnId = randomUUID()

    try {
      for (let round = 0; round < MAX_TOOL_ROUNDS; round++) {
        if (token.isCancellationRequested) return {}

        const pendingCalls: ToolCall[] = []
        let assistantText = ''
        let finishReason = 'stop'

        const events = deps.proxy.stream(
          {
            messages,
            stream: true,
            turnId,
            ...(tools.length > 0 ? { tools } : {}),
          },
          abort.signal
        )

        for await (const ev of events) {
          if (token.isCancellationRequested) break
          switch (ev.kind) {
            case 'text-delta':
              assistantText += ev.text
              stream.markdown(ev.text)
              break

            case 'tool-activity': {
              const label = ev.label ?? ev.tool
              if (ev.status === 'running') {
                stream.progress(`${label}: ${ev.message}`)
              } else if (ev.status === 'done') {
                stream.markdown(`\n> **${label}** — ${ev.message}\n\n`)
              } else {
                stream.markdown(
                  `\n> **${label}** failed — ${ev.error ?? ev.message}\n\n`
                )
              }
              break
            }

            case 'tool-call':
              pendingCalls.push({
                callId: ev.callId,
                name: ev.name,
                argsJson: ev.argsJson,
              })
              stream.progress(
                `Calling ${prettyToolName(ev.name, lmByWireName)}…`
              )
              break

            case 'done':
              finishReason = ev.finishReason
              break

            case 'error':
              deps.logger.warn(
                `chat-participant: proxy error ${ev.code ?? ''} ${ev.message}`
              )
              return { errorDetails: { message: ev.message } }

            // `metadata`, `thinking-delta`: intentionally ignored.
            default:
              break
          }
        }

        if (finishReason !== 'tool_calls' || pendingCalls.length === 0) {
          return {}
        }

        // The assistant turn that paused on tool_calls has to go into
        // the next request as one message containing both the text
        // (may be empty) and the tool_calls array. The proxy expects
        // OpenAI shape.
        messages.push({
          role: 'assistant',
          content: assistantText || null,
          tool_calls: pendingCalls.map((c) => ({
            id: c.callId,
            type: 'function' as const,
            function: { name: c.name, arguments: c.argsJson },
          })),
        })

        for (const call of pendingCalls) {
          if (token.isCancellationRequested) return {}
          const pretty = prettyToolName(call.name, lmByWireName)
          const argsPreview = truncate(prettyArgs(call.argsJson), 240)
          stream.markdown(
            `\n> 🔧 **${pretty}**${argsPreview ? `\\\n> \`${argsPreview}\`` : ''}\n\n`
          )
          let result: string
          try {
            result = await dispatchToolCall(
              call,
              lmByWireName,
              request.toolInvocationToken,
              stream,
              token
            )
          } catch (err) {
            result = `Error: ${err instanceof Error ? err.message : String(err)}`
            stream.markdown(`> ${result}\n\n`)
          }
          messages.push({
            role: 'tool',
            tool_call_id: call.callId,
            content: result,
          })
        }
      }

      // Safety: ran out of rounds before the model returned a `stop`.
      deps.logger.warn(
        `chat-participant: hit MAX_TOOL_ROUNDS (${MAX_TOOL_ROUNDS}) without a stop`
      )
      stream.markdown(
        `\n\n_Stopped after ${MAX_TOOL_ROUNDS} tool-call rounds._\n`
      )
      return {}
    } catch (err) {
      if (abort.signal.aborted) return {}
      const message = err instanceof Error ? err.message : String(err)
      deps.logger.error('chat-participant: stream failed', err)
      return { errorDetails: { message } }
    } finally {
      cancelSub.dispose()
    }
  }

  const participant = vscode.chat.createChatParticipant('l4.legalese', handler)
  participant.iconPath = deps.iconPath
  return participant
}

interface ToolCall {
  callId: string
  name: string
  argsJson: string
}

/** Hard cap on assistant↔tool round-trips per user turn. Anthropic /
 *  OpenAI rarely chain more than 5 calls deep in practice; 10 is a
 *  generous ceiling that still terminates a runaway loop. */
const MAX_TOOL_ROUNDS = 10

/** Set of built-in tool names — used by `dispatchToolCall` to choose
 *  between the inline executor switch and `vscode.lm.invokeTool`. */
const BUILTIN_TOOL_NAMES: ReadonlySet<string> = new Set(
  BUILTIN_TOOLS.map((t) => t.function.name)
)

/**
 * Snapshot `vscode.lm.tools` and merge with the built-ins. Each lm tool's
 * name is sanitized to satisfy the OpenAI function-name regex
 * (`[a-zA-Z0-9_-]{1,64}`); the original is kept in `lmByWireName` so
 * `vscode.lm.invokeTool` can be called with the registered name when
 * the model picks one.
 */
function collectTools(): {
  tools: AiProxyTool[]
  lmByWireName: Map<string, vscode.LanguageModelToolInformation>
} {
  const lmByWireName = new Map<string, vscode.LanguageModelToolInformation>()
  const out: AiProxyTool[] = [...BUILTIN_TOOLS]
  for (const t of vscode.lm.tools) {
    const wireName = sanitizeToolName(t.name)
    // Guard against an lm tool colliding with a built-in name (unlikely
    // but possible if some other extension registers `fs__read_file`).
    // Built-ins win — they're the ones we have hand-tuned descriptions
    // and executors for.
    if (BUILTIN_TOOL_NAMES.has(wireName)) continue
    lmByWireName.set(wireName, t)
    out.push({
      type: 'function',
      function: {
        name: wireName,
        description: t.description || `Tool: ${t.name}`,
        // VS Code's `inputSchema` is JSON Schema; the ai-proxy passes
        // it through to the provider verbatim. Some tools ship no
        // schema — default to an empty object schema so OpenAI accepts
        // the declaration.
        parameters:
          (t.inputSchema as Record<string, unknown> | undefined) ??
          ({ type: 'object', properties: {} } as Record<string, unknown>),
      },
    })
  }
  return { tools: out, lmByWireName }
}

/** OpenAI's function names allow `[a-zA-Z0-9_-]{1,64}`; lm tool names
 *  may include slashes, dots, spaces (especially MCP-prefixed names).
 *  Replace anything outside the allowed set with `_` and truncate. */
function sanitizeToolName(name: string): string {
  return name.replace(/[^a-zA-Z0-9_-]/g, '_').slice(0, 64)
}

/**
 * Execute a single tool call: built-ins go through the same handlers
 * the sidebar uses (with category permissions honored — `never` blocks);
 * everything else is dispatched to `vscode.lm.invokeTool`.
 */
async function dispatchToolCall(
  call: ToolCall,
  lmByWireName: Map<string, vscode.LanguageModelToolInformation>,
  // Threaded from `request.toolInvocationToken` so VS Code can attach
  // the tool invocation to this chat session (used for the inline
  // confirmation UX on tools that opt into it).
  toolInvocationToken: vscode.ChatParticipantToolToken | undefined,
  stream: vscode.ChatResponseStream,
  token: vscode.CancellationToken
): Promise<string> {
  const args = parseArgs(call.argsJson)

  if (BUILTIN_TOOL_NAMES.has(call.name)) {
    return runBuiltin(call.name, args, stream)
  }

  const lmTool = lmByWireName.get(call.name)
  if (!lmTool) {
    return `Unknown tool: ${call.name}`
  }
  // `vscode.lm.invokeTool` types `input` as `object`. Tool args from
  // the model are JSON-decoded into `unknown` upstream; for non-object
  // arg payloads (rare) we wrap so the call still goes through.
  const input: object =
    args && typeof args === 'object' ? (args as object) : { value: args }
  const result = await vscode.lm.invokeTool(
    lmTool.name,
    { input, toolInvocationToken },
    token
  )
  return stringifyLmResult(result)
}

async function runBuiltin(
  name: string,
  args: unknown,
  stream: vscode.ChatResponseStream
): Promise<string> {
  // `meta__post_status_update` is a UI-only tool — the existing
  // sidebar dispatcher short-circuits it because the text streams
  // straight into the assistant message. Mirror that here.
  if (name === 'meta__post_status_update') {
    const text = (args as { text?: string }).text ?? ''
    if (text) stream.markdown(text + '\n')
    return 'ok'
  }
  // No inline approval surface in chat yet; punt rather than dispatch
  // a tool that can't surface a follow-up question.
  if (name === 'meta__ask_user') {
    return 'meta__ask_user is not yet supported in @legalese — ask the user directly in your reply, or use the sidebar chat.'
  }

  const category = categoryForTool(name)
  if (category) {
    const perm = getPermission(category)
    if (perm === 'never') {
      return `User has disallowed the "${category}" category. Update the setting in the Legalese AI settings to enable it.`
    }
    // `ask` is treated as `always` here — the chat panel doesn't have
    // an inline approval surface yet. The proxy-side L4 rule tools
    // come through `vscode.lm.invokeTool` (above) and reuse VS Code's
    // own confirmation flow.
  }

  switch (name) {
    case 'fs__read_file':
      return fsReadFile(args as Parameters<typeof fsReadFile>[0])
    case 'fs__create_file':
      return fsCreateFile(args as Parameters<typeof fsCreateFile>[0])
    case 'fs__edit_file':
      return fsEditFile(args as Parameters<typeof fsEditFile>[0])
    case 'fs__delete_file':
      return fsDeleteFile(args as Parameters<typeof fsDeleteFile>[0])
    case 'l4__evaluate':
      return l4Evaluate(args as Parameters<typeof l4Evaluate>[0])
    default:
      return `No executor wired for built-in: ${name}`
  }
}

function parseArgs(json: string): unknown {
  if (!json || !json.trim()) return {}
  try {
    return JSON.parse(json)
  } catch (err) {
    throw new Error(
      `Invalid tool arguments JSON: ${err instanceof Error ? err.message : String(err)}`
    )
  }
}

/** Collapse a `LanguageModelToolResult` to a single string for the
 *  proxy's `role: 'tool'` content. Text parts are concatenated;
 *  non-text parts (prompt-tsx, future shapes) are skipped — they don't
 *  round-trip through the OpenAI-shaped wire format. */
function stringifyLmResult(result: vscode.LanguageModelToolResult): string {
  const parts: string[] = []
  for (const p of result.content) {
    if (p instanceof vscode.LanguageModelTextPart) {
      parts.push(p.value)
    }
  }
  return parts.join('\n')
}

/** Strip prefixes / sanitization underscores so a tool name shows up in
 *  chat the way the user would recognize it. Prefers the lm tool's
 *  original (unsanitized) name when we have it. */
function prettyToolName(
  wireName: string,
  lmByWireName: Map<string, vscode.LanguageModelToolInformation>
): string {
  const lmTool = lmByWireName.get(wireName)
  if (lmTool) return lmTool.name
  return wireName
}

/** Render a tool's JSON arguments as a single line for inline display.
 *  Falls back to the raw string if parsing fails. */
function prettyArgs(argsJson: string): string {
  if (!argsJson?.trim()) return ''
  try {
    return JSON.stringify(JSON.parse(argsJson))
  } catch {
    return argsJson
  }
}

function truncate(s: string, n: number): string {
  return s.length > n ? `${s.slice(0, n - 1)}…` : s
}

/**
 * Translate VS Code's chat history + the current prompt + the current
 * turn's attachments into the OpenAI-shaped message list the ai-proxy
 * expects.
 *
 * `ChatResponseTurn.response` is an array of response parts; we only
 * carry forward the markdown text parts because that's all the proxy
 * can ingest as assistant context. Tool-call rendering, anchors, file
 * trees etc. don't round-trip — they were UI hints, not model state.
 *
 * Attached files / selections from `request.references` are inlined
 * into the current user message as fenced code blocks ahead of the
 * prompt text, the same shape Copilot's own participants use. We don't
 * re-attach references from history turns — Copilot's convention is
 * that each turn carries its own attachments fresh.
 */
async function buildMessages(
  history: ReadonlyArray<vscode.ChatRequestTurn | vscode.ChatResponseTurn>,
  currentPrompt: string,
  references: ReadonlyArray<vscode.ChatPromptReference>
): Promise<AiChatMessage[]> {
  const out: AiChatMessage[] = []
  for (const turn of history) {
    if (turn instanceof vscode.ChatRequestTurn) {
      if (turn.prompt.trim()) {
        out.push({ role: 'user', content: turn.prompt })
      }
    } else if (turn instanceof vscode.ChatResponseTurn) {
      const text = extractResponseText(turn)
      if (text.trim()) {
        out.push({ role: 'assistant', content: text })
      }
    }
  }
  const attachmentBlock = await renderAttachments(references)
  const userContent = attachmentBlock
    ? `${attachmentBlock}\n\n${currentPrompt}`
    : currentPrompt
  out.push({ role: 'user', content: userContent })
  return out
}

/**
 * Render attached files and selections as fenced code blocks. Returns
 * `''` if no attachments produced useful content (binary, unreadable,
 * unsupported types).
 */
async function renderAttachments(
  refs: ReadonlyArray<vscode.ChatPromptReference>
): Promise<string> {
  if (!refs.length) return ''
  const blocks: string[] = []
  for (const ref of refs) {
    const v = ref.value
    if (v instanceof vscode.Uri) {
      const block = await readFileBlock(v)
      if (block) blocks.push(block)
    } else if (v instanceof vscode.Location) {
      const block = await readFileBlock(v.uri, v.range)
      if (block) blocks.push(block)
    } else if (typeof v === 'string' && v.trim()) {
      blocks.push(`### ${ref.id}\n\n${v}`)
    }
    // Other reference shapes (binary data, custom types) — skip in v1.
  }
  return blocks.join('\n\n')
}

async function readFileBlock(
  uri: vscode.Uri,
  range?: vscode.Range
): Promise<string | undefined> {
  let doc: vscode.TextDocument
  try {
    doc = await vscode.workspace.openTextDocument(uri)
  } catch {
    return undefined // binary, missing, or otherwise unreadable
  }
  const body = range ? doc.getText(range) : doc.getText()
  if (!body) return undefined
  const path = vscode.workspace.asRelativePath(uri)
  const header = range
    ? `### ${path} (lines ${range.start.line + 1}-${range.end.line + 1})`
    : `### ${path}`
  return `${header}\n\n\`\`\`${doc.languageId}\n${body}\n\`\`\``
}

function extractResponseText(turn: vscode.ChatResponseTurn): string {
  let acc = ''
  for (const part of turn.response) {
    // ChatResponseMarkdownPart is the only part that carries assistant
    // prose. Other parts (anchors, buttons, file trees, progress) are
    // UI affordances that don't belong in the next turn's context.
    if (part instanceof vscode.ChatResponseMarkdownPart) {
      acc += part.value.value
    }
  }
  return acc
}
