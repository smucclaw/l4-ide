import * as vscode from 'vscode'
import { randomUUID } from 'node:crypto'
import type { AuthManager } from './auth.js'
import type { AiProxyClient } from './ai/ai-proxy-client.js'
import type { AiLogger } from './ai/logger.js'
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
    const messages = await buildMessages(
      context.history,
      request.prompt,
      request.references
    )

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

    const abort = new AbortController()
    const cancelSub = token.onCancellationRequested(() => abort.abort())

    try {
      const events = deps.proxy.stream(
        {
          messages,
          stream: true,
          turnId: randomUUID(),
        },
        abort.signal
      )

      for await (const ev of events) {
        if (token.isCancellationRequested) break
        switch (ev.kind) {
          case 'text-delta':
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

          case 'error':
            deps.logger.warn(
              `chat-participant: proxy error ${ev.code ?? ''} ${ev.message}`
            )
            return {
              errorDetails: { message: ev.message },
            }

          case 'done':
            return {}

          // `metadata`, `thinking-delta`, `tool-call`: intentionally ignored
          // in v1. We don't advertise client tools, so the proxy won't emit
          // `tool-call`. Thinking deltas are noisy for the chat panel.
          default:
            break
        }
      }
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
