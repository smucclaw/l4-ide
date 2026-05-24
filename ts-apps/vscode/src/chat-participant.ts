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
    const messages = buildMessages(context.history, request.prompt)

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
 * Translate VS Code's chat history + the current prompt into the
 * OpenAI-shaped message list the ai-proxy expects.
 *
 * `ChatResponseTurn.response` is an array of response parts; we only
 * carry forward the markdown text parts because that's all the proxy
 * can ingest as assistant context. Tool-call rendering, anchors, file
 * trees etc. don't round-trip — they were UI hints, not model state.
 */
function buildMessages(
  history: ReadonlyArray<vscode.ChatRequestTurn | vscode.ChatResponseTurn>,
  currentPrompt: string
): AiChatMessage[] {
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
  out.push({ role: 'user', content: currentPrompt })
  return out
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
