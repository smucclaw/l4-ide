import type * as vscode from 'vscode'
import type { Messenger } from 'vscode-messenger'
import type { WebviewTypeMessageParticipant } from 'vscode-messenger-common'
import {
  AiAuthStatus,
  AiChatAbort,
  AiChatDone,
  AiChatError,
  AiChatStart,
  AiChatStarted,
  AiChatTextDelta,
  AiConversationDelete,
  AiConversationList,
  AiConversationLoad,
  AiConversationNew,
  AiMentionSearch,
  AiUsageSubscribe,
  AiUsageUnsubscribe,
  AiUsageUpdate,
  type AiMentionCandidate,
} from 'jl4-client-rpc'
import type { AuthManager } from '../auth.js'
import type { ChatService, ChatServiceEvent } from './chat-service.js'
import type { ConversationStore } from './conversation-store.js'
import type { AiLogger } from './logger.js'

/**
 * Wire every AI-chat RPC into the sidebar messenger. Called after
 * `initializeSidebarMessenger` — the two sets of handlers live on the
 * same Messenger instance; there's no collision since their methods
 * don't overlap.
 */
export function registerAiChatHandlers(deps: {
  messenger: Messenger
  frontend: WebviewTypeMessageParticipant
  auth: AuthManager
  service: ChatService
  store: ConversationStore
  logger: AiLogger
}): vscode.Disposable {
  const { messenger, frontend, auth, service, store, logger } = deps

  // ── Forward chat service events to the webview as typed notifications.
  const emit = (event: ChatServiceEvent): void => {
    switch (event.kind) {
      case 'started':
        messenger.sendNotification(AiChatStarted, frontend, {
          conversationId: event.conversationId,
          model: event.model,
        })
        break
      case 'text-delta':
        messenger.sendNotification(AiChatTextDelta, frontend, {
          conversationId: event.conversationId,
          text: event.text,
        })
        break
      case 'done':
        messenger.sendNotification(AiChatDone, frontend, {
          conversationId: event.conversationId,
          finishReason: event.finishReason,
          usage: event.usage,
        })
        break
      case 'error':
        messenger.sendNotification(AiChatError, frontend, {
          conversationId: event.conversationId,
          message: event.message,
          code: event.code,
        })
        break
      case 'tool-activity':
        // Phase 2 wires the UI for this; for Phase 1 we just log.
        logger.debug(
          `tool_activity ${event.status} ${event.tool}: ${event.message}`
        )
        break
    }
  }
  service.setEmitter(emit)

  // ── Webview → extension handlers.
  messenger.onNotification(AiChatStart, async (params) => {
    logger.debug(
      `chat/start (conv=${params.conversationId ?? '<new>'}, text=${params.text.slice(0, 40)})`
    )
    try {
      await service.start(params)
    } catch (err) {
      logger.error('chat/start failed', err)
    }
  })

  messenger.onNotification(AiChatAbort, ({ conversationId }) => {
    service.abort(conversationId)
  })

  messenger.onRequest(AiConversationList, async () => {
    const items = await store.list()
    return { items }
  })

  messenger.onRequest(AiConversationLoad, async ({ id }) => {
    const conversation = await store.load(id)
    return { conversation }
  })

  messenger.onRequest(AiConversationDelete, async ({ id }) => {
    const localOk = await store.delete(id)
    // Server-side delete is best-effort; the UI doesn't block on it.
    void deleteServerConversation(auth, id, logger)
    return { ok: localOk }
  })

  messenger.onNotification(AiConversationNew, () => {
    // No-op in extension today — the webview owns "new conversation"
    // state. Reserved for future server-side cleanup hooks.
  })

  messenger.onRequest(AiMentionSearch, async ({ query }) => {
    const items = await searchMentions(query)
    return { items }
  })

  // Usage polling is Phase 1-polish; subscribing starts a 30s timer.
  let usageTimer: NodeJS.Timeout | undefined
  messenger.onNotification(AiUsageSubscribe, () => {
    if (usageTimer) return
    const tick = (): void =>
      void fetchUsage(auth, logger).then((u) => {
        if (u) {
          messenger.sendNotification(AiUsageUpdate, frontend, u)
        }
      })
    tick()
    usageTimer = setInterval(tick, 30_000)
  })
  messenger.onNotification(AiUsageUnsubscribe, () => {
    if (usageTimer) {
      clearInterval(usageTimer)
      usageTimer = undefined
    }
  })

  // Push auth status whenever the AuthManager fires; the webview uses
  // this to enable/disable the input. The "signed in" criterion is
  // deliberately broad: any connected auth context (Legalese Cloud
  // session or a configured sk_* API key) can talk to ai.legalese.cloud.
  const pushAuthStatus = (state: { connected: boolean }): void => {
    messenger.sendNotification(AiAuthStatus, frontend, {
      signedIn: state.connected,
    })
  }
  void auth.getConnectionState().then(pushAuthStatus)
  const authSub = auth.onDidChange((state) => pushAuthStatus(state))

  return {
    dispose(): void {
      if (usageTimer) clearInterval(usageTimer)
      authSub.dispose()
    },
  }
}

/**
 * Best-effort DELETE /v1/conversations/{id} on the ai-proxy. Errors are
 * logged but never surfaced — the UI already treats local delete as the
 * source of truth for display.
 */
async function deleteServerConversation(
  auth: AuthManager,
  id: string,
  logger: AiLogger
): Promise<void> {
  try {
    const { AI_ENDPOINT } = await import('./ai-proxy-client.js')
    const headers = await auth.getAuthHeaders()
    const res = await fetch(`${AI_ENDPOINT}/v1/conversations/${id}`, {
      method: 'DELETE',
      headers,
    })
    if (!res.ok && res.status !== 404) {
      logger.warn(`server delete returned ${res.status}`)
    }
  } catch (err) {
    logger.warn(
      `server delete failed: ${err instanceof Error ? err.message : String(err)}`
    )
  }
}

/**
 * Phase 1 mention search: workspace files + the active file's selection
 * marker. Symbol/export suggestions land with LSP integration later.
 */
async function searchMentions(query: string): Promise<AiMentionCandidate[]> {
  const vscodeModule = await import('vscode')
  const items: AiMentionCandidate[] = []
  const editor = vscodeModule.window.activeTextEditor
  if (editor && !editor.selection.isEmpty) {
    items.push({ kind: 'selection', label: '@selection', target: '' })
  }
  const pattern = query ? `**/*${query}*` : '**/*.l4'
  try {
    const files = await vscodeModule.workspace.findFiles(
      pattern,
      '**/node_modules/**',
      50
    )
    for (const uri of files) {
      items.push({
        kind: 'file',
        label: vscodeModule.workspace.asRelativePath(uri, false),
        target: uri.toString(),
      })
    }
  } catch {
    // ignore
  }
  return items
}

/**
 * Pull today's token usage. Placeholder: until the ai-proxy or Legalese
 * Cloud expose a usage endpoint we can read, this returns null so the
 * UI keeps the gauge at zero. A subsequent commit wires it up.
 */
async function fetchUsage(
  _auth: AuthManager,
  _logger: AiLogger
): Promise<{ used: number; limit: number; blockOnOverage: boolean } | null> {
  return null
}
