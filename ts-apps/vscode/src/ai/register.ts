import * as vscode from 'vscode'
import type { Messenger } from 'vscode-messenger'
import type { WebviewTypeMessageParticipant } from 'vscode-messenger-common'
import {
  AiAuthStatus,
  AiChatAbort,
  AiChatApproveTool,
  AiChatDone,
  AiChatError,
  AiChatStart,
  AiChatStarted,
  AiChatTextDelta,
  AiChatToolCall,
  AiConversationDelete,
  AiConversationList,
  AiConversationLoad,
  AiConversationNew,
  AiFileOpenDiff,
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
import { categoryForTool, setPermission } from './permissions.js'
import { previewProposedContent } from './tools/fs.js'

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
  /** Map of pending approval promises keyed by callId. Populated by
   * the tool dispatcher; drained by the webview's approve/deny message. */
  approvalPending: Map<string, (decision: 'allow' | 'deny') => void>
  /** Mutable channel the dispatcher uses to emit status updates for
   * each tool call. We fill in the `emit` function here so the
   * dispatcher can forward to the webview via the same messenger. */
  toolStatusChannel: {
    emit: (
      callId: string,
      status: 'running' | 'done' | 'error',
      detail?: { result?: string; error?: string }
    ) => void
  }
}): vscode.Disposable {
  const {
    messenger,
    frontend,
    auth,
    service,
    store,
    logger,
    approvalPending,
    toolStatusChannel,
  } = deps
  logger.info(
    'registerAiChatHandlers: installing handlers on sidebar messenger'
  )

  /** Latest argsJson keyed by tool-call id, captured when the dispatcher
   *  asks the UI for approval. Used by `file/openDiff` to render the
   *  proposed contents for fs__create_file / fs__edit_file. */
  const callArgs = new Map<string, { name: string; argsJson: string }>()

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
      case 'tool-call':
        callArgs.set(event.callId, {
          name: event.name,
          argsJson: event.argsJson,
        })
        messenger.sendNotification(AiChatToolCall, frontend, {
          conversationId: event.conversationId,
          callId: event.callId,
          name: event.name,
          argsJson: event.argsJson,
          status: event.status,
          result: event.result,
          errorMessage: event.error,
        })
        break
    }
  }
  service.setEmitter(emit)

  // The dispatcher emits status updates through this channel so we
  // can forward them to the webview as AiChatToolCall notifications
  // with matching callId. One tool call may fire multiple updates
  // (running → done) — the webview merges by callId.
  toolStatusChannel.emit = (callId, status, detail) => {
    const meta = callArgs.get(callId)
    messenger.sendNotification(AiChatToolCall, frontend, {
      conversationId: '',
      callId,
      name: meta?.name ?? '',
      argsJson: meta?.argsJson ?? '{}',
      status,
      result: detail?.result,
      errorMessage: detail?.error,
    })
  }

  // ── Webview → extension handlers.
  // NB: sync handler — vscode-messenger calls the handler and forgets
  // about the return value, so there's no benefit to `async` here, and
  // it rules out any chance that an uncaught rejection inside the body
  // silently swallows the registration.
  messenger.onNotification(AiChatStart, (params) => {
    try {
      logger.info(
        `chat/start received (turn=${params?.turnId ?? '?'}, conv=${params?.conversationId ?? '<new>'}, textLen=${params?.text?.length ?? 0}, mentions=${params?.mentions?.length ?? 0})`
      )
    } catch (logErr) {
      logger.error('chat/start log-line failed', logErr)
    }
    void service.start(params).catch((err) => {
      logger.error('chat/start failed', err)
    })
  })

  messenger.onNotification(AiChatAbort, ({ turnId }) => {
    logger.info(`chat/abort received (turn=${turnId})`)
    service.abort(turnId)
  })

  // Tool approval: resolve the pending promise the dispatcher is
  // waiting on. `alwaysAllow` also bumps the permission setting to
  // `always` so subsequent calls in the same category run unattended.
  messenger.onNotification(AiChatApproveTool, ({ callId, decision }) => {
    logger.info(`tool/approve received (call=${callId}, decision=${decision})`)
    const resolver = approvalPending.get(callId)
    approvalPending.delete(callId)
    if (!resolver) {
      logger.warn(`tool/approve: no pending approval for ${callId}`)
      return
    }
    if (decision === 'alwaysAllow') {
      const meta = callArgs.get(callId)
      if (meta) {
        const category = categoryForTool(meta.name)
        if (category) {
          void setPermission(category, 'always').catch((err) =>
            logger.warn(
              `tool/approve: failed to persist always-allow for ${category}: ${err instanceof Error ? err.message : String(err)}`
            )
          )
        }
      }
      resolver('allow')
    } else {
      resolver(decision)
    }
  })

  messenger.onNotification(AiFileOpenDiff, async ({ callId }) => {
    const meta = callArgs.get(callId)
    if (!meta) return
    try {
      const preview = await previewProposedContent(
        meta.name,
        JSON.parse(meta.argsJson)
      )
      if (!preview) return
      await openProposalDiff(preview)
    } catch (err) {
      logger.warn(
        `file/openDiff failed: ${err instanceof Error ? err.message : String(err)}`
      )
    }
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
      void fetchUsage().then((u) => {
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

  // ── Proposed-content virtual scheme for diff previews. ─────────────
  // When the user cmd+clicks a filename inside a file-tool row, we want
  // VSCode's diff editor to compare the real on-disk file against the
  // proposal the model staged. Since the proposal only lives in memory,
  // we expose it via a TextDocumentContentProvider under a dedicated
  // scheme (`l4-ai-proposed:`). The path encodes a short nonce that
  // keys into `proposedByNonce` below.
  const proposedByNonce = new Map<string, string>()
  let nonceCounter = 0
  const proposedProvider: vscode.TextDocumentContentProvider = {
    provideTextDocumentContent(uri) {
      const nonce = uri.path.split('/').pop()
      return (nonce && proposedByNonce.get(nonce)) ?? ''
    },
  }
  const schemeReg = vscode.workspace.registerTextDocumentContentProvider(
    'l4-ai-proposed',
    proposedProvider
  )

  async function openProposalDiff(preview: {
    relativePath: string
    current: string
    proposed: string
  }): Promise<void> {
    const nonce = `${++nonceCounter}-${Date.now().toString(36)}`
    proposedByNonce.set(nonce, preview.proposed)
    // File extension matters — the diff editor uses it to pick the
    // language mode for syntax highlighting.
    const ext = preview.relativePath.split('.').pop() ?? 'txt'
    const proposedUri = vscode.Uri.parse(
      `l4-ai-proposed:/${preview.relativePath}.${nonce}.${ext}`
    )
    const currentUri = await resolveCurrentUri(preview.relativePath)
    // If the current file doesn't exist yet (create flow), synthesize a
    // second virtual doc for the empty side.
    const left =
      currentUri ??
      (() => {
        const emptyNonce = `empty-${nonce}`
        proposedByNonce.set(emptyNonce, '')
        return vscode.Uri.parse(
          `l4-ai-proposed:/${preview.relativePath}.${emptyNonce}.${ext}`
        )
      })()
    await vscode.commands.executeCommand(
      'vscode.diff',
      left,
      proposedUri,
      `Legalese AI — ${preview.relativePath}`
    )
  }

  async function resolveCurrentUri(
    relative: string
  ): Promise<vscode.Uri | null> {
    const folders = vscode.workspace.workspaceFolders ?? []
    if (folders.length === 0) return null
    const candidate = vscode.Uri.joinPath(folders[0]!.uri, relative)
    try {
      await vscode.workspace.fs.stat(candidate)
      return candidate
    } catch {
      return null
    }
  }

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
      schemeReg.dispose()
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
async function fetchUsage(): Promise<{
  used: number
  limit: number
  blockOnOverage: boolean
} | null> {
  return null
}
