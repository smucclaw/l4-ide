import * as vscode from 'vscode'
import type { Messenger } from 'vscode-messenger'
import type { WebviewTypeMessageParticipant } from 'vscode-messenger-common'
import {
  AiActiveFile,
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
  AiFileOpen,
  AiFileOpenDiff,
  AiMentionSearch,
  AiUsageSubscribe,
  AiUsageUnsubscribe,
  AiUsageUpdate,
  type AiMentionCandidate,
} from 'jl4-client-rpc'
import * as nodePath from 'path'
import type { AuthManager } from '../auth.js'
import type { ChatService, ChatServiceEvent } from './chat-service.js'
import type { ConversationStore } from './conversation-store.js'
import type { AiLogger } from './logger.js'
import { categoryForTool, setPermission } from './permissions.js'
import { resolveFileUri } from './tools/fs.js'
import type { ToolDispatcher } from './tool-dispatcher.js'

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
      status: 'pending-approval' | 'running' | 'done' | 'error',
      detail?: { result?: string; error?: string }
    ) => void
  }
  /** Used to recover pre-edit snapshots for the applied-diff viewer. */
  dispatcher: ToolDispatcher
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
    dispatcher,
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

  /** Drain every outstanding approval resolver as `deny`. Called on
   *  stop/new-message: the user's implicit "no" to any tool request
   *  still on screen. Safe to call when the map is empty. */
  const denyAllPendingApprovals = (reason: string): void => {
    if (approvalPending.size === 0) return
    logger.info(
      `denying ${approvalPending.size} pending approval(s) (${reason})`
    )
    for (const [callId, resolver] of approvalPending) {
      resolver('deny')
      approvalPending.delete(callId)
    }
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
    // Any pending approval from a previous turn is implicitly rejected
    // when the user starts a new one.
    denyAllPendingApprovals('new message')
    void service.start(params).catch((err) => {
      logger.error('chat/start failed', err)
    })
  })

  messenger.onNotification(AiChatAbort, ({ turnId }) => {
    logger.info(`chat/abort received (turn=${turnId})`)
    // Resolve any pending approval as 'deny' so the dispatcher can
    // unblock and the outer loop can observe the abort signal.
    denyAllPendingApprovals('abort')
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

  // Plain open — for fs__read_file and fs__create_file. Shows the
  // current on-disk file in the editor. For a create that hasn't
  // landed yet the file won't exist; we silently no-op in that case.
  messenger.onNotification(AiFileOpen, async ({ callId }) => {
    const meta = callArgs.get(callId)
    if (!meta) return
    try {
      const args = JSON.parse(meta.argsJson) as { path?: string }
      if (!args.path) return
      const uri = resolveFileUri(args.path)
      if (!uri) return
      await vscode.commands.executeCommand('vscode.open', uri)
    } catch (err) {
      logger.warn(
        `file/open failed: ${err instanceof Error ? err.message : String(err)}`
      )
    }
  })

  // Applied-diff view — for fs__edit_file and (post-run) fs__create_file.
  // Uses the pre-run snapshot the dispatcher captured so the diff
  // reflects the actual delta the tool wrote to disk, not a
  // speculative preview.
  messenger.onNotification(AiFileOpenDiff, async ({ callId }) => {
    const snapshot = dispatcher.snapshotFor(callId)
    if (!snapshot) {
      // Fallback: no snapshot means the tool didn't run (yet). Just
      // open the target file if we can resolve it.
      const meta = callArgs.get(callId)
      if (!meta) return
      const uri = dispatcher.resolveFile({
        callId,
        name: meta.name,
        argsJson: meta.argsJson,
      })
      if (uri) await vscode.commands.executeCommand('vscode.open', uri)
      return
    }
    try {
      await openAppliedDiff(snapshot)
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
      // Nonce is the first path segment (see openAppliedDiff below).
      // An earlier version dropped it at the end of the filename and
      // then tried to recover it via split('/').pop() — which matches
      // the entire filename, so the lookup missed and the "before"
      // side came back empty, making the diff show the whole file as
      // added.
      const parts = uri.path.split('/').filter(Boolean)
      const nonce = parts[0]
      return (nonce && proposedByNonce.get(nonce)) ?? ''
    },
  }
  const schemeReg = vscode.workspace.registerTextDocumentContentProvider(
    'l4-ai-proposed',
    proposedProvider
  )

  async function openAppliedDiff(snapshot: {
    callId: string
    uri: vscode.Uri
    relativePath: string
    before: string
  }): Promise<void> {
    // "Before" side: the virtual doc holds the pre-edit snapshot so
    // VSCode's diff gutter paints red/green against the now-current
    // on-disk file.
    const nonce = `${snapshot.callId}-${++nonceCounter}`
    proposedByNonce.set(nonce, snapshot.before)
    const beforeUri = vscode.Uri.parse(
      `l4-ai-proposed:/${nonce}/${snapshot.relativePath}`
    )
    await vscode.commands.executeCommand(
      'vscode.diff',
      beforeUri,
      snapshot.uri,
      `Legalese AI — ${snapshot.relativePath}`
    )
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

  // Push the currently-active editor file so the chat-input can render
  // the "include this file" chip. Pushed on every change, plus once at
  // registration so the chip appears immediately when the AI tab opens.
  const pushActiveFile = (editor: vscode.TextEditor | undefined): void => {
    if (!editor) {
      messenger.sendNotification(AiActiveFile, frontend, {
        uri: null,
        name: null,
        path: null,
        inWorkspace: false,
      })
      return
    }
    const uri = editor.document.uri
    const folder = vscode.workspace.getWorkspaceFolder(uri)
    const inWorkspace = !!folder
    const relOrAbs = inWorkspace
      ? vscode.workspace.asRelativePath(uri, false)
      : uri.fsPath
    messenger.sendNotification(AiActiveFile, frontend, {
      uri: uri.toString(),
      name: nodePath.basename(uri.fsPath),
      path: relOrAbs,
      inWorkspace,
    })
  }
  pushActiveFile(vscode.window.activeTextEditor)
  const activeFileSub = vscode.window.onDidChangeActiveTextEditor((e) =>
    pushActiveFile(e)
  )

  return {
    dispose(): void {
      if (usageTimer) clearInterval(usageTimer)
      authSub.dispose()
      activeFileSub.dispose()
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
