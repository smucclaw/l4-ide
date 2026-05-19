import * as vscode from 'vscode'
import type { Messenger } from 'vscode-messenger'
import type { WebviewTypeMessageParticipant } from 'vscode-messenger-common'
import {
  AiActiveFile,
  AiAuthStatus,
  AiChatAbort,
  AiChatAnswerUser,
  AiChatApproveTool,
  AiChatAskUser,
  AiChatDone,
  AiChatError,
  AiChatInject,
  AiChatPickAttachment,
  AiChatPreviewAttachment,
  AiChatQueueConsumed,
  AiChatStart,
  AiChatStarted,
  AiChatTextDelta,
  AiChatThinkingDelta,
  AiChatToolActivity,
  AiChatToolCall,
  AiChatTurnSpawn,
  AiConversationDelete,
  AiConversationList,
  AiConversationLoad,
  AiConversationNew,
  AiFileOpen,
  AiFileOpenDiff,
  AiGetActiveFileSelection,
  AiMentionSearch,
  AiPermissionsGet,
  AiPermissionsSet,
  AiToolRenderMeta,
  AiUsageSubscribe,
  AiUsageUnsubscribe,
  AiUsageUpdate,
  GenerateSidebarIntendedUse,
  WebviewFrontendIsReadyNotification,
  type AiChatAttachment,
  type AiMentionCandidate,
  type AiPermissionCategory,
  type AiPermissionValue,
} from 'jl4-client-rpc'
import * as nodePath from 'path'
import * as os from 'os'
import { promises as fsPromises } from 'fs'
import type { AuthManager } from '../auth.js'
import { isLocalMode } from './ai-proxy-client.js'
import type { AiProxyClient } from './ai-proxy-client.js'
import type { ServiceClient } from '../service-client.js'
import type { ChatService, ChatServiceEvent } from './chat-service.js'
import type { ConversationStore } from './conversation-store.js'
import type { AiLogger } from './logger.js'
import { MCP_L4_RULES_PREFIX } from './mcp-client.js'
import type { McpToolClient } from './mcp-client.js'
import {
  categoryForTool,
  getPermission,
  setPermission,
  type PermissionCategory,
} from './permissions.js'
import { resolveFileUri } from './tools/fs.js'
import type { ToolDispatcher } from './tool-dispatcher.js'

/**
 * Wire every AI-chat RPC into the sidebar messenger. Called after
 * `initializeSidebarMessenger` — the two sets of handlers live on the
 * same Messenger instance; there's no collision since their methods
 * don't overlap.
 */
/** Minimal adapter the sidebar provider implements so this module
 *  can decide whether to post events to the webview now or queue
 *  them until the user flips back. Typed narrowly so tests can
 *  swap in a stub without pulling in a whole mock WebviewView. */
export interface WebviewVisibilityAdapter {
  isVisible(): boolean
  onDidChangeVisibility: vscode.Event<boolean>
}

export function registerAiChatHandlers(deps: {
  messenger: Messenger
  frontend: WebviewTypeMessageParticipant
  auth: AuthManager
  service: ChatService
  store: ConversationStore
  /** Stateless one-shot LLM calls (summize). Used here to draft the
   *  deployment "Intended use" text from the exported function schemas. */
  proxy: AiProxyClient
  logger: AiLogger
  /** Map of pending approval promises keyed by callId. Populated by
   * the tool dispatcher; drained by the webview's approve/deny message. */
  approvalPending: Map<string, (decision: 'allow' | 'deny') => void>
  /** Map of pending meta__ask_user promises keyed by callId. Resolves
   * with the user's answer (empty string = skipped). */
  askUserPending: Map<string, (answer: string) => void>
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
  /** Channel the dispatcher uses to push a meta__ask_user question to
   *  the webview. We fill in the `ask` function here. */
  askUserChannel: {
    ask: (callId: string, question: string, choices?: string[]) => void
  }
  /** Used to recover pre-edit snapshots for the applied-diff viewer. */
  dispatcher: ToolDispatcher
  /** Sidebar webview visibility. When the user switches activity-bar
   *  items, the chat stream keeps running in the extension host but
   *  `webview.postMessage` calls land on a hidden view that may or
   *  may not replay them to the DOM. We instead buffer chat events
   *  here and flush on the next visible-transition so the UI catches
   *  up to the live conversation state. */
  visibility: WebviewVisibilityAdapter
  /** MCP client — used by the chat-side render-meta lookup to resolve
   *  a sanitized tool name back to its `(deployId, fnName)` pair. */
  mcp: McpToolClient
  /** REST client for jl4-service — used to fetch the function-schema
   *  endpoint (`/deployments/{id}/functions/{fn}`) for chat tool-call
   *  rendering. */
  serviceClient: ServiceClient
}): vscode.Disposable {
  const {
    messenger,
    frontend,
    auth,
    service,
    store,
    proxy,
    logger,
    approvalPending,
    askUserPending,
    toolStatusChannel,
    askUserChannel,
    dispatcher,
    visibility,
    mcp,
    serviceClient,
  } = deps
  logger.info(
    'registerAiChatHandlers: installing handlers on sidebar messenger'
  )

  /** Latest argsJson keyed by tool-call id, captured when the
   *  dispatcher asks the UI for approval. Used by `file/openDiff` to
   *  render the proposed contents for fs__create_file /
   *  fs__edit_file, AND by `toolStatusChannel.emit` to route status
   *  updates back to the right conversation — without the stored
   *  conversationId here, follow-up status pushes ship with an empty
   *  id and the webview would otherwise fall back to the user's
   *  currently-focused conversation (which breaks when several chats
   *  stream concurrently). */
  const callArgs = new Map<
    string,
    { name: string; argsJson: string; conversationId: string }
  >()

  /** Per-conversation set of "tool activity has fired this turn".
   *  Used to suppress `thinking-delta` events that arrive AFTER tool
   *  activity in non-local mode — production OpenRouter presets emit
   *  reasoning blocks between every tool round, which clutters the
   *  chat log without adding insight (the model has already chosen
   *  its next move by the time the user reads the reasoning). Local
   *  mode keeps every reasoning block since that's what dev mode is
   *  for: inspecting the model end-to-end. Reset on `started`,
   *  `done`, or `error` so the next turn starts clean. Tool-activity
   *  dedupe in the webview store remains intact because suppressed
   *  thinking blocks never reach it. */
  const seenToolActivity = new Set<string>()

  // ── Forward chat service events to the webview as typed notifications.
  //
  // `sendNow` does the actual postMessage. When the webview is
  // hidden (user switched activity-bar items, tab, etc.) we route
  // the event into `buffer.push` instead — vscode.Webview#postMessage
  // on a hidden view can silently drop in practice even with
  // retainContextWhenHidden, and a turn that streams dozens of
  // deltas while hidden otherwise strands them in limbo until the
  // next event manages to land. Buffering + flush-on-visible is the
  // fix that matches what the plan doc §12/Phase-1 calls for.
  const sendNow = (event: ChatServiceEvent): void => {
    switch (event.kind) {
      case 'started':
        seenToolActivity.delete(event.conversationId)
        messenger.sendNotification(AiChatStarted, frontend, {
          conversationId: event.conversationId,
          turnId: event.turnId,
          model: event.model,
        })
        break
      case 'text-delta':
        messenger.sendNotification(AiChatTextDelta, frontend, {
          conversationId: event.conversationId,
          text: event.text,
        })
        break
      case 'thinking-delta':
        // Reasoning / "extended thinking" deltas forwarded untouched
        // to the webview. The store accumulates consecutive frames
        // into one `thinking` block that renders as a
        // collapsed-by-default "Thinking…" toggle in
        // message-assistant.svelte.
        //
        // Suppress when we've already seen tool activity this turn
        // and we're talking to the prod proxy — keeps the chat log
        // focused on actions taken instead of running commentary
        // between tool rounds. Local mode (dev) bypasses the gate so
        // operators can inspect every reasoning chunk end-to-end.
        if (seenToolActivity.has(event.conversationId) && !isLocalMode()) {
          break
        }
        messenger.sendNotification(AiChatThinkingDelta, frontend, {
          conversationId: event.conversationId,
          text: event.text,
        })
        break
      case 'done':
        seenToolActivity.delete(event.conversationId)
        messenger.sendNotification(AiChatDone, frontend, {
          conversationId: event.conversationId,
          finishReason: event.finishReason,
          usage: event.usage,
        })
        break
      case 'error':
        seenToolActivity.delete(event.conversationId)
        messenger.sendNotification(AiChatError, frontend, {
          conversationId: event.conversationId,
          message: event.message,
          code: event.code,
        })
        break
      case 'tool-activity':
        logger.debug(
          `tool_activity ${event.status} ${event.tool}: ${event.message}`
        )
        seenToolActivity.add(event.conversationId)
        messenger.sendNotification(AiChatToolActivity, frontend, {
          conversationId: event.conversationId,
          tool: event.tool,
          status: event.status,
          message: event.message,
          input: event.input,
          output: event.output,
          ruleId: event.ruleId,
          deploymentId: event.deploymentId,
          error: event.error,
        })
        break
      case 'tool-call':
        seenToolActivity.add(event.conversationId)
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
      case 'turn-spawn':
        // A queued user message spawned a follow-up sub-turn under
        // the same conversation. Reset per-turn flags so the next
        // sub-turn's reasoning blocks aren't pre-suppressed by the
        // prior one's tool activity.
        seenToolActivity.delete(event.conversationId)
        messenger.sendNotification(AiChatTurnSpawn, frontend, {
          conversationId: event.conversationId,
          subTurnId: event.subTurnId,
        })
        break
      case 'queue-consumed':
        messenger.sendNotification(AiChatQueueConsumed, frontend, {
          conversationId: event.conversationId,
          injectionIds: event.injectionIds,
        })
        break
    }
  }
  const buffer = new ChatEventBuffer(logger)
  const emit = (event: ChatServiceEvent): void => {
    // Populate callArgs eagerly — BEFORE the visibility branch — so
    // the dispatcher's later notifyStatus events (which read this map
    // to resolve conversationId / name / argsJson for AiChatToolCall)
    // never see an empty map even when the SSE event itself is sitting
    // in the visibility buffer waiting to be flushed.
    if (event.kind === 'tool-call') {
      callArgs.set(event.callId, {
        name: event.name,
        argsJson: event.argsJson,
        conversationId: event.conversationId,
      })
    }
    if (!visibility.isVisible()) {
      buffer.push(event)
      return
    }
    sendNow(event)
  }
  service.setEmitter(emit)
  // Drain on the visible-transition. Subscribing unconditionally
  // (not only on `visible=true`) is cheap and avoids a window where
  // a rapid hidden→visible→hidden sequence leaves events stuck.
  const visibilitySub = visibility.onDidChangeVisibility((visible) => {
    if (!visible) return
    const pending = buffer.flush()
    if (pending.length === 0) return
    logger.info(
      `webview became visible — replaying ${pending.length} buffered chat event(s)`
    )
    for (const ev of pending) sendNow(ev)
  })

  // The dispatcher emits status updates through this channel so we
  // can forward them to the webview as AiChatToolCall notifications
  // with matching callId. One tool call may fire multiple updates
  // (running → done) — the webview merges by callId.
  askUserChannel.ask = (callId, question, choices) => {
    // Carry the conversationId so the webview can attach the question
    // card to the right conversation even if the user has flipped to a
    // different one in the history panel while the tool call was in
    // flight. callArgs is populated in the `emit` handler above when
    // the tool-call SSE frame lands, so by the time the dispatcher
    // reaches this ask path we always have the mapping.
    const meta = callArgs.get(callId)
    messenger.sendNotification(AiChatAskUser, frontend, {
      conversationId: meta?.conversationId ?? '',
      callId,
      question,
      choices,
    })
  }

  toolStatusChannel.emit = (callId, status, detail) => {
    // Route through the same buffered emitter the chat-service uses
    // so dispatcher-side status updates (running → done / error) are
    // visibility-aware. Going direct via messenger.sendNotification
    // here causes the failure update to drop silently when the
    // webview is hidden, leaving the row stuck on its initial
    // "running" state once the webview becomes visible again — the
    // pulsating-dot-never-flips-to-expandable bug.
    const meta = callArgs.get(callId)
    emit({
      kind: 'tool-call',
      conversationId: meta?.conversationId ?? '',
      callId,
      name: meta?.name ?? '',
      argsJson: meta?.argsJson ?? '{}',
      status,
      result: detail?.result,
      error: detail?.error,
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

  /** Resolve every pending ask-user with an empty answer (= skipped).
   *  Mirrors denyAllPendingApprovals for the meta__ask_user path so
   *  a stop/new-message doesn't leave the dispatcher hanging forever. */
  const skipAllPendingQuestions = (reason: string): void => {
    if (askUserPending.size === 0) return
    logger.info(
      `skipping ${askUserPending.size} pending meta__ask_user question(s) (${reason})`
    )
    for (const [callId, resolver] of askUserPending) {
      resolver('')
      askUserPending.delete(callId)
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
    skipAllPendingQuestions('new message')
    void service.start(params).catch((err) => {
      logger.error('chat/start failed', err)
    })
  })

  messenger.onNotification(AiChatAbort, ({ turnId }) => {
    logger.info(`chat/abort received (turn=${turnId})`)
    // Resolve any pending approval as 'deny' so the dispatcher can
    // unblock and the outer loop can observe the abort signal.
    denyAllPendingApprovals('abort')
    skipAllPendingQuestions('abort')
    service.abort(turnId)
  })

  // Mid-turn user message: append to the in-flight turn's queue.
  // The chat-service routes it as either follow-up role:user after a
  // tool round, or as the seed for a fresh sub-turn once the model
  // finishes naturally — see ChatService.start() for the routing
  // logic. The user bubble is already on screen (the webview
  // pushed it at send-time for instant feedback); this notification
  // just tells the extension where to plug the text into the model
  // request.
  messenger.onNotification(AiChatInject, (params) => {
    logger.info(
      `chat/inject received (turn=${params?.turnId ?? '?'}, conv=${params?.conversationId ?? '?'}, textLen=${params?.text?.length ?? 0})`
    )
    service.inject(params)
  })

  // Webview posts the user's answer to a meta__ask_user question.
  // Empty `answer` = skip (the dispatcher treats this as "use your
  // best guess" per the tool contract).
  messenger.onNotification(AiChatAnswerUser, ({ callId, answer }) => {
    const resolver = askUserPending.get(callId)
    askUserPending.delete(callId)
    if (!resolver) {
      logger.warn(`meta__ask_user: no pending question for ${callId}`)
      return
    }
    resolver(answer ?? '')
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

  // Draft the deployment "Intended use" text from the exported function
  // schemas. AI-gated like the chat tab: without a usable Legalese Cloud
  // session (or AI api key) we surface a sign-in nudge here rather than
  // round-tripping a failure the webview would have to translate.
  messenger.onRequest(GenerateSidebarIntendedUse, async ({ functions }) => {
    if (!auth.isAiUsable()) {
      void vscode.window.showInformationMessage(
        'Sign in with Legalese Cloud to use this feature'
      )
      return { notSignedIn: true as const }
    }
    try {
      const text = await proxy.describeIntendedUse(
        JSON.stringify(functions, null, 2)
      )
      if (!text) {
        return {
          error: 'Could not generate a description. Please try again.',
        }
      }
      return { text }
    } catch (err) {
      logger.warn(
        `describeIntendedUse failed: ${err instanceof Error ? err.message : String(err)}`
      )
      return { error: err instanceof Error ? err.message : String(err) }
    }
  })

  messenger.onNotification(AiConversationNew, () => {
    // No-op in extension today — the webview owns "new conversation"
    // state. Reserved for future server-side cleanup hooks.
  })

  const ALL_PERMISSION_CATEGORIES: PermissionCategory[] = [
    'fs.read',
    'fs.create',
    'fs.edit',
    'fs.delete',
    'l4.evaluate',
    'mcp.l4Rules',
    'meta.askUser',
  ]
  messenger.onRequest(AiPermissionsGet, async () => {
    const values: Record<AiPermissionCategory, AiPermissionValue> = {
      'fs.read': 'always',
      'fs.create': 'always',
      'fs.edit': 'always',
      'fs.delete': 'always',
      'l4.evaluate': 'always',
      'mcp.l4Rules': 'always',
      'meta.askUser': 'always',
    }
    for (const cat of ALL_PERMISSION_CATEGORIES) {
      values[cat as AiPermissionCategory] = getPermission(
        cat
      ) as AiPermissionValue
    }
    return { values }
  })
  messenger.onNotification(AiPermissionsSet, ({ category, value }) => {
    void setPermission(
      category as PermissionCategory,
      value as 'never' | 'ask' | 'always'
    ).catch((err) =>
      logger.warn(
        `permissions/set: ${category}=${value} failed: ${err instanceof Error ? err.message : String(err)}`
      )
    )
  })

  messenger.onRequest(AiChatPickAttachment, async ({ accept }) => {
    return handlePickAttachment(accept)
  })

  messenger.onNotification(
    AiChatPreviewAttachment,
    async ({ name, mediaType, dataBase64 }) => {
      try {
        await previewAttachment({ name, mediaType, dataBase64 }, logger)
      } catch (err) {
        logger.warn(
          `attachment/preview failed: ${err instanceof Error ? err.message : String(err)}`
        )
      }
    }
  )

  // Function-schema cache for chat tool-call rendering. The chat
  // expand panel asks for the schema of a rule's inputs/outputs so it
  // can render JSON args back into L4 syntax. We hit
  // `/deployments/{id}/functions/{fn}` once per deployment version
  // and cache the result; the deploymentVersion changes whenever the
  // user re-deploys the same id, so the cache invalidates naturally.
  type RenderMeta = {
    parameters: import('jl4-client-rpc').FunctionParameter
    returnSchema?: import('jl4-client-rpc').FunctionParameter
  }
  const renderMetaCache = new Map<
    string,
    { version: string; meta: RenderMeta }
  >()
  // In-flight dedup so N concurrent rows for the same rule collapse to
  // one upstream `getDeploymentStatus` + `getFunctionSchema` pair. All
  // callers await the same promise and resolve together.
  type RenderMetaResult =
    | {
        kind: 'meta'
        parameters: RenderMeta['parameters']
        returnSchema?: RenderMeta['returnSchema']
      }
    | { kind: 'unavailable' }
  const renderMetaInFlight = new Map<string, Promise<RenderMetaResult>>()
  // Fetch + cache one function's schema, keyed by (deployId, fnName)
  // and invalidated on redeploy via the deployment version SHA. Shared
  // by both the MCP-target path and the direct deployment path.
  async function resolveRenderMeta(
    deployId: string,
    fnName: string,
    logLabel: string
  ): Promise<RenderMetaResult> {
    const cacheKey = `${deployId}/${fnName}`
    const existing = renderMetaInFlight.get(cacheKey)
    if (existing) return existing
    const work = (async (): Promise<RenderMetaResult> => {
      try {
        const status = await serviceClient.getDeploymentStatus(deployId)
        const version =
          (status.metadata as { version?: string } | undefined)?.version ?? ''
        const cached = renderMetaCache.get(cacheKey)
        if (cached && cached.version === version) {
          return { kind: 'meta' as const, ...cached.meta }
        }
        const summary = (await serviceClient.getFunctionSchema(
          deployId,
          fnName
        )) as {
          parameters?: import('jl4-client-rpc').FunctionParameter
          returnSchema?: import('jl4-client-rpc').FunctionParameter
        }
        if (!summary.parameters) return { kind: 'unavailable' as const }
        const meta: RenderMeta = {
          parameters: summary.parameters,
          returnSchema: summary.returnSchema,
        }
        renderMetaCache.set(cacheKey, { version, meta })
        return { kind: 'meta' as const, ...meta }
      } catch (err) {
        logger.warn(
          `aiToolRenderMeta(${logLabel}) failed: ${err instanceof Error ? err.message : String(err)}`
        )
        return { kind: 'unavailable' as const }
      }
    })()
    renderMetaInFlight.set(cacheKey, work)
    try {
      return await work
    } finally {
      renderMetaInFlight.delete(cacheKey)
    }
  }

  messenger.onRequest(
    AiToolRenderMeta,
    async ({ toolName, deploymentId, fnName }) => {
      if (!toolName.startsWith(MCP_L4_RULES_PREFIX)) {
        return { kind: 'unavailable' as const }
      }
      // Server-side rule activities (deployment passthrough chats)
      // hand us the deployment + L4 function name directly. Resolve
      // from there — the IDE's MCP target map is keyed by sanitized
      // names and need not even cover the cloud deployment the chat
      // is bound to.
      if (deploymentId && fnName) {
        return resolveRenderMeta(deploymentId, fnName, toolName)
      }
      // Client-side rule calls: reverse the sanitized tool name to a
      // (deployId, fnName) via the MCP target map. Make sure
      // listTools() has populated it (cheap when warm, ≤3s).
      await mcp.listTools().catch(() => undefined)
      const target = mcp.getToolTarget(toolName)
      if (!target) return { kind: 'unavailable' as const }
      return resolveRenderMeta(target.deployId, target.fnName, toolName)
    }
  )

  messenger.onRequest(AiMentionSearch, async ({ query }) => {
    const items = await searchMentions(query)
    return { items }
  })

  // Usage polling. Subscribing starts a 30s timer that hits
  // /v1/usage and forwards the result to the webview. The server
  // caches the result for 10s inside TokenQuota, so a 30s cadence
  // is cheap and catches bursts quickly enough for the gauge UX.
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

  // Push auth status whenever something that affects AI credentials
  // changes. The "signed in" criterion is deliberately narrow: only a
  // verified Legalese Cloud session OR a configured `legaleseAi.apiKey`
  // counts. A self-hosted jl4-service connection does NOT imply Legalese
  // AI access, so we don't unlock the AI tab just because the deploy
  // panel happens to be connected to localhost.
  const pushAuthStatus = (): void => {
    messenger.sendNotification(AiAuthStatus, frontend, {
      signedIn: auth.isAiUsable(),
    })
  }
  // Kick off a connection-state verify so cloudUserId gets populated
  // (or cleared) before we push, but don't gate the push on `connected`.
  void auth.getConnectionState().then(() => pushAuthStatus())
  const authSub = auth.onDidChange(() => pushAuthStatus())
  const aiKeyConfigSub = vscode.workspace.onDidChangeConfiguration((e) => {
    if (e.affectsConfiguration('legaleseAi.apiKey')) pushAuthStatus()
  })

  // Identify the active editor file for the chat-input's "include
  // this file" chip: workspace-relative path, basename, workspace
  // membership. Pushed on tab / visible-editor changes.
  const activeFileIdentity = (
    editor: vscode.TextEditor | undefined
  ): {
    uri: string | null
    name: string | null
    path: string | null
    inWorkspace: boolean
  } => {
    if (!editor) {
      return { uri: null, name: null, path: null, inWorkspace: false }
    }
    const uri = editor.document.uri
    const folder = vscode.workspace.getWorkspaceFolder(uri)
    const inWorkspace = !!folder
    const relOrAbs = inWorkspace
      ? vscode.workspace.asRelativePath(uri, false)
      : uri.fsPath
    return {
      uri: uri.toString(),
      name: nodePath.basename(uri.fsPath),
      path: relOrAbs,
      inWorkspace,
    }
  }
  const pushActiveFile = (editor: vscode.TextEditor | undefined): void => {
    messenger.sendNotification(
      AiActiveFile,
      frontend,
      activeFileIdentity(editor)
    )
  }
  pushActiveFile(vscode.window.activeTextEditor)
  const activeFileSub = vscode.window.onDidChangeActiveTextEditor((e) =>
    pushActiveFile(e)
  )
  // Belt-and-suspenders: onDidChangeActiveTextEditor is meant to fire
  // with undefined when the last editor closes, but in practice the
  // chip sometimes lingers. Re-sync on any visible-editor change so
  // the chip clears as soon as the tab is gone.
  const visibleFileSub = vscode.window.onDidChangeVisibleTextEditors(() =>
    pushActiveFile(vscode.window.activeTextEditor)
  )
  // Note: we deliberately do NOT subscribe to onDidChangeTextEditorSelection.
  // It would fire on every cursor move (10–30 events/sec while typing) just
  // to keep the webview's cached `activeFile.selection` warm. Instead the
  // chat input fires a one-shot AiGetActiveFileSelection request at submit
  // time (handler below) — the webview gets the freshest range exactly when
  // it needs it, with zero polling between submits.

  messenger.onRequest(AiGetActiveFileSelection, async () => {
    const editor = vscode.window.activeTextEditor
    const base = activeFileIdentity(editor)
    if (!editor || editor.selection.isEmpty) return base
    return {
      ...base,
      selection: {
        startLine: editor.selection.start.line + 1,
        endLine: editor.selection.end.line + 1,
      },
    }
  })

  // Resend the initial snapshot (active file + auth status) on every
  // webview-ready signal. The first push above runs at extension
  // activation — long before the webview has called
  // `acquireVsCodeApi` / wired its `onNotification` hooks — so the
  // notification is queued against nothing and lost. Without this
  // replay, opening the AI tab cold shows no "attach active file"
  // chip until the user switches editor tabs. The sidebar's Deploy
  // tab already uses the same replay-on-ready pattern in
  // extension.mts; this brings the AI chat into line.
  const readySub = messenger.onNotification(
    WebviewFrontendIsReadyNotification,
    () => {
      pushActiveFile(vscode.window.activeTextEditor)
      void auth.getConnectionState().then(pushAuthStatus)
    }
  )

  return {
    dispose(): void {
      if (usageTimer) clearInterval(usageTimer)
      authSub.dispose()
      aiKeyConfigSub.dispose()
      activeFileSub.dispose()
      visibleFileSub.dispose()
      schemeReg.dispose()
      visibilitySub.dispose()
      buffer.drop()
      // vscode-messenger's onNotification returns the messenger
      // itself (fluent), not a Disposable. The registration lives
      // for the extension's lifetime; nothing to free here. Kept
      // referenced so the closure doesn't get GCed prematurely.
      void readySub
    },
  }
}

/**
 * Queue of chat-service events accumulated while the sidebar
 * webview is hidden. Coalesces:
 *
 *   - Consecutive `text-delta` / `thinking-delta` frames for the
 *     same conversation → one concatenated frame. Otherwise a turn
 *     that streams thousands of tokens while hidden fills the
 *     buffer with one entry per token.
 *   - `tool-call` updates keyed by callId → the latest status wins.
 *     We only care about the final state when replaying; the
 *     webview's store merges by callId anyway, so showing the
 *     full progression adds no information.
 *
 * `tool-activity`, `started`, `done`, and `error` are kept
 * verbatim — their ordering carries meaning (e.g. `done` after
 * the last text-delta is what flips the streaming spinner off).
 *
 * A hard cap on buffer length (`MAX_ENTRIES`) guards against an
 * unbounded chat running against a long-hidden webview.
 */
export class ChatEventBuffer {
  private static readonly MAX_ENTRIES = 5000
  private events: ChatServiceEvent[] = []

  constructor(private readonly logger: AiLogger) {}

  push(event: ChatServiceEvent): void {
    if (event.kind === 'text-delta') {
      const tail = this.events[this.events.length - 1]
      if (
        tail &&
        tail.kind === 'text-delta' &&
        tail.conversationId === event.conversationId
      ) {
        tail.text += event.text
        return
      }
    }
    if (event.kind === 'thinking-delta') {
      const tail = this.events[this.events.length - 1]
      if (
        tail &&
        tail.kind === 'thinking-delta' &&
        tail.conversationId === event.conversationId
      ) {
        tail.text += event.text
        return
      }
    }
    if (event.kind === 'tool-call') {
      const idx = this.events.findIndex(
        (e) => e.kind === 'tool-call' && e.callId === event.callId
      )
      if (idx >= 0) {
        this.events[idx] = event
        return
      }
    }
    this.events.push(event)
    if (this.events.length > ChatEventBuffer.MAX_ENTRIES) {
      // Drop the oldest to cap memory; in the extremely unlikely
      // case the user keeps the sidebar hidden through > MAX_ENTRIES
      // distinct events, the webview will see a truncated replay —
      // still better than either dropping everything or blowing
      // out the process.
      this.events.shift()
      this.logger.warn(
        `ChatEventBuffer exceeded ${ChatEventBuffer.MAX_ENTRIES}; dropping oldest event`
      )
    }
  }

  flush(): ChatServiceEvent[] {
    const out = this.events
    this.events = []
    return out
  }

  drop(): void {
    this.events = []
  }
}

/**
 * Best-effort DELETE /v1/conversations/{id} on the ai-proxy. Errors are
 * logged but never surfaced — the UI already treats local delete as the
 * source of truth for display.
 */
/** Handle an `AiChatPickAttachment` request: pop VSCode's native open
 *  dialog with a filter appropriate for the requested accept mode,
 *  read the chosen file, and return its bytes as a base64-encoded
 *  `AiChatAttachment`.
 *
 *  Size caps are tighter than the providers' hard network limits
 *  because the real bottleneck is the model's context window, not the
 *  upload size:
 *   - Images: Anthropic's 5 MB per-image hard cap (OpenAI allows more).
 *   - PDFs: hard cap 10 MB (providers accept up to 32 MB / 100 pages,
 *     but Anthropic's own token estimates are ~1500–3000 tokens per
 *     page — a 100-page PDF fills or blows through a 200k context).
 *     A soft warning fires past 2 MB to nudge the user toward
 *     splitting. */
const MAX_IMAGE_BYTES = 5 * 1024 * 1024
const MAX_PDF_BYTES = 10 * 1024 * 1024
const SOFT_PDF_WARN_BYTES = 2 * 1024 * 1024

const IMAGE_EXTENSIONS = ['png', 'jpg', 'jpeg', 'webp', 'gif']

// Office formats the providers can't read directly. We still let the
// user *pick* one (so they don't get stuck wondering why their file is
// greyed out in the dialog) but we reject it after-the-fact with a note
// nudging them to export to PDF first. The seed picker passes through
// here when the user clicks the spreadsheet seed.
const OFFICE_EXTENSIONS = ['xlsx', 'xls', 'docx', 'doc', 'pptx', 'ppt']

function mediaTypeForExtension(ext: string): string | null {
  switch (ext.toLowerCase()) {
    case 'png':
      return 'image/png'
    case 'jpg':
    case 'jpeg':
      return 'image/jpeg'
    case 'webp':
      return 'image/webp'
    case 'gif':
      return 'image/gif'
    case 'pdf':
      return 'application/pdf'
    // Text-inlineable formats. The `text-or-pdf` picker advertises
    // these as valid choices for the Get Started seed flows (policy
    // doc, BRS, contract, …). chat-service.assembleMessages inlines
    // any text-adjacent mediaType as a `<attached-file>`-wrapped
    // content part — providers' `file` blocks only accept PDFs, so
    // anything else has to ride in the text channel.
    case 'txt':
    case 'log':
      return 'text/plain'
    case 'md':
    case 'markdown':
      return 'text/markdown'
    case 'csv':
      return 'text/csv'
    case 'tsv':
      return 'text/tab-separated-values'
    case 'json':
      return 'application/json'
    case 'yaml':
    case 'yml':
      return 'application/x-yaml'
    case 'xml':
      return 'application/xml'
    case 'html':
    case 'htm':
      return 'text/html'
    // L4 source. Not an IANA type; the AI SDK's inline-text path in
    // chat-service classifies by `text/` prefix, so use `text/plain`
    // and let the filename attribute in the <attached-file> wrapper
    // tell the model it's L4.
    case 'l4':
      return 'text/plain'
    default:
      return null
  }
}

async function handlePickAttachment(
  accept: 'any' | 'text-or-pdf' | 'spreadsheet'
): Promise<{ attachment: AiChatAttachment | null; note?: string }> {
  // The "spreadsheet" seed lets the user *try* to pick an Office file —
  // the picker shows .xlsx/.docx/.pptx alongside PDF/text so it isn't
  // greyed out. We surface the "export to PDF first" nudge only after
  // they actually choose one of those formats; PDFs and text files go
  // through unchanged.
  const TEXT_LIKE_EXTENSIONS = [
    'txt',
    'log',
    'md',
    'markdown',
    'csv',
    'tsv',
    'json',
    'yaml',
    'yml',
    'xml',
    'html',
    'htm',
    'l4',
  ]
  const filters: Record<string, string[]> =
    accept === 'text-or-pdf'
      ? { 'Text or PDF': ['pdf', ...TEXT_LIKE_EXTENSIONS] }
      : accept === 'spreadsheet'
        ? {
            'Document or PDF': [
              'pdf',
              ...TEXT_LIKE_EXTENSIONS,
              ...OFFICE_EXTENSIONS,
            ],
          }
        : {
            // `any` is the paperclip button in chat-input. Offers the
            // full set of supported attachables — images + PDF + any
            // text-adjacent source (L4, YAML, CSV, markdown, …).
            // Previously this filter was just images + PDF, so users
            // couldn't attach their .l4 / .txt files through the
            // paperclip even though chat-service's inline-text path
            // already knew how to ship them.
            Attachable: [...IMAGE_EXTENSIONS, 'pdf', ...TEXT_LIKE_EXTENSIONS],
          }

  const uris = await vscode.window.showOpenDialog({
    canSelectFiles: true,
    canSelectFolders: false,
    canSelectMany: false,
    filters,
    openLabel: 'Attach',
  })
  if (!uris || uris.length === 0) return { attachment: null }
  const uri = uris[0]!
  const name = nodePath.basename(uri.fsPath)
  const ext = (name.split('.').pop() ?? '').toLowerCase()
  if (OFFICE_EXTENSIONS.includes(ext)) {
    return {
      attachment: null,
      note: 'Neither Legalese AI nor its providers can read Excel, Word or PowerPoint files directly. Export to PDF first, then attach the PDF.',
    }
  }
  const mediaType = mediaTypeForExtension(ext)
  if (!mediaType) {
    return {
      attachment: null,
      note: `Unsupported attachment type ".${ext}". Supported: images, PDF.`,
    }
  }
  let buf: Buffer
  try {
    buf = await fsPromises.readFile(uri.fsPath)
  } catch (err) {
    return {
      attachment: null,
      note: `Could not read file: ${err instanceof Error ? err.message : String(err)}`,
    }
  }
  // Route any non-image extension through the 'pdf' bucket so
  // chat-service assembleMessages ships it as a file content part
  // (text/plain, text/markdown, application/pdf all translate 1:1
  // to the provider's document block shape). Only bitmap images
  // still use image_url parts. The `pdf` kind name is a legacy
  // misnomer; intent is "file document".
  const isImage = IMAGE_EXTENSIONS.includes(ext)
  const kind: AiChatAttachment['kind'] = isImage ? 'image' : 'pdf'
  const cap = kind === 'pdf' ? MAX_PDF_BYTES : MAX_IMAGE_BYTES
  const sizeMb = (buf.byteLength / 1024 / 1024).toFixed(1)
  if (buf.byteLength > cap) {
    const capMb = cap / 1024 / 1024
    return {
      attachment: null,
      note:
        kind === 'pdf'
          ? `${name} is ${sizeMb} MB — PDFs are capped at ${capMb} MB here so they don't blow through the model's context window. Split or compress first.`
          : `${name} is ${sizeMb} MB — images are capped at ${capMb} MB (Anthropic's per-image limit). Resize or recompress first.`,
    }
  }
  // Soft warning: big PDF still fits the hard cap but will chew up a
  // meaningful slice of the context budget. Warn once; user can ignore.
  let softNote: string | undefined
  if (kind === 'pdf' && buf.byteLength > SOFT_PDF_WARN_BYTES) {
    softNote = `${name} is ${sizeMb} MB — at ~1.5–3k tokens per PDF page this can eat a large slice of the model's context window. Consider splitting the document if the assistant starts missing detail.`
  }
  return {
    attachment: {
      kind,
      name,
      mediaType,
      dataBase64: buf.toString('base64'),
    },
    ...(softNote ? { note: softNote } : {}),
  }
}

/** Drop the attachment bytes into a temp file so VSCode / the OS can
 *  open them. Images go straight through `vscode.open` (VSCode has a
 *  native image viewer). PDFs try `vscode.open` too — it succeeds when
 *  a PDF custom editor like `tomoki1207.pdf` is installed — and fall
 *  back to `env.openExternal` so the user at least gets Preview /
 *  Acrobat when it isn't. */
async function previewAttachment(
  att: { name: string; mediaType: string; dataBase64: string },
  logger: AiLogger
): Promise<void> {
  const dir = nodePath.join(os.tmpdir(), 'legalese-ai-previews')
  await fsPromises.mkdir(dir, { recursive: true })
  // Prefix with the epoch millis so repeated previews don't fight over
  // the same filename (and the OS viewer doesn't silently reuse a
  // cached open).
  const target = nodePath.join(dir, `${Date.now()}-${att.name}`)
  await fsPromises.writeFile(target, Buffer.from(att.dataBase64, 'base64'))
  const uri = vscode.Uri.file(target)
  const isPdf = att.mediaType === 'application/pdf'
  const hasPdfEditor = !!vscode.extensions.getExtension('tomoki1207.pdf')
  if (isPdf && !hasPdfEditor) {
    // No inline PDF editor — open externally. vscode.open on a raw PDF
    // URI without a registered custom editor shows the "binary file"
    // warning, which is worse UX than the OS default viewer.
    await vscode.env.openExternal(uri)
    logger.info(
      `attachment/preview: opened ${att.name} externally (install tomoki1207.pdf for in-editor PDFs)`
    )
    return
  }
  try {
    await vscode.commands.executeCommand('vscode.open', uri)
  } catch (err) {
    logger.warn(
      `attachment/preview: vscode.open failed, falling back to external: ${err instanceof Error ? err.message : String(err)}`
    )
    await vscode.env.openExternal(uri)
  }
}

async function deleteServerConversation(
  auth: AuthManager,
  id: string,
  logger: AiLogger
): Promise<void> {
  try {
    const { getAiEndpoint, isLocalMode } = await import('./ai-proxy-client.js')
    const headers = await auth.getAiAuthHeaders()
    if (!headers.Authorization && isLocalMode()) {
      headers.Authorization = 'Bearer dev-local'
    }
    const res = await fetch(`${getAiEndpoint()}/v1/conversations/${id}`, {
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
 * GET /v1/usage on the ai-proxy. Returns the calling org's
 * current-day token total, effective daily limit, and whether
 * blockOnOverage is set. Failures return null — the webview's
 * gauge gracefully sits at zero rather than disrupting the chat
 * UI when the poller hits a transient 5xx or a 401 during auth
 * refresh.
 */
async function fetchUsage(
  auth: AuthManager,
  logger: AiLogger
): Promise<{
  used: number
  limit: number
  blockOnOverage: boolean
} | null> {
  try {
    const headers = await auth.getAiAuthHeaders()
    const { getAiEndpoint, isLocalMode } = await import('./ai-proxy-client.js')
    if (!headers.Authorization && isLocalMode()) {
      headers.Authorization = 'Bearer dev-local'
    }
    if (!headers.Authorization) return null
    const res = await fetch(`${getAiEndpoint()}/v1/usage`, {
      method: 'GET',
      headers,
      signal: AbortSignal.timeout(8000),
    })
    if (!res.ok) {
      logger.debug(`fetchUsage: ${res.status} ${res.statusText}`)
      return null
    }
    const body = (await res.json()) as {
      used?: number
      limit?: number
      blockOnOverage?: boolean
    }
    if (typeof body.used !== 'number' || typeof body.limit !== 'number') {
      return null
    }
    return {
      used: body.used,
      limit: body.limit,
      blockOnOverage: !!body.blockOnOverage,
    }
  } catch (err) {
    logger.debug(
      `fetchUsage failed: ${err instanceof Error ? err.message : String(err)}`
    )
    return null
  }
}
