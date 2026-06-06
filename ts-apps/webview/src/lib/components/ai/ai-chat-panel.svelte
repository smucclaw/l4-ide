<script lang="ts">
  import { onDestroy } from 'svelte'
  import type { Messenger } from 'vscode-messenger-webview'
  import { HOST_EXTENSION } from 'vscode-messenger-common'
  import {
    AiActiveFile,
    AiAuthStatus,
    AiChatAskUser,
    AiChatDone,
    AiChatError,
    AiChatQueueConsumed,
    AiChatSeedDraft,
    AiChatStarted,
    AiChatTextDelta,
    AiChatThinkingDelta,
    AiChatToolActivity,
    AiChatToolCall,
    AiChatTurnSpawn,
    AiUsageUpdate,
    RequestOpenUrl,
    RequestSidebarLogin,
  } from 'jl4-client-rpc'
  import { createAiChatStore } from '$lib/stores/ai-chat.svelte'
  import { aiPrefs } from '$lib/stores/ai-prefs.svelte'
  import MessageList from './message-list.svelte'
  import ChatInput from './chat-input.svelte'
  import EmptyState from './empty-state.svelte'
  import ConversationHistory from './conversation-history.svelte'
  import SettingsPanel from './settings-panel.svelte'
  import DeploymentBanner from './deployment-banner.svelte'
  import CloudUpsell from '../cloud-upsell.svelte'

  let {
    messenger,
    visible,
    deploymentChatRequest = null,
  }: {
    messenger: InstanceType<typeof Messenger> | null
    visible: boolean
    /** Set by the sidebar when the user clicks "Use in chat" on a
     *  deployment. The `nonce` makes repeat clicks (same deployment)
     *  re-trigger the effect. */
    deploymentChatRequest?: {
      deploymentId: string
      apiBaseUrl: string
      /** The deployment's "Intended use" metadata, surfaced in the
       *  empty-state of the fresh deployment chat. */
      intendedUse?: string
      nonce: number
    } | null
  } = $props()

  const store = createAiChatStore(() => messenger)
  aiPrefs.init(() => messenger)

  // Honour a "Use in chat" request from the Deployment tab. Tracking
  // the nonce (not deep-equality) lets the user re-enter the same
  // deployment after closing the banner.
  let lastDeploymentNonce = -1
  $effect(() => {
    const req = deploymentChatRequest
    if (!req || req.nonce === lastDeploymentNonce) return
    lastDeploymentNonce = req.nonce
    store.startDeploymentChat(req.deploymentId, req.apiBaseUrl, req.intendedUse)
  })

  // The jl4-service connection is independent of Legalese AI
  // credentials (a self-hosted service does NOT imply AI access).
  // The store defaults `signedIn` to false; the extension fires
  // AiAuthStatus on register so the correct value arrives before the
  // user has a chance to interact.

  let historyOpen = $state(false)
  let settingsOpen = $state(false)
  let subscribed = false

  function subscribeToMessenger(m: InstanceType<typeof Messenger>): void {
    // vscode-messenger-webview returns the messenger (fluent API) from
    // onNotification, not a Disposable. Subscriptions live for the
    // lifetime of the webview; a reload clears everything.
    m.onNotification(AiChatStarted, (p) => store.onStarted(p))
    m.onNotification(AiChatTextDelta, (p) => store.onTextDelta(p))
    m.onNotification(AiChatThinkingDelta, (p) => store.onThinkingDelta(p))
    m.onNotification(AiChatDone, (p) => store.onDone(p))
    m.onNotification(AiChatError, (p) => store.onError(p))
    m.onNotification(AiChatToolCall, (p) => store.onToolCall(p))
    m.onNotification(AiChatToolActivity, (p) => store.onToolActivity(p))
    m.onNotification(AiChatTurnSpawn, (p) => store.onTurnSpawn(p))
    m.onNotification(AiChatQueueConsumed, (p) => store.onQueueConsumed(p))
    m.onNotification(AiUsageUpdate, (p) => store.onUsageUpdate(p))
    m.onNotification(AiAuthStatus, (p) => store.onAuthStatus(p))
    m.onNotification(AiActiveFile, (p) => store.onActiveFile(p))
    m.onNotification(AiChatAskUser, (p) => store.onAskUser(p))
    m.onNotification(AiChatSeedDraft, (p) => store.seedDraft(p.text))
  }

  // Attach handlers as soon as the messenger prop is non-null. An
  // earlier version of this did it in `onMount`, but Svelte runs child
  // `onMount` BEFORE the parent's — at which point the sidebar's
  // +page.svelte hasn't constructed the messenger yet. The effect
  // below re-runs when the prop flips from null to a real Messenger
  // and binds the handlers exactly once.
  $effect(() => {
    if (!messenger || subscribed) return
    subscribeToMessenger(messenger)
    subscribed = true
    void store.refreshHistory()
  })

  onDestroy(() => {
    store.usageUnsubscribe()
  })

  // Subscribe to 30s usage polling only while the tab is visible and
  // we have an open conversation.
  $effect(() => {
    const hasConv = store.current !== null
    if (visible && hasConv && store.signedIn) {
      store.usageSubscribe()
      return () => store.usageUnsubscribe()
    }
  })

  function openHistory(): void {
    void store.refreshHistory()
    historyOpen = true
  }

  async function onSeedSelect(seed: { prompt: string }): Promise<void> {
    // Every seed is document-driven: pre-fill the prompt, then
    // immediately pop the file picker so the user's next action is
    // confirming their source file rather than clicking a second
    // button. The picker accepts text + PDF — the two formats the
    // providers can read natively. Use `seedDraft` (not `setDraft`)
    // so the chat-input's sync effect picks the change up; plain
    // `setDraft` is reserved for the textarea's oninput so keystrokes
    // don't re-trigger the sync and race with a stale getDraft read.
    store.seedDraft(seed.prompt)
    const res = await store.pickAttachment('text-or-pdf')
    // Switch the "attach active file" chip off once a seed document
    // lands. Otherwise the first submit ships BOTH the editor's
    // active file AND the seed document as context — nearly
    // duplicated input that doubles token spend for no extra signal.
    // The user can still toggle it back on if they genuinely want
    // both. No-op on cancel (no attachment, no change).
    if (res.ok) store.setIncludeActiveFile(false)
  }

  async function onLoadConversation(id: string): Promise<void> {
    await store.loadConversation(id)
    historyOpen = false
  }

  async function onDeleteConversation(id: string): Promise<void> {
    await store.deleteConversation(id)
  }

  function openSettings(): void {
    settingsOpen = true
  }

  function signIn(): void {
    messenger?.sendNotification(
      RequestSidebarLogin,
      HOST_EXTENSION,
      undefined as never
    )
  }

  // Retry asks the server to run another turn against the existing
  // conversation state — no duplicated user message. The user's
  // original prompt is already persisted on disk from the aborted
  // attempt (ai-proxy saves the conversation on create, before the
  // stream starts), so the model has everything it needs to produce
  // a fresh response. The store's continueTurn() handles both the
  // happy path (server-assigned conversationId exists → ask the proxy
  // to run another pass against stored history) and the
  // first-turn-error fallback (no id yet → re-send the last user
  // message as a fresh request). It also drops the trailing errored
  // assistant bubble itself, so this handler is just a passthrough.
  function onRetry(): void {
    store.continueTurn()
  }

  const showUnauth = $derived(!store.signedIn)
  const showEmptyState = $derived(store.signedIn && !store.current)
  const showChat = $derived(store.signedIn && store.current !== null)
</script>

<div class="ai-panel">
  {#if showUnauth}
    <!-- Mirrors the Deployments tab signed-out state — both render the
         shared Legalese Cloud promo so the two tabs read the same. -->
    <CloudUpsell context="ai" onSignIn={signIn} />
  {:else}
    {#if showEmptyState}
      <EmptyState
        onSeed={onSeedSelect}
        deployment={store.deploymentBinding
          ? {
              deploymentId: store.deploymentBinding.deploymentId,
              intendedUse: store.deploymentBinding.intendedUse ?? '',
            }
          : null}
      />
    {:else if showChat && store.current}
      <MessageList
        turns={store.current.turns}
        streaming={store.current.streaming}
        pipelineActive={store.pipelineActive}
        pendingApproval={store.pendingApproval}
        pendingQuestion={store.pendingQuestion}
        {messenger}
        {onRetry}
        onApproveTool={(callId, decision) =>
          store.approveTool(callId, decision)}
        onAnswerQuestion={(answer) => store.answerQuestion(answer)}
        onOpenFile={(callId) => store.openFile(callId)}
        onOpenFileDiff={(callId) => store.openFileDiff(callId)}
      />
    {/if}

    {#if historyOpen}
      <ConversationHistory
        items={store.history}
        currentId={store.currentId}
        streamingIds={store.streamingConversationIds}
        onLoad={onLoadConversation}
        onDelete={onDeleteConversation}
        onClose={() => (historyOpen = false)}
      />
    {/if}

    {#if settingsOpen}
      <SettingsPanel {store} onClose={() => (settingsOpen = false)} />
    {/if}

    {#if store.deploymentBinding}
      <DeploymentBanner
        deploymentId={store.deploymentBinding.deploymentId}
        onOpenInBrowser={() => {
          // apiBaseUrl is always https://ai.legalese.cloud/{org}/{dep};
          // the public chat UI is the same path on the chat. subdomain.
          const url = store.deploymentBinding!.apiBaseUrl.replace(
            'https://ai.legalese.cloud',
            'https://chat.legalese.cloud'
          )
          messenger?.sendNotification(RequestOpenUrl, HOST_EXTENSION, { url })
        }}
        onClose={() => store.newConversation()}
      />
    {/if}

    <ChatInput
      {store}
      onNewConversation={() => store.newConversation()}
      onOpenHistory={openHistory}
      onOpenSettings={openSettings}
      disabled={!store.signedIn}
    />
  {/if}
</div>

<style>
  .ai-panel {
    position: relative;
    display: flex;
    flex-direction: column;
    height: 100%;
    min-height: 0;
    box-sizing: border-box;
  }
</style>
