<script lang="ts">
  import { onDestroy } from 'svelte'
  import type { Messenger } from 'vscode-messenger-webview'
  import { HOST_EXTENSION } from 'vscode-messenger-common'
  import {
    AiActiveFile,
    AiAuthStatus,
    AiChatDone,
    AiChatError,
    AiChatStarted,
    AiChatTextDelta,
    AiChatToolCall,
    AiUsageUpdate,
    RequestSidebarLogin,
    type GetSidebarConnectionStatusResponse,
  } from 'jl4-client-rpc'
  import { createAiChatStore } from '$lib/stores/ai-chat.svelte'
  import MessageList from './message-list.svelte'
  import ChatInput from './chat-input.svelte'
  import EmptyState from './empty-state.svelte'
  import ConversationHistory from './conversation-history.svelte'

  let {
    messenger,
    connectionStatus,
    visible,
  }: {
    messenger: InstanceType<typeof Messenger> | null
    connectionStatus: GetSidebarConnectionStatusResponse
    visible: boolean
  } = $props()

  const store = createAiChatStore(() => messenger)

  // Seed the signed-in flag from the existing sidebar connection status
  // so the unauth CTA isn't shown while the auth-status notification is
  // still in flight. The AiAuthStatus notification overrides this as
  // soon as it arrives.
  $effect(() => {
    store.onAuthStatus({ signedIn: connectionStatus.connected })
  })

  let historyOpen = $state(false)
  let subscribed = false

  function subscribeToMessenger(m: InstanceType<typeof Messenger>): void {
    // vscode-messenger-webview returns the messenger (fluent API) from
    // onNotification, not a Disposable. Subscriptions live for the
    // lifetime of the webview; a reload clears everything.
    m.onNotification(AiChatStarted, (p) => store.onStarted(p))
    m.onNotification(AiChatTextDelta, (p) => store.onTextDelta(p))
    m.onNotification(AiChatDone, (p) => store.onDone(p))
    m.onNotification(AiChatError, (p) => store.onError(p))
    m.onNotification(AiChatToolCall, (p) => store.onToolCall(p))
    m.onNotification(AiUsageUpdate, (p) => store.onUsageUpdate(p))
    m.onNotification(AiAuthStatus, (p) => store.onAuthStatus(p))
    m.onNotification(AiActiveFile, (p) => store.onActiveFile(p))
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

  function signIn(): void {
    messenger?.sendNotification(
      RequestSidebarLogin,
      HOST_EXTENSION,
      undefined as never
    )
  }

  function openHistory(): void {
    void store.refreshHistory()
    historyOpen = true
  }

  function onSeedSelect(seed: {
    prompt: string
    needsFile: 'text-or-pdf' | 'spreadsheet' | null
  }): void {
    // File picker lives in Phase 3 (attachments). For Phase 1 we drop
    // the seed text into the draft so the user sees something, and
    // they can paste content or just hit send for the no-file seed.
    store.setDraft(seed.prompt)
  }

  async function onLoadConversation(id: string): Promise<void> {
    await store.loadConversation(id)
    historyOpen = false
  }

  async function onDeleteConversation(id: string): Promise<void> {
    await store.deleteConversation(id)
  }

  function openSettings(): void {
    // Phase 2 content — for now this is a no-op button. See AI_CHAT_PLAN.
  }

  // Retry replays the last user message after clearing the errored
  // assistant bubble. Sign-in pops the existing login flow.
  function onRetry(): void {
    const conv = store.current
    if (!conv) return
    const lastUser = [...conv.turns].reverse().find((t) => t.role === 'user')
    if (!lastUser) return
    // Drop the errored assistant bubble so the retry cleanly appends.
    conv.turns = conv.turns.filter((t) => !t.error)
    store.send(lastUser.content)
  }

  const showUnauth = $derived(!store.signedIn)
  const showEmptyState = $derived(store.signedIn && !store.current)
  const showChat = $derived(store.signedIn && store.current !== null)
</script>

<div class="ai-panel">
  {#if showUnauth}
    <div class="signin-cta">
      <p class="cta-text">
        Sign in to Legalese Cloud to start composing rules with AI.
      </p>
    </div>
  {:else}
    {#if showEmptyState}
      <EmptyState onSeed={onSeedSelect} />
    {:else if showChat && store.current}
      <MessageList
        turns={store.current.turns}
        streaming={store.current.streaming}
        pendingApproval={store.pendingApproval}
        {onRetry}
        onSignIn={signIn}
        onApproveTool={(callId, decision) =>
          store.approveTool(callId, decision)}
        onOpenFile={(callId) => store.openFile(callId)}
        onOpenFileDiff={(callId) => store.openFileDiff(callId)}
      />
    {/if}

    {#if historyOpen}
      <ConversationHistory
        items={store.history}
        currentId={store.currentId}
        onLoad={onLoadConversation}
        onDelete={onDeleteConversation}
        onClose={() => (historyOpen = false)}
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

  .signin-cta {
    display: flex;
    flex-direction: column;
    align-items: center;
    justify-content: center;
    gap: 12px;
    flex: 1;
    text-align: center;
    padding: 24px 16px;
  }

  .cta-text {
    margin: 0;
    color: var(--vscode-descriptionForeground);
    font-size: 13px;
    max-width: 280px;
    line-height: 1.4;
  }
</style>
