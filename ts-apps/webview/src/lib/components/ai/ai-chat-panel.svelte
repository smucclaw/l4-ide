<script lang="ts">
  import { onMount, onDestroy } from 'svelte'
  import type { Messenger } from 'vscode-messenger-webview'
  import { HOST_EXTENSION } from 'vscode-messenger-common'
  import {
    AiAuthStatus,
    AiChatDone,
    AiChatError,
    AiChatStarted,
    AiChatTextDelta,
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

  function subscribeToMessenger(): void {
    if (!messenger) return
    // vscode-messenger-webview returns the messenger (fluent API) from
    // onNotification, not a Disposable. Subscriptions live for the
    // lifetime of the webview — which equals the lifetime of this
    // component inside the sidebar host — so we don't need to track
    // them explicitly. A webview reload clears everything.
    messenger.onNotification(AiChatStarted, (p) => store.onStarted(p))
    messenger.onNotification(AiChatTextDelta, (p) => store.onTextDelta(p))
    messenger.onNotification(AiChatDone, (p) => store.onDone(p))
    messenger.onNotification(AiChatError, (p) => store.onError(p))
    messenger.onNotification(AiUsageUpdate, (p) => store.onUsageUpdate(p))
    messenger.onNotification(AiAuthStatus, (p) => store.onAuthStatus(p))
  }

  onMount(() => {
    subscribeToMessenger()
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
      <button class="cta-button" onclick={signIn}>Sign in</button>
    </div>
  {:else}
    {#if showEmptyState}
      <EmptyState onSeed={onSeedSelect} />
    {:else if showChat && store.current}
      <MessageList turns={store.current.turns} {onRetry} onSignIn={signIn} />
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

  .cta-button {
    background: var(--vscode-button-background);
    color: var(--vscode-button-foreground);
    border: none;
    padding: 6px 14px;
    border-radius: 2px;
    cursor: pointer;
    font-size: 13px;
  }

  .cta-button:hover {
    background: var(--vscode-button-hoverBackground);
  }
</style>
