<script lang="ts">
  import { onDestroy } from 'svelte'
  import type { Messenger } from 'vscode-messenger-webview'
  import {
    AiActiveFile,
    AiAuthStatus,
    AiChatAskUser,
    AiChatDone,
    AiChatError,
    AiChatSeedDraft,
    AiChatStarted,
    AiChatTextDelta,
    AiChatThinkingDelta,
    AiChatToolActivity,
    AiChatToolCall,
    AiUsageUpdate,
    type GetSidebarConnectionStatusResponse,
  } from 'jl4-client-rpc'
  import { createAiChatStore } from '$lib/stores/ai-chat.svelte'
  import MessageList from './message-list.svelte'
  import ChatInput from './chat-input.svelte'
  import EmptyState from './empty-state.svelte'
  import ConversationHistory from './conversation-history.svelte'
  import SettingsPanel from './settings-panel.svelte'

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
    <!-- Mirrors the Deployments tab empty-state (same element shape,
         class names and hint text styling) so the two tabs read the
         same when signed-out. Consistency beats cleverness here. -->
    <div class="empty-state">
      <p class="hint">
        Sign in with Legalese Cloud to start composing rules with AI.
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
        pendingQuestion={store.pendingQuestion}
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

  /* Mirror the Deployments tab's empty-state styling verbatim so the
     two tabs look identical when the user is signed out. Height is
     fixed at 40vh (matching the Deployments tab) so the hint line
     sits at ~20vh from the top instead of centered across the full
     panel height — that vertical anchor is what makes the two tabs
     feel interchangeable at a glance. Kept as an AI-tab-local copy
     because Svelte CSS is component-scoped; extracting to a global
     sheet would drag in every page's `.empty-state` at once. */
  .empty-state {
    display: flex;
    flex-direction: column;
    align-items: center;
    justify-content: center;
    height: 40vh;
    text-align: center;
    color: var(--vscode-descriptionForeground);
  }

  .empty-state .hint {
    font-size: 0.95em;
    line-height: 1.2;
    max-width: 200px;
  }
</style>
