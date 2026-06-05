<script lang="ts">
  import { onMount, onDestroy } from 'svelte'
  import { page } from '$app/stores'
  import type { Messenger } from 'vscode-messenger-webview'
  import {
    AiActiveFile,
    AiAuthStatus,
    AiChatAskUser,
    AiChatDone,
    AiChatError,
    AiChatStarted,
    AiChatTextDelta,
    AiChatThinkingDelta,
    AiChatToolActivity,
    AiChatToolCall,
    AiUsageUpdate,
  } from 'jl4-client-rpc'
  import {
    auth,
    authHeaders,
    AI_API_URL,
    API_BASE,
  } from '$lib/auth/session.svelte'
  import { AiBridge } from '$lib/chat/ai-bridge'
  import { createAiChatStore } from '$lib/stores/ai-chat.svelte'
  import { aiPrefs } from '$lib/stores/ai-prefs.svelte'
  import Sidebar from '$lib/components/sidebar.svelte'
  import NotFound from '$lib/components/not-found.svelte'
  import ChatPanel from '$lib/components/ai/chat-panel.svelte'
  import ChatInput from '$lib/components/ai/chat-input.svelte'

  const org = $derived($page.params.org ?? '')
  const deployment = $derived($page.params.deployment ?? '')
  const apiBaseUrl = $derived(`${AI_API_URL}/${org}/${deployment}`)

  onMount(() => {
    void auth.init()
  })

  const orgMatches = $derived(
    auth.phase === 'authenticated' && auth.orgSlug === org
  )
  const hasChat = $derived(auth.hasPermission('ai:chat'))
  const ready = $derived(orgMatches && hasChat)

  // ── Chat wiring ──
  // Built once the auth guard passes. The bridge is cast to the Messenger
  // shape the copied store/components expect; it implements the subset they use.
  let bridge: AiBridge | null = null
  let store: ReturnType<typeof createAiChatStore> | null = $state(null)
  let messengerRef: InstanceType<typeof Messenger> | null = $state(null)
  let wired = false
  // The deployment's "Intended use" text (metadata.description), fetched from
  // the deployment API. Used to seed the empty-state and re-bind on new chat.
  let intendedUse: string | undefined

  /** GET {API_BASE}/{org}/{deployment} → { metadata: { description } }. */
  async function fetchIntendedUse(): Promise<string | undefined> {
    try {
      const res = await fetch(`${API_BASE}/${org}/${deployment}`, {
        headers: authHeaders(),
      })
      if (!res.ok) return undefined
      const data = await res.json()
      const d = data?.metadata?.description
      return typeof d === 'string' && d.trim() ? d.trim() : undefined
    } catch {
      return undefined
    }
  }

  $effect(() => {
    if (!ready || wired) return
    wired = true
    void init()
  })

  async function init(): Promise<void> {
    // Fetch the deployment's intended-use FIRST so the very first paint of the
    // empty-state shows the real text — no fallback flash, no re-bind race.
    intendedUse = await fetchIntendedUse()

    const b = new AiBridge({
      apiBaseUrl,
      deploymentId: deployment,
      orgSlug: org,
    })
    bridge = b
    const messenger = b as unknown as InstanceType<typeof Messenger>
    messengerRef = messenger

    const s = createAiChatStore(() => messenger)
    aiPrefs.init()

    // Same wiring the extension's ai-chat-panel does, minus the IDE-only events.
    b.onNotification(AiChatStarted, (p) => s.onStarted(p as never))
    b.onNotification(AiChatTextDelta, (p) => s.onTextDelta(p as never))
    b.onNotification(AiChatThinkingDelta, (p) => s.onThinkingDelta(p as never))
    b.onNotification(AiChatDone, (p) => s.onDone(p as never))
    b.onNotification(AiChatError, (p) => s.onError(p as never))
    b.onNotification(AiChatToolCall, (p) => s.onToolCall(p as never))
    b.onNotification(AiChatToolActivity, (p) => s.onToolActivity(p as never))
    b.onNotification(AiUsageUpdate, (p) => s.onUsageUpdate(p as never))
    b.onNotification(AiAuthStatus, (p) => s.onAuthStatus(p as never))
    b.onNotification(AiActiveFile, (p) => s.onActiveFile(p as never))
    b.onNotification(AiChatAskUser, (p) => s.onAskUser(p as never))

    store = s
    b.signalReady()
    void s.refreshHistory()
    // Bind the deployment with its intended-use up front; new turns route here.
    s.startDeploymentChat(deployment, apiBaseUrl, intendedUse)
    s.usageSubscribe()
  }

  onDestroy(() => {
    bridge?.dispose()
  })

  function newChat(): void {
    // webchat is always deployment-scoped, so a fresh chat must keep the
    // deployment binding (newConversation() clears it). Re-binding makes the
    // empty state show the deployment's intended-use box rather than the
    // generic "Get started" seeds.
    store?.startDeploymentChat(deployment, apiBaseUrl, intendedUse)
  }

  async function selectChat(id: string): Promise<void> {
    await store?.loadConversation(id)
  }

  async function deleteChat(id: string): Promise<void> {
    await store?.deleteConversation(id)
  }
</script>

{#if auth.phase === 'loading'}
  <div class="splash"><span class="spinner"></span></div>
{:else if auth.phase === 'unauthenticated'}
  <div class="splash">
    <div class="login-card">
      <div class="brand">Legalese AI</div>
      <p>Sign in to access this deployment chat.</p>
      <button class="login-btn" onclick={() => auth.login()}>Sign in</button>
    </div>
  </div>
{:else if !orgMatches}
  <NotFound detail="This deployment isn’t available on your account." />
{:else if !hasChat}
  <NotFound
    title="No chat access"
    detail="Your account doesn’t have AI chat enabled for this organization."
  />
{:else if store}
  <div class="app">
    <Sidebar
      onNewChat={newChat}
      onSelect={selectChat}
      onDelete={deleteChat}
      items={store.history}
      currentId={store.currentId}
      streamingIds={store.streamingConversationIds}
    />
    <main class="chat-pane">
      <ChatPanel {store} messenger={messengerRef} />
      <ChatInput {store} disabled={!store.signedIn} />
    </main>
  </div>
{:else}
  <div class="splash"><span class="spinner"></span></div>
{/if}

<style>
  .app {
    display: flex;
    height: 100vh;
    width: 100vw;
    overflow: hidden;
    background: var(--chat-bg);
  }
  .chat-pane {
    flex: 1 1 auto;
    min-width: 0;
    display: flex;
    flex-direction: column;
    background: var(--chat-bg);
  }
  .brand {
    font-weight: 600;
    color: var(--brand);
    font-size: 1.1rem;
  }

  .splash {
    display: grid;
    place-items: center;
    height: 100vh;
    background: var(--chat-bg);
  }
  .login-card {
    text-align: center;
    color: var(--vscode-foreground);
  }
  .login-card p {
    color: var(--vscode-descriptionForeground);
    margin: 0.5rem 0 1rem;
  }
  .login-btn {
    padding: 0.5rem 1.25rem;
    border: none;
    border-radius: 8px;
    background: var(--brand);
    color: #fff;
    font: inherit;
    font-weight: 500;
    cursor: pointer;
  }
  .login-btn:hover {
    background: var(--brand-hover);
  }
  .spinner {
    width: 28px;
    height: 28px;
    border: 3px solid var(--vscode-widget-border);
    border-top-color: var(--brand);
    border-radius: 50%;
    animation: spin 0.7s linear infinite;
  }
  @keyframes spin {
    to {
      transform: rotate(360deg);
    }
  }
</style>
