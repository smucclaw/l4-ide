<script lang="ts">
  import { tick } from 'svelte'
  import type { AiChatStore } from '$lib/stores/ai-chat.svelte'
  import UsageLine from './usage-line.svelte'

  let {
    store,
    onNewConversation,
    onOpenHistory,
    onOpenSettings,
    disabled,
  }: {
    store: AiChatStore
    onNewConversation: () => void
    onOpenHistory: () => void
    onOpenSettings: () => void
    disabled: boolean
  } = $props()

  let textarea = $state<HTMLTextAreaElement>()
  // Initialized to empty; the effect below syncs it from the store's
  // per-conversation draft on mount + whenever the active conversation
  // changes. Avoids Svelte's state-referenced-locally warning about
  // reading store state in an initializer.
  let text = $state<string>('')
  let isStreaming = $derived(store.current?.streaming ?? false)

  function autoresize(): void {
    if (!textarea) return
    textarea.style.height = 'auto'
    const maxH = 6 * 18 // 6 rows @ ~18px line-height
    textarea.style.height = `${Math.min(textarea.scrollHeight, maxH)}px`
  }

  function onInput(): void {
    store.setDraft(text)
    autoresize()
  }

  function onKeydown(e: KeyboardEvent): void {
    if (e.key === 'Enter' && !e.shiftKey && !e.metaKey && !e.ctrlKey) {
      e.preventDefault()
      submit()
    }
  }

  function submit(): void {
    const trimmed = text.trim()
    if (!trimmed || disabled) return
    store.send(trimmed)
    text = ''
    store.setDraft('')
    // Next tick so the textarea empties before we resize.
    void tick().then(() => autoresize())
  }

  function abort(): void {
    store.abort()
  }

  // Keep draft synced if the user switches conversations from history.
  $effect(() => {
    // Touching currentId makes this effect re-run on switch.
    void store.currentId
    const next = store.getDraft()
    if (next !== text) {
      text = next
      void tick().then(() => autoresize())
    }
  })
</script>

<div class="chat-input-box">
  <textarea
    class="chat-textarea"
    bind:this={textarea}
    bind:value={text}
    oninput={onInput}
    onkeydown={onKeydown}
    placeholder="Ask anything about your L4…"
    rows="1"
    {disabled}
  ></textarea>

  <UsageLine used={store.usedToday} limit={store.dailyLimit} />

  <div class="action-bar">
    <div class="left-actions">
      <button
        class="icon-btn"
        onclick={onNewConversation}
        title="New conversation"
        aria-label="New conversation">+</button
      >
      <button
        class="icon-btn"
        onclick={onOpenHistory}
        title="Conversation history"
        aria-label="Conversation history">⏱</button
      >
      <button
        class="icon-btn"
        onclick={onOpenSettings}
        title="AI chat settings"
        aria-label="AI chat settings">⚙</button
      >
    </div>
    <div class="right-actions">
      {#if isStreaming}
        <button
          class="submit-btn stop"
          onclick={abort}
          title="Stop"
          aria-label="Stop">■</button
        >
      {:else}
        <button
          class="submit-btn"
          onclick={submit}
          disabled={disabled || !text.trim()}
          title="Send (Enter)"
          aria-label="Send">↑</button
        >
      {/if}
    </div>
  </div>
</div>

<style>
  .chat-input-box {
    display: flex;
    flex-direction: column;
    border: 1px solid var(--vscode-widget-border, rgba(128, 128, 128, 0.35));
    border-radius: 8px;
    background: var(--vscode-input-background);
    margin: 8px;
  }
  .chat-textarea {
    width: 100%;
    box-sizing: border-box;
    resize: none;
    background: transparent;
    color: var(--vscode-input-foreground);
    border: none;
    outline: none;
    padding: 10px 12px 6px;
    font-size: 13px;
    line-height: 18px;
    font-family: var(--vscode-font-family, inherit);
    max-height: 108px;
  }
  .chat-textarea:disabled {
    opacity: 0.5;
    cursor: not-allowed;
  }
  .action-bar {
    display: flex;
    justify-content: space-between;
    padding: 4px 6px;
  }
  .left-actions,
  .right-actions {
    display: flex;
    gap: 2px;
  }
  .icon-btn {
    background: transparent;
    color: var(--vscode-descriptionForeground);
    border: none;
    padding: 2px 8px;
    font-size: 13px;
    cursor: pointer;
    border-radius: 3px;
  }
  .icon-btn:hover {
    color: var(--vscode-foreground);
    background: var(--vscode-list-hoverBackground);
  }
  .submit-btn {
    background: var(--vscode-button-background);
    color: var(--vscode-button-foreground);
    border: none;
    padding: 2px 10px;
    font-size: 13px;
    cursor: pointer;
    border-radius: 3px;
  }
  .submit-btn:hover {
    background: var(--vscode-button-hoverBackground);
  }
  .submit-btn:disabled {
    background: transparent;
    color: var(--vscode-descriptionForeground);
    cursor: not-allowed;
  }
  .submit-btn.stop {
    background: var(--vscode-errorForeground, #d7263d);
    color: white;
  }
</style>
