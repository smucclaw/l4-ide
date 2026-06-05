<script lang="ts">
  import type { AiChatStore } from '$lib/stores/ai-chat.svelte'
  import UsageLine from './usage-line.svelte'

  let {
    store,
    disabled = false,
  }: {
    store: AiChatStore
    disabled?: boolean
  } = $props()

  let text = $state('')
  let attachBusy = $state(false)
  let attachNote = $state<string | null>(null)
  let textarea = $state<HTMLTextAreaElement | null>(null)

  const streaming = $derived(store.current?.streaming ?? false)
  const canSend = $derived(!disabled && text.trim().length > 0 && !streaming)

  // Re-sync the textarea from the store's per-conversation draft whenever the
  // conversation changes or a seed is injected (e.g. an example prompt).
  let lastConvId: string | null | undefined
  let lastSeed = -1
  $effect(() => {
    const conv = store.currentId
    const seed = store.draftSeedVersion
    if (conv !== lastConvId || seed !== lastSeed) {
      lastConvId = conv
      lastSeed = seed
      text = store.getDraft()
      autosize()
    }
  })

  function autosize(): void {
    if (!textarea) return
    textarea.style.height = 'auto'
    textarea.style.height = `${Math.min(textarea.scrollHeight, 200)}px`
  }

  function onInput(): void {
    store.setDraft(text)
    autosize()
  }

  function submit(): void {
    const t = text.trim()
    if (!t || streaming || disabled) return
    void store.send(t)
    text = ''
    store.setDraft('')
    autosize()
  }

  function onKeydown(e: KeyboardEvent): void {
    if (e.key === 'Enter' && !e.shiftKey && !e.isComposing) {
      e.preventDefault()
      submit()
    }
  }

  async function pickAttachment(): Promise<void> {
    if (attachBusy) return
    attachBusy = true
    attachNote = null
    try {
      const res = await store.pickAttachment('any')
      if (res.note) attachNote = res.note
    } finally {
      attachBusy = false
    }
  }
</script>

<div class="chat-input-box">
  {#if store.stagedAttachments.length > 0}
    <div class="attachment-strip">
      {#each store.stagedAttachments as att, i (att.name + i)}
        <span class="chip">
          <span class="chip-name">{att.name}</span>
          <button
            class="chip-x"
            title="Remove"
            onclick={() => store.removeAttachment(i)}>×</button
          >
        </span>
      {/each}
    </div>
  {/if}

  {#if attachNote}
    <div class="attachment-note">{attachNote}</div>
  {/if}

  <textarea
    bind:this={textarea}
    bind:value={text}
    class="chat-textarea"
    rows="1"
    placeholder="Ask about this deployment…"
    {disabled}
    oninput={onInput}
    onkeydown={onKeydown}
  ></textarea>

  <UsageLine used={store.usedToday} limit={store.dailyLimit} />

  <div class="action-bar">
    <!-- Lower-left intentionally empty (new / history live in the sidebar). -->
    <div class="left-actions"></div>

    <div class="right-actions">
      <button
        class="icon-btn"
        title="Attach a file"
        onclick={pickAttachment}
        disabled={disabled || attachBusy}
        aria-label="Attach a file">📎</button
      >

      {#if streaming}
        <button
          class="submit-btn stop"
          title="Stop"
          onclick={() => store.abort()}
          aria-label="Stop">■</button
        >
      {:else}
        <button
          class="submit-btn"
          title="Send"
          onclick={submit}
          disabled={!canSend}
          aria-label="Send">↑</button
        >
      {/if}
    </div>
  </div>
</div>

<style>
  .chat-input-box {
    border: 1px solid var(--vscode-input-border, var(--vscode-widget-border));
    border-radius: 12px;
    background: var(--vscode-input-background);
    padding: 0.5rem 0.6rem 0.4rem;
    margin: 0.5rem 0.75rem 0.75rem;
    display: flex;
    flex-direction: column;
    gap: 0.4rem;
  }

  .attachment-strip {
    display: flex;
    flex-wrap: wrap;
    gap: 0.3rem;
  }
  .chip {
    display: inline-flex;
    align-items: center;
    gap: 0.25rem;
    padding: 0.1rem 0.4rem;
    border-radius: 6px;
    background: var(--vscode-list-hoverBackground);
    font-size: 0.78rem;
  }
  .chip-x {
    border: none;
    background: transparent;
    cursor: pointer;
    color: var(--vscode-descriptionForeground);
    font-size: 0.95rem;
    line-height: 1;
  }
  .attachment-note {
    font-size: 0.75rem;
    color: var(--vscode-descriptionForeground);
  }

  .chat-textarea {
    width: 100%;
    border: none;
    outline: none;
    resize: none;
    background: transparent;
    color: var(--vscode-input-foreground);
    font: inherit;
    line-height: 1.4;
    max-height: 200px;
    overflow-y: auto;
    box-sizing: border-box;
  }
  .chat-textarea::placeholder {
    color: var(--vscode-descriptionForeground);
  }

  .action-bar {
    display: flex;
    align-items: center;
    justify-content: space-between;
  }
  .left-actions {
    min-height: 1px;
  }
  .right-actions {
    display: flex;
    align-items: center;
    gap: 0.35rem;
  }

  .icon-btn {
    border: none;
    background: transparent;
    cursor: pointer;
    font-size: 1rem;
    padding: 0.25rem;
    border-radius: 6px;
    opacity: 0.8;
  }
  .icon-btn:hover:not(:disabled) {
    background: var(--vscode-toolbar-hoverBackground);
    opacity: 1;
  }
  .icon-btn:disabled {
    opacity: 0.4;
    cursor: default;
  }

  .submit-btn {
    width: 28px;
    height: 28px;
    display: grid;
    place-items: center;
    background: var(--brand);
    color: #fff;
    border: none;
    border-radius: 6px;
    cursor: pointer;
    font-size: 1rem;
    transition: background-color 0.1s ease-out;
  }
  .submit-btn:hover:not(:disabled) {
    background: var(--brand-hover);
  }
  .submit-btn:disabled {
    opacity: 0.45;
    cursor: default;
  }
  .submit-btn.stop {
    background: var(--brand-dark);
    color: rgba(255, 255, 255, 0.7);
    font-size: 0.7rem;
  }
  .submit-btn.stop:hover {
    background: #7f1a3f;
    color: #fff;
  }
</style>
