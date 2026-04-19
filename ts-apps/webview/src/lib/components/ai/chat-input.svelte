<script lang="ts">
  import { tick } from 'svelte'
  import type { AiChatStore } from '$lib/stores/ai-chat.svelte'
  import type { AiMentionCandidate } from 'jl4-client-rpc'
  import UsageLine from './usage-line.svelte'
  import MentionPopup from './mention-popup.svelte'

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

  // @-mention state. `from` is the index of the `@` in the textarea;
  // while the popup is open, `items` holds the last search result set.
  let mentionState = $state<null | {
    from: number
    query: string
    items: AiMentionCandidate[]
    selected: number
  }>(null)
  // Collected mention labels to attach to the next send. Cleared on submit.
  let stagedMentions = $state<
    Array<{ kind: AiMentionCandidate['kind']; label: string }>
  >([])

  function autoresize(): void {
    if (!textarea) return
    textarea.style.height = 'auto'
    const maxH = 6 * 18 // 6 rows @ ~18px line-height
    textarea.style.height = `${Math.min(textarea.scrollHeight, maxH)}px`
  }

  function onInput(): void {
    store.setDraft(text)
    autoresize()
    detectMention()
  }

  /**
   * Detect whether the cursor is inside an active `@`-mention fragment.
   * A mention trigger is an `@` that is either at the start of the
   * textarea or preceded by whitespace, followed by token chars
   * (word chars, `.`, `-`, `/`) up to the cursor. Any whitespace or
   * non-token char dismisses the popup.
   */
  function detectMention(): void {
    if (!textarea) {
      mentionState = null
      return
    }
    const cursor = textarea.selectionStart ?? text.length
    const before = text.slice(0, cursor)
    const atIdx = before.lastIndexOf('@')
    if (atIdx < 0) {
      mentionState = null
      return
    }
    const charBeforeAt = atIdx > 0 ? text[atIdx - 1] : ''
    if (atIdx > 0 && charBeforeAt !== undefined && !/\s/.test(charBeforeAt)) {
      mentionState = null
      return
    }
    const query = before.slice(atIdx + 1)
    if (/[^\w./-]/.test(query)) {
      mentionState = null
      return
    }
    void store.searchMentions(query).then((items) => {
      // Guard: the user may have moved on by the time the search returns.
      const stillOpen = textarea && textarea.selectionStart === cursor
      if (!stillOpen) return
      mentionState = {
        from: atIdx,
        query,
        items,
        selected: 0,
      }
    })
  }

  function pickMention(item: AiMentionCandidate): void {
    if (!mentionState || !textarea) return
    const { from, query } = mentionState
    const before = text.slice(0, from)
    const after = text.slice(from + 1 + query.length)
    const inserted = `@${item.label}`
    const trailingSpace = after.startsWith(' ') ? '' : ' '
    text = `${before}${inserted}${trailingSpace}${after}`
    store.setDraft(text)
    stagedMentions = [
      ...stagedMentions.filter((m) => m.label !== item.label),
      { kind: item.kind, label: item.label },
    ]
    mentionState = null
    // Move the cursor just past the inserted chip.
    const cursorPos = before.length + inserted.length + trailingSpace.length
    void tick().then(() => {
      if (textarea) {
        textarea.focus()
        textarea.setSelectionRange(cursorPos, cursorPos)
        autoresize()
      }
    })
  }

  function onKeydown(e: KeyboardEvent): void {
    if (mentionState) {
      if (e.key === 'ArrowDown') {
        e.preventDefault()
        mentionState.selected = Math.min(
          mentionState.selected + 1,
          Math.max(0, mentionState.items.length - 1)
        )
        return
      }
      if (e.key === 'ArrowUp') {
        e.preventDefault()
        mentionState.selected = Math.max(mentionState.selected - 1, 0)
        return
      }
      if (e.key === 'Enter' || e.key === 'Tab') {
        const picked = mentionState.items[mentionState.selected]
        if (picked) {
          e.preventDefault()
          pickMention(picked)
          return
        }
      }
      if (e.key === 'Escape') {
        e.preventDefault()
        mentionState = null
        return
      }
    }
    if (e.key === 'Enter' && !e.shiftKey && !e.metaKey && !e.ctrlKey) {
      e.preventDefault()
      submit()
    }
  }

  function submit(): void {
    const trimmed = text.trim()
    if (!trimmed || disabled) return
    store.send(trimmed, stagedMentions)
    stagedMentions = []
    text = ''
    store.setDraft('')
    mentionState = null
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
  {#if mentionState}
    <MentionPopup
      items={mentionState.items}
      selected={mentionState.selected}
      onPick={pickMention}
      onCancel={() => (mentionState = null)}
    />
  {/if}

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
    position: relative;
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
