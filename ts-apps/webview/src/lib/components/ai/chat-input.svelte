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

  // Sync the textarea from the store ONLY when the active conversation
  // changes. Earlier versions of this effect also depended on `text`,
  // which made it re-run on every keystroke — and race with `setDraft`,
  // clobbering the character the user just typed. Pin the effect to
  // `currentId` alone via a tracked sentinel and use $state.snapshot
  // to sidestep Svelte's fine-grained tracking for the draft read.
  let lastSyncedId: string | null | undefined = undefined
  $effect(() => {
    const id = store.currentId
    if (id === lastSyncedId) return
    lastSyncedId = id
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
    placeholder="Ask anything about your rules or L4…"
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
        aria-label="New conversation"
      >
        <svg viewBox="0 0 16 16" aria-hidden="true">
          <path
            d="M8 3v10M3 8h10"
            stroke="currentColor"
            stroke-width="1.5"
            stroke-linecap="round"
          />
        </svg>
      </button>
      <button
        class="icon-btn"
        onclick={onOpenHistory}
        title="Conversation history"
        aria-label="Conversation history"
      >
        <svg viewBox="0 0 16 16" aria-hidden="true">
          <circle
            cx="8"
            cy="8"
            r="5.5"
            stroke="currentColor"
            stroke-width="1.5"
            fill="none"
          />
          <path
            d="M8 5v3.2L10.1 10"
            stroke="currentColor"
            stroke-width="1.5"
            fill="none"
            stroke-linecap="round"
            stroke-linejoin="round"
          />
        </svg>
      </button>
      <button
        class="icon-btn"
        onclick={onOpenSettings}
        title="AI chat settings"
        aria-label="AI chat settings"
      >
        <!-- Sliders icon: three horizontal rails, each with a knob
             offset at a different position. Reads as "settings" more
             clearly than a gear at 16px. -->
        <svg
          viewBox="0 0 16 16"
          aria-hidden="true"
          fill="none"
          stroke="currentColor"
          stroke-width="1.5"
          stroke-linecap="round"
        >
          <line x1="2.5" y1="4" x2="13.5" y2="4" />
          <line x1="2.5" y1="8" x2="13.5" y2="8" />
          <line x1="2.5" y1="12" x2="13.5" y2="12" />
          <circle cx="5" cy="4" r="1.5" fill="currentColor" stroke="none" />
          <circle cx="11" cy="8" r="1.5" fill="currentColor" stroke="none" />
          <circle cx="7" cy="12" r="1.5" fill="currentColor" stroke="none" />
        </svg>
      </button>
    </div>
    <div class="right-actions">
      <button
        class="icon-btn muted"
        title="Attach files (coming soon)"
        aria-label="Attach files"
        disabled
      >
        <svg viewBox="0 0 16 16" aria-hidden="true">
          <path
            d="M12.5 7.5L7 13a3 3 0 0 1-4.2-4.2L8.8 2.8a2 2 0 0 1 2.8 2.8L5.8 11.4a1 1 0 0 1-1.4-1.4L9.2 5.2"
            stroke="currentColor"
            stroke-width="1.5"
            fill="none"
            stroke-linecap="round"
            stroke-linejoin="round"
          />
        </svg>
      </button>
      {#if isStreaming}
        <button
          class="submit-btn stop"
          onclick={abort}
          title="Stop"
          aria-label="Stop"
        >
          <svg viewBox="0 0 16 16" aria-hidden="true">
            <rect
              x="4"
              y="4"
              width="8"
              height="8"
              rx="1.5"
              fill="currentColor"
            />
          </svg>
        </button>
      {:else}
        <button
          class="submit-btn"
          onclick={submit}
          disabled={disabled || !text.trim()}
          title="Send (Enter)"
          aria-label="Send"
        >
          <svg viewBox="0 0 16 16" aria-hidden="true">
            <path
              d="M8 13V3.5M3.5 7.5L8 3l4.5 4.5"
              stroke="currentColor"
              stroke-width="1.75"
              fill="none"
              stroke-linecap="round"
              stroke-linejoin="round"
            />
          </svg>
        </button>
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
    margin: 0px;
    transition: border-color 0.12s ease-out;
  }
  .chat-input-box:focus-within {
    border-color: var(--vscode-foreground, #ccc);
  }
  .chat-textarea {
    width: 100%;
    box-sizing: border-box;
    resize: none;
    background: transparent;
    color: var(--vscode-input-foreground);
    border: none;
    outline: none;
    padding: 10px 12px 12px;
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
    align-items: center;
    padding: 6px 6px;
    gap: 8px;
  }
  .left-actions,
  .right-actions {
    display: flex;
    align-items: center;
    gap: 2px;
  }
  .icon-btn {
    display: inline-flex;
    align-items: center;
    justify-content: center;
    width: 28px;
    height: 28px;
    background: transparent;
    color: var(--vscode-descriptionForeground);
    border: none;
    cursor: pointer;
    border-radius: 4px;
    padding: 0;
    transition:
      background-color 0.1s ease-out,
      color 0.1s ease-out;
  }
  .icon-btn svg {
    width: 16px;
    height: 16px;
    display: block;
  }
  .icon-btn:hover:not(:disabled) {
    color: var(--vscode-foreground);
    background: rgba(128, 128, 128, 0.14);
  }
  .icon-btn.muted:disabled {
    opacity: 0.35;
    cursor: not-allowed;
  }
  .submit-btn {
    display: inline-flex;
    align-items: center;
    justify-content: center;
    width: 28px;
    height: 28px;
    padding: 0;
    /* Matches the sidebar footer's Deploy action button so the two
       primary CTAs share a visual family. */
    background: #c8376a;
    color: #fff;
    border: none;
    cursor: pointer;
    border-radius: 4px;
    transition:
      background-color 0.1s ease-out,
      opacity 0.1s ease-out;
  }
  .submit-btn svg {
    width: 16px;
    height: 16px;
    display: block;
  }
  .submit-btn:hover:not(:disabled) {
    background: #d94d7e;
  }
  .submit-btn:disabled {
    opacity: 0.4;
    cursor: not-allowed;
  }
  /* Streaming state: darker tint of the same crimson so it reads as
     "same affordance, different mode" rather than a new color. */
  .submit-btn.stop {
    background: #6e1636;
    /* Dim the rectangle so it doesn't hot-spot against the dark bg. */
    color: rgba(255, 255, 255, 0.55);
  }
  .submit-btn.stop:hover {
    background: #7f1a3f;
    color: rgba(255, 255, 255, 0.75);
  }
</style>
