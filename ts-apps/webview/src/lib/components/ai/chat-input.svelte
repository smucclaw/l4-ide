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
    // Hijack the main input while the model is waiting on a
    // meta__ask_user question: sending the typed text answers the
    // question (the dispatcher resumes the same turn) instead of
    // starting a new one. The ask-user card above no longer renders
    // its own input/send — everything routes through here.
    if (store.pendingQuestion) {
      store.answerQuestion(trimmed)
    } else {
      store.send(trimmed, stagedMentions)
      stagedMentions = []
    }
    text = ''
    store.setDraft('')
    mentionState = null
    // Next tick so the textarea empties before we resize.
    void tick().then(() => autoresize())
  }

  function abort(): void {
    store.abort()
  }

  // Attachment picker state. `attachBusy` guards against double-clicks
  // while the native dialog is open; `attachNote` surfaces soft warnings
  // ("this PDF might eat a lot of context") and rejection reasons from
  // the extension's size/type validation.
  let attachBusy = $state(false)
  let attachNote = $state<string | null>(null)

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

  // Sync the textarea from the store on two signals:
  //   1. active conversation switched → pull the new conv's draft.
  //   2. `draftSeedVersion` bumped by store.seedDraft() → pull the
  //      externally-seeded text (Get Started button, right-click
  //      "Ask Legalese AI about this", host AiChatSeedDraft).
  //
  // We deliberately do NOT depend on `text` itself, because that
  // races with setDraft on every keystroke and clobbers the most
  // recent character. `store.setDraft` is keystroke-only and does
  // not bump the version, so typing never retriggers this effect.
  let lastSyncedId: string | null | undefined = undefined
  let lastSyncedSeedVersion = -1
  $effect(() => {
    const id = store.currentId
    const seedVersion = store.draftSeedVersion
    if (id === lastSyncedId && seedVersion === lastSyncedSeedVersion) return
    lastSyncedId = id
    lastSyncedSeedVersion = seedVersion
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

  {#if store.stagedAttachments.length > 0}
    <div class="attachment-strip" role="list" aria-label="Staged attachments">
      {#each store.stagedAttachments as att, i (att.name + i)}
        <span class="attachment-chip" role="listitem">
          <button
            type="button"
            class="chip-body"
            title="Preview {att.name}"
            onclick={() => store.previewAttachment(att)}
          >
            <svg
              class="chip-icon"
              viewBox="0 0 16 16"
              aria-hidden="true"
              fill="none"
              stroke="currentColor"
              stroke-width="1.4"
              stroke-linejoin="round"
            >
              {#if att.kind === 'image'}
                <rect x="2" y="3" width="12" height="10" rx="1.5" />
                <circle cx="6" cy="7" r="1.2" />
                <path d="M14 11l-3.5-3.5L4 13" />
              {:else}
                <path d="M4 2h5l3 3v9H4z" />
                <path d="M9 2v3h3" />
              {/if}
            </svg>
            <span class="chip-name">{att.name}</span>
          </button>
          <button
            type="button"
            class="chip-remove"
            title="Remove attachment"
            aria-label="Remove attachment"
            onclick={() => store.removeAttachment(i)}>✕</button
          >
        </span>
      {/each}
    </div>
  {/if}
  {#if attachNote}
    <div class="attachment-note" role="status">
      {attachNote}
      <button
        type="button"
        class="note-dismiss"
        aria-label="Dismiss note"
        onclick={() => (attachNote = null)}>✕</button
      >
    </div>
  {/if}

  <textarea
    class="chat-textarea"
    bind:this={textarea}
    bind:value={text}
    oninput={onInput}
    onkeydown={onKeydown}
    placeholder={store.pendingQuestion
      ? 'Answer the question above…'
      : 'Ask anything about your rules or L4…'}
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
      {#if store.activeFile.name}
        <button
          type="button"
          class="active-file-btn"
          class:excluded={!store.includeActiveFile}
          onclick={() => store.toggleIncludeActiveFile()}
          title={store.includeActiveFile
            ? `Including "${store.activeFile.path ?? store.activeFile.name}" in this turn's context. Click to exclude.`
            : `"${store.activeFile.path ?? store.activeFile.name}" is excluded from this turn's context. Click to include.`}
        >
          {#if store.includeActiveFile}
            <!-- Document glyph: the file IS attached. -->
            <svg
              class="active-file-icon"
              viewBox="0 0 16 16"
              aria-hidden="true"
              fill="none"
              stroke="currentColor"
              stroke-width="1.4"
              stroke-linejoin="round"
            >
              <path d="M4 2h5l3 3v9H4z" />
              <path d="M9 2v3h3" />
            </svg>
          {:else}
            <!-- Eye-with-slash: the file is invisible to the model. -->
            <svg
              class="active-file-icon"
              viewBox="0 0 16 16"
              aria-hidden="true"
              fill="none"
              stroke="currentColor"
              stroke-width="1.4"
              stroke-linecap="round"
              stroke-linejoin="round"
            >
              <path d="M2 8s2.2-3.5 6-3.5S14 8 14 8s-2.2 3.5-6 3.5S2 8 2 8z" />
              <circle cx="8" cy="8" r="1.5" />
              <path d="M3 13L13 3" />
            </svg>
          {/if}
          <span class="active-file-label">{store.activeFile.name}</span>
        </button>
      {/if}
      <button
        class="icon-btn"
        title={attachBusy
          ? 'Picking attachment…'
          : 'Attach an image, PDF, or text file (.l4, .md, .json, …)'}
        aria-label="Attach an image or PDF"
        disabled={disabled || attachBusy}
        onclick={pickAttachment}
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
      {#if isStreaming && !text.trim()}
        <!-- Stop button: visible while the turn is streaming AND the
             input is empty. Once the user types anything, the button
             flips to Send so a single click queues a follow-up
             (which the store routes through AiChatInject — folded
             into the next tool round or seeded into a fresh
             sub-turn). Same behaviour applies when the model is
             paused on a meta__ask_user question: typed text turns
             the button into Send so a single Enter answers. -->
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
          title={store.pendingQuestion
            ? 'Answer the question (Enter)'
            : 'Send (Enter)'}
          aria-label={store.pendingQuestion ? 'Answer' : 'Send'}
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
    box-shadow: 0 0 10px 2px
      var(--vscode-input-background, rgba(127, 127, 127, 0.12));
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
  /* Active-file chip: intentionally flat (no border, no background).
     Slightly muted label color so it reads as secondary, not a primary
     CTA. The icon flips between a document glyph (included) and an
     eye-with-slash (excluded) to signal whether the file ships with
     the next turn. */
  .active-file-btn {
    display: inline-flex;
    align-items: center;
    gap: 4px;
    background: transparent;
    border: none;
    padding: 2px 6px 2px 4px;
    height: 24px;
    cursor: pointer;
    color: var(--vscode-descriptionForeground);
    font-family: var(--vscode-font-family, inherit);
    font-size: 12px;
    max-width: 180px;
  }
  .active-file-btn:hover {
    color: var(--vscode-foreground);
  }
  .active-file-btn.excluded {
    opacity: 0.65;
  }
  .active-file-icon {
    width: 13px;
    height: 13px;
    flex-shrink: 0;
    display: block;
  }
  .active-file-label {
    overflow: hidden;
    text-overflow: ellipsis;
    white-space: nowrap;
  }
  /* Staged attachments ride above the textarea as a compact chip row.
     Each chip shows a kind-specific glyph, the filename, and a remove
     button. Uses widget-border / description-foreground so it blends
     into the input box rather than standing out. */
  .attachment-strip {
    display: flex;
    flex-wrap: wrap;
    gap: 4px;
    padding: 6px 8px 0;
  }
  .attachment-chip {
    display: inline-flex;
    align-items: center;
    gap: 4px;
    padding: 2px 2px 2px 4px;
    /* Mild gray fill instead of an outline — reads as a captured
       value rather than an empty bordered box. Same look used on the
       user message's echoed chips in message-user.svelte. */
    background: rgba(128, 128, 128, 0.14);
    border-radius: 3px;
    font-size: 11px;
    color: var(--vscode-descriptionForeground);
    max-width: 220px;
  }
  .chip-body {
    display: inline-flex;
    align-items: center;
    gap: 4px;
    background: transparent;
    border: none;
    padding: 0;
    color: inherit;
    cursor: pointer;
    font: inherit;
    overflow: hidden;
  }
  .chip-body:hover .chip-name {
    color: var(--vscode-foreground);
    text-decoration: underline;
  }
  .chip-icon {
    width: 12px;
    height: 12px;
    flex-shrink: 0;
  }
  .chip-name {
    overflow: hidden;
    text-overflow: ellipsis;
    white-space: nowrap;
  }
  .chip-remove {
    background: transparent;
    border: none;
    color: var(--vscode-descriptionForeground);
    cursor: pointer;
    font-size: 10px;
    padding: 0 2px;
  }
  .chip-remove:hover {
    color: var(--vscode-foreground);
  }
  .attachment-note {
    display: flex;
    align-items: flex-start;
    justify-content: space-between;
    gap: 6px;
    margin: 4px 8px 0;
    padding: 6px 8px;
    border: 1px solid var(--vscode-widget-border, rgba(128, 128, 128, 0.35));
    border-radius: 3px;
    font-size: 11px;
    color: var(--vscode-descriptionForeground);
    line-height: 1.4;
  }
  .note-dismiss {
    background: transparent;
    border: none;
    color: var(--vscode-descriptionForeground);
    cursor: pointer;
    font-size: 11px;
    padding: 0 2px;
    flex-shrink: 0;
  }
  .note-dismiss:hover {
    color: var(--vscode-foreground);
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
