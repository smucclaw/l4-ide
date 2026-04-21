<script lang="ts">
  import StreamingMarkdown from './streaming-markdown.svelte'
  import ErrorBubble from './error-bubble.svelte'
  // import CopyButton from './copy-button.svelte'
  import ToolCallRow from './tool-call-row.svelte'
  import type { AssistantBlock } from '$lib/stores/ai-chat.svelte'

  let {
    content,
    streaming,
    error,
    blocks,
    onRetry,
    onOpenFile,
    onOpenFileDiff,
  }: {
    content: string
    streaming: boolean
    error?: { message: string; code?: string }
    blocks?: AssistantBlock[]
    onRetry?: () => void
    onOpenFile: (callId: string) => void
    onOpenFileDiff: (callId: string) => void
  } = $props()

  // Stable keys for the block iteration. Text blocks don't carry an
  // id, so we use the index; tool-call blocks key by callId;
  // tool-activity blocks use `ta:{tool}:{i}` so same-tool reruns get
  // distinct DOM nodes.
  function blockKey(b: AssistantBlock, i: number): string {
    if (b.kind === 'tool-call') return `tc:${b.call.callId}`
    if (b.kind === 'tool-activity') return `ta:${b.activity.tool}:${i}`
    if (b.kind === 'thinking') return `th:${i}`
    return `tx:${i}`
  }

  // Per-block expanded state for thinking blocks, keyed by `i`. Stays
  // collapsed by default — the model's reasoning stream is high-volume
  // noise the user rarely wants to read in full.
  const thinkingExpanded = $state<Record<number, boolean>>({})

  // Turn-end review card: once the turn has finished, summarize which
  // files the assistant created/edited so the user can scan for blast
  // radius at a glance and jump straight to the applied diff(s).
  type FileChange = {
    path: string
    action: 'created' | 'edited' | 'deleted'
    /** Call id whose snapshot the diff/open view should anchor to.
     *  For `created` this points at the CREATE call (the file exists
     *  post-creation; opening it shows the new content as a plain
     *  editor tab — no diff). For `edited` it points at the FIRST
     *  edit call in the turn, so the applied-diff view shows the
     *  full turn-wide delta rather than just the last edit. */
    callId: string
  }
  const fileChanges = $derived.by<FileChange[]>(() => {
    if (!blocks) return []
    // Aggregate into a single row per path so the card shows one
    // outcome per file for the whole turn, not every intermediate
    // step. Rules:
    //   created + (edited*) → created (click opens file as a tab)
    //   created + deleted → dropped (net effect = nothing)
    //   edited (only)      → edited  (click opens applied diff)
    //   deleted (only)     → deleted (click opens file)
    type PathState = {
      created: boolean
      edited: boolean
      deleted: boolean
      firstCreateId: string | null
      firstEditId: string | null
      firstDeleteId: string | null
      /** Order of first appearance so the card preserves the path
       *  ordering the user saw the tool rows in. */
      order: number
    }
    const byPath = new Map<string, PathState>()
    let nextOrder = 0
    for (const b of blocks) {
      if (b.kind !== 'tool-call' || b.call.status !== 'done') continue
      const call = b.call
      let kind: 'created' | 'edited' | 'deleted' | null = null
      if (call.name === 'fs__edit_file') kind = 'edited'
      else if (call.name === 'fs__create_file') kind = 'created'
      else if (call.name === 'fs__delete_file') kind = 'deleted'
      if (!kind) continue
      let path: string | undefined
      try {
        path = (JSON.parse(call.argsJson) as { path?: string }).path
      } catch {
        continue
      }
      if (!path) continue
      let state = byPath.get(path)
      if (!state) {
        state = {
          created: false,
          edited: false,
          deleted: false,
          firstCreateId: null,
          firstEditId: null,
          firstDeleteId: null,
          order: nextOrder++,
        }
        byPath.set(path, state)
      }
      if (kind === 'created') {
        state.created = true
        if (!state.firstCreateId) state.firstCreateId = call.callId
      } else if (kind === 'edited') {
        state.edited = true
        if (!state.firstEditId) state.firstEditId = call.callId
      } else {
        state.deleted = true
        if (!state.firstDeleteId) state.firstDeleteId = call.callId
      }
    }
    const rows: Array<FileChange & { order: number }> = []
    for (const [path, s] of byPath) {
      // Created then deleted in the same turn cancels out — nothing
      // for the user to look at afterwards, so drop the row entirely.
      if (s.created && s.deleted) continue
      if (s.created) {
        rows.push({
          path,
          action: 'created',
          callId: s.firstCreateId!,
          order: s.order,
        })
      } else if (s.deleted) {
        rows.push({
          path,
          action: 'deleted',
          callId: s.firstDeleteId!,
          order: s.order,
        })
      } else if (s.edited) {
        rows.push({
          path,
          action: 'edited',
          callId: s.firstEditId!,
          order: s.order,
        })
      }
    }
    rows.sort((a, b) => a.order - b.order)
    return rows.map(({ path, action, callId }) => ({ path, action, callId }))
  })
</script>

<div class="assistant-row">
  <div class="assistant-bubble">
    {#if blocks && blocks.length > 0}
      {#each blocks as block, i (blockKey(block, i))}
        {#if block.kind === 'text'}
          <StreamingMarkdown
            text={block.text}
            streaming={streaming && i === blocks.length - 1}
          />
        {:else if block.kind === 'tool-call'}
          <ToolCallRow
            call={block.call}
            {onOpenFile}
            onOpenDiff={onOpenFileDiff}
          />
        {:else if block.kind === 'thinking'}
          <!-- Collapsed-by-default reasoning block. Chevron flips when
               toggled; expanded body renders as italic gray text with
               preserved whitespace so chain-of-thought indentation
               stays intact. -->
          <div class="thinking">
            <button
              type="button"
              class="thinking-toggle"
              onclick={() => (thinkingExpanded[i] = !thinkingExpanded[i])}
              aria-expanded={!!thinkingExpanded[i]}
            >
              <span class="thinking-chev"
                >{thinkingExpanded[i] ? '▾' : '▸'}</span
              >
              <span class="thinking-label"
                >Thinking{streaming && i === blocks.length - 1 ? '…' : ''}</span
              >
            </button>
            {#if thinkingExpanded[i]}
              <div class="thinking-body">{block.text}</div>
            {/if}
          </div>
        {:else if block.kind === 'tool-activity'}
          <!-- Server-side activity keeps the crimson dot up front (no
               expand chevron — nothing to expand on read-only backend
               events) + bold label + monospace message. No right-side
               status; the dot and message together are enough. -->
          <div
            class="tool-call"
            class:is-error={block.activity.status === 'error'}
          >
            <div class="tool-row">
              <span class="dot" aria-hidden="true"></span>
              <span class="action">Legalesing...</span>
              {#if block.activity.message !== 'Legalesing...'}
                <span class="target plain">{block.activity.message}</span>
              {/if}
            </div>
          </div>
        {/if}
      {/each}
    {:else if content}
      <StreamingMarkdown text={content} {streaming} />
    {/if}
    <!-- {#if !streaming && content}
      <div class="assistant-copy">
        <CopyButton getText={() => content} />
      </div>
    {/if} -->

    {#if !streaming && fileChanges.length > 0}
      <div
        class="review-card"
        role="group"
        aria-label="Files changed this turn"
      >
        <div class="review-header">
          <span class="review-title"
            >Files changed this turn ({fileChanges.length})</span
          >
        </div>
        <ul class="review-list">
          {#each fileChanges as change (change.callId)}
            <li class="review-row">
              <span class="review-action review-action-{change.action}"
                >{change.action}</span
              >
              <button
                type="button"
                class="review-path"
                onclick={() =>
                  change.action === 'edited'
                    ? onOpenFileDiff(change.callId)
                    : onOpenFile(change.callId)}
                title={change.action === 'edited'
                  ? 'Open applied diff'
                  : 'Open file'}>{change.path}</button
              >
            </li>
          {/each}
        </ul>
      </div>
    {/if}
    {#if error}
      <ErrorBubble message={error.message} code={error.code} {onRetry} />
    {/if}
  </div>
</div>

<style>
  .assistant-row {
    display: flex;
    padding: 4px 0;
  }
  .assistant-bubble {
    position: relative;
    width: 100%;
    font-size: 13px;
    line-height: 1.45;
    color: var(--vscode-foreground);
  }
  /* .assistant-copy {
    position: absolute;
    top: -4px;
    right: 0;
  } */
  /* Tool-activity rows reuse the exact visual chrome used by
     tool-call rows (see tool-call-row.svelte). Styles are duplicated
     here because Svelte scopes CSS per-component; extracting into a
     global sheet would pull every page's .tool-call rules along with
     it. Keep the two blocks in sync. */
  .tool-call {
    font-size: 12px;
    padding: 6px 2px 8px;
    color: var(--vscode-descriptionForeground);
  }
  .tool-row {
    display: flex;
    align-items: baseline;
    gap: 6px;
  }
  .dot {
    position: relative;
    background: #c8376a;
    line-height: 1;
    flex-shrink: 0;
    margin: 0 8px 0 2px;
    padding: 0.2em;
    border-radius: 0.2em;
    top: -0.15em;
  }
  .action {
    color: var(--vscode-foreground);
    font-weight: 600;
  }
  .target.plain {
    color: var(--vscode-foreground);
    font-family: var(--vscode-editor-font-family, monospace);
    font-size: 0.92em;
    text-decoration: none;
  }
  .status {
    margin-left: auto;
    font-size: 11px;
    color: var(--vscode-descriptionForeground);
  }
  .status-error {
    color: var(--vscode-errorForeground, #d7263d);
  }
  .status-done {
    color: var(--vscode-foreground);
    opacity: 0.6;
  }
  /* Thinking block: chevron-prefixed toggle, collapsed by default;
     expanded body is italic and grayer than body text so it reads as
     the model talking to itself. */
  .thinking {
    margin: 4px 0;
  }
  .thinking-toggle {
    display: inline-flex;
    align-items: center;
    gap: 4px;
    background: transparent;
    border: none;
    padding: 2px 0;
    font-size: 11px;
    color: var(--vscode-descriptionForeground);
    cursor: pointer;
  }
  .thinking-toggle:hover {
    color: var(--vscode-foreground);
  }
  .thinking-chev {
    font-size: 10px;
    width: 10px;
    display: inline-block;
    text-align: center;
  }
  .thinking-label {
    text-transform: uppercase;
    letter-spacing: 0.04em;
  }
  .thinking-body {
    margin-top: 4px;
    padding: 6px 10px;
    border-left: 2px solid
      var(--vscode-widget-border, rgba(128, 128, 128, 0.35));
    color: var(--vscode-descriptionForeground);
    font-style: italic;
    font-size: 12px;
    line-height: 1.5;
    white-space: pre-wrap;
    word-break: break-word;
  }
  /* Turn-end review card: quiet panel listing files the assistant
     touched this turn so the user can scan blast radius at a glance. */
  .review-card {
    margin-top: 10px;
    padding: 8px 10px;
    border: 1px solid var(--vscode-widget-border, rgba(128, 128, 128, 0.35));
    border-radius: 4px;
    background: var(--vscode-editor-background);
    font-size: 11px;
  }
  .review-header {
    display: flex;
    justify-content: space-between;
    align-items: center;
    gap: 8px;
  }
  .review-title {
    color: var(--vscode-foreground);
    font-weight: 600;
  }
  .review-list {
    list-style: none;
    padding: 0;
    margin: 6px 0 0;
    display: flex;
    flex-direction: column;
    gap: 3px;
  }
  .review-row {
    display: flex;
    align-items: baseline;
    gap: 8px;
  }
  .review-action {
    font-family: var(--vscode-editor-font-family, monospace);
    font-size: 10px;
    text-transform: uppercase;
    opacity: 0.8;
    min-width: 52px;
  }
  .review-action-created {
    color: #78c47c;
  }
  .review-action-edited {
    color: #c8c877;
  }
  .review-action-deleted {
    color: var(--vscode-errorForeground, #d7263d);
  }
  .review-path {
    background: transparent;
    border: none;
    padding: 0;
    color: var(--vscode-foreground);
    font-family: var(--vscode-editor-font-family, monospace);
    font-size: 11px;
    cursor: pointer;
    text-align: left;
  }
  .review-path:hover {
    text-decoration: underline;
  }
</style>
