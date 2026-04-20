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
    onSignIn,
    onOpenFile,
    onOpenFileDiff,
  }: {
    content: string
    streaming: boolean
    error?: { message: string; code?: string }
    blocks?: AssistantBlock[]
    onRetry?: () => void
    onSignIn?: () => void
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
    return `tx:${i}`
  }

  // Turn-end review card: once the turn has finished, summarize which
  // files the assistant created/edited so the user can scan for blast
  // radius at a glance and jump straight to the applied diff(s).
  type FileChange = {
    path: string
    action: 'created' | 'edited' | 'deleted'
    callId: string
  }
  const fileChanges = $derived.by<FileChange[]>(() => {
    if (!blocks) return []
    // Dedupe by `path+action` so repeated edits to the same file
    // collapse into a single row. We keep the LATEST callId because
    // its pre-edit snapshot is the one we want the diff view to use
    // — earlier edits' snapshots have already been superseded. Order
    // of first appearance is preserved; de-duplication just skips the
    // repeats.
    const byKey = new Map<string, FileChange>()
    for (const b of blocks) {
      if (b.kind !== 'tool-call' || b.call.status !== 'done') continue
      const call = b.call
      let action: FileChange['action'] | null = null
      if (call.name === 'fs__edit_file') action = 'edited'
      else if (call.name === 'fs__create_file') action = 'created'
      else if (call.name === 'fs__delete_file') action = 'deleted'
      if (!action) continue
      try {
        const args = JSON.parse(call.argsJson) as { path?: string }
        if (!args.path) continue
        const key = `${action}:${args.path}`
        byKey.set(key, { path: args.path, action, callId: call.callId })
      } catch {
        // ignore malformed args
      }
    }
    return [...byKey.values()]
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
        {:else if block.kind === 'tool-activity'}
          <!-- Same chrome as a tool-call row (colored dot + bold label
               + monospace target + right-aligned status) so server-side
               activity reads as another tool entry instead of a different
               visual category. No click target because these are
               read-only backend events. -->
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
              <span class="status status-{block.activity.status}">
                {#if block.activity.status === 'running'}…
                {:else if block.activity.status === 'done'}✓
                {:else if block.activity.status === 'error'}failed
                {/if}
              </span>
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
                  change.action === 'deleted'
                    ? onOpenFile(change.callId)
                    : onOpenFileDiff(change.callId)}
                title={change.action === 'deleted'
                  ? 'Open file'
                  : 'Open applied diff'}>{change.path}</button
              >
            </li>
          {/each}
        </ul>
      </div>
    {/if}
    {#if error}
      <ErrorBubble
        message={error.message}
        code={error.code}
        {onRetry}
        {onSignIn}
      />
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
    margin-right: 2px;
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
