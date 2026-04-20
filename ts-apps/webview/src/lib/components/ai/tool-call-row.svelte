<script lang="ts">
  import type { RenderedToolCall } from '$lib/stores/ai-chat.svelte'

  let {
    call,
    onOpenFile,
    onOpenDiff,
  }: {
    call: RenderedToolCall
    /** Simple open: shows the target file in a regular editor tab.
     *  Used for read / create / delete rows. */
    onOpenFile: (callId: string) => void
    /** Applied-diff open: shows the before/after of a completed edit
     *  in the diff editor with red/green change markers. */
    onOpenDiff: (callId: string) => void
  } = $props()

  // Best-effort arg parse; keep going even if the JSON is malformed
  // so the row still renders.
  const args = $derived.by<Record<string, unknown>>(() => {
    try {
      return JSON.parse(call.argsJson) as Record<string, unknown>
    } catch {
      return {}
    }
  })

  type ClickMode = 'open' | 'diff' | null
  const view = $derived.by(() => viewFor(call.name, args))

  function viewFor(
    name: string,
    a: Record<string, unknown>
  ): { label: string; target: string | null; click: ClickMode } {
    const path = typeof a.path === 'string' ? a.path : null
    if (name === 'fs__read_file')
      return { label: 'Read', target: path, click: 'open' }
    if (name === 'fs__create_file')
      return { label: 'Create', target: path, click: 'open' }
    if (name === 'fs__edit_file')
      return {
        label: 'Edit',
        target: path,
        // Diff only makes sense once the edit has landed — before that
        // just open the file so the user can see the context.
        click: call.status === 'done' ? 'diff' : 'open',
      }
    if (name === 'fs__delete_file')
      return { label: 'Delete', target: path, click: null }
    if (name === 'lsp__diagnostics')
      // Same tabular open as read — the user can inspect the file
      // while its diagnostics are shown in the Problems panel.
      return { label: 'Check', target: path, click: 'open' }
    if (name === 'l4__evaluate')
      return { label: 'Evaluate', target: path, click: 'open' }
    if (name === 'meta__ask_user')
      return { label: 'Ask user', target: null, click: null }
    if (name.startsWith('l4-rules__'))
      // Show the rule name (minus the namespace prefix) as the target
      // so the chat reads `• Rule qualifies-for-discount` instead of
      // dumping the full mangled function name.
      return {
        label: 'L4 Rules',
        target: name.slice('l4-rules__'.length),
        click: null,
      }
    return { label: name, target: null, click: null }
  }

  function fire(): void {
    if (view.click === 'diff') onOpenDiff(call.callId)
    else if (view.click === 'open') onOpenFile(call.callId)
  }

  function onTargetClick(e: MouseEvent): void {
    if (!view.click) return
    e.preventDefault()
    fire()
  }

  function onTargetKeydown(e: KeyboardEvent): void {
    if (!view.click) return
    if (e.key === 'Enter' || e.key === ' ') {
      e.preventDefault()
      fire()
    }
  }
</script>

<div class="tool-call" class:is-error={call.status === 'error'}>
  <div class="tool-row">
    <span class="dot" aria-hidden="true"></span>
    <span class="action">{view.label}</span>
    {#if view.target}
      {#if view.click}
        <button
          type="button"
          class="target target-btn"
          title={view.click === 'diff'
            ? 'Click to open applied diff'
            : 'Click to open the file'}
          onclick={onTargetClick}
          onkeydown={onTargetKeydown}>{view.target}</button
        >
      {:else}
        <span class="target plain">{view.target}</span>
      {/if}
    {/if}
    <span class="status status-{call.status}">
      {#if call.status === 'pending-approval'}…
      {:else if call.status === 'running'}…
      {:else if call.status === 'done'}✓
      {:else if call.status === 'error'}failed
      {/if}
    </span>
  </div>

  <!-- {#if call.status === 'error' && call.error}
    <div class="err">{call.error}</div>
  {/if} -->
</div>

<style>
  .tool-call {
    font-size: 12px;
    padding: 6px 2px 8px;
    color: var(--vscode-descriptionForeground);
  }
  .tool-row {
    display: flex;
    /* Baseline alignment so the bold action label and the monospace
       filename sit on the same underline — different font metrics
       would otherwise make the filename drift up/down when centered. */
    align-items: baseline;
    gap: 6px;
  }
  .dot {
    background: #c8376a;
    line-height: 1;
    flex-shrink: 0;
    margin-right: 2px;
    padding: 0.2em;
    border-radius: 0.2em;
    margin-bottom: -0.2em;
  }
  .action {
    color: var(--vscode-foreground);
    font-weight: 600;
  }
  .target,
  .target.plain {
    color: var(--vscode-foreground);
    font-family: var(--vscode-editor-font-family, monospace);
    font-size: 0.92em;
    text-decoration: none;
  }
  .target-btn {
    background: transparent;
    border: none;
    padding: 0;
    cursor: pointer;
    color: var(--vscode-foreground);
  }
  .target-btn:hover {
    text-decoration: underline;
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
  .err {
    margin-top: 4px;
    color: var(--vscode-errorForeground, #d7263d);
    font-size: 11px;
    white-space: pre-wrap;
    word-break: break-word;
  }
</style>
