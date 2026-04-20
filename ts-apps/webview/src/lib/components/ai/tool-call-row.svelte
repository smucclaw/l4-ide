<script lang="ts">
  import type { RenderedToolCall } from '$lib/stores/ai-chat.svelte'

  /**
   * Tools exposed by the l4-rules MCP server fall into two buckets.
   *
   *   Category 1 — "L4 Deployments" infrastructure tools: file I/O and
   *                index/search over the user's deployed rules. They
   *                don't evaluate business logic; they just let the
   *                agent browse what's available.
   *   Category 2 — "L4 Rule" evaluations: every other tool name in the
   *                namespace is a deployed rule the agent can invoke
   *                with inputs. The inspection / ladder rendering is
   *                reserved for this bucket.
   *
   * Keeping this set in sync with the MCP server's built-ins; anything
   * not in here is treated as a rule.
   */
  const L4_RULES_INFRASTRUCTURE = new Set([
    'list_files',
    'read_file',
    'search_identifier',
    'search_text',
  ])

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
      return { label: 'Question', target: null, click: null }
    if (name.startsWith('l4-rules__')) {
      const sub = name.slice('l4-rules__'.length)
      // Infrastructure tools (list_files / read_file / search_*) are
      // deployment inventory, not rule evaluations — label them
      // distinctly so the user sees at a glance whether the agent is
      // browsing deployments or actually running a rule.
      const isInfra = L4_RULES_INFRASTRUCTURE.has(sub)
      return {
        label: isInfra ? 'L4 Deployments' : 'L4 Rule',
        target: sub,
        click: null,
      }
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

  // Pretty-print the tool result for the expandable details panel.
  // Most tools return JSON strings; we try-parse and re-stringify with
  // indentation. If it's plain text, just show it verbatim.
  const prettyResult = $derived.by(() => {
    if (!call.result) return null
    try {
      return JSON.stringify(JSON.parse(call.result), null, 2)
    } catch {
      return call.result
    }
  })

  const hasDetails = $derived(
    call.status === 'done'
      ? !!prettyResult
      : call.status === 'error'
        ? !!call.error
        : false
  )

  let expanded = $state(false)
  function toggle(): void {
    if (!hasDetails) return
    expanded = !expanded
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
    {#if hasDetails}
      <button
        type="button"
        class="expand-btn"
        onclick={toggle}
        title={expanded ? 'Hide details' : 'Show details'}
        aria-expanded={expanded}
        aria-label="Toggle details">{expanded ? '▾' : '▸'}</button
      >
    {/if}
  </div>

  {#if expanded && call.status === 'done' && prettyResult}
    <pre class="details">{prettyResult}</pre>
  {:else if expanded && call.status === 'error' && call.error}
    <pre class="details err-details">{call.error}</pre>
  {/if}
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
    text-align: left;
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
  .expand-btn {
    background: transparent;
    border: none;
    color: var(--vscode-descriptionForeground);
    font-size: 11px;
    padding: 0 2px;
    margin-left: 2px;
    cursor: pointer;
  }
  .expand-btn:hover {
    color: var(--vscode-foreground);
  }
  .details {
    margin: 4px 0 0 18px;
    padding: 6px 8px;
    background: var(--vscode-editor-background);
    border: 1px solid var(--vscode-widget-border, rgba(128, 128, 128, 0.35));
    border-radius: 4px;
    font-family: var(--vscode-editor-font-family, monospace);
    font-size: 11px;
    color: var(--vscode-foreground);
    white-space: pre-wrap;
    word-break: break-word;
    max-height: 320px;
    overflow-y: auto;
  }
  .err-details {
    color: var(--vscode-errorForeground, #d7263d);
  }
</style>
