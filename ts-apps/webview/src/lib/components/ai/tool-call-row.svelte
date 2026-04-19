<script lang="ts">
  import type { RenderedToolCall } from '$lib/stores/ai-chat.svelte'

  let {
    call,
    onApprove,
    onOpenDiff,
  }: {
    call: RenderedToolCall
    onApprove: (
      callId: string,
      decision: 'allow' | 'deny' | 'alwaysAllow'
    ) => void
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

  // Human-readable label for the tool — the in-chat UX is "Edit
  // foo.l4", "Read bar.l4", etc., so we pull the path out of the
  // typed args when we know the tool.
  const view = $derived.by(() => viewFor(call.name, args))

  function viewFor(
    name: string,
    a: Record<string, unknown>
  ): { icon: string; label: string; target: string | null; canDiff: boolean } {
    const path = typeof a.path === 'string' ? a.path : null
    if (name === 'fs__read_file')
      return { icon: '👁', label: 'Read', target: path, canDiff: false }
    if (name === 'fs__create_file')
      return { icon: '📝', label: 'Create', target: path, canDiff: true }
    if (name === 'fs__edit_file')
      return { icon: '📝', label: 'Edit', target: path, canDiff: true }
    if (name === 'fs__delete_file')
      return { icon: '🗑', label: 'Delete', target: path, canDiff: false }
    // Unknown tool: fall back to showing the raw name.
    return { icon: '⚙', label: name, target: null, canDiff: false }
  }

  function onTargetClick(e: MouseEvent): void {
    // cmd+click (or ctrl+click on Windows/Linux) opens the diff.
    if (!view.canDiff) return
    if (!(e.metaKey || e.ctrlKey)) return
    e.preventDefault()
    onOpenDiff(call.callId)
  }

  function onTargetKeydown(e: KeyboardEvent): void {
    if (!view.canDiff) return
    if (e.key === 'Enter' && (e.metaKey || e.ctrlKey)) {
      e.preventDefault()
      onOpenDiff(call.callId)
    }
  }
</script>

<div class="tool-call" class:is-error={call.status === 'error'}>
  <div class="tool-row">
    <span class="icon" aria-hidden="true">{view.icon}</span>
    <span class="action">{view.label}</span>
    {#if view.target}
      {#if view.canDiff}
        <button
          type="button"
          class="target target-btn"
          title="⌘ + click to open proposed diff"
          onclick={onTargetClick}
          onkeydown={onTargetKeydown}>{view.target}</button
        >
      {:else}
        <span class="target plain">{view.target}</span>
      {/if}
    {/if}
    <span class="status status-{call.status}">
      {#if call.status === 'pending-approval'}awaiting approval
      {:else if call.status === 'running'}running…
      {:else if call.status === 'done'}✓
      {:else if call.status === 'error'}failed
      {/if}
    </span>
  </div>

  {#if call.status === 'pending-approval'}
    <div class="approve-banner" role="group" aria-label="Approve tool call">
      <button
        class="approve-btn allow"
        onclick={() => onApprove(call.callId, 'allow')}>Allow</button
      >
      <button
        class="approve-btn deny"
        onclick={() => onApprove(call.callId, 'deny')}>Deny</button
      >
      <button
        class="approve-btn always"
        onclick={() => onApprove(call.callId, 'alwaysAllow')}
        title="Always allow this category of tool">Always allow</button
      >
    </div>
  {/if}

  {#if call.status === 'error' && call.error}
    <div class="err">{call.error}</div>
  {/if}
</div>

<style>
  .tool-call {
    font-size: 12px;
    padding: 4px 2px;
    color: var(--vscode-descriptionForeground);
  }
  .tool-row {
    display: flex;
    align-items: center;
    gap: 6px;
  }
  .icon {
    flex-shrink: 0;
  }
  .action {
    color: var(--vscode-foreground);
  }
  .target {
    color: var(--l4-tok-identifier, #4ec9b0);
    font-family: var(--vscode-editor-font-family, monospace);
    font-size: 0.92em;
    text-decoration: none;
    cursor: text;
  }
  .target-btn {
    background: transparent;
    border: none;
    padding: 0;
    cursor: text;
  }
  .target-btn:hover {
    text-decoration: underline;
    cursor: pointer;
  }
  .target.plain {
    font-family: var(--vscode-editor-font-family, monospace);
    font-size: 0.92em;
    color: var(--l4-tok-identifier, #4ec9b0);
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
  .approve-banner {
    display: flex;
    gap: 6px;
    padding: 6px 0 2px;
  }
  .approve-btn {
    border: 1px solid var(--vscode-widget-border, rgba(128, 128, 128, 0.35));
    background: transparent;
    color: var(--vscode-foreground);
    padding: 2px 10px;
    font-size: 11px;
    border-radius: 3px;
    cursor: pointer;
  }
  .approve-btn.allow {
    background: #c8376a;
    border-color: transparent;
    color: #fff;
  }
  .approve-btn.allow:hover {
    background: #d94d7e;
  }
  .approve-btn.deny:hover {
    border-color: var(--vscode-errorForeground, #d7263d);
    color: var(--vscode-errorForeground, #d7263d);
  }
  .approve-btn.always:hover {
    border-color: var(--vscode-foreground);
  }
  .err {
    margin-top: 4px;
    color: var(--vscode-errorForeground, #d7263d);
    font-size: 11px;
    white-space: pre-wrap;
    word-break: break-word;
  }
</style>
