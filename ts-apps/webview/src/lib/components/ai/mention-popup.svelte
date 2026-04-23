<script lang="ts" module>
  function kindLabel(kind: AiMentionCandidate['kind']): string {
    if (kind === 'file') return 'File'
    if (kind === 'symbol') return 'Symbol'
    return 'Selection'
  }
</script>

<script lang="ts">
  import type { AiMentionCandidate } from 'jl4-client-rpc'

  let {
    items,
    selected,
    onPick,
    onCancel,
  }: {
    items: AiMentionCandidate[]
    selected: number
    onPick: (item: AiMentionCandidate) => void
    onCancel: () => void
  } = $props()
</script>

<div class="mention-pop" role="listbox" aria-label="Mention suggestions">
  {#if items.length === 0}
    <div class="empty">No matches</div>
  {:else}
    {#each items as item, i}
      <button
        class="row"
        class:active={i === selected}
        role="option"
        aria-selected={i === selected}
        onclick={() => onPick(item)}
      >
        <span class="kind">{kindLabel(item.kind)}</span>
        <span class="label">{item.label}</span>
      </button>
    {/each}
  {/if}
</div>

<!-- Swallow outside clicks to avoid leaking into the editor. -->
<svelte:window onclick={onCancel} />

<style>
  .mention-pop {
    position: absolute;
    bottom: 100%;
    left: 8px;
    right: 8px;
    max-height: 240px;
    overflow-y: auto;
    background: var(
      --vscode-editorSuggestWidget-background,
      var(--vscode-dropdown-background)
    );
    border: 1px solid
      var(
        --vscode-editorSuggestWidget-border,
        var(--vscode-widget-border, rgba(128, 128, 128, 0.35))
      );
    border-radius: 4px;
    box-shadow: 0 4px 16px rgba(0, 0, 0, 0.3);
    z-index: 20;
    padding: 2px;
  }
  .empty {
    padding: 8px 10px;
    color: var(--vscode-descriptionForeground);
    font-size: 12px;
  }
  .row {
    display: flex;
    align-items: center;
    gap: 8px;
    width: 100%;
    text-align: left;
    background: transparent;
    border: none;
    color: var(--vscode-foreground);
    font-size: 12px;
    padding: 4px 8px;
    border-radius: 3px;
    cursor: pointer;
  }
  .row:hover,
  .row.active {
    background: var(
      --vscode-list-activeSelectionBackground,
      rgba(128, 128, 128, 0.25)
    );
    color: var(--vscode-list-activeSelectionForeground, inherit);
  }
  .kind {
    font-size: 10px;
    text-transform: uppercase;
    letter-spacing: 0.04em;
    color: var(--vscode-descriptionForeground);
    min-width: 55px;
  }
  .label {
    flex: 1;
    overflow: hidden;
    text-overflow: ellipsis;
    white-space: nowrap;
  }
</style>
