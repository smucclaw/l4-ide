<script lang="ts">
  import type { AiConversationSummary } from 'jl4-client-rpc'

  let {
    items,
    currentId,
    streamingIds,
    onLoad,
    onDelete,
    onClose,
  }: {
    items: AiConversationSummary[]
    currentId: string | null
    /** Ids of conversations currently attached to a live stream. The
     *  row for each shows a small spinner so the user can see which
     *  chats are burning tokens in the background — a defence against
     *  silent ghost-sessions. */
    streamingIds: string[]
    onLoad: (id: string) => void
    onDelete: (id: string) => void
    onClose: () => void
  } = $props()

  const streamingSet = $derived(new Set(streamingIds))

  function relativeTime(iso: string): string {
    const d = new Date(iso).getTime()
    const now = Date.now()
    const diff = Math.floor((now - d) / 1000)
    if (diff < 60) return 'just now'
    if (diff < 3600) return `${Math.floor(diff / 60)}m ago`
    if (diff < 86_400) return `${Math.floor(diff / 3600)}h ago`
    if (diff < 604_800) return `${Math.floor(diff / 86_400)}d ago`
    return new Date(d).toLocaleDateString()
  }

  function groupKey(iso: string): string {
    const d = new Date(iso)
    const now = new Date()
    const today = new Date(now.getFullYear(), now.getMonth(), now.getDate())
    const yesterday = new Date(today.getTime() - 86_400_000)
    if (d >= today) return 'Today'
    if (d >= yesterday) return 'Yesterday'
    if (now.getTime() - d.getTime() < 7 * 86_400_000) return 'Earlier this week'
    if (now.getTime() - d.getTime() < 30 * 86_400_000) return 'This month'
    return 'Older'
  }

  const grouped = $derived.by(() => {
    const groups = new Map<string, AiConversationSummary[]>()
    for (const item of items) {
      const key = groupKey(item.lastActiveAt)
      let bucket = groups.get(key)
      if (!bucket) {
        bucket = []
        groups.set(key, bucket)
      }
      bucket.push(item)
    }
    return [...groups.entries()]
  })
</script>

<div class="history-overlay" role="dialog" aria-label="Conversation history">
  <div class="history-head">
    <span>History</span>
    <button class="close-btn" onclick={onClose} aria-label="Close">✕</button>
  </div>
  <div class="history-scroll">
    {#if items.length === 0}
      <div class="empty-hint">No conversations yet.</div>
    {:else}
      {#each grouped as [label, bucket]}
        <div class="group-label">{label}</div>
        {#each bucket as item (item.id)}
          <div class="history-row" class:active={item.id === currentId}>
            <button
              class="row-main"
              onclick={() => onLoad(item.id)}
              title={item.title}
            >
              <span class="row-title">
                {#if streamingSet.has(item.id)}
                  <span
                    class="streaming-dot"
                    aria-label="Streaming"
                    title="A turn is still running in this conversation"
                  ></span>
                {/if}
                {item.title || 'Untitled'}
              </span>
              <span class="row-meta">{relativeTime(item.lastActiveAt)}</span>
            </button>
            <button
              class="row-delete"
              onclick={() => onDelete(item.id)}
              title="Delete conversation"
              aria-label="Delete conversation">🗑</button
            >
          </div>
        {/each}
      {/each}
    {/if}
  </div>
</div>

<style>
  .history-overlay {
    position: absolute;
    inset: 0;
    background: var(--vscode-sideBar-background);
    display: flex;
    flex-direction: column;
    z-index: 10;
  }
  .history-head {
    display: flex;
    align-items: center;
    justify-content: space-between;
    padding: 8px 12px;
    border-bottom: 1px solid
      var(--vscode-widget-border, rgba(128, 128, 128, 0.3));
    font-size: 12px;
    text-transform: uppercase;
    letter-spacing: 0.06em;
    color: var(--vscode-descriptionForeground);
  }
  .close-btn {
    background: transparent;
    border: none;
    color: var(--vscode-foreground);
    cursor: pointer;
    font-size: 12px;
  }
  .history-scroll {
    flex: 1;
    overflow-y: auto;
    padding: 4px 0;
    gap: 1px;
  }
  .empty-hint {
    padding: 24px 16px;
    color: var(--vscode-descriptionForeground);
    font-size: 12px;
    text-align: center;
  }
  .group-label {
    font-size: 11px;
    color: var(--vscode-descriptionForeground);
    text-transform: uppercase;
    letter-spacing: 0.06em;
    padding: 8px 12px 4px;
  }
  .history-row {
    display: flex;
    align-items: stretch;
    padding: 0 4px;
    margin-bottom: 1px;
  }
  .history-row.active .row-main {
    background: var(
      --vscode-list-activeSelectionBackground,
      rgba(128, 128, 128, 0.2)
    );
  }
  .row-main {
    flex: 1;
    display: flex;
    justify-content: space-between;
    align-items: center;
    gap: 8px;
    background: transparent;
    border: none;
    cursor: pointer;
    text-align: left;
    padding: 6px 8px;
    color: var(--vscode-foreground);
    border-radius: 3px;
  }
  .row-main:hover {
    background: var(--vscode-list-hoverBackground);
  }
  .row-title {
    flex: 1;
    font-size: 12px;
    overflow: hidden;
    text-overflow: ellipsis;
    white-space: nowrap;
    display: inline-flex;
    align-items: center;
    gap: 6px;
  }
  /* Pulsing crimson dot — same accent colour as the sidebar's
     active-tab underline, so a streaming chat in the history list
     reads as "live" without needing a label. Kept small and
     non-interactive; users still click the row to open it. */
  .streaming-dot {
    flex-shrink: 0;
    width: 7px;
    height: 7px;
    border-radius: 50%;
    background: #c8376a;
    animation: streaming-pulse 1.2s ease-in-out infinite;
  }
  @keyframes streaming-pulse {
    0%,
    100% {
      opacity: 0.35;
    }
    50% {
      opacity: 1;
    }
  }
  .row-meta {
    font-size: 10px;
    color: var(--vscode-descriptionForeground);
    flex-shrink: 0;
  }
  .row-delete {
    background: transparent;
    border: none;
    color: var(--vscode-descriptionForeground);
    cursor: pointer;
    padding: 0 8px;
    opacity: 0;
    border-radius: 3px;
  }
  .history-row:hover .row-delete {
    opacity: 0.7;
  }
  .row-delete:hover {
    opacity: 1;
    color: var(--vscode-errorForeground, #d7263d);
  }
</style>
