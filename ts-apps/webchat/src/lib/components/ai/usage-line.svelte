<script lang="ts" module>
  function formatCount(n: number): string {
    if (n >= 1_000_000) return `${(n / 1_000_000).toFixed(1)}M`
    if (n >= 1_000) return `${(n / 1_000).toFixed(1)}K`
    return String(n)
  }
</script>

<script lang="ts">
  let {
    used,
    limit,
  }: {
    used: number
    limit: number
  } = $props()

  const ratio = $derived(limit > 0 ? Math.min(used / limit, 1.2) : 0)
  const fillPct = $derived(Math.min(ratio, 1) * 100)
  const fillClass = $derived(ratio >= 1 ? 'over' : ratio >= 0.9 ? 'warn' : 'ok')
</script>

<div
  class="usage-line {fillClass}"
  title={limit > 0
    ? `${formatCount(used)} / ${formatCount(limit)} tokens today`
    : 'Usage data unavailable'}
>
  <div class="fill" style="width: {fillPct}%"></div>
</div>

<style>
  .usage-line {
    position: relative;
    height: 1px;
    background: var(--vscode-widget-border, rgba(128, 128, 128, 0.25));
    overflow: hidden;
  }
  .fill {
    height: 100%;
    transition: width 0.5s ease-out;
  }
  .ok .fill {
    background: var(--vscode-foreground);
    opacity: 0.35;
  }
  .warn .fill {
    background: #d3a03d;
    opacity: 0.7;
  }
  .over .fill {
    background: var(--vscode-errorForeground, #d7263d);
    opacity: 0.85;
  }
</style>
