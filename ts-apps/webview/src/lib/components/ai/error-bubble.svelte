<script lang="ts">
  let {
    message,
    code,
    onRetry,
  }: {
    message: string
    code?: string
    onRetry?: () => void
  } = $props()

  const isQuota = $derived(
    code === 'daily_token_limit_exceeded' || code === 'rate_limited'
  )
</script>

<div class="err-bubble" role="alert">
  <div class="err-icon">⚠</div>
  <div class="err-body">
    <div class="err-message">{message}</div>
    {#if !isQuota && onRetry}
      <div class="err-actions">
        <button class="err-btn" onclick={onRetry}>Retry</button>
      </div>
    {/if}
  </div>
</div>

<style>
  .err-bubble {
    display: flex;
    gap: 8px;
    align-items: flex-start;
    color: var(--vscode-foreground);
    padding: 4px 0;
    margin: 6px 0;
    font-size: 12px;
  }
  .err-icon {
    color: var(--vscode-errorForeground, #d7263d);
    line-height: 1.2;
    font-size: 14px;
  }
  .err-body {
    flex: 1;
    display: flex;
    flex-direction: column;
    gap: 6px;
  }
  .err-message {
    line-height: 1.4;
  }
  .err-actions {
    display: flex;
    gap: 6px;
  }
  /* Match the chat's primary CTA (Submit / Deploy) crimson rather than
     VSCode's default theme button blue, so the retry action reads as
     part of the AI tab's visual family. */
  .err-btn {
    background: #c8376a;
    color: #fff;
    border: none;
    padding: 3px 10px;
    border-radius: 2px;
    cursor: pointer;
    font-size: 12px;
  }
  .err-btn:hover {
    background: #d94d7e;
  }
</style>
