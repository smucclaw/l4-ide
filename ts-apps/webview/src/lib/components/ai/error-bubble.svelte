<script lang="ts">
  let {
    message,
    code,
    onRetry,
    onSignIn,
  }: {
    message: string
    code?: string
    onRetry?: () => void
    onSignIn?: () => void
  } = $props()

  const isAuth = $derived(code === 'unauthenticated')
  const isQuota = $derived(
    code === 'daily_token_limit_exceeded' || code === 'rate_limited'
  )
</script>

<div class="err-bubble" role="alert">
  <div class="err-icon">⚠</div>
  <div class="err-body">
    <div class="err-message">{message}</div>
    <div class="err-actions">
      {#if isAuth && onSignIn}
        <button class="err-btn" onclick={onSignIn}>Sign in</button>
      {:else if !isQuota && onRetry}
        <button class="err-btn" onclick={onRetry}>Retry</button>
      {/if}
    </div>
  </div>
</div>

<style>
  .err-bubble {
    display: flex;
    gap: 8px;
    align-items: flex-start;
    background: var(
      --vscode-inputValidation-errorBackground,
      rgba(255, 90, 90, 0.08)
    );
    border: 1px solid
      var(--vscode-inputValidation-errorBorder, rgba(255, 90, 90, 0.4));
    color: var(--vscode-foreground);
    border-radius: 4px;
    padding: 8px 10px;
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
  .err-btn {
    background: var(--vscode-button-background);
    color: var(--vscode-button-foreground);
    border: none;
    padding: 3px 10px;
    border-radius: 2px;
    cursor: pointer;
    font-size: 12px;
  }
  .err-btn:hover {
    background: var(--vscode-button-hoverBackground);
  }
</style>
