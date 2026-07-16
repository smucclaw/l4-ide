<script lang="ts">
  // Thin bar shown directly above the chat input while the active
  // conversation is bound to a deployment ("Use in chat" from the
  // Deployment tab). Closing it (✕) opens a fresh conversation against
  // the default Legalese AI endpoint again.
  let {
    deploymentId,
    onOpenInBrowser,
    onClose,
  }: {
    deploymentId: string
    /** Open this deployment's chat on chat.legalese.cloud in the user's
     *  external browser. */
    onOpenInBrowser: () => void
    onClose: () => void
  } = $props()
</script>

<div class="deployment-banner" role="status">
  <span class="label">
    Chatting with <span class="dep-id">{deploymentId}</span>
  </span>
  <button
    class="open-btn"
    onclick={onOpenInBrowser}
    title="Open this deployment chat in your browser"
    aria-label="Open this deployment chat in your browser"
    >Open in browser</button
  >
  <button
    class="close-btn"
    onclick={onClose}
    title="Stop chatting with this deployment"
    aria-label="Stop chatting with this deployment">✕</button
  >
</div>

<style>
  .deployment-banner {
    display: flex;
    align-items: center;
    gap: 8px;
    padding: 5px 8px 10px 10px;
    margin-bottom: -4px;
    border-radius: 6px 6px 0 0;
    font-size: 0.85em;
    /* The crimson primary action color (matching Deploy / Submit),
       heavily knocked back so it reads as a subtle on-brand tint
       rather than a solid accent bar. */
    background: color-mix(in srgb, #c8376a 18%, transparent);
    color: var(--vscode-foreground);
  }

  .label {
    flex: 1;
    min-width: 0;
    overflow: hidden;
    text-overflow: ellipsis;
    white-space: nowrap;
  }

  .dep-id {
    font-weight: 600;
  }

  /* Bold text-only action (no icon / chrome) sitting just before the
     close ✕. */
  .open-btn {
    background: none;
    border: none;
    color: var(--vscode-foreground);
    cursor: pointer;
    font: inherit;
    font-weight: 600;
    flex-shrink: 0;
    padding: 0 2px;
    line-height: 1;
    opacity: 0.85;
  }

  .open-btn:hover {
    opacity: 1;
  }

  .close-btn {
    background: none;
    border: none;
    color: var(--vscode-foreground);
    cursor: pointer;
    opacity: 0.6;
    font-size: 0.95em;
    flex-shrink: 0;
    padding: 0 2px;
    line-height: 1;
  }

  .close-btn:hover {
    opacity: 1;
  }
</style>
