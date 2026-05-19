<script lang="ts">
  // Thin bar shown directly above the chat input while the active
  // conversation is bound to a deployment ("Use in chat" from the
  // Deployment tab). Closing it (✕) opens a fresh conversation against
  // the default Legalese AI endpoint again.
  let {
    deploymentId,
    onClose,
  }: {
    deploymentId: string
    onClose: () => void
  } = $props()
</script>

<div class="deployment-banner" role="status">
  <span class="label">
    Chatting with <span class="dep-id">{deploymentId}</span>
  </span>
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
    padding: 4px 8px 10px 10px;
    margin-bottom: -4px;
    border-radius: 6px 6px 0 0;
    font-size: 0.85em;
    /* The VSCode action color, heavily knocked back so it reads as a
       subtle tint that darkens or lightens with the user's theme
       rather than a solid accent bar. */
    background: color-mix(
      in srgb,
      var(--vscode-button-background) 18%,
      transparent
    );
    color: var(--vscode-foreground);
    border-top: 1px solid var(--vscode-panel-border, transparent);
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
