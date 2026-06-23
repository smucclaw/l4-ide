<script lang="ts">
  // Thin accent bar shown directly above the chat input naming the
  // deployment this chat is bound to. The webchat is always
  // deployment-scoped, so (unlike the IDE webview) there's nothing to
  // close — the bar is purely informational and always present.
  let {
    deploymentId,
  }: {
    deploymentId: string
  } = $props()
</script>

<div class="deployment-banner" role="status">
  <span class="label">
    Chatting with <span class="dep-id">{deploymentId}</span>
  </span>
</div>

<style>
  .deployment-banner {
    box-sizing: border-box;
    width: 100%;
    max-width: 800px;
    margin: 0 0.75rem -6px;
    /* Tuck the flat bottom edge behind the input's rounded top so the
       two read as one unit. */
    padding: 6px 12px 12px;
    border-radius: 12px 12px 0 0;
    font-size: 0.75em;
    /* The action color, heavily knocked back so it reads as a subtle
       tint that adapts to the theme rather than a solid accent bar. */
    background: color-mix(
      in srgb,
      var(--vscode-button-background) 18%,
      transparent
    );
    color: var(--vscode-foreground);
  }

  .label {
    display: block;
    min-width: 0;
    overflow: hidden;
    text-overflow: ellipsis;
    white-space: nowrap;
  }

  .dep-id {
    font-weight: 600;
  }
</style>
