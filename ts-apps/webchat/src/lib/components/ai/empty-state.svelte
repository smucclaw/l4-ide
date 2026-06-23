<script lang="ts">
  let {
    deployment = null,
  }: {
    /** The deployment this chat is bound to. webchat is always deployment-
     *  scoped, so the empty state always shows this info box. */
    deployment?: {
      deploymentId: string
      intendedUse: string
    } | null
  } = $props()

  // Fallback when the deployment has no "Intended use" metadata set.
  const FALLBACK_INTENDED_USE =
    'Ask questions regarding the rules in this deployment or attach files that should be checked for compliance'

  const intendedUseText = $derived(
    deployment?.intendedUse?.trim()
      ? deployment.intendedUse.trim()
      : FALLBACK_INTENDED_USE
  )
</script>

<div class="empty-state">
  <div class="deployment-info" role="note">
    <p class="info-heading">Intended Use</p>
    <p class="info-body">{intendedUseText}</p>
  </div>
</div>

<style>
  .empty-state {
    display: flex;
    flex-direction: column;
    align-items: center;
    justify-content: center;
    gap: 12px;
    flex: 1;
    padding: 24px 16px;
  }
  .deployment-info {
    width: 100%;
    max-width: 320px;
    box-sizing: border-box;
    padding: 14px 16px;
  }
  .info-heading {
    margin: 0 0 8px;
    color: var(--vscode-descriptionForeground);
    font-size: 12px;
    text-transform: uppercase;
    letter-spacing: 0.06em;
  }
  .dep-id {
    /* Keep the deployment id verbatim (no uppercasing) and monospaced. */
    text-transform: none;
    letter-spacing: 0;
    font-family: var(
      --vscode-editor-font-family,
      ui-monospace,
      SFMono-Regular,
      Menlo,
      monospace
    );
    font-size: 12px;
  }
  .info-body {
    margin: 0;
    color: var(--vscode-descriptionForeground);
    /* A shade more transparent than the "Intended use" title so the
       heading stays the visual anchor. */
    opacity: 0.75;
    font-size: 13px;
    line-height: 1.5;
    white-space: pre-wrap;
  }
</style>
