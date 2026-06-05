<script lang="ts">
  let {
    onSeed,
    deployment = null,
  }: {
    /** Invoked with the pre-filled text for the selected seed. The
     * parent drops it into the textarea and triggers the file picker
     * — every seed expects a source document. */
    onSeed: (seed: { prompt: string }) => void
    /** Set when the chat is bound to a deployment ("Use in chat").
     *  In that mode we replace the generic "Get started" seeds with
     *  an info box describing what this deployment is for. */
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
    <p class="info-heading">
      Intended Use for <code class="dep-id">{deployment?.deploymentId}</code>
    </p>
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
  .empty-heading {
    margin: 0;
    color: var(--vscode-descriptionForeground);
    font-size: 12px;
    text-transform: uppercase;
    letter-spacing: 0.06em;
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
  .seed-buttons {
    display: flex;
    flex-direction: column;
    gap: 8px;
    width: 100%;
    max-width: 320px;
  }
  .seed-button {
    width: 100%;
    text-align: left;
    background: transparent;
    color: var(--vscode-foreground);
    border: 1px solid rgba(128, 128, 128, 0.3);
    border-radius: 4px;
    padding: 10px 12px;
    font-size: 12px;
    line-height: 1.4;
    cursor: pointer;
    transition:
      border-color 0.12s ease-out,
      background-color 0.12s ease-out;
  }
  .seed-button:hover {
    background: rgba(128, 128, 128, 0.08);
    border-color: rgba(180, 180, 180, 0.6);
  }
  .seed-button:focus,
  .seed-button:focus-visible {
    outline: none;
    border-color: rgba(200, 200, 200, 0.75);
  }
</style>
