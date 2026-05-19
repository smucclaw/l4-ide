<script lang="ts">
  /**
   * Deployment-level metadata editor. Currently a single field — the
   * mission / role description used to ground the AI assistant for a
   * deployment — but structured as its own screen so more
   * per-deployment configuration can land here later.
   *
   * Deliberately standalone: it owns only the metadata fields (and an
   * optional Back affordance), never the primary action button. The
   * host wires navigation + the commit action, so this can be mounted
   * inside the deploy flow or opened on its own from elsewhere in the
   * extension. It does NOT fetch any remote deployment state — the
   * caller passes whatever initial value it wants shown.
   */
  interface Props {
    /** Two-way bound mission/description text. */
    mission: string
    /** Shown in the sub-heading for context when present. */
    deploymentId?: string
    heading?: string
    /** When provided, a Back button is rendered and calls this. */
    onBack?: () => void
    /** When provided, a "Generate" button is rendered under the input.
     *  Resolves to the drafted text, or null when generation was
     *  unavailable (e.g. the host already surfaced a sign-in nudge) —
     *  in which case the field is left untouched. */
    onGenerate?: () => Promise<string | null>
  }

  let {
    mission = $bindable(''),
    deploymentId,
    heading = 'Deployment metadata',
    onBack,
    onGenerate,
  }: Props = $props()

  let generating = $state(false)

  async function runGenerate() {
    if (generating || !onGenerate) return
    generating = true
    try {
      const text = await onGenerate()
      if (text) mission = text
    } finally {
      generating = false
    }
  }
</script>

<div class="deployment-metadata">
  {#if onBack}
    <button class="back-btn" onclick={onBack}>&larr; Change deployment</button>
  {/if}

  <div class="screen-heading">
    {heading}
    {#if deploymentId}
      <span class="screen-heading-sub">for {deploymentId}</span>
    {/if}
  </div>

  <div class="form-group">
    <label class="form-label" for="deployment-intended-use">Intended use</label>
    <textarea
      id="deployment-intended-use"
      class="form-input form-textarea"
      bind:value={mission}
      rows="8"
      maxlength="4000"
      placeholder="Explain how the deployed rules are to be used (<4000 chars)"
    ></textarea>
    {#if onGenerate}
      <button
        class="generate-btn"
        disabled={generating}
        onclick={runGenerate}
        title="Draft a description from the deployed function schemas"
      >
        {#if generating}
          <span class="spinner" aria-hidden="true"></span>
          Generating…
        {:else}
          ✨ Generate
        {/if}
      </button>
    {/if}
  </div>
</div>

<style>
  .deployment-metadata {
    padding: 4px 0;
    font-size: 1em;
    line-height: 1.2;
  }

  .back-btn {
    background: none;
    border: none;
    color: var(--vscode-descriptionForeground);
    cursor: pointer;
    font-size: 0.88em;
    padding: 2px 0 6px;
    margin-bottom: 8px;
  }

  .back-btn:hover {
    color: var(--vscode-foreground);
  }

  .screen-heading {
    font-size: 0.92em;
    font-weight: 600;
    color: var(--vscode-foreground);
    margin-bottom: 10px;
  }

  .screen-heading-sub {
    font-weight: 400;
    color: var(--vscode-descriptionForeground);
  }

  .form-group {
    margin-bottom: 10px;
  }

  .form-label {
    display: block;
    font-size: 0.85em;
    text-transform: uppercase;
    letter-spacing: 0.05em;
    color: var(--vscode-descriptionForeground);
    opacity: 0.7;
    margin-bottom: 4px;
  }

  .form-input {
    width: 100%;
    box-sizing: border-box;
    padding: 4px 6px;
    background: var(--vscode-input-background, #3c3c3c);
    color: var(--vscode-input-foreground, #ccc);
    border: 1px solid var(--vscode-input-border, #555);
    border-radius: 3px;
    outline: none;
  }

  .form-input:focus {
    border-color: var(--vscode-foreground, #ccc);
  }

  .form-textarea {
    font-family: inherit;
    font-size: inherit;
    line-height: 1.4;
    resize: vertical;
    min-height: 112px;
  }

  .generate-btn {
    margin-top: 6px;
    display: inline-flex;
    align-items: center;
    gap: 6px;
    background: none;
    border: 1px solid var(--vscode-input-border, #555);
    color: var(--vscode-descriptionForeground);
    cursor: pointer;
    font-size: 0.85em;
    padding: 3px 10px;
    border-radius: 3px;
  }

  .spinner {
    width: 10px;
    height: 10px;
    border: 2px solid var(--vscode-descriptionForeground);
    border-top-color: transparent;
    border-radius: 50%;
    animation: spin 0.7s linear infinite;
  }

  @keyframes spin {
    to {
      transform: rotate(360deg);
    }
  }

  .generate-btn:hover:not(:disabled) {
    color: var(--vscode-foreground);
    border-color: var(--vscode-foreground, #ccc);
  }

  .generate-btn:disabled {
    opacity: 0.6;
    cursor: default;
  }
</style>
