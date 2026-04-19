<script lang="ts">
  let {
    onSeed,
  }: {
    /** Invoked with the pre-filled text for the selected seed. The
     * parent drops it into the textarea and optionally triggers a file
     * picker for seeds that need one. */
    onSeed: (seed: {
      prompt: string
      needsFile: 'text-or-pdf' | 'spreadsheet' | null
    }) => void
  } = $props()

  const seeds: Array<{
    label: string
    prompt: string
    needsFile: 'text-or-pdf' | 'spreadsheet' | null
  }> = [
    {
      label: 'Turn my policy document into L4 for use as MCP',
      prompt:
        'Turn this policy document into precise and easily auditable L4 rules for use as an MCP tool with the goal to allow an AI agent to check compliance.',
      needsFile: 'text-or-pdf',
    },
    {
      label: 'Convert my spreadsheet into REST APIs I can use as webhooks',
      prompt:
        'Convert this spreadsheet into REST API endpoints I can use as webhooks.',
      needsFile: 'spreadsheet',
    },
    {
      label:
        'Help me convert a legal text to L4 and create example scenarios challenging the rules',
      prompt:
        'Help me convert a legal text to L4 and create example scenarios challenging the rules.',
      needsFile: null,
    },
  ]
</script>

<div class="empty-state">
  <p class="empty-heading">Get started</p>
  <div class="seed-buttons">
    {#each seeds as seed}
      <button class="seed-button" type="button" onclick={() => onSeed(seed)}>
        {seed.label}
      </button>
    {/each}
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
    border: 1px solid var(--vscode-widget-border, rgba(128, 128, 128, 0.35));
    border-radius: 4px;
    padding: 10px 12px;
    font-size: 12px;
    line-height: 1.4;
    cursor: pointer;
  }
  .seed-button:hover {
    background: var(--vscode-list-hoverBackground);
    border-color: var(--vscode-focusBorder, rgba(128, 128, 128, 0.55));
  }
</style>
