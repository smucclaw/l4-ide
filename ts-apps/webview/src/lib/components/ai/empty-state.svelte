<script lang="ts">
  let {
    onSeed,
  }: {
    /** Invoked with the pre-filled text for the selected seed. The
     * parent drops it into the textarea and triggers the file picker
     * — every seed expects a source document. */
    onSeed: (seed: { prompt: string }) => void
  } = $props()

  // Each seed: (1) opens a file picker for the user's source document,
  // (2) drops a pre-crafted prompt into the input so the only thing
  // left for the user to do is click ↑.
  const seeds: Array<{
    label: string
    prompt: string
  }> = [
    {
      label:
        'Turn my policy document into precise L4 to use as MCP tools for AI agents to check their compliance',
      prompt:
        'Turn the attached policy document into precise, auditable L4 rules and expose them as MCP tools so an AI agent can check compliance against them. Cover every decision the policy makes; annotate each rule with a @desc citing the clause it encodes; mark the top-level decision functions with @export so they ship as MCP tools; deploy the result so the tools are immediately callable.',
    },
    {
      label:
        'Convert my investment contract into accurate L4 and create scenarios for a next investment round',
      prompt:
        'Convert the attached investment contract into accurate L4, faithfully modelling the economic terms (valuation, cap table changes, dilution, pro-rata, liquidation preferences, anti-dilution). Annotate each rule with a @desc pointing at the contract clause. Then create a set of test scenarios for a plausible next investment round I can play around with — vary round size, pre-money valuation and new-investor stake — and include L4 expressions I can evaluate to see how outcomes shift across the scenarios.',
    },
    {
      label:
        'Create L4 APIs from my business requirement spec document so my website forms can verify against it',
      prompt:
        'Read the attached business requirement specification and create a well-tested L4 API / WebMCP endpoint that my website forms can POST against to verify user input. Derive the input schema and the validation rules from the spec, mark the verification function with @export so it ships as both a REST endpoint and a WebMCP tool, write test scenarios covering the happy path plus the edge cases the spec calls out, and run them with l4__evaluate so I can see they pass before deploying.',
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
