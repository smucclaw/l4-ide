<script lang="ts">
  import { legalExamples, type LegalExample } from '$lib/legal-examples'

  interface Props {
    onExampleSelect: (example: LegalExample) => void
  }

  const { onExampleSelect }: Props = $props()
</script>

<div class="example-selector">
  <h3>Examples</h3>
  <div class="button-grid">
    {#each legalExamples as example}
      <button class="example-button" onclick={() => onExampleSelect(example)}>
        {example.name}
      </button>
    {/each}
  </div>
</div>

<style>
  .example-selector {
    padding: 1rem;
  }

  .button-grid {
    display: flex;
    flex-direction: column;
    gap: 0.5rem;
  }

  .example-button {
    padding: 0.5rem 1rem;
    border: 1px solid #ccc;
    background: white;
    cursor: pointer;
    text-align: left;
  }

  .example-button:hover {
    background: #f5f5f5;
  }
</style>
