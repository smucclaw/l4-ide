<script lang="ts">
  import { legalExamples, type LegalExample } from '$lib/legal-examples'

  interface Props {
    onExampleSelect: (example: LegalExample) => void
  }

  const { onExampleSelect }: Props = $props()
</script>

<div class="example-selector">
  <div class="button-grid">
    <button
      class="example-button external"
      onclick={() => window.open('https://legalese.com/l4', '_blank')}
    >
      Visit the L4 Documentation
    </button>
    <button
      class="example-button external"
      onclick={() =>
        window.open('https://github.com/smucclaw/l4-ide', '_blank')}
    >
      L4-IDE on Github
    </button>
    <h4>Load example files</h4>
    {#each legalExamples as example}
      <button class="example-button" onclick={() => onExampleSelect(example)}>
        {example.name}
      </button>
    {/each}
  </div>
  <div class="info-grid">
    L4 is a programming language designed specifically for translating legal
    documents into precise, executable code. Use this online-playground to
    tinker and share your L4 code.
  </div>
</div>

<style>
  .example-selector {
    padding: 1rem;
    font-family: 'Merriweather', Times, serif;
    color: rgb(30, 29, 28);
    height: 100%;
    background: rgba(250, 250, 249, 0.88);
    display: flex;
    flex-direction: column;
    gap: 0.5rem;
  }

  h4 {
    font-size: 0.75rem;
    font-weight: bold;
    text-transform: uppercase;
    margin-top: 1.5rem;
    margin-bottom: 0.5rem;
  }

  .button-grid {
    display: flex;
    flex-direction: column;
    gap: 0.5rem;
    flex: 1;
  }

  .example-button {
    color: rgba(30, 29, 28, 0.698);
    font-size: 0.9em;
    cursor: pointer;
    text-align: left;
    position: relative;
  }

  .example-button:hover {
    color: rgba(30, 29, 28);
    background: rgb(243, 242, 241);
    margin: -0.2em -0.5em;
    padding: 0.2em 0.5em;
    border-radius: 3px;
  }

  .example-button:active {
    color: rgba(30, 29, 28);
    background: rgb(243, 242, 241);
    margin: -0.2em -0.5em;
    padding: 0.2em 0.5em;
  }

  .example-button.external::after {
    content: '';
    display: inline-block;
    vertical-align: middle;
    margin-left: 0.5em;
    width: 0.75em;
    height: 0.75em;
    background: url('data:image/svg+xml;utf8,<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 512 512" fill="gray"><path d="M336 0c-8.8 0-16 7.2-16 16s7.2 16 16 16l121.4 0L212.7 276.7c-6.2 6.2-6.2 16.4 0 22.6s16.4 6.2 22.6 0L480 54.6 480 176c0 8.8 7.2 16 16 16s16-7.2 16-16l0-160c0-8.8-7.2-16-16-16L336 0zM64 32C28.7 32 0 60.7 0 96L0 448c0 35.3 28.7 64 64 64l352 0c35.3 0 64-28.7 64-64l0-144c0-8.8-7.2-16-16-16s-16 7.2-16 16l0 144c0 17.7-14.3 32-32 32L64 480c-17.7 0-32-14.3-32-32L32 96c0-17.7 14.3-32 32-32l144 0c8.8 0 16-7.2 16-16s-7.2-16-16-16L64 32z"/></svg>')
      no-repeat center center;
    background-size: contain;
    pointer-events: none;
  }

  .info-grid {
    padding: 0.3rem;
    color: rgba(30, 29, 28, 0.452);
    font-size: 0.8rem;
  }
</style>
