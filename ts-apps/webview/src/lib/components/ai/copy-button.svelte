<script lang="ts">
  let { getText }: { getText: () => string } = $props()
  let copied = $state(false)

  async function copy(): Promise<void> {
    const text = getText()
    if (!text) return
    try {
      await navigator.clipboard.writeText(text)
      copied = true
      setTimeout(() => (copied = false), 1200)
    } catch {
      // Webviews can restrict clipboard in rare cases. Fail silently;
      // the user can select + copy manually.
    }
  }
</script>

<button
  class="copy-btn"
  onclick={copy}
  title={copied ? 'Copied' : 'Copy'}
  aria-label="Copy to clipboard"
>
  {copied ? '✓' : '⧉'}
</button>

<style>
  .copy-btn {
    background: transparent;
    border: 1px solid transparent;
    color: var(--vscode-descriptionForeground);
    font-size: 12px;
    padding: 2px 6px;
    border-radius: 3px;
    cursor: pointer;
    opacity: 0;
    transition: opacity 0.12s ease-in-out;
  }
  :global(*:hover) > .copy-btn,
  .copy-btn:focus {
    opacity: 0.8;
  }
  .copy-btn:hover {
    opacity: 1;
    border-color: var(--vscode-widget-border, rgba(128, 128, 128, 0.35));
  }
</style>
