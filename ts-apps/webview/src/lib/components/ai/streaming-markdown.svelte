<script lang="ts">
  import { marked } from 'marked'
  import { colorize } from '@repo/l4-highlight'

  let { text, streaming }: { text: string; streaming: boolean } = $props()

  /**
   * Block-by-block streaming: while tokens are still arriving, split
   * the buffer at the last "safe" newline-newline boundary and only
   * parse the committed prefix. The trailing in-flight block renders
   * as plain preformatted text with a blinking cursor until its
   * enclosing block closes, at which point the cut moves forward and
   * that block is committed. No flicker from half-parsed code fences
   * or lists.
   */
  function findSafeCut(s: string): number {
    // If we're inside an open ``` fence, cut before it.
    const fenceMatches = s.match(/```/g)
    const fenceCount = fenceMatches?.length ?? 0
    if (fenceCount % 2 === 1) {
      const lastFenceStart = s.lastIndexOf('```')
      const safeBefore = s.lastIndexOf('\n\n', lastFenceStart)
      return safeBefore > 0 ? safeBefore + 2 : 0
    }
    // Otherwise the last \n\n is a safe cut point (preceded by a
    // complete block). If we never saw one, nothing committed yet.
    const idx = s.lastIndexOf('\n\n')
    return idx > 0 ? idx + 2 : 0
  }

  const committed = $derived(
    streaming ? text.slice(0, findSafeCut(text)) : text
  )
  const trailing = $derived(streaming ? text.slice(committed.length) : '')

  // Custom renderer: L4 code via @repo/l4-highlight (matches the Docs
  // and Inspector tabs); other code blocks render as plain HTML so
  // the webview CSS can style them consistently.
  const renderer = new marked.Renderer()
  renderer.code = ({ text: code, lang }) => {
    if (lang === 'l4' || lang === 'jl4' || lang === 'l4-file') {
      return `<pre><code class="language-l4">${colorize(code)}</code></pre>`
    }
    const escaped = code
      .replace(/&/g, '&amp;')
      .replace(/</g, '&lt;')
      .replace(/>/g, '&gt;')
    const cls = lang ? ` class="language-${lang}"` : ''
    return `<pre><code${cls}>${escaped}</code></pre>`
  }
  // Links open externally — but the webview host intercepts clicks on
  // http(s) links via VSCode; keep default rendering.
  marked.setOptions({ breaks: false, gfm: true })

  const committedHtml = $derived(
    (marked.parse(committed, { renderer, async: false }) as string) || ''
  )
</script>

<div class="streaming-md">
  {@html committedHtml}
  {#if trailing}
    <pre class="in-flight">{trailing}<span class="cursor">▋</span></pre>
  {/if}
</div>

<style>
  .streaming-md :global(pre) {
    background: var(--vscode-textCodeBlock-background);
    padding: 8px 10px;
    border-radius: 4px;
    overflow-x: auto;
    margin: 8px 0;
    font-size: 12px;
    line-height: 1.45;
  }
  .streaming-md :global(code) {
    font-family: var(--vscode-editor-font-family, monospace);
  }
  .streaming-md :global(p) {
    margin: 4px 0 10px;
  }
  .streaming-md :global(ul),
  .streaming-md :global(ol) {
    margin: 4px 0 10px;
    padding-left: 20px;
  }
  .streaming-md :global(li) {
    margin: 2px 0;
  }
  .streaming-md :global(a) {
    color: var(--vscode-textLink-foreground);
    text-decoration: none;
  }
  .streaming-md :global(a:hover) {
    text-decoration: underline;
  }
  .streaming-md :global(table) {
    border-collapse: collapse;
    margin: 6px 0 10px;
  }
  .streaming-md :global(th),
  .streaming-md :global(td) {
    border: 1px solid var(--vscode-widget-border, rgba(128, 128, 128, 0.35));
    padding: 4px 8px;
  }

  .in-flight {
    white-space: pre-wrap;
    font-family: inherit;
    background: none;
    padding: 0;
    margin: 0 0 8px;
  }

  .cursor {
    display: inline-block;
    margin-left: 1px;
    animation: blink 0.9s steps(1) infinite;
  }

  @keyframes blink {
    50% {
      opacity: 0;
    }
  }
</style>
