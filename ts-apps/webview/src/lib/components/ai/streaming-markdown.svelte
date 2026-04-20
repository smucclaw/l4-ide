<script lang="ts">
  import { marked } from 'marked'
  import markedKatex from 'marked-katex-extension'
  import 'katex/dist/katex.min.css'
  import { colorize } from '@repo/l4-highlight'

  // Wire KaTeX through marked so `$…$` / `$$…$$` in an assistant
  // response renders as math instead of literal dollar signs. `throwOnError`
  // stays false so a typo in the expression degrades to the raw source
  // in a red span rather than ejecting the whole render pipeline.
  marked.use(markedKatex({ throwOnError: false }))

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
    if (lang === 'l4' || lang === 'l4-file') {
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

  // Match the in-flight block's geometry to whatever element marked
  // will produce once this slice commits, so the content doesn't
  // visibly settle a few pixels when the cut advances. Only a handful
  // of block kinds are worth detecting here; everything else renders
  // as a paragraph by default.
  const trailingKind = $derived<'code' | 'paragraph'>(
    /^\s*```/.test(trailing) ? 'code' : 'paragraph'
  )
</script>

<div class="streaming-md">
  {@html committedHtml}
  {#if trailing}
    <div class="in-flight in-flight-{trailingKind}">
      {trailing}<span class="cursor">▋</span>
    </div>
  {/if}
</div>

<style>
  /* Fenced code block: thin border panel, no filled background.
     Matches the ToolCard card style so prose + code sit on the same
     sidebar surface. */
  .streaming-md :global(pre) {
    background: var(--vscode-editor-background);
    padding: 8px 10px;
    border: 1px solid var(--vscode-panel-border, #444);
    border-radius: 4px;
    overflow-x: auto;
    margin: 8px 0;
    font-size: 12px;
    line-height: 1.45;
  }
  .streaming-md :global(pre code) {
    background: none;
    padding: 0;
    color: inherit;
    font-family: var(--vscode-editor-font-family, monospace);
    font-size: inherit;
  }
  /* Inline code: transparent background, slightly under body-font
     size, and the teal identifier tone used by the Docs tab. Fenced
     blocks override this with `color: inherit` via .streaming-md
     :global(pre code) above so the highlighter can pick colors. */
  .streaming-md :global(code) {
    background: transparent;
    padding: 0;
    font-family: var(--vscode-editor-font-family, monospace);
    font-size: 0.92em;
    color: var(--l4-tok-identifier, #4ec9b0);
  }
  .streaming-md :global(p) {
    margin: 8px 0 10px;
  }
  /* Headings. Browser defaults wildly overshoot for a sidebar-scale
     chat: 2em for h1 on top of 21px default line-height visually
     breaks the conversation flow. Scale everything to the body font
     with modest top/bottom breathing room, tapering h3+ to near-body
     weight so nested outlines stay readable in a narrow column.
     First-child collapse kills the extra top margin when a heading
     opens an assistant message. */
  .streaming-md :global(h1),
  .streaming-md :global(h2),
  .streaming-md :global(h3),
  .streaming-md :global(h4),
  .streaming-md :global(h5),
  .streaming-md :global(h6) {
    color: var(--vscode-foreground);
    font-weight: 600;
    line-height: 1.25;
    margin: 14px 0 6px;
  }
  .streaming-md :global(h1:first-child),
  .streaming-md :global(h2:first-child),
  .streaming-md :global(h3:first-child),
  .streaming-md :global(h4:first-child),
  .streaming-md :global(h5:first-child),
  .streaming-md :global(h6:first-child) {
    margin-top: 0;
  }
  .streaming-md :global(h1) {
    font-size: 1.25em;
  }
  .streaming-md :global(h2) {
    font-size: 1.15em;
  }
  .streaming-md :global(h3) {
    font-size: 1.05em;
  }
  .streaming-md :global(h4),
  .streaming-md :global(h5),
  .streaming-md :global(h6) {
    font-size: 1em;
  }
  .streaming-md :global(ul),
  .streaming-md :global(ol) {
    margin: 8px 0 10px;
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
    margin: 8px 0 10px;
  }
  /* Horizontal rule: the default 1px solid line reads heavier than a
     prose break needs in a narrow sidebar. Swap to a half-opacity
     widget-border hairline with generous margin so `---` feels like a
     gentle pause rather than a visual block. */
  .streaming-md :global(hr) {
    border: none;
    border-top: 1px solid var(--vscode-widget-border, rgba(128, 128, 128, 0.35));
    opacity: 0.5;
    margin: 14px 0;
  }
  .streaming-md :global(th),
  .streaming-md :global(td) {
    border: 1px solid var(--vscode-widget-border, rgba(128, 128, 128, 0.35));
    padding: 4px 8px;
  }

  /* In-flight block: mirrors the paragraph or pre geometry that marked
     will emit once the cut advances, so committing doesn't shift the
     content downward by the margin/padding delta. */
  .in-flight {
    white-space: pre-wrap;
    word-break: break-word;
    color: inherit;
  }
  .in-flight-paragraph {
    font-family: inherit;
    background: transparent;
    padding: 0;
    margin: 8px 0 10px;
  }
  .in-flight-code {
    font-family: var(--vscode-editor-font-family, monospace);
    font-size: 12px;
    line-height: 1.45;
    background: var(--vscode-editor-background);
    border: 1px solid var(--vscode-panel-border, #444);
    border-radius: 4px;
    padding: 8px 10px;
    margin: 8px 0;
    overflow-x: auto;
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
