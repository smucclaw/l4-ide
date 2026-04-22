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
  // the webview CSS can style them consistently. Every code block is
  // wrapped in a `.code-block-wrap` so we can position a Copy
  // button in its top-right. Click handling is delegated on the
  // container — marked's output is injected via @html and can't
  // bind Svelte handlers directly.
  const renderer = new marked.Renderer()
  renderer.code = ({ text: code, lang }) => {
    const highlighted =
      lang === 'l4' || lang === 'l4-file'
        ? `<code class="language-l4">${colorize(code)}</code>`
        : `<code${lang ? ` class="language-${lang}"` : ''}>${code
            .replace(/&/g, '&amp;')
            .replace(/</g, '&lt;')
            .replace(/>/g, '&gt;')}</code>`
    // Store the raw (un-escaped, un-highlighted) source on a data
    // attribute so the copy handler yields the exact text the
    // assistant produced — not the HTML-entity-expanded version
    // and not the highlighter's colorized markup.
    const encodedSource = encodeURIComponent(code)
    return `<div class="code-block-wrap"><pre>${highlighted}</pre><button type="button" class="code-copy-btn" data-copy-src="${encodedSource}" title="Copy code" aria-label="Copy code"><svg viewBox="0 0 16 16" width="12" height="12" aria-hidden="true" fill="none" stroke="currentColor" stroke-width="1.5"><rect x="4" y="4" width="9" height="9" rx="1.5"/><path d="M4 10H3V3.5A1.5 1.5 0 0 1 4.5 2H10"/></svg><span class="code-copy-label">Copy</span></button></div>`
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

  // Delegated click handler for `.code-copy-btn` buttons injected by
  // the marked renderer. We can't bind Svelte handlers through
  // `{@html}`, so one listener on the outer div catches every
  // button click and reads the source from its data attribute.
  // Shows a 1.5s "Copied" state so the user has visual confirmation.
  function onCopyClick(e: MouseEvent): void {
    const target = e.target as HTMLElement | null
    const btn = target?.closest?.('.code-copy-btn') as HTMLButtonElement | null
    if (!btn) return
    e.preventDefault()
    const encoded = btn.dataset.copySrc
    if (!encoded) return
    const source = decodeURIComponent(encoded)
    void navigator.clipboard.writeText(source).then(
      () => {
        btn.classList.add('code-copy-btn-done')
        const label = btn.querySelector('.code-copy-label')
        const prior = label ? label.textContent : null
        if (label) label.textContent = 'Copied'
        setTimeout(() => {
          btn.classList.remove('code-copy-btn-done')
          if (label && prior !== null) label.textContent = prior
        }, 1500)
      },
      () => {
        // Clipboard write denied — unusual in a VSCode webview.
        // Silently drop; the user can still select + Cmd+C.
      }
    )
  }
</script>

<!-- svelte-ignore a11y_click_events_have_key_events -->
<!-- svelte-ignore a11y_no_static_element_interactions -->
<div class="streaming-md" onclick={onCopyClick}>
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
     sidebar surface. Wrapped in `.code-block-wrap` so a Copy button
     can sit in the corner without escaping the `<pre>`'s scroll
     container. */
  .streaming-md :global(.code-block-wrap) {
    position: relative;
    margin: 8px 0;
  }
  .streaming-md :global(.code-block-wrap pre) {
    margin: 0;
  }
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
  /* Copy button — bottom-right of the code block. Faded until the
     wrap is hovered so it doesn't compete with the code for
     attention, then firms up on hover. Enters a green "Copied"
     state for 1.5s after a successful write. */
  .streaming-md :global(.code-copy-btn) {
    position: absolute;
    right: 6px;
    bottom: 6px;
    display: inline-flex;
    align-items: center;
    gap: 4px;
    padding: 2px 6px;
    font-size: 10px;
    line-height: 1;
    font-family: var(--vscode-font-family, sans-serif);
    color: var(--vscode-descriptionForeground);
    background: var(--vscode-sideBar-background, rgba(0, 0, 0, 0.2));
    border: 1px solid var(--vscode-widget-border, rgba(128, 128, 128, 0.4));
    border-radius: 3px;
    cursor: pointer;
    opacity: 0;
    transition:
      opacity 0.12s ease-out,
      color 0.12s ease-out,
      border-color 0.12s ease-out;
  }
  .streaming-md :global(.code-block-wrap:hover .code-copy-btn),
  .streaming-md :global(.code-copy-btn:focus-visible) {
    opacity: 0.9;
  }
  .streaming-md :global(.code-copy-btn:hover) {
    opacity: 1;
    color: var(--vscode-foreground);
    border-color: var(--vscode-foreground);
  }
  .streaming-md :global(.code-copy-btn-done) {
    opacity: 1 !important;
    color: #78c47c;
    border-color: #78c47c;
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
