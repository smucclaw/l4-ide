<script lang="ts">
  import { tick } from 'svelte'
  import { marked, type Token } from 'marked'
  import type { Messenger } from 'vscode-messenger-webview'
  import { HOST_EXTENSION } from 'vscode-messenger-common'
  import { RequestNewL4File } from 'jl4-client-rpc'
  import { colorize } from '@repo/l4-highlight'

  let { messenger }: { messenger: InstanceType<typeof Messenger> | null } =
    $props()

  const DOCS_BASE = 'https://legalese.com/l4'
  const SITE_BASE = 'https://legalese.com'
  const SEARCH_INDEX_URL = `${SITE_BASE}/search-index.json`

  let history: string[] = $state([])
  let currentUrl: string = $state(`${DOCS_BASE}/README.md`)
  let html: string = $state('')
  let loading: boolean = $state(true)

  // Search state
  interface SearchEntry {
    title: string
    slug: string
    section?: string
    anchor?: string
    keywords: string[]
    excerpt: string
    breadcrumb: string[]
  }

  let searchIndex: SearchEntry[] = $state([])
  let searchQuery: string = $state('')
  let searchResults: SearchEntry[] = $state([])
  let searchFocused: boolean = $state(false)
  let selectedIndex: number = $state(0)

  marked.setOptions({ breaks: false, gfm: true })

  // Resolve URLs in the token tree before rendering (avoids <base> interference).
  // Internal docs links use data-href instead of href to prevent VSCode from
  // intercepting the click and opening the browser.
  function walkTokens(token: Token) {
    if ('href' in token && typeof token.href === 'string') {
      token.href = resolveUrl(token.href)
    }
  }

  // Custom renderer
  const renderer = new marked.Renderer()
  renderer.heading = ({ text, depth, tokens }) => {
    const id = text
      .toLowerCase()
      .replace(/[^\w\s-]/g, '')
      .replace(/\s+/g, '-')
      .replace(/^-+|-+$/g, '')
    const inner = renderer.parser!.parseInline(tokens)
    return `<h${depth} id="${id}">${inner}</h${depth}>`
  }
  renderer.link = ({ href, title, tokens }) => {
    const resolved = resolveUrl(href)
    const titleAttr = title ? ` title="${title}"` : ''
    // Render child tokens to get the inner HTML (handles nested images etc.)
    const inner = renderer.parser!.parseInline(tokens)
    if (resolved.startsWith(DOCS_BASE)) {
      // Internal link: use data-href to prevent VSCode from opening the browser
      return `<a data-href="${resolved}"${titleAttr} class="doc-link">${inner}</a>`
    }
    return `<a href="${resolved}"${titleAttr}>${inner}</a>`
  }
  let codeBlockId = 0
  renderer.code = ({ text, lang }) => {
    if (lang === 'l4' || lang === 'jl4' || lang === 'l4-file') {
      const id = `l4-code-${codeBlockId++}`
      const copyBtn =
        lang === 'l4-file'
          ? `<button class="new-file-btn" data-code-id="${id}" title="Copy into new L4 file">Copy into new file</button>`
          : ''
      return `<div class="code-block-wrapper">${copyBtn}<pre><code id="${id}" class="language-l4">${colorize(text)}</code></pre></div>`
    }
    const escaped = text
      .replace(/&/g, '&amp;')
      .replace(/</g, '&lt;')
      .replace(/>/g, '&gt;')
    const cls = lang ? ` class="language-${lang}"` : ''
    return `<pre><code${cls}>${escaped}</code></pre>`
  }

  function resolveUrl(href: string): string {
    if (href.startsWith('http://') || href.startsWith('https://')) return href
    const base = currentUrl.replace(/\/[^/]*$/, '/')
    return new URL(href, base).toString()
  }

  async function loadPage(url: string) {
    loading = true
    try {
      // Strip anchor for fetching
      const fetchUrl = url.split('#')[0]
      const resp = await fetch(fetchUrl)
      if (!resp.ok) {
        html = `<p>Failed to load: ${resp.status}</p>`
        return
      }
      const markdown = await resp.text()
      html = await marked.parse(markdown, { renderer, walkTokens })
    } catch (err) {
      html = `<p>Could not load documentation: ${err instanceof Error ? err.message : String(err)}</p>`
    } finally {
      loading = false
    }

    // Scroll to anchor if present — must happen after loading = false
    // so the {#if loading} block has swapped to show the rendered HTML
    const hashIdx = url.indexOf('#')
    if (hashIdx !== -1) {
      const anchor = url.slice(hashIdx + 1)
      await tick()
      requestAnimationFrame(() => {
        document
          .getElementById(anchor)
          ?.scrollIntoView({ behavior: 'smooth', block: 'start' })
      })
    }
  }

  async function loadSearchIndex() {
    if (searchIndex.length > 0) return
    try {
      const resp = await fetch(SEARCH_INDEX_URL)
      if (!resp.ok) return
      const parsed = (await resp.json()) as { entries: SearchEntry[] }
      searchIndex = parsed.entries
    } catch {
      // Search won't work but docs still load
    }
  }

  function highlightMatches(text: string, query: string): string {
    if (!query.trim()) return escapeHtml(text)
    const terms = query
      .toLowerCase()
      .split(/\s+/)
      .filter((t) => t.length >= 2)
    if (terms.length === 0) return escapeHtml(text)
    const pattern = terms
      .map((t) => t.replace(/[.*+?^${}()|[\]\\]/g, '\\$&'))
      .join('|')
    const regex = new RegExp(`(${pattern})`, 'gi')
    return escapeHtml(text).replace(regex, '<mark>$1</mark>')
  }

  function escapeHtml(text: string): string {
    return text
      .replace(/&/g, '&amp;')
      .replace(/</g, '&lt;')
      .replace(/>/g, '&gt;')
  }

  function doSearch(query: string) {
    if (!query.trim()) {
      searchResults = []
      return
    }
    const queryLower = query.toLowerCase().trim()
    const queryTerms = queryLower.split(/\s+/).filter((t) => t.length >= 2)
    if (queryTerms.length === 0) {
      if (queryLower.length >= 2) queryTerms.push(queryLower)
      else {
        searchResults = []
        return
      }
    }

    type ScoredEntry = SearchEntry & { score: number }
    const scored: ScoredEntry[] = []

    for (const entry of searchIndex) {
      let score = 0
      const titleLower = entry.title.toLowerCase()
      const sectionLower = (entry.section ?? '').toLowerCase()
      const excerptLower = entry.excerpt.toLowerCase()

      // Title match (highest weight)
      if (titleLower.includes(queryLower)) score += 100
      else if (queryTerms.some((t) => titleLower.includes(t))) score += 50

      // Section match (high weight)
      if (sectionLower.includes(queryLower)) score += 80
      else if (queryTerms.some((t) => sectionLower.includes(t))) score += 40

      // Keywords match (medium weight)
      const matchedKw = entry.keywords.filter((kw) =>
        queryTerms.some(
          (t) => kw.toLowerCase().includes(t) || t.includes(kw.toLowerCase())
        )
      )
      if (matchedKw.length > 0) score += matchedKw.length * 10

      // Excerpt match (lower weight)
      if (excerptLower.includes(queryLower)) score += 20
      else if (queryTerms.some((t) => excerptLower.includes(t))) score += 10

      // Boost exact matches
      if (titleLower === queryLower || sectionLower === queryLower) score += 200

      // Slight penalty for section entries
      if (entry.section && score > 0) score *= 0.9

      if (score > 0) scored.push({ ...entry, score })
    }

    // Sort by match rank, then score, then alphabetical
    const getDisplayName = (e: ScoredEntry) =>
      (e.section || e.title).toLowerCase()
    const getMatchRank = (e: ScoredEntry): number => {
      const dn = getDisplayName(e)
      if (dn === queryLower) return 6
      if (
        dn.startsWith(queryLower + ' ') ||
        dn.startsWith(queryLower + ':') ||
        dn.startsWith(queryLower + '-')
      )
        return 5
      if (dn.startsWith(queryLower)) return 4
      if (dn.endsWith(' ' + queryLower)) return 2
      try {
        if (new RegExp(`\\b${queryLower}\\b`).test(dn)) return 1
      } catch {
        /* invalid regex from special chars */
      }
      return 0
    }

    scored.sort((a, b) => {
      const aRank = getMatchRank(a)
      const bRank = getMatchRank(b)
      if (aRank !== bRank) return bRank - aRank
      if (b.score !== a.score) return b.score - a.score
      return (a.section || a.title).localeCompare(b.section || b.title)
    })

    searchResults = scored.slice(0, 12)
    selectedIndex = 0
  }

  function navigateToEntry(entry: SearchEntry) {
    const url = entry.anchor
      ? `${DOCS_BASE}/${entry.slug}.md#${entry.anchor}`
      : `${DOCS_BASE}/${entry.slug}.md`
    history = [...history, currentUrl]
    currentUrl = url
    searchQuery = ''
    searchResults = []
    searchFocused = false
    loadPage(currentUrl)
  }

  function handleSearchKeydown(e: KeyboardEvent) {
    if (searchResults.length === 0) return
    if (e.key === 'ArrowDown') {
      e.preventDefault()
      selectedIndex = Math.min(selectedIndex + 1, searchResults.length - 1)
    } else if (e.key === 'ArrowUp') {
      e.preventDefault()
      selectedIndex = Math.max(selectedIndex - 1, 0)
    } else if (e.key === 'Enter') {
      e.preventDefault()
      if (searchResults[selectedIndex])
        navigateToEntry(searchResults[selectedIndex])
    } else if (e.key === 'Escape') {
      searchQuery = ''
      searchResults = []
    }
  }

  function navigateTo(url: string) {
    history = [...history, currentUrl]
    currentUrl = url
    loadPage(currentUrl)
  }

  function handleClick(e: MouseEvent) {
    // Handle "New file" button on l4-file code blocks
    const newFileBtn = (e.target as HTMLElement).closest('.new-file-btn')
    if (newFileBtn) {
      e.preventDefault()
      const codeId = newFileBtn.getAttribute('data-code-id')
      if (codeId) {
        const codeEl = document.getElementById(codeId)
        if (codeEl) {
          messenger?.sendNotification(RequestNewL4File, HOST_EXTENSION, {
            content: codeEl.textContent ?? '',
          })
        }
      }
      return
    }

    const target = (e.target as HTMLElement).closest('a')
    if (!target) return

    // Internal docs links use data-href (no href, so VSCode won't intercept)
    const dataHref = target.getAttribute('data-href')
    if (dataHref) {
      e.preventDefault()
      navigateTo(dataHref)
      return
    }

    // External links: let VSCode open them in the browser (default behavior)
  }

  function goBack() {
    if (history.length === 0) return
    const prev = history[history.length - 1]
    history = history.slice(0, -1)
    currentUrl = prev
    loadPage(currentUrl)
  }

  function goHome() {
    const homeUrl = `${DOCS_BASE}/README.md`
    if (currentUrl !== homeUrl) {
      history = [...history, currentUrl]
      currentUrl = homeUrl
      loadPage(currentUrl)
    }
  }

  let initialized = false
  $effect(() => {
    if (!initialized) {
      initialized = true
      loadPage(currentUrl)
      loadSearchIndex()
    }
  })

  $effect(() => {
    doSearch(searchQuery)
  })
</script>

<!-- svelte-ignore a11y_click_events_have_key_events -->
<!-- svelte-ignore a11y_no_static_element_interactions -->
<div class="docs-panel">
  <div class="toolbar">
    {#if history.length > 0}
      <button class="back-btn" onclick={goBack} title="Back">&larr;</button>
    {/if}
    <button class="home-btn" onclick={goHome} title="Home">
      <svg width="14" height="14" viewBox="0 0 16 16" fill="currentColor"
        ><path d="M8 1.1L1 7v8h5v-4h4v4h5V7L8 1.1z" /></svg
      >
    </button>
    <div class="search-bar">
      <input
        class="search-input"
        type="text"
        placeholder="Search docs..."
        bind:value={searchQuery}
        onfocus={() => {
          searchFocused = true
          loadSearchIndex()
        }}
        onblur={() => setTimeout(() => (searchFocused = false), 200)}
        onkeydown={handleSearchKeydown}
      />
      {#if searchFocused && (searchResults.length > 0 || searchQuery.trim())}
        <div class="search-results">
          {#if searchQuery.trim() && searchResults.length === 0}
            <div class="search-empty">No results for "{searchQuery}"</div>
          {/if}
          {#each searchResults as entry, idx}
            <button
              class="search-result"
              class:selected={idx === selectedIndex}
              onmousedown={() => navigateToEntry(entry)}
              onmouseenter={() => (selectedIndex = idx)}
            >
              <span class="result-title">
                {#if entry.section}
                  {@html highlightMatches(entry.section, searchQuery)}
                {:else}
                  {@html highlightMatches(entry.title, searchQuery)}
                {/if}
              </span>
              <span class="result-breadcrumb">
                {entry.breadcrumb.slice(0, -1).join(' › ')}
                {#if entry.section}
                  <span class="result-breadcrumb-sep"> › {entry.title}</span>
                {/if}
              </span>
              {#if entry.excerpt}
                <span class="result-excerpt"
                  >{@html highlightMatches(entry.excerpt, searchQuery)}</span
                >
              {/if}
            </button>
          {/each}
        </div>
      {/if}
    </div>
  </div>

  {#if loading}
    <div class="loading">Loading...</div>
  {:else}
    <div class="markdown-body" onclick={handleClick}>
      {@html html}
    </div>
  {/if}
</div>

<style>
  .docs-panel {
    padding: 0;
  }

  .toolbar {
    display: flex;
    align-items: center;
    gap: 6px;
    margin-bottom: 8px;
  }

  .back-btn {
    background: none;
    border: 1px solid var(--vscode-input-border, #555);
    border-radius: 3px;
    color: var(--vscode-foreground);
    cursor: pointer;
    font-size: 0.92em;
    padding: 4px 8px;
    flex-shrink: 0;
  }

  .back-btn:hover,
  .home-btn:hover {
    background: var(--vscode-list-hoverBackground, #2a2d2e);
  }

  .home-btn {
    background: none;
    border: 1px solid var(--vscode-input-border, #555);
    border-radius: 3px;
    color: var(--vscode-descriptionForeground);
    cursor: pointer;
    padding: 4px 6px;
    flex-shrink: 0;
    display: flex;
    align-items: center;
  }

  .home-btn:hover {
    color: var(--vscode-foreground);
  }

  .search-bar {
    position: relative;
    flex: 1;
    min-width: 0;
  }

  .search-input {
    width: 100%;
    box-sizing: border-box;
    padding: 5px 8px;
    font-size: 0.92em;
    background: var(--vscode-input-background, #3c3c3c);
    color: var(--vscode-input-foreground, #ccc);
    border: 1px solid var(--vscode-input-border, #555);
    border-radius: 3px;
    outline: none;
  }

  .search-input:focus {
    border-color: var(--vscode-foreground, #ccc);
  }

  .search-results {
    position: absolute;
    top: 100%;
    left: 0;
    right: 0;
    background: var(--vscode-menu-background, #252526);
    border: 1px solid var(--vscode-menu-border, #454545);
    border-radius: 4px;
    box-shadow: 0 2px 8px rgba(0, 0, 0, 0.3);
    z-index: 100;
    max-height: 300px;
    overflow-y: auto;
  }

  .search-empty {
    padding: 10px;
    text-align: center;
    font-size: 0.88em;
    color: var(--vscode-descriptionForeground);
  }

  .search-result {
    display: flex;
    flex-direction: column;
    gap: 2px;
    width: 100%;
    padding: 8px 12px;
    text-align: left;
    background: none;
    border: none;
    color: var(--vscode-menu-foreground, #ccc);
    cursor: pointer;
    border-bottom: 1px solid var(--vscode-menu-separatorBackground, #454545);
  }

  .search-result:last-child {
    border-bottom: none;
  }

  .search-result:hover,
  .search-result.selected {
    background: var(--vscode-menu-selectionBackground, #04395e);
    color: var(--vscode-menu-selectionForeground, #fff);
  }

  .result-title {
    font-size: 0.92em;
    font-weight: 500;
  }

  .result-breadcrumb {
    font-size: 0.78em;
    color: var(--vscode-descriptionForeground);
  }

  .result-breadcrumb-sep {
    opacity: 0.6;
  }

  .search-result:hover .result-breadcrumb,
  .search-result.selected .result-breadcrumb {
    color: var(--vscode-menu-selectionForeground, #fff);
    opacity: 0.7;
  }

  .result-excerpt {
    font-size: 0.82em;
    color: var(--vscode-descriptionForeground);
    display: -webkit-box;
    line-clamp: 2;
    -webkit-line-clamp: 2;
    -webkit-box-orient: vertical;
    overflow: hidden;
  }

  .search-result:hover .result-excerpt,
  .search-result.selected .result-excerpt {
    color: var(--vscode-menu-selectionForeground, #fff);
    opacity: 0.8;
  }

  :global(mark) {
    background: var(
      --vscode-editor-findMatchHighlightBackground,
      rgba(234, 92, 0, 0.33)
    );
    color: inherit;
    border-radius: 2px;
    padding: 0 1px;
  }

  .loading {
    color: var(--vscode-descriptionForeground);
    padding: 20px 0;
    text-align: center;
  }

  .markdown-body {
    font-size: var(--vscode-font-size, 13px);
    line-height: 1.6;
    color: var(--vscode-foreground);
    word-wrap: break-word;
    padding: 0 2px 1em 2px;
  }

  .markdown-body :global(h1) {
    font-size: 1.4em;
    font-weight: 600;
    margin: 16px 0 8px;
    padding-bottom: 4px;
    border-bottom: 1px solid var(--vscode-panel-border, #444);
  }

  .markdown-body :global(h2) {
    font-size: 1.2em;
    font-weight: 600;
    margin: 14px 0 6px;
  }

  .markdown-body :global(h3) {
    font-size: 1.05em;
    font-weight: 600;
    margin: 12px 0 4px;
  }

  .markdown-body :global(p) {
    margin: 8px 0;
  }

  .markdown-body :global(a),
  .markdown-body :global(.doc-link) {
    color: #c8376a;
    text-decoration: none;
    cursor: pointer;
  }

  .markdown-body :global(a:hover),
  .markdown-body :global(.doc-link:hover) {
    text-decoration: underline;
  }

  .markdown-body :global(code) {
    font-family: var(--vscode-editor-font-family, monospace);
    font-size: 0.9em;
    padding: 1px 4px;
    border-radius: 3px;
    background: var(--vscode-editor-background, #1e1e1e);
    color: var(--l4-tok-identifier, #4ec9b0);
  }

  .markdown-body :global(pre) {
    background: var(--vscode-editor-background, #1e1e1e);
    border: 1px solid var(--vscode-panel-border, #444);
    border-radius: 4px;
    padding: 10px 12px;
    overflow-x: auto;
    margin: 8px 0;
  }

  .markdown-body :global(pre code) {
    padding: 0;
    background: none;
    color: var(--vscode-foreground);
    font-size: 0.88em;
    line-height: 1.5;
  }

  .markdown-body :global(.code-block-wrapper) {
    position: relative;
  }

  .markdown-body :global(.new-file-btn) {
    position: absolute;
    top: 6px;
    right: 6px;
    font-size: 0.78em;
    padding: 2px 8px;
    border-radius: 3px;
    border: 1px solid var(--vscode-panel-border, #444);
    background: var(--vscode-sideBarSectionHeader-background, #252526);
    color: var(--vscode-descriptionForeground);
    cursor: pointer;
    opacity: 0;
    transition: opacity 0.15s;
  }

  .markdown-body :global(.code-block-wrapper:hover .new-file-btn) {
    opacity: 1;
  }

  .markdown-body :global(.new-file-btn:hover) {
    color: var(--vscode-foreground);
    border-color: var(--vscode-focusBorder, #007fd4);
  }

  .markdown-body :global(ul),
  .markdown-body :global(ol) {
    padding-left: 20px;
    margin: 6px 0;
  }

  .markdown-body :global(li) {
    margin: 3px 0;
  }

  .markdown-body :global(blockquote) {
    border-left: 3px solid var(--vscode-panel-border, #444);
    margin: 8px 0;
    padding: 4px 12px;
    color: var(--vscode-descriptionForeground);
  }

  .markdown-body :global(table) {
    border-collapse: collapse;
    width: 100%;
    margin: 8px 0;
  }

  .markdown-body :global(th),
  .markdown-body :global(td) {
    border: 1px solid var(--vscode-panel-border, #444);
    padding: 4px 8px;
    font-size: 0.92em;
  }

  .markdown-body :global(th) {
    background: var(--vscode-sideBarSectionHeader-background, #252526);
    font-weight: 600;
  }

  .markdown-body :global(hr) {
    border: none;
    border-top: 1px solid var(--vscode-panel-border, #444);
    margin: 12px 0;
  }

  .markdown-body :global(img) {
    max-width: 100%;
    height: auto;
  }

  .markdown-body :global(img[src$='l4.svg']) {
    max-width: 160px;
    max-height: 160px;
  }

  .markdown-body :global(strong) {
    font-weight: 600;
  }
</style>
