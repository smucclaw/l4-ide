<script lang="ts">
  import { tick, untrack } from 'svelte'
  import type { DirectiveResult, SrcPos } from 'jl4-client-rpc'
  import type * as Monaco from '@codingame/monaco-vscode-editor-api'

  interface Props {
    monaco?: typeof Monaco
    onRevealSource?: (fileUri: string, line: number) => void
  }
  let { monaco, onRevealSource }: Props = $props()

  type ColorizedEntry = { header: string; body: string; contentKey: string }
  let colorized: Record<string, ColorizedEntry> = $state({})

  /**
   * Format a result value string by converting commas into line-breaks with
   * indentation based on parenthesis nesting depth. Parentheses that wrap
   * record values (e.g. `(Foo WITH a = 1, b = 2)`) are stripped.
   */
  function formatResultValue(text: string): string {
    let result = ''
    let depth = 0
    const indent = () => '\n' + '  '.repeat(depth)

    for (let i = 0; i < text.length; i++) {
      const ch = text[i]
      if (ch === '(') {
        depth++
      } else if (ch === ')') {
        depth = Math.max(0, depth - 1)
      } else if (ch === ',') {
        if (text[i + 1] === ' ') i++
        result += indent()
      } else if (text.startsWith(' WITH ', i)) {
        depth++
        result += ' WITH'
        result += indent()
        i += 5
      } else {
        result += ch
      }
    }
    return result
  }

  $effect(() => {
    if (!monaco) return
    const m = monaco
    const toColorize = sections.map((s) => ({
      id: s.directiveId,
      lineContent: s.lineContent,
      prettyText: s.prettyText,
      formattedPretty: formatResultValue(s.prettyText),
      contentKey: `${s.lineContent}\0${s.prettyText}`,
    }))
    // Read colorized without tracking to avoid a reactive cycle
    // (the effect only re-runs when `sections` changes, not when `colorized` changes)
    const current = untrack(() => colorized)
    for (const { id, lineContent, formattedPretty, contentKey } of toColorize) {
      if (current[id]?.contentKey === contentKey) continue
      const truncated = lineContent.trim()
      Promise.all([
        m.editor.colorize(truncated, 'jl4', { tabSize: 2 }),
        m.editor.colorize(formattedPretty, 'jl4', { tabSize: 2 }),
      ]).then(([header, body]) => {
        colorized = {
          ...colorized,
          [id]: {
            header: header.replace(/<br\s*\/?>\s*$/, ''),
            body,
            contentKey,
          },
        }
      })
    }
    // Prune stale entries without triggering a re-run
    const liveIds = new Set(toColorize.map((t) => t.id))
    const staleKeys = Object.keys(current).filter((id) => !liveIds.has(id))
    if (staleKeys.length > 0) {
      const pruned = { ...current }
      for (const id of staleKeys) delete pruned[id]
      colorized = pruned
    }
  })

  export interface ResultSection {
    renderKey: number
    directiveId: string
    fileUri: string
    prettyText: string
    success: boolean | null
    structuredValue: unknown | null
    srcLine: number
    srcColumn: number
    lineContent: string
    collapsed: boolean
    stale: boolean
  }

  let nextRenderKey = 0
  let sections: ResultSection[] = $state([])
  let collapsedFiles: Set<string> = $state(new Set())

  // ---------------------------------------------------------------------------
  // File grouping
  // ---------------------------------------------------------------------------

  function displayFileName(uri: string): string {
    const decoded = decodeURIComponent(uri.replace(/^file:\/\//, ''))
    return decoded.split('/').pop() ?? uri
  }

  type FileGroup = {
    fileUri: string
    displayName: string
    sections: ResultSection[]
  }

  let fileGroups: FileGroup[] = $derived.by(() => {
    const groups = new Map<string, ResultSection[]>()
    for (const s of sections) {
      const existing = groups.get(s.fileUri)
      if (existing) existing.push(s)
      else groups.set(s.fileUri, [s])
    }
    return Array.from(groups, ([fileUri, sects]) => ({
      fileUri,
      displayName: displayFileName(fileUri),
      sections: sects,
    }))
  })

  let hasMultipleFiles: boolean = $derived(fileGroups.length > 1)

  function toggleFileCollapse(fileUri: string) {
    const next = new Set(collapsedFiles)
    if (next.has(fileUri)) next.delete(fileUri)
    else next.add(fileUri)
    collapsedFiles = next
  }

  // ---------------------------------------------------------------------------
  // Section management
  // ---------------------------------------------------------------------------

  export function clear() {
    sections = []
  }

  export function addOrScrollToResult(
    directiveId: string,
    srcPos: SrcPos,
    result: DirectiveResult,
    lineContent: string,
    fileUri: string = ''
  ): 'ok' | 'scrolled' {
    const existing = sections.find((s) => s.directiveId === directiveId)
    if (existing) {
      if (existing.stale) {
        // Re-activate the stale section with fresh data
        sections = sections.map((s) =>
          s.directiveId === directiveId
            ? {
                ...s,
                prettyText: result.prettyText,
                success: result.success,
                structuredValue: result.structuredValue,
                srcLine: srcPos.line,
                srcColumn: srcPos.column,
                lineContent,
                stale: false,
              }
            : s
        )
      }
      tick().then(() => {
        const el = document.getElementById(`section-${directiveId}`)
        el?.scrollIntoView({ behavior: 'smooth', block: 'nearest' })
        el?.classList.add('highlight-flash')
        setTimeout(() => el?.classList.remove('highlight-flash'), 600)
      })
      return 'scrolled'
    }

    const newSection: ResultSection = {
      renderKey: nextRenderKey++,
      directiveId,
      fileUri,
      prettyText: result.prettyText,
      success: result.success,
      structuredValue: result.structuredValue,
      srcLine: srcPos.line,
      srcColumn: srcPos.column,
      lineContent,
      collapsed: false,
      stale: false,
    }

    const idx = sections.findIndex(
      (s) =>
        s.fileUri.localeCompare(newSection.fileUri) > 0 ||
        (s.fileUri === newSection.fileUri &&
          (s.srcLine > newSection.srcLine ||
            (s.srcLine === newSection.srcLine &&
              s.srcColumn > newSection.srcColumn)))
    )
    if (idx === -1) {
      sections = [...sections, newSection]
    } else {
      sections = [...sections.slice(0, idx), newSection, ...sections.slice(idx)]
    }

    tick().then(() => {
      const el = document.getElementById(`section-${directiveId}`)
      el?.scrollIntoView({ behavior: 'smooth', block: 'nearest' })
    })

    return 'ok'
  }

  export function updateSection(
    directiveId: string,
    prettyText: string,
    success: boolean | null
  ) {
    sections = sections.map((s) =>
      s.directiveId === directiveId ? { ...s, prettyText, success } : s
    )
  }

  /**
   * Sync open sections against a fresh batch of results from the LSP.
   *
   * When `uri` is provided, only sections belonging to that file are synced;
   * sections from other files are left untouched.
   *
   * Pass 1 – content-based matching.
   * Pass 2 – positional fallback.
   * Unmatched sections become stale.
   */
  export function syncSections(
    results: Array<{
      directiveId: string
      prettyText: string
      success: boolean | null
      lineContent: string
    }>,
    uri?: string
  ) {
    // Scope: only sync sections belonging to the notified file
    const toSync = uri ? sections.filter((s) => s.fileUri === uri) : sections
    const untouched = uri ? sections.filter((s) => s.fileUri !== uri) : []

    if (results.length === 0) {
      sections = [
        ...untouched,
        ...toSync.map((s) => ({
          ...s,
          success: null as boolean | null,
          stale: true,
        })),
      ]
      return
    }

    const parseLine = (id: string) => parseInt(id.split(':')[0], 10)
    const parseCol = (id: string) => parseInt(id.split(':')[1], 10)

    type RemapEntry = {
      newId: string
      newLine: number
      newCol: number
      prettyText: string
      success: boolean | null
      lineContent: string
    }
    const remappings = new Map<string, RemapEntry>()
    const matchedResultIds = new Set<string>()

    // Pass 1 (PRIMARY): content-based matching
    const sectionsByContent = Map.groupBy(toSync, (s) => s.lineContent)
    const resultsByContent = Map.groupBy(results, (r) => r.lineContent)

    for (const [content, sects] of sectionsByContent) {
      const matchingResults = resultsByContent.get(content)
      if (!matchingResults) continue

      if (sects.length === 1 && matchingResults.length === 1) {
        const r = matchingResults[0]
        remappings.set(sects[0].directiveId, {
          newId: r.directiveId,
          newLine: parseLine(r.directiveId),
          newCol: parseCol(r.directiveId),
          prettyText: r.prettyText,
          success: r.success,
          lineContent: r.lineContent,
        })
        matchedResultIds.add(r.directiveId)
      } else {
        const usedResultIds = new Set<string>()
        for (const sect of sects) {
          let best: (typeof matchingResults)[0] | null = null
          let bestDist = Infinity
          for (const result of matchingResults) {
            if (usedResultIds.has(result.directiveId)) continue
            const dist = Math.abs(sect.srcLine - parseLine(result.directiveId))
            if (dist < bestDist) {
              bestDist = dist
              best = result
            }
          }
          if (best !== null) {
            usedResultIds.add(best.directiveId)
            matchedResultIds.add(best.directiveId)
            remappings.set(sect.directiveId, {
              newId: best.directiveId,
              newLine: parseLine(best.directiveId),
              newCol: parseCol(best.directiveId),
              prettyText: best.prettyText,
              success: best.success,
              lineContent: best.lineContent,
            })
          }
        }
      }
    }

    // Pass 2 (FALLBACK): positional match by directiveId (same line:col)
    for (const s of toSync) {
      if (s.stale || remappings.has(s.directiveId)) continue
      const positionalMatch = results.find(
        (r) =>
          r.directiveId === s.directiveId &&
          !matchedResultIds.has(r.directiveId)
      )
      if (positionalMatch) {
        remappings.set(s.directiveId, {
          newId: positionalMatch.directiveId,
          newLine: parseLine(positionalMatch.directiveId),
          newCol: parseCol(positionalMatch.directiveId),
          prettyText: positionalMatch.prettyText,
          success: positionalMatch.success,
          lineContent: positionalMatch.lineContent,
        })
        matchedResultIds.add(positionalMatch.directiveId)
      }
    }

    // Apply remappings; unmatched sections become stale
    const synced = toSync.map((s) => {
      const remap = remappings.get(s.directiveId)
      if (remap) {
        return {
          ...s,
          directiveId: remap.newId,
          srcLine: remap.newLine,
          srcColumn: remap.newCol,
          prettyText: remap.prettyText,
          success: remap.success,
          lineContent: remap.lineContent,
          stale: false,
        }
      }
      return { ...s, success: null as boolean | null, stale: true }
    })

    sections = [...untouched, ...synced].sort(
      (a, b) =>
        a.fileUri.localeCompare(b.fileUri) ||
        a.srcLine - b.srcLine ||
        a.srcColumn - b.srcColumn
    )
  }

  function removeSection(directiveId: string) {
    sections = sections.filter((s) => s.directiveId !== directiveId)
  }

  function removeFileGroup(fileUri: string) {
    sections = sections.filter((s) => s.fileUri !== fileUri)
  }

  function toggleCollapse(directiveId: string) {
    sections = sections.map((s) =>
      s.directiveId === directiveId ? { ...s, collapsed: !s.collapsed } : s
    )
  }

  function successClass(success: boolean | null): string {
    if (success === true) return 'success'
    if (success === false) return 'failure'
    return ''
  }
</script>

<div class="inspector-panel">
  {#if sections.length === 0}
    <div class="empty-state">
      <p class="hint">
        Click "Track result" above an #EVAL, #EVALTRACE, #CHECK, or #ASSERT
        directive to add it here
      </p>
    </div>
  {:else}
    {#each fileGroups as group (group.fileUri)}
      <div
        class="file-group"
        class:collapsed={hasMultipleFiles && collapsedFiles.has(group.fileUri)}
      >
        {#if hasMultipleFiles}
          <div class="file-header">
            <button
              class="file-header-toggle"
              onclick={() => toggleFileCollapse(group.fileUri)}
              title={collapsedFiles.has(group.fileUri)
                ? 'Expand file'
                : 'Collapse file'}
            >
              <span
                class="chevron"
                class:rotated={!collapsedFiles.has(group.fileUri)}>&#9002;</span
              >
              <span class="file-name" title={group.fileUri}
                >{group.displayName}</span
              >
            </button>
            <span class="file-result-count">
              {group.sections.length} result{group.sections.length !== 1
                ? 's'
                : ''}
            </span>
            <button
              class="remove-all-btn"
              onclick={(e: MouseEvent) => {
                e.stopPropagation()
                removeFileGroup(group.fileUri)
              }}
              title="Remove all results from this file"
            >
              Remove all
            </button>
          </div>
        {/if}

        {#if !hasMultipleFiles || !collapsedFiles.has(group.fileUri)}
          {#each group.sections as section (section.renderKey)}
            <div
              id="section-{section.directiveId}"
              class="result-section {successClass(section.success)}"
            >
              <div class="section-header">
                <button
                  class="section-header-toggle"
                  onclick={(e: MouseEvent) => {
                    if ((e.metaKey || e.ctrlKey) && onRevealSource)
                      onRevealSource(section.fileUri, section.srcLine)
                    else toggleCollapse(section.directiveId)
                  }}
                  title={onRevealSource
                    ? (section.collapsed ? 'Expand' : 'Collapse') +
                      ' (⌘/Ctrl-click to reveal source)'
                    : section.collapsed
                      ? 'Expand'
                      : 'Collapse'}
                >
                  <span class="chevron" class:rotated={!section.collapsed}
                    >&#9002;</span
                  >
                  {#if !section.stale}
                    <span class="source-location">{section.srcLine}:</span>
                  {/if}
                  <span
                    class="directive-label"
                    title={section.lineContent.trim()}
                  >
                    {#if colorized[section.directiveId]}
                      {@html colorized[section.directiveId].header}
                    {:else}
                      {section.lineContent.trim()}
                    {/if}
                  </span>
                </button>
                <button
                  class="dismiss-btn"
                  onclick={() => removeSection(section.directiveId)}
                  title="Remove this result"
                >
                  &#10005;
                </button>
              </div>

              {#if !section.collapsed}
                <div class="section-body">
                  {#if colorized[section.directiveId]}
                    <pre class="result-code">{@html colorized[
                        section.directiveId
                      ].body}</pre>
                  {:else}
                    <pre class="result-code">{formatResultValue(
                        section.prettyText
                      )}</pre>
                  {/if}
                </div>
              {/if}
            </div>
          {/each}
        {/if}
      </div>
    {/each}
  {/if}
</div>

<style>
  .inspector-panel {
    font-family: 'Merriweather', Times, serif;
    font-size: 13px;
    color: #1e1d1c;
    background: #fafaf9;
    padding: 8px;
    overflow-y: auto;
    height: 100%;
  }

  .empty-state {
    display: flex;
    flex-direction: column;
    align-items: center;
    justify-content: center;
    height: 60%;
    opacity: 0.6;
    text-align: center;
  }

  .empty-state .hint {
    font-size: 0.9em;
    line-height: 1.2em;
    margin-top: 4px;
    opacity: 0.7;
    max-width: 200px;
  }

  .file-group {
    display: flex;
    flex-direction: column;
    gap: 8px;
    margin-bottom: 8px;
  }

  .file-group.collapsed {
    margin-bottom: 0;
  }

  .file-header {
    display: flex;
    align-items: center;
    background: #e8e7e6;
    border: 1px solid #ddd;
    border-radius: 4px;
    user-select: none;
    font-size: 0.85em;
    font-weight: 500;
  }

  .file-header-toggle {
    display: flex;
    align-items: center;
    gap: 6px;
    flex: 1;
    min-width: 0;
    padding: 4px 8px;
    background: none;
    border: none;
    color: inherit;
    cursor: pointer;
    font-family: inherit;
    font-size: inherit;
    font-weight: inherit;
    text-align: left;
  }

  .file-name {
    overflow: hidden;
    text-overflow: ellipsis;
    white-space: nowrap;
    flex: 1;
    min-width: 0;
  }

  .file-result-count {
    font-size: 0.85em;
    color: #777;
    opacity: 0.8;
    flex-shrink: 0;
    padding: 0 4px;
  }

  .remove-all-btn {
    background: none;
    border: none;
    color: #1e1d1c;
    cursor: pointer;
    opacity: 0.5;
    font-size: 0.9em;
    padding: 4px 8px;
    flex-shrink: 0;
  }

  .remove-all-btn:hover {
    opacity: 1;
  }

  .result-section {
    border: 1px solid #ddd;
    border-left-width: 3px;
    border-radius: 4px;
    overflow: hidden;
  }

  .result-section.success {
    border-left: 3px solid #75beff;
  }

  .result-section.failure {
    border-left: 3px solid #f14c4c;
  }

  .section-header {
    display: flex;
    align-items: center;
    background: #f0efee;
    user-select: none;
    min-width: 0;
  }

  .section-header:hover {
    background: #e8e7e6;
  }

  .section-header-toggle {
    display: flex;
    align-items: center;
    gap: 6px;
    flex: 1;
    min-width: 0;
    padding: 3px 4px 3px 8px;
    background: none;
    border: none;
    color: inherit;
    cursor: pointer;
    font-family: inherit;
    font-size: inherit;
    text-align: left;
  }

  .chevron {
    display: inline-block;
    color: #777;
    transition: transform 0.15s;
    transform-origin: 25% 50%;
    margin: 0 -3px 0 1px;
  }

  .chevron.rotated {
    transform: rotate(90deg);
  }

  .directive-label {
    font-family: monospace;
    font-size: 0.82em;
    color: #333;
    white-space: nowrap;
    overflow: hidden;
    text-overflow: ellipsis;
    flex: 1;
    min-width: 0;
  }

  .source-location {
    font-family: monospace;
    font-size: 0.78em;
    color: #777;
    flex-shrink: 0;
  }

  /* Ensure Monaco colorize() token spans aren't reset by parent color */
  .directive-label :global(span[class^='mtk']),
  .result-code :global(span[class^='mtk']) {
    font-family: inherit;
  }

  .dismiss-btn {
    margin-left: auto;
    background: none;
    border: none;
    color: #1e1d1c;
    cursor: pointer;
    opacity: 0.5;
    font-size: 12px;
    padding: 0 4px;
    flex-shrink: 0;
  }

  .dismiss-btn:hover {
    opacity: 1;
  }

  .section-body {
    max-height: 40vh;
    overflow-y: auto;
    padding: 8px 12px;
    background: #fff;
  }

  .result-code {
    margin: 0;
    white-space: pre-wrap;
    word-break: break-word;
    font-family: monospace;
    font-size: 13px;
    line-height: 1.5;
    tab-size: 2;
  }

  :global(.highlight-flash) {
    animation: flash 0.6s ease-out;
  }

  @keyframes flash {
    0% {
      background: rgba(234, 92, 0, 0.33);
    }
    100% {
      background: transparent;
    }
  }
</style>
