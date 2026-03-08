<script lang="ts">
  import { tick } from 'svelte'
  import type { DirectiveResult, SrcPos } from 'jl4-client-rpc'

  export interface ResultSection {
    directiveId: string
    directiveType: string
    prettyText: string
    success: boolean | null
    structuredValue: unknown | null
    srcLine: number
    srcColumn: number
    lineContent: string
    collapsed: boolean
  }

  let sections: ResultSection[] = $state([])

  export function clear() {
    sections = []
  }

  export function addOrScrollToResult(
    directiveId: string,
    srcPos: SrcPos,
    result: DirectiveResult,
    lineContent: string
  ): 'ok' | 'scrolled' {
    const existing = sections.find((s) => s.directiveId === directiveId)
    if (existing) {
      tick().then(() => {
        const el = document.getElementById(`section-${directiveId}`)
        el?.scrollIntoView({ behavior: 'smooth', block: 'nearest' })
        el?.classList.add('highlight-flash')
        setTimeout(() => el?.classList.remove('highlight-flash'), 600)
      })
      return 'scrolled'
    }

    const newSection: ResultSection = {
      directiveId,
      directiveType: result.directiveType,
      prettyText: result.prettyText,
      success: result.success,
      structuredValue: result.structuredValue,
      srcLine: srcPos.line,
      srcColumn: srcPos.column,
      lineContent,
      collapsed: false,
    }

    const idx = sections.findIndex(
      (s) =>
        s.srcLine > newSection.srcLine ||
        (s.srcLine === newSection.srcLine && s.srcColumn > newSection.srcColumn)
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
   * Sync all open sections against a fresh batch of results from the LSP.
   *
   * Pass 1 – exact directiveId match: update content in-place.
   * Pass 2 – content match: stale sections (id no longer live) are matched
   *   against unmatched results by their lineContent string, so sections
   *   track their directive even after line insertions/deletions.
   * Pass 3 – proximity fallback: any remaining stale sections are matched
   *   to the closest unmatched result by line-number distance.
   */
  export function syncSections(
    results: Array<{
      directiveId: string
      prettyText: string
      success: boolean | null
      lineContent: string
    }>
  ) {
    if (results.length === 0) return

    const liveIds = new Set(results.map((r) => r.directiveId))

    // Pass 1: exact directiveId matches
    const matchedResultIds = new Set<string>()
    sections = sections.map((s) => {
      const match = results.find((r) => r.directiveId === s.directiveId)
      if (match) {
        matchedResultIds.add(match.directiveId)
        return {
          ...s,
          prettyText: match.prettyText,
          success: match.success,
          lineContent: match.lineContent,
        }
      }
      return s
    })

    const staleSections = sections.filter((s) => !liveIds.has(s.directiveId))
    if (staleSections.length === 0) return

    const unmatchedResults = results.filter(
      (r) => !matchedResultIds.has(r.directiveId)
    )

    const parseLine = (id: string) => parseInt(id.split(':')[0], 10)
    const remappings = new Map<
      string,
      {
        newId: string
        newLine: number
        prettyText: string
        success: boolean | null
        lineContent: string
      }
    >()

    // Group stale sections and unmatched results by lineContent
    const staleByContent = Map.groupBy(staleSections, (s) => s.lineContent)
    const resultsByContent = Map.groupBy(unmatchedResults, (r) => r.lineContent)

    for (const [content, stales] of staleByContent) {
      const matchingResults = resultsByContent.get(content)
      if (!matchingResults) continue // no live result with this content → section stays uncolored

      if (stales.length === 1 && matchingResults.length === 1) {
        // Pass 2: unique content match — direct 1:1 remap
        remappings.set(stales[0].directiveId, {
          newId: matchingResults[0].directiveId,
          newLine: parseLine(matchingResults[0].directiveId),
          prettyText: matchingResults[0].prettyText,
          success: matchingResults[0].success,
          lineContent: matchingResults[0].lineContent,
        })
      } else {
        // Pass 3: ambiguous (duplicate content) — use proximity within the group
        const usedResultIds = new Set<string>()
        for (const stale of stales) {
          let best: (typeof matchingResults)[0] | null = null
          let bestDist = Infinity
          for (const result of matchingResults) {
            if (usedResultIds.has(result.directiveId)) continue
            const dist = Math.abs(stale.srcLine - parseLine(result.directiveId))
            if (dist < bestDist) {
              bestDist = dist
              best = result
            }
          }
          if (best !== null) {
            usedResultIds.add(best.directiveId)
            remappings.set(stale.directiveId, {
              newId: best.directiveId,
              newLine: parseLine(best.directiveId),
              prettyText: best.prettyText,
              success: best.success,
              lineContent: best.lineContent,
            })
          }
        }
      }
    }

    const staleIds = new Set(staleSections.map((s) => s.directiveId))

    // Remap matched stale sections; unmatched ones lose their success colour (user dismisses manually)
    sections = sections
      .map((s) => {
        const remap = remappings.get(s.directiveId)
        if (remap) {
          return {
            ...s,
            directiveId: remap.newId,
            srcLine: remap.newLine,
            srcColumn: parseInt(remap.newId.split(':')[1], 10),
            prettyText: remap.prettyText,
            success: remap.success,
            lineContent: remap.lineContent,
          }
        }
        // Unmatched stale section: clear the success indicator so it shows uncolored
        if (staleIds.has(s.directiveId) && !remappings.has(s.directiveId)) {
          return { ...s, success: null }
        }
        return s
      })
      .sort((a, b) => a.srcLine - b.srcLine || a.srcColumn - b.srcColumn)
  }

  function removeSection(directiveId: string) {
    sections = sections.filter((s) => s.directiveId !== directiveId)
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

  function truncate(text: string, max = 60): string {
    const trimmed = text.trim()
    return trimmed.length > max ? trimmed.slice(0, max) + '…' : trimmed
  }
</script>

<div class="inspector-panel">
  {#if sections.length === 0}
    <div class="empty-state">
      <p class="hint">
        Click "Track result" above an #EVAL, #EVALTRACE, #CHECK, or #ASSERT
        directive to add it here.
      </p>
    </div>
  {:else}
    {#each sections as section (section.directiveId)}
      <div
        id="section-{section.directiveId}"
        class="result-section {successClass(section.success)}"
      >
        <div class="section-header">
          <button
            class="collapse-toggle"
            onclick={() => toggleCollapse(section.directiveId)}
            title={section.collapsed ? 'Expand' : 'Collapse'}
          >
            <span class="chevron" class:rotated={!section.collapsed}
              >&#9002;</span
            >
          </button>
          <span class="source-location">{section.srcLine}:</span>
          <span class="directive-label" title={section.lineContent.trim()}>
            {truncate(section.lineContent)}
          </span>
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
            <pre class="result-code">{section.prettyText}</pre>
          </div>
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
    margin-top: 4px;
    opacity: 0.7;
    max-width: 200px;
  }

  .result-section {
    border: 1px solid #ddd;
    background: #fff;
    border-radius: 4px;
    margin-bottom: 8px;
    overflow: hidden;
  }

  .result-section.success {
    border-left: 2px solid #75beff;
  }

  .result-section.failure {
    border-left: 2px solid #f14c4c;
  }

  .section-header {
    display: flex;
    align-items: center;
    gap: 6px;
    padding: 3px 4px 3px 8px;
    background: #f0efee;
    cursor: default;
    user-select: none;
    min-width: 0;
  }

  .collapse-toggle {
    background: none;
    border: none;
    color: #1e1d1c;
    cursor: pointer;
    padding: 0;
    font-size: 10px;
    display: flex;
    align-items: center;
    flex-shrink: 0;
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
    background: #fafaf9;
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
