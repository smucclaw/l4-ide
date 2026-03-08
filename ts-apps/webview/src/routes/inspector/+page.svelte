<script lang="ts">
  import { onMount, tick } from 'svelte'
  import { Messenger } from 'vscode-messenger-webview'
  import { HOST_EXTENSION } from 'vscode-messenger-common'
  import {
    AddInspectorResult,
    RemoveInspectorResult,
    SyncInspectorResults,
    WebviewFrontendIsReadyNotification,
    type AddInspectorResultMessage,
    type SyncInspectorResultsMessage,
    type WebviewFrontendIsReadyMessage,
  } from 'jl4-client-rpc'
  import type { WebviewApi } from 'vscode-webview'

  interface ResultSection {
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

  function addOrScrollToResult(msg: AddInspectorResultMessage) {
    const existing = sections.find((s) => s.directiveId === msg.directiveId)
    if (existing) {
      tick().then(() => {
        const el = document.getElementById(`section-${msg.directiveId}`)
        el?.scrollIntoView({ behavior: 'smooth', block: 'nearest' })
        el?.classList.add('highlight-flash')
        setTimeout(() => el?.classList.remove('highlight-flash'), 600)
      })
      return 'scrolled' as const
    }

    const newSection: ResultSection = {
      directiveId: msg.directiveId,
      directiveType: msg.result.directiveType,
      prettyText: msg.result.prettyText,
      success: msg.result.success,
      structuredValue: msg.result.structuredValue,
      srcLine: msg.srcPos.line,
      srcColumn: msg.srcPos.column,
      lineContent: msg.lineContent,
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
      const el = document.getElementById(`section-${msg.directiveId}`)
      el?.scrollIntoView({ behavior: 'smooth', block: 'nearest' })
    })

    return 'ok' as const
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
  function syncSections(msg: SyncInspectorResultsMessage) {
    const results = msg.results
    if (results.length === 0) return

    const liveIds = new Set(results.map((r) => r.directiveId))

    // directiveId format: "${uri}:${line}:${col}" — line is second-to-last colon-segment
    const parseLine = (id: string) => {
      const parts = id.split(':')
      return parseInt(parts.at(-2)!, 10)
    }
    const parseCol = (id: string) => {
      const parts = id.split(':')
      return parseInt(parts.at(-1)!, 10)
    }

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

    const remappings = new Map<
      string,
      {
        newId: string
        newLine: number
        newCol: number
        prettyText: string
        success: boolean | null
        lineContent: string
      }
    >()

    const staleByContent = Map.groupBy(staleSections, (s) => s.lineContent)
    const resultsByContent = Map.groupBy(unmatchedResults, (r) => r.lineContent)

    for (const [content, stales] of staleByContent) {
      const matchingResults = resultsByContent.get(content)
      if (!matchingResults) continue

      if (stales.length === 1 && matchingResults.length === 1) {
        // Pass 2: unique content match — direct 1:1 remap
        remappings.set(stales[0].directiveId, {
          newId: matchingResults[0].directiveId,
          newLine: parseLine(matchingResults[0].directiveId),
          newCol: parseCol(matchingResults[0].directiveId),
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
              newCol: parseCol(best.directiveId),
              prettyText: best.prettyText,
              success: best.success,
              lineContent: best.lineContent,
            })
          }
        }
      }
    }

    // Remove stale sections with no match (directive was deleted), remap the rest
    const removedIds = new Set(
      staleSections
        .filter((s) => !remappings.has(s.directiveId))
        .map((s) => s.directiveId)
    )

    sections = sections
      .filter((s) => !removedIds.has(s.directiveId))
      .map((s) => {
        const remap = remappings.get(s.directiveId)
        return remap
          ? {
              ...s,
              directiveId: remap.newId,
              srcLine: remap.newLine,
              srcColumn: remap.newCol,
              prettyText: remap.prettyText,
              success: remap.success,
              lineContent: remap.lineContent,
            }
          : s
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

  onMount(() => {
    // eslint-disable-next-line no-undef
    const vsCodeApi: WebviewApi<null> = acquireVsCodeApi()
    const messenger = new Messenger(vsCodeApi, { debugLog: true })

    messenger.sendNotification(
      WebviewFrontendIsReadyNotification,
      HOST_EXTENSION,
      { $type: 'webviewReady' } as WebviewFrontendIsReadyMessage
    )

    messenger.onRequest(
      AddInspectorResult,
      async (msg: AddInspectorResultMessage) => {
        const action = addOrScrollToResult(msg)
        return { $type: action }
      }
    )

    messenger.onNotification(RemoveInspectorResult, (msg) => {
      removeSection(msg.directiveId)
    })

    messenger.onNotification(SyncInspectorResults, (msg) => {
      syncSections(msg)
    })

    messenger.start()
  })
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
            <span class="chevron" class:rotated={!section.collapsed}>▶</span>
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
            ✕
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
    font-family: var(--vscode-font-family, sans-serif);
    font-size: var(--vscode-font-size, 13px);
    color: var(--vscode-foreground);
    background: var(--vscode-editor-background);
    padding: 8px;
    overflow-y: auto;
    height: 100vh;
  }

  .empty-state {
    display: flex;
    flex-direction: column;
    align-items: center;
    justify-content: center;
    height: 60vh;
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
    border: 1px solid var(--vscode-panel-border, #444);
    border-radius: 4px;
    margin-bottom: 8px;
    overflow: hidden;
  }

  .result-section.success {
    border-left: 3px solid var(--vscode-editorInfo-foreground, #75beff);
  }

  .result-section.failure {
    border-left: 3px solid var(--vscode-testing-iconFailed, #f14c4c);
  }

  .section-header {
    display: flex;
    align-items: center;
    gap: 6px;
    padding: 3px 4px 3px 8px;
    background: var(--vscode-sideBarSectionHeader-background, #252526);
    border-bottom: 1px solid var(--vscode-panel-border, #444);
    cursor: default;
    user-select: none;
    min-width: 0;
  }

  .collapse-toggle {
    background: none;
    border: none;
    color: var(--vscode-foreground);
    cursor: pointer;
    padding: 0;
    font-size: 10px;
    display: flex;
    align-items: center;
    flex-shrink: 0;
  }

  .chevron {
    display: inline-block;
    transition: transform 0.15s;
  }

  .chevron.rotated {
    transform: rotate(90deg);
  }

  .source-location {
    font-family: var(--vscode-editor-font-family, monospace);
    font-size: 0.78em;
    color: var(--vscode-descriptionForeground);
    opacity: 0.7;
    flex-shrink: 0;
  }

  .directive-label {
    font-family: var(--vscode-editor-font-family, monospace);
    font-size: 0.82em;
    color: var(--vscode-foreground);
    white-space: nowrap;
    overflow: hidden;
    text-overflow: ellipsis;
    flex: 1;
    min-width: 0;
  }

  .dismiss-btn {
    margin-left: auto;
    background: none;
    border: none;
    color: var(--vscode-foreground);
    cursor: pointer;
    opacity: 0.5;
    font-size: 14px;
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
    background: var(--vscode-editor-background);
  }

  .result-code {
    margin: 0;
    white-space: pre-wrap;
    word-break: break-word;
    font-family: var(--vscode-editor-font-family, monospace);
    font-size: var(--vscode-editor-font-size, 13px);
    line-height: 1.5;
    tab-size: 2;
  }

  :global(.highlight-flash) {
    animation: flash 0.6s ease-out;
  }

  @keyframes flash {
    0% {
      background: var(
        --vscode-editor-findMatchHighlightBackground,
        rgba(234, 92, 0, 0.33)
      );
    }
    100% {
      background: transparent;
    }
  }
</style>
