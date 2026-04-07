<script lang="ts">
  import { tick } from 'svelte'
  import type { Messenger } from 'vscode-messenger-webview'
  import {
    AddInspectorResult,
    RemoveInspectorResult,
    SyncInspectorResults,
    RequestRevealLocation,
    type AddInspectorResultMessage,
    type SyncInspectorResultsMessage,
  } from 'jl4-client-rpc'
  import { HOST_EXTENSION } from 'vscode-messenger-common'
  import { colorize, escapeHtml } from '@repo/l4-highlight'

  let { messenger }: { messenger: InstanceType<typeof Messenger> | null } =
    $props()

  interface ResultSection {
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
  let registered = false

  // Register messenger handlers when messenger becomes available
  $effect(() => {
    if (messenger && !registered) {
      registered = true

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
    }
  })

  // ---------------------------------------------------------------------------
  // directiveId helpers — format is "uri:line:col" where uri may contain colons
  // ---------------------------------------------------------------------------

  function parseFileUri(id: string): string {
    const parts = id.split(':')
    return parts.slice(0, -2).join(':')
  }

  function parseLine(id: string): number {
    const parts = id.split(':')
    return parseInt(parts.at(-2)!, 10)
  }

  function parseCol(id: string): number {
    const parts = id.split(':')
    return parseInt(parts.at(-1)!, 10)
  }

  function displayFileName(uri: string): string {
    const decoded = decodeURIComponent(uri.replace(/^file:\/\//, ''))
    return decoded.split('/').pop() ?? uri
  }

  // ---------------------------------------------------------------------------
  // File grouping
  // ---------------------------------------------------------------------------

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

  function revealInEditor(uri: string, line: number) {
    messenger?.sendNotification(RequestRevealLocation, HOST_EXTENSION, {
      uri,
      line,
    })
  }

  // ---------------------------------------------------------------------------
  // Section management
  // ---------------------------------------------------------------------------

  function addOrScrollToResult(msg: AddInspectorResultMessage) {
    const existing = sections.find((s) => s.directiveId === msg.directiveId)
    if (existing) {
      if (existing.stale) {
        sections = sections.map((s) =>
          s.directiveId === msg.directiveId
            ? {
                ...s,
                prettyText: msg.result.prettyText,
                success: msg.result.success,
                structuredValue: msg.result.structuredValue,
                srcLine: msg.srcPos.line,
                srcColumn: msg.srcPos.column,
                lineContent: msg.lineContent,
                stale: false,
              }
            : s
        )
      }
      tick().then(() => {
        const el = document.getElementById(`section-${msg.directiveId}`)
        el?.scrollIntoView({ behavior: 'smooth', block: 'nearest' })
        el?.classList.add('highlight-flash')
        setTimeout(() => el?.classList.remove('highlight-flash'), 600)
      })
      return 'scrolled' as const
    }

    const newSection: ResultSection = {
      renderKey: nextRenderKey++,
      directiveId: msg.directiveId,
      fileUri: parseFileUri(msg.directiveId),
      prettyText: msg.result.prettyText,
      success: msg.result.success,
      structuredValue: msg.result.structuredValue,
      srcLine: msg.srcPos.line,
      srcColumn: msg.srcPos.column,
      lineContent: msg.lineContent,
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
      const el = document.getElementById(`section-${msg.directiveId}`)
      el?.scrollIntoView({ behavior: 'smooth', block: 'nearest' })
    })

    return 'ok' as const
  }

  function syncSections(msg: SyncInspectorResultsMessage) {
    const results = msg.results
    const syncUri =
      results.length > 0 ? parseFileUri(results[0].directiveId) : msg.uri
    const toSync = syncUri
      ? sections.filter((s) => s.fileUri === syncUri)
      : sections
    const untouched = syncUri
      ? sections.filter((s) => s.fileUri !== syncUri)
      : []

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

  type ColorizedEntry = { header: string; body: string }
  /**
   * Format a result value string by converting commas into line-breaks
   * with indentation based on parenthesis nesting depth.
   * Parentheses that wrap record values (e.g. `(Foo WITH ...)`) are stripped.
   */
  function formatResultValue(text: string): string {
    let result = ''
    let depth = 0
    const indent = () => '\n' + '  '.repeat(depth)

    for (let i = 0; i < text.length; i++) {
      const ch = text[i]
      if (ch === '(') {
        depth++
        // Strip the opening paren — the content will be indented
      } else if (ch === ')') {
        depth = Math.max(0, depth - 1)
        // Strip the closing paren
      } else if (ch === ',') {
        // Skip optional space after comma
        if (text[i + 1] === ' ') i++
        result += indent()
      } else if (text.startsWith(' WITH ', i)) {
        depth++
        result += ' WITH'
        result += indent()
        i += 5 // skip " WITH"
      } else {
        result += ch
      }
    }
    return result
  }

  const colorized: Record<string, ColorizedEntry> = $derived(
    Object.fromEntries(
      sections.map((s) => [
        s.directiveId,
        {
          header: colorize(s.lineContent.trim()),
          body: colorize(formatResultValue(s.prettyText)),
        },
      ])
    )
  )
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
                    if (e.metaKey || e.ctrlKey)
                      revealInEditor(section.fileUri, section.srcLine)
                    else toggleCollapse(section.directiveId)
                  }}
                  title={section.collapsed ? 'Expand' : 'Collapse'}
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
                    {@html colorized[section.directiveId]?.header ??
                      escapeHtml(section.lineContent.trim())}
                  </span>
                </button>
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
                  <pre class="result-code">{@html colorized[section.directiveId]
                      ?.body ?? escapeHtml(section.prettyText)}</pre>
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
    padding: 0 0 16px;
    display: flex;
    flex-direction: column;
    gap: 24px;
  }

  .empty-state {
    display: flex;
    flex-direction: column;
    align-items: center;
    justify-content: center;
    height: 40vh;
    text-align: center;
    color: var(--vscode-descriptionForeground);
  }

  .empty-state .hint {
    font-size: 0.95em;
    line-height: 1.2;
    max-width: 200px;
  }

  .file-header {
    display: flex;
    align-items: center;
    background: var(--vscode-sideBarSectionHeader-background, #252526);
    user-select: none;
    font-size: 0.92em;
    font-weight: 500;
  }

  .file-header-toggle {
    display: flex;
    align-items: center;
    gap: 6px;
    flex: 1;
    min-width: 0;
    padding: 4px 5px;
    background: none;
    border: none;
    color: var(--vscode-foreground);
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
    font-size: 0.82em;
    color: var(--vscode-descriptionForeground);
    opacity: 0.6;
    flex-shrink: 0;
  }

  .remove-all-btn {
    background: none;
    border: none;
    color: var(--vscode-foreground);
    cursor: pointer;
    opacity: 0.5;
    font-size: 0.9em;
    flex-shrink: 0;
    padding: 4px 8px;
  }

  .remove-all-btn:hover {
    opacity: 1;
  }

  .file-group {
    display: flex;
    flex-direction: column;
    gap: 8px;
  }

  .file-group.collapsed {
    margin-bottom: -16px;
  }

  .result-section {
    border: 1px solid var(--vscode-panel-border, #444);
    border-left-width: 3px;
    border-radius: 4px;
    overflow: hidden;
  }

  .result-section.success {
    border-left: 3px solid #75beff;
  }

  .result-section.failure {
    border-left: 3px solid var(--vscode-testing-iconFailed, #f14c4c);
  }

  .section-header {
    display: flex;
    align-items: center;
    background: var(--vscode-sideBarSectionHeader-background, #252526);
    border-bottom: 1px solid var(--vscode-panel-border, #444);
    user-select: none;
    min-width: 0;
  }

  .section-header-toggle {
    display: flex;
    align-items: center;
    gap: 6px;
    flex: 1;
    min-width: 0;
    padding: 5px 8px;
    background: none;
    border: none;
    color: var(--vscode-foreground);
    cursor: pointer;
    font-family: inherit;
    font-size: inherit;
    text-align: left;
  }

  .section-header:hover {
    background: var(--vscode-list-hoverBackground, #2a2d2e);
  }

  .chevron {
    display: inline-block;
    color: var(--vscode-descriptionForeground);
    transition: transform 0.15s;
    transform-origin: 25% 50%;
  }

  .chevron.rotated {
    transform: rotate(90deg);
  }

  .source-location {
    font-family: var(--vscode-editor-font-family, monospace);
    font-size: 0.85em;
    color: var(--vscode-descriptionForeground);
    opacity: 0.7;
    flex-shrink: 0;
  }

  .directive-label {
    font-family: var(--vscode-editor-font-family, monospace);
    font-size: 0.88em;
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
    font-size: 12px;
    padding: 0 7px 0 4px;
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

  :global(.tok-keyword) {
    color: var(--l4-tok-keyword, #569cd6);
  }
  :global(.tok-directive) {
    color: var(--l4-tok-directive, #c586c0);
    font-weight: bold;
  }
  :global(.tok-comment) {
    color: var(--l4-tok-comment, #8d949d);
  }
  :global(.tok-string) {
    color: var(--l4-tok-string, #ce9178);
  }
  :global(.tok-variable) {
    color: var(--l4-tok-operator, #d4d4d4);
  }
  :global(.tok-number) {
    color: var(--l4-tok-number, #b5cea8);
  }
  :global(.tok-operator) {
    color: var(--l4-tok-variable, #9cdcfe);
  }
  :global(.tok-identifier) {
    color: var(--l4-tok-identifier, #4ec9b0);
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
