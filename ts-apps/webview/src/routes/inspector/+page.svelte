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
  import { colorize, escapeHtml } from '@repo/l4-highlight'

  interface ResultSection {
    directiveId: string
    fileUri: string
    directiveType: string
    prettyText: string
    success: boolean | null
    structuredValue: unknown | null
    srcLine: number
    srcColumn: number
    lineContent: string
    collapsed: boolean
    stale: boolean
  }

  let sections: ResultSection[] = $state([])
  let collapsedFiles: Set<string> = $state(new Set())

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

  // ---------------------------------------------------------------------------
  // Section management
  // ---------------------------------------------------------------------------

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
      fileUri: parseFileUri(msg.directiveId),
      directiveType: msg.result.directiveType,
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

  /**
   * Sync open sections against a fresh batch of results from the LSP.
   *
   * When `msg.uri` is set, only sections belonging to that file are synced;
   * sections from other files are left untouched.
   *
   * Pass 1 (PRIMARY) – content-based matching.
   * Pass 2 (FALLBACK) – positional match by directiveId.
   * Unmatched sections become stale.
   */
  function syncSections(msg: SyncInspectorResultsMessage) {
    const results = msg.results

    // Derive the file URI to scope the sync.
    // Primary: extract from the results' directiveIds (always reliable since they
    // include the URI). Fallback: msg.uri (needed when results is empty).
    // Without a URI, sync affects all sections (backward compat).
    const syncUri =
      results.length > 0 ? parseFileUri(results[0].directiveId) : msg.uri

    // Scope: only sync sections belonging to the notified file
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

    // Pass 2 (FALLBACK): positional match
    const directivePrefix = (line: string) =>
      line.trimStart().split(/\s/)[0] ?? ''
    for (const s of toSync) {
      if (s.stale || remappings.has(s.directiveId)) continue
      const sPrefix = directivePrefix(s.lineContent)
      const positionalMatch = results.find(
        (r) =>
          r.directiveId === s.directiveId &&
          !matchedResultIds.has(r.directiveId) &&
          directivePrefix(r.lineContent) === sPrefix
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

  function truncate(text: string, max = 60): string {
    const trimmed = text.trim()
    return trimmed.length > max ? trimmed.slice(0, max) + '…' : trimmed
  }

  type ColorizedEntry = { header: string; body: string }
  const colorized: Record<string, ColorizedEntry> = $derived(
    Object.fromEntries(
      sections.map((s) => [
        s.directiveId,
        {
          header: colorize(s.lineContent.trim()),
          body: colorize(s.prettyText),
        },
      ])
    )
  )

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

    // Listen for token color updates from the extension host (theme changes)
    window.addEventListener('message', (event) => {
      const msg = event.data
      if (msg?.type === 'l4-token-colors' && msg.colors) {
        const root = document.documentElement
        for (const [key, value] of Object.entries(
          msg.colors as Record<string, string>
        )) {
          root.style.setProperty(`--l4-tok-${key}`, value)
        }
      }
    })
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
    {#each fileGroups as group (group.fileUri)}
      {#if hasMultipleFiles}
        <div class="file-header">
          <button
            class="collapse-toggle"
            onclick={() => toggleFileCollapse(group.fileUri)}
            title={collapsedFiles.has(group.fileUri)
              ? 'Expand file'
              : 'Collapse file'}
          >
            <span
              class="chevron"
              class:rotated={!collapsedFiles.has(group.fileUri)}>&#9002;</span
            >
          </button>
          <span class="file-name" title={group.fileUri}
            >{group.displayName}</span
          >
          <button
            class="remove-all-btn"
            onclick={() => removeFileGroup(group.fileUri)}
            title="Remove all results from this file"
          >
            Remove all
          </button>
        </div>
      {/if}

      {#if !hasMultipleFiles || !collapsedFiles.has(group.fileUri)}
        {#each group.sections as section (section.directiveId)}
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
              {#if !section.stale}
                <span class="source-location">{section.srcLine}:</span>
              {/if}
              <span class="directive-label" title={section.lineContent.trim()}>
                {@html colorized[section.directiveId]?.header ??
                  escapeHtml(section.lineContent.trim())}
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
                <pre class="result-code">{@html colorized[section.directiveId]
                    ?.body ?? escapeHtml(section.prettyText)}</pre>
              </div>
            {/if}
          </div>
        {/each}
      {/if}
    {/each}
  {/if}
</div>

<style>
  .inspector-panel {
    font-family: var(--vscode-font-family, sans-serif);
    font-size: var(--vscode-font-size, 13px);
    color: var(--vscode-foreground);
    background: var(--vscode-panel-background);
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

  .file-header {
    display: flex;
    align-items: center;
    gap: 6px;
    padding: 4px 8px;
    margin-top: 4px;
    margin-bottom: 4px;
    background: var(--vscode-sideBarSectionHeader-background, #252526);
    border: 1px solid var(--vscode-panel-border, #444);
    border-radius: 4px;
    cursor: default;
    user-select: none;
    font-size: 0.85em;
    font-weight: 500;
  }

  .file-name {
    overflow: hidden;
    text-overflow: ellipsis;
    white-space: nowrap;
    flex: 1;
    min-width: 0;
  }

  .remove-all-btn {
    margin-left: auto;
    background: none;
    border: none;
    color: var(--vscode-foreground);
    cursor: pointer;
    opacity: 0.5;
    font-size: 0.9em;
    flex-shrink: 0;
  }

  .remove-all-btn:hover {
    opacity: 1;
  }

  .result-section {
    border: 1px solid var(--vscode-panel-border, #444);
    border-radius: 4px;
    margin-bottom: 8px;
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
    color: var(--vscode-descriptionForeground);
    transition: transform 0.15s;
    transform-origin: 25% 50%;
    margin: 0 -3px 0 1px;
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

  /* Token colors — injected by the extension host from the active VS Code color theme
     via --l4-tok-* CSS custom properties. Fallbacks match VS Code Default Dark+. */
  :global(.tok-keyword) {
    color: var(--l4-tok-keyword, #569cd6);
  }
  :global(.tok-directive) {
    color: var(--l4-tok-directive, #c586c0);
    font-weight: bold;
  }
  :global(.tok-comment) {
    color: var(--l4-tok-comment, #6a9955);
    font-style: italic;
  }
  :global(.tok-string) {
    color: var(--l4-tok-string, #ce9178);
  }
  :global(.tok-variable) {
    color: var(--l4-tok-variable, #9cdcfe);
  }
  :global(.tok-number) {
    color: var(--l4-tok-number, #b5cea8);
  }
  :global(.tok-operator) {
    color: var(--l4-tok-operator, #d4d4d4);
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
