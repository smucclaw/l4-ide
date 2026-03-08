<script lang="ts">
  import { onMount, tick } from 'svelte'
  import { Messenger } from 'vscode-messenger-webview'
  import { HOST_EXTENSION } from 'vscode-messenger-common'
  import {
    AddInspectorResult,
    RemoveInspectorResult,
    UpdateInspectorResult,
    WebviewFrontendIsReadyNotification,
    type AddInspectorResultMessage,
    type UpdateInspectorResultMessage,
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
    collapsed: boolean
  }

  let sections: ResultSection[] = $state([])

  function addOrScrollToResult(msg: AddInspectorResultMessage) {
    const existing = sections.find((s) => s.directiveId === msg.directiveId)
    if (existing) {
      // Already present — scroll to it
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
      collapsed: false,
    }

    // Insert sorted by source position
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

  function updateSection(msg: UpdateInspectorResultMessage) {
    sections = sections.map((s) =>
      s.directiveId === msg.directiveId
        ? {
            ...s,
            prettyText: msg.result.prettyText,
            success: msg.result.success,
          }
        : s
    )
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

    messenger.onNotification(UpdateInspectorResult, (msg) => {
      updateSection(msg)
    })

    messenger.start()
  })
</script>

<div class="inspector-panel">
  {#if sections.length === 0}
    <div class="empty-state">
      <p class="hint">
        Click "Render result" above an #EVAL, #EVALTRACE, #CHECK, or #ASSERT
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
          <span class="directive-badge">
            {section.directiveType}
          </span>
          <span class="source-location">
            Line {section.srcLine}
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
    max-width: 180px;
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
    gap: 8px;
    padding: 6px 8px;
    background: var(--vscode-sideBarSectionHeader-background, #252526);
    border-bottom: 1px solid var(--vscode-panel-border, #444);
    cursor: default;
    user-select: none;
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
  }

  .chevron {
    display: inline-block;
    transition: transform 0.15s;
  }

  .chevron.rotated {
    transform: rotate(90deg);
  }

  .directive-badge {
    font-weight: 600;
    font-size: 0.85em;
    color: var(--vscode-descriptionForeground);
  }

  .source-location {
    font-size: 0.8em;
    color: var(--vscode-descriptionForeground);
    opacity: 0.7;
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

  /* Flash animation when scrolling to an existing section */
  :global(.highlight-flash) {
    animation: flash 0.6s ease-out;
  }

  @keyframes flash {
    0% {
      background: var(--vscode-editor-findMatchHighlightBackground, #ea5c0055);
    }
    100% {
      background: transparent;
    }
  }
</style>
