<script lang="ts">
  import type { RenderedToolCall } from '$lib/stores/ai-chat.svelte'
  import { colorize } from '@repo/l4-highlight'

  /**
   * Tools exposed by the l4-rules MCP server fall into two buckets.
   *
   *   Category 1 — "L4 Deployments" infrastructure tools: file I/O and
   *                index/search over the user's deployed rules. They
   *                don't evaluate business logic; they just let the
   *                agent browse what's available.
   *   Category 2 — "L4 Rule" evaluations: every other tool name in the
   *                namespace is a deployed rule the agent can invoke
   *                with inputs. The inspection / ladder rendering is
   *                reserved for this bucket.
   *
   * Keeping this set in sync with the MCP server's built-ins; anything
   * not in here is treated as a rule.
   */
  const L4_RULES_INFRASTRUCTURE = new Set([
    'list_files',
    'read_file',
    'search_identifier',
    'search_text',
  ])

  let {
    call,
    onOpenFile,
    onOpenDiff,
  }: {
    call: RenderedToolCall
    /** Simple open: shows the target file in a regular editor tab.
     *  Used for read / create / delete rows. */
    onOpenFile: (callId: string) => void
    /** Applied-diff open: shows the before/after of a completed edit
     *  in the diff editor with red/green change markers. */
    onOpenDiff: (callId: string) => void
  } = $props()

  // Best-effort arg parse; keep going even if the JSON is malformed
  // so the row still renders.
  const args = $derived.by<Record<string, unknown>>(() => {
    try {
      return JSON.parse(call.argsJson) as Record<string, unknown>
    } catch {
      return {}
    }
  })

  type ClickMode = 'open' | 'diff' | null
  const view = $derived.by(() => viewFor(call.name, args))

  function viewFor(
    name: string,
    a: Record<string, unknown>
  ): { label: string; target: string | null; click: ClickMode } {
    const path = typeof a.path === 'string' ? a.path : null
    if (name === 'fs__read_file')
      return { label: 'Read', target: path, click: 'open' }
    if (name === 'fs__create_file')
      return { label: 'Create', target: path, click: 'open' }
    if (name === 'fs__edit_file')
      return {
        label: 'Edit',
        target: path,
        // Diff only makes sense once the edit has landed — before that
        // just open the file so the user can see the context.
        click: call.status === 'done' ? 'diff' : 'open',
      }
    if (name === 'fs__delete_file')
      return { label: 'Delete', target: path, click: null }
    if (name === 'lsp__diagnostics')
      // Same tabular open as read — the user can inspect the file
      // while its diagnostics are shown in the Problems panel.
      return { label: 'Check', target: path, click: 'open' }
    if (name === 'l4__evaluate')
      return { label: 'Evaluate', target: path, click: 'open' }
    if (name === 'meta__ask_user')
      return { label: 'Question', target: null, click: null }
    if (name.startsWith('l4-rules__')) {
      const sub = name.slice('l4-rules__'.length)
      // Infrastructure tools (list_files / read_file / search_*) are
      // deployment inventory, not rule evaluations — label them
      // distinctly so the user sees at a glance whether the agent is
      // browsing deployments or actually running a rule.
      const isInfra = L4_RULES_INFRASTRUCTURE.has(sub)
      return {
        label: isInfra ? 'L4 Deployments' : 'L4 Rule',
        target: sub,
        click: null,
      }
    }
    return { label: name, target: null, click: null }
  }

  function fire(): void {
    if (view.click === 'diff') onOpenDiff(call.callId)
    else if (view.click === 'open') onOpenFile(call.callId)
  }

  function onTargetClick(e: MouseEvent): void {
    if (!view.click) return
    e.preventDefault()
    fire()
  }

  function onTargetKeydown(e: KeyboardEvent): void {
    if (!view.click) return
    if (e.key === 'Enter' || e.key === ' ') {
      e.preventDefault()
      fire()
    }
  }

  // Pretty-print the tool result for the expandable details panel.
  // Most tools return JSON strings; we try-parse and re-stringify with
  // indentation. If it's plain text, just show it verbatim.
  const prettyResult = $derived.by(() => {
    if (!call.result) return null
    try {
      return JSON.stringify(JSON.parse(call.result), null, 2)
    } catch {
      return call.result
    }
  })

  // Rule evaluations (the l4-rules__ namespace minus the infrastructure
  // tools) get a richer expanded view: a distinct Arguments section and
  // a Result section formatted the same way the Inspector tab renders
  // #EVAL output — parens collapsed into indentation, L4 tokens syntax-
  // highlighted. `startTime` and `events` are surfaced separately in
  // the args so the reader's eye lands on them: they're the contract-
  // state inputs, everything else is ordinary rule arguments.
  const isRuleCall = $derived.by(() => {
    if (!call.name.startsWith('l4-rules__')) return false
    const sub = call.name.slice('l4-rules__'.length)
    return !L4_RULES_INFRASTRUCTURE.has(sub)
  })

  function partitionRuleArgs(a: Record<string, unknown>): {
    primary: Record<string, unknown>
    startTime: unknown | undefined
    events: unknown | undefined
  } {
    const { startTime, events, ...primary } = a
    return { primary, startTime, events }
  }
  const ruleArgs = $derived.by(() =>
    isRuleCall ? partitionRuleArgs(args) : null
  )

  /**
   * Mirror of inspector-panel's formatResultValue: break commas into
   * indented lines, drop the parens that were only there to group a
   * record, and turn ` WITH ` into a block-opening indent. Keeps the
   * AI rule-result readout visually congruent with #EVAL in Inspector.
   */
  function formatResultValue(text: string): string {
    let out = ''
    let depth = 0
    const indent = (): string => '\n' + '  '.repeat(depth)
    for (let i = 0; i < text.length; i++) {
      const ch = text[i]
      if (ch === '(') {
        depth++
      } else if (ch === ')') {
        depth = Math.max(0, depth - 1)
      } else if (ch === ',') {
        if (text[i + 1] === ' ') i++
        out += indent()
      } else if (text.startsWith(' WITH ', i)) {
        depth++
        out += ' WITH' + indent()
        i += 5
      } else {
        out += ch
      }
    }
    return out
  }

  // Try to extract a human-facing value from the MCP result. MCP tools
  // return `content[]` as text blocks that mcp-client.ts flattens to a
  // single string — often that's already the pretty value, sometimes
  // it's wrapped JSON. Unwrap common shapes ({value}, {result}, raw
  // scalars) so we can pass the value to the L4 colorizer.
  function unwrapResultValue(raw: string): string {
    const trimmed = raw.trim()
    if (!trimmed) return raw
    try {
      const parsed = JSON.parse(trimmed)
      if (parsed === null || parsed === undefined) return trimmed
      if (typeof parsed === 'string') return parsed
      if (typeof parsed === 'number' || typeof parsed === 'boolean')
        return String(parsed)
      if (typeof parsed === 'object') {
        const obj = parsed as Record<string, unknown>
        for (const k of ['value', 'result', 'output']) {
          if (typeof obj[k] === 'string') return obj[k] as string
          if (typeof obj[k] === 'number' || typeof obj[k] === 'boolean')
            return String(obj[k])
        }
        return JSON.stringify(parsed, null, 2)
      }
      return trimmed
    } catch {
      return trimmed
    }
  }

  const ruleResultHtml = $derived.by(() => {
    if (!isRuleCall || !call.result) return null
    const value = unwrapResultValue(call.result)
    return colorize(formatResultValue(value))
  })

  const hasDetails = $derived(
    call.status === 'done'
      ? !!prettyResult
      : call.status === 'error'
        ? !!call.error
        : false
  )

  let expanded = $state(false)
  function toggle(): void {
    if (!hasDetails) return
    expanded = !expanded
  }
</script>

<div class="tool-call" class:is-error={call.status === 'error'}>
  <div class="tool-row">
    <span class="dot" aria-hidden="true"></span>
    <span class="action">{view.label}</span>
    {#if view.target}
      {#if view.click}
        <button
          type="button"
          class="target target-btn"
          title={view.click === 'diff'
            ? 'Click to open applied diff'
            : 'Click to open the file'}
          onclick={onTargetClick}
          onkeydown={onTargetKeydown}>{view.target}</button
        >
      {:else}
        <span class="target plain">{view.target}</span>
      {/if}
    {/if}
    <span class="status status-{call.status}">
      {#if call.status === 'pending-approval'}…
      {:else if call.status === 'running'}…
      {:else if call.status === 'done'}✓
      {:else if call.status === 'error'}failed
      {/if}
    </span>
    {#if hasDetails}
      <button
        type="button"
        class="expand-btn"
        onclick={toggle}
        title={expanded ? 'Hide details' : 'Show details'}
        aria-expanded={expanded}
        aria-label="Toggle details">{expanded ? '▾' : '▸'}</button
      >
    {/if}
  </div>

  {#if expanded && call.status === 'done' && isRuleCall && ruleArgs}
    <div class="rule-details">
      <section class="rule-section">
        <h4 class="rule-section-title">Arguments</h4>
        <pre class="rule-block">{JSON.stringify(
            ruleArgs.primary,
            null,
            2
          )}</pre>
        {#if ruleArgs.startTime !== undefined}
          <div class="rule-subfield">
            <span class="rule-subfield-label">startTime</span>
            <pre class="rule-block rule-block-inline">{JSON.stringify(
                ruleArgs.startTime,
                null,
                2
              )}</pre>
          </div>
        {/if}
        {#if ruleArgs.events !== undefined}
          <div class="rule-subfield">
            <span class="rule-subfield-label">events</span>
            <pre class="rule-block rule-block-inline">{JSON.stringify(
                ruleArgs.events,
                null,
                2
              )}</pre>
          </div>
        {/if}
      </section>
      <section class="rule-section">
        <h4 class="rule-section-title">Result</h4>
        {#if ruleResultHtml}
          <pre class="rule-block rule-result">{@html ruleResultHtml}</pre>
        {:else}
          <pre class="rule-block rule-result empty">(no result)</pre>
        {/if}
      </section>
    </div>
  {:else if expanded && call.status === 'done' && prettyResult}
    <pre class="details">{prettyResult}</pre>
  {:else if expanded && call.status === 'error' && call.error}
    <pre class="details err-details">{call.error}</pre>
  {/if}
</div>

<style>
  .tool-call {
    font-size: 12px;
    padding: 6px 2px 8px;
    color: var(--vscode-descriptionForeground);
  }
  .tool-row {
    display: flex;
    /* Baseline alignment so the bold action label and the monospace
       filename sit on the same underline — different font metrics
       would otherwise make the filename drift up/down when centered. */
    align-items: baseline;
    gap: 6px;
  }
  .dot {
    background: #c8376a;
    line-height: 1;
    flex-shrink: 0;
    margin-right: 2px;
    padding: 0.2em;
    border-radius: 0.2em;
    margin-bottom: -0.2em;
  }
  .action {
    color: var(--vscode-foreground);
    font-weight: 600;
  }
  .target,
  .target.plain {
    text-align: left;
    color: var(--vscode-foreground);
    font-family: var(--vscode-editor-font-family, monospace);
    font-size: 0.92em;
    text-decoration: none;
  }
  .target-btn {
    background: transparent;
    border: none;
    padding: 0;
    cursor: pointer;
    color: var(--vscode-foreground);
  }
  .target-btn:hover {
    text-decoration: underline;
  }
  .status {
    margin-left: auto;
    font-size: 11px;
    color: var(--vscode-descriptionForeground);
  }
  .status-error {
    color: var(--vscode-errorForeground, #d7263d);
  }
  .status-done {
    color: var(--vscode-foreground);
    opacity: 0.6;
  }
  .err {
    margin-top: 4px;
    color: var(--vscode-errorForeground, #d7263d);
    font-size: 11px;
    white-space: pre-wrap;
    word-break: break-word;
  }
  .expand-btn {
    background: transparent;
    border: none;
    color: var(--vscode-descriptionForeground);
    font-size: 11px;
    padding: 0 2px;
    margin-left: 2px;
    cursor: pointer;
  }
  .expand-btn:hover {
    color: var(--vscode-foreground);
  }
  .details {
    margin: 4px 0 0 18px;
    padding: 6px 8px;
    background: var(--vscode-editor-background);
    border: 1px solid var(--vscode-widget-border, rgba(128, 128, 128, 0.35));
    border-radius: 4px;
    font-family: var(--vscode-editor-font-family, monospace);
    font-size: 11px;
    color: var(--vscode-foreground);
    white-space: pre-wrap;
    word-break: break-word;
    max-height: 320px;
    overflow-y: auto;
  }
  .err-details {
    color: var(--vscode-errorForeground, #d7263d);
  }
  /* Rule-eval expanded view: two stacked panels (Arguments, Result)
     styled close to the Inspector tab's result section so reading a
     rule call here feels like reading an #EVAL there. */
  .rule-details {
    margin: 4px 0 0 18px;
    display: flex;
    flex-direction: column;
    gap: 8px;
  }
  .rule-section {
    border: 1px solid var(--vscode-widget-border, rgba(128, 128, 128, 0.35));
    border-radius: 4px;
    overflow: hidden;
  }
  .rule-section-title {
    margin: 0;
    padding: 4px 8px;
    font-size: 10px;
    font-weight: 600;
    text-transform: uppercase;
    letter-spacing: 0.06em;
    color: var(--vscode-descriptionForeground);
    background: var(--vscode-sideBarSectionHeader-background, transparent);
    border-bottom: 1px solid
      var(--vscode-widget-border, rgba(128, 128, 128, 0.35));
  }
  .rule-block {
    margin: 0;
    padding: 6px 8px;
    background: var(--vscode-editor-background);
    font-family: var(--vscode-editor-font-family, monospace);
    font-size: 11px;
    line-height: 1.5;
    color: var(--vscode-foreground);
    white-space: pre-wrap;
    word-break: break-word;
    max-height: 320px;
    overflow-y: auto;
    tab-size: 2;
  }
  .rule-block-inline {
    border-top: 1px solid var(--vscode-widget-border, rgba(128, 128, 128, 0.35));
  }
  .rule-subfield {
    display: flex;
    flex-direction: column;
  }
  .rule-subfield-label {
    padding: 3px 8px;
    font-size: 10px;
    font-weight: 600;
    color: var(--vscode-descriptionForeground);
    background: var(--vscode-editor-background);
    border-top: 1px solid var(--vscode-widget-border, rgba(128, 128, 128, 0.35));
  }
  .rule-result.empty {
    color: var(--vscode-descriptionForeground);
    font-style: italic;
  }
</style>
