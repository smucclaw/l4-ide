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
  // tools) get a richer expanded view: the arguments that went in and
  // the returned value, both in one panel separated by a hairline, with
  // the result formatted the same way the Inspector tab renders #EVAL
  // output (parens collapsed into indentation, L4 tokens highlighted).
  const isRuleCall = $derived.by(() => {
    if (!call.name.startsWith('l4-rules__')) return false
    const sub = call.name.slice('l4-rules__'.length)
    return !L4_RULES_INFRASTRUCTURE.has(sub)
  })

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

  // l4-rules MCP tools wrap their output as
  //   { "contents": { "result": { "value": "..." }, ... } }
  // We only ever want contents.result.value here — everything else
  // (types, traces, diagnostics) is noise for the chat view. If the
  // shape doesn't match we fall back to null so the result row
  // silently disappears rather than showing something misleading.
  function extractRuleResultValue(raw: string): string | null {
    try {
      const parsed = JSON.parse(raw.trim()) as unknown
      const contents = (parsed as { contents?: unknown } | null)?.contents
      const result = (contents as { result?: unknown } | null)?.result
      const value = (result as { value?: unknown } | null)?.value
      if (value === undefined || value === null) return null
      if (typeof value === 'string') return value
      return JSON.stringify(value, null, 2)
    } catch {
      return null
    }
  }

  const ruleResultHtml = $derived.by(() => {
    if (!isRuleCall || !call.result) return null
    const value = extractRuleResultValue(call.result)
    if (value === null) return null
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
    <!-- Leading chevron acts as the expand handle. Same glyph
         (`&#9002;`) the deployment tool-card and inspector-panel use,
         painted in the chat's primary crimson by default; turns red
         on error and gold when awaiting approval, so the row's state
         reads at a glance without needing a separate status column on
         the right. Disabled-looking (no pointer) when there's nothing
         to expand. -->
    <button
      type="button"
      class="expand-lead status-{call.status}"
      class:expanded
      class:has-details={hasDetails}
      onclick={hasDetails ? toggle : undefined}
      aria-expanded={hasDetails ? expanded : undefined}
      aria-label={hasDetails
        ? expanded
          ? 'Hide details'
          : 'Show details'
        : undefined}
      tabindex={hasDetails ? 0 : -1}>&#9002;</button
    >
    {#if hasDetails}
      <!-- When the row has something to expand, the label itself
           also toggles — bigger hit area than the chevron alone and
           matches Cursor / Claude Code habits. Falls back to a plain
           <span> otherwise so the row isn't a tab-stop with no
           action. -->
      <button
        type="button"
        class="action action-btn"
        onclick={toggle}
        aria-expanded={expanded}
        aria-label={expanded ? 'Hide details' : 'Show details'}
        >{view.label}</button
      >
    {:else}
      <span class="action">{view.label}</span>
    {/if}
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
  </div>

  {#if expanded && call.status === 'done' && isRuleCall}
    <!-- Match the deployment tool-card's expand layout: INPUT / OUTPUT
         section labels (small-caps, description-foreground) with a
         hairline separator between them. Keeps chat tool-expansion
         visually consistent with the sidebar's Deploy / Deployments
         cards. -->
    <div class="rule-details">
      <div class="section-label">Input</div>
      <pre class="rule-block rule-args">{JSON.stringify(args, null, 2)}</pre>
      {#if ruleResultHtml}
        <div class="section-label">Output</div>
        <pre class="rule-block rule-result">{@html ruleResultHtml}</pre>
      {/if}
    </div>
  {:else if expanded && call.status === 'done' && prettyResult}
    <!-- Generic tool output: wrap in the same bordered panel as the
         rule-eval expand so every tool-call expansion shares one
         visual shape. Inputs are intentionally omitted — they're
         already visible on the row itself (target + label), so
         surfacing them a second time inside the panel is noise. -->
    <div class="rule-details">
      <div class="section-label">Output</div>
      <pre class="rule-block rule-result">{prettyResult}</pre>
    </div>
  {:else if expanded && call.status === 'error' && call.error}
    <div class="rule-details">
      <div class="section-label">Output</div>
      <pre class="rule-block rule-result err-details">{call.error}</pre>
    </div>
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
  /* Expand handle — same `&#9002;` glyph the deployment tool-card and
     inspector-panel use. Lives IN FRONT of the row so the row reads
     as "open this". Painted in the chat's primary crimson by default;
     red on error; gold when awaiting approval. When there's nothing
     to expand (still running, no result yet) the chevron stays visible
     but isn't clickable — callers can see state without a second
     widget. */
  .expand-lead {
    background: transparent;
    border: none;
    padding: 0 2px;
    margin-right: 0;
    font-size: 11px;
    line-height: 1;
    flex-shrink: 0;
    color: #c8376a;
    transition: transform 0.15s;
    transform-origin: 25% 50%;
    cursor: default;
  }
  .expand-lead.has-details {
    cursor: pointer;
  }
  .expand-lead.expanded {
    transform: rotate(90deg);
  }
  .expand-lead.status-error {
    color: var(--vscode-errorForeground, #d7263d);
  }
  .expand-lead.status-pending-approval {
    /* Warning amber — same kind of "act on me" signal VSCode uses for
       editor warnings. `--vscode-editorWarning-foreground` maps to
       gold in every bundled theme. */
    color: var(--vscode-editorWarning-foreground, #e0a84a);
  }
  .action {
    color: var(--vscode-foreground);
    font-weight: 600;
  }
  /* Button variant of .action: keep the span form's typography
     intact (font-weight: 600, color, etc.) — using the `font`
     shorthand here would reset weight/family from the default button
     styles, which is why the label lost its bold. Only reset the
     default button chrome. */
  .action-btn {
    background: transparent;
    border: none;
    padding: 0;
    color: var(--vscode-foreground);
    font-family: inherit;
    font-size: inherit;
    font-weight: 600;
    line-height: inherit;
    cursor: pointer;
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
  .err {
    margin-top: 4px;
    color: var(--vscode-errorForeground, #d7263d);
    font-size: 11px;
    white-space: pre-wrap;
    word-break: break-word;
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
  /* Rule-eval expanded view: one bordered panel holding the call args
     and the returned value, separated by a hairline. Result rendering
     matches the Inspector tab's #EVAL block so the two views read
     identically. */
  .rule-details {
    margin: 4px 0 0 18px;
    border: 1px solid var(--vscode-widget-border, rgba(128, 128, 128, 0.35));
    border-radius: 4px;
    background: var(--vscode-editor-background);
    overflow: hidden;
  }
  .rule-block {
    margin: 0;
    padding: 6px 8px;
    background: transparent;
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
  /* Matches the deployment tool-card's section labels (small-caps,
     letter-spaced, muted) so the AI chat's expanded rule card reads
     the same as the sidebar Deploy / Deployments cards. */
  .section-label {
    font-size: 0.82em;
    text-transform: uppercase;
    letter-spacing: 0.05em;
    color: var(--vscode-descriptionForeground);
    opacity: 0.7;
    padding: 6px 8px 2px;
  }
  .section-label + .rule-block {
    padding-top: 2px;
  }
  /* Hairline between the Input pre and the Output section — makes
     the two sections visually distinct inside the shared panel.
     Scoped to `:not(:first-child)` so the generic-output case
     (Output as the only section) doesn't double up with the
     panel's own top border, which read as a stacked pair of
     lines above the code box. */
  .section-label:not(:first-child):has(+ .rule-result) {
    border-top: 1px solid var(--vscode-widget-border, rgba(128, 128, 128, 0.35));
    margin-top: 4px;
  }
</style>
