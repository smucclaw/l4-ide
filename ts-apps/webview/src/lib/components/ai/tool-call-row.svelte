<script lang="ts">
  import type { RenderedToolCall } from '$lib/stores/ai-chat.svelte'
  import { colorize } from '@repo/l4-highlight'
  import {
    AiToolRenderMeta,
    renderArgumentsAsL4,
    renderJsonAsL4,
    type FunctionParameter,
  } from 'jl4-client-rpc'
  import type { Messenger } from 'vscode-messenger-webview'

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
    messenger,
    onOpenFile,
    onOpenDiff,
  }: {
    call: RenderedToolCall
    /** Webview→extension messenger; used to fetch render metadata
     *  (arg/return schema with `x-l4-type`) on demand for L4-syntax
     *  rendering of rule call arguments. Optional: when null the row
     *  falls back to plain JSON. */
    messenger: InstanceType<typeof Messenger> | null
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
  /** Each tool gets two labels:
   *   - `running`: present-continuous, used while the call is
   *     in-flight ("Editing…"). Conveys "the model is working on
   *     this right now" instead of just naming the verb.
   *   - `settled`: noun/past-participle, used once the call resolves
   *     to done / error ("Edit"). Pairs with the chevron that opens
   *     the result / diff.
   */
  const view = $derived.by(() => viewFor(call.name, args))

  function viewFor(
    name: string,
    a: Record<string, unknown>
  ): {
    running: string
    settled: string
    target: string | null
    click: ClickMode
  } {
    const path = typeof a.path === 'string' ? a.path : null
    if (name === 'fs__read_file')
      // "Read" works as both base and past tense (read/read/read) —
      // settled label stays unchanged.
      return {
        running: 'Reading…',
        settled: 'Read',
        target: path,
        click: 'open',
      }
    if (name === 'fs__create_file')
      return {
        running: 'Creating…',
        settled: 'Created',
        target: path,
        click: 'open',
      }
    if (name === 'fs__edit_file')
      return {
        running: 'Editing…',
        settled: 'Edited',
        target: path,
        // Diff only makes sense once the edit has landed — before that
        // just open the file so the user can see the context.
        click: call.status === 'done' ? 'diff' : 'open',
      }
    if (name === 'fs__delete_file')
      return {
        running: 'Deleting…',
        settled: 'Deleted',
        target: path,
        click: null,
      }
    if (name === 'l4__evaluate')
      return {
        running: 'Evaluating…',
        settled: 'Evaluated',
        target: path,
        click: 'open',
      }
    if (name === 'l4__refactor') {
      // `action` is the discriminator on the refactor tool's args.
      // Today we ship `rename`; new actions plug in here without
      // changing the tool name. Falls back to the raw action string
      // if a future action lands before this map is updated.
      const action = typeof a.action === 'string' ? a.action : ''
      const subtitle =
        action === 'rename' ? 'renaming L4 identifier' : action || 'refactor'
      return {
        running: 'Refactoring…',
        settled: 'Refactoring',
        target: subtitle,
        click: path ? 'open' : null,
      }
    }
    if (name === 'meta__ask_user')
      return {
        running: 'Asking…',
        settled: 'Asked',
        target: null,
        click: null,
      }
    if (name.startsWith('l4-rules__')) {
      const sub = name.slice('l4-rules__'.length)
      // Infrastructure tools (list_files / read_file / search_*) are
      // deployment inventory, not rule evaluations — label them
      // distinctly so the user sees at a glance whether the agent is
      // browsing deployments or actually running a rule. Same label
      // running and settled — MCP rule names are user-deployed and
      // a present-continuous variant doesn't add useful info beyond
      // the pulsating dot already conveying activity.
      const isInfra = L4_RULES_INFRASTRUCTURE.has(sub)
      const mcpLabel = isInfra ? 'L4 Deployments' : 'L4 Rule'
      // For rule evaluations, prefer the original L4 function name
      // (with spaces) parsed from the MCP description trailer over the
      // wire-level sanitised slug — matches what the server-side rule
      // activity card shows for the same rule.
      const ruleTarget = !isInfra && call.ruleFnName ? call.ruleFnName : sub
      return {
        running: mcpLabel,
        settled: mcpLabel,
        target: ruleTarget,
        click: null,
      }
    }
    return { running: name, settled: name, target: null, click: null }
  }

  /** Active label — present-continuous while running / pending
   *  approval, settled noun once the call resolves. */
  const label = $derived(
    call.status === 'running' || call.status === 'pending-approval'
      ? view.running
      : view.settled
  )

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

  // Schema-less fallback: when the function-schema fetch hasn't
  // resolved (or failed), show the extracted result value as plain
  // text. No L4 colorize and no indentation pass — both look wrong
  // on JSON-shaped payloads (the colorizer paints random tokens that
  // happen to overlap with L4 keywords; the indenter splits commas
  // mid-JSON-array). The schema-driven `returnL4` takes precedence
  // in the JSX whenever it's available.
  const ruleResultText = $derived.by(() => {
    if (!isRuleCall || !call.result) return null
    return extractRuleResultValue(call.result)
  })

  // Render-meta for L4-syntax rendering of rule arguments.
  // Lazy-fetched: we only ask the extension for the schema once the
  // row's expand panel is opened on a rule call, and cache the result
  // for the lifetime of this component.
  let renderMeta = $state<{
    parameters: FunctionParameter
    returnSchema?: FunctionParameter
  } | null>(null)
  let renderMetaInFlight = false
  async function ensureRenderMeta(): Promise<void> {
    if (renderMeta || renderMetaInFlight || !messenger || !isRuleCall) return
    renderMetaInFlight = true
    try {
      const res = await messenger.sendRequest(
        AiToolRenderMeta,
        { type: 'extension' },
        {
          toolName: call.name,
          ...(call.deploymentId ? { deploymentId: call.deploymentId } : {}),
          ...(call.ruleFnName ? { fnName: call.ruleFnName } : {}),
        }
      )
      if (res.kind === 'meta') {
        renderMeta = {
          parameters: res.parameters,
          returnSchema: res.returnSchema,
        }
      }
      // `unavailable` responses leave `renderMeta` null and the
      // in-flight flag clears below, so the next effect tick retries
      // once the call settles (the MCP tool list may not have been
      // populated yet when the first row mounted on a streaming turn).
    } catch {
      // Network / proxy hiccup — retry on the next effect tick.
    } finally {
      renderMetaInFlight = false
    }
  }

  // Pre-fetch render-meta as soon as the row mounts on a rule call,
  // not on first expand. The previous on-expand fire produced a
  // visible flicker — the panel briefly painted the JSON fallback
  // before the schema arrived and the L4 view swapped in. Fetching
  // up front means by the time the user clicks the chevron the
  // metadata is already in memory and the L4 view renders on the
  // first frame. The extension caches by deployment version, so the
  // cost across many cards in a long conversation is one roundtrip
  // per distinct (deployId, fnName).
  //
  // We re-trigger on `call.status` so the second attempt happens once
  // the call moves to running/done — by then the extension's MCP
  // target map is warm. Without this, multiple rows for the same rule
  // in a streaming turn would race the MCP list refresh, the early
  // ones would get `unavailable`, and only the last row (mounting
  // after the list settled) would render as L4.
  $effect(() => {
    void call.status
    if (isRuleCall && !renderMeta) void ensureRenderMeta()
  })

  // L4-rendered argument block. Falls back to null when we don't have
  // a schema yet (or the tool isn't a rule), and the panel shows
  // pretty-printed JSON instead.
  const argsL4 = $derived.by(() => {
    if (!isRuleCall || !renderMeta) return null
    try {
      // Rule calls wrap their args under `arguments`; deontic functions
      // also carry top-level `startTime` / `events`. We render only the
      // `arguments` payload here — the simulation envelope is rare and
      // not user-edited L4 syntax anyway, so JSON is fine for it.
      const inner =
        typeof args.arguments === 'object' && args.arguments !== null
          ? (args.arguments as Record<string, unknown>)
          : args
      // The schema's `arguments` property mirrors the JSON shape for
      // deontic functions; for non-deontic the schema IS the args
      // object itself.
      const argsSchema =
        renderMeta.parameters.properties?.arguments ?? renderMeta.parameters
      return colorize(renderArgumentsAsL4(inner, argsSchema))
    } catch {
      return null
    }
  })

  // L4-rendered return value, walked alongside `returnSchema`. Covers
  // every value shape the evaluator emits — records (constructor-wrapped
  // single-key objects), enums (bare strings matching schema.enum),
  // primitives, and arrays. When the schema isn't available the JSX
  // falls back to `ruleResultText` (plain extracted value, no
  // formatting, no colorize).
  const returnL4 = $derived.by(() => {
    if (!isRuleCall || !renderMeta?.returnSchema || !call.result) return null
    try {
      const parsed = JSON.parse(call.result.trim()) as unknown
      const contents = (parsed as { contents?: unknown } | null)?.contents
      const result = (contents as { result?: unknown } | null)?.result
      const value = (result as { value?: unknown } | null)?.value
      if (value === undefined || value === null) return null
      return colorize(renderJsonAsL4(value, renderMeta.returnSchema))
    } catch {
      return null
    }
  })

  const hasDetails = $derived(
    call.status === 'done'
      ? !!prettyResult
      : call.status === 'error'
        ? !!call.error
        : false
  )

  /**
   * Lift a "Lines N-M" suffix out of the tool result for fs__read_file
   * and fs__edit_file rows so the row reads "Read foo.l4 (Lines
   * 12-87)" / "Edited foo.l4 (Lines 23-45)" with the range in subtle
   * grey after the path. Skipped when the row has no result to parse
   * yet (pulsating variant) or when the read covered the whole file
   * — "(Lines 1-N)" on every full-file read is noise, the path alone
   * already says "I read this file".
   *
   * Result header formats (set by tools/fs.ts):
   *   read slice:       `[<path> <start>-<end>/<total>]…`
   *   read keyword hit: `[<path> keywords="…" matches=N chunks=K/M]` (skipped)
   *   edit snippet:     `[<path> <start>-<end>] Edited …`
   *   edit whole-file:  `[<path> 1-N/N] Wrote …` (skipped via total rule)
   *
   * The presence of `/<total>` in the header signals a read-style
   * range; without it the range is an edit anchor (always shown).
   */
  const readLineSuffix = $derived.by<string | null>(() => {
    if (call.name !== 'fs__read_file' && call.name !== 'fs__edit_file') {
      return null
    }
    if (!call.result) return null
    // Trailing `[^\]]*` swallows anything between the (optional)
    // /total and the closing bracket — fs__read_file appends
    // `, next startLine=<n>` when there's more file to read, so
    // anchoring on `\]` directly would skip every paginated read.
    const m = call.result.match(/^\[[^\]]*?\s(\d+)-(\d+)(?:\/(\d+))?[^\]]*\]/)
    if (!m) return null
    const start = parseInt(m[1]!, 10)
    const end = parseInt(m[2]!, 10)
    const total = m[3] != null ? parseInt(m[3], 10) : null
    if (total !== null && start === 1 && end === total) return null
    return `Lines ${start}-${end}`
  })

  let expanded = $state(false)
  function toggle(): void {
    if (!hasDetails) return
    expanded = !expanded
  }

  // Auto-expand an L4 Rule evaluation once it resolves to `done` AND
  // the L4 schema (`renderMeta`) has loaded: the input/output is the
  // whole point of a rule call, so making the user click the chevron
  // is friction — but we hold the expand until the schema is in so
  // the panel paints the L4 view on the first frame instead of
  // flashing the JSON fallback and swapping. If the schema never
  // resolves the row simply stays shut (the user can still open it
  // manually to see the JSON). Generic tool calls (read/edit/search)
  // never auto-expand.
  //
  // One-shot latch so a manual collapse afterwards sticks: the effect
  // fires once and never fights the user's subsequent toggle.
  let autoExpanded = false
  $effect(() => {
    if (
      !autoExpanded &&
      isRuleCall &&
      call.status === 'done' &&
      hasDetails &&
      renderMeta
    ) {
      autoExpanded = true
      expanded = true
    }
  })

  // While ANY tool call is in flight we render the row in the
  // dot-prefixed style (same chrome as the server-side tool-activity
  // rows in message-assistant.svelte) — no chevron, not expandable,
  // dot pulsating. The instant the call resolves to `done` or
  // `error`, the row swaps to the standard chevron card so the user
  // can expand the diff or the error message. The proxy now streams
  // partial tool-call frames as soon as the model commits to a tool
  // name (toolCallStreaming) so the pulsating dot appears
  // immediately — even before the args JSON has finished streaming.
  // `pending-approval` stays on the chevron variant so the gold
  // "act on me" cue is preserved.
  const isPulsating = $derived(call.status === 'running')
</script>

<div class="tool-call" class:is-error={call.status === 'error'}>
  <div class="tool-row">
    {#if isPulsating}
      <!-- In-flight create / edit. Dot-prefixed, non-expandable; the
           dot pulsates to signal "still working". Once the call
           resolves the row swaps to the chevron variant below and
           becomes expandable. -->
      <span class="dot pulsating" aria-hidden="true"></span>
      <span class="action">{label}</span>
      {#if view.target}
        <span class="target plain"
          >{view.target}{#if readLineSuffix}<span class="read-range"
              >({readLineSuffix})</span
            >{/if}</span
        >
      {/if}
    {:else}
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
          >{label}</button
        >
      {:else}
        <span class="action">{label}</span>
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
            onkeydown={onTargetKeydown}
            >{view.target}{#if readLineSuffix}<span class="read-range"
                >({readLineSuffix})</span
              >{/if}</button
          >
        {:else}
          <span class="target plain"
            >{view.target}{#if readLineSuffix}<span class="read-range"
                >({readLineSuffix})</span
              >{/if}</span
          >
        {/if}
      {/if}
    {/if}
  </div>

  {#if expanded && call.status === 'done' && isRuleCall}
    <!-- Match the deployment tool-card's expand layout: INPUT / OUTPUT
         section labels (small-caps, description-foreground) with a
         hairline separator between them. Keeps chat tool-expansion
         visually consistent with the sidebar's Deploy / Deployments
         cards. The Input pre-renders as L4 syntax (Type WITH …) once
         the function-schema metadata loads; until then we show the
         raw JSON the model sent so there's never a blank state. -->
    <div class="rule-details">
      <div class="section-label">Input</div>
      {#if Object.keys(args).length === 0}
        <!-- Zero-arg rule call: the empty `{}` JSON the model sent
             reads as "the rule was invoked with no inputs," which L4
             writes as `NOTHING`. Render the keyword instead of the
             literal `{}` so a no-arg evaluation matches how a human
             would have transcribed the same call. -->
        <pre class="rule-block rule-args">{@html colorize('NOTHING')}</pre>
      {:else if argsL4}
        <pre class="rule-block rule-args">{@html argsL4}</pre>
      {:else}
        <pre class="rule-block rule-args">{JSON.stringify(args, null, 2)}</pre>
      {/if}
      {#if returnL4}
        <div class="section-label">Output</div>
        <pre class="rule-block rule-result">{@html returnL4}</pre>
      {:else if ruleResultText}
        <div class="section-label">Output</div>
        <pre class="rule-block rule-result">{ruleResultText}</pre>
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
    /* Slightly looser than the default to give the filename (and any
       wrapped lines) a touch more breathing room — ~1px over normal. */
    line-height: 14px;
  }
  .target-btn {
    background: transparent;
    border: none;
    padding: 0;
    cursor: pointer;
    color: var(--vscode-foreground);
  }
  /* Subtle suffix for fs__read_file rows: the line range that came
     back, e.g. "(Lines 1-100)". Uses descriptionForeground (the same
     muted grey VSCode uses for hints / file-path subtitles) so the
     filename stays the visual anchor and the range reads as
     metadata. */
  .read-range {
    /* inline-block so the parent .target-btn's hover underline does not
       propagate across the range (text-decoration doesn't cross into an
       atomic inline box) — only the filename gets underlined on hover. */
    display: inline-block;
    margin-left: 4px;
    color: var(--vscode-descriptionForeground);
    font-family: var(--vscode-editor-font-family, monospace);
    font-size: 0.85em;
    text-decoration: none;
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
    /* Preserve indentation; long lines scroll horizontally instead of
       wrapping. Word-wrap obscures the L4-syntax indentation pattern
       (e.g. `LIST` items, nested `WITH` blocks). */
    white-space: pre;
    overflow: auto;
    max-height: 320px;
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

  /* Crimson square dot — same shape and offsets used by the
     server-side `tool-activity` rows in message-assistant.svelte. Kept
     in sync visually so a running create/edit reads the same as a
     server activity row. Pulsates while the call is in flight; the
     row swaps to the chevron variant once the result lands. */
  .dot {
    position: relative;
    background: #c8376a;
    line-height: 1;
    flex-shrink: 0;
    margin: 0 8px 0 2px;
    padding: 0.2em;
    border-radius: 0.2em;
    top: -0.15em;
  }
  .dot.pulsating {
    animation: tool-dot-pulse 1.1s ease-in-out infinite;
  }
  @keyframes tool-dot-pulse {
    0%,
    100% {
      opacity: 1;
      transform: scale(1);
    }
    50% {
      opacity: 0.45;
      transform: scale(0.78);
    }
  }
  /* Honour the user's reduced-motion preference — fade only, no
     transform-driven pulse. */
  @media (prefers-reduced-motion: reduce) {
    .dot.pulsating {
      animation-duration: 1.6s;
    }
    @keyframes tool-dot-pulse {
      0%,
      100% {
        opacity: 1;
        transform: none;
      }
      50% {
        opacity: 0.5;
        transform: none;
      }
    }
  }
</style>
