<script lang="ts">
  import StreamingMarkdown from './streaming-markdown.svelte'
  import ErrorBubble from './error-bubble.svelte'
  // import CopyButton from './copy-button.svelte'
  import ToolCallRow from './tool-call-row.svelte'
  import type {
    AssistantBlock,
    RenderedToolActivity,
    RenderedToolCall,
  } from '$lib/stores/ai-chat.svelte'

  import type { Messenger } from 'vscode-messenger-webview'

  let {
    content,
    streaming,
    pipelineActive = false,
    error,
    blocks,
    usage,
    messenger,
    onRetry,
    onOpenFile,
    onOpenFileDiff,
  }: {
    content: string
    streaming: boolean
    /** True until every queued user message has been folded into the
     *  pipeline. Suppresses the per-turn usage badge and the "Files
     *  changed" review card so they don't render mid-pipeline (when
     *  this turn is "done" but more processing is still queued).
     *  Defaults to false so untouched callers keep the prior
     *  behaviour of showing the cards as soon as the turn settles. */
    pipelineActive?: boolean
    error?: { message: string; code?: string }
    blocks?: AssistantBlock[]
    usage?: { promptTokens: number; completionTokens: number }
    /** Webview→extension messenger; threaded into ToolCallRow so the
     *  tool-call card can fetch L4 render-meta on demand. */
    messenger: InstanceType<typeof Messenger> | null
    onRetry?: () => void
    onOpenFile: (callId: string) => void
    onOpenFileDiff: (callId: string) => void
  } = $props()

  // Token badge — "• 1.2k in / 380 out" at the bottom of a
  // completed assistant bubble. Only renders after the stream
  // finishes (terminal chunk carries usage) and when the numbers
  // are actually present; tool-pause turns and pure-error turns
  // skip it silently.
  function formatTokenCount(n: number): string {
    if (n >= 1_000_000) return `${(n / 1_000_000).toFixed(1)}M`
    if (n >= 1_000) return `${(n / 1_000).toFixed(1)}k`
    return `${n}`
  }

  // Stable keys for the block iteration. Text blocks don't carry an
  // id, so we use the index; tool-call blocks key by callId;
  // tool-activity blocks use `ta:{tool}:{i}` so same-tool reruns get
  // distinct DOM nodes.
  function blockKey(b: AssistantBlock, i: number): string {
    if (b.kind === 'tool-call') return `tc:${b.call.callId}`
    if (b.kind === 'tool-activity') return `ta:${b.activity.tool}:${i}`
    if (b.kind === 'thinking') return `th:${i}`
    return `tx:${i}`
  }

  // Project an L4-rule tool-activity (the proxy ran a deployed rule
  // server-side) onto the SAME shape a client-side rule tool-call
  // uses, so it flows through the exact same <ToolCallRow> render
  // path — one source of truth for the INPUT/OUTPUT card, the L4
  // colorize, the auto-expand-on-done, the pulsating dot. The only
  // difference between the two is the trigger (server activity event
  // vs client tool-call event); the pixels are identical.
  //
  // `name` is rebuilt as `l4-rules__<fn>` so ToolCallRow's existing
  // rule detection (`isRuleCall`) lights up unchanged. `evaluate_rule`
  // wraps the real args under `arguments` alongside deployment/
  // function_name routing fields — unwrap that envelope so the card
  // shows just the rule's inputs, identical to a direct rule call.
  // The server-side rule activity carries the *raw MCP CallToolResult*
  // as `output` (`{ content: [{ type: 'text', text: '<json>' }] }`),
  // because the ai-proxy forwards the JSON-RPC `result` verbatim. A
  // client-side rule tool-call, by contrast, has already been flattened
  // to just the joined text by the IDE's mcp-client. ToolCallRow's
  // INPUT/OUTPUT extraction (`extractRuleResultValue` / `returnL4`)
  // expects that flattened string — it digs for `.contents.result.value`
  // directly. So mirror the client-side flatten here: pull the text
  // parts out of the MCP envelope and join them exactly the way
  // mcp-client.ts does. Anything that isn't the MCP shape (e.g. an
  // `{ error, message }` envelope from a failed call) falls through to
  // the plain JSON stringify so the row still shows something.
  function normalizeActivityResult(output: unknown): string | undefined {
    if (output === undefined) return undefined
    if (
      output &&
      typeof output === 'object' &&
      Array.isArray((output as { content?: unknown }).content)
    ) {
      const parts = (
        output as { content: Array<{ type?: string; text?: string }> }
      ).content
      const text = parts
        .map((c) =>
          c.type === 'text' && typeof c.text === 'string'
            ? c.text
            : JSON.stringify(c)
        )
        .join('\n')
      if (text.length > 0) return text
    }
    return JSON.stringify(output)
  }

  function ruleCallFor(a: RenderedToolActivity): RenderedToolCall {
    let argsObj: unknown = a.input
    if (a.input && typeof a.input === 'object') {
      const inp = a.input as Record<string, unknown>
      if ('function_name' in inp || 'deployment' in inp) {
        const inner =
          inp.arguments && typeof inp.arguments === 'object'
            ? (inp.arguments as Record<string, unknown>)
            : {}
        argsObj =
          inp.startTime !== undefined || inp.events !== undefined
            ? { arguments: inner, startTime: inp.startTime, events: inp.events }
            : inner
      }
    }
    return {
      callId: `ta:${a.ruleKey ?? a.tool}`,
      name: `l4-rules__${a.ruleId ?? a.tool}`,
      argsJson: JSON.stringify(argsObj ?? {}),
      status: a.status,
      result: normalizeActivityResult(a.output),
      error: a.error,
      // Carry the deployment + L4 function name so the render-meta
      // lookup can fetch the schema directly (the IDE's MCP target
      // map need not cover the cloud deployment a passthrough chat
      // is bound to).
      ...(a.deploymentId ? { deploymentId: a.deploymentId } : {}),
      ...(a.ruleId ? { ruleFnName: a.ruleId } : {}),
    }
  }

  // Per-block expanded state for thinking blocks, keyed by `i`. Stays
  // collapsed by default — the model's reasoning stream is high-volume
  // noise the user rarely wants to read in full.
  const thinkingExpanded = $state<Record<number, boolean>>({})

  // Turn-end review card: once the turn has finished, summarize which
  // files the assistant created/edited so the user can scan for blast
  // radius at a glance and jump straight to the applied diff(s).
  type FileChange = {
    path: string
    action: 'created' | 'edited' | 'deleted'
    /** Call id whose snapshot the diff/open view should anchor to.
     *  For `created` this points at the CREATE call (the file exists
     *  post-creation; opening it shows the new content as a plain
     *  editor tab — no diff). For `edited` it points at the FIRST
     *  edit call in the turn, so the applied-diff view shows the
     *  full turn-wide delta rather than just the last edit. */
    callId: string
  }
  const fileChanges = $derived.by<FileChange[]>(() => {
    if (!blocks) return []
    // Aggregate into a single row per path so the card shows one
    // outcome per file for the whole turn, not every intermediate
    // step. Rules:
    //   created + (edited*) → created (click opens file as a tab)
    //   created + deleted → dropped (net effect = nothing)
    //   edited (only)      → edited  (click opens applied diff)
    //   deleted (only)     → deleted (click opens file)
    type PathState = {
      created: boolean
      edited: boolean
      deleted: boolean
      firstCreateId: string | null
      firstEditId: string | null
      firstDeleteId: string | null
      /** Order of first appearance so the card preserves the path
       *  ordering the user saw the tool rows in. */
      order: number
    }
    const byPath = new Map<string, PathState>()
    let nextOrder = 0
    for (const b of blocks) {
      if (b.kind !== 'tool-call' || b.call.status !== 'done') continue
      const call = b.call
      let kind: 'created' | 'edited' | 'deleted' | null = null
      if (call.name === 'fs__edit_file') kind = 'edited'
      else if (call.name === 'fs__create_file') kind = 'created'
      else if (call.name === 'fs__delete_file') kind = 'deleted'
      if (!kind) continue
      let path: string | undefined
      try {
        path = (JSON.parse(call.argsJson) as { path?: string }).path
      } catch {
        continue
      }
      if (!path) continue
      let state = byPath.get(path)
      if (!state) {
        state = {
          created: false,
          edited: false,
          deleted: false,
          firstCreateId: null,
          firstEditId: null,
          firstDeleteId: null,
          order: nextOrder++,
        }
        byPath.set(path, state)
      }
      if (kind === 'created') {
        state.created = true
        if (!state.firstCreateId) state.firstCreateId = call.callId
      } else if (kind === 'edited') {
        state.edited = true
        if (!state.firstEditId) state.firstEditId = call.callId
      } else {
        state.deleted = true
        if (!state.firstDeleteId) state.firstDeleteId = call.callId
      }
    }
    const rows: Array<FileChange & { order: number }> = []
    for (const [path, s] of byPath) {
      // Created then deleted in the same turn cancels out — nothing
      // for the user to look at afterwards, so drop the row entirely.
      if (s.created && s.deleted) continue
      if (s.created) {
        rows.push({
          path,
          action: 'created',
          callId: s.firstCreateId!,
          order: s.order,
        })
      } else if (s.deleted) {
        rows.push({
          path,
          action: 'deleted',
          callId: s.firstDeleteId!,
          order: s.order,
        })
      } else if (s.edited) {
        rows.push({
          path,
          action: 'edited',
          callId: s.firstEditId!,
          order: s.order,
        })
      }
    }
    rows.sort((a, b) => a.order - b.order)
    return rows.map(({ path, action, callId }) => ({ path, action, callId }))
  })
</script>

<div class="assistant-row">
  <div class="assistant-bubble" class:is-streaming={streaming}>
    {#if blocks && blocks.length > 0}
      {#each blocks as block, i (blockKey(block, i))}
        {#if block.kind === 'text'}
          <StreamingMarkdown
            text={block.text}
            streaming={streaming && i === blocks.length - 1}
          />
        {:else if block.kind === 'tool-call'}
          <ToolCallRow
            call={block.call}
            {messenger}
            {onOpenFile}
            onOpenDiff={onOpenFileDiff}
          />
        {:else if block.kind === 'thinking'}
          <!-- Collapsed-by-default reasoning block. Chevron flips when
               toggled; expanded body renders as italic gray text with
               preserved whitespace so chain-of-thought indentation
               stays intact. -->
          <div class="thinking">
            <button
              type="button"
              class="thinking-toggle"
              onclick={() => (thinkingExpanded[i] = !thinkingExpanded[i])}
              aria-expanded={!!thinkingExpanded[i]}
            >
              <span class="thinking-chev"
                >{thinkingExpanded[i] ? '▾' : '▸'}</span
              >
              <span class="thinking-label"
                >Thinking{streaming && i === blocks.length - 1 ? '…' : ''}</span
              >
            </button>
            {#if thinkingExpanded[i]}
              <div class="thinking-body">{block.text}</div>
            {/if}
          </div>
        {:else if block.kind === 'tool-activity' && block.activity.ruleKey}
          <!-- L4-rule activity: the proxy evaluated a deployed rule
               server-side. Render it through the SAME ToolCallRow as
               a client-side rule call so the pulsating row, the
               auto-expanding INPUT/OUTPUT card and the L4 colorize
               are pixel-identical — the only difference is which
               event triggered it. -->
          <ToolCallRow
            call={ruleCallFor(block.activity)}
            {messenger}
            {onOpenFile}
            onOpenDiff={onOpenFileDiff}
          />
        {:else if block.kind === 'tool-activity'}
          <!-- Plain status activity (e.g. doc search): keeps the
               crimson dot up front (no expand chevron — nothing to
               expand on read-only backend events) + bold label +
               monospace message. The dot pulses iff this row is the
               trailing element in the assistant bubble — driven by
               `:last-child` in the <style> below, no JS bookkeeping.
               As soon as another block (text-delta, another activity,
               a tool-call) is appended after it, the row falls out of
               `:last-child` and the dot freezes solid. Errored rows
               opt out via the `is-error` class.

               The action label is tool-aware: `search_l4_docs` keeps
               the opaque "Legalesing…" string (its message body IS
               the label — there's no useful detail to expose), every
               other inspectable server tool (file infra + meta
               discovery) is a deployment-browsing call and gets
               labelled "L4 Deployments" — matching what
               ToolCallRow shows for the client-side counterpart so
               the two paths read the same to the user. -->
          {@const actionLabel =
            block.activity.tool === 'search_l4_docs'
              ? 'Legalesing...'
              : 'L4 Deployments'}
          <div
            class="tool-call tool-activity-row"
            class:is-error={block.activity.status === 'error'}
          >
            <div class="tool-row">
              <span class="dot" aria-hidden="true"></span>
              <span class="action">{actionLabel}</span>
              {#if block.activity.message !== actionLabel}
                <span class="target plain">{block.activity.message}</span>
              {/if}
            </div>
          </div>
        {/if}
      {/each}
    {:else if content}
      <StreamingMarkdown text={content} {streaming} />
    {/if}
    <!-- {#if !streaming && content}
      <div class="assistant-copy">
        <CopyButton getText={() => content} />
      </div>
    {/if} -->

    {#if !streaming && !pipelineActive && fileChanges.length > 0}
      <div
        class="review-card"
        role="group"
        aria-label="Files changed this turn"
      >
        <div class="review-header">
          <span class="review-title"
            >Files changed this turn ({fileChanges.length})</span
          >
        </div>
        <ul class="review-list">
          {#each fileChanges as change (change.callId)}
            <li class="review-row">
              <span class="review-action review-action-{change.action}"
                >{change.action}</span
              >
              <button
                type="button"
                class="review-path"
                onclick={() =>
                  change.action === 'edited'
                    ? onOpenFileDiff(change.callId)
                    : onOpenFile(change.callId)}
                title={change.action === 'edited'
                  ? 'Open applied diff'
                  : 'Open file'}>{change.path}</button
              >
            </li>
          {/each}
        </ul>
      </div>
    {/if}
    {#if error}
      <ErrorBubble message={error.message} code={error.code} {onRetry} />
    {/if}
    {#if !streaming && !pipelineActive && !error && usage}
      <!-- Per-turn token footer. Kept quiet (small, grey, dotted
           separator) so it reads as metadata rather than content.
           Prompt tokens come first because they dominate the cost
           on a long context; completion tokens second so users can
           spot overflowing answers. -->
      <div class="usage-badge" title="Tokens consumed by this turn">
        <span class="usage-dot">·</span>
        <span>{formatTokenCount(usage.promptTokens)} in</span>
        <span class="usage-sep">/</span>
        <span>{formatTokenCount(usage.completionTokens)} out</span>
      </div>
    {/if}
  </div>
</div>

<style>
  .assistant-row {
    display: flex;
    padding: 4px 0;
  }
  .assistant-bubble {
    position: relative;
    width: 100%;
    font-size: 13px;
    line-height: 1.45;
    color: var(--vscode-foreground);
  }
  /* .assistant-copy {
    position: absolute;
    top: -4px;
    right: 0;
  } */
  /* Tool-activity rows reuse the exact visual chrome used by
     tool-call rows (see tool-call-row.svelte). Styles are duplicated
     here because Svelte scopes CSS per-component; extracting into a
     global sheet would pull every page's .tool-call rules along with
     it. Keep the two blocks in sync. */
  .tool-call {
    font-size: 12px;
    padding: 6px 2px 8px;
    color: var(--vscode-descriptionForeground);
  }
  .tool-row {
    display: flex;
    align-items: baseline;
    gap: 6px;
  }
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
  /* Pulsate the activity dot on the trailing row only, AND only
     while the turn is still streaming. Three independent gates,
     all expressed in CSS:
       1. `:last-child`  — once another block lands AFTER this row
          (text, tool-call, another activity), it stops being last
          and the dot freezes. "No new info from the model yet" is
          encoded as DOM position, no JS bookkeeping.
       2. `:not(.is-error)` — errored rows opt out so the failure
          reads visually.
       3. `.assistant-bubble.is-streaming` — once the turn settles
          (server `done`, user Stop, new turn supersedes this one),
          the bubble loses `is-streaming` and every dot under it
          freezes. No status mutation needed — activity rows aren't
          really "errored" just because the user stopped the run. */
  .assistant-bubble.is-streaming
    .tool-activity-row:last-child:not(.is-error)
    .dot {
    animation: tool-activity-dot-pulse 1.1s ease-in-out infinite;
  }
  @keyframes tool-activity-dot-pulse {
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
  @media (prefers-reduced-motion: reduce) {
    .assistant-bubble.is-streaming
      .tool-activity-row:last-child:not(.is-error)
      .dot {
      animation-duration: 1.6s;
    }
    @keyframes tool-activity-dot-pulse {
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
  .action {
    color: var(--vscode-foreground);
    font-weight: 600;
  }
  .target.plain {
    color: var(--vscode-foreground);
    font-family: var(--vscode-editor-font-family, monospace);
    font-size: 0.92em;
    text-decoration: none;
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
  /* Thinking block: chevron-prefixed toggle, collapsed by default;
     expanded body is italic and grayer than body text so it reads as
     the model talking to itself. */
  .thinking {
    margin: 4px 0;
  }
  .thinking-toggle {
    display: inline-flex;
    align-items: center;
    gap: 4px;
    background: transparent;
    border: none;
    padding: 2px 0;
    font-size: 11px;
    color: var(--vscode-descriptionForeground);
    cursor: pointer;
  }
  .thinking-toggle:hover {
    color: var(--vscode-foreground);
  }
  .thinking-chev {
    font-size: 10px;
    width: 10px;
    display: inline-block;
    text-align: center;
  }
  .thinking-label {
    text-transform: uppercase;
    letter-spacing: 0.04em;
  }
  .thinking-body {
    margin-top: 4px;
    padding: 6px 10px;
    border-left: 2px solid
      var(--vscode-widget-border, rgba(128, 128, 128, 0.35));
    color: var(--vscode-descriptionForeground);
    font-style: italic;
    font-size: 12px;
    line-height: 1.5;
    white-space: pre-wrap;
    word-break: break-word;
  }
  /* Turn-end review card: quiet panel listing files the assistant
     touched this turn so the user can scan blast radius at a glance. */
  .review-card {
    margin-top: 10px;
    padding: 8px 10px;
    border: 1px solid var(--vscode-widget-border, rgba(128, 128, 128, 0.35));
    border-radius: 4px;
    background: var(--vscode-editor-background);
    font-size: 11px;
  }
  .review-header {
    display: flex;
    justify-content: space-between;
    align-items: center;
    gap: 8px;
  }
  .review-title {
    color: var(--vscode-foreground);
    font-weight: 600;
  }
  .review-list {
    list-style: none;
    padding: 0;
    margin: 6px 0 0;
    display: flex;
    flex-direction: column;
    gap: 3px;
  }
  .review-row {
    display: flex;
    align-items: baseline;
    gap: 8px;
  }
  .review-action {
    font-family: var(--vscode-editor-font-family, monospace);
    font-size: 10px;
    text-transform: uppercase;
    opacity: 0.8;
    min-width: 52px;
  }
  .review-action-created {
    color: #78c47c;
  }
  .review-action-edited {
    color: #c8c877;
  }
  .review-action-deleted {
    color: var(--vscode-errorForeground, #d7263d);
  }
  .review-path {
    background: transparent;
    border: none;
    padding: 0;
    color: var(--vscode-foreground);
    font-family: var(--vscode-editor-font-family, monospace);
    font-size: 11px;
    cursor: pointer;
    text-align: left;
  }
  .review-path:hover {
    text-decoration: underline;
  }
  /* Token badge — ambient "I/O budget used" line at the bottom of
     a finished assistant bubble. Small, grey, trailing space above
     so it reads as metadata rather than content. */
  .usage-badge {
    margin-top: 8px;
    display: inline-flex;
    align-items: center;
    gap: 4px;
    font-size: 10px;
    color: var(--vscode-descriptionForeground);
    opacity: 0.65;
    font-family: var(--vscode-editor-font-family, monospace);
  }
  .usage-dot {
    font-size: 14px;
    line-height: 1;
  }
  .usage-sep {
    opacity: 0.6;
  }
</style>
