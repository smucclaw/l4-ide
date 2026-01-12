<script lang="ts">
  import type { LirContext } from '@repo/layout-ir'
  import type { Unique } from '@repo/viz-expr'
  import type { LadderGraphLirNode } from '$lib/layout-ir/ladder-graph/ladder.svelte.js'
  import { FalseVal, TrueVal, UnknownVal } from '$lib/eval/type.js'

  interface PartialEvalSidebarProps {
    context: LirContext
    ladderGraph: LadderGraphLirNode
  }

  let { context, ladderGraph }: PartialEvalSidebarProps = $props()

  const analysis = $derived(ladderGraph.getPartialEvalAnalysis(context))

  const labelFor = (unique: Unique) =>
    ladderGraph.getLabelForUnique(context, unique)

  const unassignedButUnclassified = $derived.by((): Unique[] => {
    if (!analysis) return []
    const still = new Set<Unique>(analysis.stillNeeded)
    const dont = new Set<Unique>(analysis.dontCare)
    return analysis.notAsked.filter((u) => !still.has(u) && !dont.has(u))
  })

  function assign(unique: Unique, value: TrueVal | FalseVal | UnknownVal) {
    ladderGraph.submitNewBinding(context, { unique, value })
  }
</script>

<aside class="partial-eval-sidebar">
  <div class="text-sm font-semibold flex items-center justify-between">
    <span>Inputs</span>
    {#if analysis}
      <span class="text-xs text-muted-foreground">
        {analysis.usedBdd ? 'BDD' : 'fold'}{#if analysis.bddStats}
          · {analysis.bddStats.support} live
        {/if}
      </span>
    {/if}
  </div>

  {#if !analysis}
    <div class="text-xs text-muted-foreground mt-2">No analysis yet.</div>
  {:else}
    <div class="bucket">
      <div class="bucket-title">Still Needed</div>
      {#if analysis.stillNeeded.length === 0}
        <div class="bucket-empty">
          {analysis.isDetermined ? 'Result determined.' : 'None (yet).'}
        </div>
      {:else}
        <ul class="bucket-list">
          {#each analysis.ranked as unique (unique)}
            <li class="bucket-item relevant">
              <span class="param-name">{labelFor(unique)}</span>
              <div class="quick-assign">
                <button
                  class="btn t"
                  onclick={() => assign(unique, new TrueVal())}
                >
                  T
                </button>
                <button
                  class="btn f"
                  onclick={() => assign(unique, new FalseVal())}
                >
                  F
                </button>
                <button
                  class="btn u"
                  onclick={() => assign(unique, new UnknownVal())}
                >
                  ?
                </button>
              </div>
            </li>
          {/each}
        </ul>
      {/if}
    </div>

    <div class="bucket">
      <div class="bucket-title">Not Asked</div>
      {#if unassignedButUnclassified.length === 0}
        <div class="bucket-empty">No other unassigned inputs.</div>
      {:else}
        <ul class="bucket-list">
          {#each unassignedButUnclassified as unique (unique)}
            <li class="bucket-item">
              <span class="param-name">{labelFor(unique)}</span>
              <div class="quick-assign">
                <button
                  class="btn t"
                  onclick={() => assign(unique, new TrueVal())}
                >
                  T
                </button>
                <button
                  class="btn f"
                  onclick={() => assign(unique, new FalseVal())}
                >
                  F
                </button>
                <button
                  class="btn u"
                  onclick={() => assign(unique, new UnknownVal())}
                >
                  ?
                </button>
              </div>
            </li>
          {/each}
        </ul>
      {/if}
    </div>

    <div class="bucket">
      <div class="bucket-title">Don’t Care</div>
      {#if analysis.dontCare.length === 0}
        <div class="bucket-empty">None.</div>
      {:else}
        <ul class="bucket-list">
          {#each analysis.dontCare as unique (unique)}
            <li class="bucket-item irrelevant">
              <span class="param-name line-through opacity-70"
                >{labelFor(unique)}</span
              >
            </li>
          {/each}
        </ul>
      {/if}
    </div>

    <div class="bucket">
      <div class="bucket-title">Restricted</div>
      <pre class="expr">{analysis.restrictedExprText}</pre>
      <div class="result">
        Result: <span class="font-semibold"
          >{analysis.overallResult.toPretty()}</span
        >
      </div>
    </div>
  {/if}
</aside>

<style>
  .partial-eval-sidebar {
    height: 100%;
    overflow-y: auto;
    width: 320px;
    padding: 0.75rem;
    border-radius: 0.375rem;
    border: 1px solid var(--color-border, rgba(0, 0, 0, 0.15));
    background: var(--color-background, white);
    color: var(--color-foreground, inherit);
  }

  .bucket {
    margin-top: 0.75rem;
  }

  .bucket-title {
    margin-bottom: 0.25rem;
    font-size: 0.75rem;
    font-weight: 600;
    text-transform: uppercase;
    letter-spacing: 0.05em;
    color: var(--color-muted-foreground, rgba(0, 0, 0, 0.55));
  }

  .bucket-empty {
    font-size: 0.75rem;
    color: var(--color-muted-foreground, rgba(0, 0, 0, 0.55));
  }

  .bucket-list {
    list-style: none;
    margin: 0;
    padding: 0;
    display: flex;
    flex-direction: column;
    gap: 0.25rem;
  }

  .bucket-item {
    display: flex;
    align-items: center;
    justify-content: space-between;
    gap: 0.5rem;
    padding: 0.25rem 0.5rem;
    border-radius: 0.25rem;
    border: 1px solid var(--color-border, rgba(0, 0, 0, 0.15));
    background: var(--color-card, var(--color-background, white));
  }

  .bucket-item.relevant {
    border-color: #fcd34d;
    background: #fffbeb;
  }

  .bucket-item.irrelevant {
    background: var(--color-muted, rgba(0, 0, 0, 0.04));
    opacity: 0.7;
  }

  .param-name {
    max-width: 14ch;
    text-align: left;
    font-family:
      ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, 'Liberation Mono',
      'Courier New', monospace;
    font-size: 0.75rem;
    overflow: hidden;
    text-overflow: ellipsis;
    white-space: nowrap;
  }

  .quick-assign {
    display: flex;
    gap: 0.25rem;
  }

  .btn {
    font-size: 0.7rem;
    font-weight: 600;
    line-height: 1;
    padding: 0.25rem 0.375rem;
    border-radius: 0.25rem;
    border: 1px solid var(--color-border, rgba(0, 0, 0, 0.15));
    background: var(--color-background, white);
    color: inherit;
  }

  .btn:hover {
    background: var(--color-accent, rgba(0, 0, 0, 0.06));
  }

  .btn.t {
    color: #047857;
  }
  .btn.f {
    color: #b91c1c;
  }
  .btn.u {
    color: var(--color-muted-foreground, rgba(0, 0, 0, 0.55));
  }

  .expr {
    font-size: 0.75rem;
    background: var(--color-muted, rgba(0, 0, 0, 0.04));
    border-radius: 0.25rem;
    padding: 0.5rem;
    overflow-x: auto;
  }

  .result {
    margin-top: 0.5rem;
    font-size: 0.75rem;
  }
</style>
