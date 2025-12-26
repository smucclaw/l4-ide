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

<style lang="postcss">
  @reference 'tailwindcss';

  .partial-eval-sidebar {
    @apply h-full overflow-y-auto border border-border rounded-md bg-background p-3;
    width: 320px;
  }

  .bucket {
    @apply mt-3;
  }

  .bucket-title {
    @apply text-xs font-semibold uppercase tracking-wide text-muted-foreground mb-1;
  }

  .bucket-empty {
    @apply text-xs text-muted-foreground;
  }

  .bucket-list {
    @apply list-none m-0 p-0 space-y-1;
  }

  .bucket-item {
    @apply flex items-center justify-between gap-2 rounded border border-border bg-card px-2 py-1;
  }

  .bucket-item.relevant {
    @apply border-amber-300 bg-amber-50;
  }

  .bucket-item.irrelevant {
    @apply bg-muted opacity-70;
  }

  .param-name {
    @apply text-xs font-mono text-left truncate;
    max-width: 14ch;
  }

  .quick-assign {
    @apply flex gap-1;
  }

  .btn {
    @apply text-[0.7rem] font-semibold leading-none px-1.5 py-1 rounded border border-border bg-background hover:bg-accent;
  }

  .btn.t {
    @apply text-emerald-700;
  }
  .btn.f {
    @apply text-red-700;
  }
  .btn.u {
    @apply text-muted-foreground;
  }

  .expr {
    @apply text-xs bg-muted rounded p-2 overflow-x-auto;
  }

  .result {
    @apply text-xs mt-2;
  }
</style>
