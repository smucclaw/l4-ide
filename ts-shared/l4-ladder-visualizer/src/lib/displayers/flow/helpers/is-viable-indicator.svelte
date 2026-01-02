<script lang="ts" module>
  import type { Snippet } from 'svelte'
  import { useLadderEnv } from '$lib/ladder-env.js'
  import type { LirContext } from '@repo/layout-ir'
  import type { LadderLirNode } from '$lib/layout-ir/ladder-graph/ladder.svelte.js'

  import { FadedNodeCSSClass } from '$lib/layout-ir/ladder-graph/node-styles.js'
  import {
    IrrelevantNodeCSSClass,
    ShortCircuitedNodeCSSClass,
  } from '$lib/layout-ir/ladder-graph/node-styles.js'

  interface IsViableIndicatorProps {
    context: LirContext
    node: LadderLirNode
    children: Snippet
  }
</script>

<script lang="ts">
  let { context, node, children }: IsViableIndicatorProps = $props()

  const ladderEnv = useLadderEnv()
  const ladderGraph = ladderEnv.getTopFunDeclLirNode(context).getBody(context)
</script>

<div
  class={[
    'ladder-viability-transition',
    ladderGraph.nodeIsInNonViableSubgraph(context, node)
      ? FadedNodeCSSClass
      : '',
    ladderGraph.nodeIsInIrrelevantSubgraph(context, node)
      ? IrrelevantNodeCSSClass
      : '',
    ladderGraph.nodeIsInShortCircuitedSubgraph(context, node)
      ? ShortCircuitedNodeCSSClass
      : '',
  ].join(' ')}
>
  {@render children()}
</div>
