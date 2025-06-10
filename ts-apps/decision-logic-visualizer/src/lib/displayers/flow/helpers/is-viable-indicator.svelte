<script lang="ts" module>
  import type { Snippet } from 'svelte'
  import { useLadderEnv } from '$lib/ladder-env.js'
  import type { LirContext } from '$lib/layout-ir/core'
  import type { LadderLirNode } from '$lib/layout-ir/ladder-graph/ladder.svelte.js'

  import { FadedNodeCSSClass } from '$lib/layout-ir/ladder-graph/node-styles.js'

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
  class={ladderGraph.nodeIsInNonViableSubgraph(context, node)
    ? FadedNodeCSSClass
    : ''}
>
  {@render children()}
</div>
