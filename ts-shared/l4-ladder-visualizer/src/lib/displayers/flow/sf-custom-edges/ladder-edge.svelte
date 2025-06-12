<script lang="ts">
  import {
    BezierEdge,
    // EdgeLabel,
  } from '@xyflow/svelte'
  import { useLadderEnv } from '$lib/ladder-env.js'
  import type { LadderEdgeProps } from '../svelteflow-types.js'
  import {
    HighlightedEdgeStyle,
    NonHighlightedEdgeStyle,
    FadedEdgeStyle,
  } from '$lib/layout-ir/ladder-graph/edge-attributes.js'
  import { isNNFLadderGraphLirNode } from '$lib/layout-ir/ladder-graph/ladder.svelte.js'

  let {
    sourceX,
    sourceY,
    sourcePosition,
    targetX,
    targetY,
    targetPosition,
    data,
  }: LadderEdgeProps = $props()

  const ladderEnv = useLadderEnv()
  if (!data) {
    throw new Error('Internal error: data prop not found in LadderEdge')
  }
  const ladderGraph = ladderEnv
    .getTopFunDeclLirNode(data.context)
    .getBody(data.context)

  const maybeFadedEdgeStyle = $derived(
    ladderGraph.edgeIsInNonViableSubgraph(data.context, data.originalEdge)
      ? FadedEdgeStyle
      : ''
  )
</script>

<BezierEdge
  {sourceX}
  {sourceY}
  {sourcePosition}
  {targetX}
  {targetY}
  {targetPosition}
  label={ladderEnv.shouldEnableZenMode() ? undefined : data?.label}
  pathOptions={{ curvature: 1 }}
  style={[
    'transition-property: opacity; transition-duration: 300ms;',
    // highlight edge if it's in the highlighted subgraph
    isNNFLadderGraphLirNode(ladderGraph) &&
    ladderGraph.edgeIsInHighlightedSubgraph(data.context, data.originalEdge)
      ? HighlightedEdgeStyle
      : NonHighlightedEdgeStyle,
    // fade edge if it's in the non-viable subgraph
    maybeFadedEdgeStyle,
  ].join(' ')}
  labelStyle={maybeFadedEdgeStyle}
/>
<!-- TODO: Not sure that the lower opacity look is great for labels -->
<!-- 
Relevant docs / examples
* Their repo: xyflow/examples/svelte/src/routes/examples/edges/CustomBezierEdge.svelte
* https://next.svelteflow.dev/examples/edges/custom-edges
* https://next.svelteflow.dev/examples/edges/edge-labels
-->
