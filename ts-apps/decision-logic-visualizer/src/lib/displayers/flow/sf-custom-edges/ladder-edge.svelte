<script lang="ts">
  import {
    BezierEdge,
    // EdgeLabel,
  } from '@xyflow/svelte'
  import { useLadderEnv } from '$lib/ladder-env.js'
  import type { LadderEdgeProps } from '../svelteflow-types.js'

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
  style={'transition-property: opacity; transition-duration: 300ms;' +
    data?.edgeStyles}
  labelStyle={data?.labelStyles}
/>
<!-- TODO: Not sure that the lower opacity look is great for labels -->
<!-- 
Relevant docs / examples
* Their repo: xyflow/examples/svelte/src/routes/examples/edges/CustomBezierEdge.svelte
* https://next.svelteflow.dev/examples/edges/custom-edges
* https://next.svelteflow.dev/examples/edges/edge-labels
-->
