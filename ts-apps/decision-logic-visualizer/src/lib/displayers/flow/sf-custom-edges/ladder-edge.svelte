<script lang="ts">
  import {
    BezierEdge,
    // EdgeLabel,
  } from '@xyflow/svelte'
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
</script>

<BezierEdge
  {sourceX}
  {sourceY}
  {sourcePosition}
  {targetX}
  {targetY}
  {targetPosition}
  label={data?.context.shouldEnableZenMode() ? undefined : data?.label}
  pathOptions={{ curvature: 1 }}
  style={"transition-property: opacity; transition-duration: 300ms;" + data?.edgeStyles}
/>

<!-- 
Relevant docs / examples
* Their repo: xyflow/examples/svelte/src/routes/examples/edges/CustomBezierEdge.svelte
* https://next.svelteflow.dev/examples/edges/custom-edges
* https://next.svelteflow.dev/examples/edges/edge-labels
-->
