<script lang="ts">
  import {
    BezierEdge,
    // EdgeLabel,
  } from '@xyflow/svelte'
  import type { LadderEdgeProps } from '../types.svelte.js'
  import { defaultLadderEdgeData } from '../types.svelte.js'

  let {
    sourceX,
    sourceY,
    sourcePosition,
    targetX,
    targetY,
    targetPosition,
    data = defaultLadderEdgeData,
  }: LadderEdgeProps = $props()

  // console.log(`var(${data.strokeColorCSSVar})`)
  const pathStyle = `stroke: var(${data.strokeColorCSSVar});`
  console.log(pathStyle)
</script>

<!-- Simple Bezier Edge version -->
<!-- {labelStyle} -->
<!-- {markerStart} -->
<!-- {markerEnd} -->
<BezierEdge
  {sourceX}
  {sourceY}
  {sourcePosition}
  {targetX}
  {targetY}
  {targetPosition}
  label={data.label}
  pathOptions={{ curvature: 1 }}
  style={pathStyle}
/>

<!-- 
Relevant docs / examples
* Their repo: xyflow/examples/svelte/src/routes/examples/edges/CustomBezierEdge.svelte
* https://next.svelteflow.dev/examples/edges/custom-edges
* https://next.svelteflow.dev/examples/edges/edge-labels
-->
