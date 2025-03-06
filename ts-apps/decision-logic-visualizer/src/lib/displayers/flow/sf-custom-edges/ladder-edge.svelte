<script lang="ts">
  import {
    BezierEdge,
    // EdgeLabel,
  } from '@xyflow/svelte'
  import type { LadderEdgeProps } from '../types.svelte.js'
  import { defaultLadderEdgeAttrs } from '../types.svelte.js'

  let {
    sourceX,
    sourceY,
    sourcePosition,
    targetX,
    targetY,
    targetPosition,
    data = defaultLadderEdgeAttrs,
  }: LadderEdgeProps = $props()

  // stroke-width: 2px;
  // TODO: try using a css class instead; might be able to avoid the !important that way
  const pathStyle = `stroke: var(${data.strokeColorCSSVar}) !important;`
  console.log(pathStyle)
</script>

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
