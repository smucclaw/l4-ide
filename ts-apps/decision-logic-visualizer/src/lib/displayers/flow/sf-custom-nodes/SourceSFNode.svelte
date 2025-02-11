<script lang="ts">
  import { Handle } from '@xyflow/svelte'
  import type { NodeProps, Node } from '@xyflow/svelte'
  import {
    groupingNodehandleStyle,
    defaultSFHandlesInfo,
  } from '../types.svelte.js'
  // They require a data prop, but we don't need it
  let { data }: NodeProps<Node> = $props() // eslint-disable-line
</script>

<!-- See Meng's layman for this 1px thing -->
<div style="opacity:0.4">
  <Handle
    type="source"
    position={defaultSFHandlesInfo.sourcePosition}
    style={groupingNodehandleStyle}
  />
  <!-- The bit of text is there to improve the layouting -->
  <div style="opacity:0">-</div>
  <Handle
    type="target"
    position={defaultSFHandlesInfo.targetPosition}
    style={groupingNodehandleStyle}
  />
</div>
