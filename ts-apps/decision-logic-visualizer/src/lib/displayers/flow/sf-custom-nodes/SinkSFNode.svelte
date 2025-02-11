<!--
TODO: maybe reduce code duplication between this and SourceSFNode
-->
<script lang="ts">
  import { Handle } from '@xyflow/svelte'
  import type { NodeProps, Node } from '@xyflow/svelte'
  import {
    groupingNodehandleStyle,
    defaultSFHandlesInfo,
  } from '../types.svelte.js'
  // eslint-disable-next-line no-empty-pattern
  let {}: NodeProps<Node> = $props()
</script>

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
