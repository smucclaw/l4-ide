<script lang="ts">
  import { Handle } from '@xyflow/svelte'
  import {
    defaultSFHandlesInfo,
    type GroupingNodeDisplayerProps,
  } from '../types.svelte.js'

  let { data }: GroupingNodeDisplayerProps = $props()
</script>

<div class="grouping-node relative">
  <Handle
    type="source"
    style="opacity:0;"
    position={defaultSFHandlesInfo.sourcePosition}
  />
  <div class="annotation">{data.annotation}</div>
  <!-- The bit of text is there to improve the layouting -->
  <div style="opacity:0;">-</div>
  <Handle
    type="target"
    style="opacity:0.3;"
    position={defaultSFHandlesInfo.targetPosition}
  />
</div>
