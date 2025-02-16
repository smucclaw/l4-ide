<script lang="ts">
  import { Handle } from '@xyflow/svelte'
  import type { NodeProps, Node } from '@xyflow/svelte'
  import { defaultSFHandlesInfo } from '../types.svelte.js'
  // eslint-disable-next-line no-empty-pattern
  let {}: NodeProps<Node> = $props()
</script>

<div class="grouping-node">
  <Handle type="source" position={defaultSFHandlesInfo.sourcePosition} />
  <!-- The bit of text is there to improve the layouting -->
  <div style="opacity:0;">-</div>
  <Handle type="target" position={defaultSFHandlesInfo.targetPosition} />
</div>
