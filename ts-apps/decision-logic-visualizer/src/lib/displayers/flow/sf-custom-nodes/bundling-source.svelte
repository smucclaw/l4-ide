<script lang="ts">
  import { Handle } from '@xyflow/svelte'
  import {
    type BundlingNodeDisplayerProps,
    defaultSFHandlesInfo,
  } from '../svelteflow-types.js'

  let { data }: BundlingNodeDisplayerProps = $props()
</script>

<div class="bundling-node relative">
  <Handle
    type="source"
    style="opacity:0;"
    position={defaultSFHandlesInfo.sourcePosition}
  />
  {#if !data.ladderEnv.shouldEnableZenMode()}
    <div class={['node-annotation', ...data.node.getAllClasses(data.context)]}>
      {data.annotation}
    </div>
  {/if}
  <!-- The bit of text is there to improve the layouting -->
  <div style="opacity:0;">-</div>
  <Handle
    type="target"
    style="opacity:0.3;"
    position={defaultSFHandlesInfo.targetPosition}
  />
</div>
