<script lang="ts">
  import { defaultSFHandlesInfo } from '../svelteflow-types.js'
  import type { NodeProps, Node } from '@xyflow/svelte'
  import { Handle } from '@xyflow/svelte'

  // eslint-disable-next-line no-empty-pattern
  let {}: NodeProps<Node> = $props()

  const graphicSize = 84
</script>

<div class="svelte-flow__node-basic content-bearing-sf-node-border">
  <Handle
    type="target"
    position={defaultSFHandlesInfo.targetPosition}
    style="opacity:0.3"
  />
  <!-- <div class="text-3xl fontmono"><pre>{'>'}</pre></div> -->
  <svg
    width="90%"
    height={graphicSize * 0.45}
    viewBox="0 0 60 100"
    xmlns="http://www.w3.org/2000/svg"
  >
    <!-- Right Angle Bracket '>' -->
    <path
      d="M0,0 L60,50 L0,100"
      fill="none"
      stroke="var(--xy-node-color)"
      stroke-width="3"
      stroke-linecap="round"
    />
  </svg>
  <Handle
    type="source"
    position={defaultSFHandlesInfo.sourcePosition}
    style="opacity:0"
  />
</div>
