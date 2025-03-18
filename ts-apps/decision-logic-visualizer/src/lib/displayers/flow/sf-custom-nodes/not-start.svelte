<script lang="ts">
  import { defaultSFHandlesInfo } from '../svelteflow-types.js'
  import type { NodeProps, Node } from '@xyflow/svelte'
  import { Handle } from '@xyflow/svelte'

  // eslint-disable-next-line no-empty-pattern
  let {}: NodeProps<Node> = $props()

  const graphicSize = 100
</script>

<div class="svelte-flow__node-basic content-bearing-sf-node-border m-0 p-0">
  <Handle
    type="target"
    position={defaultSFHandlesInfo.targetPosition}
    style="opacity: 0.4"
  />
  <!-- <div>{'<NOT|'}</div> -->
  <svg
    width="92%"
    height={graphicSize * 0.5}
    viewBox="0 0 105 100"
    xmlns="http://www.w3.org/2000/svg"
  >
    <!-- Left Angle Bracket '<'
    -->
    <path
      d="M40,10 L5,50 L40,90"
      fill="none"
      stroke="var(--xy-node-color)"
      stroke-width="3"
      stroke-linecap="round"
    />

    <!-- "NOT" Text -->
    <text
      x="67"
      y="50"
      font-size="42"
      dominant-baseline="middle"
      text-anchor="middle"
    >
      NOT
    </text>

    <!-- Vertical Line '|' -->
    <!-- <line
      x1="100"
      y1="10"
      x2="100"
      y2="90"
      stroke="var(--xy-node-color)"
      stroke-width="2"
      stroke-linecap="round"
    /> -->
  </svg>
  <Handle
    type="source"
    position={defaultSFHandlesInfo.sourcePosition}
    style="opacity: 0.3"
  />
</div>
