<script lang="ts">
  import {
    defaultSFHandlesInfo,
    type LadderNodeDisplayerProps,
  } from '../svelteflow-types.js'
  import { Handle } from '@xyflow/svelte'
  import { useLadderEnv } from '$lib/ladder-env.js'
  import {
    isNNFLadderGraphLirNode,
    NotEndLirNode,
  } from '$lib/layout-ir/ladder-graph/ladder.svelte.js'
  import WithNonBundlingNodeBaseStyles from '$lib/displayers/flow/helpers/with-non-bundling-node-base-styles.svelte'

  let { data }: LadderNodeDisplayerProps = $props()

  const ladderGraph = useLadderEnv()
    .getTopFunDeclLirNode(data.context)
    .getBody(data.context)

  // Highlight if the parent NOT group is selected
  // (which in turn will be the case if the NotStart node is selected)
  const isSelected = $derived(
    isNNFLadderGraphLirNode(ladderGraph) &&
      ladderGraph.nodeIsSelected(data.context, data.node as NotEndLirNode)
  )

  const graphicSize = 84
</script>

<WithNonBundlingNodeBaseStyles>
  <div class={[...data.node.getAllClasses(data.context)]}>
    <Handle
      type="target"
      position={defaultSFHandlesInfo.targetPosition}
      style="opacity:0.3"
    />
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
        stroke={isSelected
          ? 'var(--color-highlighted-path-in-flow)'
          : 'var(--ladder-node-color)'}
        stroke-width={isSelected
          ? 'calc(3px + var(--path-highlight-delta))'
          : '3px'}
        stroke-linecap="round"
      />
    </svg>
    <Handle
      type="source"
      position={defaultSFHandlesInfo.sourcePosition}
      style="opacity:0"
    />
  </div>
</WithNonBundlingNodeBaseStyles>
