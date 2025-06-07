<script lang="ts">
  import {
    defaultSFHandlesInfo,
    type LadderNodeDisplayerProps,
  } from '../svelteflow-types.js'
  import { Handle } from '@xyflow/svelte'
  import { useLadderEnv } from '$lib/ladder-env.js'
  import {
    type LadderLirNode,
    isNNFLadderGraphLirNode,
    isSelectableLadderLirNode,
    NotStartLirNode,
  } from '$lib/layout-ir/ladder-graph/ladder.svelte.js'
  import WithNonBundlingNodeBaseStyles from '$lib/displayers/flow/helpers/with-non-bundling-node-base-styles.svelte'
  import WithSelectableNodeContextMenu from '$lib/displayers/flow/helpers/with-selectable-node-context-menu.svelte'

  let { data }: LadderNodeDisplayerProps = $props()

  const ladderGraph = useLadderEnv()
    .getTopFunDeclLirNode(data.context)
    .getBody(data.context)

  const isSelected = $derived(
    isNNFLadderGraphLirNode(ladderGraph) &&
      ladderGraph.nodeIsSelected(data.context, data.node as NotStartLirNode)
  )

  const graphicSize = 100
</script>

{#snippet coreNotStartUI()}
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
      stroke={isSelected
        ? 'var(--color-highlighted-path-in-flow)'
        : 'var(--ladder-node-color)'}
      stroke-width={isSelected ? '4' : '3'}
      stroke-linecap="round"
    />

    <!-- "NOT" Text -->
    <text
      x="67"
      y="50"
      font-size="42"
      dominant-baseline="middle"
      text-anchor="middle"
      fill={isSelected
        ? 'var(--color-highlighted-path-in-flow)'
        : 'var(--ladder-node-color)'}
    >
      NOT
    </text>

    <!-- Vertical Line '|' -->
    <!-- <line
x1="100"
y1="10"
x2="100"
y2="90"
stroke="var(--ladder-node-color)"
stroke-width="2"
stroke-linecap="round"
/> -->
  </svg>
{/snippet}

<WithNonBundlingNodeBaseStyles>
  <div class={['m-0 p-0', ...data.node.getAllClasses(data.context)]}>
    <Handle
      type="target"
      position={defaultSFHandlesInfo.targetPosition}
      style="opacity: 0.4"
    />
    {#if isNNFLadderGraphLirNode(ladderGraph)}
      <WithSelectableNodeContextMenu
        context={data.context}
        node={data.node as NotStartLirNode}
        {ladderGraph}
        onSelect={() => {
          const notGraph = (data.node as NotStartLirNode).getWholeNotGraph(
            data.context,
            ladderGraph
          )
          if (!notGraph) {
            throw new Error('Not graph not found')
          }
          const nodes = notGraph
            .getVertices()
            .map((v) => data.context.get(v))
            .filter((node): node is LadderLirNode => node !== undefined)
            .filter(isSelectableLadderLirNode)
          nodes.forEach((node) => {
            // TODO: Might not want to actually select the negand too
            ladderGraph.toggleNodeSelection(data.context, node)
          })
        }}
      >
        {@render coreNotStartUI()}
      </WithSelectableNodeContextMenu>
    {:else}
      {@render coreNotStartUI()}
    {/if}
    <Handle
      type="source"
      position={defaultSFHandlesInfo.sourcePosition}
      style="opacity: 0.3"
    />
  </div>
</WithNonBundlingNodeBaseStyles>
