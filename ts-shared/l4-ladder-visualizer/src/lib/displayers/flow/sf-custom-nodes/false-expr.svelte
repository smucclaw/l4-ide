<script lang="ts">
  import { FalseExprLirNode } from '$lib/layout-ir/ladder-graph/ladder.svelte.js'
  import type { LadderNodeDisplayerProps } from '../svelteflow-types.js'
  import WithNormalHandles from '$lib/displayers/flow/helpers/with-normal-handles.svelte'
  import WithNonBundlingNodeBaseStyles from '$lib/displayers/flow/helpers/with-non-bundling-node-base-styles.svelte'
  import IsViableIndicator from '$lib/displayers/flow/helpers/is-viable-indicator.svelte'
  import ValueIndicator from '$lib/displayers/flow/helpers/value-indicator.svelte'
  import WithSelectableNodeContextMenu from '$lib/displayers/flow/helpers/with-selectable-node-context-menu.svelte'
  import { useLadderEnv } from '$lib/ladder-env.js'
  import { isNNFLadderGraphLirNode } from '$lib/layout-ir/ladder-graph/ladder.svelte.js'

  let { data }: LadderNodeDisplayerProps = $props()

  const ladderEnv = useLadderEnv()
  const ladderGraph = ladderEnv
    .getTopFunDeclLirNode(data.context)
    .getBody(data.context)

  const maybeHighlightedStyle = $derived(
    isNNFLadderGraphLirNode(ladderGraph) &&
      ladderGraph.nodeIsSelected(data.context, data.node as FalseExprLirNode)
      ? 'highlighted-ladder-node'
      : ''
  )
</script>

{#snippet coreFalseExprUI()}
  <div class="label-wrapper-for-content-bearing-sf-node">
    {(data.node as FalseExprLirNode).toPretty(data.context)}
  </div>
{/snippet}

<!-- select-none to prevent text selection, to hint that this is a constant.
 Also no cursor-pointer -->
<WithNonBundlingNodeBaseStyles>
  <IsViableIndicator context={data.context} node={data.node}>
    <ValueIndicator
      value={(data.node as FalseExprLirNode).getValue(data.context)}
      additionalClasses={[
        'bool-lit-node-border',
        'select-none',
        maybeHighlightedStyle,
      ]}
    >
      <WithNormalHandles>
        {#if isNNFLadderGraphLirNode(ladderGraph)}
          <WithSelectableNodeContextMenu
            context={data.context}
            node={data.node as FalseExprLirNode}
            {ladderGraph}
          >
            {@render coreFalseExprUI()}
          </WithSelectableNodeContextMenu>
        {:else}
          {@render coreFalseExprUI()}
        {/if}
      </WithNormalHandles>
    </ValueIndicator>
  </IsViableIndicator>
</WithNonBundlingNodeBaseStyles>
