<script lang="ts">
  import { InertExprLirNode } from '$lib/layout-ir/ladder-graph/ladder.svelte.js'
  import type { LadderNodeDisplayerProps } from '../svelteflow-types.js'
  import WithNormalHandles from '$lib/displayers/flow/helpers/with-normal-handles.svelte'
  import WithNonBundlingNodeBaseStyles from '$lib/displayers/flow/helpers/with-non-bundling-node-base-styles.svelte'
  import ValueIndicator from '$lib/displayers/flow/helpers/value-indicator.svelte'
  import IsViableIndicator from '$lib/displayers/flow/helpers/is-viable-indicator.svelte'
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
      ladderGraph.nodeIsSelected(data.context, data.node as InertExprLirNode)
      ? 'highlighted-ladder-node'
      : ''
  )
</script>

{#snippet coreInertExprUI()}
  <div class="label-wrapper-for-content-bearing-sf-node">
    {(data.node as InertExprLirNode).toPretty(data.context)}
  </div>
{/snippet}

<!-- select-none to prevent text selection, to hint that this is a constant.
 Also no cursor-pointer -->
<WithNonBundlingNodeBaseStyles>
  <IsViableIndicator context={data.context} node={data.node}>
    <ValueIndicator
      value={(data.node as InertExprLirNode).getValue(data.context)}
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
            node={data.node as InertExprLirNode}
            {ladderGraph}
          >
            {@render coreInertExprUI()}
          </WithSelectableNodeContextMenu>
        {:else}
          {@render coreInertExprUI()}
        {/if}
      </WithNormalHandles>
    </ValueIndicator>
  </IsViableIndicator>
</WithNonBundlingNodeBaseStyles>
