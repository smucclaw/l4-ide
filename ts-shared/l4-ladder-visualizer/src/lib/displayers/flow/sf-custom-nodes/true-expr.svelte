<script lang="ts">
  import { TrueExprLirNode } from '$lib/layout-ir/ladder-graph/ladder.svelte.js'
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
      ladderGraph.nodeIsSelected(data.context, data.node as TrueExprLirNode)
      ? 'highlighted-ladder-node'
      : ''
  )
</script>

{#snippet coreTrueExprUI()}
  <div class="label-wrapper-for-content-bearing-sf-node">
    {(data.node as TrueExprLirNode).toPretty(data.context)}
  </div>
{/snippet}

<!-- select-none to prevent text selection, to hint that this is a constant.
 Also no cursor-pointer -->
<WithNonBundlingNodeBaseStyles>
  <IsViableIndicator context={data.context} node={data.node}>
    <ValueIndicator
      value={(data.node as TrueExprLirNode).getValue(data.context)}
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
            node={data.node as TrueExprLirNode}
            {ladderGraph}
          >
            {@render coreTrueExprUI()}
          </WithSelectableNodeContextMenu>
        {:else}
          {@render coreTrueExprUI()}
        {/if}
      </WithNormalHandles>
    </ValueIndicator>
  </IsViableIndicator>
</WithNonBundlingNodeBaseStyles>
