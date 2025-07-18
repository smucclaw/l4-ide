<!--    
  How this could be extended in the future:
  - Support more than just boolean arguments; have a form UI
  - Allow for non-atomic expressions to be supplied as arguments (this would likely require subflows)
 -->
<script lang="ts">
  import type { LadderNodeDisplayerProps } from '../svelteflow-types.js'
  import { cycle } from '$lib/eval/type.js'
  import { useLadderEnv } from '$lib/ladder-env.js'
  import {
    isNNFLadderGraphLirNode,
    type AppArgLirNode,
    type AppLirNode,
  } from '$lib/layout-ir/ladder-graph/ladder.svelte.js'

  import WithNormalHandles from '$lib/displayers/flow/helpers/with-normal-handles.svelte'
  import WithNonBundlingNodeBaseStyles from '$lib/displayers/flow/helpers/with-non-bundling-node-base-styles.svelte'
  import WithSelectableNodeContextMenu from '$lib/displayers/flow/helpers/with-selectable-node-context-menu.svelte'
  import IsViableIndicator from '$lib/displayers/flow/helpers/is-viable-indicator.svelte'
  import ValueIndicator from '$lib/displayers/flow/helpers/value-indicator.svelte'

  let { data }: LadderNodeDisplayerProps = $props()

  const ladderGraph = useLadderEnv()
    .getTopFunDeclLirNode(data.context)
    .getBody(data.context)
  const node = data.node as AppLirNode

  const maybeHighlightedStyle = $derived(
    isNNFLadderGraphLirNode(ladderGraph) &&
      ladderGraph.nodeIsSelected(data.context, node as AppLirNode)
      ? 'highlighted-ladder-node'
      : ''
  )
</script>

<!---------------------------------------------------
               Core App UI
--------------------------------------------------->
{#snippet coreAppUI()}
  <div
    class="flex flex-col gap-2 label-wrapper-for-content-bearing-sf-node p-4"
  >
    <!-- Function name -->
    <div class="font-bold text-[1.1rem]">
      {node.getFnName(data.context).label}
    </div>
    <!-- Args (see also note above)-->
    <div class="flex flex-wrap gap-1 justify-center">
      {#each node.getArgs(data.context) as arg}
        {@render argUI(arg)}
      {/each}
    </div>
  </div>
{/snippet}

<!---------------------------------------------------
               App Arg UI
--------------------------------------------------->
{#snippet argUI(arg: AppArgLirNode)}
  <IsViableIndicator context={data.context} node={arg}>
    <ValueIndicator
      value={arg.getValue(data.context, ladderGraph)}
      additionalClasses={['border', 'border-black', 'rounded-lg']}
    >
      <!-- Yes, we need cursor-pointer here. -->
      <button
        class={['p-2', 'text-xs', 'cursor-pointer']}
        onclick={async () => {
          console.log('clicked: ', arg.getLabel(data.context), arg.getId())

          const newValue = cycle(arg.getValue(data.context, ladderGraph))
          await ladderGraph.submitNewBinding(data.context, {
            unique: arg.getUnique(data.context),
            value: newValue,
          })
        }}
      >
        {arg.getLabel(data.context)}
      </button>
    </ValueIndicator>
  </IsViableIndicator>
{/snippet}

<WithNonBundlingNodeBaseStyles>
  <IsViableIndicator context={data.context} {node}>
    <!-- bg-gray
   to evoke the idea of a fn being a 'black box'
  (but not using solid black b/c don't want too much contrast between this and a uboolvarnode) -->
    <!-- TODO: Add a value indicator for the App itself 
         NOTE: We do NOT want cursor-pointer for the App UI itself.
    -->
    <div
      class={[
        'bg-gray-100 app-node-border',
        // It's easier if the highlighted border styles are on the same element as the normal border styles.
        // TODO: This could prob be cleaner.
        maybeHighlightedStyle,
      ]}
    >
      <WithNormalHandles>
        {#if isNNFLadderGraphLirNode(ladderGraph)}
          <WithSelectableNodeContextMenu
            context={data.context}
            {node}
            {ladderGraph}
          >
            {@render coreAppUI()}
          </WithSelectableNodeContextMenu>
        {:else}
          {@render coreAppUI()}
        {/if}
      </WithNormalHandles>
    </div>
  </IsViableIndicator>
</WithNonBundlingNodeBaseStyles>

<style>
  .app-node-border {
    border: calc(var(--ladder-node-border-width) + 1px) solid
      var(--ladder-node-border-color);
    border-radius: 20px;
  }
</style>
