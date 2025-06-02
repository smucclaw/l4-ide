<!--    
  How this could be extended in the future:
  - Support more than just boolean arguments; have a form UI
  - Allow for non-atomic expressions to be supplied as arguments (this would likely require subflows)
 -->
<script lang="ts">
  import type { LadderNodeDisplayerProps } from '../svelteflow-types.js'
  import { cycle } from '$lib/eval/type.js'
  import { useLadderEnv } from '$lib/ladder-env.js'
  import type { AppLirNode } from '$lib/layout-ir/ladder-graph/ladder.svelte.js'
  import WithNormalHandles from '$lib/displayers/flow/helpers/with-normal-handles.svelte'
  import WithContentfulNodeStyles from '$lib/displayers/flow/helpers/with-contentful-node-styles.svelte'
  import WithHighlightableNodeContextMenu from '$lib/displayers/flow/helpers/with-highlightable-node-context-menu.svelte'
  import ValueIndicator from '$lib/displayers/flow/helpers/value-indicator.svelte'
  import type {
    AppArgLirNode,
    AppLirNode,
  } from '$lib/layout-ir/ladder-graph/ladder.svelte.js'

  let { data }: LadderNodeDisplayerProps = $props()

  const ladderGraph = useLadderEnv()
    .getTopFunDeclLirNode(data.context)
    .getBody(data.context)
  const pathsTracker = ladderGraph.getPathsTracker(data.context)

  const node = data.node as AppLirNode
</script>

<!-- App Arg UI -->
{#snippet argUI(arg: AppArgLirNode)}
  <ValueIndicator
    value={arg.getValue(data.context, ladderGraph)}
    additionalClasses={[
      'border',
      'border-black',
      'rounded-lg',
      ...arg.getAllClasses(data.context),
    ]}
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
{/snippet}

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

<WithContentfulNodeStyles>
  <!-- bg-gray
 to evoke the idea of a fn being a 'black box'
(but not using solid black b/c don't want too much contrast between this and a uboolvarnode) -->
  <!-- TODO: Add a value indicator for the App itself 
       NOTE: We do NOT want cursor-pointer for the App UI itself.
  -->
  <div
    class={[
      'bg-gray-100 app-node-border',
      ...data.node.getAllClasses(data.context),
    ]}
  >
    <WithNormalHandles>
      {#if pathsTracker}
        <WithHighlightableNodeContextMenu
          onSelect={() => {
            pathsTracker.toggleNodeSelection(
              data.context,
              data.context.get(data.originalLirId) as AppLirNode,
              ladderGraph
            )
          }}
        >
          {@render coreAppUI()}
        </WithHighlightableNodeContextMenu>
      {/if}
    </WithNormalHandles>
  </div>
</WithContentfulNodeStyles>

<style>
  .app-node-border {
    border: calc(var(--ladder-node-border-width) + 1px) solid
      var(--color-primary);
    border-radius: 20px;
  }
</style>
