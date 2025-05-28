<!--    
  How this could be extended in the future:
  - Support more than just boolean arguments; have a form UI
  - Allow for non-atomic expressions to be supplied as arguments (this would likely require subflows)
 -->
<script lang="ts">
  import type { AppDisplayerProps } from '../svelteflow-types.js'
  import { cycle } from '$lib/eval/type.js'
  import { useLadderEnv } from '$lib/ladder-env.js'
  import WithNormalHandles from '$lib/displayers/flow/helpers/with-normal-handles.svelte'
  import WithContentfulNodeStyles from '$lib/displayers/flow/helpers/with-contentful-node-styles.svelte'
  import ValueIndicator from '$lib/displayers/flow/helpers/value-indicator.svelte'
  import { onDestroy } from 'svelte'
  import type { LirContext, LirId } from '$lib/layout-ir/core.js'

  let { data }: AppDisplayerProps = $props()

  // Get LadderEnv, L4 Connection
  const ladderGraph = useLadderEnv()
    .getTopFunDeclLirNode(data.context)
    .getBody(data.context)

  // The values of the arguments of the App
  const argValues = $state(data.args.map((arg) => arg.getValue(data.context)))
  const onArgValueChange = (context: LirContext, id: LirId) => {
    data.args.forEach((arg, i) => {
      if (id === arg.getId()) {
        argValues[i] = arg.getValue(context)
      }
    })
  }
  const unsub = useLadderEnv().getLirRegistry().subscribe(onArgValueChange)

  onDestroy(unsub.unsubscribe)
</script>

<!-- App Arg UI -->
{#snippet argUI(arg: (typeof data.args)[number], i: number)}
  <ValueIndicator
    value={argValues[i]}
    borderClasses={['border', 'border-black', 'rounded-lg']}
  >
    <button
      class={['p-2', 'text-xs', ...arg.getAllClasses(data.context)]}
      onclick={async () => {
        console.log('clicked: ', arg.getLabel(data.context), arg.getId())

        const newValue = cycle(arg.getValue(data.context))
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

<WithContentfulNodeStyles>
  <!-- bg-gray
 to evoke the idea of a fn being a 'black box'
(but not using solid black b/c don't want too much contrast between this and a uboolvarnode) -->
  <!-- TODO: Add a value indicator for the App itself -->
  <div class={['bg-gray-100 app-node-border', ...data.classes]}>
    <WithNormalHandles>
      <div
        class="flex flex-col gap-2 label-wrapper-for-content-bearing-sf-node p-4"
      >
        <!-- Function name -->
        <div class="font-bold text-[1.1rem]">
          {data.fnName.label}
        </div>
        <!-- Args (see also note above)-->
        <div class="flex flex-wrap gap-1 justify-center">
          {#each data.args as arg, i}
            {@render argUI(arg, i)}
          {/each}
        </div>
      </div>
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
