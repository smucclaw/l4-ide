<!-- Adopted from the SF DefaultNode implementation
https://github.com/xyflow/xyflow/blob/migrate/svelte5/packages/svelte/src/lib/components/nodes/DefaultNode.svelte
-->
<script lang="ts">
  import type { AppDisplayerProps } from '../svelteflow-types.js'
  import { defaultSFHandlesInfo } from '../svelteflow-types.js'
  import { Handle } from '@xyflow/svelte'
  import { cycle } from '$lib/eval/type.js'
  import { useLadderEnv } from '$lib/ladder-env.js'
  let { data }: AppDisplayerProps = $props()

  const ladderGraph = useLadderEnv()
    .getTopFunDeclLirNode(data.context)
    .getBody(data.context)
</script>

<!-- bg-gray
 to evoke the idea of a fn being a 'black box'
(but not using solid black b/c don't want too much contrast between this and a uboolvarnode) -->
<div
  class={[
    'svelte-flow__node-basic bg-gray-100 app-node-border transition-opacity duration-300',
    ...data.classes,
  ]}
>
  <Handle type="target" position={defaultSFHandlesInfo.targetPosition} />

  <div
    class="flex flex-col gap-2 label-wrapper-for-content-bearing-sf-node p-4"
  >
    <div class="font-bold text-[1.1rem]">
      {data.fnName.label}
    </div>

    <!-- The code below isn't the cleanest -- this is a simple prototype that will likely be replaced down the road.

    How this could be extended in the future:
    - Support more than just boolean arguments; have a form UI
    - Allow for non-atomic expressions to be supplied as arguments (this would likely require subflows)
    -->
    <div class="flex flex-wrap gap-1 justify-center">
      {#each data.args as arg}
        <button
          class={[
            'border',
            'border-black',
            'p-2',
            'text-xs',
            'rounded-lg',
            'cursor-pointer',
            'bg-white',
            ...arg.getAllClasses(data.context),
          ]}
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
      {/each}
    </div>
  </div>

  <Handle type="source" position={defaultSFHandlesInfo.sourcePosition} />
</div>

<style>
  .app-node-border {
    border: calc(var(--ladder-node-border-width) + 1px) solid
      var(--color-primary);
    border-radius: 20px;
  }
</style>
