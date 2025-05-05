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

<div
  class={[
    'svelte-flow__node-basic app-node-border transition-opacity duration-300',
    ...data.classes,
  ]}
>
  <Handle type="target" position={defaultSFHandlesInfo.targetPosition} />

  <div class="flex flex-col gap-2 label-wrapper-for-content-bearing-sf-node">
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
            'border-gray-400',
            'p-2',
            'rounded-lg',
            ...arg.getAllClasses(data.context),
          ]}
          onclick={() => {
            console.log('clicked: ', arg.getLabel(data.context), arg.getId())

            const newValue = cycle(arg.getValue(data.context))
            ladderGraph.submitNewBinding(data.context, {
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
  /* TODO: Consider making a util class if we are going to have the same border for all content-bearing nodes */
  .app-node-border {
    border: var(--ladder-node-border, var(--ladder-node-border-default));
    border-radius: var(
      --ladder-node-border-radius,
      var(--ladder-node-border-radius-default)
    );
  }

  .true-val {
    background-color: var(--color-true-value);
  }
  .true-val::before {
    content: '✓';
    margin-right: 0.25rem;
  }

  .false-val {
    background-color: var(--color-false-value);
  }
  .false-val::before {
    content: '✗';
    margin-right: 0.25rem;
  }
</style>
