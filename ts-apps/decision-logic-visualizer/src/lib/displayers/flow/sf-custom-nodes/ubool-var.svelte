<!-- Adopted from the SF DefaultNode implementation
https://github.com/xyflow/xyflow/blob/migrate/svelte5/packages/svelte/src/lib/components/nodes/DefaultNode.svelte
-->
<script lang="ts">
  import type { UBoolVarDisplayerProps } from '../svelteflow-types.js'
  import { useLadderEnv } from '$lib/ladder-env.js'
  import { UBoolVarLirNode } from '$lib/layout-ir/ladder-graph/ladder.svelte.js'
  import * as Tooltip from '$lib/ui-primitives/tooltip/index.js'
  import { cycle } from '$lib/eval/type.js'
  import WithNormalHandles from '$lib/displayers/flow/helpers/with-normal-handles.svelte'

  let { data }: UBoolVarDisplayerProps = $props()

  const ladderEnv = useLadderEnv()
  const l4Conn = ladderEnv.getL4Connection()
</script>

{#snippet inlineUI()}
  <div class="absolute bottom-1 right-1">
    <Tooltip.Provider>
      <Tooltip.Root>
        <Tooltip.Trigger>
          <button
            aria-label="Unfold to definition"
            class="px-0.5 text-[0.625rem] rounded border border-border bg-background hover:bg-accent hover:text-accent-foreground transition-colors duration-150"
            onclick={() => {
              console.log('inline lir id', data.originalLirId.toString())

              const uniq = (
                data.context.get(data.originalLirId) as UBoolVarLirNode
              ).getUnique(data.context)
              l4Conn.inlineExprs(
                [uniq],
                ladderEnv.getVersionedTextDocIdentifier()
              )
            }}
          >
            +
          </button>
        </Tooltip.Trigger>
        <Tooltip.Content>Unfold to definition</Tooltip.Content>
      </Tooltip.Root>
    </Tooltip.Provider>
  </div>
{/snippet}

<!-- Need to use data.bleh to maintain reactivity -- can't, e.g., do `const bleh = data.bleh` 
TODO: Look into why this is the case --- are they not re-mounting the ubool-var component? 
-->
<div
  class={[
    'base-sf-node-styles bool-var-node-border transition-opacity duration-300',
    ...data.classes,
  ]}
>
  <WithNormalHandles>
    <button
      class="label-wrapper-for-content-bearing-sf-node cursor-pointer"
      onclick={() => {
        const ladderGraph = ladderEnv
          .getTopFunDeclLirNode(data.context)
          .getBody(data.context)
        const node = data.context.get(data.originalLirId) as UBoolVarLirNode

        const newValue = cycle(node.getValue(data.context))
        ladderGraph.submitNewBinding(data.context, {
          unique: node.getUnique(data.context),
          value: newValue,
        })
      }}
    >
      {data.name.label}
    </button>
    {#if data.canInline}
      {@render inlineUI()}
    {/if}
  </WithNormalHandles>
</div>

<style>
  .bool-var-node-border {
    border: var(--ladder-node-border, var(--ladder-node-border-default));
    border-radius: var(
      --ladder-node-border-radius,
      var(--ladder-node-border-radius-default)
    );
  }
</style>
