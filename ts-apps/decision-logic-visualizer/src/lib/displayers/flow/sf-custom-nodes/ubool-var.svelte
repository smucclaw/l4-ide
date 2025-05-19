<!-- Adopted from the SF DefaultNode implementation
https://github.com/xyflow/xyflow/blob/migrate/svelte5/packages/svelte/src/lib/components/nodes/DefaultNode.svelte
-->
<script lang="ts">
  import type { UBoolVarDisplayerProps } from '../svelteflow-types.js'
  import { defaultSFHandlesInfo } from '../svelteflow-types.js'
  import { useLadderEnv } from '$lib/ladder-env.js'
  import { UBoolVarLirNode } from '$lib/layout-ir/ladder-graph/ladder.svelte.js'
  import { Handle } from '@xyflow/svelte'

  let { data }: UBoolVarDisplayerProps = $props()

  const ladderEnv = useLadderEnv()
  const l4Conn = ladderEnv.getL4Connection()
</script>

<!-- Need to use data.bleh to maintain reactivity -- can't, e.g., do `const bleh = data.bleh` 
TODO: Look into why this is the case --- are they not re-mounting the ubool-var component? 
-->
<div
  class={[
    'svelte-flow__node-basic bool-var-node-border transition-opacity duration-300',
    ...data.classes,
  ]}
>
  <Handle type="target" position={defaultSFHandlesInfo.targetPosition} />
  <div class="label-wrapper-for-content-bearing-sf-node cursor-pointer">
    {data.name.label}
  </div>
  {#if data.canInline}
    <div class="absolute bottom-1 right-1">
      <button
        class="px-1 text-xs rounded border border-border bg-background hover:bg-accent hover:text-accent-foreground transition-colors duration-150"
        onclick={() => {
          console.log('inline lir id', data.originalLirId.toString())

          const uniq = (
            data.context.get(data.originalLirId) as UBoolVarLirNode
          ).getUnique(data.context)
          l4Conn.inlineExprs([uniq], ladderEnv.getVersionedTextDocIdentifier())
        }}
      >
        jh todo
      </button>
    </div>
  {/if}
  <Handle type="source" position={defaultSFHandlesInfo.sourcePosition} />
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
