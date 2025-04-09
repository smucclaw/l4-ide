<!-- Adopted from the SF DefaultNode implementation
https://github.com/xyflow/xyflow/blob/migrate/svelte5/packages/svelte/src/lib/components/nodes/DefaultNode.svelte
-->
<script lang="ts">
  import type { BoolVarDisplayerProps } from '../svelteflow-types.js'
  import { defaultSFHandlesInfo } from '../svelteflow-types.js'
  import { Handle } from '@xyflow/svelte'

  let { data }: BoolVarDisplayerProps = $props()
</script>

<!-- Need to use data.bleh to maintain reactivity -- can't, e.g., do `const bleh = data.bleh` -->
<div class={['svelte-flow__node-basic bool-var-node-border transition-opacity duration-300', ...data.classes]}>
  <Handle type="target" position={defaultSFHandlesInfo.targetPosition} />
  <div class="label-wrapper-for-content-bearing-sf-node">
    {data.name.label}
  </div>
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

  .true-val {
    background-color: var(--color-true-value);
  }

  .false-val {
    background-color: var(--color-false-value);
  }
</style>
