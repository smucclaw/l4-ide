<!-- Adopted from the SF DefaultNode implementation
https://github.com/xyflow/xyflow/blob/migrate/svelte5/packages/svelte/src/lib/components/nodes/DefaultNode.svelte
-->
<script lang="ts">
  import type { BoolVarDisplayerProps } from '../svelteflow-types.js'
  import { defaultSFHandlesInfo } from '../svelteflow-types.js'
  import { Handle } from '@xyflow/svelte'

  let { data }: BoolVarDisplayerProps = $props()
</script>

<!-- Need to use data.value instead of value to maintain reactivity -->
<div
  class={[
    'svelte-flow__node-basic bool-var-node-border',
    ...data.value.getClasses(),
  ]}
>
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
