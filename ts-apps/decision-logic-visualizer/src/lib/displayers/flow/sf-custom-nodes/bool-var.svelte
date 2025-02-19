<!-- Adopted from the SF DefaultNode implementation
https://github.com/xyflow/xyflow/blob/migrate/svelte5/packages/svelte/src/lib/components/nodes/DefaultNode.svelte

TODO: Will add UI for things like Value in the next iteration
-->
<script lang="ts">
  import type { BoolVarDisplayerProps } from '../types.svelte.js'
  import { defaultSFHandlesInfo } from '../types.svelte.js'
  import { Handle } from '@xyflow/svelte'
  // import { match } from 'ts-pattern'

  let { data }: BoolVarDisplayerProps = $props()
</script>

<div class="svelte-flow__node-basic bool-var-node-border">
  <Handle type="target" position={defaultSFHandlesInfo.targetPosition} />
  <div class="label-wrapper-for-content-bearing-sf-node">
    {data.name.label}
  </div>
  <Handle type="source" position={defaultSFHandlesInfo.sourcePosition} />
</div>

<!-- match(originalExpr.value)
  .with('True', () => true)
  .with('False', () => false)
  .with('Unknown', () => undefined)
  .exhaustive() -->

<style>
  .bool-var-node-border {
    border: var(--xy-node-border, var(--xy-node-border-default));
    border-radius: var(
      --xy-node-border-radius,
      var(--xy-node-border-radius-default)
    );
  }
</style>
