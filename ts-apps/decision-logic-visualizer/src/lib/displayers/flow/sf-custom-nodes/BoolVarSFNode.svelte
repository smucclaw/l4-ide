<!-- Adopted from the SF DefaultNode implementation
https://github.com/xyflow/xyflow/blob/migrate/svelte5/packages/svelte/src/lib/components/nodes/DefaultNode.svelte

TODO: Will add UI for things like Value in the next iteration
-->
<script lang="ts">
  import type { BoolVarDisplayerProps } from '../types.svelte.js'
  import { defaultSFHandlesInfo } from '../types.svelte.js'
  import { Handle } from '@xyflow/svelte'

  let { data }: BoolVarDisplayerProps = $props()
</script>

<div class="bool-var-node">
  <Handle type="target" position={defaultSFHandlesInfo.targetPosition} />
  <div class="px-2.5 min-w-[10ch] leading-normal">
    {data.label}
  </div>
  <Handle type="source" position={defaultSFHandlesInfo.sourcePosition} />
</div>
