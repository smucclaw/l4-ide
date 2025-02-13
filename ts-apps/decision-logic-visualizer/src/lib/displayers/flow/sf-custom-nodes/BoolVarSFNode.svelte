<!-- Adopted from the SF DefaultNode implementation
https://github.com/xyflow/xyflow/blob/migrate/svelte5/packages/svelte/src/lib/components/nodes/DefaultNode.svelte

TODO: Will add UI for things like Value in the next iteration
-->
<script lang="ts">
  import type { BoolVarDisplayerProps } from '../types.svelte.js'
  import { defaultSFHandlesInfo } from '../types.svelte.js'
  import { Handle } from '@xyflow/svelte'

  let {
    data,
    // targetPosition = Position.Top,
    // sourcePosition = Position.Bottom,
  }: BoolVarDisplayerProps = $props()
</script>

<Handle type="target" position={defaultSFHandlesInfo.targetPosition} />
{data.label}
<Handle type="source" position={defaultSFHandlesInfo.sourcePosition} />
