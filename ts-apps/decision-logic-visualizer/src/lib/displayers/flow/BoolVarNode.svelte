<!-- Adopted from the SF DefaultNode implementation
https://github.com/xyflow/xyflow/blob/migrate/svelte5/packages/svelte/src/lib/components/nodes/DefaultNode.svelte

TODO: Will add UI for things like Value in the next iteration
-->
<script lang="ts">
  import type { BoolVarDisplayerProps } from './types.svelte'
  import { Handle, Position } from '@xyflow/svelte'

  const defaultHandlesPosition = {
    sourcePosition: Position.Bottom,
    targetPosition: Position.Top,
  }
  let { data, handlesInfo = defaultHandlesPosition }: BoolVarDisplayerProps =
    $props()
</script>

<Handle type="target" position={handlesInfo.targetPosition} />
{data.label}
<Handle type="source" position={handlesInfo.sourcePosition} />
