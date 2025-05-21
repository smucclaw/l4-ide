<script lang="ts">
  import { TrueExprLirNode } from '$lib/layout-ir/ladder-graph/ladder.svelte.js'
  import type { BoolLitDisplayerProps } from '../svelteflow-types.js'
  import { defaultSFHandlesInfo } from '../svelteflow-types.js'
  import { Handle } from '@xyflow/svelte'

  let { data }: BoolLitDisplayerProps = $props()
</script>

<!-- select-none to prevent text selection, to hint that this is a constant.
 Also no cursor-pointer -->
<div
  class={[
    'svelte-flow__node-basic bool-lit-node-border transition-opacity duration-300',
    'select-none',
    ...data.classes,
  ]}
>
  <Handle type="target" position={defaultSFHandlesInfo.targetPosition} />
  <div class="label-wrapper-for-content-bearing-sf-node font-extralight">
    {(data.context.get(data.originalLirId) as TrueExprLirNode).toPretty(
      data.context
    )}
  </div>
  <Handle type="source" position={defaultSFHandlesInfo.sourcePosition} />
</div>
