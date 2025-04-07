<!--
This is the component that consumers of the DLV lib will import,
when trying to make ladder diagrams.

This component only exists so that we can put SvelteFlowProvider
 above FlowBase (which uses SF hooks) in the component hierarchy -->
<script lang="ts">
  import { SvelteFlowProvider } from '@xyflow/svelte'
  import type { LadderFlowDisplayerProps } from '../props.js'
  import { setLirRegistryInSvelteContext } from '$lib/layout-ir/core.js'
  import FlowBase from './flow-base.svelte'

  const { context, node: declLirNode, lir }: LadderFlowDisplayerProps = $props()
  setLirRegistryInSvelteContext(lir)

  let baseFlowComponent: ReturnType<typeof FlowBase>

  // TODO: Expose a wrapped version of SF's fitView, etc
</script>

<SvelteFlowProvider>
  <div style="height: 100%">
    <FlowBase {context} node={declLirNode} bind:this={baseFlowComponent} />
  </div>
</SvelteFlowProvider>
