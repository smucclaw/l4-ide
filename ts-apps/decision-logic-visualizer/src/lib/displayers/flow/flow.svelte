<!-- 
This is the component that consumers of the DLV lib will import,
when trying to make ladder diagrams.

This component only exists so that we can put SvelteFlowProvider
 above FlowBase (which uses SF hooks) in the component hierarchy -->
<script lang="ts">
  import { SvelteFlowProvider } from '@xyflow/svelte'
  import { type ExprFlowDisplayerProps } from './types.svelte.js'
  import FlowBase from './flow-base.svelte'

  const { context, node: exprLirNode }: ExprFlowDisplayerProps = $props()

  let baseFlowComponent: ReturnType<typeof FlowBase>

  // TODO: Expose a wrapped version of SF's fitView, etc
</script>

<SvelteFlowProvider>
  <FlowBase {context} node={exprLirNode} bind:this={baseFlowComponent} />
</SvelteFlowProvider>
