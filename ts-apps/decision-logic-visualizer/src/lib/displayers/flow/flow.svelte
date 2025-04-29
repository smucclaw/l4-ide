<!--
This is the component that consumers of the DLV lib will import,
when trying to make ladder diagrams.

This component only exists so that we can put SvelteFlowProvider
 above FlowBase (which uses SF hooks) in the component hierarchy -->
<script lang="ts">
  import { SvelteFlowProvider } from '@xyflow/svelte'
  import type { LadderFlowDisplayerProps } from './flow-props.js'
  import FlowBase from './flow-base.svelte'
  import { initializeLadderEnv } from '$lib/ladder-env.js'

  const {
    context,
    node: funDeclLirNode,
    lir,
  }: LadderFlowDisplayerProps = $props()

  initializeLadderEnv(context, lir, funDeclLirNode)

  let baseFlowComponent: ReturnType<typeof FlowBase>

  // TODO: Expose a wrapped version of SF's fitView, etc
</script>

<SvelteFlowProvider>
  <div style="height: 100%">
    <FlowBase {context} node={funDeclLirNode} bind:this={baseFlowComponent} />
  </div>
</SvelteFlowProvider>
