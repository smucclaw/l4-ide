<!--
This is the component that consumers of the DLV lib will import,
when trying to make ladder diagrams. -->
<script lang="ts">
  import { SvelteFlowProvider } from '@xyflow/svelte'
  import type { LadderFlowDisplayerProps } from './flow-props.js'
  import FlowBase from './flow-base.svelte'
  import LadderEnvProvider from '$lib/displayers/ladder-env-provider.svelte'

  const {
    context,
    node,
    lir,
    backendApi,
    versionedDocId,
  }: LadderFlowDisplayerProps = $props()

  let baseFlowComponent: ReturnType<typeof FlowBase>

  // TODO: Expose a wrapped version of SF's fitView, etc
</script>

<!-- SvelteFlowProvider supplies SF hooks used by FlowBase -->
<SvelteFlowProvider>
  <LadderEnvProvider {context} {lir} {node} {backendApi} {versionedDocId}>
    <div style="height: 100%">
      <FlowBase {context} {node} bind:this={baseFlowComponent} />
    </div>
  </LadderEnvProvider>
</SvelteFlowProvider>
