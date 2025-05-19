<!--
This is the component that consumers of the DLV lib will import,
when trying to make ladder diagrams. -->
<script lang="ts">
  import { SvelteFlowProvider } from '@xyflow/svelte'
  import type { LadderFlowDisplayerProps } from './flow-props.js'
  import FlowBase from './flow-base.svelte'
  import LadderEnvProvider from '$lib/displayers/ladder-env-provider.svelte'
  import { LADDER_VIZ_ROOT_TYPE } from '$lib/ladder-env.js'

  const { context, node, env }: LadderFlowDisplayerProps = $props()

  let baseFlowComponent: ReturnType<typeof FlowBase>

  // Check preconditions
  // TODO: Think more about whether we should really require consumers to set the lir root in the future;
  // but for now, it's enough to just check that it's been done.
  if (env.getLirRegistry().getRoot(context, LADDER_VIZ_ROOT_TYPE) !== node) {
    throw new Error(
      'Consumer of the visualizer lib must set LADDER_VIZ_ROOT_TYPE = funDeclLirNode in the LirRegistry'
    )
  }

  // TODO: Expose a wrapped version of SF's fitView, etc
</script>

<!-- SvelteFlowProvider supplies SF hooks used by FlowBase -->
<SvelteFlowProvider>
  <LadderEnvProvider {env}>
    <div style="height: 100%">
      <FlowBase {context} {node} bind:this={baseFlowComponent} />
    </div>
  </LadderEnvProvider>
</SvelteFlowProvider>
