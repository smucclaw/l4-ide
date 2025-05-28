<!--
This is the component that consumers of the DLV lib will import,
when trying to make ladder diagrams. -->
<script lang="ts">
  import { SvelteFlowProvider } from '@xyflow/svelte'
  import type { LadderFlowDisplayerProps } from './flow-props.js'
  import FlowBase from './flow-base.svelte'
  import LadderEnvProvider from '$lib/displayers/ladder-env-provider.svelte'
  import * as Tooltip from '$lib/ui-primitives/tooltip/index.js'

  const { context, node, env }: LadderFlowDisplayerProps = $props()

  let baseFlowComponent: ReturnType<typeof FlowBase>

  // Check preconditions
  // TODO: Think more about whether we should really require consumers to set the lir root in the future;
  // but for now, it's enough to just check that it's been done.
  if (env.getTopFunDeclLirNode(context) !== node) {
    throw new Error(
      'Consumer of the visualizer lib must set funDeclNode in the LirRegistry and pass the key in to LadderEnv'
    )
  }

  // TODO: Expose a wrapped version of SF's fitView, etc
</script>

<!-- SvelteFlowProvider supplies SF hooks used by FlowBase -->
<SvelteFlowProvider>
  <LadderEnvProvider {env}>
    <!--
      Note: we are effectively standardizing on using the tooltip component from Shadcn-Svelte/Bits-UI.

      On Tooltip.Provider, from [Shadcn-Svelte/Bits-UI docs](https://bits-ui.com/docs/components/tooltip):

      > The Tooltip.Provider component is required to be an ancestor of the Tooltip.Root component. It provides shared state for the tooltip components used within it.  

      So, e.g., the delayDuration specified below will apply to all Tooltip.Roots in FlowBase (unless explicitly overridden).

      IMPORTANT: Cannot put Tooltip.Provider in +layout.svelte,
      likely because of complications with SvelteFlowProvider.
      -->
    <Tooltip.Provider delayDuration={100}>
      <div style="height: 100%">
        <FlowBase {context} {node} bind:this={baseFlowComponent} />
      </div>
    </Tooltip.Provider>
  </LadderEnvProvider>
</SvelteFlowProvider>
