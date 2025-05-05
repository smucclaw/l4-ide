<script lang="ts" module>
  import type { Snippet } from 'svelte'
  import type { LadderFlowDisplayerProps } from './flow/flow-props.js'

  interface LadderEnvProviderProps extends LadderFlowDisplayerProps {
    children: Snippet
  }
</script>

<script lang="ts">
  import { LadderEnv } from '$lib/ladder-env.js'

  const {
    context,
    lir,
    node: funDeclLirNode,
    children,
  }: LadderEnvProviderProps = $props()

  // Initialize the LadderEnv and make it available to children components
  const ladderEnv = LadderEnv.make(context, lir, funDeclLirNode)
  ladderEnv.setInSvelteContext()
</script>

{@render children()}
