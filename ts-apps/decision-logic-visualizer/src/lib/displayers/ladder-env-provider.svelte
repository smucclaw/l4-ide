<script lang="ts" module>
  import type { Snippet } from 'svelte'

  export interface LadderEnvProviderProps {
    env: LadderEnv
    children: Snippet
  }
</script>

<script lang="ts">
  import { LadderEnv } from '$lib/ladder-env.js'

  const { env, children }: LadderEnvProviderProps = $props()

  // Make the LadderEnv available to children components
  env.setInSvelteContext()
</script>

{@render children()}
