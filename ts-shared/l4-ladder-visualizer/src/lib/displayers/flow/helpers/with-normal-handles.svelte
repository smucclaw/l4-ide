<!-- 
 SvelteFlow handles for non-bundling / content-bearing nodes 
-->
<script lang="ts" module>
  import type { Snippet } from 'svelte'

  interface NormalHandlesProps {
    children: Snippet
  }
</script>

<script lang="ts">
  import { Handle } from '@xyflow/svelte'
  import { defaultSFHandlesInfo } from '../svelteflow-types.js'

  let { children }: NormalHandlesProps = $props()
</script>

<Handle type="target" position={defaultSFHandlesInfo.targetPosition} />
{@render children()}
<Handle type="source" position={defaultSFHandlesInfo.sourcePosition} />
