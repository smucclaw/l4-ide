<!-- Just trying to see if I can get the stokc example working -->
<script lang="ts">
  import { SvelteFlowProvider } from '@xyflow/svelte'
  import Flow from '$lib/displayers/flow-dagre/flow.svelte'
</script>

<SvelteFlowProvider>
  <Flow />
</SvelteFlowProvider>
