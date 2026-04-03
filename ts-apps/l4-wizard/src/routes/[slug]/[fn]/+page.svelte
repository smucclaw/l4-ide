<script lang="ts">
  import { page } from '$app/stores'
  import Wizard from '$lib/components/Wizard.svelte'

  const defaultServiceUrl =
    import.meta.env.VITE_DECISION_SERVICE_URL ?? 'http://localhost:8001'

  let sessionId = $derived($page.params.slug ?? '')
  let fn = $derived($page.params.fn ?? '')

  // For UUID-based sessions, the function name is sessionId:functionName
  let functionName = $derived(`${sessionId}:${fn}`)
</script>

<svelte:head>
  <title>L4 Wizard - {fn}</title>
</svelte:head>

<main>
  <Wizard serviceUrl={defaultServiceUrl} {functionName} />
</main>
