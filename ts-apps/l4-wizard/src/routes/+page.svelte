<script lang="ts">
  import { page } from '$app/stores'
  import Wizard from '$lib/components/Wizard.svelte'
  import FunctionList from '$lib/components/FunctionList.svelte'

  // Get configuration from URL params or environment
  // VITE_DECISION_SERVICE_URL is set at build time for production deployments
  const defaultServiceUrl =
    import.meta.env.VITE_DECISION_SERVICE_URL ?? 'http://localhost:8001'
  let serviceUrl = $derived(
    $page.url.searchParams.get('service') ?? defaultServiceUrl
  )
  let functionName = $derived($page.url.searchParams.get('fn'))
</script>

<svelte:head>
  <title>L4 Wizard{functionName ? ` - ${functionName}` : ''}</title>
</svelte:head>

<main>
  {#if functionName}
    <Wizard {serviceUrl} {functionName} />
  {:else}
    <FunctionList {serviceUrl} />
  {/if}
</main>
