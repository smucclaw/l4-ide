<script lang="ts">
  import { page } from '$app/stores'
  import Wizard from '$lib/components/Wizard.svelte'
  import FunctionList from '$lib/components/FunctionList.svelte'

  // Get configuration from URL params or environment
  let serviceUrl = $derived(
    $page.url.searchParams.get('service') ?? 'http://localhost:8001'
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
