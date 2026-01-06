<script lang="ts">
  import { page } from '$app/stores'
  import Wizard from '$lib/components/Wizard.svelte'

  // Get configuration from URL params or environment
  let serviceUrl = $derived(
    $page.url.searchParams.get('service') ?? 'http://localhost:8001'
  )
  let functionName = $derived($page.url.searchParams.get('fn') ?? 'example')
</script>

<svelte:head>
  <title>L4 Wizard - {functionName}</title>
</svelte:head>

<main>
  <Wizard {serviceUrl} {functionName} />
</main>
