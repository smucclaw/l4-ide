<script lang="ts">
  import { page } from '$app/stores'
  import { goto } from '$app/navigation'
  import { base } from '$app/paths'
  import Wizard from '$lib/components/Wizard.svelte'

  const defaultServiceUrl =
    import.meta.env.VITE_DECISION_SERVICE_URL ?? 'http://localhost:8001'

  let slug = $derived($page.params.slug ?? '')

  // Check if the slug looks like a UUID (session ID)
  // UUIDs are 32 hex chars, optionally with hyphens
  const uuidPattern =
    /^[0-9a-f]{8}-?[0-9a-f]{4}-?[0-9a-f]{4}-?[0-9a-f]{4}-?[0-9a-f]{12}$/i
  let isUuid = $derived(uuidPattern.test(slug))

  interface FunctionInfo {
    function: {
      name: string
      description: string
    }
    type: string
  }

  let functions: FunctionInfo[] = $state([])
  let isLoading = $state(true)
  let error: string | null = $state(null)
  let mode: 'wizard' | 'session-list' = $state('wizard')

  $effect(() => {
    if (isUuid) {
      // This is a session UUID - load functions for this session
      mode = 'session-list'
      loadSessionFunctions()
    } else {
      // This is a function name - show the wizard directly
      mode = 'wizard'
      isLoading = false
    }
  })

  async function loadSessionFunctions() {
    try {
      isLoading = true
      error = null

      // Query the decision service for the session
      const response = await fetch(
        `${defaultServiceUrl}/functions/${encodeURIComponent(slug)}`
      )

      if (!response.ok) {
        throw new Error(`Session not found: ${response.status}`)
      }

      const data = await response.json()

      // If this returns a single function, redirect to the function page
      if (data.function?.name) {
        goto(
          `${base}/${encodeURIComponent(slug)}/${encodeURIComponent(data.function.name)}`,
          { replaceState: true }
        )
        return
      }

      // Multiple functions - show list
      functions = data.functions ?? []
      isLoading = false
    } catch (e) {
      error = e instanceof Error ? e.message : 'Failed to load session'
      isLoading = false
    }
  }

  function selectFunction(name: string) {
    goto(`${base}/${encodeURIComponent(slug)}/${encodeURIComponent(name)}`)
  }
</script>

<svelte:head>
  <title>L4 Wizard - {slug}</title>
</svelte:head>

{#if mode === 'wizard'}
  <!-- Direct function access -->
  <main>
    <Wizard serviceUrl={defaultServiceUrl} functionName={slug} />
  </main>
{:else}
  <!-- Session function list -->
  <div class="mx-auto max-w-4xl px-4 py-8">
    <!-- Back link -->
    <a
      href={`${base}/`}
      class="mb-4 inline-flex items-center text-sm text-gray-500 hover:text-gray-700"
    >
      <svg
        class="mr-1 h-4 w-4"
        fill="none"
        stroke="currentColor"
        viewBox="0 0 24 24"
      >
        <path
          stroke-linecap="round"
          stroke-linejoin="round"
          stroke-width="2"
          d="M15 19l-7-7 7-7"
        />
      </svg>
      All functions
    </a>

    <header class="mb-8">
      <h1 class="text-2xl font-bold text-gray-900">Session Functions</h1>
      <p class="mt-2 font-mono text-sm text-gray-500">{slug}</p>
    </header>

    {#if isLoading}
      <div class="py-12 text-center text-gray-500">Loading session...</div>
    {:else if error}
      <div class="rounded-lg border border-red-300 bg-red-50 p-6 text-center">
        <p class="text-red-700">{error}</p>
        <p class="mt-2 text-sm text-red-600">
          This session may have expired or the decision service is unavailable.
        </p>
        <button
          type="button"
          class="mt-4 rounded-md bg-red-100 px-4 py-2 text-sm text-red-700 hover:bg-red-200"
          onclick={loadSessionFunctions}
        >
          Retry
        </button>
      </div>
    {:else if functions.length === 0}
      <div
        class="rounded-lg border-2 border-dashed border-gray-300 bg-gray-50 p-8 text-center"
      >
        <p class="text-gray-600">No functions found in this session</p>
      </div>
    {:else}
      <div class="grid gap-4 sm:grid-cols-2">
        {#each functions as fn (fn.function.name)}
          <button
            type="button"
            class="group rounded-lg border-2 border-gray-200 bg-white p-6 text-left transition-all hover:border-blue-400 hover:shadow-md"
            onclick={() => selectFunction(fn.function.name)}
          >
            <h2
              class="text-lg font-semibold text-gray-900 group-hover:text-blue-600"
            >
              {fn.function.name}
            </h2>
            {#if fn.function.description}
              <p class="mt-2 line-clamp-3 text-sm text-gray-600">
                {fn.function.description}
              </p>
            {/if}
          </button>
        {/each}
      </div>
    {/if}
  </div>
{/if}
