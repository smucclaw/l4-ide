<script lang="ts">
  import { goto } from '$app/navigation'

  type Props = {
    serviceUrl: string
  }

  let { serviceUrl }: Props = $props()

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

  $effect(() => {
    loadFunctions()
  })

  async function loadFunctions() {
    try {
      isLoading = true
      error = null
      const response = await fetch(`${serviceUrl}/functions`)
      if (!response.ok) {
        throw new Error(`Failed to fetch functions: ${response.status}`)
      }
      functions = await response.json()
      isLoading = false
    } catch (e) {
      error = e instanceof Error ? e.message : 'Failed to load functions'
      isLoading = false
    }
  }

  function selectFunction(name: string) {
    goto(`?fn=${encodeURIComponent(name)}`)
  }
</script>

<div class="mx-auto max-w-4xl px-4 py-8">
  <header class="mb-8 text-center">
    <h1 class="text-3xl font-bold text-gray-900">L4 Wizard</h1>
    <p class="mt-2 text-gray-600">Select a decision function to explore</p>
  </header>

  {#if isLoading}
    <div class="py-12 text-center text-gray-500">
      Loading available functions...
    </div>
  {:else if error}
    <div class="rounded-lg border border-red-300 bg-red-50 p-6 text-center">
      <p class="text-red-700">{error}</p>
      <p class="mt-2 text-sm text-red-600">
        Make sure the decision service is running at {serviceUrl}
      </p>
      <button
        type="button"
        class="mt-4 rounded-md bg-red-100 px-4 py-2 text-sm text-red-700 hover:bg-red-200"
        onclick={loadFunctions}
      >
        Retry
      </button>
    </div>
  {:else if functions.length === 0}
    <div
      class="rounded-lg border-2 border-dashed border-gray-300 bg-gray-50 p-8 text-center"
    >
      <p class="text-gray-600">No functions available</p>
      <p class="mt-2 text-sm text-gray-500">
        Load some L4 files into the decision service to get started
      </p>
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
          <div
            class="mt-4 flex items-center text-sm text-blue-600 opacity-0 transition-opacity group-hover:opacity-100"
          >
            <span>Open wizard</span>
            <svg
              class="ml-1 h-4 w-4"
              fill="none"
              stroke="currentColor"
              viewBox="0 0 24 24"
            >
              <path
                stroke-linecap="round"
                stroke-linejoin="round"
                stroke-width="2"
                d="M9 5l7 7-7 7"
              />
            </svg>
          </div>
        </button>
      {/each}
    </div>
  {/if}

  <footer
    class="mt-12 border-t border-gray-200 pt-6 text-center text-sm text-gray-500"
  >
    <p>Connected to: {serviceUrl}</p>
  </footer>
</div>
