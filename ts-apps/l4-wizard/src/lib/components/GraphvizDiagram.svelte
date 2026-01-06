<script lang="ts">
  import type { DecisionServiceClient } from '../decision-service.js'
  import { fetchGraphviz } from '../decision-service.js'

  type Props = {
    client: DecisionServiceClient | null
    functionName: string
    bindings: Record<string, unknown>
  }

  let { client, functionName, bindings }: Props = $props()

  let svgContent = $state<string | null>(null)
  let isLoading = $state(false)
  let error = $state<string | null>(null)

  // Fetch graphviz when bindings change
  $effect(() => {
    if (!client) return

    const fn = functionName
    const currentBindings = bindings

    // Only fetch if we have at least one binding
    if (Object.keys(currentBindings).length === 0) {
      svgContent = null
      isLoading = false
      error = null
      return
    }

    async function loadGraphviz() {
      try {
        isLoading = true
        error = null
        const svg = await fetchGraphviz(client!, fn, currentBindings, 'svg')
        svgContent = svg
        isLoading = false
      } catch (e) {
        error = e instanceof Error ? e.message : 'Failed to load graphviz'
        isLoading = false
      }
    }

    loadGraphviz()
  })
</script>

<div class="rounded-lg border-2 border-gray-200 bg-white p-4">
  <h3 class="mb-3 text-sm font-semibold text-gray-700">Graphviz Visualization</h3>

  {#if isLoading}
    <div class="py-8 text-center text-sm text-gray-500">Loading visualization...</div>
  {:else if error}
    <div class="rounded border border-gray-300 bg-gray-50 p-4 text-sm text-gray-600">
      {error}
    </div>
  {:else if svgContent}
    <div class="overflow-auto">
      {@html svgContent}
    </div>
  {:else}
    <div class="rounded-lg border-2 border-dashed border-gray-300 bg-gray-50 p-8 text-center text-sm text-gray-500">
      No graphviz visualization available
    </div>
  {/if}
</div>
