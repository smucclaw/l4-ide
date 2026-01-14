<script lang="ts">
  import type { DecisionServiceClient } from '../decision-service.js'
  import {
    fetchStateGraphList,
    fetchStateGraphSvg,
    type StateGraphInfo,
  } from '../decision-service.js'

  type Props = {
    client: DecisionServiceClient | null
    functionName: string
  }

  let { client, functionName }: Props = $props()

  let graphs = $state<StateGraphInfo[]>([])
  let selectedGraph = $state<string | null>(null)
  let svgContent = $state<string | null>(null)
  let isLoading = $state(false)
  let isLoadingGraphs = $state(false)
  let error = $state<string | null>(null)

  // Fetch list of graphs when client or function changes
  $effect(() => {
    if (!client) {
      graphs = []
      selectedGraph = null
      svgContent = null
      return
    }

    const fn = functionName

    // Reset state when function changes to avoid stale graph selection
    selectedGraph = null
    svgContent = null
    graphs = []

    async function loadGraphList() {
      try {
        isLoadingGraphs = true
        error = null
        const response = await fetchStateGraphList(client!, fn)
        graphs = response.graphs

        // Auto-select first graph if available
        if (graphs.length > 0) {
          selectedGraph = graphs[0].graphName
        }

        isLoadingGraphs = false
      } catch (e) {
        error = e instanceof Error ? e.message : 'Failed to load state graphs'
        isLoadingGraphs = false
        graphs = []
      }
    }

    loadGraphList()
  })

  // Fetch SVG when selected graph changes
  $effect(() => {
    if (!client || !selectedGraph) {
      svgContent = null
      return
    }

    const fn = functionName
    const graphName = selectedGraph

    async function loadGraphSvg() {
      try {
        isLoading = true
        error = null
        const svg = await fetchStateGraphSvg(client!, fn, graphName)
        svgContent = svg
        isLoading = false
      } catch (e) {
        error =
          e instanceof Error ? e.message : 'Failed to load state graph SVG'
        isLoading = false
      }
    }

    loadGraphSvg()
  })
</script>

<div class="rounded-lg border-2 border-gray-200 bg-white p-4">
  <div class="mb-3 flex items-center justify-between">
    <h3 class="text-sm font-semibold text-gray-700">State Transition Graphs</h3>

    {#if graphs.length > 1}
      <select
        class="rounded border border-gray-300 px-2 py-1 text-sm"
        bind:value={selectedGraph}
      >
        {#each graphs as graph}
          <option value={graph.graphName}>{graph.graphName}</option>
        {/each}
      </select>
    {/if}
  </div>

  {#if isLoadingGraphs}
    <div class="py-8 text-center text-sm text-gray-500">
      Loading state graphs...
    </div>
  {:else if error}
    <div
      class="rounded border border-gray-300 bg-gray-50 p-4 text-sm text-gray-600"
    >
      {error}
    </div>
  {:else if graphs.length === 0}
    <div
      class="rounded-lg border-2 border-dashed border-gray-300 bg-gray-50 p-8 text-center text-sm text-gray-500"
    >
      No regulative rules found in this module.
      <div class="mt-2 text-xs">
        State graphs are generated from MUST/MAY/SHANT rules.
      </div>
    </div>
  {:else if isLoading}
    <div class="py-8 text-center text-sm text-gray-500">
      Loading visualization...
    </div>
  {:else if svgContent}
    <div class="overflow-auto">
      <div class="text-xs text-gray-500 mb-2">
        {#if selectedGraph}
          <span class="font-medium">{selectedGraph}</span> - Contract automaton showing
          states and transitions
        {/if}
      </div>
      {@html svgContent}
    </div>
  {:else}
    <div
      class="rounded-lg border-2 border-dashed border-gray-300 bg-gray-50 p-8 text-center text-sm text-gray-500"
    >
      Select a state graph to view
    </div>
  {/if}
</div>
