<script lang="ts">
  import type { Parameter, Parameters, QueryAsk, QueryAtom } from '@repo/decision-service-types'
  import type { ParameterState, ParameterStatus } from '../types.js'
  import {
    createClient,
    fetchFunctionMetadata,
    fetchQueryPlan,
    evaluateFunction,
    type DecisionServiceClient,
  } from '../decision-service.js'
  import ParameterGrid from './ParameterGrid.svelte'
  import OutcomeBanner from './OutcomeBanner.svelte'

  type Props = {
    serviceUrl: string
    functionName: string
  }

  let { serviceUrl, functionName }: Props = $props()

  let bindings: Record<string, unknown> = $state({})
  let parameters: ParameterState[] = $state([])
  let result: unknown = $state(null)
  let isDetermined = $state(false)
  let isLoading = $state(true)
  let error: string | null = $state(null)

  let client: DecisionServiceClient | null = $state(null)
  let functionDescription = $state('')

  // Initialize on mount
  $effect(() => {
    init()
  })

  async function init() {
    try {
      client = createClient(serviceUrl)

      // Fetch function metadata
      const metadata = await fetchFunctionMetadata(client, functionName)
      functionDescription = metadata.description

      // Initialize parameters from schema
      parameters = buildParametersFromSchema(metadata.parameters)

      // Get initial query plan
      await updateQueryPlan()

      isLoading = false
    } catch (e) {
      error = e instanceof Error ? e.message : 'Failed to initialize'
      isLoading = false
    }
  }

  function buildParametersFromSchema(params: Parameters): ParameterState[] {
    const paramList: ParameterState[] = []

    for (const [key, schema] of Object.entries(params.properties) as [string, Parameter][]) {
      paramList.push({
        key,
        label: schema.alias ?? key,
        schema,
        value: undefined,
        status: 'unanswered-relevant',
        rank: paramList.length,
        asks: [],
      })
    }

    return paramList
  }

  async function updateQueryPlan() {
    if (!client) return

    try {
      const plan = await fetchQueryPlan(client, functionName, bindings)

      // Update parameter statuses based on query plan
      const stillNeededKeys = new Set(plan.stillNeeded.map((a: QueryAtom) => a.label))
      const rankedKeys = plan.ranked.map((a: QueryAtom) => a.label)

      // Build a map from ask container to asks
      const asksByContainer = new Map<string, QueryAsk[]>()
      for (const ask of plan.asks) {
        const existing = asksByContainer.get(ask.container) ?? []
        existing.push(ask)
        asksByContainer.set(ask.container, existing)
      }

      // Get the top-ranked "next" asks
      const nextAsks: QueryAtom[] = plan.asks.length > 0 ? plan.asks[0]?.atoms ?? [] : []
      const nextLabels = new Set(nextAsks.map((a: QueryAtom) => a.label))

      parameters = parameters.map((param) => {
        const hasValue = bindings[param.key] !== undefined
        const isStillNeeded = stillNeededKeys.has(param.key)
        const isNext = nextLabels.has(param.key)
        const rankIndex = rankedKeys.indexOf(param.key)

        let status: ParameterStatus
        if (hasValue) {
          status = 'answered'
        } else if (!isStillNeeded) {
          status = 'irrelevant'
        } else if (isNext) {
          status = 'unanswered-next'
        } else {
          status = 'unanswered-relevant'
        }

        return {
          ...param,
          status,
          rank: rankIndex >= 0 ? rankIndex : 999,
          asks: asksByContainer.get(param.key) ?? [],
        }
      })

      // Update determination status
      isDetermined = plan.determined !== null
      if (plan.determined !== null) {
        result = plan.determined
      }
    } catch (e) {
      console.error('Failed to update query plan:', e)
    }
  }

  async function handleParameterChange(key: string, value: unknown) {
    // Update bindings
    if (value === undefined) {
      const newBindings = { ...bindings }
      delete newBindings[key]
      bindings = newBindings
    } else {
      bindings = { ...bindings, [key]: value }
    }

    // Update parameter value
    parameters = parameters.map((p) =>
      p.key === key ? { ...p, value } : p
    )

    // Refresh query plan and evaluation
    await updateQueryPlan()

    // If we have bindings, try to evaluate
    if (Object.keys(bindings).length > 0 && client) {
      try {
        const evalResult = await evaluateFunction(
          client,
          functionName,
          bindings
        )
        result = evalResult.result
      } catch {
        // Evaluation may fail if not all required inputs are provided
      }
    }
  }

  function handleReset() {
    bindings = {}
    result = null
    isDetermined = false
    parameters = parameters.map((p) => ({
      ...p,
      value: undefined,
      status: 'unanswered-relevant',
    }))
    updateQueryPlan()
  }
</script>

<div class="mx-auto max-w-4xl px-4 py-8">
  <!-- Header -->
  <header class="mb-8">
    <div class="flex items-center justify-between">
      <h1 class="text-2xl font-bold text-gray-900">{functionName}</h1>
      <button
        type="button"
        class="rounded-md bg-gray-100 px-3 py-1.5 text-sm text-gray-600 hover:bg-gray-200"
        onclick={handleReset}
      >
        Reset
      </button>
    </div>
    {#if functionDescription}
      <p class="mt-2 text-gray-600">{functionDescription}</p>
    {/if}
  </header>

  {#if isLoading}
    <div class="py-12 text-center text-gray-500">Loading...</div>
  {:else if error}
    <div class="rounded-lg border border-red-300 bg-red-50 p-4 text-red-700">
      {error}
    </div>
  {:else}
    <!-- Outcome Banner -->
    <section class="mb-8">
      <OutcomeBanner {result} {isDetermined} />
    </section>

    <!-- Parameter Grid -->
    <section>
      <h2 class="mb-4 text-lg font-medium text-gray-700">
        Answer the following questions:
      </h2>
      <ParameterGrid
        {parameters}
        onchange={handleParameterChange}
      />
    </section>
  {/if}
</div>
