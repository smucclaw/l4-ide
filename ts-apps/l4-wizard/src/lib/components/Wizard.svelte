<script lang="ts">
  import type {
    Parameter,
    Parameters,
    QueryAsk,
    QueryAtom,
    Ladder,
  } from '@repo/decision-service-types'
  import type { ParameterState, ParameterStatus } from '../types.js'
  import {
    createClient,
    fetchFunctionMetadata,
    fetchQueryPlan,
    evaluateFunction,
    type DecisionServiceClient,
  } from '../decision-service.js'
  import { groupParametersByLadder, type ParameterGroup } from '../ladder.js'
  import ParameterGrid from './ParameterGrid.svelte'
  import OutcomeBanner from './OutcomeBanner.svelte'
  import LadderDiagram from './LadderDiagram.svelte'
  import GraphvizDiagram from './GraphvizDiagram.svelte'

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
  let ladder: Ladder = $state(null)

  // Track the current config to detect changes (non-reactive)
  let currentUrl = ''
  let currentFn = ''

  // Initialize when serviceUrl or functionName changes
  $effect(() => {
    const url = serviceUrl
    const fn = functionName

    // Only reinitialize if URL or function name actually changed
    if (url !== currentUrl || fn !== currentFn) {
      currentUrl = url
      currentFn = fn
      init(url, fn)
    }
  })

  async function init(url: string, fn: string) {
    try {
      isLoading = true
      error = null

      client = createClient(url)

      // Fetch function metadata
      const metadata = await fetchFunctionMetadata(client, fn)
      functionDescription = metadata.description

      // Initialize parameters from schema
      parameters = buildParametersFromSchema(metadata.parameters)

      // Get initial query plan with empty bindings
      await updateQueryPlan(fn, {})

      isLoading = false
    } catch (e) {
      error = e instanceof Error ? e.message : 'Failed to initialize'
      isLoading = false
    }
  }

  function buildParametersFromSchema(params: Parameters): ParameterState[] {
    const paramList: ParameterState[] = []

    if (!params || !params.properties) {
      return []
    }

    for (const [key, schema] of Object.entries(params.properties) as [
      string,
      Parameter,
    ][]) {
      paramList.push({
        key,
        label: schema.alias ?? key,
        schema,
        value: undefined,
        status: 'unanswered-relevant',
        rank: paramList.length,
        asks: [],
        group: undefined, // Will be set by ladder-based grouping
      })
    }

    return paramList
  }

  // Strip backticks from L4 identifiers
  function stripBackticks(s: string): string {
    return s.replace(/^`|`$/g, '')
  }

  async function updateQueryPlan(
    fn: string,
    currentBindings: Record<string, unknown>
  ) {
    if (!client) return

    try {
      const plan = await fetchQueryPlan(client, fn, currentBindings)

      // Store the ladder for later use
      ladder = plan.ladder

      // Build set of parameters that are still needed based on asks
      // The asks array tells us which parameters we need to query
      const stillNeededKeys = new Set(
        plan.asks.map((ask: QueryAsk) => stripBackticks(ask.container))
      )

      // Ranked parameters (for ordering)
      const rankedKeys = plan.asks.map((ask: QueryAsk) =>
        stripBackticks(ask.container)
      )

      // Build a map from ask container to asks (normalized by stripping backticks)
      const asksByContainer = new Map<string, QueryAsk[]>()
      for (const ask of plan.asks) {
        const normalizedKey = stripBackticks(ask.container)
        const existing = asksByContainer.get(normalizedKey) ?? []
        existing.push(ask)
        asksByContainer.set(normalizedKey, existing)
      }

      // Get the top-ranked "next" asks
      const nextAsks: QueryAtom[] =
        plan.asks.length > 0 ? (plan.asks[0]?.atoms ?? []) : []
      const nextLabels = new Set(
        nextAsks.map((a: QueryAtom) => stripBackticks(a.label))
      )

      // Extract parameter groups from the ladder structure
      const parameterGroups = groupParametersByLadder(ladder, plan.asks)

      // Build a map from parameter key to its group info
      const paramToGroupInfo = new Map<
        string,
        { groupId: string; depth: number; groupLabel: string }
      >()

      function collectParamGroupInfo(group: ParameterGroup, parentLabel = '') {
        const groupLabel = parentLabel
          ? `${parentLabel} > ${group.label}`
          : group.label
        for (const paramKey of group.parameters) {
          paramToGroupInfo.set(paramKey, {
            groupId: group.id,
            depth: group.depth,
            groupLabel: groupLabel,
          })
        }
        // Recursively process child groups
        for (const child of group.children) {
          collectParamGroupInfo(child, groupLabel)
        }
      }

      for (const group of parameterGroups) {
        collectParamGroupInfo(group)
      }

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

        // Get ladder-based grouping info
        const groupInfo = paramToGroupInfo.get(param.key)

        // Fallback: if parameter not in ladder groups, use prefix-based grouping
        let finalGroup = groupInfo?.groupLabel

        if (!finalGroup) {
          const prefixMatch = param.key.match(/^([a-z]+)_/)
          if (prefixMatch) {
            const prefix = prefixMatch[1]
            const prefixLabels: Record<string, string> = {
              p: 'Person',
              f: 'Father',
              m: 'Mother',
            }
            finalGroup = prefixLabels[prefix] || prefix
          }
        }

        return {
          ...param,
          status,
          rank: rankIndex >= 0 ? rankIndex : 999,
          asks: asksByContainer.get(param.key) ?? [],
          group: finalGroup,
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
    parameters = parameters.map((p) => (p.key === key ? { ...p, value } : p))

    // Refresh query plan and evaluation
    await updateQueryPlan(functionName, bindings)

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

  function handleLadderNodeClick(key: string, currentValue: unknown) {
    // Cycle through: undefined → true → false → undefined
    let nextValue: unknown
    if (currentValue === undefined) {
      nextValue = true
    } else if (currentValue === true) {
      nextValue = false
    } else {
      nextValue = undefined
    }
    handleParameterChange(key, nextValue)
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
    updateQueryPlan(functionName, {})
  }
</script>

<div class="mx-auto max-w-4xl px-4 py-8">
  <!-- Back link -->
  <a
    href="/wizard"
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

    <!-- Ladder Diagram -->
    <section class="mb-8">
      <LadderDiagram {ladder} {bindings} onNodeClick={handleLadderNodeClick} />
    </section>

    <!-- Parameter Grid -->
    <section>
      <h2 class="mb-4 text-lg font-medium text-gray-700">
        Answer the following questions:
      </h2>
      <ParameterGrid {parameters} onchange={handleParameterChange} />
    </section>

    <!-- Graphviz Diagram -->
    <section class="mt-8">
      <GraphvizDiagram {client} {functionName} {bindings} />
    </section>
  {/if}
</div>
