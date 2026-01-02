<script lang="ts">
  import type { ParameterState } from '../types.js'
  import ParameterCard from './ParameterCard.svelte'

  type Props = {
    parameters: ParameterState[]
    onchange: (key: string, value: unknown) => void
  }

  let { parameters, onchange }: Props = $props()

  // Separate relevant and irrelevant parameters
  let relevantParams = $derived(
    parameters
      .filter((p) => p.status !== 'irrelevant')
      .sort((a, b) => a.rank - b.rank)
  )

  let irrelevantParams = $derived(
    parameters.filter((p) => p.status === 'irrelevant')
  )

  let hasIrrelevant = $derived(irrelevantParams.length > 0)
</script>

<div class="space-y-6">
  <!-- Relevant parameters -->
  <div class="grid gap-4 sm:grid-cols-2 lg:grid-cols-3">
    {#each relevantParams as param (param.key)}
      <ParameterCard
        paramKey={param.key}
        label={param.label}
        schema={param.schema}
        value={param.value}
        status={param.status}
        error={param.error}
        {onchange}
      />
    {/each}
  </div>

  <!-- Irrelevant parameters (collapsed section) -->
  {#if hasIrrelevant}
    <details class="rounded-lg border border-gray-200 bg-gray-50">
      <summary
        class="cursor-pointer px-4 py-2 text-sm text-gray-500 hover:text-gray-700"
      >
        {irrelevantParams.length} parameter{irrelevantParams.length === 1
          ? ''
          : 's'} no longer relevant
      </summary>
      <div class="grid gap-4 p-4 sm:grid-cols-2 lg:grid-cols-3">
        {#each irrelevantParams as param (param.key)}
          <ParameterCard
            paramKey={param.key}
            label={param.label}
            schema={param.schema}
            value={param.value}
            status={param.status}
            error={param.error}
            {onchange}
          />
        {/each}
      </div>
    </details>
  {/if}
</div>
