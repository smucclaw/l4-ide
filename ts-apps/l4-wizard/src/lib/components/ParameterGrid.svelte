<script lang="ts">
  import type { ParameterState } from '../types.js'
  import ParameterCard from './ParameterCard.svelte'

  type Props = {
    parameters: ParameterState[]
    onchange: (key: string, value: unknown) => void
  }

  let { parameters, onchange }: Props = $props()

  // Group parameters by their group attribute
  let grouped = $derived.by(() => {
    const sorted = [...parameters].sort((a, b) => a.rank - b.rank)
    const groups = new Map<string | undefined, ParameterState[]>()

    for (const param of sorted) {
      const groupKey = param.group
      if (!groups.has(groupKey)) {
        groups.set(groupKey, [])
      }
      groups.get(groupKey)!.push(param)
    }

    return groups
  })

  // Get human-readable group labels
  function getGroupLabel(group: string | undefined): string {
    if (!group) return ''
    const labels: Record<string, string> = {
      p: 'Person',
      f: 'Father',
      m: 'Mother',
    }
    return labels[group] || group
  }
</script>

<div class="space-y-6">
  {#each Array.from(grouped.entries()) as [group, params]}
    {#if group}
      <!-- Grouped parameters with visual container -->
      <div class="rounded-lg border-2 border-gray-200 bg-gray-50/30 p-4">
        <h3 class="mb-3 text-sm font-semibold text-gray-700">
          {getGroupLabel(group)}
        </h3>
        <div class="grid gap-4 sm:grid-cols-2 lg:grid-cols-3">
          {#each params as param (param.key)}
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
      </div>
    {:else}
      <!-- Ungrouped parameters at root level -->
      <div class="grid gap-4 sm:grid-cols-2 lg:grid-cols-3">
        {#each params as param (param.key)}
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
    {/if}
  {/each}
</div>
