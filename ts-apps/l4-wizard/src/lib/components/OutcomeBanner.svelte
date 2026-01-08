<script lang="ts">
  type Props = {
    result: unknown
    isDetermined: boolean
  }

  let { result, isDetermined }: Props = $props()

  function formatResult(r: unknown): string {
    if (r === true) return 'Yes'
    if (r === false) return 'No'
    if (r === null || r === undefined) return 'Unknown'
    if (typeof r === 'number') return r.toLocaleString()
    if (typeof r === 'string') return r
    return JSON.stringify(r)
  }

  let isPositive = $derived(result === true)
  let isNegative = $derived(result === false)
</script>

{#if isDetermined}
  <div
    class="rounded-lg p-6 text-center shadow-lg transition-all duration-300
      {isPositive
      ? 'border-2 border-green-500 bg-green-50'
      : isNegative
        ? 'border-2 border-red-500 bg-red-50'
        : 'border-2 border-blue-500 bg-blue-50'}"
  >
    <div class="text-sm font-medium uppercase tracking-wide text-gray-500">
      Result
    </div>
    <div
      class="mt-2 text-3xl font-bold
        {isPositive
        ? 'text-green-700'
        : isNegative
          ? 'text-red-700'
          : 'text-blue-700'}"
    >
      {formatResult(result)}
    </div>
  </div>
{:else}
  <div
    class="rounded-lg border-2 border-dashed border-gray-300 bg-gray-50 p-6 text-center"
  >
    <div class="text-sm text-gray-500">
      Answer questions to determine the result
    </div>
  </div>
{/if}
