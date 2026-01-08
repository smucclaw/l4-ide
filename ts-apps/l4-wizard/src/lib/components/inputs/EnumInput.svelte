<script lang="ts">
  type Props = {
    value: unknown
    options: string[]
    onChange: (value: string | undefined) => void
    disabled?: boolean
  }

  let { value, options, onChange, disabled = false }: Props = $props()

  function handleClick(option: string) {
    if (disabled) return
    // Toggle off if clicking the same value
    if (value === option) {
      onChange(undefined)
    } else {
      onChange(option)
    }
  }
</script>

{#if options.length <= 4}
  <!-- Radio-style buttons for small enum -->
  <div class="flex flex-wrap gap-2">
    {#each options as option}
      <button
        type="button"
        class="rounded-md border-2 px-3 py-1.5 text-sm font-medium transition-colors
          {value === option
          ? 'border-blue-500 bg-blue-100 text-blue-800'
          : 'border-gray-300 bg-white text-gray-700 hover:bg-gray-50'}
          {disabled ? 'cursor-not-allowed opacity-50' : 'cursor-pointer'}"
        onclick={() => handleClick(option)}
        {disabled}
      >
        {option}
      </button>
    {/each}
  </div>
{:else}
  <!-- Dropdown for large enum -->
  <select
    class="w-full rounded-md border border-gray-300 px-3 py-2 text-sm
      focus:border-blue-500 focus:outline-none focus:ring-1 focus:ring-blue-500
      disabled:cursor-not-allowed disabled:bg-gray-100 disabled:opacity-50"
    value={value ?? ''}
    onchange={(e) => {
      const target = e.target as HTMLSelectElement
      onChange(target.value || undefined)
    }}
    {disabled}
  >
    <option value="">-- Select --</option>
    {#each options as option}
      <option value={option}>{option}</option>
    {/each}
  </select>
{/if}
