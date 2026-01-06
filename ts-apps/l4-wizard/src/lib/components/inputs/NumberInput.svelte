<script lang="ts">
  type Props = {
    value: unknown
    onChange: (value: number | undefined) => void
    disabled?: boolean
  }

  let { value, onChange, disabled = false }: Props = $props()

  function handleInput(event: Event) {
    const target = event.target as HTMLInputElement
    const strValue = target.value.trim()
    if (strValue === '') {
      onChange(undefined)
    } else {
      const num = parseFloat(strValue)
      if (!isNaN(num)) {
        onChange(num)
      }
    }
  }

  let displayValue = $derived(
    value !== undefined && value !== null ? String(value) : ''
  )
</script>

<input
  type="number"
  class="w-full rounded-md border border-gray-300 px-3 py-2 text-sm
    focus:border-blue-500 focus:outline-none focus:ring-1 focus:ring-blue-500
    disabled:cursor-not-allowed disabled:bg-gray-100 disabled:opacity-50"
  value={displayValue}
  oninput={handleInput}
  {disabled}
  step="any"
/>
