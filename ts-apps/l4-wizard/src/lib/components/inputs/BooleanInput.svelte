<script lang="ts">
  type Props = {
    value: unknown
    onChange: (value: boolean | undefined) => void
    disabled?: boolean
  }

  let { value, onChange, disabled = false }: Props = $props()

  function handleClick(newValue: boolean) {
    if (disabled) return
    // Toggle off if clicking the same value
    if (value === newValue) {
      onChange(undefined)
    } else {
      onChange(newValue)
    }
  }

  let isTrue = $derived(value === true)
  let isFalse = $derived(value === false)
</script>

<div class="flex gap-2">
  <button
    type="button"
    class="flex-1 rounded-md border-2 px-4 py-2 text-sm font-medium transition-colors
      {isTrue
      ? 'border-green-500 bg-green-100 text-green-800'
      : 'border-gray-300 bg-white text-gray-700 hover:bg-gray-50'}
      {disabled ? 'cursor-not-allowed opacity-50' : 'cursor-pointer'}"
    onclick={() => handleClick(true)}
    {disabled}
  >
    Yes
  </button>
  <button
    type="button"
    class="flex-1 rounded-md border-2 px-4 py-2 text-sm font-medium transition-colors
      {isFalse
      ? 'border-red-500 bg-red-100 text-red-800'
      : 'border-gray-300 bg-white text-gray-700 hover:bg-gray-50'}
      {disabled ? 'cursor-not-allowed opacity-50' : 'cursor-pointer'}"
    onclick={() => handleClick(false)}
    {disabled}
  >
    No
  </button>
</div>
