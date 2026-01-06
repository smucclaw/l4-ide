<script lang="ts">
  import type { Parameter } from '@repo/decision-service-types'
  import type { ParameterStatus } from '../types.js'
  import BooleanInput from './inputs/BooleanInput.svelte'
  import NumberInput from './inputs/NumberInput.svelte'
  import TextInput from './inputs/TextInput.svelte'
  import EnumInput from './inputs/EnumInput.svelte'

  type Props = {
    paramKey: string
    label: string
    schema: Parameter
    value: unknown
    status: ParameterStatus
    error?: string
    nestingLevel?: number
    onchange: (key: string, value: unknown) => void
  }

  let { paramKey, label, schema, value, status, error, nestingLevel = 0, onchange }: Props =
    $props()

  const statusClasses: Record<ParameterStatus, string> = {
    'unanswered-relevant': 'border-gray-300 bg-white scale-100',
    'unanswered-next':
      'border-blue-500 bg-blue-50 ring-2 ring-blue-200 shadow-md scale-100',
    answered: 'border-green-500 bg-green-50 scale-100',
    irrelevant: 'border-gray-200 bg-gray-100 opacity-50 scale-[0.98]',
    error: 'border-red-500 bg-red-50 scale-100',
  }

  function handleChange(newValue: unknown) {
    onchange(paramKey, newValue)
  }

  function getInputType(): 'boolean' | 'number' | 'text' | 'enum' {
    if (schema.enum && schema.enum.length > 0) return 'enum'
    if (schema.type === 'boolean') return 'boolean'
    if (schema.type === 'number') return 'number'
    return 'text'
  }

  let inputType = $derived(getInputType())
</script>

<div
  class="rounded-lg border-2 p-4 transition-all duration-300 ease-out {statusClasses[status]}"
>
  <label class="block">
    <span
      class="mb-2 block text-sm font-medium transition-colors duration-300 ease-out {status ===
      'irrelevant'
        ? 'text-gray-400'
        : 'text-gray-700'}"
    >
      {label}
      {#if status === 'unanswered-next'}
        <span class="ml-2 text-xs text-blue-600">(recommended)</span>
      {/if}
      {#if status === 'irrelevant'}
        <span class="ml-2 text-xs italic text-gray-400">(not needed)</span>
      {/if}
    </span>

    {#if schema.description}
      <p
        class="mb-2 text-xs transition-colors duration-300 ease-out {status ===
        'irrelevant'
          ? 'text-gray-400'
          : 'text-gray-500'}"
      >
        {schema.description}
      </p>
    {/if}

    {#if inputType === 'boolean'}
      <BooleanInput {value} onChange={handleChange} disabled={status === 'irrelevant'} />
    {:else if inputType === 'enum'}
      <EnumInput
        {value}
        options={schema.enum ?? []}
        onChange={handleChange}
        disabled={status === 'irrelevant'}
      />
    {:else if inputType === 'number'}
      <NumberInput {value} onChange={handleChange} disabled={status === 'irrelevant'} />
    {:else}
      <TextInput {value} onChange={handleChange} disabled={status === 'irrelevant'} />
    {/if}

    {#if error}
      <p class="mt-1 text-xs text-red-600">{error}</p>
    {/if}

    {#if status === 'answered'}
      <button
        type="button"
        class="mt-2 text-xs text-gray-500 hover:text-gray-700"
        onclick={() => handleChange(undefined)}
      >
        Clear
      </button>
    {/if}
  </label>
</div>
