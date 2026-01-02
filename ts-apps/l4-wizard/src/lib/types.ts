import type { Parameter, QueryAsk } from '@repo/decision-service-types'

export type ParameterStatus =
  | 'unanswered-relevant'
  | 'unanswered-next'
  | 'answered'
  | 'irrelevant'
  | 'error'

export type ParameterState = {
  key: string
  label: string
  schema: Parameter
  value: unknown
  status: ParameterStatus
  error?: string
  /** Ranking from query-plan (lower = more important) */
  rank: number
  /** Associated asks from query-plan */
  asks: QueryAsk[]
}

export type WizardState = {
  /** Current user-provided values */
  bindings: Record<string, unknown>
  /** All parameters with their current state */
  parameters: ParameterState[]
  /** Current result (null if undetermined) */
  result: unknown | null
  /** Whether result is determined */
  isDetermined: boolean
  /** Loading state */
  isLoading: boolean
  /** Error message if any */
  error: string | null
}

export function createInitialState(): WizardState {
  return {
    bindings: {},
    parameters: [],
    result: null,
    isDetermined: false,
    isLoading: false,
    error: null,
  }
}
