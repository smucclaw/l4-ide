import type { QueryPlanResponse } from '@repo/decision-service-types'
import type { RequestType } from 'vscode-messenger-common'

export type DecisionServiceQueryPlanParams = {
  docUri: string
  fnName: string
  bindings: Record<string, boolean>
}

export const DecisionServiceQueryPlanRequest: RequestType<
  DecisionServiceQueryPlanParams,
  QueryPlanResponse
> = {
  method: 'jl4/decisionService/queryPlan',
}
