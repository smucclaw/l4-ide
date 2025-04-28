import { EvalAppRequestParams, EvalAppResult } from '@repo/viz-expr'
import { IAmDisposable } from './utils.js'

export interface L4LanguageClient extends IAmDisposable {
  start(): Promise<void>

  sendEvalAppRequest(params: EvalAppRequestParams): Promise<EvalAppResult>
}
