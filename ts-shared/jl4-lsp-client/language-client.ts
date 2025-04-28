import type { Disposable } from 'vscode-languageclient'
import { EvalAppRequestParams, EvalAppResult } from '@repo/viz-expr'

export interface L4LanguageClient extends Disposable {
  start(): Promise<void>
  stop(): Promise<void>

  sendEvalAppRequest(params: EvalAppRequestParams): Promise<EvalAppResult>
}
