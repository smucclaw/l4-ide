import type {
  LspResponse,
  EvalAppRequestParams,
  EvalAppResult,
} from './custom-protocol.js'
import { IAmDisposable } from './utils.js'

export interface L4LanguageClient extends IAmDisposable {
  start(): Promise<void>

  sendEvalAppRequest(
    params: EvalAppRequestParams
  ): Promise<LspResponse<EvalAppResult>>
}
