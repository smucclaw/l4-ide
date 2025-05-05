import type { LadderBackendApi } from 'jl4-client-rpc'
import { EvalAppRequestType } from 'jl4-client-rpc'
import { EvalAppRequestParams } from '@repo/viz-expr'

/** Higher-level wrapper around functionality provided by the Ladder backend.
 * The software design here was inspired by VSCode-Lean's 'EditorConnection' and 'EditorApi'.
 */
export class L4Connection {
  constructor(private readonly api: LadderBackendApi) {}

  /** Evaluate an App with actual arguments on the backend. */
  async evalApp(
    appExpr: EvalAppRequestParams['appExpr'],
    args: EvalAppRequestParams['args'],
    verTxtDocId: EvalAppRequestParams['verTxtDocId']
  ) {
    const params: EvalAppRequestParams = {
      $type: 'EvalAppRequestParams',
      appExpr,
      args,
      verTxtDocId,
    }
    return this.api.sendClientRequest(EvalAppRequestType, params)
  }
}
