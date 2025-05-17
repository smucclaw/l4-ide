import type {
  InlineExprsRequestParams,
  InlineExprsSuccess,
} from '@repo/viz-expr'
import type { EvalAppResult, LadderBackendApi } from 'jl4-client-rpc'
import {
  EvalAppRequestType,
  EvalAppRequestParams,
  InlineExprsRequestType,
} from 'jl4-client-rpc'

/** Higher-level wrapper around functionality provided by the Ladder backend.
 * The software design here was inspired by VSCode-Lean's 'EditorConnection' and 'EditorApi'.
 */
export class L4Connection {
  constructor(private readonly api: LadderBackendApi) {}

  /** Inline exprs with the given Uniques */
  async inlineExprs(
    uniques: InlineExprsRequestParams['uniques'],
    verDocId: InlineExprsRequestParams['verDocId']
  ): Promise<InlineExprsSuccess | null> {
    const params: InlineExprsRequestParams = {
      uniques,
      verDocId,
    }
    return this.api.sendClientRequest(InlineExprsRequestType, params)
  }

  /** Evaluate an App with actual arguments on the backend. */
  async evalApp(
    appExpr: EvalAppRequestParams['appExpr'],
    args: EvalAppRequestParams['args'],
    verDocId: EvalAppRequestParams['verDocId']
  ): Promise<EvalAppResult | null> {
    const params: EvalAppRequestParams = {
      appExpr,
      args,
      verDocId,
    }
    return this.api.sendClientRequest(EvalAppRequestType, params)
  }
}
