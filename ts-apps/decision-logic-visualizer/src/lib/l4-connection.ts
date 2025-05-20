import type { InlineExprsRequestParams } from '@repo/viz-expr'
import { makeVizInfoDecoder } from '@repo/viz-expr'
import type { EvalAppResult, LadderBackendApi } from 'jl4-client-rpc'
import {
  EvalAppRequestType,
  EvalAppRequestParams,
  InlineExprsRequestType,
} from 'jl4-client-rpc'

const decodeInlineExprsResult = makeVizInfoDecoder()

/** Higher-level wrapper around functionality provided by the Ladder backend.
 * The software design here was inspired by VSCode-Lean's 'EditorConnection' and 'EditorApi'.
 */
export class L4Connection {
  constructor(private readonly api: LadderBackendApi) {}

  // TODO: Think about how we can remove the need for consumers of L4Connection to supply a `verDocId`
  // it feels like something that L4Connection can just get by itself from LadderEnv; or perhaps it can be passed in when L4Connection is created

  /** Initiate the process of inlining exprs with the given Uniques */
  async inlineExprs(
    uniques: InlineExprsRequestParams['uniques'],
    verDocId: InlineExprsRequestParams['verDocId']
  ): Promise<void> {
    const params: InlineExprsRequestParams = {
      uniques,
      verDocId,
    }
    const renderLadderInfoEither = decodeInlineExprsResult(
      await this.api.sendClientRequest(InlineExprsRequestType, params)
    )
    // TODO: Improve this in the future
    if (renderLadderInfoEither._tag === 'Right') {
      await this.api.updateViz(renderLadderInfoEither.right)
    } else {
      throw new Error('Error: Failed to decode InlineExprsRequestParams')
    }
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
