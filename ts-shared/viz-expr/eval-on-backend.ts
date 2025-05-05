import { Schema } from 'effect'
import { BoolValue, IRId } from './viz-expr.js'
import { VersionedDocId } from './versioned-doc-id.js'

/**********************************************
      Request payload: EvalAppRequestParams
***********************************************/

export const EvalAppRequestParams = Schema.Struct({
  $type: Schema.tag('EvalAppRequestParams'),
  // We can think about batching requests in the future
  appExpr: IRId,
  args: Schema.Array(BoolValue).annotations({
    description:
      'The actual arguments for the App. Would be nice to be able to handle different kinds of values in the future.',
  }),
  verTxtDocId: VersionedDocId.annotations({
    description:
      'To serve as an independent check that the backend and frontend are talking about the same version of the document / to avoid race conditions.',
  }),
}).annotations({
  identifier: 'EvalAppRequestParams',
  description:
    'EvalAppRequestParams is the payload for the request to evaluate an App expr with actual arguments on the backend.',
})

export type EvalAppRequestParams = Schema.Schema.Type<
  typeof EvalAppRequestParams
>

/**********************************************
        Response payload: EvalAppResult        
***********************************************/

export const EvalAppResult = Schema.Struct({
  $type: Schema.tag('EvalAppResult'),
  value: BoolValue,
})

export type EvalAppResult = Schema.Schema.Type<typeof EvalAppResult>
