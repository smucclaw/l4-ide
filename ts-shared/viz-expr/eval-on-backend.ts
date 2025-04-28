import { Schema } from 'effect'
import { BoolValue, IRId } from './viz-expr.js'
import { VersionId } from './version-id.js'

/**********************************************
        Request payload: EvalAppParams
***********************************************/

export const EvalAppParams = Schema.Struct({
  $type: Schema.tag('EvalAppParams'),
  // We can think about batching requests in the future
  appExpr: IRId,
  arguments: Schema.Array(BoolValue).annotations({
    description:
      'The actual arguments for the App. Would be nice to be able to handle different kinds of values in the future.',
  }),
  versionId: VersionId.annotations({
    description:
      'To serve as an independent check that the backend and frontend are talking about the same version of the document / to avoid race conditions.',
  }),
}).annotations({
  identifier: 'EvalAppParams',
  description:
    'EvalAppParams is the payload for the request to evaluate an App expr with actual arguments on the backend.',
})

export type EvalAppParams = Schema.Schema.Type<typeof EvalAppParams>

/**********************************************
        Response payload: EvalAppResult        
***********************************************/

export const EvalAppResult = Schema.Struct({
  $type: Schema.tag('EvalAppResult'),
  value: BoolValue,
})

export type EvalAppResult = Schema.Schema.Type<typeof EvalAppResult>
