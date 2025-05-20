// TODO: Not sure if this should be in viz-expr or in jl4-client-rpc

import { Schema } from 'effect'
import { VersionedDocId } from './versioned-doc-id.js'

/**********************************************
            Request payload: 
            InlineExprsRequestParams
***********************************************/

export const InlineExprsRequestParams = Schema.Struct({
  uniques: Schema.NonEmptyArray(Schema.Number).annotations({
    description: 'The Uniques of the exprs to inline.',
  }),
  verDocId: VersionedDocId,
}).annotations({
  identifier: 'InlineExprsRequestParams',
})

export type InlineExprsRequestParams = Schema.Schema.Type<
  typeof InlineExprsRequestParams
>

/**********************************************
      Response payload: InlineExprsSuccess
      (keeping it simple for now)
***********************************************/

/** We don't, e.g., have a FunDecl in the payload,
 * because we just want to know whether the request was successful.
 * If it is (e.g., if we were able to find the definienda for all the uniques in the request params),
 * then the server will respond with InlineExprsSuccess before updating the viz accordingly.
 */
export const InlineExprsSuccess = Schema.Struct({
  $type: Schema.tag('InlineExprsSuccess'),
})

export type InlineExprsSuccess = Schema.Schema.Type<typeof InlineExprsSuccess>

// TODO: Look quickly into how other LSPs handle structured errors for custom methods
// Most obvious structured error we might want: InlineExprsErrorDefiniensNotFound
