// TODO: Not sure if this should be in viz-expr or in jl4-client-rpc

import { Schema } from 'effect'
import { VersionedDocId } from './versioned-doc-id.js'
import { RenderAsLadderInfo } from '@repo/viz-expr'

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
            Response payload: 
          basically RenderAsLadderInfo
***********************************************/

export type InlineExprsResult = RenderAsLadderInfo

// TODO: Look quickly into how other LSPs handle structured errors for custom methods
// Most obvious structured error we might want: InlineExprsErrorDefiniensNotFound
