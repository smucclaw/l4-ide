import { Schema } from 'effect'
// import { Integer } from './effect-utils.js'

/** This is just the VersionedTextDocumentIdentifier as an Effect schema.
 * https://github.com/microsoft/vscode-languageserver-node/blob/df05883f34b39255d40d68cef55caf2e93cff35f/types/src/main.ts#L1797
 *
 * I'm writing an Effect Schema that's iso to the LSP type using the LSP type directly,
 * because that seems necessary to write an Effect schema for EvalAppRequestParams.
 * But perhaps we should just give up on having an Effect schema
 * for EvalAppRequestParams.
 */
export const VersionedDocId = Schema.Struct({
  uri: Schema.String,
  version: Schema.Number,
}).annotations({
  identifier: 'VersionedDocId',
  description:
    "VersionedDocId is meant to correspond to LSP's VersionedTextDocumentIdentifier",
})

export type VersionedDocId = Schema.Schema.Type<typeof VersionedDocId>
