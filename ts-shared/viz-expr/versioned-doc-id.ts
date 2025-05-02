import { Schema } from 'effect'
// import { Integer } from './effect-utils.js'

/** This is just the VersionedTextDocumentIdentifier as an Effect schema.
 * https://github.com/microsoft/vscode-languageserver-node/blob/df05883f34b39255d40d68cef55caf2e93cff35f/types/src/main.ts#L1797
 */
export const VersionedDocId = Schema.Struct({
  $type: Schema.tag('VersionedDocId'),
  uri: Schema.String,
  version: Schema.Number,
}).annotations({
  identifier: 'VersionedDocId',
  description:
    "VersionedDocId is meant to correspond to LSP's VersionedTextDocumentIdentifier",
})

export type VersionedDocId = Schema.Schema.Type<typeof VersionedDocId>
