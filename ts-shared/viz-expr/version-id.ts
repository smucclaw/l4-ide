import { Schema } from 'effect'
import { Integer } from './effect-utils.js'

/******************************************
            VersionId
*******************************************/

/** This is just the VersionedTextDocumentIdentifier as an Effect schema.
 * https://github.com/microsoft/vscode-languageserver-node/blob/df05883f34b39255d40d68cef55caf2e93cff35f/types/src/main.ts#L1797
 */
export const VersionId = Schema.Struct({
  $type: Schema.tag('VersionId'),
  uri: Schema.String,
  // Using a refinement predicate for this might be overkill
  version: Integer,
}).annotations({
  identifier: 'VersionId',
  description:
    "VersionId is meant to correspond to LSP's VersionedTextDocumentIdentifier",
})

export type VersionId = Schema.Schema.Type<typeof VersionId>
