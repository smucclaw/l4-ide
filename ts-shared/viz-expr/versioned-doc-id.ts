import { Schema } from 'effect'

/** This is an Effect Schema that is isomorphic to the current LSP [VersionedTextDocumentIdentifier](https://github.com/microsoft/vscode-languageserver-node/blob/df05883f34b39255d40d68cef55caf2e93cff35f/types/src/main.ts#L1797).
 *
 * I am not using the LSP type using the LSP VersionedTextDocumentIdentifier type directly,
 * because there's a subtle difference in the design intent of the VersionedTextDocumentIdentifier and our VersionedDocId.
 * As the VersionedTextDocumentIdentifier name suggests,
 * this is a type that seems specific to *text* documents;
 * this impression is further corroborated by how the LSP spec
 * also has, e.g., a [VersionedNotebookDocumentIdentifier](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#versionedNotebookDocumentIdentifier)
 * that is structurally the same as the VersionedTextDocumentIdentifier.
 *
 * But whether the source program was a text document or notebook
 * (or, to use an even further possibility, something created in a
 * structural editor), is *not* something that the *Ladder visualizer* cares about.
 * (Or at least, that was a deliberate
 * design decision I had been trying out --- so, e.g, the Ladder visualizer
 * does not know about things like source positions or ranges.)
 * It is for this reason that I have named this type `VersionedDocId`.
 */
export const VersionedDocId = Schema.Struct({
  uri: Schema.String,
  version: Schema.Number,
}).annotations({
  identifier: 'VersionedDocId',
  description:
    "VersionedDocId is meant to be isomorphic to the current LSP VersionedTextDocumentIdentifier/ VersionedNotebookDocumentIdentifier. It's just that it's deliberately agnostic about whether the source program was a *text* document or notebook or something else altogether.",
})

export type VersionedDocId = Schema.Schema.Type<typeof VersionedDocId>
