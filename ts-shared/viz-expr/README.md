# viz-expr

The `viz-expr` package contains types/[Effect](https://effect.website/) schemas/interfaces and util functions
for data / payloads for the ladder visualizer.

## Key Files

- `viz-expr.ts`: 'VizExpr' or 'IREXpr': Types for the intermediate expressions that the Ladder visualizer uses as input, as well as utilities for pretty printing the intermediate expressions and exporting a JSON schema for them.
- `eval-on-backend.ts`: Protocol types for evaluating expressions on the backend.
- `inline-exprs.ts`: Protocol types for the inlining-expression functionality.
- `versioned-doc-id.ts`: Document version identifier schema. Isomorphic to LSP's `VersionedTextDocumentIdentifier` but agnostic to document type.
- `index.ts`: Barrel file exporting all main types and schemas.

---

For more details, see the source files and Effect schema documentation.
