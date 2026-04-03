# Specification: @ref Annotation Attachment

## Executive Summary

This document specifies how `@ref` annotations should be attached to AST nodes. Unlike `@desc` (which attaches only to declarations), `@ref` can attach to **any AST node** to provide external reference links for documentation, traceability, and source mapping.

## Current Implementation Status

| Component               | Status | Location                                               |
| ----------------------- | ------ | ------------------------------------------------------ |
| Lexer recognizes `@ref` | ✅     | `jl4-core/src/L4/Lexer.hs:77` — `TRef !Text !AnnoType` |
| Parser collects `@ref`  | ✅     | `jl4-core/src/L4/Parser.hs:62` — `PState.refs`         |
| `Ref` type in AST       | ✅     | `jl4-core/src/L4/Syntax.hs:639`                        |
| Attachment to AST       | ❌     | TODO in `Parser.hs:216-218`                            |

### Parser TODO (existing)

```haskell
-- jl4-core/src/L4/Parser.hs:216-218
-- TODO:
-- (1) should ref-src /ref-map be allowed anywhere else than at the toplevel
-- (2) should we add it to the AST at all? Currently we don't need it
```

## Related Specifications

| Annotation | Spec                                       | Attachment Rule                                    |
| ---------- | ------------------------------------------ | -------------------------------------------------- |
| `@nlg`     | (implemented)                              | Attaches to `Name` nodes based on source position  |
| `@desc`    | `doc/dev/specs/todo/EXPORT-SYNTAX-SPEC.md` | Attaches to immediately following `TopDecl`        |
| `@ref`     | this document                              | Attaches to **any** immediately following AST node |

## Design

### Attachment Rule

`@ref` annotations attach to the **immediately following AST node** regardless of its type. This provides maximum flexibility for authors to annotate:

- Declarations (`DECLARE`, `DECIDE`, `ASSUME`)
- Expressions (function calls, literals, operators)
- Types
- Names
- Clauses within declarations

### Example Usage

```l4
@ref https://example.com/regulations/section-5
DECIDE
  @ref https://example.com/regulations/section-5.1
  GIVEN age IS A Number
  GIVETH A Boolean
  isAdult age MEANS
    @ref https://example.com/regulations/section-5.1.a
    age >= 18
```

### Annotation Variants

The lexer recognizes the following `@ref` variants:

| Syntax              | Token                | Purpose                           | Status         |
| ------------------- | -------------------- | --------------------------------- | -------------- |
| `@ref <url> [type]` | `TRef Text AnnoType` | Reference link with optional type | ✅ Keep        |
| `@ref <filename>`   | `TRef Text AnnoType` | Clickable file link (Ctrl+click)  | ✅ Keep        |
| `@ref-map <text>`   | `TRefMap Text`       | Inline reference mapping          | ✅ Keep        |
| `@ref-src <file>`   | `TRefSrc Text`       | CSV file loading                  | ❌ **REMOVED** |

### Removed: `@ref-src` (CSV Loading)

**Decision**: The `@ref-src` annotation for loading reference mappings from CSV files has been removed from the language.

**Rationale**:

- No file IO in the language core (required for WASM compatibility)
- CSV parsing added complexity with limited benefit
- Inline `@ref-map` provides equivalent functionality without file dependencies

**Migration**: Convert CSV entries to inline `@ref-map` annotations:

```l4
-- BEFORE (removed):
@ref-src citations.csv

-- AFTER (use inline mappings):
@ref-map "1981/61 sec. 2" https://www.legislation.gov.uk/ukpga/1981/61/section/2
@ref-map "1981/61 sec. 3" https://www.legislation.gov.uk/ukpga/1981/61/section/3
```

### Removed: Regex Patterns in References

**Decision**: Regex patterns (e.g., `regex:sg-c-(\d{4})-([a-z]+)-(\d+)`) are no longer supported.

**Rationale**:

- Required `pcre2` C library (not WASM-compatible)
- Only one regex pattern was used in practice
- Verbatim matching is simpler and sufficient for most use cases

**Migration**: Use explicit `@ref-map` entries for each reference:

```l4
-- BEFORE (removed):
@ref-map "regex:sg-c-(\d{4})-([a-z]+)-(\d+)" https://www.elitigation.sg/gd/s/$1_$2_$3

-- AFTER (explicit entries):
@ref-map "sg-c-2025-sghcf-14" https://www.elitigation.sg/gd/s/2025_sghcf_14
@ref-map "sg-c-2024-sghc-123" https://www.elitigation.sg/gd/s/2024_sghc_123
```

### New: File Reference Links

`@ref` with a filename (not a URL) creates a **clickable file link** in the IDE:

```l4
-- Ctrl+click opens the file if found in IMPORT search paths
DECIDE foo IS TRUE @ref legal-opinion.pdf
DECIDE bar IS FALSE @ref ../docs/regulation-summary.md
```

The IDE will search for the file in:

1. Current directory (same as the L4 file)
2. Directories specified in IMPORT search paths

## Implementation

### Phase 1: Add `annRef` to Anno

Extend the `Extension` type in `Syntax.hs` to include `Ref`:

```haskell
data Extension = Extension
  { inherit  :: Maybe Inherit
  , nlg      :: Maybe Nlg
  , desc     :: Maybe Desc
  , ref      :: Maybe Ref    -- NEW
  }
```

Add corresponding lens:

```haskell
annRef :: Lens' Anno (Maybe Ref)
annRef = #extra % #ref
```

### Phase 2: Create `addRefCommentsToAst`

In `jl4-core/src/L4/Parser/ResolveAnnotation.hs`, create attachment logic following the `@nlg` pattern but with broader scope:

```haskell
-- | Attach @ref annotations to the AST nodes that immediately follow them.
-- Unlike @nlg (which targets Name nodes) or @desc (which targets TopDecl),
-- @ref can attach to any AST node.
addRefCommentsToAst :: HasRef a => [Ref] -> a -> (a, RefS)
addRefCommentsToAst refs ast = ...
```

Key difference from `@nlg`:

- `@nlg` only attaches to `Name` nodes (specific)
- `@ref` attaches to any node (generic)

This means the `HasRef` class needs instances for all AST node types, not just `Name`.

### Phase 3: Wire up in Parser

Modify `execParserForTokens` in `Parser.hs`:

```haskell
execParserForTokens p file input ts =
  case runJl4Parser env st p (showNormalizedUri file) stream of
    Left err -> Left (...)
    Right (a, pstate) ->
      let
        (a1, nlgS) = Resolve.addNlgCommentsToAst pstate.nlgs a
        (a2, descS) = Resolve.addDescCommentsToAst pstate.descs a1  -- from @desc spec
        (a3, refS) = Resolve.addRefCommentsToAst pstate.refs a2    -- NEW
      in
        Right (a3, nlgS.warnings ++ descS.warnings ++ refS.warnings, pstate)
```

## Use Cases

### Documentation & Traceability

Link L4 rules to source legislation, requirements documents, or external specifications:

```l4
@ref https://legislation.gov/act/2024/section-42
DECIDE premium calculation ...
```

### IDE Integration

The LSP could use `@ref` annotations to:

- Show hover tooltips with linked documentation
- Provide "Go to Reference" navigation
- Display reference links in code lenses

### Audit & Compliance

Track which rules implement which regulatory requirements:

```l4
@ref compliance:SOX-404
@ref audit:2024-Q1-review
DECIDE financialControl ...
```

## Testing

Add test files:

- `jl4/examples/ok/ref-declaration.l4` — refs on declarations
- `jl4/examples/ok/ref-expression.l4` — refs on expressions
- `jl4/examples/ok/ref-nested.l4` — refs at multiple nesting levels

## References

- `jl4-core/src/L4/Parser/ResolveAnnotation.hs` — existing `@nlg` attachment pattern
- `doc/dev/specs/todo/EXPORT-SYNTAX-SPEC.md` — `@desc` attachment specification
- `doc/dev/specs/todo/WASM-LSP-SPEC.md` — WASM compatibility (reason for `@ref-src` removal: No file-io)
- Issue #635 — decision service improvements (related)
