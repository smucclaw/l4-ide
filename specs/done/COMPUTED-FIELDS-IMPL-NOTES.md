# Computed Fields: Implementation Notes

Companion to `COMPUTED-FIELDS-SPEC.md`. This file captures the blast radius analysis so it survives context compaction.

## Core Change: Add `Maybe (Expr n)` to `TypedName`

```haskell
-- Current (Syntax.hs:102-105):
data TypedName n = MkTypedName Anno n (Type' n)

-- Proposed:
data TypedName n = MkTypedName Anno n (Type' n) (Maybe (Expr n))
--                                                ^^^^^^^^^^^^^^
--                                    Nothing = stored field, Just expr = computed field
```

## Files That Pattern-Match or Construct `MkTypedName`

All paths relative to `jl4-core/src/L4/` unless noted.

| # | File | Lines | What it does | Change needed |
|---|------|-------|-------------|---------------|
| 1 | `Syntax.hs` | 102-105 | Type definition | Add field |
| 2 | `Parser.hs` | 838-845 | `reqParam` creates `MkTypedName` | Parse optional `MEANS expr`, pass `Nothing` or `Just expr` |
| 3 | `Print.hs` | 88 | Pretty-prints field | Print MEANS clause if present |
| 4 | `Names.hs` | 21 | `HasName` instance extracts name | Add `_` to pattern |
| 5 | `TypeCheck.hs` | 819 | `typedNameOptionallyNamedType` | Add `_` to pattern |
| 6 | `TypeCheck.hs` | 821-831 | `inferSelector` — type-checks fields | Add `_` to pattern; **also** type-check MEANS expr against field type, inject sibling fields into scope |
| 7 | `JsonSchema.hs` | 229-235 | `addField` for JSON schema gen | Add `_` to pattern; exclude computed fields from input schema |
| 8 | `JsonSchema.hs` | 282-297 | `addConField` for enum constructors | Add `_` to pattern |
| 9 | `Parser/ResolveAnnotation.hs` | 266-269 | `HasNlg` instance | Add `_` to pattern + reconstruct with field |
| 10 | `Parser/ResolveAnnotation.hs` | 657-660 | `HasDesc` instance | Add `_` to pattern + reconstruct with field |
| 11 | `TypeCheck/Annotation.hs` | 321-324 | `nlgTypedName` | Add `_` to pattern + reconstruct with field |
| 12 | `EvaluateLazy/Machine.hs` | 2138 | `scanConDecl` extracts field names | Add `_` to pattern |
| 13 | `EvaluateLazy/Machine.hs` | 2245-2254 | Creates selector functions for fields | **Key change:** for computed fields, generate a selector that evaluates the MEANS expr instead of extracting a stored value |

Outside `jl4-core/`:

| # | File | Lines | Change needed |
|---|------|-------|---------------|
| 14 | `jl4-decision-service/src/Examples.hs` | 246, 250 | Add `_` to pattern |
| 15 | `jl4-decision-service/src/Backend/FunctionSchema.hs` | 176, 180 | Add `_` to pattern |
| 16 | `jl4-decision-service/src/Server.hs` | 964, 968 | Add `_` to pattern |

## Implementation Status

All phases complete:

1. ✅ **Syntax.hs** — `MkTypedName` extended with `Maybe (Expr n)`; `ComputedSelector` added to `TermKind`
2. ✅ **Parser.hs** — `reqParam` parses optional `MEANS expr`
3. ✅ **Print.hs** — prints MEANS clause for computed fields
4. ✅ **Desugar.hs** — `desugarComputedFields` (synthetic DECIDEs), `detectComputedFieldCycles` (intra-record SCC analysis), `extractComputedFieldNames` (for CheckEnv)
5. ✅ **TypeCheck.hs** — cycle detection errors injected before type checking; `computedFields` map in `CheckEnv` for constructor rejection (`SuppliedComputedField` error); `scanFunSigDecide` tags synthetic DECIDEs as `ComputedSelector`
6. ✅ **JsonSchema.hs** — computed fields naturally excluded from input schemas by desugar-early strategy
7. ✅ **LSP** — `ComputedSelector` shown as Field with `(computed)` detail in completions; same semantic highlighting as stored selectors
8. ✅ **Record construction** — computed fields rejected in `WITH`/`OF` with clear error message
9. ✅ **Documentation** — DECLARE.md, MEANS.md, GLOSSARY.md updated; referential transparency paragraph added

## Strategy: Desugar Early (Recommended)

For each computed field `f` with `MEANS expr` on record `T` with stored fields `s1..sN`:

Generate a synthetic top-level function:
```
f(record) = let s1 = record's s1; ... sN = record's sN in expr
```

The existing projection machinery (`Proj` → `App`) then just calls this function. No evaluator changes needed beyond wiring up the synthetic function.

## Comma Parsing

No special handling was needed.  `parseAppArgs` (juxtaposition-based function
application) does not consume commas — it never did need to, since L4 uses
Haskell-style `f x y` for function application.  Commas are only consumed by
explicit comma-separated contexts (OF, LIST, WITH, CONSIDER, CONCAT, HAS fields).

This means `recordDecl'` keeps its `lsepBy ... TComma`, the colloquial form
`DECLARE Foo HAS x IS A NUMBER, y IS A NUMBER` still works, and MEANS bodies
can freely use any expression syntax.  No parser state flags are needed.

See `specs/done/COMPUTED-FIELDS-COMMA-PARSING.md` for full design analysis.

## Test Files (committed)

Golden tests (in `jl4/examples/`):
- `ok/computed-fields.l4` — comprehensive tests (863 examples): basic, chained, external, nested, WHERE/LET/IN, colloquial commas
- `not-ok/tc/computed-fields-cycle.l4` — mutual cycle (a → b → a)
- `not-ok/tc/computed-fields-self-cycle.l4` — self-referencing cycle (a → a)
- `not-ok/tc/computed-fields-override.l4` — supplying computed field in WITH constructor

Experiment files (in `jl4/experiments/`):
- `computed-fields-basic.l4` — simple boolean from number
- `computed-fields-chain.l4` — computed depending on computed
- `computed-fields-nested.l4` — nested record access
- `computed-fields-external.l4` — calling top-level functions
- `computed-fields-external-of.l4` — same but using OF syntax
- `computed-fields-legal.l4` — Employment Act pattern
- `computed-fields-age.l4` — age-from-birthdate snapshot
- `computed-fields-cycle.l4` — should fail (cycle)
- `computed-fields-override.l4` — should fail (constructor override)
- `computed-fields-where-let.l4` — WHERE/LET/IN in MEANS, with commas
- `carInsurance.l4` — rewritten to use computed fields as showcase
