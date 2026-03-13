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

## Recommended Implementation Order

1. **Syntax.hs** — add the field to `MkTypedName`
2. **Fix all pattern matches** — items 3-6, 8-12, 14-16 above (mechanical: add `_` wildcard)
3. **Parser.hs** — extend `reqParam` to optionally parse `MEANS expr`
4. **Print.hs** — extend printer for the MEANS clause
5. **TypeCheck.hs `inferSelector`** — type-check the MEANS expr with sibling fields in scope; cycle detection
6. **EvaluateLazy/Machine.hs** — generate thunk-based selectors for computed fields
7. **JsonSchema.hs `addField`** — exclude computed fields from input schemas, include in output
8. **Record construction** — reject computed fields in `WITH`/`OF` (TypeCheck + Desugar)

## Strategy: Desugar Early (Recommended)

For each computed field `f` with `MEANS expr` on record `T` with stored fields `s1..sN`:

Generate a synthetic top-level function:
```
f(record) = let s1 = record's s1; ... sN = record's sN in expr
```

The existing projection machinery (`Proj` → `App`) then just calls this function. No evaluator changes needed beyond wiring up the synthetic function.

## Test Files (committed)

- `jl4/experiments/computed-fields-basic.l4` — simple boolean from number
- `jl4/experiments/computed-fields-chain.l4` — computed depending on computed
- `jl4/experiments/computed-fields-nested.l4` — nested record access
- `jl4/experiments/computed-fields-external.l4` — calling top-level functions
- `jl4/experiments/computed-fields-legal.l4` — Employment Act pattern
- `jl4/experiments/computed-fields-age.l4` — age-from-birthdate snapshot
- `jl4/experiments/computed-fields-cycle.l4` — should fail (cycle)
- `jl4/experiments/computed-fields-override.l4` — should fail (constructor override)
