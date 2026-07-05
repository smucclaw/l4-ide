# DONE: exhaustiveness checking for `CONSIDER` over *imported* enums

## Status

**Resolved** on branch `mengwong/consider-exhaustiveness`, together with the
WHERE/LET-nested case and the builtin `BOOLEAN` case, by re-pointing the
exhaustiveness oracle at `entityInfo`. See
`specs/todo/consider-exhaustiveness-scope-hardening.md` for the full write-up of
the implemented approach and why it superseded the interim/split-field plans.

## The gap (as originally reported)

A non-exhaustive `CONSIDER` over an enum imported from another module was not
flagged:

```
-- region.l4 (or any library, e.g. `time`'s `Meridiem Indicator`)
DECLARE Region IS ONE OF central; suburban; rural
```
```
-- main.l4
IMPORT region
GIVEN r IS A Region
`rate` MEANS
    CONSIDER r
      WHEN central  THEN 6
      WHEN suburban THEN 4     -- `rural` omitted; was NOT flagged
```

## Root cause (confirmed)

The oracle (`buildConstructorLookup`) folded over the frame-local
`declareDeclarations` map, which is reset to `Map.empty` at both import
boundaries (`jl4-core/src/L4/Import/Resolution.hs`, `jl4-lsp/src/LSP/L4/Rules.hs`).
`entityInfo` — which carries every constructor as a `KnownTerm _ Constructor`
entry — **is** unioned across imports, but the oracle never read it.

## Fix

`constructorsInScopeFromEntityInfo` (`jl4-core/src/L4/TypeCheck.hs`) now derives
the type→constructors map from `entityInfo` instead of `declareDeclarations`.
Because `entityInfo` is cumulative across scopes and unioned across imports, an
imported enum's constructors are visible to `checkConsider` with no new field,
no propagation plumbing, and no import-boundary changes. Output shape and keying
(type-head `Unique` → `[Resolved]`) are unchanged, so `concretizeInfo` and its
analysis were untouched.

## Verified

`l4 check` on a module importing the `time` library and matching
`Meridiem Indicator` non-exhaustively (omitting `pm`) now reports
`The following branches still need to be considered: WHEN pm THEN` at type-check
time (previously accepted, then crashed at eval).

Note: this case is not covered by a golden fixture because any library import
writes the resolved absolute filesystem path into the generated golden (a
harness limitation — the import-resolution logger output is not path-sanitized).
It is covered by the `BOOLEAN` golden (same `entityInfo`-sourced oracle path) and
by live verification.
