# Retire the CONSIDER-exhaustiveness bug family

## Status: IMPLEMENTED (via entityInfo-sourcing, not the split-field design below)

Branch `mengwong/consider-exhaustiveness`. Two adversarial workflows (a design
red-team and a whole-family sweep) converged on a fix that is **smaller and more
complete** than the split-field proposal drafted further down this document. The
draft below is retained as the record of the alternatives considered.

### What shipped

The exhaustiveness oracle no longer reads the frame-local, import-reset
`declareDeclarations` map at all. `constructorsInScopeFromEntityInfo`
(`jl4-core/src/L4/TypeCheck.hs`) derives the type→constructors map from
`entityInfo`, which is already (a) extended per scope, (b) unioned across both
import boundaries, and (c) inclusive of builtin constructors. It scans
`KnownTerm _ Constructor` entries, extracts each constructor's result-type head
`Unique` (strip `Forall`/`Fun`, take the `TyApp` head), and reconstructs each
constructor as its definition `Resolved` (`Def u name`) straight from the entity
entry. The output has the **exact same shape and keying** the old declares-based
`buildConstructorLookup` produced (`Map Unique [Resolved]`, keyed by type-head
`Unique`), so `concretizeInfo` and its "Lower Your Guards" analysis were not
touched — the red-team's false-positive-freeness proof (adding keys can't change
an existing key's value; distinct types have distinct `Unique`s) carries over
verbatim.

This single-function change closes, in one move and with **no new record field,
no propagation plumbing, no import-boundary edits, and no `NFData`/positional-
pattern churn**:

- non-exhaustive `CONSIDER` over an **imported** enum (subsumes
  `consider-exhaustiveness-imported-enums.md` — now marked DONE);
- non-exhaustive `CONSIDER` over an enum declared in an enclosing **WHERE/LET**
  block (the original archetype; the interim `withDeclares` union was reverted
  since entityInfo-sourcing subsumes it);
- non-exhaustive `CONSIDER` over builtin **`BOOLEAN`** (a live bug the family
  sweep found and repro'd: `WHEN TRUE` with no `FALSE` type-checked clean, then
  crashed at eval; the false `-- BOOLEAN … works correctly` comment was fixed);
- non-exhaustive `CONSIDER` over builtin **`MAYBE`** and **`EITHER`** (complete
  `Constructor` sets; enabling them was confirmed safe against the full corpus,
  891/0, i.e. no intentional partial matches in prelude/libraries/examples).

`declareDeclarations`/`assumeDeclarations` are back to pure by-`SrcRange` lookup
maps (their only remaining readers).

### Deliberately scoped OUT (see follow-ups)

- Builtin `LIST`/`CONTRACT`/`EVENT` are excluded from the oracle
  (`builtinNonExhaustiveTypeUniques`) because their scanned constructor set is
  **incomplete**: `LIST`'s `cons` is tagged `Computable` (so the scan yields only
  `[EMPTY]`), and the regulative `CONTRACT`/`EVENT` deontic operators are not all
  plain constructors and are matched partially by the core. Checking against a
  partial set would mis-report. → `consider-exhaustiveness-builtin-containers.md`.
  (`MAYBE`/`EITHER`, which have complete sets, are **not** excluded — see above.)
- The `SynonymDecl -> []` TODO was **empirically ruled out** as a live bug (a
  sweep agent built + ran `synonym_enum.l4`; it already warns, because guards
  carry the underlying enum type, not the synonym's `Unique`).

### Round 2 (2026-07): the deferred items, adjudicated and implemented

A second adversarial pass (5 empirical scouts → 5 red-team reviewers →
synthesis; then implementation; see the commits stacked on this branch after
the entityInfo fix) worked through every deferred item and found three NEW
live engine bugs that jumped the queue:

**Shipped, in commit order:**

1. **E1+E2 (NEW)** — `desugarPat`'s pattern-variable recycling map was keyed
   by constructor alone, conflating SIBLING occurrences of one constructor in
   a single branch: false blocking redundancy on valid code AND
   self-referential constraints that hung `l4 check`/the LSP (exit 124).
   Re-keyed by (scrutinee position, constructor); `expandToPattern` gained an
   occurs-check backstop.
2. **T1d (NEW)** — literal (`PatLit`) and expression (`PatExpr`) patterns
   desugar to ZERO guards, so the analysis treated refutable arms as
   match-alls (blocking false redundancy on `WHEN JUST 5 / WHEN JUST x`;
   false-negative exhaustiveness). The analysis now stands down for any
   CONSIDER containing them. The principled fix (opaque refutable guards) is
   deferred — see T3e below.
3. **T1b** — `isPrimitiveType` compared type NAMES, so a user enum named
   `NUMBER` (reachable via constructor inference) silently skipped analysis.
   Now compares builtin `Unique`s.
4. **T1a** — one shared `unionImportedCheckEnv` (jl4-core) for both import
   boundaries; documented left-bias (the Rules.hs `assert` was compiled out
   at `-O` anyway; a `Unique` embeds its defining module, so colliding keys
   are identical copies).
5. **T1c** — missing-arm suggestions are now pasteable L4: arity-padded
   underscores (``WHEN square `_` THEN``), nested parens
   (``WHEN wa (JUST `_`) THEN``), via a dedicated renderer.
6. **P1 `@partial`** — author-declared partiality (rides the @export desc
   channel end-to-end); suppresses missing-branch warnings only, per Decide.
7. **T2a LIST** — enabled by injecting `listUnique -> [EMPTY, cons]` into the
   oracle (no consInfo re-tag); prelude/loop remediation per
   `consider-exhaustiveness-builtin-containers.md`.
8. **Prelude `maximum` bug (NEW, answer-changing, isolated commit)** —
   `maximum` over `LIST OF MAYBE NUMBER` delegated to `minimum1`, and
   `maximum1` folded with `min`: both returned the MINIMUM.
9. **T2b** — `SuspiciousBinderPattern`, an SInfo-severity (never blocking)
   hint when a top-level binder arm's name is case-insensitively equal or one
   edit (4-char floor) from an UNCOVERED constructor of the scrutinee's type
   (the misspelled-constructor-becomes-binder hole; trailing typo binders are
   invisible to both warnings).

**Still deferred, with reasons:**

- **T3b `computedFields` cross-module** — keyed by `RawName`, so a naive
  boundary union would conflate same-named records across modules (false
  `SuppliedComputedField` errors) — worse than the gap. Real fix = re-key by
  `Unique`. Impact today is error-message quality only (an importer supplying
  an imported record's computed field still hard-errors via the out-of-scope
  fallback).
- **T3c loud `concretizeInfo` NoInfo fallback** — legitimate NoInfo remains
  by design (excluded builtins, type variables, ASSUMEd opaque types); a loud
  version would false-alarm. The exclusion set is now small and documented at
  its definition site.
- **T3d `FrameLocal`/`InScope` newtypes** — no live enumeration reader of any
  frame-local map remains; the newtype would churn every CheckEnv consumer to
  guard a hypothetical. Revisit if an enumeration-style consumer is added.
- **T3e literal-aware LYG constraints** — the principled successor to T1d's
  bail-out: opaque refutable guards for `PatLit`, constructor-valued
  `EXACTLY` as constructor patterns. Would restore true positives for
  genuinely-partial literal matches and could eventually subsume the
  primitive-type skip. Needs design care in the redundancy contradiction
  checker.
- **T3f (NEW) disjunction-lossy `normalizeRefinement`** — `Nabla`'s Semigroup
  UNIONS the constraint sets of uncovered DISJUNCTS into one set, an
  approximation that (a) under-enumerates missing arms on multi-field
  records (a two-BOOLEAN record match of `V TRUE TRUE` reports two of the
  three uncovered combinations) and (b) fully under-DETECTS some multi-branch
  nested misses (fixture `ok/pattern-nested-partial-undetected.l4` pins the
  silent case and doubles as the termination tripwire). The real fix is a
  proper Lower-Your-Guards residual-set representation — same design bucket
  as T3e.
- **Diamond-import environment starvation (NEW, out of scope)** — in
  `resolveImports` (Resolution.hs ~179-242), when A imports B and C and both
  import D, C is type-checked with an EMPTY dependency environment (D was
  already marked resolved by B's branch) and its errors are swallowed.
  Affects the `checkWithImports` consumers (wasm/actus-analyzer/API), not the
  CLI/LSP Shake path. **Issue to file**, with a pending ImportResolutionSpec
  diamond case.
- **EVENT type-name shadowing (NEW, out of scope)** — see
  `consider-exhaustiveness-builtin-containers.md`; blocks the (otherwise
  sound) EVENT exhaustiveness enablement. **Issue to file.**
- Smaller notes: `isStringCoercible` still matches by type NAME (same
  pattern T1b fixed; harmless today); combining `@export` and `@partial` as
  two separate annotation LINES picks one winner (the desc channel keeps a
  single leading desc) — combine on one line (`@export partial`) instead;
  local dev: `~/.local/share/jl4/libraries` SHADOWS the repo's libraries in
  import resolution — set `JL4_LIBRARY_PATH` to the repo's
  `jl4-core/libraries` when testing library changes, or stale installed
  copies will masquerade as regressions.

---

## Historical: the split-field proposal (NOT implemented — superseded above)

Draft proposal. Superseded the interim union fix and subsumed the imported-enum
follow-up. Retained for the record.

## TL;DR

`CONSIDER` exhaustiveness checking silently no-ops in two places — inside a
`WHERE`/`LET` block, and over an enum imported from another module. Both are the
same defect: the type-checker's exhaustiveness analysis reads
`declareDeclarations` with `Map.elems` (it wants *every enum in scope*), but that
field is maintained as a **current-frame** map (replaced on entering a scope,
reset to empty at the import boundary) because its *other* reader only ever looks
up the current frame by `SrcRange`. One field, two readers, incompatible
invariants, and nothing forces them to stay reconciled.

The interim fix (`Map.fromList rdeclares <> s.declareDeclarations` in
`withDeclares`) patches the `WHERE` symptom by making the field accumulate. This
proposal instead **splits the two contracts** so the bug class cannot recur, and
fixes the imported-enum case as a natural consequence rather than as a second
special case.

## Background: the exact defect

`CheckEnv` (`jl4-core/src/L4/TypeCheck/Types.hs`) carries four frame-local maps:

| field | reader(s) | reader contract |
|---|---|---|
| `functionTypeSigs` | `lookupFunTypeSigByAnno` | by `SrcRange`, current frame only |
| `declTypeSigs`     | `lookupDeclTypeSigByAnno` | by `SrcRange`, current frame only |
| `assumeDeclarations` | `lookupAssumeCheckedByAnno` | by `SrcRange`, current frame only |
| `declareDeclarations` | `lookupDeclareCheckedByAnno` **and** `checkConsider` | by `SrcRange` **and** `Map.elems` (all in scope) |

The first three are read in exactly one way — a `SrcRange` lookup for the thing
being checked right now, which always lives in the current frame. For them,
"replace the map on entering a scope, reset it at the import boundary" is *correct*
and efficient.

`declareDeclarations` grew a second reader when exhaustiveness analysis was added
(`jl4-core/src/L4/TypeCheck.hs`):

```haskell
-- checkConsider, ~line 1158
resolvedDecls <- asks (Map.elems . (.declareDeclarations))
let cl = buildConstructorLookup resolvedDecls        -- Map Unique [Resolved]
```

`buildConstructorLookup` needs the constructor set of the *scrutinee's* type,
which may be declared in an enclosing block or an imported module — i.e. it needs
"all enums in scope," not "this frame's declares." But the field's writers were
never revisited:

- `withDeclares` (`local \s -> s { declareDeclarations = Map.fromList rdeclares … }`)
  replaced the map on entering every `WHERE`/`LET`. A block with no local
  `DECLARE`s installed `Map.empty`, dropping the enclosing enums. → **WHERE bug.**
- `unionCheckEnv` (`jl4-lsp/src/LSP/L4/Rules.hs`, ~line 545) and the import env base
  (`jl4-core/src/L4/Import/Resolution.hs`, ~line 377) set
  `declareDeclarations = Map.empty` per dependency. → **imported-enum bug.**

Crucially, the constructor **names** stay in scope the whole time
(`extendKnownMany topDeclares` extends `environment`/`entityInfo` cumulatively and
wraps the `local`), so name resolution and type-checking of the branches succeed.
Only the type→constructors enumeration comes back empty, so `checkConsider`
computes an empty "missing" set and emits nothing. The omitted case then fails
only at eval time with `Value &NNN@… has no corresponding pattern` (exit 0). A
hole invisible on every path except the one enumeration.

## Design: split "in scope" from "of this frame"

Introduce a dedicated, monoidal field for the enumeration reader and leave
`declareDeclarations` as a purely frame-local map for the `SrcRange` reader.

### 1. New `CheckEnv` field

```haskell
data CheckEnv = MkCheckEnv
  { …
  , declareDeclarations :: !(Map SrcRange (DeclChecked (Declare Resolved)))
    -- ^ FRAME-LOCAL. Read only by lookupDeclareCheckedByAnno, by SrcRange.
    --   Replaced per scope / reset at import boundary — correct for that reader.
  , constructorsInScope :: !(Map Unique [Resolved])
    -- ^ CUMULATIVE. type Unique -> its data constructors, for every enum/record
    --   visible at this point (enclosing blocks + imported modules). Only ever
    --   extended; never replaced. Read by checkConsider.
  , …
  }
```

`Map Unique [Resolved]` is exactly the shape `buildConstructorLookup` already
produces, and it is trivially monoidal: keys are type `Unique`s (globally unique
per definition), so `Map.union` never collides. It is also lightweight — a
constructor-name list per type — far cheaper to thread through the Shake cache
than the full `DeclChecked (Declare Resolved)` map.

### 2. Populate it where declares become known

In `withDeclares`, fold the block's own declares into the inherited map instead of
touching `declareDeclarations`'s enumeration duty:

```haskell
extendKnownMany topDeclares . local \s -> s
  { declareDeclarations = Map.fromList rdeclares          -- back to frame-local
  , assumeDeclarations  = Map.fromList rassumes           -- back to frame-local
  , constructorsInScope =
      buildConstructorLookup (snd <$> rdeclares) <> s.constructorsInScope
  }
```

Because `constructorsInScope` is unioned with `s.constructorsInScope`, nested
scopes accumulate automatically and no callsite has to "remember" to preserve the
parent. (This also lets us **revert** the interim `assumeDeclarations` union:
assumed types are opaque — you cannot `ASSUME X IS ONE OF …` — so they contribute
no constructors and never needed accumulating.)

### 3. Read it in `checkConsider`

```haskell
cl <- asks (.constructorsInScope)      -- was: buildConstructorLookup <$> Map.elems …
```

Also removes a per-`CONSIDER` recomputation of `buildConstructorLookup`.

### 4. Propagate across the import boundary

Add `constructorsInScope :: Map Unique [Resolved]` to `TypeCheckResult`
(`jl4-lsp/src/LSP/L4/Rules.hs`) and its `NFData` instance, populate it from the
checked module, and **union** (not reset) in `unionCheckEnv`:

```haskell
, constructorsInScope =
    Map.union cEnv.constructorsInScope tcRes.constructorsInScope
```

Producing it: have `checkProgram` / `doCheckProgramWithDependencies` return the
module's top-level `buildConstructorLookup` result (checkProgram's top-level
`withDeclares` already has the module `rdeclares`), and thread it into
`CheckResult` → `TypeCheckResult`. This is the only genuinely new plumbing, and it
carries the small `Map Unique [Resolved]`, not the heavy declares map.

## Why this retires the class, not just the instances

- The enumeration reader now reads a field whose **type and name say "cumulative,
  in scope"** and whose only sane combinator is `<>`. There is no per-frame
  "current" version of it to accidentally install.
- The by-`SrcRange` readers keep their correct, efficient replace-per-frame maps.
  The reset-at-boundary (`Map.empty`) is now applied **only** to genuinely
  frame-local maps, where it is correct.
- The next feature that needs "all X in scope" has a worked pattern to copy
  (monoidal field, accumulate in `local`, union across imports) instead of
  overloading a lookup map and re-triggering this exact bug.

## Alternatives considered

1. **Keep the interim fix only** (union `declareDeclarations` in `withDeclares`).
   Fixes the `WHERE` case; leaves the imported-enum case; perpetuates the
   two-consumers overload, so the trap remains for the next `Map.elems` reader and
   for anyone who "cleans up" the union back to a replace.
2. **Also union `declareDeclarations` through `TypeCheckResult`** (extend interim
   fix to imports). Fixes both symptoms but ships the full `DeclChecked` map
   through the Shake cache for a job that needs only type→constructors, and still
   overloads one field with two contracts.
3. **Rebuild the lookup from `entityInfo`** (already merged across imports). No new
   field, but hacky: `KnownType`'s `[Resolved]` is the type's *parameters*, not its
   constructors; constructors are separate `KnownTerm _ Constructor` entries that
   would have to be scanned and grouped by result-type `Unique`, disambiguating
   record vs enum constructors. Fragile and implicit.

Option 3's one virtue — "the thing is already propagated" — is captured cleanly by
making `constructorsInScope` a first-class propagated value instead of
reverse-engineering it.

## Change list

- `jl4-core/src/L4/TypeCheck/Types.hs`: add `constructorsInScope` to `CheckEnv`
  (+ `initialCheckEnv = Map.empty`); it participates in `Eq`/`NFData`/`Generic`
  automatically; carry it verbatim in `extendEnv`.
- `jl4-core/src/L4/TypeCheck.hs`: populate in `withDeclares`; read in
  `checkConsider`; revert `declareDeclarations`/`assumeDeclarations` to
  frame-local; surface the module's map from `checkProgram` /
  `doCheckProgramWithDependencies` into `CheckResult`.
- `jl4-lsp/src/LSP/L4/Rules.hs`: add field to `TypeCheckResult` (+ `NFData`),
  populate from `CheckResult`, union in `unionCheckEnv`.
- `jl4-core/src/L4/Import/Resolution.hs`: base value `Map.empty` (unchanged shape).

## Testing

- Keep the shipped fixtures (`ok/consider-exhaustive-in-where.l4`,
  `not-ok/tc/consider-nonexhaustive-in-where.l4`).
- Add a multi-module pair: importer with a non-exhaustive `CONSIDER` over an
  imported enum → must be flagged; exhaustive variant → must pass and evaluate.
- Add a nested `WHERE` inside a module that *also* imports an enum, matching over
  the imported type, to exercise both accumulation paths at once.
- Regression: confirm Shake early-cutoff still invalidates the importer when the
  imported enum gains/loses a constructor (the new `TypeCheckResult` field must be
  forced in `rnf`).
- Full suites: `jl4-test`, `jl4-core-test`, `l4-cli-test`.

## Risk

Low–moderate. The `CheckEnv` change is mechanical. The only real surface is the new
`TypeCheckResult` field and its `NFData`/cache behaviour; the mitigation is that it
is a small, strict, monoidal map, and the early-cutoff test above pins the
invalidation behaviour. Behaviour change is strictly *more* diagnostics (previously
missed non-exhaustive matches now reported); the full golden suite bounds the blast
radius, as it did for the interim fix (887/0).
