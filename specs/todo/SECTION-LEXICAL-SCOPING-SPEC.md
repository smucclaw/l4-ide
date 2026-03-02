# Section Lexical Scoping Specification

**Status:** Draft
**Author:** Meng Wong, with analysis from Claude
**Date:** 2026-03-02
**Branch:** TBD

---

## 1. Motivation

### 1.1 The Legal Parallel

Legislation routinely uses the phrase "for purposes of this subsection" to define terms
whose scope is limited to the enclosing subsection. Within that subsection, the defined
term is used without qualification -- nobody writes "this subsection's X" every time
they refer to X. Qualification is only needed when referring to a term defined in a
*different* section.

This convention is the legal equivalent of **lexical scoping** in programming languages.
A variable defined in a block is directly accessible within that block; outer definitions
of the same name are shadowed; cross-scope access requires explicit qualification.

### 1.2 The Problem

Currently, L4's section system generates qualified names for all definitions inside
sections. When the same `RawName` is defined in two different sections, *any* unqualified
reference to that name -- even from within the same section -- triggers an
`AmbiguousTermError` or `AmbiguousTypeError`. The compiler reports "multiple definitions,
cannot choose" and requires full qualification everywhere.

**Example of current behavior:**

```l4
§ employment
ASSUME `base salary` IS A NUMBER

§ consulting
ASSUME `base salary` IS A NUMBER

DECIDE `total compensation` IS A NUMBER
DECIDE `total compensation` MEANS `base salary` * 1.1
-- ERROR: AmbiguousTermError for `base salary`
-- compiler sees both employment.`base salary` and consulting.`base salary`
-- even though we are inside § consulting
```

The user must write `consulting.`base salary`` to disambiguate, which is redundant and
unnatural since the reference is within the defining section.

### 1.3 The Goal

Unqualified name references should resolve to the **nearest enclosing section's binding
first**, only requiring qualification when accessing names from other sections. This
matches both legal convention and programmer expectations from lexical scoping.

**Desired behavior for the example above:**

```l4
§ consulting
ASSUME `base salary` IS A NUMBER

DECIDE `total compensation` IS A NUMBER
DECIDE `total compensation` MEANS `base salary` * 1.1
-- OK: resolves to consulting.`base salary` (same section, no ambiguity)
```

---

## 2. Current Implementation

### 2.1 How Sections Work Today

The type checker processes sections in a three-phase pipeline:

1. **Phase 1 (Type declarations):** `scanTyDeclSection` scans DECLARE and ASSUME type-level
   declarations. Notably, this phase does NOT call `pushSection` -- it does not track the
   section stack at all.

2. **Phase 2 (Function signatures):** `scanFunSigSection` scans DECIDE and term-level ASSUME
   signatures. This phase DOES call `pushSection` to track the section stack, and calls
   `withQualified` to register both unqualified and qualified name variants.

3. **Phase 3 (Inference):** `inferSection` infers the body of each declaration. It does NOT
   call `pushSection` either.

### 2.2 How Names Are Registered

The function `withQualified` (TypeCheck.hs, line 2045) is responsible for generating
qualified name variants. Given a definition `foo` inside `§ a AKA x`:

```haskell
withQualified :: [Resolved] -> CheckEntity -> Check CheckInfo
withQualified rs ce = do
  sects <- asks (.sectionStack)
  -- generates: ["foo", "a.foo", "x.foo"]
  -- all share the same Unique, pointing to the same entity
```

All variants are added to the flat `Environment` map (`Map RawName [Unique]`). For nested
sections `§ a` > `§§ b`, it generates the Cartesian product of all section name variants.

### 2.3 How Name Resolution Works

When the type checker encounters an unqualified name reference, it calls
`lookupRawNameInEnvironment` which looks up the `RawName` in the flat `Environment` map.
If multiple `Unique`s are found (because the same `RawName` is defined in multiple
sections), it feeds them all into `resolveTerm'` (or `resolveType`), which attempts
type-directed disambiguation via `choose`. If type information cannot distinguish
the candidates, an `AmbiguousTermError` is raised.

**The core issue:** The `Environment` is a flat map with no notion of "current section."
All names from all sections are equally visible, with no preference for the local binding.

### 2.4 How Qualified Name Resolution Works

Qualified names (e.g., `a.foo`) are resolved in the `Proj` case of `inferExpr`. The
expression `a's foo` is parsed as `Proj ann (Var _ a) foo`. The function
`extractProjNameChain` converts this to a `QualifiedName` which is then looked up in the
environment. This path works correctly and is unaffected by this proposal.

---

## 3. Proposed Behavior

### 3.1 Name Resolution Priority

When resolving an unqualified name `x`, the type checker should prefer candidates in
the following priority order:

1. **Same section:** Definitions in the current section (innermost enclosing `§`)
2. **Parent sections:** Definitions in enclosing parent sections, from innermost to outermost
3. **Top level:** Definitions at the module top level (outside any `§`)
4. **Ambiguity error:** If multiple candidates remain at the same priority level after
   filtering, report ambiguity as before

This is standard lexical scoping with shadowing.

### 3.2 Fully Qualified Access Unchanged

Fully qualified names (`a.foo`, `a.b.foo`) bypass this priority system entirely.
They resolve directly via `QualifiedName` lookup and continue to work exactly as today.

### 3.3 Examples

#### 3.3.1 Basic Shadowing

```l4
§ a
ASSUME x IS A BOOLEAN

§ b
ASSUME x IS A NUMBER

DECIDE y IS A NUMBER
DECIDE y MEANS x + 1
-- Resolves: x -> b's x (NUMBER) -- same section preferred
```

#### 3.3.2 Nested Section Access to Parent

```l4
§ a
ASSUME x IS A BOOLEAN

§§ aa
ASSUME y IS A BOOLEAN
DECIDE z IS A BOOLEAN
DECIDE z MEANS x AND y
-- Resolves: x -> a's x (inherited from parent section)
-- Resolves: y -> aa's y (same section)
```

#### 3.3.3 Nested Section Shadowing

```l4
§ a
ASSUME x IS A BOOLEAN

§§ aa
ASSUME x IS A NUMBER
DECIDE y IS A NUMBER
DECIDE y MEANS x + 1
-- Resolves: x -> aa's x (NUMBER) -- inner shadows outer
-- To access outer: a.x
```

#### 3.3.4 Cross-Section Access Still Requires Qualification

```l4
§ a
ASSUME x IS A BOOLEAN

§ b
DECIDE y IS A BOOLEAN
DECIDE y MEANS a.x
-- Must qualify: a.x (§ a is not a parent of § b, it is a sibling)
```

#### 3.3.5 AKA Aliases Follow Section Membership

```l4
§ a AKA alpha
ASSUME x IS A BOOLEAN

§ b AKA beta
ASSUME x IS A NUMBER

DECIDE y IS A NUMBER
DECIDE y MEANS x + 1
-- Resolves: x -> b's x (same section, NUMBER)
-- Also accessible as: b.x, beta.x, a.x, alpha.x
```

#### 3.3.6 Ambiguity Within Same Section (Error, Unchanged)

```l4
§ a
§§ b
ASSUME x IS A BOOLEAN
§§ c
ASSUME x IS A NUMBER

-- If reference to x appears directly inside § a (not inside §§ b or §§ c):
-- BOTH b.x and c.x are children of § a at the same depth
-- This remains an ambiguity error (must qualify as b.x or c.x)
```

---

## 4. Implementation Approach

### 4.1 Core Idea: Annotate Environment Entries with Section Depth

The simplest approach that requires minimal structural change: annotate each `Unique` in
the `Environment` with its section path (the `sectionStack` at the time of registration).
Then, at resolution time, compare the current `sectionStack` against each candidate's
section path to compute a priority score.

### 4.2 Detailed Changes

#### 4.2.1 Extend `Environment` Type (TypeCheck/Types.hs)

**Option A -- Annotated Environment (recommended):**

Add section path information to the environment entries:

```haskell
-- Current:
type Environment = Map RawName [Unique]

-- Proposed:
type Environment = Map RawName [EnvironmentEntry]

data EnvironmentEntry = MkEnvironmentEntry
  { unique      :: !Unique
  , sectionPath :: ![NonEmpty Text]
    -- ^ The sectionStack at the point this name was registered.
    --   Empty list means top-level (no enclosing section).
  }
```

**Option B -- Resolve-time filtering (simpler, less invasive):**

Keep the `Environment` type unchanged. Instead, modify `resolveTerm'` and `resolveType`
to filter/rank candidates using the `sectionStack` from `CheckEnv`. This requires
looking up each candidate's defining section from `EntityInfo` or from a side map.

**Recommendation:** Option A is cleaner because it co-locates the information where it is
needed, but Option B can be implemented as a first step with zero type changes.

#### 4.2.2 Record Section Path at Registration Time

In `withQualified` (TypeCheck.hs, ~line 2045), the `sectionStack` is already available
via `asks (.sectionStack)`. Currently this information is used only to generate qualified
name variants. We additionally need to store it alongside the `Unique` in the environment.

If using Option A, modify `extendEnv` (TypeCheck/Types.hs, ~line 835) to accept and store
the section path.

If using Option B, introduce a new map field in `CheckEnv`:

```haskell
data CheckEnv = MkCheckEnv
  { ...
  , sectionPathOf :: !(Map Unique [NonEmpty Text])
    -- ^ Section path at which each Unique was defined
  }
```

Populate this in `withQualified`.

#### 4.2.3 Prefer Local Bindings at Resolution Time

Modify `resolveTerm'` (TypeCheck.hs, ~line 553) and `resolveType` (~line 888) to rank
candidates before choosing. The ranking function:

```haskell
-- | Compute how "close" a candidate's section path is to the current section.
-- Returns Nothing if the candidate is not in the current section's ancestry.
-- Returns Just 0 for same section, Just 1 for parent, Just 2 for grandparent, etc.
sectionProximity :: [NonEmpty Text] -> [NonEmpty Text] -> Maybe Int
sectionProximity currentPath candidatePath
  | candidatePath `isPrefixOf` currentPath = Just (length currentPath - length candidatePath)
  | otherwise = Nothing
```

The resolution logic becomes:

```haskell
resolveTerm' p n = do
  options <- lookupRawNameInEnvironment (rawName n)
  currentSections <- asks (.sectionStack)
  let candidates = mapMaybe (proc currentSections) options
  case candidates of
    [] -> outOfScope ...
    _  -> do
      -- Group by proximity, prefer closest
      let grouped = groupAndSortByProximity candidates
      case grouped of
        [(_, [single])] -> single          -- unique closest match
        ((_, closest):_)
          | length closest == 1 -> head closest  -- single closest
          | otherwise -> choose closest <|> ambiguousTerm ...
        _ -> ... -- shouldn't happen
```

#### 4.2.4 Propagate Section Stack to All Three Phases

Currently, only `scanFunSigSection` calls `pushSection`. For consistency, both
`scanTyDeclSection` and `inferSection` should also push the section stack. This
ensures that:

- Type declarations (DECLARE) inside sections get correct section paths
- Expression inference knows the current section for resolution priority

**Files to modify:**
- `scanTyDeclSection` (~line 2138): Add `pushSection` call, mirroring `scanFunSigSection`
- `inferSection` (~line 437): Add `pushSection` call

This is also arguably a latent bug fix: currently, DECLARE names inside sections do not
receive qualified name variants because `scanTyDeclSection` skips `pushSection`.

### 4.3 File Summary

| File | Changes |
|------|---------|
| `jl4-core/src/L4/TypeCheck/Types.hs` | Add section path to `Environment` or `CheckEnv` |
| `jl4-core/src/L4/TypeCheck.hs` | Modify `resolveTerm'`, `resolveType` to prefer local; add `pushSection` to `scanTyDeclSection` and `inferSection`; modify `withQualified`/`extendEnv` to store section path |

---

## 5. Edge Cases and Design Decisions

### 5.1 Sibling Sections at Same Depth

```l4
§ a
  §§ b
  ASSUME x IS A BOOLEAN
  §§ c
  ASSUME x IS A NUMBER

-- Reference to x from directly inside § a (not inside §§ b or §§ c):
-- Both are children at depth 1 relative to § a
-- Neither is "closer" than the other
-- Result: AmbiguousTermError (same as today, but only after filtering out non-ancestors)
```

**Decision:** Sibling sections do not shadow each other. Ambiguity among candidates at the
same depth is an error, as it is today.

### 5.2 Top-Level Definitions vs Section Definitions

```l4
ASSUME x IS A BOOLEAN    -- top-level

§ a
ASSUME x IS A NUMBER     -- inside section

DECIDE y IS A NUMBER
DECIDE y MEANS x + 1
-- Inside § a: resolves to a's x (NUMBER, closer)
-- Outside § a: resolves to top-level x (BOOLEAN, the only candidate in scope)
```

**Decision:** Top-level definitions have the lowest priority (section path = `[]`), so
any section-level definition shadows them within that section.

### 5.3 Type-Directed Resolution Interaction

Currently, type-directed resolution via `choose` can disambiguate between candidates with
different types. The section proximity filter should be applied **before** type-directed
resolution, as a first pass. Rationale: section scoping is a lexical property that should
take precedence over type inference. If two candidates are at the same proximity, then
type-directed resolution can still disambiguate among them.

**Decision:** Section proximity filtering happens first; type-directed disambiguation is
the fallback within the same proximity level.

### 5.4 AKA Aliases

AKA aliases on sections create additional qualified name paths but do not change the
section hierarchy. A definition inside `§ a AKA alpha` has section path `[a :| [alpha]]`.
The proximity computation uses the section path structure, not the individual alias names,
so AKA does not create ambiguity.

The `withQualified` function already generates all qualified name variants (using the
Cartesian product of section name aliases). This is unaffected. The change is only to
the unqualified resolution path.

### 5.5 Imports and Cross-Module Sections

Imported names come from other modules and have their own section paths (relative to
their defining module). For cross-module resolution:

**Decision:** Imported names are treated as having an "external" section path that never
matches the current module's section ancestry. They can only be accessed via their
qualified names or via unqualified names that happen to be unique. This preserves the
current behavior for imports.

### 5.6 Forward References

L4 currently allows forward references within a module (the three-phase pipeline scans
all declarations before inferring bodies). Lexical scoping does not change this: the
proximity filter is based on the section structure, not the textual order of declarations
within a section.

### 5.7 Recursive Definitions

A function can reference itself by its unqualified name. Since it is defined in the same
section as where it is used, it will always be the closest candidate. No change needed.

---

## 6. Backward Compatibility

### 6.1 What Could Break

Existing code that relies on the current flat scoping could theoretically break if:

1. A program has the same name defined in two sections, AND
2. A reference to that name appears inside one of those sections, AND
3. The reference currently resolves successfully via type-directed disambiguation to the
   *other* section's binding (not the local one)

This scenario is extremely unlikely in practice. It would mean the programmer intentionally
uses a foreign section's binding via type inference while ignoring the identically-named
local binding. If this ever occurs, it would be confusing code that arguably should be
rewritten with explicit qualification.

### 6.2 What Will Not Break

- **Fully qualified references:** All `a.b.x` style references are unaffected.
- **Unambiguous unqualified references:** Names that exist in only one section continue
  to resolve identically.
- **Existing test cases:** The `nested-sections.l4` test defines `a1` in both `§ a` and
  `§ b` but only uses fully qualified references (`a.a1`, `b.a1`). The `#CHECK a3` on
  line 9 (inside `§§§ aaa`) works because `a3` is defined only once. These all continue
  to work.
- **Programs without sections:** Completely unaffected.
- **Programs with sections but no name collisions:** Completely unaffected.

### 6.3 Migration Path

No migration is needed for existing code. The change makes previously-invalid code
(unqualified references that triggered ambiguity errors) become valid. It does not
change the meaning of currently-valid code in any practical scenario (see 6.1).

### 6.4 Golden Test Updates

The existing `nested-sections.golden` should remain unchanged since it uses fully qualified
references. New test cases should be added (see Section 7).

---

## 7. Test Plan

### 7.1 New Test File: `section-lexical-scoping.l4`

```l4
-- Test: section lexical scoping with shadowed names

§ employment
ASSUME `base salary` IS A NUMBER
ASSUME bonus IS A NUMBER

§§ `full time`
ASSUME bonus IS A BOOLEAN  -- shadows employment.bonus within this subsection

-- Within §§ `full time`, unqualified bonus should resolve to BOOLEAN
#CHECK bonus
-- Should be: BOOLEAN

-- Access parent section's `base salary` without qualification
#CHECK `base salary`
-- Should be: NUMBER

§ consulting
ASSUME `base salary` IS A NUMBER
ASSUME `hourly rate` IS A NUMBER

-- Within § consulting, unqualified `base salary` resolves to consulting's definition
#CHECK `base salary`
-- Should be: NUMBER

-- Cross-section access requires qualification
#CHECK employment.`base salary`
-- Should be: NUMBER

-- Top-level after all sections
-- (These require qualified access since we're outside both sections)
#CHECK employment.`base salary`
#CHECK consulting.`hourly rate`
```

### 7.2 New Test File: `section-scoping-not-ok.l4`

```l4
-- Test: ambiguity errors that should still occur

§ a
§§ b
ASSUME x IS A BOOLEAN
§§ c
ASSUME x IS A NUMBER

-- Reference from § a (parent of both §§ b and §§ c):
-- both are equidistant children, should still be ambiguous
-- #CHECK x  -- should produce AmbiguousTermError
```

### 7.3 Existing Test Preservation

Run `cabal test all` and verify that `nested-sections.golden` and all other golden tests
pass without modification.

---

## 8. Open Questions

1. **Should section scoping apply to types as well as terms?** The current proposal
   modifies both `resolveTerm'` and `resolveType`. Types defined in sections (via DECLARE)
   should follow the same lexical scoping rules. Confirm this is desired.

2. **Should `scanTyDeclSection` also push sections?** Currently it does not, which means
   DECLARE names inside sections may not get qualified name variants. This appears to be a
   pre-existing bug. Fixing it as part of this work is recommended but may have separate
   test implications.

3. **Performance:** The proximity computation adds a comparison of section path lists at
   each name resolution. For typical programs with shallow nesting (2-3 levels), this is
   negligible. For pathological cases with deeply nested sections, the overhead is bounded
   by O(depth * candidates). Confirm this is acceptable.

4. **IDE impact:** The LSP server uses the same type checker. Section-aware resolution
   will automatically propagate to IDE features (go-to-definition, hover, autocomplete).
   Verify that the `InfoMap` and `ScopeMap` correctly reflect the resolved bindings.

5. **Error message quality:** When a name is resolved via section proximity (shadowing an
   outer definition), should the compiler emit a hint? E.g., "Note: `x` resolves to
   consulting.x; employment.x is shadowed." This is useful but not required for v1.

---

## 9. Audit-Grade Traceability (Desugaring Step)

To maintain L4's promise of audit-grade traceability, section-scoped name resolution
should be implemented as an explicit **desugaring step** rather than as implicit behavior
buried in the type checker. The compiler should:

1. **Rewrite** unqualified references to fully qualified names during a desugaring pass,
   using the lexical scoping rules above to determine which binding was chosen.

2. **Log the resolution decision** in the desugared AST or in a side-channel trace,
   recording: the original unqualified name, the resolved qualified name, the section
   path that was searched, and the reason (e.g., "resolved to `Subsection 2`.`age of majority`
   because it is the nearest enclosing section binding").

3. **Expose the trace** to downstream consumers (the evaluation trace, the NLG explainer,
   the IDE hover info) so that a human auditor can see at a glance which binding was
   chosen and why, without having to mentally reconstruct the scoping rules.

This approach means the evaluator never sees unqualified ambiguous names — by the time
evaluation runs, every name has been explicitly qualified. The desugaring log serves as
an audit trail, analogous to how a legal opinion letter would note "the term 'age of
majority' as defined in Subsection 2 of Part VII."

**Implementation note:** This could be integrated into the existing `L4/Desugar.hs`
pipeline, which already handles mixfix resolution and other syntactic transformations.
Add a `ResolveSectionScope` pass that runs after the current desugaring steps.

---

## 10. Summary

| Aspect | Current | Proposed |
|--------|---------|----------|
| Same-section reference | Ambiguous if name exists elsewhere | Resolves to local binding |
| Parent-section reference | Ambiguous if name exists elsewhere | Resolves to nearest ancestor |
| Cross-section reference | Must qualify | Must qualify (unchanged) |
| Fully qualified reference | Works | Works (unchanged) |
| No-collision reference | Works | Works (unchanged) |
| Backward compatibility | N/A | No breakage for well-written code |

The change brings L4's section scoping in line with both legal drafting convention and
programming language best practices, making unqualified names resolve to the nearest
lexical binding.

---

## 11. References

- Lexical scoping: standard in most modern programming languages (Scheme, Haskell, Python,
  JavaScript, Rust)
- Legal drafting: "For purposes of this subsection" scoping convention
- L4 existing implementation: `jl4-core/src/L4/TypeCheck.hs` (`withQualified`,
  `resolveTerm'`, `resolveType`, `pushSection`), `jl4-core/src/L4/TypeCheck/Types.hs`
  (`CheckEnv`, `Environment`, `sectionStack`)
- Existing test: `jl4/examples/ok/nested-sections.l4`
