# Specification: Computed Fields (Methods) in DECLARE Records

## Executive Summary

Extend L4's `DECLARE ... HAS` syntax to support **computed fields** — fields whose values are defined by a `MEANS` expression rather than stored directly. This gives L4 a lightweight form of OOP-style methods without introducing a separate method concept, preserving the uniform access principle: callers use `record's field` identically for stored and computed fields.

### The Apostrophe Triple Pun

English `'s` is three-ways ambiguous:

- **Possessive** — `person's name` = "the name belonging to person"
- **Copula "is"** — `person's adult` = "the person is adult"
- **"has"** — `person's tickets` = "the person has tickets"

L4 exploits all three readings with a single accessor syntax. This means computed boolean fields don't need an `is` or `has` prefix — write `adult` not `isAdult`, `experienced` not `hasExperience`, because the `'s` accessor supplies the verb for free. The same `record's field` syntax reads naturally whether the field is a stored attribute, a boolean predicate, or a possession check.

## Motivation

### Current State

L4 records are pure data. To compute a derived property, you write a separate top-level `DECIDE` or `MEANS` function:

```l4
DECLARE Driver HAS
    `name`           IS A STRING
    `age`            IS A NUMBER
    `accident count` IS A NUMBER

-- Computed property, defined externally
GIVEN driver IS A Driver
GIVETH A BOOLEAN
DECIDE `driver is an adult` IF driver's age >= 18
```

### Problems with External Computed Properties

1. **Discoverability** — nothing on the `Driver` type tells you that `adult` exists as a derivable property. You must grep the codebase.
2. **No uniform access** — stored fields use `driver's age`, but computed properties use a different calling convention: `\`driver is an adult\`` as a standalone function. Callers must know which is which.
3. **Legal isomorphism gap** — in legislation, definitions are co-located: *"'adult' means a person who has attained the age of 18 years"* appears in the same definitions section as other attributes of "person." Splitting them across the file breaks the structural parallel to legal text.
4. **No lazy self-reference** — a computed property cannot easily reference another computed property on the same record without manual plumbing.

### Desired State

```l4
DECLARE Driver HAS
    `name`           IS A STRING
    `age`            IS A NUMBER
    `accident count` IS A NUMBER
    `adult`          IS A BOOLEAN
        MEANS `age` >= 18
    `experienced`    IS A BOOLEAN
        MEANS `years licensed` >= 2
    `years licensed` IS A NUMBER
```

Now `driver's adult` and `driver's age` use the same syntax. The caller doesn't know or care which fields are stored vs. computed.

## Syntax

### Grammar Extension

Within a `DECLARE ... HAS` block, each field may optionally have a `MEANS` clause:

```
field ::= field-name IS A type
        | field-name IS A type MEANS expr
```

The `MEANS` clause is indented under the field declaration, following standard L4 layout rules:

```l4
DECLARE TypeName HAS
    stored-field    IS A SomeType
    computed-field  IS A SomeType
        MEANS expression
```

### Scope Within MEANS

Inside a computed field's `MEANS` expression, **all sibling field names are in scope** without qualification. That is, within the MEANS clause of a field on record `T`, bare names resolve to fields of the same record:

```l4
DECLARE Policy HAS
    `vehicle value`   IS A NUMBER
    `risk multiplier` IS A NUMBER
    `base premium`    IS A NUMBER
        MEANS `vehicle value` * `risk multiplier`
```

Here `vehicle value` and `risk multiplier` inside the MEANS body refer to the enclosing record's fields, as if there were an implicit `self's` prefix.

### Nested Record Access

Computed fields can access fields on nested records using the standard `'s` syntax:

```l4
DECLARE Policy HAS
    `driver`       IS A Driver
    `eligible`     IS A BOOLEAN
        MEANS `driver`'s `adult`
          AND `driver`'s `experienced`
          AND `driver`'s `accident count` <= 3
```

### Computed Fields Depending on Other Computed Fields

This is explicitly supported. Lazy evaluation resolves the dependency order:

```l4
DECLARE Quote HAS
    `driver`         IS A Driver
    `vehicle value`  IS A NUMBER
    `risk category`  IS A `Risk Category`
        MEANS `assess risk` OF `driver`
    `base premium`   IS A NUMBER
        MEANS `premium for` `risk category`, `vehicle value`
    `affordable`     IS A BOOLEAN
        MEANS `base premium` <= 500
```

Here `affordable` depends on `base premium` (computed), which depends on `risk category` (also computed) and `vehicle value` (stored). Lazy evaluation handles this naturally.

## Semantics

### Uniform Access Principle

A computed field is indistinguishable from a stored field at the point of use. Both are accessed via `record's fieldName`. The only difference is internal: a stored field reads from the record's data, while a computed field evaluates its MEANS expression.

This follows Bertrand Meyer's Uniform Access Principle (Eiffel) and is analogous to:
- Python's `@property` decorator
- Kotlin's `val x: Int get() = ...`
- C#'s computed properties
- Scala's `def` vs `val` in class bodies

### Evaluation Model

Computed fields are **lazy thunks**, consistent with L4's existing lazy evaluator (`EvaluateLazy/Machine.hs`). A computed field's MEANS expression is evaluated at most once per record instance, on first access, and the result is memoized.

Environment construction for a computed field `f` on record `R`:
1. Bind all stored fields of `R` to their values
2. Bind all computed fields of `R` (other than `f`) to lazy thunks
3. Evaluate `f`'s MEANS expression in this environment

### Cycle Detection

Circular dependencies between computed fields are **a compile-time error**:

```l4
-- ERROR: circular dependency between `a` and `b`
DECLARE Bad HAS
    `a` IS A NUMBER MEANS `b` + 1
    `b` IS A NUMBER MEANS `a` + 1
```

The compiler builds a dependency graph of field references within MEANS clauses. If the graph contains a cycle, emit an error listing the cycle. This analysis happens during type checking.

### Construction

When constructing a record, computed fields are **omitted** from both positional (`OF`) and named (`WITH`) syntax:

```l4
DECLARE Driver HAS
    `name`           IS A STRING
    `age`            IS A NUMBER
    `years licensed` IS A NUMBER
    `accident count` IS A NUMBER
    `adult`          IS A BOOLEAN MEANS `age` >= 18
    `experienced`    IS A BOOLEAN MEANS `years licensed` >= 2

-- Only stored fields are supplied:
exampleDriver MEANS Driver WITH
    `name`           IS "Alice"
    `age`            IS 25
    `years licensed` IS 3
    `accident count` IS 0

-- Positional: only stored fields, in declaration order (skipping computed)
exampleDriver2 MEANS Driver OF "Alice", 25, 3, 0
```

Supplying a computed field in a constructor is a **compile-time error** — you cannot override a MEANS clause.

### Type Checking

A computed field's MEANS expression is checked against the field's declared type. Within the MEANS body:

1. All sibling fields are in scope with their declared types
2. The expression must have the declared field type
3. Top-level functions and other declarations are also in scope (computed fields can call external functions)

```l4
DECLARE Driver HAS
    `age` IS A NUMBER
    `adult` IS A BOOLEAN MEANS `age` >= 18
    --                         ^^^^^^^^^^^
    --  Checked: NUMBER >= NUMBER → BOOLEAN ✓
    --  Field type: BOOLEAN ✓
```

## Extended Examples

### Insurance Policy (Full Worked Example)

```l4
DECLARE `Risk Category` IS ONE OF
    `Low Risk`, `Medium Risk`, `High Risk`, `Uninsurable`

DECLARE Driver HAS
    `name`             IS A STRING
    `age`              IS A NUMBER
    `years licensed`   IS A NUMBER
    `accident count`   IS A NUMBER
    `has tickets`      IS A BOOLEAN
    `adult`            IS A BOOLEAN
        MEANS `age` >= 18
    `experienced`      IS A BOOLEAN
        MEANS `years licensed` >= 2
    `risk category`    IS A `Risk Category`
        MEANS IF `accident count` = 0
              THEN IF `has tickets` THEN `Medium Risk` ELSE `Low Risk`
              ELSE IF `accident count` <= 2 THEN `High Risk`
              ELSE `Uninsurable`
    `eligible`         IS A BOOLEAN
        MEANS `adult`
          AND `experienced`
          AND `accident count` <= 3

DECLARE Policy HAS
    `driver`           IS A Driver
    `vehicle value`    IS A NUMBER
    `base premium`     IS A MAYBE NUMBER
        MEANS CONSIDER `driver`'s `risk category`
              WHEN `Low Risk`    THEN JUST (`vehicle value` * 0.02)
              WHEN `Medium Risk` THEN JUST (`vehicle value` * 0.04)
              WHEN `High Risk`   THEN JUST (`vehicle value` * 0.08)
              WHEN `Uninsurable` THEN NOTHING

-- Usage: all access is uniform
alice MEANS Driver WITH
    `name` IS "Alice"
    `age` IS 25
    `years licensed` IS 3
    `accident count` IS 0
    `has tickets` IS FALSE

alicePolicy MEANS Policy WITH
    `driver` IS alice
    `vehicle value` IS 20000

-- These all use the same 's syntax:
#EVAL alice's `name`            -- "Alice"       (stored)
#EVAL alice's `adult`           -- TRUE           (computed)
#EVAL alice's `risk category`   -- Low Risk       (computed)
#EVAL alicePolicy's `base premium`  -- JUST 400.0 (computed, depends on computed)
```

### Legislative Definition (Legal Isomorphism)

Consider Singapore's Employment Act, which defines "employee" with several criteria:

```l4
DECLARE Employee HAS
    `name`               IS A STRING
    `date of birth`      IS A Date
    `date of employment` IS A Date
    `monthly salary`     IS A NUMBER
    `current date`       IS A Date
    `age`                IS A NUMBER
        MEANS `years between` `date of birth`, `current date`
    `years of service`   IS A NUMBER
        MEANS `years between` `date of employment`, `current date`
    `covered by Act`     IS A BOOLEAN
        MEANS `monthly salary` <= 4500
          AND `age` >= 16
    `eligible for retirement benefit` IS A BOOLEAN
        MEANS `covered by Act`
          AND `years of service` >= 5
          AND `age` >= 62
```

This mirrors the legislative structure where the definitions section co-locates all attributes and derived properties of a defined term.

### Purchase Contract (from existing purchase.l4)

The ad-hoc method pattern from `purchase.l4` (line 113: "this is an ad-hoc method") becomes a proper computed field:

```l4
DECLARE Party HAS
    `name`  IS A STRING
    `cash`  IS A Money
    `inv`   IS A LIST OF InventoryItem
    `ready` IS A BOOLEAN
        MEANS EXISTS item IN `inv`
              SUCH THAT item's `count` >= `required count`
```

## Implementation Plan

### Phase 1: Parser Extension

**File:** `jl4-core/src/L4/Parser.hs`

Extend the field parser to accept an optional `MEANS` clause after the type annotation.

Currently, `recordDecl'` calls `reqParam` which parses `name IS A type`. Extend this to:

```
name IS A type [MEANS expr]
```

The MEANS keyword is already a lexer token (`TKMeans`). The indentation-sensitivity of L4's layout parser should handle the continuation naturally — the MEANS expression must be indented further than the field name.

**Estimated scope:** ~15 lines changed in `Parser.hs`.

### Phase 2: AST Extension

**File:** `jl4-core/src/L4/Syntax.hs`

Extend `TypedName` to carry an optional MEANS expression:

```haskell
-- Current:
data TypedName n = MkTypedName Anno n (Type' n)

-- Proposed:
data TypedName n = MkTypedName Anno n (Type' n) (Maybe (Expr n))
```

The `Maybe (Expr n)` is `Nothing` for stored fields and `Just expr` for computed fields.

**Estimated scope:** ~5 lines in `Syntax.hs`, plus mechanical updates wherever `MkTypedName` is pattern-matched or constructed (grep for `MkTypedName`).

### Phase 3: Type Checker Extension

**File:** `jl4-core/src/L4/TypeCheck.hs`

When checking a `RecordDecl`:

1. Collect all field names and types
2. For each computed field, check its MEANS expression in a context where all sibling fields are bound at their declared types
3. Verify the expression type matches the declared field type
4. Build a dependency graph of computed field references; reject cycles

**Estimated scope:** ~40-60 lines added to `TypeCheck.hs`.

### Phase 4: Evaluator Extension

**File:** `jl4-core/src/L4/EvaluateLazy/Machine.hs`

Currently, record projection (`Proj`) is desugared to function application (line 289-290):

```haskell
Proj _ann e l -> ForwardExpr env (App emptyAnno l [e])
```

This means field names are already treated as functions from record → field value. For computed fields, the generated accessor function should evaluate the MEANS expression with the record's other fields in scope, rather than extracting a stored value.

Two implementation strategies:

**Strategy A (desugar early):** During desugaring, for each computed field, generate a synthetic top-level function that takes the record as input, extracts sibling fields, and evaluates the MEANS body. The accessor for a computed field then calls this function. This requires no evaluator changes — it's pure desugaring.

**Strategy B (evaluate late):** Extend the evaluator's record representation to include thunks for computed fields. On projection, if the field is computed, force the thunk. This is more principled but touches more code.

**Recommendation:** Strategy A (desugar early) is simpler and more consistent with L4's existing approach of desugaring `Proj` to function application.

**Estimated scope:** ~30-50 lines in `Desugar.hs`.

### Phase 5: Record Construction

**File:** `jl4-core/src/L4/TypeCheck.hs`, `jl4-core/src/L4/Desugar.hs`

When type-checking `OF` (positional) or `WITH` (named) record construction:

1. Partition fields into stored and computed
2. For positional (`OF`): expect arguments only for stored fields, in declaration order
3. For named (`WITH`): accept only stored field names; reject computed field names
4. Emit clear error messages: *"field 'adult' is a computed field and cannot be supplied in a constructor"*

**Estimated scope:** ~20-30 lines across TypeCheck and Desugar.

### Phase 6: Tooling

- **LSP (jl4-lsp):** Autocomplete for `record's ...` should include computed fields, perhaps with a visual indicator (e.g., italic or a `(computed)` annotation)
- **Inspector/Visualizer:** Show computed fields distinctly from stored fields in type tooltips
- **JSON Schema (jl4):** Computed fields should appear in output schemas but not input schemas

### Phase 7: Documentation and Examples

- Update `doc/reference/types/DECLARE.md`
- Add `doc/reference/types/computed-fields.l4` example file
- Add tutorial in `doc/tutorials/`
- Update `doc/reference/GLOSSARY.md`

## Design Decisions

### Why MEANS and Not a New Keyword?

`MEANS` is already L4's keyword for defining the body of a function. A computed field is semantically a zero-argument function scoped to its record — reusing `MEANS` reinforces this parallel without adding vocabulary.

### Why Not Allow Constructor Override of Computed Fields?

Allowing users to supply a value for a computed field would break the invariant that the field's value is always consistent with its MEANS definition. This could lead to contradictions:

```l4
-- If this were allowed, adult could be FALSE even though age is 25
Driver WITH `name` IS "Alice", `age` IS 25, `adult` IS FALSE  -- ERROR
```

In legal terms, you wouldn't allow someone to override a statutory definition.

### Why Lazy (Not Eager) Evaluation?

Eager evaluation of all computed fields at construction time would:
1. Force unnecessary computation (many fields may never be accessed)
2. Require a topological sort of fields at construction time
3. Break if a computed field depends on external state or functions with side effects (future concern)

Lazy evaluation is already L4's model and is the natural fit.

### Why Not Haskell-Style Typeclasses / Rust impl Blocks?

Separate method blocks (`METHODS FOR Driver`) were considered (Option B in the design discussion). They were rejected because:
1. Legal text co-locates definitions — splitting data from derived properties breaks legal isomorphism
2. Discoverability suffers — users must look in two places
3. "METHODS" is programmer jargon with no good legal equivalent
4. The inline MEANS syntax is simpler and sufficient for the use cases

If future needs require open extension (adding methods to types defined in other modules), a `METHODS FOR` block can be added later as a complementary feature without conflicting with computed fields.

## Test Plan

### Golden File Tests

Add to `jl4/ok/`:

| File | Tests |
|------|-------|
| `computed-fields-basic.l4` | Simple computed field, access via `'s` |
| `computed-fields-chain.l4` | Computed field depending on another computed field |
| `computed-fields-nested.l4` | Computed field accessing nested record fields |
| `computed-fields-external.l4` | Computed field calling top-level functions |
| `computed-fields-construction.l4` | Record construction omitting computed fields |

Add to `jl4/not-ok/`:

| File | Tests |
|------|-------|
| `computed-fields-cycle.l4` | Circular dependency between computed fields |
| `computed-fields-type-mismatch.l4` | MEANS expression type doesn't match field type |
| `computed-fields-override.l4` | Attempting to supply computed field in constructor |

### Property Tests

- For any record with computed fields, constructing and immediately projecting a computed field should yield the same result as evaluating the MEANS expression directly with the stored field values.

## Open Questions

1. **Can computed fields reference `GIVEN` parameters from an enclosing scope?** For example, if a record is constructed inside a `GIVEN`/`DECIDE` block, can a computed field see those bindings? Initial answer: no — computed fields should be self-contained, referencing only sibling fields and top-level definitions. This keeps the semantics clean and avoids capturing mutable context.

2. **Should computed fields be visible in pattern matching?** If you `CONSIDER` a record `WHEN` matching fields, should you be able to match on a computed field? Initial answer: no — pattern matching should only destructure stored data. Computed fields are projections, not structure.

3. **Serialization:** When a record with computed fields is serialized to JSON (e.g., for the decision service API), should computed fields be included in the output? Initial answer: yes, in output; no, in input schemas. This matches the "transparent to the caller" principle.

4. **Interaction with ASSUME:** If a record type is introduced via `ASSUME` (unimplemented, just a type signature), can computed fields be assumed too? This would allow specifying a contract interface where some fields are "virtual."

5. **Mutable state (future):** The `purchase.l4` example uses `UPDATE seller's inv's item's count += 60`. If `ready` is a computed field on Party, it would automatically reflect inventory changes after an UPDATE — no manual recalculation needed. This is a powerful consequence of lazy evaluation but needs careful design once mutable state is formalized.
