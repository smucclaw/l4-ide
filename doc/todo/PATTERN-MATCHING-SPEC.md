# Specification: Pattern Matching in Function Definitions

**Status:** 📋 Draft  
**Issue:** TBD

## Executive Summary

Haskell's pattern matching in function definitions elegantly expresses what would otherwise require decision tables or nested conditionals. L4 already allows parameters to be omitted from `DECIDE`/`MEANS` lines (inferred from `GIVEN`). We propose extending this: when parameters ARE specified, they can be **literal patterns** that get matched directly, enabling multiple function clauses.

This brings the elegance of:

```haskell
factorial 0 = 1
factorial n = n * factorial (n - 1)
```

to L4:

```l4
GIVEN n IS A NUMBER
GIVETH A NUMBER
DECIDE factorial 0 IS 1
DECIDE factorial n IS n * factorial (n - 1)
```

## Motivation

### Current State: CONSIDER/WHEN

L4 currently handles case analysis via `CONSIDER`/`WHEN`:

```l4
GIVEN n IS A NUMBER
GIVETH A NUMBER
DECIDE factorial IS
  IF n EQUALS 0
  THEN 1
  ELSE n * factorial (n - 1)
```

Or for algebraic data types (from prelude.l4):

```l4
GIVEN list IS A LIST OF a
GIVETH A BOOLEAN
null list MEANS
  CONSIDER list
  WHEN EMPTY            THEN TRUE
  WHEN x FOLLOWED BY xs THEN FALSE
```

### The Problem

1. **Verbosity**: `CONSIDER`/`WHEN` adds syntactic overhead for simple cases
2. **Separation**: The pattern and the result are distant from each other
3. **Decision Tables**: Legal rules often look like decision tables, which map naturally to pattern matching
4. **Familiarity**: Haskell-inspired users expect pattern matching at the function level

### The Opportunity

L4 already has this syntax for functions:

```l4
GIVEN x IS A NUMBER
GIVETH A NUMBER
DECIDE double IS x * 2      -- parameter 'x' inferred from GIVEN
```

And this equivalent form:

```l4
GIVEN x IS A NUMBER
GIVETH A NUMBER
DECIDE double x IS x * 2    -- parameter 'x' explicit
```

**Key insight**: If explicit parameters can be _variables_, why not _patterns_?

## Proposed Syntax

### Basic Pattern Matching

Multiple `DECIDE` clauses with the same function name, distinguished by patterns:

```l4
GIVEN n IS A NUMBER
GIVETH A NUMBER
DECIDE factorial 0 IS 1
DECIDE factorial n IS n * factorial (n - 1)
```

The clauses are tried **top-to-bottom**, first match wins (like Haskell).

### Pattern Types

#### 1. Literal Patterns

Match exact values:

```l4
GIVEN day IS A STRING
GIVETH A BOOLEAN
DECIDE `is weekend` "Saturday" IS TRUE
DECIDE `is weekend` "Sunday"   IS TRUE
DECIDE `is weekend` day        IS FALSE
```

#### 2. Constructor Patterns

Match algebraic data type constructors:

```l4
GIVEN list IS A LIST OF a
GIVETH A NUMBER
DECIDE length EMPTY IS 0
DECIDE length (x FOLLOWED BY xs) IS 1 + length xs
```

#### 3. Wildcard Patterns

Use `_` or any name starting with `_` to match anything without binding:

```l4
GIVEN x IS A NUMBER
      y IS A NUMBER
GIVETH A NUMBER
DECIDE first x _  IS x      -- anonymous wildcard
DECIDE first x _y IS x      -- named wildcard (documents what's ignored)
```

**Named wildcards** (`_foo`) are useful for:

- Documenting what the ignored parameter represents
- Suppressing "unused variable" warnings if you do bind it
- Self-documenting decision tables

```l4
GIVEN customer_type IS A CustomerType
      order_total IS A NUMBER
      is_member IS A BOOLEAN
GIVETH A NUMBER

-- Named wildcards document what's being ignored
DECIDE discount Premium    _total  _member IS 20
DECIDE discount Regular    total   TRUE    IS 10 WHERE total > 100
DECIDE discount Regular    _total  TRUE    IS 5
DECIDE discount _type      _total  _member IS 0
```

#### 4. Nested Patterns

Patterns can be nested:

```l4
GIVEN x IS A MAYBE (MAYBE NUMBER)
GIVETH A NUMBER
DECIDE `deep unwrap` NOTHING         IS 0
DECIDE `deep unwrap` (JUST NOTHING)  IS 0
DECIDE `deep unwrap` (JUST (JUST n)) IS n
```

#### 5. Guard Patterns (Future Extension)

For when simple matching isn't enough:

```l4
GIVEN n IS A NUMBER
GIVETH A TEXT
DECIDE classify n IS "negative" WHEN n < 0
DECIDE classify n IS "zero"     WHEN n EQUALS 0
DECIDE classify n IS "positive" WHEN n > 0
```

Or with `WHERE` guards:

```l4
GIVEN n IS A NUMBER
GIVETH A TEXT
DECIDE classify n IS "negative" WHERE n < 0
DECIDE classify n IS "zero"     WHERE n EQUALS 0
DECIDE classify n IS "positive" WHERE n > 0
```

### Relationship to Decision Tables

Pattern matching is isomorphic to decision tables. Consider:

| is_public_holiday | day_of_week | parking_cost |
| ----------------- | ----------- | ------------ |
| TRUE              | \_          | 0            |
| FALSE             | "Saturday"  | 5            |
| FALSE             | "Sunday"    | 5            |
| FALSE             | \_          | 10           |

This becomes:

```l4
GIVEN is_public_holiday IS A BOOLEAN
      day_of_week IS A STRING
GIVETH A NUMBER
DECIDE `parking cost` TRUE  _          IS 0
DECIDE `parking cost` FALSE "Saturday" IS 5
DECIDE `parking cost` FALSE "Sunday"   IS 5
DECIDE `parking cost` FALSE _          IS 10
```

### Alternative Syntax: Tabular Form

For decision-table-heavy use cases, we could support a tabular syntax:

```l4
GIVEN is_public_holiday IS A BOOLEAN
      day_of_week IS A STRING
GIVETH A NUMBER
DECIDE `parking cost`
  | TRUE  | _          | 0  |
  | FALSE | "Saturday" | 5  |
  | FALSE | "Sunday"   | 5  |
  | FALSE | _          | 10 |
```

This is sugar for the multi-clause form above.

## Detailed Design

### Parameter-GIVEN Correspondence (Critical)

**Rule:** Pattern parameters must correspond positionally to `GIVEN` parameters, and variable patterns should use the same names as their corresponding `GIVEN` names.

This prevents confusing scope issues:

```l4
-- GOOD: parameter name matches GIVEN name
GIVEN a IS A NUMBER
GIVETH A NUMBER
DECIDE double a IS a * 2

-- BAD: parameter name differs from GIVEN name
GIVEN a IS A NUMBER
GIVETH A NUMBER
DECIDE double b IS a * 2   -- Error! 'b' doesn't match GIVEN 'a'
                           -- And 'a' is not bound by the pattern
```

**Why this matters:**

1. **Clarity**: Readers know exactly which GIVEN each pattern position corresponds to
2. **No accidental capture**: Can't accidentally reference a GIVEN that wasn't bound
3. **Consistency**: Same name in GIVEN and DECIDE reinforces the connection

**What's allowed in pattern position:**

| Pattern               | Meaning                            | Example                                     |
| --------------------- | ---------------------------------- | ------------------------------------------- |
| Same name as GIVEN    | Binds that parameter               | `DECIDE f x IS x + 1` (where GIVEN has `x`) |
| Literal               | Matches that value                 | `DECIDE f 0 IS 1`                           |
| Constructor           | Matches and destructures           | `DECIDE f (JUST x) IS x`                    |
| Wildcard `_`          | Matches anything, no binding       | `DECIDE f _ IS 0`                           |
| Named wildcard `_foo` | Matches anything, documents intent | `DECIDE f _unused IS 0`                     |

**What's NOT allowed:**

```l4
GIVEN x IS A NUMBER
      y IS A NUMBER
GIVETH A NUMBER

-- Error: 'a' and 'b' don't match GIVEN names 'x' and 'y'
DECIDE f a b IS a + b

-- Error: Can't reference 'x' when pattern uses different name
DECIDE f a _ IS x + a

-- OK: Use GIVEN names or wildcards
DECIDE f x y IS x + y
DECIDE f x _ IS x
DECIDE f _ y IS y
DECIDE f _ _ IS 0
```

**Multiple clauses with pattern matching:**

```l4
GIVEN n IS A NUMBER
GIVETH A NUMBER
DECIDE factorial 0 IS 1           -- '0' is literal pattern for 'n'
DECIDE factorial n IS n * ...     -- 'n' binds (matches GIVEN name)
```

### Scope Resolution

When a pattern contains a variable name:

- The name **must match** the corresponding `GIVEN` parameter name
- In **pattern position** (left of `IS`), it **binds** that parameter
- In **expression position** (right of `IS`), it **references** the binding

```l4
GIVEN n IS A NUMBER
GIVETH A NUMBER
DECIDE factorial 0 IS 1           -- '0' is pattern literal
DECIDE factorial n IS n * ...     -- first 'n' binds, second 'n' references
```

### Exhaustiveness Checking

The compiler should warn when patterns are non-exhaustive:

```l4
GIVEN b IS A BOOLEAN
GIVETH A NUMBER
DECIDE f TRUE IS 1
-- Warning: Pattern match is non-exhaustive. Missing: FALSE
```

And when patterns are redundant:

```l4
GIVEN n IS A NUMBER
GIVETH A NUMBER
DECIDE f _ IS 0
DECIDE f 1 IS 1   -- Warning: Redundant pattern (unreachable)
```

### Overlap Detection

When patterns overlap, later clauses may be unreachable:

```l4
GIVEN x IS A NUMBER
      y IS A NUMBER
GIVETH A NUMBER
DECIDE f x y IS x + y
DECIDE f 0 0 IS 0     -- Warning: Redundant (first clause matches everything)
```

### Type Checking

Patterns must be compatible with the `GIVEN` types:

```l4
GIVEN n IS A NUMBER
GIVETH A NUMBER
DECIDE f "hello" IS 0  -- Error: Cannot match TEXT pattern against NUMBER
```

### Order Sensitivity

Clauses are matched top-to-bottom. This affects semantics:

```l4
-- Version 1: Specific first (correct)
DECIDE f 0 IS "zero"
DECIDE f n IS "other"

-- Version 2: General first (0 never matched!)
DECIDE f n IS "other"
DECIDE f 0 IS "zero"   -- Warning: Unreachable pattern
```

## Implementation Approaches

### Approach 1: Desugar to CONSIDER/WHEN (Recommended for MVP)

Transform pattern-matching functions into the existing `CONSIDER`/`WHEN` form during parsing or an early compiler pass.

**Transform:**

```l4
DECIDE factorial 0 IS 1
DECIDE factorial n IS n * factorial (n - 1)
```

**Into:**

```l4
factorial n MEANS
  CONSIDER n
  WHEN 0 THEN 1
  OTHERWISE n * factorial (n - 1)
```

For multiple parameters:

```l4
DECIDE f 0 y IS y
DECIDE f x 0 IS x
DECIDE f x y IS x + y
```

**Into:**

```l4
f x y MEANS
  CONSIDER x
  WHEN 0 THEN y
  OTHERWISE
    CONSIDER y
    WHEN 0 THEN x
    OTHERWISE x + y
```

**Pros:**

- Minimal changes to evaluator
- Reuses existing pattern-matching infrastructure
- Type checking already works

**Cons:**

- Nested `CONSIDER` can be inefficient (decision tree vs. decision list)
- Complex patterns may generate large intermediate code

**Estimated effort:** ~300 LOC in parser, ~200 LOC in desugaring pass

### Approach 2: Pattern Compilation to Decision Trees

Compile patterns directly to an efficient decision tree (like GHC does).

The algorithm:

1. Build a **pattern matrix** from all clauses
2. Choose a **column** to examine (heuristics matter)
3. **Split** the matrix based on constructors in that column
4. Recurse until all patterns are simple variable/wildcard bindings
5. Generate code that examines one variable at a time

**Example pattern matrix:**

```
| n | Result           |
|---|------------------|
| 0 | 1                |
| n | n * factorial... |
```

**Decision tree:**

```
examine n:
  case 0 → return 1
  case _ → let n = <input>; return n * factorial (n - 1)
```

**Pros:**

- Optimal runtime performance
- Natural exhaustiveness/redundancy checking
- Industry-standard approach

**Cons:**

- More complex implementation (~800 LOC)
- Need to integrate with existing evaluator
- Decision tree representation needed in AST or IR

**Estimated effort:** ~800 LOC for pattern compiler, ~200 LOC AST changes

### Approach 3: Multi-Clause Function as First-Class AST Node

Add a new AST node type for multi-clause functions:

```haskell
data Decide n =
    MkDecide Anno (TypeSig n) (AppForm n) (Expr n)
  | MkDecideMulti Anno (TypeSig n) n [Clause n]  -- NEW

data Clause n =
  MkClause Anno [Pattern n] (Maybe (Guard n)) (Expr n)

data Pattern n =
    PatVar n                           -- x
  | PatWild                            -- _
  | PatLit Literal                     -- 0, "hello", TRUE
  | PatCon n [Pattern n]               -- JUST x, x FOLLOWED BY xs
  | PatAs n (Pattern n)                -- x@(JUST y)
```

**Pros:**

- Clean separation of concerns
- Pattern matching is explicit in AST
- Enables IDE features (jump to clause, etc.)

**Cons:**

- More invasive AST changes
- All downstream passes must handle new node type
- Type checker needs pattern-specific logic

**Estimated effort:** ~500 LOC AST changes, ~600 LOC type checker, ~400 LOC evaluator

### Approach 4: Hybrid - AST Node + Desugar

Keep the new AST node but desugar to `CONSIDER`/`WHEN` before evaluation:

1. Parse into `MkDecideMulti`
2. Type check patterns directly (good errors)
3. Desugar to `MkDecide` with `CONSIDER`/`WHEN` before eval
4. Evaluate using existing machinery

**Pros:**

- Best error messages (knows about patterns)
- Minimal evaluator changes
- IDE can show patterns

**Cons:**

- Two representations to maintain
- Desugaring pass needed

**Estimated effort:** ~400 LOC AST, ~400 LOC type checker, ~300 LOC desugaring

## Recommended Implementation Path

### Phase 1: MVP via Desugaring (Approach 1)

1. **Parser changes** (~200 LOC):

   - Recognize multiple `DECIDE` clauses with same name
   - Parse literal/constructor patterns in parameter position
   - Group clauses together

2. **Desugaring pass** (~300 LOC):

   - Transform multi-clause functions to single clause with `CONSIDER`
   - Handle nested patterns via nested `CONSIDER`
   - Preserve source locations for error messages

3. **Warnings** (~100 LOC):
   - Basic exhaustiveness check (are all constructors covered?)
   - Basic redundancy check (is any clause unreachable?)

**Total:** ~600 LOC, ~3-5 days

### Phase 2: AST Representation (Approach 4)

1. Add `Pattern` type to AST
2. Add `MkDecideMulti` constructor
3. Type check patterns with good error messages
4. Desugar before evaluation

**Total:** ~800 LOC additional, ~5-7 days

### Phase 3: Decision Tree Compilation (Future)

1. Implement pattern matrix
2. Implement column selection heuristics
3. Generate decision tree IR
4. Evaluate decision trees directly

**Total:** ~1000 LOC additional, ~7-10 days

## Syntax Ambiguity Analysis

### Potential Ambiguity: Variable vs. Pattern

```l4
GIVEN x IS A NUMBER
GIVETH A NUMBER
DECIDE f x IS x * 2
```

Is `x` a pattern variable (binding) or a reference to the `GIVEN` parameter?

**Resolution:** In pattern position (before `IS`), names are always **bindings**. The fact that it shadows the `GIVEN` name is intentional - that's how the binding happens.

### Potential Ambiguity: Function Call vs. Constructor Pattern

```l4
DECIDE f (JUST x) IS ...   -- JUST is constructor pattern
DECIDE f (g x) IS ...      -- Is this constructor or function call?
```

**Resolution:** Only **declared type constructors** are valid in patterns. Function calls are not patterns. The type checker validates this.

### Potential Ambiguity: Numeric Literals

```l4
DECIDE f 0 IS ...      -- Match zero
DECIDE f (0) IS ...    -- Match zero (parenthesized)
DECIDE f (-1) IS ...   -- Match negative one? Or negate pattern?
```

**Resolution:** Negative numbers are literal patterns. No arithmetic in patterns.

## Interaction with Existing Features

### With TYPICALLY (Default Values)

Pattern matching composes with `TYPICALLY`:

```l4
GIVEN x IS A NUMBER TYPICALLY 0
GIVETH A TEXT
DECIDE describe 0 IS "default"
DECIDE describe n IS "custom: " ++ show n
```

If `x` is not provided, it defaults to 0, matching the first clause.

### With WHERE Clauses

Local definitions work within each clause:

```l4
GIVEN n IS A NUMBER
GIVETH A NUMBER
DECIDE fib 0 IS 0
DECIDE fib 1 IS 1
DECIDE fib n IS fib a + fib b
  WHERE a IS n - 1
        b IS n - 2
```

### With AKA (Aliases)

Aliases apply to the function, not individual clauses:

```l4
GIVEN n IS A NUMBER
GIVETH A NUMBER
DECIDE factorial 0 AKA `fact` IS 1   -- AKA on first clause defines alias
DECIDE factorial n IS n * fact (n - 1)
```

### With @export Annotations

Export applies to the function as a whole:

```l4
@export
GIVEN n IS A NUMBER
GIVETH A NUMBER
DECIDE factorial 0 IS 1
DECIDE factorial n IS n * factorial (n - 1)
```

## Examples

### Example 1: Fibonacci

**Haskell:**

```haskell
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)
```

**L4:**

```l4
GIVEN n IS A NUMBER
GIVETH A NUMBER
DECIDE fib 0 IS 0
DECIDE fib 1 IS 1
DECIDE fib n IS fib (n - 1) + fib (n - 2)
```

### Example 2: List Length

**Current L4:**

```l4
GIVEN list IS A LIST OF a
GIVETH A NUMBER
length list MEANS
  CONSIDER list
  WHEN EMPTY            THEN 0
  WHEN x FOLLOWED BY xs THEN 1 + length xs
```

**With pattern matching:**

```l4
GIVEN list IS A LIST OF a
GIVETH A NUMBER
DECIDE length EMPTY IS 0
DECIDE length (x FOLLOWED BY xs) IS 1 + length xs
```

### Example 3: Maybe Handling

**Current L4:**

```l4
GIVEN x IS A MAYBE a
GIVETH AN a
fromMaybe default x MEANS
  CONSIDER x
  WHEN NOTHING THEN default
  WHEN JUST y  THEN y
```

**With pattern matching:**

```l4
GIVEN default IS AN a
      x IS A MAYBE a
GIVETH AN a
DECIDE fromMaybe default NOTHING  IS default
DECIDE fromMaybe _       (JUST y) IS y
```

### Example 4: Legal Rule as Decision Table

**Insurance coverage rule:**

```l4
GIVEN damage_type IS A DamageType
      caused_by IS A CauseType
      is_contents IS A BOOLEAN
GIVETH A BOOLEAN

-- Covered scenarios
DECIDE `is covered` _        Water    _     IS TRUE
DECIDE `is covered` _        Fire     _     IS TRUE
DECIDE `is covered` Cosmetic _        _     IS FALSE
DECIDE `is covered` _        Rodents  TRUE  IS FALSE
DECIDE `is covered` _        Rodents  FALSE IS TRUE
DECIDE `is covered` _        Birds    TRUE  IS TRUE
DECIDE `is covered` _        Birds    FALSE IS FALSE
DECIDE `is covered` _        _        _     IS TRUE   -- Default: covered
```

### Example 5: Tax Brackets

```l4
DECLARE TaxBracket
  IS Low
  OR Medium
  OR High

GIVEN income IS A NUMBER
GIVETH A TaxBracket
DECIDE bracket n IS Low    WHERE n < 50000
DECIDE bracket n IS Medium WHERE n < 100000
DECIDE bracket n IS High
```

### Example 6: Nested Patterns

```l4
GIVEN x IS A MAYBE (EITHER NUMBER TEXT)
GIVETH A TEXT

DECIDE describe NOTHING              IS "empty"
DECIDE describe (JUST (LEFT n))      IS "number: " ++ show n
DECIDE describe (JUST (RIGHT t))     IS "text: " ++ t
```

## Testing Plan

### Parser Tests

```l4
-- Should parse: multiple clauses
GIVEN n IS A NUMBER
GIVETH A NUMBER
DECIDE f 0 IS 1
DECIDE f n IS n

-- Should parse: constructor patterns
GIVEN x IS A MAYBE NUMBER
GIVETH A NUMBER
DECIDE g NOTHING IS 0
DECIDE g (JUST n) IS n

-- Should parse: wildcard
GIVEN x IS A NUMBER
GIVETH A NUMBER
DECIDE h _ IS 0

-- Should parse: nested
GIVEN x IS A LIST OF MAYBE NUMBER
GIVETH A NUMBER
DECIDE i ((JUST n) FOLLOWED BY _) IS n
DECIDE i _ IS 0
```

### Type Checking Tests

```l4
-- Should error: pattern type mismatch
GIVEN n IS A NUMBER
GIVETH A NUMBER
DECIDE f "hello" IS 0  -- Error: TEXT pattern, NUMBER expected

-- Should error: constructor arity
GIVEN x IS A MAYBE NUMBER
GIVETH A NUMBER
DECIDE g (JUST a b) IS 0  -- Error: JUST takes 1 argument

-- Should warn: non-exhaustive
GIVEN b IS A BOOLEAN
GIVETH A NUMBER
DECIDE f TRUE IS 1  -- Warning: Missing FALSE

-- Should warn: redundant
GIVEN n IS A NUMBER
GIVETH A NUMBER
DECIDE f _ IS 0
DECIDE f 0 IS 1  -- Warning: Unreachable
```

### Evaluation Tests

```l4
-- Factorial
ASSERT factorial 0 EQUALS 1
ASSERT factorial 5 EQUALS 120

-- List operations
ASSERT length EMPTY EQUALS 0
ASSERT length (LIST 1, 2, 3) EQUALS 3

-- Maybe
ASSERT fromMaybe 0 NOTHING EQUALS 0
ASSERT fromMaybe 0 (JUST 42) EQUALS 42
```

### Golden Tests

Create golden files for:

- Desugared output of pattern-matching functions
- Error messages for type mismatches
- Warnings for non-exhaustive/redundant patterns

## Future Extensions

### As-Patterns

Bind a name to the whole while also destructuring:

```l4
DECIDE f all@(x FOLLOWED BY xs) IS ...  -- 'all' bound to whole list
```

### View Patterns

Apply a function before matching:

```l4
DECIDE f (length -> 0) IS "empty"
DECIDE f (length -> n) IS "has " ++ show n ++ " elements"
```

### Pattern Synonyms

Define reusable pattern abstractions:

```l4
PATTERN Singleton x IS x FOLLOWED BY EMPTY

DECIDE f (Singleton x) IS x
```

### Or-Patterns

Match multiple patterns with same result:

```l4
DECIDE `is weekend` ("Saturday" | "Sunday") IS TRUE
DECIDE `is weekend` _ IS FALSE
```

## References

- [Haskell Pattern Matching](https://www.haskell.org/tutorial/patterns.html)
- [GHC Pattern Match Compiler](https://www.microsoft.com/en-us/research/wp-content/uploads/1987/01/slpj-book-1987-r90.pdf) (Chapter 5)
- [ML Pattern Compilation](https://www.cs.tufts.edu/~nr/cs257/archive/luc-maranget/jun08.pdf)
- L4 `CONSIDER`/`WHEN` implementation in `L4/Parser.hs`
- L4 Prelude patterns in `jl4-core/libraries/prelude.l4`
