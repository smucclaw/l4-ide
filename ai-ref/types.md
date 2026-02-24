# L4 Type System

Type theory foundations and FP correspondence.

---

## Type System Overview

**Paradigm:** Hindley-Milner-style with algebraic data types
**Inference:** Bidirectional type checking
**Purity:** Pure functional core, DEONTIC effect type for regulative rules
**Evaluation:** Lazy (call-by-need)

---

## Primitive Types

| L4 Type | Description | Haskell | Coercion from STRING |
|---------|-------------|---------|---------------------|
| BOOLEAN | TRUE/FALSE (not True/true) | Bool | - |
| NUMBER | Numeric values | Rational/Double | - |
| STRING | Text | String/Text | - |
| DATE | Calendar dates (YYYY-MM-DD) | Day | TODATE |
| TIME | Time of day (HH:MM:SS or HH:MM) | TimeOfDay | TOTIME |
| DATETIME | Date+time+tz (ISO-8601) | UTCTime + TZ | TODATETIME |

---

## Type Constructors

| L4 | Description | Haskell | Type Theory |
|-----|-------------|---------|-------------|
| LIST OF T | Homogeneous list | [T] | μX. 1 + T × X (inductive) |
| MAYBE T | Optional value | Maybe T | 1 + T (option type) |
| T1 -> T2 | Function type | T1 -> T2 | Arrow type (→) |
| DEONTIC | Effect wrapper | Custom monad | Modal type (□, ◇) |

---

## Algebraic Data Types

### Product Types (Records)

```l4
DECLARE Person HAS
    name IS A STRING
    age IS A NUMBER
```

**Type theory:** Product type `String × Number`
**Haskell:** `data Person = Person { name :: String, age :: Number }`
**Elimination:** Field access `person's name`

### Sum Types (Variants)

```l4
DECLARE Status IS ONE OF
    Active
    | Suspended STRING
    | Closed
```

**Type theory:** Sum type `1 + String + 1`
**Haskell:** `data Status = Active | Suspended String | Closed`
**Elimination:** Pattern matching via CONSIDER

---

## Function Types

```l4
GIVEN x IS A T1, y IS A T2
GIVETH T3
f MEANS expression
```

**Type:** `T1 -> T2 -> T3` (curried)
**Application:** `f arg1 arg2`
**Partial application:** Supported implicitly

---

## Type Correspondence Table

| L4 Construct | Type | Haskell | Other Languages | Category Theory |
|--------------|------|---------|-----------------|----------------|
| `DECLARE T HAS ...` | Product | `data T = T { ... }` | struct/class/interface | Product (×) |
| `IS ONE OF` | Sum | `data T = A \| B` | enum/union/variant/sealed class | Coproduct (+) |
| `GIVEN...GIVETH` | Arrow | `a -> b` | function signature/def/fn | Exponential (B^A) |
| `MAYBE T` | Option | `Maybe T` | Optional/null/nil/None/? | T + 1 |
| `LIST OF T` | List | `[T]` | Array/List/Vec/Sequence | Free monoid |
| `CONSIDER` | Match | `case...of` | switch/case/match/when | Catamorphism |
| `BRANCH` | Guards | `\| cond = ...` | if-elseif chain/cond | - |
| `WHERE` / `LET...IN` | Binding | `where` / `let...in` | let/const/var/local | Substitution |
| `DEONTIC` | Effect | Custom monad | - (domain-specific) | Kleisli category |
| `'s` possessive | Accessor | Record field / lens | dot notation / .field | Projection |

---

## Pattern Matching Types

### List Patterns

```l4
WHEN EMPTY                    -- [] :: [T]
WHEN x FOLLOWED BY xs         -- (x:xs) :: [T]
```

### Maybe Patterns

```l4
WHEN Nothing                  -- Nothing :: Maybe T
WHEN Just x                   -- Just x :: Maybe T
```

### Constructor Patterns

```l4
WHEN ConstructorName          -- Nullary constructor
WHEN ConstructorName arg      -- Unary constructor
WHEN ConstructorName x y      -- N-ary constructor
```

---

## Type Inference Rules

### Variables

```
Γ ⊢ x : τ    if (x : τ) ∈ Γ
```

### Application

```
Γ ⊢ f : τ₁ → τ₂    Γ ⊢ e : τ₁
─────────────────────────────
       Γ ⊢ f e : τ₂
```

### Abstraction (Lambda)

```
Γ, x : τ₁ ⊢ e : τ₂
──────────────────────────
Γ ⊢ (GIVEN x YIELD e) : τ₁ → τ₂
```

### Conditional

```
Γ ⊢ c : BOOLEAN    Γ ⊢ e₁ : τ    Γ ⊢ e₂ : τ
─────────────────────────────────────────
    Γ ⊢ IF c THEN e₁ ELSE e₂ : τ
```

### Pattern Matching

```
Γ ⊢ e : T    Γ, vars(pᵢ) ⊢ eᵢ : τ  (for all i)
──────────────────────────────────────────────
Γ ⊢ CONSIDER e WHEN p₁ THEN e₁ ... : τ
```

---

## Type Checking Strategy

**Bidirectional:** Mix of inference and checking modes

**Checking mode:**
- Function definitions (signature provided)
- Branches must match declared type

**Inference mode:**
- Expressions without explicit type
- Field access, operators
- Bottom-up propagation

---

## Polymorphism

**Parametric polymorphism:** Supported in prelude functions
```haskell
-- In Haskell land
map :: (a -> b) -> [a] -> [b]
filter :: (a -> Bool) -> [a] -> [a]
```

**L4 usage:** Type variables implicit in prelude
```l4
map (GIVEN x YIELD x * 2) numbers  -- Inferred
```

---

## DEONTIC Type (Effect System)

**Semantics:** Modal logic operators
- `MUST` ≈ necessity (□)
- `MAY` ≈ possibility (◇)
- `SHANT` ≈ forbidden (¬□)

**Type signature:**
```l4
GIVETH A DEONTIC
```

**Composition:**
- Sequential: HENCE
- Alternative: LEST
- Conditional: IF guard

**Trace semantics:** `#TRACE` evaluates DEONTIC, produces obligation graph

---

## Type Errors

**Common errors:**
- Type mismatch in branches
- Undefined field access
- Arity mismatch in application
- Missing type annotation (GIVETH)
- Pattern match non-exhaustive (warning)

---

## Subtyping

**Limited subtyping:** Mostly nominal
**Exception:** Numeric literals have flexible type

---

## Unit Type

**Implicit:** `()` not exposed in syntax
**Use:** Nullary constructors effectively unit
```l4
DECLARE Status IS ONE OF Active | Closed
-- Active :: Status (nullary, implicitly Active :: () -> Status)
```

---

## Lazy Evaluation

**Strategy:** Call-by-need (Haskell-style)
**Implications:**
- Infinite lists possible (with care)
- WHERE bindings evaluated lazily
- Short-circuit boolean ops (AND, OR)

**Trace:** Evaluation produces trace for debugging

---

## Type Annotations in Code

```l4
-- Recommended: Top-level signatures (but type inference works without)
GIVEN x IS A T
GIVETH U  -- or GIVES U

-- Also valid: Omit GIVETH, let type inference determine return type
GIVEN x IS A T
f MEANS ...

-- Optional: Local type ascriptions (not in syntax yet)
-- Inferred: Expression types
```

---

## Advanced: Fixed Points

**Recursion:** Via letrec semantics in WHERE
```l4
factorial MEANS
    IF n <= 1 THEN 1
    ELSE n * (factorial (n - 1))
-- Fixed point: factorial = fix (λf. λn. if ...)
```

---

## Search Terms

**Type theory:** arrow type, product type, sum type, ADT, algebraic data type, inductive type, catamorphism, elimination form, introduction form, coproduct, exponential object

**Haskell:** Maybe monad, list type, pattern matching, lazy evaluation, Hindley-Milner, bidirectional typing, where clause, let-in, data declaration, record syntax, guards, case expression

**FP:** parametric polymorphism, higher-order function, lambda calculus, call-by-need, currying, partial application, pure function, referential transparency, immutable data

**Other languages:** struct (C/Go/Rust), class (Java/Python/JS), interface (TypeScript/Go), enum (Rust/Java/Swift), union (C/TypeScript), sealed class (Kotlin), Optional (Java/Swift), nullable (Kotlin/C#), Array (JS), Vec (Rust), dict/map/HashMap → (see prelude DICTIONARY)

**Modal logic:** necessity operator, possibility operator, deontic logic, temporal logic, Kripke semantics, obligation, permission, prohibition
