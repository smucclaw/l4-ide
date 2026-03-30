# Type Theory

Formal type theory foundations of L4's type system. This page is for advanced users -- language implementors, functional programming developers, and type theory enthusiasts -- who want to understand the precise mechanics behind L4's type checking, inference, and evaluation.

For practical usage of L4 types, see the [Types Reference](README.md).

---

## Type System Overview

L4's type system draws from well-established foundations in programming language theory:

- **Paradigm:** Hindley-Milner-style with algebraic data types
- **Inference:** Bidirectional type checking (mixing inference and checking modes)
- **Purity:** Pure functional core with no side effects in the expression language
- **Effect type:** DEONTIC serves as a modal effect type for regulative rules
- **Evaluation:** Lazy (call-by-need), following Haskell semantics

The combination of Hindley-Milner inference with bidirectional checking means that most expressions do not require explicit type annotations, while top-level function signatures (via GIVEN/GIVETH) provide checking anchors that improve error messages and serve as documentation.

---

## Type Inference Rules

The following rules describe how types are assigned to L4 expressions. The notation uses a typing context (environment) written as a Greek capital gamma, and the turnstile symbol to mean "entails" or "has type".

### Variables

A variable has the type assigned to it in the environment:

```
Γ ⊢ x : τ    if (x : τ) ∈ Γ
```

### Application

If `f` has function type and `e` has the argument type, then the application `f e` has the return type:

```
Γ ⊢ f : τ₁ → τ₂    Γ ⊢ e : τ₁
─────────────────────────────────
         Γ ⊢ f e : τ₂
```

### Abstraction (Lambda / GIVEN...YIELD)

If, extending the environment with a binding for `x`, the body `e` has type τ₂, then the lambda (written `GIVEN x YIELD e` in L4) has arrow type:

```
   Γ, x : τ₁ ⊢ e : τ₂
──────────────────────────────
Γ ⊢ (GIVEN x YIELD e) : τ₁ → τ₂
```

### Conditional (IF/THEN/ELSE)

The condition must be BOOLEAN, and both branches must have the same type:

```
Γ ⊢ c : BOOLEAN    Γ ⊢ e₁ : τ    Γ ⊢ e₂ : τ
────────────────────────────────────────────────
       Γ ⊢ IF c THEN e₁ ELSE e₂ : τ
```

### Pattern Matching (CONSIDER/WHEN)

The scrutinee must have a known type T, and all branches must produce the same result type. Pattern variables are brought into scope within each branch:

```
Γ ⊢ e : T    Γ, vars(pᵢ) ⊢ eᵢ : τ  (for all i)
──────────────────────────────────────────────────
  Γ ⊢ CONSIDER e WHEN p₁ THEN e₁ ... : τ
```

---

## Bidirectional Type Checking

L4 uses a bidirectional type checking strategy that combines two modes:

### Checking Mode

In checking mode, the expected type flows **downward** into an expression. This is used when:

- A function definition has an explicit signature via GIVETH -- the declared return type is pushed into the function body
- Branches of IF/THEN/ELSE and CONSIDER/WHEN are checked against the type established by the first branch or the enclosing signature
- Arguments to functions with known parameter types

Checking mode produces better error messages because the system knows what type it expects.

### Inference Mode

In inference mode, the type flows **upward** from subexpressions. This is used when:

- Expressions appear without an explicit type annotation
- Field access (`person's name`) -- the field type is looked up in the record declaration
- Operators -- the result type is determined by the operator's definition
- Literals -- NUMBER, STRING, BOOLEAN literals have known types

### How They Interact

In practice, top-level GIVEN/GIVETH declarations establish checking anchors, while inference fills in the types of internal expressions. This means you rarely need to annotate local bindings: the top-level signature provides enough information for the checker to propagate types throughout the function body.

---

## Parametric Polymorphism

L4 supports parametric polymorphism (generics) primarily through prelude functions. Type variables are implicit -- the system infers the concrete types at each call site.

Prelude functions like `map` and `filter` are polymorphic:

```
map    : (a -> b) -> LIST OF a -> LIST OF b
filter : (a -> BOOLEAN) -> LIST OF a -> LIST OF a
```

In L4 usage, the type variables are instantiated by inference:

```l4
map (GIVEN x YIELD x * 2) numbers
```

Here `a` and `b` are both inferred as NUMBER from the multiplication operator and the type of `numbers`.

User-defined polymorphism is available through type parameters declared with `IS A TYPE`:

```l4
GIVEN T IS A TYPE
GIVEN xs IS A LIST OF T
GIVETH A LIST OF T
`reverse` MEANS ...
```

---

## DEONTIC as Modal Type

The DEONTIC type is L4's effect type for regulative rules. It has a modal logic interpretation that connects legal obligation theory (deontic logic) to type theory.

### Modal Operators

| L4 Keyword | Deontic Meaning | Modal Logic       | Symbol  |
| ---------- | --------------- | ----------------- | ------- |
| MUST       | Obligation      | Necessity         | Box     |
| MAY        | Permission      | Possibility       | Diamond |
| SHANT      | Prohibition     | Negated necessity | Not-Box |

In modal logic terms:

- **MUST p** asserts that p is obligatory -- in all compliant futures, p holds. This corresponds to the necessity operator.
- **MAY p** asserts that p is permitted -- there exists a compliant future where p holds. This corresponds to the possibility operator.
- **SHANT p** asserts that p is forbidden -- in no compliant future does p hold. This corresponds to the negation of the necessity operator applied to p.

### Composition

DEONTIC expressions compose through:

- **HENCE** -- Sequential composition. After one obligation is fulfilled, the next obligation follows.
- **LEST** -- Alternative composition. If an obligation is breached, the LEST branch specifies consequences.
- **IF** -- Conditional guard. The deontic obligation only activates when the condition holds.

### Trace Semantics

When evaluated with `#TRACE`, a DEONTIC expression produces an obligation graph -- a structured trace showing the sequence of obligations, permissions, and prohibitions, along with their conditions and consequences. This trace is the basis for L4's explainable AI capabilities: it provides an audit-grade record of how the system arrived at its conclusions.

### Type Signature

A regulative rule declares its effect type as:

```l4
GIVETH A DEONTIC
```

Within the type system, DEONTIC acts as a custom monad (in Haskell terms) or, in category-theoretic terms, lives in a Kleisli category.

---

## Subtyping

L4 has **limited, mostly nominal** subtyping:

- Types are distinguished by name, not by structure. Two record types with identical fields but different names are different types.
- **Numeric literal flexibility:** Numeric literals have a flexible type that can be used where NUMBER is expected, without explicit coercion. This is the primary exception to strict nominal typing.
- There is no structural subtyping or row polymorphism.

---

## Unit Type

The unit type (written `()` in Haskell) is **implicit** in L4 -- it is not directly exposed in the surface syntax.

Its primary manifestation is through nullary constructors in sum types:

```l4
DECLARE Status IS ONE OF Active | Closed
```

Here, `Active` and `Closed` are nullary constructors. Internally, they can be understood as functions from unit to Status (`() -> Status`), though this is not visible to the L4 programmer.

---

## Lazy Evaluation

L4 uses **call-by-need** evaluation semantics, following Haskell:

- **Expressions are not evaluated until their value is needed.** A WHERE binding that is never referenced in the result is never computed.
- **Once evaluated, the result is memoized.** Subsequent references to the same binding reuse the computed value.

### Implications

- **Infinite data structures** are possible in principle (e.g., infinite lists), though this is rarely needed in legal domain code.
- **Short-circuit evaluation:** AND and OR do not evaluate their second operand if the first operand determines the result. This is a natural consequence of laziness, not a special case.
- **WHERE bindings are lazy:** Definitions in a WHERE clause are only evaluated if referenced. This allows defining multiple helper values without paying for unused ones.
- **Evaluation traces:** The `#TRACE` directive records which expressions were actually evaluated and in what order, providing visibility into the lazy evaluation process for debugging.

---

## Fixed Points and Recursion

Recursive definitions in L4 use **letrec semantics** in WHERE blocks. A function can refer to itself in its own definition:

```l4
GIVEN n IS A NUMBER
GIVETH A NUMBER
factorial MEANS
    IF n <= 1 THEN 1
    ELSE n * (factorial (n - 1))
```

In type-theoretic terms, this is interpreted as a fixed point:

```
factorial = fix (λf. λn. if n ≤ 1 then 1 else n × f(n - 1))
```

Where `fix` is the fixed-point combinator satisfying `fix f = f (fix f)`. The type checker handles recursive bindings by first assuming a type for the recursive name, then checking the body against that assumption, and finally verifying consistency.

Mutual recursion (multiple functions referencing each other in the same WHERE block) follows the same letrec semantics.

---

## See Also

- **[Types Reference](README.md)** -- Practical guide to all L4 types
- **[Algebraic Types](../../concepts/algebraic-types.md)** -- Conceptual explanation of product and sum types
- **[Regulative Rules](../regulative/README.md)** -- MUST, MAY, SHANT and regulative rule structure
- **[GLOSSARY](../GLOSSARY.md)** -- Complete language feature index
