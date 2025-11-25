# Specification: Bidirectional Type Checking for L4

## Executive Summary

This document specifies adding **bidirectional type checking** to L4's type checker, enabling expected types to flow downward from context (like GIVETH annotations) to guide type inference at expression sites. This will unlock type-directed operations like JSONDECODE for records without explicit type annotations.

## Motivation

### Current Problem

When you define a typed function in L4:

```l4
GIVEN jsonStr IS A STRING
GIVETH A MAYBE Person
decodePerson jsonStr MEANS JSONDECODE jsonStr
```

The type checker knows:
- ✅ `decodePerson` has type `STRING -> MAYBE Person`
- ✅ The return type is `MAYBE Person`

But it **doesn't** propagate this expected type down to the expression `JSONDECODE jsonStr`. As a result:
- ❌ `JSONDECODE` is evaluated without knowing it should produce `MAYBE Person`
- ❌ Type-directed decoding can't use the Person type to decode JSON objects
- ❌ Generic decoding returns `JUST OF NOTHING` instead of `JUST OF Person {...}`

### Desired Behavior

With bidirectional type checking:
- ✅ The expected return type `MAYBE Person` flows down to `JSONDECODE jsonStr`
- ✅ JSONDECODE receives type information: "you should produce a MAYBE Person"
- ✅ Evaluation uses this type to guide decoding: decode JSON object to Person record
- ✅ Round-trip encoding/decoding works automatically

## Background: Bidirectional Type Checking

### What Is It?

Bidirectional type checking is a type checking approach with two modes:

1. **Synthesis (↑)**: "What type does this expression have?"
   - Information flows upward from expression to context
   - Example: `5 + 3` synthesizes type `NUMBER`

2. **Checking (↓)**: "Does this expression have the expected type?"
   - Information flows downward from context to expression
   - Example: Given expected type `STRING`, check that `"hello"` has type `STRING`

### Why Bidirectional?

Unidirectional (synthesis-only) type checking struggles with:
- Polymorphic functions without enough context
- Overloaded operators
- Type-directed operations (like JSONDECODE)
- Implicit conversions

Bidirectional type checking provides:
- Better type inference
- More precise error messages
- Type-directed code generation
- Support for dependent types and refinement types

### Key Papers

1. **Pierce & Turner (2000)**: "Local Type Inference" - introduced bidirectional typing
2. **Dunfield & Krishnaswami (2013)**: "Complete and Easy Bidirectional Typechecking for Higher-Rank Polymorphism"
3. **Löh et al. (2015)**: "Practical bidirectional type checking" in GHC

## Specification

### 1. Type Checking Modes

Extend L4's type checker with two mutually recursive judgment forms:

```
Γ ⊢ e ⇒ τ    (Synthesis: infer type τ from expression e)
Γ ⊢ e ⇐ τ    (Checking: check expression e against expected type τ)
```

Where:
- `Γ` is the typing context (environment)
- `e` is an expression
- `τ` is a type
- `⇒` means "synthesizes" (infers a type)
- `⇐` means "checks against" (validates expected type)

### 2. Expression Forms

#### 2.1 Synthesis Mode (↑)

These expressions **produce** a type:

- **Variables**: `Γ ⊢ x ⇒ τ` if `x : τ ∈ Γ`
- **Literals**:
  - `Γ ⊢ n ⇒ NUMBER` (numeric literals)
  - `Γ ⊢ "s" ⇒ STRING` (string literals)
  - `Γ ⊢ TRUE ⇒ BOOLEAN`, `Γ ⊢ FALSE ⇒ BOOLEAN`
- **Application**: If `Γ ⊢ f ⇒ τ₁ → τ₂` and `Γ ⊢ e ⇐ τ₁` then `Γ ⊢ f e ⇒ τ₂`
- **Annotated expressions**: `Γ ⊢ (e : τ) ⇒ τ` if `Γ ⊢ e ⇐ τ`

#### 2.2 Checking Mode (↓)

These expressions **consume** an expected type:

- **Lambda abstractions**:
  ```
  If τ = τ₁ → τ₂ and Γ, x : τ₁ ⊢ e ⇐ τ₂
  Then Γ ⊢ (λx. e) ⇐ τ₁ → τ₂
  ```

- **Record construction**:
  ```
  If τ = Record {f₁ : τ₁, ..., fₙ : τₙ}
  And Γ ⊢ e₁ ⇐ τ₁, ..., Γ ⊢ eₙ ⇐ τₙ
  Then Γ ⊢ Record WITH f₁ IS e₁, ..., fₙ IS eₙ ⇐ τ
  ```

- **CONSIDER/WHEN patterns**:
  ```
  If Γ ⊢ scrutinee ⇒ τ_scrutinee
  And for each branch: Γ ⊢ branch ⇐ τ_expected
  Then Γ ⊢ CONSIDER scrutinee WHEN ... ⇐ τ_expected
  ```

#### 2.3 Mode Switching

- **Subsumption**: `If Γ ⊢ e ⇒ τ' and τ' <: τ then Γ ⊢ e ⇐ τ`
  - Allows synthesis results to be checked against expected types
  - Handles subtyping

### 3. Function Definitions with GIVETH

Current behavior:
```l4
GIVEN x IS A τ₁
GIVETH τ₂
f x MEANS e
```

The type checker:
1. ✅ Assigns `f : τ₁ → τ₂`
2. ✅ Checks body `e` synthesizes some type
3. ❌ **Doesn't** check `e` against expected type `τ₂`

New behavior with bidirectional typing:
1. ✅ Assigns `f : τ₁ → τ₂`
2. ✅ Type checks body in checking mode: `Γ, x : τ₁ ⊢ e ⇐ τ₂`
3. ✅ Expected type `τ₂` flows down into `e`

### 4. Type-Directed Operations

Certain operations benefit from expected type information:

#### 4.1 JSONDECODE

**Signature**:
```haskell
JSONDECODE : STRING -> MAYBE α
```

**Type-directed behavior**:

When checking `JSONDECODE s ⇐ MAYBE Person`:
1. Extract expected result type: `MAYBE Person`
2. Extract inner type: `Person`
3. Pass type information to evaluator via Anno
4. Evaluator uses Person type to decode JSON object to record

**Implementation approach**:
- During type checking, store expected type in Anno's resolvedInfo
- Evaluation reads this type from Anno to guide decoding

#### 4.2 Future: Overloaded Literals

Expected types could guide interpretation of numeric literals:
```l4
-- Expected type disambiguates
DECIDE x IS 42        -- Could be NUMBER
DECIDE y IS 42 :: Int32   -- Definitely Int32
```

### 5. Implementation Plan

#### Phase 1: Core Infrastructure

**File**: `jl4-core/src/L4/TypeCheck.hs`

1. Add checking mode to Check monad:
   ```haskell
   data CheckMode = Synth | Check (Type' Resolved)

   -- Current: only synthesis
   inferExpr :: Expr Name -> Check (Expr Resolved, Type' Resolved)

   -- New: bidirectional
   synthExpr :: Expr Name -> Check (Expr Resolved, Type' Resolved)
   checkExpr :: Expr Name -> Type' Resolved -> Check (Expr Resolved)
   ```

2. Refactor existing `inferExpr` to call `synthExpr`

3. Implement mode switching via subsumption

#### Phase 2: Expression Forms

Update each expression form to support checking mode:

1. **Lambda/GIVEN expressions**: Check body against return type
2. **Application**: Check arguments against parameter types
3. **Record construction**: Check fields against record type
4. **CONSIDER/WHEN**: Check branches against expected result type

#### Phase 3: Function Definitions

**File**: `jl4-core/src/L4/TypeCheck/Decl.hs`

Modify `checkDecl` for function definitions:
```haskell
-- For: GIVEN x IS τ₁ GIVETH τ₂ f x MEANS e
checkFunDecl name givenTypes returnType body = do
  -- Current: just synthesize
  -- (bodyResolved, bodyType) <- inferExpr body

  -- New: check against expected return type
  bodyResolved <- checkExpr body returnType
  -- Ensure expected type flows into body
```

#### Phase 4: Annotation Storage

Store expected types in Anno during type checking:

```haskell
-- When checking e ⇐ τ:
checkExpr e expectedType = do
  eResolved <- ... -- check expression
  pure $ eResolved & annoOf % annInfo ?~ TypeInfo expectedType Nothing
```

This makes the expected type available during evaluation.

#### Phase 5: Evaluation Integration

**File**: `jl4-core/src/L4/EvaluateLazy/Machine.hs`

The infrastructure is already in place!
- Type information flows through Frame constructors ✅
- `runBuiltin` accepts `Maybe (Type' Resolved)` ✅
- `decodeJsonToValueTyped` uses type information ✅

No evaluation changes needed - just need type checker to populate Anno.

### 6. Examples

#### Example 1: JSONDECODE with Record Type

**Input**:
```l4
DECLARE Person HAS
  name IS A STRING
  age IS A NUMBER

GIVEN jsonStr IS A STRING
GIVETH A MAYBE Person
decodePerson jsonStr MEANS JSONDECODE jsonStr

DECIDE json IS "{\"name\":\"Alice\",\"age\":30}"
DECIDE result IS decodePerson json
#EVAL result
```

**Current behavior**:
```
result = JUST OF NOTHING
```

**With bidirectional typing**:
```
result = JUST OF Person WITH name IS "Alice" age IS 30
```

**Why it works**:
1. Type checker sees `decodePerson : STRING -> MAYBE Person`
2. When checking body `JSONDECODE jsonStr`, expected type is `MAYBE Person`
3. Type checker stores `MAYBE Person` in Anno of JSONDECODE expression
4. Evaluator extracts type from Anno, uses it to decode JSON to Person

#### Example 2: Nested Function Calls

**Input**:
```l4
GIVEN x IS A NUMBER
GIVETH A STRING
formatNumber x MEANS x AS STRING

GIVEN json IS A STRING
GIVETH A MAYBE STRING
decodeAndFormat json MEANS
  CONSIDER JSONDECODE json
  WHEN JUST WITH payload IS n
  THEN formatNumber n
  ELSE "error"
```

**Analysis**:
- Expected type of `decodeAndFormat` body: `MAYBE STRING`
- Expected type of THEN branch: `STRING`
- Expected type of `formatNumber n`: `STRING` ✅
- Expected type of ELSE branch: `STRING` ✅

#### Example 3: Record Construction

**Input**:
```l4
DECLARE Response HAS
  status IS A STRING
  code IS A NUMBER

GIVEN err IS A STRING
GIVETH A Response
makeError err MEANS
  Response WITH
    status IS err
    code IS 500
```

**With bidirectional typing**:
1. Expected return type: `Response`
2. Check record construction against `Response` type
3. Verify fields match: `status : STRING`, `code : NUMBER` ✅

### 7. Test Cases

#### Test 1: Basic JSONDECODE

```l4
-- File: jl4/examples/ok/bidirectional-jsondecode-basic.l4

DECLARE Person HAS
  name IS A STRING
  age IS A NUMBER

GIVEN s IS A STRING
GIVETH A MAYBE Person
decodePerson s MEANS JSONDECODE s

DECIDE json IS "{\"name\":\"Bob\",\"age\":25}"
DECIDE result IS decodePerson json

#EVAL result
-- Expected: JUST OF Person WITH name IS "Bob" age IS 25
```

#### Test 2: Nested Records

```l4
-- File: jl4/examples/ok/bidirectional-jsondecode-nested.l4

DECLARE Address HAS
  street IS A STRING
  city IS A STRING

DECLARE Employee HAS
  name IS A STRING
  address IS AN Address

GIVEN s IS A STRING
GIVETH A MAYBE Employee
decodeEmployee s MEANS JSONDECODE s

DECIDE json IS "{\"name\":\"Carol\",\"address\":{\"street\":\"123 Main\",\"city\":\"Boston\"}}"
DECIDE result IS decodeEmployee json

#EVAL result
-- Expected: JUST OF Employee WITH
--   name IS "Carol"
--   address IS Address WITH street IS "123 Main" city IS "Boston"
```

#### Test 3: Extra JSON Fields Ignored

```l4
-- File: jl4/examples/ok/bidirectional-jsondecode-extra-fields.l4

DECLARE Person HAS
  name IS A STRING

GIVEN s IS A STRING
GIVETH A MAYBE Person
decodePerson s MEANS JSONDECODE s

-- JSON has extra field "age" that's not in Person type
DECIDE json IS "{\"name\":\"Dave\",\"age\":30,\"country\":\"USA\"}"
DECIDE result IS decodePerson json

#EVAL result
-- Expected: JUST OF Person WITH name IS "Dave"
-- (age and country fields ignored)
```

#### Test 4: Type Mismatch

```l4
-- File: jl4/examples/not-ok/bidirectional-jsondecode-type-error.l4

DECLARE Person HAS
  name IS A STRING
  age IS A NUMBER

GIVEN s IS A STRING
GIVETH A MAYBE Person
decodePerson s MEANS JSONDECODE s

-- JSON has age as string, not number
DECIDE json IS "{\"name\":\"Eve\",\"age\":\"thirty\"}"
DECIDE result IS decodePerson json

#EVAL result
-- Expected: NOTHING (decode failed due to type mismatch)
```

#### Test 5: Missing Required Fields

```l4
-- File: jl4/examples/not-ok/bidirectional-jsondecode-missing-field.l4

DECLARE Person HAS
  name IS A STRING
  age IS A NUMBER

GIVEN s IS A STRING
GIVETH A MAYBE Person
decodePerson s MEANS JSONDECODE s

-- JSON missing required "age" field
DECIDE json IS "{\"name\":\"Frank\"}"
DECIDE result IS decodePerson json

#EVAL result
-- Expected: NOTHING (decode failed due to missing field)
```

### 8. Error Messages

Bidirectional type checking enables better error messages:

#### Current (synthesis-only):
```
Type error: Cannot unify expected type Person with inferred type α
```

#### With bidirectional:
```
Type error in function decodePerson:
  Expected: MAYBE Person
  Expression: JSONDECODE jsonStr

  The expression JSONDECODE produces a polymorphic MAYBE type,
  but the function signature specifies MAYBE Person.

  Hint: The type checker can infer the Person type, but evaluation
  needs this type information to decode JSON objects correctly.
```

### 9. Related Work

#### Haskell
- Uses bidirectional typing for type class resolution
- `TypeApplications` extension makes expected types explicit
- Example: `read @Int "42"` supplies expected type explicitly

#### Rust
- Type inference flows in both directions
- `let x: Vec<i32> = ...` provides expected type to RHS
- Method calls guide type parameter inference

#### TypeScript
- Contextual typing: expected types guide inference
- Example: `arr.map((x) => x * 2)` infers `x` type from `arr`

#### Idris/Agda
- Full bidirectional type checking for dependent types
- Expected types essential for type-level computation

### 10. Implementation Checklist

- [ ] **Phase 1**: Core infrastructure
  - [ ] Add `CheckMode` type
  - [ ] Split `inferExpr` into `synthExpr` and `checkExpr`
  - [ ] Implement subsumption rule
  - [ ] Add test: basic synthesis vs checking

- [ ] **Phase 2**: Expression forms
  - [ ] Lambda/GIVEN: check body against return type
  - [ ] Application: check arguments
  - [ ] Record construction: check fields
  - [ ] CONSIDER/WHEN: check branches
  - [ ] Add tests for each form

- [ ] **Phase 3**: Function definitions
  - [ ] Modify `checkDecl` for GIVETH functions
  - [ ] Check body against return type
  - [ ] Add test: function with explicit return type

- [ ] **Phase 4**: Annotation storage
  - [ ] Store expected type in Anno during checking
  - [ ] Verify Anno propagates to evaluation
  - [ ] Add test: Anno contains expected type

- [ ] **Phase 5**: Integration
  - [ ] Test JSONDECODE with Person type
  - [ ] Test nested records
  - [ ] Test extra fields ignored
  - [ ] Test error cases
  - [ ] Update all 7 test files

- [ ] **Phase 6**: Documentation
  - [ ] Update type checker docs
  - [ ] Add examples to language guide
  - [ ] Document JSONDECODE behavior
  - [ ] Add troubleshooting guide

### 11. Success Criteria

✅ Implementation is complete when:

1. **JSONDECODE works with typed functions**:
   ```l4
   GIVEN s IS A STRING
   GIVETH A MAYBE Person
   decode s MEANS JSONDECODE s
   ```
   Returns `JUST OF Person {...}` not `JUST OF NOTHING`

2. **All existing tests pass** (375+ examples)

3. **New bidirectional tests pass** (5+ examples)

4. **Error messages improve** for type mismatches

5. **Type annotations propagate** to Anno during checking

6. **Round-trip encoding/decoding works**:
   ```l4
   DECIDE p IS Person WITH name IS "Alice" age IS 30
   DECIDE json IS JSONENCODE p
   DECIDE p2 IS decodePerson json  -- p2 = JUST OF Person {...}
   ```

### 12. Non-Goals

This spec **does not** cover:

- ❌ Type inference for untyped functions (still requires GIVETH)
- ❌ Subtyping beyond simple subsumption
- ❌ Dependent types or refinement types
- ❌ Type-directed overloading resolution
- ❌ Type classes or traits
- ❌ Higher-rank polymorphism

These could be future enhancements building on bidirectional infrastructure.

### 13. References

**Academic Papers**:
1. Pierce, B. C., & Turner, D. N. (2000). "Local type inference." ACM TOPLAS, 22(1), 1-44.
2. Dunfield, J., & Krishnaswami, N. R. (2013). "Complete and easy bidirectional typechecking for higher-rank polymorphism." ICFP 2013.
3. Löh, A., McBride, C., & Swierstra, W. (2015). "A tutorial implementation of a dependently typed lambda calculus."

**Implementation Examples**:
- GHC's bidirectional type checker
- Agda's elaboration algorithm
- TypeScript's contextual typing
- Rust's inference engine

**L4 Codebase**:
- `jl4-core/src/L4/TypeCheck.hs` - Main type checker
- `jl4-core/src/L4/TypeCheck/Decl.hs` - Declaration checking
- `jl4-core/src/L4/EvaluateLazy/Machine.hs` - Evaluation (already has type-directed infrastructure!)

---

**Document Version**: 1.0
**Date**: 2025-11-25
**Author**: Claude Code + Meng Wong
**Status**: Ready for Implementation
