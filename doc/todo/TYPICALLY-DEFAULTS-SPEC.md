# Specification: TYPICALLY Keyword for Default Values

**Status:** 🚧 In Progress (Core Implementation Complete)
**Related:** `doc/default-values.md` (conceptual background), `BOOLEAN-MINIMIZATION-SPEC.md`

## Implementation Progress

| Phase | Description                                            | Status                  |
| ----- | ------------------------------------------------------ | ----------------------- |
| 1     | Lexer - add TYPICALLY keyword                          | ✅ Complete             |
| 2     | AST - extend TypedName, OptionallyTypedName, Assume    | ✅ Complete             |
| 3     | Parser - parse TYPICALLY clauses                       | ✅ Complete             |
| 4     | Type Checking - validate TYPICALLY values              | ✅ Complete             |
| 5a    | IDE - Syntax highlighting                              | ✅ Complete (automatic) |
| 5b    | IDE - Autocomplete                                     | ✅ Complete (automatic) |
| 5c    | IDE - Hover showing defaults                           | ⏳ Deferred (see note)  |
| 6     | Strict directive variants (#EVALSTRICT, #ASSERTSTRICT) | ✅ Complete             |
| 7     | Decision Service API - defaultMode parameter           | ⏳ Not started          |

> **Note:** The presumptive wrappers currently expose `JUST`/`NOTHING` and will continue to do so until the runtime adopts the four-state `InputState` model described later in this document.

**Note on hover:** Extending the `Info` type to include TYPICALLY values conflicts with Optics generic traversals used in visualization code. Requires architectural changes to use a separate `TypicallyMap` instead of modifying `Info`. Deferred to future work.

**Test files:**

- `jl4/examples/ok/typically-basic.l4` - comprehensive examples
- `jl4/examples/ok/evalstrict.l4` - strict directive variants
- `jl4/examples/not-ok/tc/typically-type-mismatch.l4` - type error testing
- `jl4/examples/not-ok/tc/typically-on-assume.l4` - TYPICALLY on ASSUME error

**Bonus:** ASSUME deprecation warnings implemented (see `ASSUME-DEPRECATION-SPEC.md`)

## Executive Summary

Add a `TYPICALLY` keyword to L4 that allows specifying default values for:

1. **DECLARE fields** - default values for record type fields
2. **GIVEN parameters** - default values for function parameters
3. **ASSUME declarations** - default values for assumed external values

These defaults represent **rebuttable presumptions** - they hold unless explicitly overridden by user input.

## Motivation

### Legal Domain: Rebuttable Presumptions

In legal reasoning, many terms carry implicit assumptions:

- A "person" is typically a natural person with mental capacity
- A contract party is typically not under duress
- A transaction is typically at arm's length
- A taxpayer is typically a resident

These are **rebuttable presumptions** - defaults that apply unless proven otherwise. L4 should support this pattern natively.

### UX: Reducing Question Burden

In interactive applications (chatbots, web forms), asking every possible question is tedious. With TYPICALLY:

```
Without TYPICALLY:
  Chatbot: Are you married?
  Chatbot: Do you have spousal approval?
  Chatbot: Are you buying beer only?
  Chatbot: Do you have parental approval?
  Chatbot: Are you legally emancipated?

With TYPICALLY (married TYPICALLY FALSE, etc.):
  Chatbot: How old are you?
  User: 30
  Chatbot: You can purchase alcohol! (assumed unmarried based on typical case)
```

### Formal Verification: Explicit Assumptions

When verifying contracts, TYPICALLY values make assumptions explicit and auditable:

```
Result: TRUE
Assumptions used:
  - has_capacity = TRUE (TYPICALLY)
  - is_under_duress = FALSE (TYPICALLY)
  - is_natural_person = TRUE (TYPICALLY)
```

## Syntax Design

### In DECLARE (Record Type Fields)

```l4
DECLARE Person HAS
  name IS A STRING
  age IS A NUMBER
  has_capacity IS A BOOLEAN TYPICALLY TRUE
  is_natural_person IS A BOOLEAN TYPICALLY TRUE
  is_under_duress IS A BOOLEAN TYPICALLY FALSE
  jurisdiction IS A STRING TYPICALLY "default"
```

### In GIVEN (Function Parameters)

```l4
GIVEN
  buyer IS A Person
  age IS A NUMBER
  married IS A BOOLEAN TYPICALLY FALSE
  has_spousal_approval IS A BOOLEAN TYPICALLY FALSE
  beer_only IS A BOOLEAN TYPICALLY FALSE
  has_parental_approval IS A BOOLEAN TYPICALLY FALSE
  is_emancipated IS A BOOLEAN TYPICALLY FALSE
GIVETH A BOOLEAN
DECIDE `may purchase alcohol` IF ...
```

### In ASSUME (External Declarations)

**TYPICALLY is NOT allowed on ASSUME declarations.** This is a type checking error.

```l4
-- ERROR: TYPICALLY not allowed on ASSUME
ASSUME `person has capacity` IS A BOOLEAN TYPICALLY TRUE

-- OK: ASSUME without TYPICALLY
ASSUME `person has capacity` IS A BOOLEAN
```

**Rationale:** The ASSUME keyword is deprecated (see `RUNTIME-INPUT-STATE-SPEC.md`). Supporting TYPICALLY on ASSUME would require complex runtime changes for strict/non-strict evaluation modes. Since ASSUME is being phased out, the simpler solution is to disallow the combination entirely. Use GIVEN or DECLARE with TYPICALLY instead.

### Grammar Extension

```ebnf
TypeAnnotation ::= "IS" Article? Type TypicallyClause?

TypicallyClause ::= "TYPICALLY" Literal

Literal ::= BoolLiteral | NumberLiteral | StringLiteral

BoolLiteral ::= "TRUE" | "FALSE"
```

## Semantics (Compile-Time)

### What TYPICALLY Means

At the language level, TYPICALLY is simply **metadata attached to a type annotation**. It declares:

> "If no value is provided at runtime, this default should be used."

The AST stores the default value, and downstream consumers (evaluator, decision service, form generators) decide how to use it.

```haskell
-- Compile-time: TYPICALLY is just an optional literal in the AST
data TypedName n = MkTypedName
  { tnAnno :: Anno
  , tnName :: n
  , tnType :: Type' n
  , tnTypically :: Maybe (Expr n)  -- Nothing = no default, Just = has default
  }
```

### Type Checking

The TYPICALLY value must:

1. Be a **literal** (compile-time constant)
2. Match the declared type

```haskell
-- OK
x IS A BOOLEAN TYPICALLY TRUE
age IS A NUMBER TYPICALLY 18

-- ERROR: type mismatch
x IS A BOOLEAN TYPICALLY 42

-- ERROR: not a literal
x IS A BOOLEAN TYPICALLY (a AND b)
```

### Scope of This Spec

This spec covers **compile-time concerns only**:

- Syntax for declaring TYPICALLY values
- AST representation
- Type checking rules
- Making defaults available to downstream consumers

**Runtime concerns are separate.** How interactive applications track user input states (explicit value vs "I don't know" vs "not yet asked") is covered in `RUNTIME-INPUT-STATE-SPEC.md`.

### Evaluation Semantics (Basic)

The basic evaluation rule is simple:

```haskell
resolveParameter :: Maybe Value -> Maybe Value -> Value
resolveParameter (Just v) _            = v        -- Input provided
resolveParameter Nothing  (Just deflt) = deflt    -- Use TYPICALLY
resolveParameter Nothing  Nothing      = VUnknown -- No input, no default
```

For richer runtime behavior (distinguishing "I don't know" from "not asked"), see `RUNTIME-INPUT-STATE-SPEC.md`.

## Implementation Plan

### Phase 1: Lexer & Parser

**File:** `jl4-core/src/L4/Lexer.hs`

Add TYPICALLY keyword:

```haskell
keywords :: Map Text TKeywords
keywords = Map.fromList
  [ ...
  , ("TYPICALLY", TKTypically)
  ...
  ]
```

**File:** `jl4-core/src/L4/Parser.hs`

Extend type annotation parsing:

```haskell
parseTypicallyClause :: Parser (Maybe (Expr Name))
parseTypicallyClause = optional $ do
  keyword TKTypically
  parseLiteral

parseTypeAnnotation :: Parser (Type' Name, Maybe (Expr Name))
parseTypeAnnotation = do
  keyword TKIs
  optional (keyword TKA <|> keyword TKAn)
  ty <- parseType
  typically <- parseTypicallyClause
  pure (ty, typically)
```

### Phase 2: AST Extension

**File:** `jl4-core/src/L4/Syntax.hs`

Extend relevant AST nodes:

```haskell
-- For DECLARE fields
data TypedName n = MkTypedName
  { tnAnno :: Anno
  , tnName :: n
  , tnType :: Type' n
  , tnTypically :: Maybe (Expr n)  -- NEW: default value
  }

-- For GIVEN parameters (in TypeSig)
data TypeSigItem n = MkTypeSigItem
  { tsiAnno :: Anno
  , tsiName :: n
  , tsiType :: Type' n
  , tsiTypically :: Maybe (Expr n)  -- NEW: default value
  }

-- For ASSUME declarations
data Assume n = MkAssume
  { assumeAnno :: Anno
  , assumeTypeSig :: TypeSig n
  , assumeAppForm :: AppForm n
  , assumeType :: Maybe (Type' n)
  , assumeTypically :: Maybe (Expr n)  -- NEW: default value
  }
```

### Phase 3: Type Checking

**File:** `jl4-core/src/L4/TypeCheck.hs`

Validate TYPICALLY expressions:

```haskell
checkTypically :: Type' Resolved -> Maybe (Expr Name) -> TC (Maybe (Expr Resolved))
checkTypically _ Nothing = pure Nothing
checkTypically expectedTy (Just expr) = do
  (expr', actualTy) <- inferExpr expr
  unify expectedTy actualTy
  -- Ensure it's a literal (compile-time constant)
  unless (isLiteral expr') $
    throwError "TYPICALLY value must be a literal"
  pure (Just expr')
```

### Phase 4: Expose Defaults to Consumers

Make TYPICALLY values accessible to downstream code:

**File:** `jl4-core/src/L4/Syntax.hs` or new module

```haskell
-- Extract all TYPICALLY defaults from a function's type signature
getDefaults :: Decide Resolved -> Map Text (Expr Resolved)
getDefaults decide = Map.fromList
  [ (name, expr)
  | item <- typeSigItems (decideTypeSig decide)
  , Just expr <- [tsiTypically item]
  , let name = tsiName item
  ]
```

This allows the decision service, form generators, and other consumers to query what defaults exist without duplicating parsing logic.

### Phase 5: IDE Support

**File:** `jl4-lsp/src/...`

- Syntax highlighting for TYPICALLY keyword
- Hover information showing default values
- Autocomplete suggesting TYPICALLY after type annotations
- Diagnostics for type mismatches in TYPICALLY values

### Phase 6: Strict Directive Variants

To support explicit control over whether TYPICALLY defaults are honored during evaluation, we added three new directive variants:

| Directive               | Description                                                                           |
| ----------------------- | ------------------------------------------------------------------------------------- |
| `#EVALSTRICT expr`      | Evaluate expression, treating missing values as Unknown (ignoring TYPICALLY defaults) |
| `#EVALTRACESTRICT expr` | Same as #EVALSTRICT but with trace output                                             |
| `#ASSERTSTRICT expr`    | Assert expression in strict mode                                                      |

**Semantics:**

- **Non-strict (default):** `#EVAL`, `#EVALTRACE`, `#ASSERT` honor TYPICALLY defaults. If a value is not provided but has a TYPICALLY annotation, the default is used.
- **Strict:** `#EVALSTRICT`, `#EVALTRACESTRICT`, `#ASSERTSTRICT` ignore TYPICALLY defaults. Missing values remain Unknown, forcing the user to provide all values explicitly.

**Use cases:**

1. **Testing:** Verify behavior without assumptions
2. **Audit:** See exactly what values are needed vs defaulted
3. **Strict mode applications:** Some contexts require explicit confirmation of all facts

**Implementation files:**

- `jl4-core/src/L4/Lexer.hs` - Added `TLazyEvalStrictDirective`, `TLazyEvalTraceStrictDirective`, `TAssertStrictDirective` tokens
- `jl4-core/src/L4/Syntax.hs` - Added `LazyEvalStrict`, `LazyEvalTraceStrict`, `AssertStrict` AST variants
- `jl4-core/src/L4/Parser.hs` - Parser cases for new directives
- `jl4-core/src/L4/EvaluateLazy/Machine.hs` - Added `strict :: !Bool` field to `EvalDirective`
- `jl4-core/src/L4/Print.hs`, `L4/Nlg.hs`, `L4/TypeCheck.hs` - Pretty-printing, NLG, type checking support

**Example:**

```l4
GIVEN x IS A BOOLEAN TYPICALLY TRUE
GIVETH A BOOLEAN
DECIDE foo IF x

-- Honors default: evaluates to TRUE
#EVAL foo

-- Ignores default: evaluates to Unknown
#EVALSTRICT foo
```

**Note:** The `strict` flag is currently stored in `EvalDirective` but does not yet change evaluation behavior. The semantic implementation (actually using TYPICALLY values during evaluation) is future work.

### Phase 7: Presumptive Evaluation (PEVAL/PASSERT)

**Status:** 🚧 In Progress (Syntax Complete, Semantics Incomplete)

The EVALSTRICT approach (Phase 6) subtracts defaults. PEVAL takes the opposite approach: it **presumes defaults** when explicit values are not provided, using a Maybe-wrapper pattern to distinguish "use default" from "use this explicit value."

| Directive                  | Description                                                         |
| -------------------------- | ------------------------------------------------------------------- |
| `#PEVAL expr args...`      | Evaluate expression with presumptive defaults for Nothing arguments |
| `#PEVALTRACE expr args...` | Same as #PEVAL but with trace output                                |
| `#PASSERT expr args...`    | Assert expression in presumptive mode                               |

`#PASSERT` now unwraps the `Maybe` result and treats `NOTHING` as a failed assertion, so a missing required input causes the directive to go red instead of silently passing.

**Design Approach:**

The compiler generates a "presumptive" wrapper for every function with TYPICALLY defaults. The wrapper has Maybe-wrapped parameter types:

```l4
-- Original function
GIVEN age IS A NUMBER TYPICALLY 18
GIVETH A BOOLEAN
DECIDE `can vote` IF age >= 18

-- Generated presumptive wrapper (conceptual)
-- Parameter becomes: Maybe NUMBER
-- Logic:
--   Nothing + TYPICALLY defined → use default (18)
--   Nothing + no TYPICALLY → Unknown
--   Just v → use v
```

> **Implementation note (Jan 2026):** The type checker synthesizes `'presumptive …'` helpers and `inferSection` now rewrites `#PEVAL`, `#PEVALTRACE`, and `#PASSERT` directives to call them. CLI output therefore surfaces `Maybe` results (`JUST 30`, `NOTHING`, etc.), while the runtime `maybeApplyDefaults` fallback remains as a safety net for any expressions that still bypass the wrappers.

**Current Implementation Status:**

The current implementation uses a **simplified auto-apply approach** that automatically applies TYPICALLY defaults when a function evaluates to a closure:

```l4
GIVEN
  age IS A NUMBER TYPICALLY 18
GIVETH A BOOLEAN
DECIDE `can vote` IF age >= 18

-- EVAL returns a closure (no automatic default application)
#EVAL `can vote`  -- Returns: <function>

-- PEVAL automatically applies the TYPICALLY default when result is a closure
#PEVAL `can vote`  -- Returns: TRUE (using age=18)
```

**How auto-apply works:**

1. `#PEVAL 'can vote'` evaluates to a `ValClosure` (since no arguments provided)
2. `maybeApplyDefaults` detects the closure has TYPICALLY defaults in its GivenSig
3. Defaults are automatically extracted and applied to evaluate the closure body
4. Returns the final result

This auto-apply behavior will remain as a convenience feature even after wrapper generation is implemented.

**Future Design: Explicit MAYBE-Wrapped Parameters**

The intended long-term design uses explicit MAYBE-wrapped parameters for fine-grained control. This requires wrapper function generation:

```l4
-- Desired syntax (not yet implemented):
GIVEN age IS A MAYBE NUMBER
GIVETH A MAYBE BOOLEAN
DECIDE `presumptive can vote` IS
  CONSIDER age
    WHEN NOTHING  -> JUST (`can vote` 18)   -- Use TYPICALLY default
    WHEN (JUST a) -> JUST (`can vote` a)    -- Use explicit value

-- Then you could call with explicit control:
#PEVAL `presumptive can vote` NOTHING       -- Use default: TRUE
#PEVAL `presumptive can vote` (JUST 25)     -- Explicit age: TRUE
#PEVAL `presumptive can vote` (JUST 15)     -- Explicit age: FALSE
```

With directive rewriting enabled, the CLI now prints the `Maybe` wrapper explicitly:

```
#PEVAL `can vote`
-- JUST TRUE  (default age = 18)

#PEVAL `presumptive can vote` (NOTHING)
-- JUST TRUE  (default age = 18)

#PEVAL `presumptive can vote` (JUST 15)
-- JUST FALSE
```

**Why the explicit approach is better:**

- Fine-grained control: choose which parameters use defaults
- Composable: can pass MAYBE values through multiple functions
- Auditable: explicit about which defaults are being used
- Type-safe: the Maybe wrapper makes optionality explicit in the type system

**Runtime Semantics: Unknown Propagation**

When a wrapper receives `NOTHING` for a parameter:

- **Has TYPICALLY:** Use the default value
- **No TYPICALLY:** Return `NOTHING` (Unknown), which propagates through computation

This enables graceful handling in interactive applications:

```l4
GIVEN
  `has spousal approval` IS A BOOLEAN TYPICALLY FALSE
  `beer only` IS A BOOLEAN  -- No TYPICALLY!
  `has parental approval` IS A BOOLEAN TYPICALLY FALSE
GIVETH A BOOLEAN
DECIDE `may purchase alcohol` ...

-- Returns TRUE (beer only = TRUE, others use defaults)
#PEVAL `may purchase alcohol` (JUST TRUE) NOTHING NOTHING

-- Returns NOTHING (Unknown) because beer only has no default
#PEVAL `may purchase alcohol` NOTHING NOTHING (JUST FALSE)
```

**UI can detect Unknown and prompt:**

```
Result: Unknown
Missing inputs without defaults:
  - beer only (required, no default value)

Would you like to provide a value?
```

This is **not a compile-time error** - it's a runtime behavior that allows graceful degradation.

**Implementation Gap & Architectural Challenge:**

The wrapper generation approach requires:

1. Automatically generating `presumptive <fn>` wrappers for each DECIDE with TYPICALLY defaults
2. Wrappers take `MAYBE T` parameters instead of `T`
3. Wrappers unwrap NOTHING → use TYPICALLY default, JUST v → use v
4. PEVAL would call the wrapper, not the original function

**Key Architectural Decision:**

There are three approaches to wrapper generation:

**Option A: Runtime Generation (Current WIP)**

- Generate wrappers during `evalDecide` (evaluation phase)
- Store in a special runtime map
- Challenge: Wrappers aren't available during type checking/name resolution
- Users can't reference `presumptive foo` in their L4 code directly
- Would need special lookup mechanism in PEVAL

**Option B: Compile-Time Generation (Type Checking)**

- Generate wrapper DECIDE statements during type checking
- Add to AST as synthetic declarations
- Wrappers become regular functions accessible by name
- Benefit: Full integration with type system and name resolution
- Challenge: Requires AST transformation pass during type checking

**Option C: Hybrid - Manual Wrapper Pattern**

- Don't auto-generate; provide tools for users to write wrappers
- Document the pattern in spec
- Users write: `DECIDE 'presumptive foo' ...` manually
- Simpler implementation, more explicit

**Decision: Implementing Option B (Compile-Time Generation)**

Chosen approach: Generate presumptive wrappers during type checking as synthetic DECIDE statements.

**Critical Requirement: Transitive Presumptive Propagation**

When a presumptive wrapper calls another function, it MUST call the presumptive version (if it exists). This ensures TYPICALLY defaults apply at every level of the call stack.

Example:

```l4
GIVEN age IS A NUMBER TYPICALLY 18
GIVETH A BOOLEAN
DECIDE `can vote` IF age >= 18

GIVEN age IS A NUMBER TYPICALLY 18
GIVETH A BOOLEAN
DECIDE `can drive` IF `can vote` age AND age >= 16
```

Generated `'presumptive can drive'` must call `'presumptive can vote'`, not regular `'can vote'`:

```l4
DECIDE `presumptive can drive` IS
  GIVEN age IS A MAYBE NUMBER
  GIVETH A MAYBE BOOLEAN
  CONSIDER age
    WHEN NOTHING ->
      -- Use default, call presumptive version
      (`presumptive can vote` (JUST 18)) AND (18 >= 16)
    WHEN (JUST a) ->
      -- Explicit value, call presumptive version
      (`presumptive can vote` (JUST a)) AND (a >= 16)
```

> **Status:** Wrapper bodies are currently left untouched—the generated helpers still invoke the original functions. The transitive rewrite will be re-enabled once the wrapper invocation path (argument injection + result handling) is stable and protected by regression tests.

**Implementation: Two-Pass Transformation**

**Pass 1: Generate Wrappers**

1. Scan all DECIDE statements in the section
2. For each with TYPICALLY defaults in GIVEN:
   - Create `'presumptive <name>'` DECIDE
   - Generate GIVEN with MAYBE-wrapped parameters
   - Generate skeleton body (placeholder)
3. Build map: `originalName -> presumptiveName`

**Pass 2: Rewrite Function Calls**

1. For each generated wrapper:
   - Traverse expression tree in wrapper body
   - Find all App nodes (function applications)
   - If called function has presumptive version:
     - Rewrite: `foo arg` → `presumptive foo (JUST arg)`
     - Ensures transitive propagation of presumptive context
2. This ensures defaults apply at ALL call levels

**Current Status (Jan 2026):**

- The type checker generates `'presumptive …'` DECIDEs for every function whose GIVEN clause includes TYPICALLY defaults (parameters are `MAYBE`-wrapped and the return type becomes `MAYBE <original>`), so downstream APIs can opt in to the prefix calling convention.
- Wrapper bodies are presently left untouched—the generated helpers invoke the original definitions. Until the transitive rewrite pass lands, calling `'presumptive outer'` does **not** automatically lift the `inner` calls inside `outer`; only the arguments supplied through the wrapper participate in the presumptive contract.
- `inferSection` now rewrites presumptive directives to call the wrappers. Each missing argument is padded with `NOTHING`, explicit arguments become `JUST <expr>`, and CLI output surfaces the resulting `Maybe`. `#PASSERT` unwraps the `Maybe` and treats `NOTHING` as a failing assertion.
- Runtime auto-defaults remain as a fallback for legacy expressions, while the Decision Service and CLI share the same prefix-only wrapper contract. Migrating to the four-state `InputState` model is the next milestone once the API exposes the required metadata.

**Implementation Options:**

1. **Option A: Compile-time wrapper generation**

   - In `evalDecide`, for each function with TYPICALLY defaults, generate a "presumptive" version
   - The presumptive version wraps params in Maybe types
   - Body unwraps: `Just v → v`, `Nothing → TYPICALLY default or Unknown`
   - Store with special name (e.g., prefix "presumptive ")
   - PEVAL looks up and calls the presumptive version

2. **Option B: Runtime transformation**
   - When PEVAL evaluates a function application with `Nothing` arguments
   - Check if the corresponding param has a TYPICALLY default
   - Substitute the default value for `Nothing`
   - Requires threading the `presumptive` flag through function application

**Current Implementation Files:**

- `jl4-core/src/L4/Lexer.hs:56-76` - `TPresumptiveEvalDirective`, `TPresumptiveEvalTraceDirective`, `TPresumptiveAssertDirective` tokens
- `jl4-core/src/L4/Syntax.hs:163-168` - `PresumptiveEval`, `PresumptiveEvalTrace`, `PresumptiveAssert` AST constructors
- `jl4-core/src/L4/Parser.hs:467-492` - Parser cases for PEVAL/PEVALTRACE/PASSERT
- `jl4-core/src/L4/Print.hs:198-200` - Pretty-printing for `#PEVAL`, `#PEVALTRACE`
- `jl4-core/src/L4/EvaluateLazy/Machine.hs:1618-1634` - `evalDirective` handlers setting `presumptive=True`
- `jl4-core/src/L4/EvaluateLazy/Machine.hs:797-802` - `extractTypicallyDefaults` function

**Implementation Status:**

✅ **Working (Dec 2025):**

- Parsing + type checking of TYPICALLY clauses in DECLARE / GIVEN / ASSUME
- Compile-time generation of `'presumptive …'` wrappers with `MAYBE` parameters and return types (for Decision Service + future API clients)
- Mixfix resolver skips any identifier that already starts with `'presumptive ` so wrappers stay in canonical prefix form
- Runtime `maybeApplyDefaults` auto-applies defaults for CLI directives when evaluation yields a closure
- Test file `jl4/examples/ok/peval-test.l4` exercises the runtime auto-default path

🚧 **Paused:**

- Directive rewriting to call wrappers (gated by `enablePresumptiveDirectiveRewriting`)
- Wrapper-body rewriting / transitive `'presumptive …'` propagation

⏳ **Next:** Move the decision-service input model from `Maybe` to the four-state `InputState` (per `doc/todo/RUNTIME-INPUT-STATE-SPEC.md`) so presumptions, explicit refusals, and unknown answers are distinguishable end-to-end.

**Test File:** `jl4/examples/ok/peval-test.l4` (uses current auto-apply approach)

## Test Cases

### Unit Tests: Parser

```haskell
describe "TYPICALLY parsing" $ do
  it "parses TYPICALLY TRUE in DECLARE" $ do
    parse "DECLARE Foo HAS x IS A BOOLEAN TYPICALLY TRUE"
      `shouldParseTo` Declare ... (Just (Lit True))

  it "parses TYPICALLY FALSE in GIVEN" $ do
    parse "GIVEN x IS A BOOLEAN TYPICALLY FALSE GIVETH ..."
      `shouldParseTo` ...

  it "parses TYPICALLY with numbers" $ do
    parse "DECLARE Foo HAS age IS A NUMBER TYPICALLY 18"
      `shouldParseTo` ...

  it "parses TYPICALLY with strings" $ do
    parse "ASSUME jurisdiction IS A STRING TYPICALLY \"Singapore\""
      `shouldParseTo` ...

  it "rejects TYPICALLY with non-literals" $ do
    parse "DECLARE Foo HAS x IS A BOOLEAN TYPICALLY (a AND b)"
      `shouldFailWith` "TYPICALLY value must be a literal"
```

### Unit Tests: Type Checking

```haskell
describe "TYPICALLY type checking" $ do
  it "accepts matching types" $ do
    typeCheck "DECLARE Foo HAS x IS A BOOLEAN TYPICALLY TRUE"
      `shouldSucceed`

  it "rejects mismatched types" $ do
    typeCheck "DECLARE Foo HAS x IS A BOOLEAN TYPICALLY 42"
      `shouldFailWith` "Expected BOOLEAN, got NUMBER"

  it "rejects non-literal expressions" $ do
    typeCheck "GIVEN x IS A BOOLEAN TYPICALLY (y AND z) ..."
      `shouldFailWith` "TYPICALLY value must be a literal"
```

### Unit Tests: AST Extraction

```haskell
describe "TYPICALLY in AST" $ do
  it "extracts defaults from parsed function" $ do
    let ast = parse [l4|
      GIVEN
        x IS A BOOLEAN TYPICALLY TRUE
        y IS A BOOLEAN TYPICALLY FALSE
        z IS A NUMBER  -- no default
      GIVETH A BOOLEAN
      DECIDE foo IF x AND NOT y
      |]
    getDefaults ast `shouldBe` Map.fromList
      [ ("x", Lit True)
      , ("y", Lit False)
      -- z not present (no TYPICALLY)
      ]

  it "extracts defaults from DECLARE" $ do
    let ast = parse [l4|
      DECLARE Person HAS
        has_capacity IS A BOOLEAN TYPICALLY TRUE
        name IS A STRING  -- no default
      |]
    getFieldDefaults ast `shouldBe` Map.fromList
      [ ("has_capacity", Lit True)
      ]
```

### Golden Tests: Pretty Printing

```haskell
describe "TYPICALLY pretty printing" $ do
  it "round-trips TYPICALLY in GIVEN" $
    goldenPretty "typically_given" [l4|
      GIVEN x IS A BOOLEAN TYPICALLY TRUE
      GIVETH A BOOLEAN
      DECIDE foo IF x
      |]

  it "round-trips TYPICALLY in DECLARE" $
    goldenPretty "typically_declare" [l4|
      DECLARE Person HAS
        has_capacity IS A BOOLEAN TYPICALLY TRUE
        is_under_duress IS A BOOLEAN TYPICALLY FALSE
      |]
```

**Note:** Runtime behavior tests (evaluation with defaults, decision service integration) belong in `RUNTIME-INPUT-STATE-SPEC.md`.

## Edge Cases

### 1. TYPICALLY on Optional Fields

What if a field is `MAYBE T` with TYPICALLY?

```l4
DECLARE Contract HAS
  arbitration_clause IS A MAYBE STRING TYPICALLY NOTHING
```

This means "by default, no arbitration clause" - the TYPICALLY value is `NOTHING`, not a default string.

### 2. TYPICALLY with Complex Types

TYPICALLY should only accept literals, not complex expressions:

```l4
-- OK: literal
DECLARE Foo HAS x IS A NUMBER TYPICALLY 42

-- ERROR: not a literal
DECLARE Foo HAS x IS A NUMBER TYPICALLY (40 + 2)

-- ERROR: not a literal
DECLARE Foo HAS items IS A LIST OF NUMBER TYPICALLY [1, 2, 3]
```

For complex defaults, use a separate definition:

```l4
defaultItems MEANS [1, 2, 3]

GIVEN items IS A LIST OF NUMBER  -- no TYPICALLY, handled in logic
DECIDE foo IF ...
  WHERE items' = IF items == [] THEN defaultItems ELSE items
```

### 3. Nested Record Defaults

When a record field has a record type with its own defaults:

```l4
DECLARE Address HAS
  country IS A STRING TYPICALLY "Singapore"

DECLARE Person HAS
  name IS A STRING
  address IS AN Address  -- No TYPICALLY on the whole address
```

If `Person.address` is provided but `Person.address.country` is omitted, should country default to "Singapore"?

**Recommendation:** Yes, defaults apply at each level independently.

### 4. TYPICALLY and Partial Evaluation

TYPICALLY integrates with Boolean Minimization (see `BOOLEAN-MINIMIZATION-SPEC.md`) and Runtime Input State (see `RUNTIME-INPUT-STATE-SPEC.md`):

- With TYPICALLY, omitted fields can resolve to default values
- This reduces the number of questions the chatbot needs to ask
- The runtime state model distinguishes "not asked" from "I don't know"

The compile-time TYPICALLY value is **metadata**. How it's used at runtime depends on the application's input state model.

### 5. Conflicting Defaults

What if DECLARE and GIVEN both specify TYPICALLY for the same concept?

```l4
DECLARE Person HAS
  age IS A NUMBER TYPICALLY 30

GIVEN p IS A Person
      age IS A NUMBER TYPICALLY 21  -- Different default!
DECIDE ...
```

**Rule:** The innermost (most specific) TYPICALLY wins. Here, the GIVEN's TYPICALLY 21 overrides the DECLARE's TYPICALLY 30 for this function.

## Migration Path

### Backward Compatibility

TYPICALLY is purely additive - existing L4 code without TYPICALLY continues to work exactly as before.

### Incremental Adoption

Teams can add TYPICALLY to their codebase incrementally:

1. Start with most commonly assumed values
2. Add TYPICALLY to ASSUME declarations for external facts
3. Add TYPICALLY to GIVEN for function parameters
4. Add TYPICALLY to DECLARE for record types

## Future Work

### Arbitrary Expressions as Defaults

The current implementation restricts TYPICALLY values to **literals only** (TRUE, FALSE, numbers, strings). In a future iteration, we want to lift this restriction and allow **arbitrary value expressions** that may require function evaluation:

```l4
-- Future: computed defaults
DECLARE Person HAS
  birth_year IS A NUMBER
  age IS A NUMBER TYPICALLY (currentYear - birth_year)

-- Future: function call as default
GIVEN
  tax_rate IS A NUMBER TYPICALLY (lookupTaxRate jurisdiction)

-- Future: conditional defaults
DECLARE Contract HAS
  governing_law IS A STRING TYPICALLY
    IF jurisdiction == "US" THEN "Delaware"
    ELSE "Singapore"
```

**Implications:**

1. **Evaluation Required:** Non-literal defaults must be evaluated at runtime, not just stored as metadata
2. **Dependency Ordering:** Default expressions may reference other fields/parameters, requiring careful evaluation order
3. **Circular Dependencies:** Must detect and reject circular default dependencies
4. **Performance:** Complex defaults may have runtime cost; consider caching or lazy evaluation
5. **Error Handling:** Default evaluation can fail (e.g., `lookupTaxRate` throws); need error semantics

This is deferred to a future iteration to keep the initial implementation simple and well-tested.

## Related Work

- **Prolog's Closed World Assumption:** Unknown facts are assumed false
- **SQL DEFAULT:** Column defaults in database schemas
- **TypeScript's Optional with Default:** `function foo(x: number = 42)`
- **Haskell's Default class:** Type class for default values
- **Legal Presumptions:** Rebuttable presumptions in evidence law

## References

- `doc/default-values.md`: Original conceptual design
- `RUNTIME-INPUT-STATE-SPEC.md`: Four-state runtime model for interactive applications
- `BOOLEAN-MINIMIZATION-SPEC.md`: Integration with partial evaluation
- [Negation as Failure](https://en.wikipedia.org/wiki/Negation_as_failure)
- [Closed-world assumption](https://en.wikipedia.org/wiki/Closed-world_assumption)
- [Default Logic](https://en.wikipedia.org/wiki/Default_logic)
