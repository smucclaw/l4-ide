# Specification: Runtime Input State Model

**Status:** 📋 Draft
**Related:** `TYPICALLY-DEFAULTS-SPEC.md` (compile-time defaults), `BOOLEAN-MINIMIZATION-SPEC.md` (partial evaluation), `doc/default-values.md` (conceptual background)

## Implementation Progress

| Phase | Description                                         | Status                                                                             |
| ----- | --------------------------------------------------- | ---------------------------------------------------------------------------------- |
| 1     | TYPICALLY syntax (lexer, parser, AST)               | ✅ Complete (see TYPICALLY-DEFAULTS-SPEC.md)                                       |
| 2     | TYPICALLY type checking                             | ✅ Complete                                                                        |
| 3     | Strict directive variants (#EVALSTRICT, etc.)       | ✅ Parsed, ⏳ No runtime difference yet                                            |
| 4     | TYPICALLY on ASSUME is error                        | ✅ Complete                                                                        |
| 5     | Extract TYPICALLY defaults from AST                 | ✅ Complete (`extractTypicallyDefaults` in Machine.hs)                             |
| 6     | Decision Service API defaultMode parameter          | ⏳ In progress (`DefaultMode` type added, `FnArguments.fnDefaultMode` field added) |
| 7     | Decision Service wrapper generation                 | ⏳ Not started                                                                     |
| 8     | Decision Service uses defaults based on defaultMode | ⏳ Not started                                                                     |

> **Current CLI status:** The presumptive wrappers still expose `JUST`/`NOTHING`, and the CLI now surfaces those `Maybe` results after rewriting directives. The four-state model described below remains the target for the Decision Service.
> **Current State:** The evaluator exports `extractTypicallyDefaults` to extract TYPICALLY defaults from a `GivenSig`. The core evaluator requires all arguments when calling functions - it does NOT apply TYPICALLY defaults internally. Default handling is delegated to the Decision Service API layer.

**Architecture Decision:** TYPICALLY defaults are applied at the **API layer** (Decision Service), not in the core evaluator. The evaluator always receives concrete values or produces `ValAssumed` for unknowns. The Decision Service translates the four-state input model into evaluator inputs based on `defaultMode`.

**Test files:**

- `jl4/examples/ok/typically-basic.l4` - TYPICALLY syntax
- `jl4/examples/ok/evalstrict.l4` - strict directive parsing
- `jl4/examples/not-ok/tc/typically-type-mismatch.l4` - type mismatch error
- `jl4/examples/not-ok/tc/typically-on-assume.l4` - TYPICALLY on ASSUME error

---

## Implementation Roadmap

This section provides detailed implementation guidance for the remaining phases.

### Phase 5: Extract TYPICALLY Defaults from AST ✅

**Status:** Complete

**Implementation:** `extractTypicallyDefaults` function added to `L4/EvaluateLazy/Machine.hs`:

```haskell
-- | Extract TYPICALLY defaults from a GivenSig
-- Returns a map from parameter Unique to the default expression
extractTypicallyDefaults :: GivenSig Resolved -> Map Unique (Expr Resolved)
extractTypicallyDefaults (MkGivenSig _ann otns) =
  Map.fromList
    [ (getUnique n, expr)
    | MkOptionallyTypedName _ann n _mty (Just expr) <- otns
    ]
```

This function is exported from `L4.EvaluateLazy.Machine` for use by wrapper generators.

---

### Phase 6: Wrapper-Based Default Handling (Chosen Approach)

**Goal:** Generate wrapper functions that handle default substitution at the L4 level, keeping the core evaluator unchanged.

> **DEPRECATED:** The earlier approach of threading `strictMode` through the evaluator has been abandoned in favor of this wrapper-based approach. The core evaluator remains strict (all arguments required) and does not distinguish between `#EVAL` and `#EVALSTRICT` at runtime.

**Architecture:**

```
┌─────────────────────────────────────────────────────────────────────────┐
│                         Input Sources                                    │
├─────────────────────────────────────────────────────────────────────────┤
│  Decision Service API    │    LSP/IDE    │    Direct L4 Code            │
│  { age: 20 }             │    hover/eval │    #EVAL foo 20              │
└──────────┬───────────────┴───────┬───────┴────────────┬─────────────────┘
           │                       │                    │
           ▼                       ▼                    ▼
┌─────────────────────────────────────────────────────────────────────────┐
│                    Wrapper Generator (Compiler/API)                      │
│  - Uses extractTypicallyDefaults to get defaults                        │
│  - Generates wrapper that accepts Maybe inputs                          │
│  - In honor-defaults mode: substitutes TYPICALLY for Nothing            │
│  - In ignore-defaults mode: passes Nothing through as Unknown           │
└──────────────────────────────────┬──────────────────────────────────────┘
                                   │
                                   ▼
┌─────────────────────────────────────────────────────────────────────────┐
│                         Core Evaluator                                   │
│  - Unchanged: requires all arguments                                    │
│  - Receives concrete values or ValAssumed                               │
│  - No knowledge of "defaultMode" or "strictness"                        │
└─────────────────────────────────────────────────────────────────────────┘
```

**Wrapper Pattern:**

For a function with TYPICALLY defaults:

```l4
-- Original function
GIVEN
  age IS A NUMBER
  married IS A BOOLEAN TYPICALLY FALSE
GIVETH A BOOLEAN
DECIDE `can marry` IF age >= 18 AND NOT married
```

**Naming Convention:**

Wrappers use a consistent prefix naming pattern:

- `presumptive <fn>` - honors TYPICALLY defaults (default behavior)
- `strict <fn>` - ignores defaults, treats all missing inputs as Unknown

This naming reflects legal terminology where "presumptive" means "based on assumed facts unless proven otherwise".

**Honor-defaults wrapper** (generated):

```l4
GIVEN
  age IS A Maybe NUMBER
  married IS A Maybe BOOLEAN
GIVETH A Maybe BOOLEAN
DECIDE `presumptive can marry` IS
  CONSIDER (age, married)
    WHEN (Just a, Just m)  -> Just (`can marry` a m)
    WHEN (Just a, Nothing) -> Just (`can marry` a FALSE)  -- TYPICALLY FALSE
    WHEN (Nothing, _)      -> Nothing                     -- Required input missing
```

**Ignore-defaults wrapper** (strict mode):

```l4
GIVEN
  age IS A Maybe NUMBER
  married IS A Maybe BOOLEAN
GIVETH A Maybe BOOLEAN
DECIDE `strict can marry` IS
  CONSIDER (age, married)
    WHEN (Just a, Just m)  -> Just (`can marry` a m)
    WHEN _                 -> Nothing  -- Any missing input = Unknown
```

**Key Benefits:**

1. **Core evaluator unchanged** - No threading of strict mode through evaluation
2. **Defaults are explicit** - The wrapper makes default substitution visible in generated code
3. **Composable** - Different wrappers for different behaviors
4. **Auditable** - Easy to see which defaults were applied by examining wrapper logic
5. **Works everywhere** - Same pattern for Decision Service, LSP, and userland code

**Implementation Notes:**

- The compiler can generate these wrappers automatically for exported functions
- The Decision Service uses wrapper generation (already does similar code generation)
- The LSP can invoke the appropriate wrapper based on user preference
- Users can write their own wrappers for custom default behavior

---

### Phase 7: Four-State Input Model

**Goal:** Extend the input model to distinguish all four states from the spec.

The `Maybe a` type only gives us two states. For the full four-state model:

```haskell
-- | Runtime state for a single input parameter
data InputState a
  = Explicit a          -- User provided a concrete value
  | ExplicitUnknown     -- User explicitly said "I don't know"
  | NotProvided         -- No input yet (may use TYPICALLY default)
  | NotApplicable       -- Question doesn't apply in this context
  deriving (Eq, Show, Functor)
```

**Wrapper with four states:**

```l4
GIVEN
  age IS A InputState NUMBER
  married IS A InputState BOOLEAN
GIVETH A BOOLEAN
DECIDE `can marry with full state` IS
  CONSIDER (age, married)
    WHEN (Explicit a, Explicit m)     -> `can marry` a m
    WHEN (Explicit a, NotProvided)    -> `can marry` a FALSE  -- Use TYPICALLY
    WHEN (Explicit a, ExplicitUnknown)-> UNKNOWN              -- User said "I don't know"
    WHEN (NotProvided, _)             -> UNKNOWN              -- Required input missing
    WHEN (NotApplicable, _)           -> ...                  -- Handle as appropriate
```

**Key distinction:**

- `NotProvided` + has TYPICALLY → use the default
- `ExplicitUnknown` → stay Unknown even if TYPICALLY exists (user explicitly doesn't know)

---

### Phase 8: Decision Service API Integration

**Goal:** The Decision Service uses the wrapper pattern with `defaultMode` parameter.

**API Request:**

```json
{
  "function": "can marry",
  "defaultMode": "honor-defaults", // REQUIRED: "honor-defaults" or "ignore-defaults"
  "fnArguments": {
    "age": 20
    // "married" omitted = NotProvided
  },
  "explicitUnknowns": [] // Optional: parameters user said "I don't know" for
}
```

**API Behavior:**

| defaultMode     | Input State                   | Behavior                              |
| --------------- | ----------------------------- | ------------------------------------- |
| honor-defaults  | `NotProvided` + has TYPICALLY | Use TYPICALLY default                 |
| honor-defaults  | `NotProvided` + no TYPICALLY  | Treat as Unknown                      |
| honor-defaults  | `ExplicitUnknown`             | Treat as Unknown (don't use default)  |
| ignore-defaults | `NotProvided`                 | Treat as Unknown (never use defaults) |
| ignore-defaults | `ExplicitUnknown`             | Treat as Unknown                      |

**Implementation:**

The Decision Service:

1. Extracts TYPICALLY defaults using `extractTypicallyDefaults`
2. Based on `defaultMode`, generates or selects the appropriate wrapper
3. Translates API inputs to `InputState` values
4. Calls the wrapper with `InputState` arguments
5. Returns result with audit trail of which defaults were used

**Response:**

```json
{
  "result": true,
  "inputResolution": {
    "age": { "value": 20, "source": "explicit" },
    "married": { "value": false, "source": "typically-default" }
  },
  "usedDefaults": { "married": false }
}
```

---

## Deprecated Approaches

The following approaches were considered but **deprecated** in favor of the wrapper-based approach:

### ❌ Evaluator-Level Strict Mode (Deprecated)

Threading `strictMode :: Bool` through the evaluator and having `#EVAL` vs `#EVALSTRICT` produce different runtime behavior was rejected because:

1. Adds complexity to the core evaluator
2. Makes default behavior implicit/hidden
3. Harder to audit which defaults were applied
4. Requires changes throughout the evaluation pipeline

### ❌ TYPICALLY on ASSUME (Deprecated)

Originally, TYPICALLY could appear on ASSUME declarations. This was removed because:

1. ASSUME is being deprecated
2. Confusing semantics (ASSUME suggests a value, TYPICALLY suggests a default)
3. TYPICALLY on GIVEN parameters in DECIDE is clearer

---

## Note on ASSUME Keyword Deprecation

**Background:** The `ASSUME` keyword was originally introduced as a placeholder for explicit type declarations—essentially a visible delta between type inference and type checking. When the type checker inferred types for otherwise undefined expressions, the LSP would offer an automatic `ASSUME` reification.

**The Problem:** The keyword "ASSUME" is misleading to non-programmers. To a layperson, `ASSUME x IS A BOOLEAN` might suggest not just that `x` is typed as a boolean, but that `x` is _valued_ `TRUE`! This semantic confusion is problematic for a language designed to be accessible to legal professionals.

**Long-term Direction:** We intend to deprecate explicit use of `ASSUME` in well-styled L4 programs. Alternative approaches under consideration:

- Using `GIVEN` for function parameters (already standard)
- Using `DECLARE` for module-level type declarations
- Implicit type inference without visible ASSUME artifacts
- A less ambiguous keyword if explicit declaration is needed

**For this spec:** The runtime input state model applies to whatever mechanism provides external inputs to decisions—whether that's `ASSUME`, `GIVEN`, or a future alternative. The core four-state model (explicit value, explicit unknown, not yet asked, not applicable) is independent of the surface syntax.

## Executive Summary

Interactive applications (chatbots, web forms, decision services) need to track **four distinct states** for each input parameter:

1. **Explicit value** - User provided a concrete answer
2. **Explicit unknown** - User said "I don't know"
3. **Not yet asked** - No user input yet (may use TYPICALLY default)
4. **Not applicable** - Question doesn't apply in this context

This spec defines the `Either (Maybe a) (Maybe a)` runtime representation and how it integrates with TYPICALLY defaults from compile-time.

## Motivation

### The Problem

Consider a chatbot collecting information for an alcohol purchase decision:

```
Chatbot: Are you married?
```

The user might:

1. Say "Yes" → we know they're married
2. Say "No" → we know they're unmarried
3. Say "I don't know" / "I'd rather not say" → explicit uncertainty
4. Not answer yet (chatbot hasn't asked) → no input

These are **four different states**, but a simple `Maybe Bool` only gives us two (value or nothing).

### Why It Matters

| State                    | With `Maybe Bool` | What Should Happen                         |
| ------------------------ | ----------------- | ------------------------------------------ |
| User says "Yes"          | `Just True` ✓     | Use `True`                                 |
| User says "No"           | `Just False` ✓    | Use `False`                                |
| User says "I don't know" | `Nothing` ✗       | Propagate as Unknown in three-valued logic |
| Not yet asked            | `Nothing` ✗       | Use TYPICALLY default if available         |

With `Maybe Bool`, we can't distinguish "I don't know" from "not asked". This matters because:

- "Not asked" → use TYPICALLY default → may resolve the decision
- "I don't know" → truly Unknown → may require asking other questions

## The Four-State Model

### Type Definition

```haskell
-- | Runtime state for a single input parameter
--
-- Left branch: No user input yet
--   Left Nothing      = No user input, no compile-time default
--   Left (Just v)     = No user input, has TYPICALLY default of v
--
-- Right branch: User has provided input
--   Right Nothing     = User explicitly said "I don't know"
--   Right (Just v)    = User explicitly provided value v
--
type WithDefault a = Either (Maybe a) (Maybe a)
```

### State Diagram

```
                    ┌─────────────────────────────────────┐
                    │         WithDefault a               │
                    └─────────────────────────────────────┘
                                    │
                    ┌───────────────┴───────────────┐
                    │                               │
              Left (Maybe a)                  Right (Maybe a)
           "No user input yet"              "User has responded"
                    │                               │
            ┌───────┴───────┐               ┌───────┴───────┐
            │               │               │               │
      Left Nothing    Left (Just v)   Right Nothing   Right (Just v)
      "No default"    "TYPICALLY v"   "I don't know"  "Explicit v"
```

### State Transitions

```
Initial state (from L4 compile):
  - Has TYPICALLY → Left (Just defaultValue)
  - No TYPICALLY  → Left Nothing

User provides input:
  - User says value v    → Right (Just v)
  - User says "I don't know" → Right Nothing

User clears input (resets to default):
  - Back to Left branch with original default
```

### Resolution to Three-Valued Logic

When evaluating, resolve `WithDefault Bool` to `TriBool`:

```haskell
resolve :: WithDefault Bool -> TriBool
resolve (Right (Just True))  = TTrue      -- User said true
resolve (Right (Just False)) = TFalse     -- User said false
resolve (Right Nothing)      = TUnknown   -- User said "I don't know"
resolve (Left (Just True))   = TTrue      -- Using TYPICALLY TRUE
resolve (Left (Just False))  = TFalse     -- Using TYPICALLY FALSE
resolve (Left Nothing)       = TUnknown   -- No input, no default
```

## Integration with TYPICALLY

### Compile-Time → Runtime

When loading a function for interactive use:

```haskell
-- From TYPICALLY-DEFAULTS-SPEC: compile-time defaults
getDefaults :: Decide Resolved -> Map Text (Expr Resolved)

-- Initialize runtime state from compile-time defaults
initializeInputState :: Map Text (Expr Resolved) -> Map Text (WithDefault Value)
initializeInputState defaults = Map.mapWithKey toInitialState allParams
  where
    toInitialState name _ = case Map.lookup name defaults of
      Just expr -> Left (Just (evalLiteral expr))  -- Has TYPICALLY
      Nothing   -> Left Nothing                     -- No TYPICALLY
```

### Example

```l4
GIVEN
  age IS A NUMBER
  married IS A BOOLEAN TYPICALLY FALSE
  has_approval IS A BOOLEAN
GIVETH A BOOLEAN
DECIDE foo IF ...
```

Initial runtime state:

```haskell
{ "age"          -> Left Nothing        -- No default, not asked
, "married"      -> Left (Just False)   -- TYPICALLY FALSE, not asked
, "has_approval" -> Left Nothing        -- No default, not asked
}
```

After user says age=30:

```haskell
{ "age"          -> Right (Just 30)     -- User provided
, "married"      -> Left (Just False)   -- Still using default
, "has_approval" -> Left Nothing        -- Still not asked
}
```

After user says "I don't know" for married:

```haskell
{ "age"          -> Right (Just 30)     -- User provided
, "married"      -> Right Nothing       -- User said "I don't know"
, "has_approval" -> Left Nothing        -- Still not asked
}
```

## API Design

### Required: Explicit Default Handling Mode

**Critical:** Runtime evaluators (decision service API, ladder diagram visualizer, chatbots) **MUST** operate in one of two explicit modes:

| Mode                | Behavior                                             | Use Case                                              |
| ------------------- | ---------------------------------------------------- | ----------------------------------------------------- |
| **honor-defaults**  | Use TYPICALLY values when input not provided         | Production chatbots, quick evaluations                |
| **ignore-defaults** | Treat missing inputs as Unknown, never use TYPICALLY | Formal verification, audit trails, "what-if" analysis |

**Why this matters:**

```l4
GIVEN married IS A BOOLEAN TYPICALLY FALSE
GIVETH A BOOLEAN
DECIDE `may purchase alcohol` IF age >= 21 OR (age >= 18 AND NOT married)
```

- **honor-defaults mode**: User age 19, doesn't answer married question → uses `married = FALSE` → returns `TRUE`
- **ignore-defaults mode**: User age 19, doesn't answer married question → `married = Unknown` → returns `Unknown`

These are **different answers to the same query**. The API must make the mode explicit—never implicitly choose one.

**API request example:**

```json
{
  "function": "may purchase alcohol",
  "defaultMode": "honor-defaults", // REQUIRED field
  "fnArguments": {
    "age": 19
  }
}
```

**Ladder Diagram Visualizer:**

Similarly, the ladder diagram must operate in:

- **default-aware mode**: Shows simplified tree assuming defaults apply
- **default-blind mode**: Shows full tree with all possible branches

This affects which questions appear "relevant" in the visualization.

### Decision Service Request

```json
{
  "fnArguments": {
    "age": 30,
    "married": null,
    "has_approval": { "_notProvided": true }
  }
}
```

Interpretation:

- `"age": 30` → `Right (Just 30)` - explicit value
- `"married": null` → `Right Nothing` - explicit "I don't know"
- `"has_approval": { "_notProvided": true }` → `Left _` - use default
- Field omitted entirely → `Left _` - use default

### Alternative: Simpler API (Recommended for MVP)

For simpler integration, use field presence:

```json
{
  "fnArguments": {
    "age": 30
  },
  "explicitUnknowns": ["married"]
}
```

- Present in `fnArguments` → `Right (Just value)`
- Present in `explicitUnknowns` → `Right Nothing`
- Absent from both → `Left _` (use TYPICALLY if available)

### Response: Tracking What Was Used

```json
{
  "result": true,
  "inputResolution": {
    "age": { "value": 30, "source": "explicit" },
    "married": { "value": false, "source": "typically", "default": false },
    "has_approval": {
      "value": null,
      "source": "unknown",
      "reason": "no input, no default"
    }
  }
}
```

## Implementation

### Core Types

```haskell
module L4.Runtime.InputState where

import Data.Map (Map)

-- | The four states for a runtime input
data InputState a
  = NotProvided (Maybe a)   -- Left branch: (Maybe default)
  | Provided (Maybe a)      -- Right branch: (Maybe explicit value)
  deriving (Eq, Show, Functor)

-- | Equivalent to Either (Maybe a) (Maybe a), but with clearer names
type WithDefault a = InputState a

-- | Smart constructors
notAsked :: InputState a
notAsked = NotProvided Nothing

withDefault :: a -> InputState a
withDefault v = NotProvided (Just v)

explicit :: a -> InputState a
explicit v = Provided (Just v)

explicitUnknown :: InputState a
explicitUnknown = Provided Nothing

-- | Check if user has provided any input
hasUserInput :: InputState a -> Bool
hasUserInput (Provided _)    = True
hasUserInput (NotProvided _) = False

-- | Get the effective value (if determinable)
effectiveValue :: InputState a -> Maybe a
effectiveValue (Provided (Just v))    = Just v   -- Explicit value
effectiveValue (Provided Nothing)     = Nothing  -- "I don't know"
effectiveValue (NotProvided (Just v)) = Just v   -- Using default
effectiveValue (NotProvided Nothing)  = Nothing  -- No input, no default

-- | Get the source of the value
data ValueSource = Explicit | Default | Unknown
  deriving (Eq, Show)

valueSource :: InputState a -> ValueSource
valueSource (Provided (Just _))    = Explicit
valueSource (Provided Nothing)     = Unknown  -- User said "I don't know"
valueSource (NotProvided (Just _)) = Default
valueSource (NotProvided Nothing)  = Unknown  -- No input, no default
```

### Integration with Three-Valued Logic

```haskell
-- | Convert InputState Bool to TriBool for evaluation
toTriBool :: InputState Bool -> TriBool
toTriBool (Provided (Just True))    = TTrue
toTriBool (Provided (Just False))   = TFalse
toTriBool (Provided Nothing)        = TUnknown  -- "I don't know"
toTriBool (NotProvided (Just True)) = TTrue     -- TYPICALLY TRUE
toTriBool (NotProvided (Just False))= TFalse    -- TYPICALLY FALSE
toTriBool (NotProvided Nothing)     = TUnknown  -- No input, no default
```

### Form/Chatbot State Management

```haskell
-- | State for an interactive session
data SessionState = SessionState
  { ssInputs    :: Map Text (InputState Value)
  , ssDefaults  :: Map Text Value  -- From TYPICALLY (immutable)
  , ssAsked     :: Set Text        -- Questions we've asked
  }

-- | Initialize from compiled function
initSession :: Decide Resolved -> SessionState
initSession decide = SessionState
  { ssInputs   = Map.map NotProvided (getDefaults decide)
  , ssDefaults = getDefaults decide
  , ssAsked    = Set.empty
  }

-- | Record user's answer
recordAnswer :: Text -> Maybe Value -> SessionState -> SessionState
recordAnswer param maybeVal ss = ss
  { ssInputs = Map.insert param (Provided maybeVal) (ssInputs ss)
  , ssAsked  = Set.insert param (ssAsked ss)
  }

-- | Reset a parameter to its default
resetToDefault :: Text -> SessionState -> SessionState
resetToDefault param ss = ss
  { ssInputs = Map.insert param
      (NotProvided (Map.lookup param (ssDefaults ss)))
      (ssInputs ss)
  }
```

## Test Cases

### Unit Tests: State Transitions

```haskell
describe "InputState" $ do
  describe "construction" $ do
    it "notAsked has no value" $
      effectiveValue (notAsked :: InputState Bool) `shouldBe` Nothing

    it "withDefault has default value" $
      effectiveValue (withDefault True) `shouldBe` Just True

    it "explicit has explicit value" $
      effectiveValue (explicit False) `shouldBe` Just False

    it "explicitUnknown has no value" $
      effectiveValue (explicitUnknown :: InputState Bool) `shouldBe` Nothing

  describe "valueSource" $ do
    it "explicit value is Explicit" $
      valueSource (explicit True) `shouldBe` Explicit

    it "default value is Default" $
      valueSource (withDefault True) `shouldBe` Default

    it "explicit unknown is Unknown" $
      valueSource (explicitUnknown :: InputState Bool) `shouldBe` Unknown

    it "not asked without default is Unknown" $
      valueSource (notAsked :: InputState Bool) `shouldBe` Unknown

  describe "hasUserInput" $ do
    it "Provided _ has user input" $ do
      hasUserInput (explicit True) `shouldBe` True
      hasUserInput explicitUnknown `shouldBe` True

    it "NotProvided _ has no user input" $ do
      hasUserInput (notAsked :: InputState Bool) `shouldBe` False
      hasUserInput (withDefault True) `shouldBe` False
```

### Unit Tests: Three-Valued Resolution

```haskell
describe "toTriBool" $ do
  it "explicit True -> TTrue" $
    toTriBool (explicit True) `shouldBe` TTrue

  it "explicit False -> TFalse" $
    toTriBool (explicit False) `shouldBe` TFalse

  it "explicit unknown -> TUnknown" $
    toTriBool explicitUnknown `shouldBe` TUnknown

  it "default True -> TTrue" $
    toTriBool (withDefault True) `shouldBe` TTrue

  it "default False -> TFalse" $
    toTriBool (withDefault False) `shouldBe` TFalse

  it "not asked, no default -> TUnknown" $
    toTriBool notAsked `shouldBe` TUnknown
```

### Integration Tests: Session Management

```haskell
describe "SessionState" $ do
  let func = parseAndCheck [l4|
    GIVEN
      x IS A BOOLEAN TYPICALLY TRUE
      y IS A BOOLEAN
    GIVETH A BOOLEAN
    DECIDE foo IF x AND y
    |]

  it "initializes with TYPICALLY defaults" $ do
    let ss = initSession func
    ssInputs ss ! "x" `shouldBe` NotProvided (Just True)
    ssInputs ss ! "y" `shouldBe` NotProvided Nothing

  it "records explicit answer" $ do
    let ss = initSession func
        ss' = recordAnswer "y" (Just (VBool False)) ss
    ssInputs ss' ! "y" `shouldBe` Provided (Just (VBool False))

  it "records 'I don't know'" $ do
    let ss = initSession func
        ss' = recordAnswer "x" Nothing ss
    ssInputs ss' ! "x" `shouldBe` Provided Nothing  -- Overrides default!

  it "resets to default" $ do
    let ss = initSession func
        ss' = recordAnswer "x" (Just (VBool False)) ss
        ss'' = resetToDefault "x" ss'
    ssInputs ss'' ! "x" `shouldBe` NotProvided (Just True)
```

### Integration Tests: API

```haskell
describe "API input parsing" $ do
  it "present value -> Provided (Just v)" $ do
    parseInput "{\"x\": true}" `shouldBe`
      Map.singleton "x" (Provided (Just True))

  it "null value -> Provided Nothing (I don't know)" $ do
    parseInput "{\"x\": null}" `shouldBe`
      Map.singleton "x" (Provided Nothing)

  it "absent field -> NotProvided" $ do
    parseInput "{}" `shouldBe` Map.empty
    -- Absent fields get NotProvided with default during resolution
```

## Edge Cases

### 1. Numeric "I Don't Know"

For non-boolean types, `null` still means "I don't know":

```json
{ "age": null }
```

_User doesn't know their age_

This resolves to `Provided Nothing` regardless of type.

### 2. Nested Records

For nested records like `Person.address.country`:

```json
{
  "person": {
    "address": {
      "country": null // "I don't know the country"
    }
  }
}
```

Each level can independently be:

- Provided with value
- Provided as unknown (null)
- Not provided (use default)

### 3. Lists and Collections

For list inputs, `null` means "I don't know if there's a list":

```json
{ "items": null }      // Don't know
{ "items": [] }        // Explicitly empty list
{ "items": [1, 2, 3] } // Explicit list
```

### 4. Resetting vs. Not Asking

UI should distinguish:

- "Clear my answer" → reset to `NotProvided` (use default)
- "I don't know" → set to `Provided Nothing`

```haskell
clearAnswer :: Text -> SessionState -> SessionState
clearAnswer = resetToDefault  -- Back to NotProvided

answerUnknown :: Text -> SessionState -> SessionState
answerUnknown param = recordAnswer param Nothing  -- Provided Nothing
```

## Relationship to Other Specs

### TYPICALLY-DEFAULTS-SPEC.md

That spec defines **compile-time** defaults. This spec defines how those defaults are **used at runtime**.

```
Compile-time (TYPICALLY):
  "married IS A BOOLEAN TYPICALLY FALSE"
       ↓
  AST: TypedName { tnTypically = Just (Lit False) }
       ↓
Runtime (this spec):
  Initial state: NotProvided (Just False)
       ↓
  After user input: Provided (Just True) or Provided Nothing
```

### BOOLEAN-MINIMIZATION-SPEC.md

Partial evaluation uses `InputState` to determine:

- Which parameters are resolved (have effective values)
- Which parameters need to be asked
- Which parameters are explicitly unknown (can't be resolved by asking)

```haskell
-- For partial evaluation
needsQuestion :: InputState a -> Bool
needsQuestion (NotProvided Nothing) = True   -- No default, not asked
needsQuestion _                     = False  -- Has value or user responded

isExplicitlyUnknown :: InputState a -> Bool
isExplicitlyUnknown (Provided Nothing) = True
isExplicitlyUnknown _                  = False
```

## References

- `doc/default-values.md`: Original conceptual design with `Either (Maybe a) (Maybe a)`
- `TYPICALLY-DEFAULTS-SPEC.md`: Compile-time TYPICALLY keyword
- `BOOLEAN-MINIMIZATION-SPEC.md`: Partial evaluation with unknowns
- [Kleene's three-valued logic](https://en.wikipedia.org/wiki/Three-valued_logic#Kleene_and_Priest_logics)
