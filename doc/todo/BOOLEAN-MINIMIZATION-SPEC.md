# Specification: Interactive Boolean Minimization for Query Relevance

**Status:** 📋 Draft
**Issue:** #638

## Executive Summary

When a user provides partial input to a boolean decision function, the system should:

1. Determine which parameters are **relevant** given the known inputs
2. Return a **prioritized list** of parameters still needed to reach a decision
3. Support **iterative refinement** as the user provides more information

This enables conversational interfaces where an LLM chatbot can ask only the questions that matter, rather than requiring all inputs upfront.

## Motivation

### The Problem

Consider a rule like:

```l4
DECIDE `one may purchase alcohol` IF
            you are of 21 years of age
       AND     you are unmarried
            OR your spouse has given approval
            OR you're only buying beer, nothing harder
  OR        you are below 21 years of age
       AND  your parent has given approval
            OR you are legally emancipated
```

Naively, the decision function requires all 6 parameters:

- age
- marital status
- spousal approval
- beverage type
- parental approval
- legally emancipated

But if the user says they're 30 years old, then `parental approval` and `legally emancipated` become **don't care** terms - they can't affect the outcome. The only relevant remaining questions are about `marital status`, `spousal approval`, and `beverage type`.

### The Goal

Enable conversational flows like:

```
Client: Can I buy booze in Atlantis?
Chatbot: It depends. How old are you?
Client: Thirty.
Chatbot: Are you married? And what kind of booze?
Client: I'm buying wine. Not married.
Chatbot: You're good to go! If you had been married, you'd need spousal approval.
```

## Background: CS Theory for Implementers

### Three-Valued Logic (Kleene Logic)

In classical boolean logic, we have two values: `True` and `False`. When dealing with partial information, we extend to **three-valued logic** with an additional value representing "unknown" or "undetermined".

**Kleene's strong logic** defines operations as:

```
AND truth table:        OR truth table:         NOT truth table:
    T   U   F               T   U   F               T → F
T   T   U   F           T   T   T   T               U → U
U   U   U   F           U   T   U   U               F → T
F   F   F   F           F   T   U   F
```

Key insight: `False AND Unknown = False` (short-circuit), but `True AND Unknown = Unknown` (need more info).

**Implementation in Haskell:**

```haskell
data TriBool = TTrue | TFalse | TUnknown
  deriving (Eq, Show)

triAnd :: TriBool -> TriBool -> TriBool
triAnd TFalse _        = TFalse   -- F ∧ x = F
triAnd _ TFalse        = TFalse   -- x ∧ F = F
triAnd TTrue TTrue     = TTrue    -- T ∧ T = T
triAnd _ _             = TUnknown -- otherwise unknown

triOr :: TriBool -> TriBool -> TriBool
triOr TTrue _          = TTrue    -- T ∨ x = T
triOr _ TTrue          = TTrue    -- x ∨ T = T
triOr TFalse TFalse    = TFalse   -- F ∨ F = F
triOr _ _              = TUnknown -- otherwise unknown

triNot :: TriBool -> TriBool
triNot TTrue    = TFalse
triNot TFalse   = TTrue
triNot TUnknown = TUnknown
```

### Binary Decision Diagrams (BDDs)

A BDD represents a boolean function as a directed acyclic graph where:

- Each internal node represents a boolean variable
- Each node has two outgoing edges (true/false)
- Terminal nodes are 0 (false) or 1 (true)

BDDs are **ordered** (OBDD) when variables appear in a fixed order on all paths. This enables efficient operations like:

- Variable elimination (cofactoring)
- Don't-care detection
- Formula simplification

#### Example: Building a BDD

Consider the formula: `(A AND B) OR C`

```
Variable order: A < B < C

        A
       / \
      /   \
     B     B
    / \   / \
   0   C 0   C
      / \   / \
     0   1 0   1

Simplified (sharing common subgraphs):

        A
       / \
      B   B
     / \ / \
    0   C   C
       / \ / \
      0   1   1

Further reduced:
        A
       / \
      B   1
     / \
    0   C
       / \
      0   1
```

Wait, let me redo this more carefully. For `(A ∧ B) ∨ C`:

```
When A=F: (F ∧ B) ∨ C = F ∨ C = C
When A=T: (T ∧ B) ∨ C = B ∨ C

        A
       / \
     (F)  (T)
      |    |
      C   B∨C
     / \
    0   1

For B∨C:
      B
     / \
    C   1
   / \
  0   1

Full BDD:
          A
         / \
        C   B
       / \ / \
      0  1 C  1
          / \
         0   1
```

#### Cofactoring (Shannon Expansion)

**Cofactoring** substitutes a variable with a constant value.

For function `f(A,B,C)`:

- `f|_{A=T}` means "f with A set to True" (positive cofactor)
- `f|_{A=F}` means "f with A set to False" (negative cofactor)

**Shannon expansion:** `f = (A ∧ f|_{A=T}) ∨ (¬A ∧ f|_{A=F})`

In BDD terms, cofactoring means following one edge from a variable node:

```haskell
-- Cofactor a BDD with respect to variable v and value b
cofactor :: BDD -> Var -> Bool -> BDD
cofactor (Leaf val) _ _ = Leaf val
cofactor (Node var lo hi) v b
  | var == v  = if b then hi else lo        -- substitute
  | var < v   = Node var (cofactor lo v b) (cofactor hi v b)  -- recurse
  | otherwise = Node var lo hi              -- v not in this subtree
```

**Example:** For `f = (A ∧ B) ∨ C`

- `f|_{A=T} = B ∨ C`
- `f|_{A=F} = C`
- `f|_{C=T} = T` (tautology!)
- `f|_{A=T, B=T} = T`

#### Don't-Care Variables

A variable `x` is a **don't-care** in function `f` if `f|_{x=T} = f|_{x=F}`.

This means the variable's value doesn't affect the outcome.

```haskell
isDontCare :: BDD -> Var -> Bool
isDontCare bdd v = cofactor bdd v True == cofactor bdd v False
```

**Example:** In `(A ∧ B) ∨ C` with `C=True`:

- After cofactoring: `f|_{C=T} = T`
- Both A and B become don't-cares (the function is constantly True)

### Variable Ordering and BDD Size

BDD size is **extremely sensitive** to variable ordering.

**Example:** `(A₁ ∧ B₁) ∨ (A₂ ∧ B₂) ∨ ... ∨ (Aₙ ∧ Bₙ)`

- Order `A₁ < B₁ < A₂ < B₂ < ...`: BDD has O(n) nodes
- Order `A₁ < A₂ < ... < B₁ < B₂ < ...`: BDD has O(2ⁿ) nodes!

**Heuristics for good orderings:**

1. Variables that appear together in clauses should be close in order
2. Variables with more "influence" (appear in more clauses) should be earlier
3. Use dynamic reordering (sifting algorithm)

### Existing Infrastructure

L4 already has:

1. **Boolean expression representation** (`L4.Syntax`)

   - `And`, `Or`, `Not`, `Implies` constructors
   - `Expr Resolved` with full type information

2. **Boolean transformations** (`L4.Transform`)

   - `simplify` - converts to CNF via NNF
   - `nnf` - negation normal form
   - `cnf` - conjunctive normal form
   - `neg` - push negation inward

3. **Partial value handling** (`Backend/Api.hs`)

   - `FnUnknown` - missing/null value
   - `FnUncertain` - explicitly uncertain

4. **Evaluation trace** (`L4.EvaluateLazy.Trace`)
   - Full execution trace with expression→value mappings
   - Could identify which variables were actually accessed

### References

- [Espresso heuristic logic minimizer](https://en.wikipedia.org/wiki/Espresso_heuristic_logic_minimizer)
- [Claude conversation on BDDs for L4](https://claude.ai/share/3c00dd49-fe52-41b2-9588-41dbf123d21a)

## Design

### API Extension

#### New Endpoint: Partial Evaluation

```
POST /functions/{name}/partial-evaluation
Content-Type: application/json

{
  "fnArguments": {
    "age": 30
    // Other fields simply omitted - not provided yet
  }
}
```

#### Handling Partial Inputs: The MAYBE Layer

**Problem:** How do we distinguish between:

1. User provided a value: `age = 30`
2. User hasn't been asked yet: field omitted
3. User explicitly says "I don't know": `age = null`?
4. User says "not applicable": different from unknown!

**Solution:** The implementation wraps all inputs in `MAYBE` internally.

**Option A: Omission = Unknown (Recommended for MVP)**

Simply omit fields that aren't known yet:

```json
// User only knows their age
{ "fnArguments": { "age": 30 } }

// After learning marital status
{ "fnArguments": { "age": 30, "married": false } }
```

The implementation treats missing fields as `NOTHING` (unknown):

```l4
-- Generated wrapper internally converts:
-- { "age": 30 }
-- into:
-- age = JUST 30, married = NOTHING, spousal_approval = NOTHING, ...
```

**Option B: Explicit Unknown Marker**

For cases where "I don't know" is different from "not asked yet":

```json
{
  "fnArguments": {
    "age": 30,
    "married": { "_unknown": true }, // User says "I don't know"
    "spousal_approval": null // Treated same as omitted
  }
}
```

**Option C: Three-State Fields**

Full explicit control:

```json
{
  "fnArguments": {
    "age": { "_value": 30 }, // Known value
    "married": { "_unknown": true }, // Explicitly unknown
    "spousal_approval": { "_notAsked": true } // Not yet queried
    // beer_only omitted = not asked
  }
}
```

**Recommendation:** Start with Option A (omission = unknown). It's simplest and matches how conversational interfaces naturally work - you only send what you've learned so far.

#### Internal Type Transformation

The partial evaluation endpoint transforms the function signature:

**Original L4:**

```l4
GIVEN
  age IS A NUMBER
  married IS A BOOLEAN
  spousal_approval IS A BOOLEAN
GIVETH A BOOLEAN
`may purchase alcohol` ...
```

**Internal transformation for partial eval:**

```l4
GIVEN
  age IS A MAYBE NUMBER
  married IS A MAYBE BOOLEAN
  spousal_approval IS A MAYBE BOOLEAN
GIVETH A PartialResult  -- Either result or required params
`may purchase alcohol partial` ...
```

This MAYBE wrapping enables:

1. Three-valued evaluation (JUST True, JUST False, NOTHING)
2. Tracking which inputs were provided vs omitted
3. Clean propagation of "unknown" through boolean operations

#### Response Schema

```typescript
interface PartialEvaluationResponse {
  // If we can determine the answer
  result?: boolean;

  // If we need more information
  required?: RequiredParameter[];

  // Explanation of current state
  reasoning?: ReasoningTree;
}

interface RequiredParameter {
  name: string;

  // Why this parameter matters
  reason: string;

  // Priority: lower = ask first
  priority: number;

  // What happens if true vs false
  impact: {
    ifTrue: PartialOutcome;
    ifFalse: PartialOutcome;
  };
}

interface PartialOutcome {
  // Can we determine the result with this value?
  determinable: boolean;

  // If determinable, what is it?
  result?: boolean;

  // If not determinable, what else do we need?
  stillRequired?: string[];
}
```

#### Example Response

For `age=30, everything else unknown`:

```json
{
  "required": [
    {
      "name": "marital_status",
      "reason": "Determines whether spousal approval is relevant",
      "priority": 1,
      "impact": {
        "ifTrue": {
          "determinable": false,
          "stillRequired": ["spousal_approval", "beverage_type"]
        },
        "ifFalse": {
          "determinable": true,
          "result": true
        }
      }
    },
    {
      "name": "beverage_type",
      "reason": "Beer exemption may apply",
      "priority": 2,
      "impact": {
        "ifTrue": {
          "determinable": true,
          "result": true
        },
        "ifFalse": {
          "determinable": false,
          "stillRequired": ["marital_status", "spousal_approval"]
        }
      }
    }
  ],
  "reasoning": { ... }
}
```

### Implementation Approaches

#### Approach 1: Symbolic Evaluation with Three-Valued Logic

Extend the evaluator to handle a third value: `Unknown`.

The MAYBE wrapper from the API layer maps naturally to three-valued logic:

- `JUST True` → `TTrue`
- `JUST False` → `TFalse`
- `NOTHING` → `TUnknown`

```haskell
data TriBool = TTrue | TFalse | TUnknown
  deriving (Eq, Show)

-- Convert from L4's MAYBE BOOLEAN
fromMaybeBool :: Maybe Bool -> TriBool
fromMaybeBool (Just True)  = TTrue
fromMaybeBool (Just False) = TFalse
fromMaybeBool Nothing      = TUnknown

-- Convert back for results
toMaybeBool :: TriBool -> Maybe Bool
toMaybeBool TTrue    = Just True
toMaybeBool TFalse   = Just False
toMaybeBool TUnknown = Nothing

evalTriBool :: Expr Resolved -> Env -> TriBool
evalTriBool (And _ e1 e2) env =
  case (evalTriBool e1 env, evalTriBool e2 env) of
    (TFalse, _)       -> TFalse      -- short-circuit
    (_, TFalse)       -> TFalse
    (TTrue, TTrue)    -> TTrue
    _                 -> TUnknown

evalTriBool (Or _ e1 e2) env =
  case (evalTriBool e1 env, evalTriBool e2 env) of
    (TTrue, _)        -> TTrue       -- short-circuit
    (_, TTrue)        -> TTrue
    (TFalse, TFalse)  -> TFalse
    _                 -> TUnknown

-- Variables look up in env; missing = NOTHING = TUnknown
evalTriBool (Var _ name) env =
  case Map.lookup name env of
    Just (Just b)  -> if b then TTrue else TFalse
    Just Nothing   -> TUnknown  -- Explicitly NOTHING
    Nothing        -> TUnknown  -- Field omitted from input
```

**Pros:**

- Simple to implement
- Integrates with existing evaluator
- Preserves short-circuit behavior

**Cons:**

- Doesn't naturally provide variable prioritization
- Can't distinguish "don't care" from "needed"

#### Approach 2: BDD Construction and Cofactoring

Build a BDD from the boolean expression, then use **cofactoring** to simplify given known values.

```haskell
-- Build BDD from L4 expression
exprToBDD :: Expr Resolved -> BDD

-- Cofactor: substitute variable with constant
cofactor :: BDD -> Var -> Bool -> BDD

-- Get remaining variables after cofactoring
remainingVars :: BDD -> [Var]

-- Check if BDD is constant (tautology or contradiction)
isConstant :: BDD -> Maybe Bool
```

Given known inputs, repeatedly cofactor to simplify:

```haskell
partialEval :: BDD -> [(Var, Bool)] -> PartialResult
partialEval bdd knowns =
  let bdd' = foldl' (\b (v, val) -> cofactor b v val) bdd knowns
  in case isConstant bdd' of
       Just result -> Determined result
       Nothing     -> NeedMore (remainingVars bdd')
```

**Pros:**

- Canonical representation
- Efficient operations
- Natural don't-care detection

**Cons:**

- BDD size can explode for some orderings
- Need variable ordering heuristics
- Additional dependency

#### Approach 3: Lazy Evaluation with Dependency Tracking

Leverage the existing lazy evaluator, but track which `Unknown` values were **forced** during evaluation.

```haskell
data TrackedUnknown = TrackedUnknown
  { varName :: Text
  , forced  :: IORef Bool
  }

-- During evaluation, when we encounter Unknown and need its value,
-- mark it as forced
evalWithTracking :: Expr Resolved -> Env -> IO (Either [Text] Bool)
```

**Pros:**

- Uses existing infrastructure
- Handles arbitrary expressions (not just boolean)
- Natural short-circuit behavior

**Cons:**

- Requires evaluation machinery changes
- Multiple evaluation passes for full analysis
- Order-dependent results

### Recommended Approach: Hybrid

Combine approaches for best results:

1. **Phase 1: Three-valued evaluation** - Quick determination of known/unknown result
2. **Phase 2: BDD for prioritization** - When result is unknown, build BDD to analyze which variables to ask for first
3. **Phase 3: Impact analysis** - For each candidate variable, cofactor and check if result becomes determinable

### Variable Prioritization Heuristics

When multiple variables are needed, prioritize based on:

1. **Decision power** - Variables that appear higher in BDD (more discriminating)
2. **Determination likelihood** - Variables where one value leads to determined result
3. **User-specified ordering** - Honor `@priority` annotations if present
4. **Lexical order** - Fallback: order of appearance in GIVEN clause

### Integration with Existing Trace

The evaluation trace already captures which expressions were evaluated. We can:

1. Extract boolean subexpressions from trace
2. Identify which variables were actually accessed
3. Map accessed variables to "definitely relevant" set

## Implementation Plan

### Phase 1: Three-Valued Evaluation (MVP)

**Goal:** Return `{result: true|false}` or `{required: [param_names]}`

1. Add `TriBool` type and basic evaluation
2. Add `/partial-evaluation` endpoint
3. Return flat list of unknown variables that were encountered

**Scope:** ~200 LOC, 1-2 days

### Phase 2: BDD-Based Prioritization

**Goal:** Order required parameters by importance

1. Add BDD library dependency (e.g., `cudd`, `obdd`, or pure Haskell)
2. Convert boolean `Expr Resolved` to BDD
3. Implement cofactoring for known values
4. Use BDD structure for variable ordering

**Scope:** ~500 LOC, 3-5 days

### Phase 3: Impact Analysis

**Goal:** For each required parameter, show what happens if true/false

1. For each remaining variable, cofactor with true and false
2. Recursively analyze resulting BDDs
3. Return structured impact information

**Scope:** ~300 LOC, 2-3 days

### Phase 4: Chatbot Integration

**Goal:** Natural language explanation generation

1. Template-based explanation of why each parameter matters
2. Integration with LLM for natural phrasing
3. Conversation state management

**Scope:** ~400 LOC + prompts, 3-5 days

## Worked Examples

### Example 1: Alcohol Purchase (from Issue #638)

**L4 Source:**

```l4
GIVEN
  age IS A NUMBER
  married IS A BOOLEAN
  spousal_approval IS A BOOLEAN
  beer_only IS A BOOLEAN
  parental_approval IS A BOOLEAN
  emancipated IS A BOOLEAN
GIVETH A BOOLEAN
DECIDE `may purchase alcohol` IF
        age >= 21
   AND     NOT married
        OR spousal_approval
        OR beer_only
  OR    age < 21
   AND  parental_approval
        OR emancipated
```

**Logical formula (simplified variable names):**

```
f(A,M,S,B,P,E) = (A≥21 ∧ (¬M ∨ S ∨ B)) ∨ (A<21 ∧ (P ∨ E))

Where:
  A = age (we'll treat A≥21 as boolean variable "adult")
  M = married
  S = spousal_approval
  B = beer_only
  P = parental_approval
  E = emancipated
```

**As pure boolean (with `adult` as variable):**

```
f(adult,M,S,B,P,E) = (adult ∧ (¬M ∨ S ∨ B)) ∨ (¬adult ∧ (P ∨ E))
```

#### Scenario 1: No inputs known

**Request:**

```json
{ "fnArguments": {} }
```

**Analysis:**

- Three-valued eval: `Unknown` (need more info)
- BDD analysis: `adult` appears at root → ask first
- `adult=T` leads to subtree with {M,S,B}
- `adult=F` leads to subtree with {P,E}

**Expected Response:**

```json
{
  "required": [
    {
      "name": "age",
      "reason": "Determines which set of rules applies",
      "priority": 1,
      "impact": {
        "ifTrue": {
          "determinable": false,
          "stillRequired": ["married", "spousal_approval", "beer_only"]
        },
        "ifFalse": {
          "determinable": false,
          "stillRequired": ["parental_approval", "emancipated"]
        }
      }
    }
  ]
}
```

#### Scenario 2: age=30 (adult=True)

**Request:**

```json
{ "fnArguments": { "age": 30 } }
```

**Analysis:**

- Cofactor with adult=T: `f|_{adult=T} = ¬M ∨ S ∨ B`
- P and E are now don't-cares
- If M=F: result is True immediately
- If B=T: result is True immediately
- If S=T: result is True immediately

**Expected Response:**

```json
{
  "required": [
    {
      "name": "married",
      "reason": "Unmarried adults can purchase without further approval",
      "priority": 1,
      "impact": {
        "ifTrue": {
          "determinable": false,
          "stillRequired": ["spousal_approval", "beer_only"]
        },
        "ifFalse": {
          "determinable": true,
          "result": true
        }
      }
    },
    {
      "name": "beer_only",
      "reason": "Beer purchases have simplified rules",
      "priority": 2,
      "impact": {
        "ifTrue": {
          "determinable": true,
          "result": true
        },
        "ifFalse": {
          "determinable": false,
          "stillRequired": ["married", "spousal_approval"]
        }
      }
    },
    {
      "name": "spousal_approval",
      "reason": "Married adults need spousal approval for non-beer",
      "priority": 3,
      "impact": {
        "ifTrue": {
          "determinable": true,
          "result": true
        },
        "ifFalse": {
          "determinable": false,
          "stillRequired": ["married", "beer_only"]
        }
      }
    }
  ]
}
```

#### Scenario 3: age=30, married=False

**Request:**

```json
{ "fnArguments": { "age": 30, "married": false } }
```

**Analysis:**

- Cofactor: `f|_{adult=T, M=F} = T ∨ S ∨ B = T`
- Result is determined! (tautology)

**Expected Response:**

```json
{
  "result": true,
  "reasoning": {
    "explanation": "Adults who are unmarried may purchase alcohol",
    "relevantInputs": ["age", "married"],
    "irrelevantInputs": [
      "spousal_approval",
      "beer_only",
      "parental_approval",
      "emancipated"
    ]
  }
}
```

#### Scenario 4: age=30, married=True, beer_only=False, spousal_approval=False

**Request:**

```json
{
  "fnArguments": {
    "age": 30,
    "married": true,
    "beer_only": false,
    "spousal_approval": false
  }
}
```

**Analysis:**

- Cofactor: `f|_{adult=T, M=T, B=F, S=F} = F ∨ F ∨ F = F`
- Result is determined! (contradiction under these conditions)

**Expected Response:**

```json
{
  "result": false,
  "reasoning": {
    "explanation": "Married adults purchasing non-beer require spousal approval",
    "relevantInputs": ["age", "married", "spousal_approval", "beer_only"],
    "irrelevantInputs": ["parental_approval", "emancipated"]
  }
}
```

#### Scenario 5: age=19 (adult=False)

**Request:**

```json
{ "fnArguments": { "age": 19 } }
```

**Analysis:**

- Cofactor with adult=F: `f|_{adult=F} = P ∨ E`
- M, S, B are now don't-cares
- Either P=T or E=T gives True

**Expected Response:**

```json
{
  "required": [
    {
      "name": "parental_approval",
      "priority": 1,
      "impact": {
        "ifTrue": { "determinable": true, "result": true },
        "ifFalse": { "determinable": false, "stillRequired": ["emancipated"] }
      }
    },
    {
      "name": "emancipated",
      "priority": 2,
      "impact": {
        "ifTrue": { "determinable": true, "result": true },
        "ifFalse": {
          "determinable": false,
          "stillRequired": ["parental_approval"]
        }
      }
    }
  ]
}
```

### Example 2: Insurance Coverage (vermin_and_rodent)

**L4 Source (simplified):**

```l4
GIVEN i IS Inputs
GIVETH A BOOLEAN
DECIDE `insurance covered` i IF
    `not covered if`
         `loss or damage by animals`
     AND NOT `damage to contents and caused by birds`
              OR `ensuing covered loss`
                  AND NOT `exclusion apply`
```

This is more complex because it has nested structure with WHERE clauses.

**Variables:**

- rodents, insects, vermin, birds (animal damage types)
- to_contents (damage type)
- ensuing_loss
- other_exclusion, appliance, pool, plumbing (exclusion sources)

**Key observation:** The structure is:

```
covered = not_covered_if(
  animals AND NOT (contents_birds OR (ensuing AND NOT exclusions))
)
```

Where `not_covered_if` is the identity function (returns its argument).

So: `covered = animals ∧ ¬(contents_birds ∨ (ensuing ∧ ¬exclusions))`

Equivalently: `covered = animals ∧ ¬contents_birds ∧ (¬ensuing ∨ exclusions)`

#### Scenario: No animal damage

**Request:**

```json
{
  "fnArguments": {
    "i": {
      "Loss or Damage.caused by rodents": false,
      "Loss or Damage.caused by insects": false,
      "Loss or Damage.caused by vermin": false,
      "Loss or Damage.caused by birds": false
    }
  }
}
```

**Analysis:**

- `animals = F ∨ F ∨ F ∨ F = F`
- `covered = F ∧ ... = F`
- Result determined immediately

**Expected Response:**

```json
{
  "result": false,
  "reasoning": {
    "explanation": "No animal-caused damage, so exclusion does not apply (but neither does coverage under this clause)"
  }
}
```

### Example 3: Edge Cases

#### 3a: Tautology

**L4:** `DECIDE always_true IF x OR NOT x`

**Any input:** Result = True (tautology)

```json
{ "result": true }
```

#### 3b: Contradiction

**L4:** `DECIDE never_true IF x AND NOT x`

**Any input:** Result = False (contradiction)

```json
{ "result": false }
```

#### 3c: Single Variable

**L4:** `DECIDE identity x IF x`

**No input:**

```json
{
  "required": [
    {
      "name": "x",
      "priority": 1,
      "impact": {
        "ifTrue": { "determinable": true, "result": true },
        "ifFalse": { "determinable": true, "result": false }
      }
    }
  ]
}
```

#### 3d: Deeply Nested

**L4:**

```l4
DECIDE deep IF
  a AND b AND c AND d AND e AND f AND g AND h
```

**With a=True, b=True, c=True, d=True, e=True, f=True, g=True:**

```json
{
  "required": [
    {
      "name": "h",
      "priority": 1,
      "impact": {
        "ifTrue": { "determinable": true, "result": true },
        "ifFalse": { "determinable": true, "result": false }
      }
    }
  ]
}
```

#### 3e: XOR (Neither value determines)

**L4:** `DECIDE xor_example IF (a AND NOT b) OR (NOT a AND b)`

**No input:**

```json
{
  "required": [
    {
      "name": "a",
      "priority": 1,
      "impact": {
        "ifTrue": { "determinable": false, "stillRequired": ["b"] },
        "ifFalse": { "determinable": false, "stillRequired": ["b"] }
      }
    },
    {
      "name": "b",
      "priority": 1,
      "impact": {
        "ifTrue": { "determinable": false, "stillRequired": ["a"] },
        "ifFalse": { "determinable": false, "stillRequired": ["a"] }
      }
    }
  ]
}
```

Note: Both variables have equal priority since neither alone determines the result.

#### 3f: Implies

**L4:** `DECIDE implication IF a IMPLIES b` (equivalent to `NOT a OR b`)

**With a=False:**

```json
{ "result": true }
```

**With a=True:**

```json
{
  "required": [
    {
      "name": "b",
      "impact": {
        "ifTrue": { "determinable": true, "result": true },
        "ifFalse": { "determinable": true, "result": false }
      }
    }
  ]
}
```

## Testing Plan

### Unit Tests: Three-Valued Logic

```haskell
describe "TriBool" $ do
  describe "triAnd" $ do
    it "F ∧ F = F" $ triAnd TFalse TFalse `shouldBe` TFalse
    it "F ∧ T = F" $ triAnd TFalse TTrue `shouldBe` TFalse
    it "F ∧ U = F" $ triAnd TFalse TUnknown `shouldBe` TFalse  -- short-circuit!
    it "T ∧ F = F" $ triAnd TTrue TFalse `shouldBe` TFalse
    it "T ∧ T = T" $ triAnd TTrue TTrue `shouldBe` TTrue
    it "T ∧ U = U" $ triAnd TTrue TUnknown `shouldBe` TUnknown
    it "U ∧ F = F" $ triAnd TUnknown TFalse `shouldBe` TFalse  -- short-circuit!
    it "U ∧ T = U" $ triAnd TUnknown TTrue `shouldBe` TUnknown
    it "U ∧ U = U" $ triAnd TUnknown TUnknown `shouldBe` TUnknown

  describe "triOr" $ do
    it "F ∨ F = F" $ triOr TFalse TFalse `shouldBe` TFalse
    it "F ∨ T = T" $ triOr TFalse TTrue `shouldBe` TTrue
    it "F ∨ U = U" $ triOr TFalse TUnknown `shouldBe` TUnknown
    it "T ∨ F = T" $ triOr TTrue TFalse `shouldBe` TTrue
    it "T ∨ T = T" $ triOr TTrue TTrue `shouldBe` TTrue
    it "T ∨ U = T" $ triOr TTrue TUnknown `shouldBe` TTrue  -- short-circuit!
    it "U ∨ F = U" $ triOr TUnknown TFalse `shouldBe` TUnknown
    it "U ∨ T = T" $ triOr TUnknown TTrue `shouldBe` TTrue  -- short-circuit!
    it "U ∨ U = U" $ triOr TUnknown TUnknown `shouldBe` TUnknown

  describe "triNot" $ do
    it "¬F = T" $ triNot TFalse `shouldBe` TTrue
    it "¬T = F" $ triNot TTrue `shouldBe` TFalse
    it "¬U = U" $ triNot TUnknown `shouldBe` TUnknown
```

### Unit Tests: BDD Operations

```haskell
describe "BDD" $ do
  describe "construction" $ do
    it "builds leaf True" $
      exprToBDD (Lit True) `shouldBe` Leaf True

    it "builds leaf False" $
      exprToBDD (Lit False) `shouldBe` Leaf False

    it "builds single variable" $
      exprToBDD (Var "x") `shouldBe` Node "x" (Leaf False) (Leaf True)

    it "builds A AND B" $
      exprToBDD (And (Var "a") (Var "b"))
        `shouldBe` Node "a" (Leaf False) (Node "b" (Leaf False) (Leaf True))

    it "builds A OR B" $
      exprToBDD (Or (Var "a") (Var "b"))
        `shouldBe` Node "a" (Node "b" (Leaf False) (Leaf True)) (Leaf True)

  describe "cofactor" $ do
    let ab = exprToBDD (And (Var "a") (Var "b"))

    it "cofactor(A∧B, a=T) = B" $
      cofactor ab "a" True `shouldBe` Node "b" (Leaf False) (Leaf True)

    it "cofactor(A∧B, a=F) = F" $
      cofactor ab "a" False `shouldBe` Leaf False

    it "cofactor(A∧B, b=T) = A" $
      cofactor ab "b" True `shouldBe` Node "a" (Leaf False) (Leaf True)

    it "cofactor(A∧B, b=F) = F" $
      cofactor ab "b" False `shouldBe` Leaf False

  describe "isConstant" $ do
    it "Leaf True is constant True" $
      isConstant (Leaf True) `shouldBe` Just True

    it "Leaf False is constant False" $
      isConstant (Leaf False) `shouldBe` Just False

    it "Variable is not constant" $
      isConstant (Node "x" (Leaf False) (Leaf True)) `shouldBe` Nothing

  describe "remainingVars" $ do
    it "Leaf has no variables" $
      remainingVars (Leaf True) `shouldBe` []

    it "Single node has one variable" $
      remainingVars (Node "x" (Leaf False) (Leaf True)) `shouldBe` ["x"]

    it "A∧B has two variables" $
      remainingVars (exprToBDD (And (Var "a") (Var "b")))
        `shouldBe` ["a", "b"]

  describe "isDontCare" $ do
    let aOrB = exprToBDD (Or (Var "a") (Var "b"))

    it "in A∨B, neither is don't-care initially" $ do
      isDontCare aOrB "a" `shouldBe` False
      isDontCare aOrB "b" `shouldBe` False

    it "in A∨B with A=T, B is don't-care" $
      isDontCare (cofactor aOrB "a" True) "b" `shouldBe` True

    it "in (A∨B)∧C, with A=T, B is don't-care but C is not" $ do
      let f = exprToBDD (And (Or (Var "a") (Var "b")) (Var "c"))
      let f' = cofactor f "a" True  -- becomes just C
      isDontCare f' "b" `shouldBe` True
      isDontCare f' "c" `shouldBe` False
```

### Unit Tests: Expression Evaluation

```haskell
describe "evalTriBool" $ do
  let env = Map.fromList [("a", TTrue), ("b", TFalse)]
  let envPartial = Map.fromList [("a", TTrue)]  -- b unknown

  it "evaluates constant True" $
    evalTriBool (Lit True) Map.empty `shouldBe` TTrue

  it "evaluates variable from env" $
    evalTriBool (Var "a") env `shouldBe` TTrue

  it "evaluates missing variable as Unknown" $
    evalTriBool (Var "x") env `shouldBe` TUnknown

  it "short-circuits AND with False" $
    evalTriBool (And (Var "b") (Var "x")) env `shouldBe` TFalse

  it "returns Unknown for AND with True and missing" $
    evalTriBool (And (Var "a") (Var "b")) envPartial `shouldBe` TUnknown

  it "short-circuits OR with True" $
    evalTriBool (Or (Var "a") (Var "x")) env `shouldBe` TTrue

  it "evaluates complex expression" $
    evalTriBool (Or (And (Var "a") (Var "b")) (Var "a")) env `shouldBe` TTrue
```

### Integration Tests: Alcohol Example

```haskell
describe "alcohol purchase partial evaluation" $ do
  let alcoholFunc = "may_purchase_alcohol"

  it "with no inputs, asks for age first" $ do
    resp <- partialEval alcoholFunc Map.empty
    resp.result `shouldBe` Nothing
    (head resp.required).name `shouldBe` "age"
    (head resp.required).priority `shouldBe` 1

  it "with age=30, result still unknown but parental/emancipated irrelevant" $ do
    resp <- partialEval alcoholFunc (Map.singleton "age" (FnLitInt 30))
    resp.result `shouldBe` Nothing
    let reqNames = map (.name) resp.required
    reqNames `shouldContain` ["married"]
    reqNames `shouldNotContain` ["parental_approval"]
    reqNames `shouldNotContain` ["emancipated"]

  it "with age=30, married=false, result is True" $ do
    resp <- partialEval alcoholFunc
      (Map.fromList [("age", FnLitInt 30), ("married", FnLitBool False)])
    resp.result `shouldBe` Just True

  it "with age=30, married=true, asks for spousal_approval or beer_only" $ do
    resp <- partialEval alcoholFunc
      (Map.fromList [("age", FnLitInt 30), ("married", FnLitBool True)])
    resp.result `shouldBe` Nothing
    let reqNames = map (.name) resp.required
    reqNames `shouldContain` ["spousal_approval"]
    reqNames `shouldContain` ["beer_only"]

  it "with age=19, asks for parental_approval or emancipated" $ do
    resp <- partialEval alcoholFunc (Map.singleton "age" (FnLitInt 19))
    resp.result `shouldBe` Nothing
    let reqNames = map (.name) resp.required
    reqNames `shouldContain` ["parental_approval"]
    reqNames `shouldContain` ["emancipated"]
    reqNames `shouldNotContain` ["married"]
    reqNames `shouldNotContain` ["spousal_approval"]

  it "with age=19, emancipated=true, result is True" $ do
    resp <- partialEval alcoholFunc
      (Map.fromList [("age", FnLitInt 19), ("emancipated", FnLitBool True)])
    resp.result `shouldBe` Just True

  it "with age=19, emancipated=false, parental_approval=false, result is False" $ do
    resp <- partialEval alcoholFunc
      (Map.fromList
        [ ("age", FnLitInt 19)
        , ("emancipated", FnLitBool False)
        , ("parental_approval", FnLitBool False)
        ])
    resp.result `shouldBe` Just False
```

### Integration Tests: Edge Cases

```haskell
describe "edge cases" $ do
  it "tautology returns True with no inputs" $ do
    -- x OR NOT x
    resp <- partialEval "tautology_func" Map.empty
    resp.result `shouldBe` Just True
    resp.required `shouldBe` []

  it "contradiction returns False with no inputs" $ do
    -- x AND NOT x
    resp <- partialEval "contradiction_func" Map.empty
    resp.result `shouldBe` Just False
    resp.required `shouldBe` []

  it "single variable asks for it" $ do
    -- just x
    resp <- partialEval "identity_func" Map.empty
    resp.result `shouldBe` Nothing
    length resp.required `shouldBe` 1
    (head resp.required).name `shouldBe` "x"

  it "XOR gives equal priority to both variables" $ do
    -- (a AND NOT b) OR (NOT a AND b)
    resp <- partialEval "xor_func" Map.empty
    resp.result `shouldBe` Nothing
    length resp.required `shouldBe` 2
    let priorities = map (.priority) resp.required
    head priorities `shouldBe` last priorities  -- equal priority

  it "implication with premise false is True" $ do
    -- a IMPLIES b, with a=False
    resp <- partialEval "implication_func" (Map.singleton "a" (FnLitBool False))
    resp.result `shouldBe` Just True

  it "deeply nested AND with all but one True asks for last" $ do
    -- a AND b AND c AND d AND e AND f AND g AND h
    let allButH = Map.fromList
          [ ("a", FnLitBool True), ("b", FnLitBool True)
          , ("c", FnLitBool True), ("d", FnLitBool True)
          , ("e", FnLitBool True), ("f", FnLitBool True)
          , ("g", FnLitBool True)
          ]
    resp <- partialEval "deep_and_func" allButH
    resp.result `shouldBe` Nothing
    length resp.required `shouldBe` 1
    (head resp.required).name `shouldBe` "h"

  it "deeply nested AND with one False returns False" $ do
    let withOneFalse = Map.fromList
          [ ("a", FnLitBool True), ("b", FnLitBool False)  -- b is False!
          ]
    resp <- partialEval "deep_and_func" withOneFalse
    resp.result `shouldBe` Just False
```

### Golden Tests

```haskell
describe "golden tests" $ do
  it "alcohol_no_inputs" $
    goldenTest "alcohol_no_inputs" $
      partialEval "may_purchase_alcohol" Map.empty

  it "alcohol_adult" $
    goldenTest "alcohol_adult" $
      partialEval "may_purchase_alcohol" (Map.singleton "age" (FnLitInt 30))

  it "alcohol_minor" $
    goldenTest "alcohol_minor" $
      partialEval "may_purchase_alcohol" (Map.singleton "age" (FnLitInt 19))

  it "insurance_no_animal_damage" $
    goldenTest "insurance_no_animal" $
      partialEval "vermin_and_rodent"
        (Map.singleton "i" (FnObject
          [ ("Loss or Damage.caused by rodents", FnLitBool False)
          , ("Loss or Damage.caused by insects", FnLitBool False)
          , ("Loss or Damage.caused by vermin", FnLitBool False)
          , ("Loss or Damage.caused by birds", FnLitBool False)
          ]))
```

## API Versioning

This is a new endpoint, so no backward compatibility concerns. However, we should:

1. Mark as experimental/beta initially
2. Use `Accept: application/vnd.l4.partial-eval.v1+json` for versioning
3. Stabilize after chatbot integration validates the design

## Future Enhancements

### TYPICALLY: Overrideable Defaults (Not Yet Implemented)

**Status:** Conceptual design exists in `doc/default-values.md`, but `TYPICALLY` is not a keyword in the current L4 lexer.

**Motivation:** When we talk about a "person" in legal contexts, we typically assume they are:

- A natural person (not a corporation)
- Over the age of majority
- Have mental capacity
- Not under duress

These are **rebuttable presumptions** - defaults that hold unless explicitly overridden.

**Proposed Syntax:**

```l4
DECLARE Person HAS
  age IS A NUMBER
  has_capacity IS A BOOLEAN TYPICALLY TRUE
  is_natural_person IS A BOOLEAN TYPICALLY TRUE
  is_under_duress IS A BOOLEAN TYPICALLY FALSE
```

Or at the ASSUME level:

```l4
ASSUME `person has capacity` IS BOOLEAN TYPICALLY TRUE
```

**Integration with Partial Evaluation:**

With TYPICALLY, the partial evaluation logic changes:

| Input State   | Without TYPICALLY   | With TYPICALLY      |
| ------------- | ------------------- | ------------------- |
| Field omitted | `NOTHING` (unknown) | Use default value   |
| Field = null  | `NOTHING` (unknown) | `NOTHING` (unknown) |
| Field = value | `JUST value`        | `JUST value`        |

This means:

1. **Fewer questions needed:** If defaults match common cases, many parameters become don't-cares
2. **Explicit uncertainty:** User can still say "I don't know" (null) to override the default
3. **Audit trail:** System can report "assumed X was true based on typical case"

**Example with Alcohol Purchase:**

```l4
GIVEN
  age IS A NUMBER
  married IS A BOOLEAN TYPICALLY FALSE  -- Most buyers are single
  has_parental_approval IS A BOOLEAN TYPICALLY FALSE
  is_emancipated IS A BOOLEAN TYPICALLY FALSE
GIVETH A BOOLEAN
`may purchase alcohol` ...
```

**Partial eval with age=30:**

- Without TYPICALLY: asks about married, spousal_approval, beer_only
- With TYPICALLY: assumes married=FALSE → result=TRUE immediately!
- User can override by explicitly providing married=TRUE

**Implementation Notes:**

1. Add `TYPICALLY` keyword to lexer
2. Extend `Assume` AST node to include optional default value
3. Extend type declarations to include defaults per field
4. Modify three-valued eval to use defaults for missing fields
5. Track which defaults were used in reasoning trace

**See Also:** `doc/default-values.md` for detailed design discussion of `WithDefault` types.

### Non-Boolean Functions

Extend to numeric and multi-valued functions:

- "If salary > $100k, tax rate could be 30% or 35% depending on..."
- Interval arithmetic for numeric unknowns

### Explanations

Generate human-readable explanations:

- "I need to know your age because if you're over 21, the rules are simpler"
- "Your parental approval doesn't matter since you're already 30"

### Caching

Cache BDDs for frequently-queried functions to avoid reconstruction.

### IDE Integration

Show "hot paths" in the visualization:

- Highlight which branches are still live given partial input
- Gray out eliminated branches

## External Libraries: What to Reuse vs. Implement

**Recommendation:** Use existing BDD libraries rather than reimplementing. The algorithms are well-understood but tricky to get right efficiently.

### Haskell BDD Libraries

| Library                                                                        | Type                  | Pros                                                                        | Cons                                                                          | Recommendation                                   |
| ------------------------------------------------------------------------------ | --------------------- | --------------------------------------------------------------------------- | ----------------------------------------------------------------------------- | ------------------------------------------------ |
| [**obdd**](https://hackage.haskell.org/package/obdd)                           | Pure Haskell          | Simple API, no FFI, easy debugging, graphviz visualization                  | No node sharing (inefficient for large BDDs), "mostly educational" per author | ✅ **Start here** for MVP                        |
| [**cudd**](https://hackage.haskell.org/package/cudd)                           | FFI to CUDD C library | Production-grade performance, dynamic variable reordering, complement edges | Requires C library installation, more complex API                             | ✅ **Use for production** if performance matters |
| [**decision-diagrams**](https://hackage.haskell.org/package/decision-diagrams) | Pure Haskell          | BDD + ZDD support, well-maintained                                          | Less mature than CUDD                                                         | Consider for ZDD use cases                       |
| [**hgoes/bdd**](https://github.com/hgoes/bdd)                                  | Pure Haskell          | Shared nodes (more efficient than obdd)                                     | Less documentation                                                            | Alternative to obdd                              |

### Recommended Approach

**Phase 1 (MVP):** Use `obdd` for simplicity

```haskell
-- In cabal file:
build-depends: obdd >= 0.8

-- Usage:
import OBDD (OBDD, unit, and, or, not, fold, satisfiable)

exprToBDD :: Expr Resolved -> OBDD Text
exprToBDD (Lit True)    = OBDD.constant True
exprToBDD (Lit False)   = OBDD.constant False
exprToBDD (Var name)    = OBDD.unit name True
exprToBDD (And e1 e2)   = OBDD.and (exprToBDD e1) (exprToBDD e2)
exprToBDD (Or e1 e2)    = OBDD.or (exprToBDD e1) (exprToBDD e2)
exprToBDD (Not e)       = OBDD.not (exprToBDD e)
```

**Phase 2 (Performance):** Switch to `cudd` if needed

```haskell
-- In cabal file:
build-depends: cudd >= 0.1

-- Usage requires DDManager:
import Cudd.Cudd (DDManager, bAnd, bOr, bNot, ...)
```

### SAT/SMT Solvers (Alternative Approach)

Instead of BDDs, we could use SMT solving via [**sbv**](https://hackage.haskell.org/package/sbv):

```haskell
-- Check satisfiability with constraints
import Data.SBV

checkWithKnowns :: Expr -> [(Text, Bool)] -> IO (Maybe Bool)
checkWithKnowns expr knowns = do
  result <- sat $ do
    vars <- mapM (sbool . unpack) (freeVars expr)
    -- Add constraints for known values
    forM_ knowns $ \(name, val) ->
      constrain $ lookupVar name vars .== literal val
    -- Return the expression
    return $ exprToSBV expr vars
  case result of
    Unsatisfiable -> Just False  -- Can never be true
    Satisfiable _ -> Nothing     -- Could be true, depends on unknowns
```

**Pros:** Very powerful, handles arithmetic constraints too (not just boolean)
**Cons:** Requires external solver (Z3, CVC5), heavier dependency, overkill for pure boolean

### Espresso Logic Minimizer

[Espresso](https://en.wikipedia.org/wiki/Espresso_heuristic_logic_minimizer) is the classic two-level logic minimizer. Available as:

- [**PyEDA**](https://pyeda.readthedocs.io/en/latest/2llm.html) - Python with C extension
- [**espresso-logic-minimizer**](https://www.npmjs.com/package/espresso-logic-minimizer) - Node.js bridge
- [**espresso-logic**](https://github.com/classabbyamp/espresso-logic) - Modern C rehost

**No Haskell bindings exist**, so we'd need to either:

1. Call via FFI to C library
2. Use as subprocess (input/output PLA files)
3. Reimplement in Haskell (not recommended - complex)

**Our use case doesn't need Espresso.** Espresso minimizes to sum-of-products form for circuit synthesis. We need:

- Partial evaluation (BDDs do this naturally via cofactoring)
- Variable relevance detection (BDDs tell us remaining variables)
- Impact analysis (BDDs via cofactoring both ways)

### What We Need to Implement

| Component           | Reuse Library?                           | Notes                                 |
| ------------------- | ---------------------------------------- | ------------------------------------- |
| BDD construction    | ✅ Yes (obdd/cudd)                       | Don't reinvent                        |
| Cofactoring         | ✅ Yes (built into BDD libs)             | Standard operation                    |
| Variable ordering   | ✅ Partial (cudd has dynamic reordering) | May need heuristics for initial order |
| Three-valued eval   | ❌ Implement                             | ~50 LOC, simple                       |
| L4 Expr → BDD       | ❌ Implement                             | ~100 LOC, straightforward             |
| Priority heuristics | ❌ Implement                             | ~100 LOC, our domain logic            |
| API endpoint        | ❌ Implement                             | ~200 LOC, REST integration            |
| Impact analysis     | ❌ Implement                             | ~100 LOC, calls cofactor              |

**Total new code:** ~550 LOC (excluding tests)
**Library code reused:** ~10,000+ LOC in BDD implementation

### Installation Notes

**obdd:**

```bash
cabal install obdd
# No external dependencies
```

**cudd:**

```bash
# First install CUDD C library
brew install cudd  # macOS
apt install libcudd-dev  # Ubuntu

# Then Haskell bindings
cabal install cudd
```

**sbv:**

```bash
# Install Z3 solver
brew install z3  # macOS
apt install z3  # Ubuntu

# Then Haskell library
cabal install sbv
```

## References

- Issue #638: Implement Interactive Boolean Minimization for Query Relevance Reduction
- Issue #635: Critical L4 Decision Service Improvements (parent issue)
- `L4.Transform`: Existing boolean simplification
- `L4.EvaluateLazy.Trace`: Evaluation tracing infrastructure
- [Espresso](https://en.wikipedia.org/wiki/Espresso_heuristic_logic_minimizer): Classic logic minimizer (not needed for our use case)
- [CUDD](https://github.com/ivmai/cudd): Production BDD library (C)
- [cudd Haskell bindings](https://hackage.haskell.org/package/cudd): High-performance Haskell interface
- [obdd](https://hackage.haskell.org/package/obdd): Pure Haskell OBDD library (good for MVP)
- [decision-diagrams](https://hackage.haskell.org/package/decision-diagrams): Pure Haskell BDD/ZDD
- [sbv](https://hackage.haskell.org/package/sbv): SMT-based verification (alternative approach)
- [PyEDA](https://pyeda.readthedocs.io/en/latest/2llm.html): Python Espresso bindings
