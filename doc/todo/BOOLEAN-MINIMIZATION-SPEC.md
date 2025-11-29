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

## Background

### Binary Decision Diagrams (BDDs)

A BDD represents a boolean function as a directed acyclic graph where:
- Each internal node represents a boolean variable
- Each node has two outgoing edges (true/false)
- Terminal nodes are 0 (false) or 1 (true)

BDDs are **ordered** (OBDD) when variables appear in a fixed order on all paths. This enables efficient operations like:
- Variable elimination (cofactoring)
- Don't-care detection
- Formula simplification

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
    "age": 30,
    "marital_status": null,      // unknown
    "spousal_approval": null,
    "beverage_type": null,
    "parental_approval": null,
    "legally_emancipated": null
  }
}
```

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

```haskell
data TriBool = TTrue | TFalse | TUnknown

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

## Testing Plan

### Unit Tests

```haskell
describe "three-valued evaluation" $ do
  it "short-circuits AND with known False" $ do
    eval (And _ (Lit False) Unknown) `shouldBe` TFalse

  it "requires both for AND with known True" $ do
    eval (And _ (Lit True) Unknown) `shouldBe` TUnknown

describe "BDD construction" $ do
  it "builds correct BDD for simple OR" $ ...
  it "cofactors correctly" $ ...
  it "identifies don't-care variables" $ ...

describe "partial evaluation API" $ do
  it "returns result when determinable" $ ...
  it "returns required params when not determinable" $ ...
  it "prioritizes high-impact variables" $ ...
```

### Integration Tests

Test against the alcohol purchase example from Issue #638:

1. No inputs → requires age first
2. Age=30 → requires marital_status or beverage_type
3. Age=30, unmarried → result=True
4. Age=19 → requires parental_approval or legally_emancipated
5. Age=19, legally_emancipated=True → result=True

### Golden Tests

Capture expected partial evaluation responses for representative functions.

## API Versioning

This is a new endpoint, so no backward compatibility concerns. However, we should:

1. Mark as experimental/beta initially
2. Use `Accept: application/vnd.l4.partial-eval.v1+json` for versioning
3. Stabilize after chatbot integration validates the design

## Future Enhancements

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

## References

- Issue #638: Implement Interactive Boolean Minimization for Query Relevance Reduction
- Issue #635: Critical L4 Decision Service Improvements (parent issue)
- `L4.Transform`: Existing boolean simplification
- `L4.EvaluateLazy.Trace`: Evaluation tracing infrastructure
- [Espresso](https://en.wikipedia.org/wiki/Espresso_heuristic_logic_minimizer): Classic logic minimizer
- [CUDD](https://github.com/ivmai/cudd): BDD library (C, with Haskell bindings)
- [obdd](https://hackage.haskell.org/package/obdd): Pure Haskell OBDD library
