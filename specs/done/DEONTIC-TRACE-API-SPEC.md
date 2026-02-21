# Deontic Evaluation via the Existing `/evaluation` Endpoint

**Status:** Proposal
**Date:** 2026-02-22
**Related:** [JL4-SERVICE-SPEC](../done/JL4-SERVICE-SPEC.md), [STATE-GRAPH-REST-API-IMPLEMENTATION](../done/STATE-GRAPH-REST-API-IMPLEMENTATION.md), [BOUNDED-DEONTICS-SPEC](../todo/BOUNDED-DEONTICS-SPEC.md)

---

## Motivation

### Current Behaviour

The jl4-service exposes L4 functions as REST APIs. For constitutive rules (`DECIDE`), the `/evaluation` endpoint works well: pass arguments in, get a result out.

For **regulative rules** (deontic — `PARTY x MUST/MAY/SHANT/DO y`), the service currently offers:

- **State graphs** (`GET .../state-graphs`) — static GraphViz visualization of the contract automaton
- **Evaluation** (`POST .../evaluation`) — but this **rejects** deontic return values with `"#EVAL produced obligation."` ([jl4-service/src/Backend/Jl4.hs:623](../../jl4-service/src/Backend/Jl4.hs))

There is **no way** to submit a sequence of events ("PARTY X DOES Y AT time T") via the REST API and receive the resulting contract state (FULFILLED, BREACH, or residual obligation).

### The Gap

L4's core already supports stateless deontic evaluation via the `EVALTRACE` builtin:

```
EVALTRACE : FORALL party action.
    DEONTIC party action          -- the contract
    -> NUMBER                     -- start time
    -> LIST (EVENT party action)  -- event stream
    -> DEONTIC party action       -- resulting state
```

This is used in `.l4` source files via the `#TRACE` directive:

```l4
#TRACE `the sale contract` AT 1 WITH
  PARTY `the seller` DOES `deliver the goods` AT 5
  PARTY `the buyer`  DOES `pay the invoice`   AT 20
-- Result: FULFILLED
```

The language can do this. The REST layer cannot.

### Desired Outcome

API consumers can:

1. Call the **existing `/evaluation` endpoint** with optional `startTime` and `events` fields to evaluate deontic functions
2. Receive a structured JSON response indicating the contract state: `FULFILLED`, `BREACH` (with reason), or a **residual obligation** (the "next step" the contract is waiting for)
3. Call `/evaluation` on a deontic function **without events** to get the initial obligation (what the contract expects first)
4. Use this for contract simulation, compliance checking, and what-if analysis — all statelessly

---

## Proposed Design

### No New Endpoint

Extend the existing `POST /deployments/{id}/functions/{fn}/evaluation` with two optional fields: `startTime` and `events`. The endpoint behaviour is determined by the function's return type and the presence of these fields:

| Return type                        | `events` present | Behaviour                                                                        |
| ---------------------------------- | ---------------- | -------------------------------------------------------------------------------- |
| Non-deontic (BOOLEAN, STRING, ...) | No               | Existing behaviour (unchanged)                                                   |
| Non-deontic                        | Yes              | **400 Bad Request** — events only apply to deontic functions                     |
| DEONTIC                            | No               | Evaluate and return the **initial obligation** (what the contract expects first) |
| DEONTIC                            | Yes              | Evaluate with `EVALTRACE`, return resulting contract state                       |

### Request Schema

The existing `FnArguments` type gains two optional fields:

```json
{
  "fnArguments": {
    "patron": { "NaturalPerson": { "name": "John Doe" } },
    "company": { "Restaurant": { "name": "EatAtJoes, Inc." } },
    "symtab": {
      "Symbol Table": {
        "bill": 0,
        "beers": 0,
        "potatoes": 0,
        "log": ["", "nothing has happened yet"]
      }
    }
  },
  "startTime": 1,
  "events": [
    {
      "party": { "NaturalPerson": { "name": "John Doe" } },
      "action": { "order": { "itemName": "beer", "quantity": 1 } },
      "at": 2
    },
    {
      "party": { "NaturalPerson": { "name": "John Doe" } },
      "action": { "order": { "itemName": "potato", "quantity": 1 } },
      "at": 12
    },
    {
      "party": { "NaturalPerson": { "name": "John Doe" } },
      "action": { "getBill": {} },
      "at": 20
    },
    {
      "party": { "Restaurant": { "name": "EatAtJoes, Inc." } },
      "action": { "showBill": { "amount": 12 } },
      "at": 30
    },
    {
      "party": { "NaturalPerson": { "name": "John Doe" } },
      "action": { "pay": { "amount": 12 } },
      "at": 40
    }
  ]
}
```

#### New Fields on `FnArguments`

| Field       | Type     | Required | Description                                                            |
| ----------- | -------- | -------- | ---------------------------------------------------------------------- |
| `startTime` | `number` | No       | Start time for contract evaluation. Required when `events` is present. |
| `events`    | `array`  | No       | Ordered list of events to replay against the contract.                 |

#### Event Object

| Field    | Type     | Required | Description                                                      |
| -------- | -------- | -------- | ---------------------------------------------------------------- |
| `party`  | `value`  | Yes      | The acting party, serialized matching the function's Party type  |
| `action` | `value`  | Yes      | The action taken, serialized matching the function's Action type |
| `at`     | `number` | Yes      | The timestamp when this event occurred                           |

### Response Schema

The response uses the **existing `ResponseWithReason` structure** — deontic results are serialized as structured `FnLiteral` values in `fnResult`, just like any other return type.

#### Case 1: FULFILLED

```json
{
  "fnResult": {
    "value": "FULFILLED"
  },
  "reasoning": { "payload": { ... } },
  "graphviz": null
}
```

#### Case 2: BREACH

```json
{
  "fnResult": {
    "value": {
      "BREACH": {
        "reason": "deadline_missed",
        "party": { "NaturalPerson": { "name": "John Doe" } },
        "action": "pay",
        "deadline": 10,
        "timestamp": 400
      }
    }
  },
  "reasoning": { "payload": { ... } },
  "graphviz": null
}
```

#### Case 3: Residual Obligation (contract still in progress)

```json
{
  "fnResult": {
    "value": {
      "OBLIGATION": {
        "party": { "NaturalPerson": { "name": "John Doe" } },
        "modal": "MUST",
        "action": { "pay": { "amount": 12 } },
        "deadline": 10
      }
    }
  },
  "reasoning": { "payload": { ... } },
  "graphviz": null
}
```

#### Case 4: No events, deontic function — initial obligation

```json
{
  "fnResult": {
    "value": {
      "OBLIGATION": {
        "party": { "NaturalPerson": { "name": "John Doe" } },
        "modal": "MUST",
        "action": { "order": { "itemName": "beer", "quantity": 1 } },
        "deadline": 10
      }
    }
  },
  "reasoning": { "payload": { ... } },
  "graphviz": null
}
```

This is the "show me the first thing this contract expects" case. The function is evaluated with its arguments but no events, so `EVALTRACE` is called with an empty event list (or the function is directly evaluated, producing the unevaluated obligation value). The result is the contract's initial state — what the first party must/may/shant do.

#### Why This Response Shape Works

- **Consistent**: Same `ResponseWithReason` envelope for all function types. Callers inspect `fnResult.value` to determine the result type, just as they would for any other function.
- **Self-describing**: The outer key (`"FULFILLED"`, `"BREACH"`, `"OBLIGATION"`) tells you the contract state. No new response types or endpoints.
- **Backward-compatible**: Non-deontic functions continue to return `{ "fnResult": { "value": true } }` etc. No change.

---

## Implementation Plan

### Phase 1: Extend `/evaluation` for Deontic Functions

**Approach:** Add optional `startTime`/`events` to `FnArguments`, generate an `EVALTRACE` wrapper when present, and lift the `ValObligation` rejection in `nfToFnLiteral` to serialize deontic results as `FnLiteral`.

#### Step 1.1: Extend `FnArguments` with Optional Fields

**File:** `jl4-service/src/Backend/Api.hs`

```haskell
data FnArguments = FnArguments
  { fnEvalBackend :: Maybe EvalBackend
  , fnArguments :: Map Text (Maybe FnLiteral)
  , startTime   :: Maybe Scientific            -- NEW: optional start time for deontic eval
  , events      :: Maybe [TraceEvent]          -- NEW: optional event list for deontic eval
  }
  deriving stock (Show, Read, Ord, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | A single event for deontic trace evaluation
data TraceEvent = TraceEvent
  { party  :: FnLiteral    -- Party value (matches function's Party type)
  , action :: FnLiteral    -- Action value (matches function's Action type)
  , at     :: Scientific   -- Timestamp
  }
  deriving stock (Show, Read, Ord, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)
```

Because `startTime` and `events` are `Maybe`, existing callers sending `{ "fnArguments": { ... } }` continue to work — the new fields default to `Nothing`.

#### Step 1.2: Detect Deontic Functions

When a function has return type `DEONTIC party action`, extract the party and action type parameters. This information is needed to validate requests and generate correct wrapper code.

**File:** `jl4-service/src/Backend/Jl4.hs`

```haskell
-- | Check if a function returns a DEONTIC type.
-- Returns the party and action type if so.
isDeonticReturn :: Decide Resolved -> Maybe (Type' Resolved, Type' Resolved)
isDeonticReturn (MkDecide _ (MkTypeSig _ _ (Just retType)) _ _) =
  case retType of
    TyApp _ name [partyTy, actionTy]
      | getUnique name == contractUnique -> Just (partyTy, actionTy)
    _ -> Nothing
isDeonticReturn _ = Nothing
```

#### Step 1.3: Branching Logic in the Handler

**File:** `jl4-service/src/DataPlane.hs`

The existing `evalFunctionHandler` checks the combination of return type and event presence:

```haskell
evalFunctionHandler deployId fnName mTraceHeader mTraceParam mGraphViz fnArgs = do
  (fns, _meta) <- requireDeploymentReady deployId
  vf <- requireFunction fns fnName

  let hasEvents = isJust fnArgs.events
      isDeontic = case vf.fnCompiled of
        Just compiled -> isJust (isDeonticReturn compiled.compiledDecide)
        Nothing -> False

  case (isDeontic, hasEvents) of
    -- Non-deontic, no events: existing path (unchanged)
    (False, False) ->
      runEvaluatorFor vf fnArgs.fnEvalBackend (Map.toList fnArgs.fnArguments)
        Nothing mTraceHeader mTraceParam mGraphViz

    -- Non-deontic with events: error
    (False, True) ->
      throwError err400 { errBody = "startTime and events are only valid for functions returning DEONTIC" }

    -- Deontic with events: EVALTRACE wrapper
    (True, True) ->
      runDeonticEvaluator vf fnArgs mTraceHeader mTraceParam mGraphViz

    -- Deontic without events: evaluate and return initial obligation
    (True, False) ->
      runDeonticEvaluator vf (fnArgs { events = Just [], startTime = Just 0 }) mTraceHeader mTraceParam mGraphViz
```

For the "no events, deontic" case: we call `EVALTRACE` with an empty event list and `startTime = 0`. This produces the unevaluated initial obligation — exactly what the contract expects first.

#### Step 1.4: Deontic Evaluator

**File:** `jl4-service/src/DataPlane.hs` — new function

```haskell
-- | Evaluate a deontic function with events via EVALTRACE
runDeonticEvaluator
  :: ValidatedFunction
  -> FnArguments      -- contains startTime and events
  -> Maybe Text       -- X-L4-Trace header
  -> Maybe TraceLevel -- ?trace= query param
  -> Maybe Bool       -- ?graphviz= query param
  -> AppM SimpleResponse
runDeonticEvaluator vf fnArgs mTraceHeader mTraceParam mGraphViz = do
  compiled <- case vf.fnCompiled of
    Nothing -> throwError err500 { errBody = "No compiled module" }
    Just c -> pure c

  let traceLevel = determineTraceLevel mTraceHeader mTraceParam
      includeGraphViz = traceLevel == TraceFull && Maybe.fromMaybe False mGraphViz
      startTime = Maybe.fromMaybe 0 fnArgs.startTime
      eventList = Maybe.fromMaybe [] fnArgs.events

  -- Generate wrapper code that calls EVALTRACE
  wrapper <- case generateDeonticWrapper vf.fnImpl.name compiled fnArgs.fnArguments startTime eventList traceLevel of
    Left err -> throwError err400 { errBody = textToLBS err }
    Right code -> pure code

  -- Evaluate using existing infrastructure
  evaluationResult <- timeoutAction $
    evaluateWrapperInContext ... wrapper compiled

  -- Handle result (deontic values now serializable)
  case evaluationResult of
    ...
```

#### Step 1.5: Code Generation for Deontic Wrapper

**File:** `jl4-service/src/Backend/CodeGen.hs` — new function

```haskell
-- | Generate L4 wrapper for deontic evaluation with events.
-- Produces code that calls EVALTRACE within an #EVAL directive.
generateDeonticWrapper
  :: Text                         -- ^ Target function name
  -> [(Text, Type' Resolved)]     -- ^ GIVEN parameter names and types
  -> [(Text, Type' Resolved)]     -- ^ ASSUME parameter names and types
  -> Aeson.Value                  -- ^ Input arguments as JSON object
  -> Scientific                   -- ^ Start time
  -> [TraceEvent]                 -- ^ Events (may be empty for initial state)
  -> TraceLevel                   -- ^ Whether to generate EVAL or EVALTRACE
  -> Either Text GeneratedCode
```

This composes the existing argument-decoding logic with the new EVALTRACE call. The generated wrapper looks like:

```l4
-- ========== GENERATED DEONTIC WRAPPER ==========
DECLARE InputArgs HAS
  `patron (input)` IS A MAYBE Actor
, `company (input)` IS A MAYBE Actor
, `symtab (input)` IS A MAYBE `Symbol Table`

GIVEN jsn IS A STRING
GIVETH AN EITHER STRING InputArgs
decodeArgs jsn MEANS JSONDECODE jsn

DECIDE inputJson IS "{ ... escaped JSON ... }"

-- Event party/action decoders (one pair of DECLARE + decode per event)
DECLARE EventParty1 HAS `p (ev)` IS A MAYBE Actor
DECIDE ep1Json IS "{ ... }"
-- ...repeat for each event...

#EVAL
  CONSIDER decodeArgs inputJson
    WHEN RIGHT args THEN
      CONSIDER args's `patron (input)`
        WHEN JUST patron THEN
          CONSIDER args's `company (input)`
            WHEN JUST company THEN
              CONSIDER args's `symtab (input)`
                WHEN JUST symtab THEN
                  JUST (EVALTRACE
                    (`order beer` patron company symtab)
                    1
                    (LIST
                      EVENT ep1 ea1 2
                    , EVENT ep2 ea2 12
                    , ...
                    ))
                WHEN NOTHING THEN NOTHING
            WHEN NOTHING THEN NOTHING
        WHEN NOTHING THEN NOTHING
    WHEN LEFT error THEN NOTHING
```

**Key insight:** The wrapper uses `#EVAL` (not `#TRACE`) and calls the `EVALTRACE` builtin function directly. This is exactly what the `#TRACE` directive desugars to internally — see `contractToEvalDirective` in [Machine.hs:2177-2179](../../jl4-core/src/L4/EvaluateLazy/Machine.hs):

```haskell
contractToEvalDirective contract t evs =
  pure $ App emptyAnno TypeCheck.evalContractRef [contract, t, evListExpr]
```

For the **empty events case** (initial obligation), the generated code simply evaluates `EVALTRACE contract 0 (LIST)` — which returns the contract itself as a `ValObligation`, representing the first obligation.

#### Step 1.6: Serialize Deontic Results to `FnLiteral`

Currently `nfToFnLiteral` in `Jl4.hs:616-626` throws on `ValObligation`, `ValBreached`, and `ValROp`. We replace these error cases with proper serialization:

**File:** `jl4-service/src/Backend/Jl4.hs`

```haskell
nfToFnLiteral ei = \case
  ...
  -- FULFILLED terminal → string "FULFILLED"
  Eval.ValConstructor r []
    | isFulfilled r -> pure $ FnLitString "FULFILLED"

  -- BREACH → structured object
  Eval.ValBreached reason -> do
    breachLit <- breachToFnLiteral ei reason
    pure $ FnObject [("BREACH", breachLit)]

  -- Residual obligation → structured object
  Eval.ValObligation _env party action _hence _henceExpr _lestExpr -> do
    obligLit <- obligationToFnLiteral ei party action
    pure $ FnObject [("OBLIGATION", obligLit)]

  -- Regulative AND/OR → structured object
  Eval.ValROp _env op left right -> do
    -- Phase 1: serialize first obligation
    -- Phase 2: serialize full tree
    ...

  Eval.ValEnvironment{} -> throwError $ InterpreterError "#EVAL produced environment."
  ...
```

Where the helper functions produce:

```haskell
-- | Serialize breach reason to FnLiteral
breachToFnLiteral :: EntityInfo -> ReasonForBreach NF -> ExceptT EvaluatorError IO FnLiteral
breachToFnLiteral ei = \case
  DeadlineMissed party action deadline eventParty eventAction eventTime ->
    FnObject
      [ ("reason", FnLitString "deadline_missed")
      , ("party", partyLit)
      , ("action", actionLit)
      , ("deadline", FnLitDouble $ fromRational deadline)
      , ("timestamp", FnLitDouble $ fromRational eventTime)
      ]
  ExplicitBreach mParty mReason ->
    FnObject
      [ ("reason", FnLitString "explicit")
      , ("party", maybe FnUnknown id partyLit)
      ]

-- | Serialize obligation to FnLiteral
obligationToFnLiteral :: EntityInfo -> Either RExpr (Value a) -> RAction Resolved -> ExceptT EvaluatorError IO FnLiteral
obligationToFnLiteral ei party action =
  FnObject
    [ ("party", partyLit)
    , ("modal", FnLitString $ modalToText action.modal)
    , ("action", actionLit)
    , ("deadline", deadlineLit)
    ]

modalToText :: DeonticModal -> Text
modalToText = \case
  DMust    -> "MUST"
  DMay     -> "MAY"
  DMustNot -> "SHANT"
  DDo      -> "DO"
```

#### Step 1.7: Validation

The handler validates:

1. **Events without startTime** → `400 "startTime is required when events are provided"`
2. **Events on non-deontic function** → `400 "startTime and events are only valid for functions returning DEONTIC"`
3. **startTime without events on non-deontic function** → same 400

No validation is needed for:

- Empty event list — allowed (returns initial obligation)
- Event timestamp ordering — L4's evaluator handles this naturally (unmatched events are simply skipped)

---

### Phase 2: Rich Obligation Serialization

Phase 1 returns minimal obligation info. Phase 2 adds:

- **Nested obligations** — when `ValROp` (RAND/ROR) composes multiple obligations, return the full tree:

  ```json
  {
    "fnResult": {
      "value": {
        "OBLIGATION": {
          "operator": "AND",
          "children": [
            { "party": "seller", "modal": "MUST", "action": "deliver" },
            { "party": "buyer", "modal": "MUST", "action": "provide address" }
          ]
        }
      }
    }
  }
  ```

- **HENCE/LEST preview** — include the consequence clauses so the caller knows what happens on compliance/breach without inspecting the source

- **Deadline context** — for `WITHIN` clauses, compute the absolute deadline from start time + relative deadline, not just the relative value

---

### Phase 3: Event Discovery Endpoint

Add a single new endpoint that tells callers what events the contract is currently expecting:

```
GET /deployments/{id}/functions/{fn}/expected-events
```

Response:

```json
{
  "expectedEvents": [
    {
      "party": { "type": "Actor", "description": "the patron" },
      "action": {
        "type": "Action",
        "pattern": "order",
        "fields": { "itemName": "STRING", "quantity": "NUMBER" }
      },
      "modal": "MUST",
      "deadline": 10
    }
  ]
}
```

This uses the state graph extraction combined with the current position to determine what events could advance the contract.

---

## Files to Modify

| File                                 | Change                                                                                                                                                                                                           |
| ------------------------------------ | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `jl4-service/src/Backend/Api.hs`     | Add `TraceEvent` type; add `startTime` and `events` fields to `FnArguments`                                                                                                                                      |
| `jl4-service/src/Backend/CodeGen.hs` | Add `generateDeonticWrapper` function                                                                                                                                                                            |
| `jl4-service/src/Backend/Jl4.hs`     | Add `isDeonticReturn`; replace `ValObligation`/`ValBreached`/`ValROp` error cases in `nfToFnLiteral` with proper serialization; add helper functions `breachToFnLiteral`, `obligationToFnLiteral`, `modalToText` |
| `jl4-service/src/DataPlane.hs`       | Add branching logic in `evalFunctionHandler`; add `runDeonticEvaluator`                                                                                                                                          |
| `jl4-service/src/Schema.hs`          | Add `ToSchema TraceEvent`                                                                                                                                                                                        |

### No New Files Required

The implementation fits entirely within the existing jl4-service module structure.

### Test Files

| File                                  | Purpose                                                                 |
| ------------------------------------- | ----------------------------------------------------------------------- |
| `jl4-service/test/IntegrationSpec.hs` | Add deontic evaluation integration tests                                |
| `jl4-service/test/TestData.hs`        | Add/extend test L4 source with deontic rules suitable for trace testing |

---

## Testing Plan

### Unit Tests

1. **`generateDeonticWrapper` produces valid L4 code** — for various event/argument combinations, verify the generated wrapper parses and typechecks
2. **`nfToFnLiteral` handles deontic values** — FULFILLED, BREACH (deadline missed, explicit), obligation, RAND/ROR compositions
3. **Event JSON roundtrip** — verify `TraceEvent` serializes/deserializes correctly for various party/action shapes (nullary constructors, constructors with fields)
4. **`FnArguments` backward compatibility** — verify that requests without `startTime`/`events` still parse correctly (fields are optional)

### Integration Tests

Using the `restaurant1.l4` example (adapted for test data):

1. **All events present → FULFILLED**

   ```
   POST /evaluation with all 5 events → { "fnResult": { "value": "FULFILLED" } }
   ```

2. **Missing final event → OBLIGATION (residual)**

   ```
   POST /evaluation with 4 events (no pay) → { "fnResult": { "value": { "OBLIGATION": { "party": ..., "modal": "MUST", "action": "pay ..." } } } }
   ```

3. **Late event → BREACH**

   ```
   POST /evaluation with pay AT 400 (exceeds WITHIN 10) → { "fnResult": { "value": { "BREACH": { ... } } } }
   ```

4. **Wrong amount → OBLIGATION (unmatched action)**

   ```
   POST /evaluation with pay 11 instead of 12 → { "fnResult": { "value": { "OBLIGATION": { ... } } } }
   ```

5. **Empty events → initial OBLIGATION**

   ```
   POST /evaluation with events: [] → { "fnResult": { "value": { "OBLIGATION": { "party": ..., "modal": "MUST", "action": "order beer ..." } } } }
   ```

6. **No events field at all, deontic function → initial OBLIGATION**

   ```
   POST /evaluation without events field → same as empty events (initial obligation)
   ```

7. **Events on non-deontic function → 400 error**
   ```
   POST /evaluation with events on a BOOLEAN function → 400 "startTime and events are only valid for functions returning DEONTIC"
   ```

Using the `saleContractJL4` test data already in `TestData.hs`:

8. **Simple sale contract: deliver + pay → FULFILLED**
9. **Simple sale contract: deliver only → residual OBLIGATION for buyer**
10. **Simple sale contract: no events within deadline → BREACH**

### Trace Output Tests

11. **With `?trace=full`** — verify reasoning tree is populated for deontic evaluation
12. **With `?trace=full&graphviz=true`** — verify DOT output is included

### Backward Compatibility Tests

13. **Existing non-deontic evaluation** — verify all existing integration tests continue to pass unchanged
14. **Request without new fields** — verify `{ "fnArguments": { ... } }` without `startTime`/`events` works exactly as before

---

## Backward Compatibility

- **No breaking changes** — `startTime` and `events` are optional fields that default to `Nothing`
- Existing callers sending `{ "fnArguments": { ... } }` are completely unaffected
- The `/evaluation` endpoint, the `/evaluation/batch` endpoint, and all other endpoints remain unchanged in their existing behaviour
- The `/state-graphs` endpoints are unaffected

---

## Open Questions

### Q1: How to serialize party/action constructors in events?

**Option A (Proposed):** Use the same JSON format as `FnLiteral` — constructors become `{ "ConstructorName": { "field1": value1 } }` or `{ "ConstructorName": {} }` for nullary. This matches the existing `/evaluation` input format.

**Option B:** A flat string-based format like `"NaturalPerson \"John\""` matching L4 syntax. Simpler but harder to generate programmatically.

**Recommendation:** Option A — consistent with existing API conventions.

### Q2: Should we support EVALTRACE as a code-generation wrapper or as native evaluation?

**Option A (Proposed):** Code generation. Generate L4 source with `EVALTRACE` call, evaluate via `evaluateWrapperInContext`. This reuses all existing infrastructure and handles JSONDECODE of arguments naturally.

**Option B:** Native evaluation. Add a direct evaluation path in `Backend.Jl4` that constructs `ValObligation` and `ValEvent` values programmatically, then invokes the evaluator. More efficient but requires deep integration with the evaluation machinery.

**Recommendation:** Option A for Phase 1 — faster to implement, proven pattern. Option B can be explored in a future optimization pass if performance matters.

### Q3: What if the function has both GIVEN params and returns DEONTIC?

This is the common case (see `restaurant1.l4` — the contract functions take `patron`, `company`, `symtab` as GIVEN parameters). The wrapper must:

1. Decode `fnArguments` into the GIVEN parameter types (existing logic)
2. Construct the function call with those arguments
3. Wrap the function call in `EVALTRACE ... startTime events`

This is handled by composing the existing `generateEvalWrapper` logic with the new deontic wrapper generation.

### Q4: Maximum event list size?

The existing `timeoutAction` wrapper (configurable `--eval-timeout` and `--max-eval-memory-mb`) applies to deontic evaluation too. Long event lists will hit these limits naturally. No additional limit is proposed for Phase 1.

### Q5: Should batch evaluation support events too?

Not in Phase 1. The `/evaluation/batch` endpoint evaluates the same function across multiple input cases. Adding events would mean each batch case could have different event lists, which significantly complicates the request schema. If needed, this can be added later.

---

## Example: Full API Walkthrough

### 1. Deploy the restaurant contract

```bash
zip -r restaurant.zip restaurant1.l4 prelude.l4
curl -X POST http://localhost:8080/deployments \
  -F "sources=@restaurant.zip" -F "id=restaurant"
```

### 2. List functions (discover deontic functions)

```bash
curl http://localhost:8080/deployments/restaurant/functions
# [{ "name": "order beer", "description": "patron orders a beer" }, ...]
```

### 3. View the state graph

```bash
curl http://localhost:8080/deployments/restaurant/functions/order%20beer/state-graphs
# { "graphs": [{ "graphName": "patron orders a beer" }] }
```

### 4. Get the initial obligation (no events)

```bash
curl -X POST \
  'http://localhost:8080/deployments/restaurant/functions/order%20beer/evaluation' \
  -H 'Content-Type: application/json' \
  -d '{
    "fnArguments": {
      "patron": { "NaturalPerson": { "name": "John Doe" } },
      "company": { "Restaurant": { "name": "EatAtJoes, Inc." } },
      "symtab": { "Symbol Table": { "bill": 0, "beers": 0, "potatoes": 0, "log": ["", "nothing has happened yet"] } }
    }
  }'
```

Response — what the contract expects first:

```json
{
  "fnResult": {
    "value": {
      "OBLIGATION": {
        "party": { "NaturalPerson": { "name": "John Doe" } },
        "modal": "MUST",
        "action": { "order": { "itemName": "beer", "quantity": 1 } },
        "deadline": 10
      }
    }
  },
  "reasoning": { "payload": {} },
  "graphviz": null
}
```

### 5. Evaluate the contract with all events

```bash
curl -X POST \
  'http://localhost:8080/deployments/restaurant/functions/order%20beer/evaluation?trace=full' \
  -H 'Content-Type: application/json' \
  -d '{
    "fnArguments": {
      "patron": { "NaturalPerson": { "name": "John Doe" } },
      "company": { "Restaurant": { "name": "EatAtJoes, Inc." } },
      "symtab": { "Symbol Table": { "bill": 0, "beers": 0, "potatoes": 0, "log": ["", "nothing has happened yet"] } }
    },
    "startTime": 1,
    "events": [
      { "party": { "NaturalPerson": { "name": "John Doe" } },
        "action": { "order": { "itemName": "beer", "quantity": 1 } }, "at": 2 },
      { "party": { "NaturalPerson": { "name": "John Doe" } },
        "action": { "order": { "itemName": "potato", "quantity": 1 } }, "at": 12 },
      { "party": { "NaturalPerson": { "name": "John Doe" } },
        "action": { "getBill": {} }, "at": 20 },
      { "party": { "Restaurant": { "name": "EatAtJoes, Inc." } },
        "action": { "showBill": { "amount": 12 } }, "at": 30 },
      { "party": { "NaturalPerson": { "name": "John Doe" } },
        "action": { "pay": { "amount": 12 } }, "at": 40 }
    ]
  }'
```

Response:

```json
{
  "fnResult": {
    "value": "FULFILLED"
  },
  "reasoning": { "payload": { ... } },
  "graphviz": { "dot": "digraph { ... }" }
}
```

### 6. What if the patron doesn't pay?

Same call but remove the last event:

```json
{
  "fnResult": {
    "value": {
      "OBLIGATION": {
        "party": { "NaturalPerson": { "name": "John Doe" } },
        "modal": "MUST",
        "action": { "pay": { "amount": 12 } },
        "deadline": 10
      }
    }
  },
  "reasoning": { "payload": { ... } }
}
```

### 7. What if the patron pays late?

Change last event to `"at": 400`:

```json
{
  "fnResult": {
    "value": {
      "BREACH": {
        "reason": "deadline_missed",
        "party": { "NaturalPerson": { "name": "John Doe" } },
        "action": "pay",
        "deadline": 10,
        "timestamp": 400
      }
    }
  },
  "reasoning": { "payload": { ... } }
}
```
