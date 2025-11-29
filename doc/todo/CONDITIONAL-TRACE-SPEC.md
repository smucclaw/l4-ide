# Specification: Conditional Decision Trace Returns

## Executive Summary

This document specifies a mechanism for clients to control whether the decision service returns the full evaluation trace (reasoning tree) or just the result value. This optimization reduces response payload size for clients that don't need the trace.

## Motivation

### Current Behavior

Every evaluation request returns both:
1. **Result value**: The computed answer (typically small)
2. **Reasoning tree**: Full evaluation trace showing how the result was derived (potentially large)

```json
{
  "values": [["result", "TRUE"]],
  "reasoning": {
    "payload": {
      "payload": {
        "exampleCode": ["isEligible 25"],
        "explanation": ["Result: TRUE"]
      },
      "children": [
        {
          "payload": { "exampleCode": ["25 >= 18"], "explanation": ["Result: TRUE"] },
          "children": [
            { "payload": { "exampleCode": ["25"], "explanation": ["Result: 25"] }, "children": [] },
            { "payload": { "exampleCode": ["18"], "explanation": ["Result: 18"] }, "children": [] }
          ]
        }
      ]
    }
  }
}
```

### Problem

For complex evaluations, the reasoning tree can be **orders of magnitude larger** than the result itself:
- Simple boolean result: ~10 bytes
- Reasoning tree for that result: 10KB - 1MB+

Many use cases only need the result:
- Batch processing thousands of cases
- Real-time API calls where latency matters
- Mobile clients on limited bandwidth
- Integration with systems that don't display reasoning

### Desired Behavior

Clients can request:
1. **Result only** (no trace) - smallest, fastest
2. **Result + trace** (current behavior) - full debugging/explanation capability

## Design

### Option A: HTTP Header (Recommended)

Use a custom HTTP header to control trace inclusion:

```
X-L4-Trace: none | full
```

**Examples:**

```bash
# No trace (result only)
curl -X POST /functions/myFunc/evaluation \
  -H "X-L4-Trace: none" \
  -d '{"fnArguments": {"x": 5}}'

# Full trace (current behavior, also the default)
curl -X POST /functions/myFunc/evaluation \
  -H "X-L4-Trace: full" \
  -d '{"fnArguments": {"x": 5}}'
```

**Pros:**
- Clean separation of control (header) from data (body)
- Doesn't change the request body schema
- Easy to add globally via API gateway/proxy
- RESTful - headers are the standard place for request modifiers

**Cons:**
- Some clients make headers harder to set than body fields

### Option B: Query Parameter

```
POST /functions/myFunc/evaluation?trace=none
POST /functions/myFunc/evaluation?trace=full
```

**Pros:**
- Very easy to use in browsers and simple clients
- Visible in logs

**Cons:**
- Mixes control with resource identification
- Can be cached incorrectly if not careful

### Option C: Request Body Field

```json
{
  "fnArguments": {"x": 5},
  "trace": "none"
}
```

**Pros:**
- Self-contained request
- Works with any HTTP client

**Cons:**
- Changes the request schema
- Mixes control with data

### Recommendation

**Use Option A (HTTP Header)** as the primary mechanism, with Option B (query parameter) as a fallback for clients that can't set headers easily.

The header takes precedence if both are provided.

## Response Formats

### With Trace (`X-L4-Trace: full` or default)

No change from current behavior:

```json
{
  "values": [["result", "TRUE"]],
  "reasoning": {
    "payload": {
      "payload": {
        "exampleCode": ["isEligible 25"],
        "explanation": ["Result: TRUE"]
      },
      "children": [...]
    }
  }
}
```

### Without Trace (`X-L4-Trace: none`)

The `reasoning` field contains an empty tree:

```json
{
  "values": [["result", "TRUE"]],
  "reasoning": {
    "payload": {
      "payload": {
        "exampleCode": [],
        "explanation": []
      },
      "children": []
    }
  }
}
```

**Alternative**: Omit `reasoning` entirely when not requested. However, this changes the response schema which may break clients expecting the field. The empty tree approach maintains schema compatibility.

## Implementation

### Step 1: Define Trace Level Type

**File:** `jl4-decision-service/src/Server.hs`

```haskell
-- | Control how much trace information to return
data TraceLevel
  = TraceNone  -- ^ No trace, result only
  | TraceFull  -- ^ Full evaluation trace
  deriving (Show, Eq, Ord, Enum, Bounded)

instance FromHttpApiData TraceLevel where
  parseQueryParam t = case Text.toLower t of
    "none" -> Right TraceNone
    "full" -> Right TraceFull
    _ -> Left $ "Invalid trace level: " <> t <> ". Expected: none, full"

-- | Parse trace level from header value
parseTraceHeader :: Maybe ByteString -> TraceLevel
parseTraceHeader Nothing = TraceFull  -- Default to full trace
parseTraceHeader (Just bs) = case Text.toLower (decodeUtf8 bs) of
  "none" -> TraceNone
  "full" -> TraceFull
  _ -> TraceFull  -- Default to full on invalid value
```

### Step 2: Add Header to API

**File:** `jl4-decision-service/src/Server.hs`

Modify the evaluation endpoint to accept the header:

```haskell
data SingleFunctionApi' mode = SingleFunctionApi
  { -- ... other fields ...
  , evalFunction ::
      mode
        :- "evaluation"
          :> Summary "Evaluate a function with arguments"
          :> Header "X-L4-Trace" Text  -- NEW
          :> QueryParam "trace" TraceLevel  -- NEW (fallback)
          :> ReqBody '[JSON] FnArguments
          :> OperationId "evalFunction"
          :> Post '[JSON] SimpleResponse
  -- ...
  }
```

### Step 3: Modify Handler

**File:** `jl4-decision-service/src/Server.hs`

```haskell
evalFunctionHandler
  :: String
  -> Maybe Text      -- X-L4-Trace header
  -> Maybe TraceLevel -- trace query param
  -> FnArguments
  -> AppM SimpleResponse
evalFunctionHandler name' mTraceHeader mTraceParam args = do
  let traceLevel = determineTraceLevel mTraceHeader mTraceParam
  functionsTVar <- asks (.functionDatabase)
  functions <- liftIO $ readTVarIO functionsTVar
  let fnArgs = Map.assocs args.fnArguments
      eval fnImpl = runEvaluatorFor args.fnEvalBackend fnImpl fnArgs Nothing traceLevel
  case Map.lookup name functions of
    Nothing -> withUUIDFunction name eval (\k -> throwError (k err404))
    Just fnImpl -> eval fnImpl
 where
  name = Text.pack name'

-- | Determine trace level from header and query param
-- Header takes precedence over query param
determineTraceLevel :: Maybe Text -> Maybe TraceLevel -> TraceLevel
determineTraceLevel (Just headerVal) _ = parseTraceHeader (Just $ encodeUtf8 headerVal)
determineTraceLevel Nothing (Just paramVal) = paramVal
determineTraceLevel Nothing Nothing = TraceFull  -- Default
```

### Step 4: Pass Trace Level to Evaluator

**File:** `jl4-decision-service/src/Server.hs`

```haskell
runEvaluatorFor
  :: Maybe EvalBackend
  -> ValidatedFunction
  -> [(Text, Maybe FnLiteral)]
  -> Maybe (Set Text)
  -> TraceLevel  -- NEW parameter
  -> AppM SimpleResponse
runEvaluatorFor engine validatedFunc args outputFilter traceLevel = do
  eval <- evaluationEngine evalBackend validatedFunc
  evaluationResult <-
    timeoutAction $
      runExceptT
        ( eval.runFunction
            args
            outputFilter
            traceLevel  -- Pass to runFunction
        )
  case evaluationResult of
    Left err -> pure $ SimpleError err
    Right r -> pure $ SimpleResponse r
 where
  evalBackend = Maybe.fromMaybe JL4 engine
```

### Step 5: Modify RunFunction Type

**File:** `jl4-decision-service/src/Backend/Api.hs`

```haskell
-- | Control how much trace information to return
data TraceLevel
  = TraceNone
  | TraceFull
  deriving (Show, Eq, Ord, Enum, Bounded, Generic)

newtype RunFunction = RunFunction
  { runFunction ::
      [(Text, Maybe FnLiteral)] ->
      Maybe (Set Text) ->
      TraceLevel ->  -- NEW parameter
      ExceptT EvaluatorError IO ResponseWithReason
  }
```

### Step 6: Implement Conditional Trace in Backend

**File:** `jl4-decision-service/src/Backend/Jl4.hs`

```haskell
createFunction ::
  FilePath ->
  FunctionDeclaration ->
  Text ->
  ModuleContext ->
  RunFunction
createFunction filepath fnDecl fnImpl moduleContext =
  RunFunction
    { runFunction = \params' _outFilter traceLevel -> do
        (initErrs, mTcRes) <- typecheckModule filepath fnImpl moduleContext

        tcRes <- case mTcRes of
          Nothing -> throwError $ InterpreterError (mconcat initErrs)
          Just tcRes -> pure tcRes

        params <- assumeNoUnknowns params'

        funExpr <- getFunctionDefinition funRawName tcRes.module'

        let recordMap = getAllRecords tcRes.module'
        appliedFunExpr <- buildEvalFunApp funRawName funExpr recordMap params

        let
          -- Choose directive based on trace level
          directive = case traceLevel of
            TraceNone -> mkEval appliedFunExpr      -- #EVAL (no trace)
            TraceFull -> mkEvalTrace appliedFunExpr -- #EVALTRACE (with trace)

          l4InputWithEval =
            Text.unlines
              [ fnImpl
              , prettyLayout $ mkTopDeclDirective directive
              ]

        (errs, mEvalRes) <- evaluateModule filepath l4InputWithEval moduleContext

        case mEvalRes of
          Nothing -> throwError $ InterpreterError (mconcat errs)
          Just [Eval.MkEvalDirectiveResult{result, trace}] -> case result of
            Eval.Assertion _ -> throwError $ InterpreterError $ "L4: Got an assertion instead of a normal result."
            Eval.Reduction (Left evalExc) -> throwError $ InterpreterError $ Text.show evalExc
            Eval.Reduction (Right val) -> do
              r <- nfToFnLiteral val
              pure $
                ResponseWithReason
                  { values = [("result", r)]
                  , reasoning = case traceLevel of
                      TraceNone -> emptyTree  -- Don't build tree
                      TraceFull -> buildReasoningTree trace
                  }
          Just [] -> throwError $ InterpreterError "L4: No #EVAL found in the program."
          Just _xs -> throwError $ InterpreterError "L4: More than ONE #EVAL found in the program."
    }
 where
  funRawName = mkNormalName fnDecl.name

-- | Create #EVAL directive (no trace)
mkEval :: Expr n -> Directive n
mkEval = LazyEval emptyAnno

-- | Create #EVALTRACE directive (with trace)
mkEvalTrace :: Expr n -> Directive n
mkEvalTrace = LazyEvalTrace emptyAnno
```

### Step 7: Update Batch Endpoint

The batch endpoint should also support trace control:

**File:** `jl4-decision-service/src/Server.hs`

```haskell
data SingleFunctionApi' mode = SingleFunctionApi
  { -- ...
  , batchFunction ::
      mode
        :- "batch"
          :> Summary "Run a function using a batch of arguments"
          :> Header "X-L4-Trace" Text  -- NEW
          :> QueryParam "trace" TraceLevel  -- NEW
          :> ReqBody '[JSON] BatchRequest
          :> Post '[JSON] BatchResponse
  }
```

For batch operations, `TraceNone` is likely the common case since processing thousands of records with full traces would be prohibitively expensive.

## OpenAPI Schema Update

**File:** `jl4-decision-service/src/Schema.hs`

Update the OpenAPI schema to document the new header:

```haskell
-- Add to schema generation
traceHeaderSchema :: OpenApi.Header
traceHeaderSchema = mempty
  & #description ?~ "Control evaluation trace detail level"
  & #schema ?~ OpenApi.Inline (mempty
      & #type ?~ OpenApi.OpenApiString
      & #enum ?~ ["none", "full"]
      & #default_ ?~ "full"
    )
```

## Testing Plan

### Unit Tests

1. **Header parsing**:
   ```haskell
   parseTraceHeader Nothing == TraceFull
   parseTraceHeader (Just "none") == TraceNone
   parseTraceHeader (Just "NONE") == TraceNone  -- case insensitive
   parseTraceHeader (Just "full") == TraceFull
   parseTraceHeader (Just "invalid") == TraceFull  -- default
   ```

2. **Trace level precedence**:
   ```haskell
   determineTraceLevel (Just "none") (Just TraceFull) == TraceNone  -- header wins
   determineTraceLevel Nothing (Just TraceNone) == TraceNone
   determineTraceLevel Nothing Nothing == TraceFull
   ```

### Integration Tests

1. **Result-only response**:
   - Send request with `X-L4-Trace: none`
   - Verify response has empty reasoning tree
   - Verify result value is correct

2. **Full trace response**:
   - Send request with `X-L4-Trace: full`
   - Verify response has populated reasoning tree

3. **Default behavior**:
   - Send request without header
   - Verify response has full trace (backward compatible)

4. **Query parameter fallback**:
   - Send request with `?trace=none`
   - Verify response has empty reasoning tree

### Performance Tests

1. **Measure response size** with and without trace for various complexity levels
2. **Measure latency** improvement when trace is disabled
3. **Batch throughput** comparison with trace disabled

## Migration

### Backward Compatibility

- **Default is full trace**: Existing clients get same behavior
- **Response schema unchanged**: `reasoning` field always present (may be empty)
- **No breaking changes**: All existing requests continue to work

### Client Updates

Clients wanting smaller responses simply add the header:

```bash
# Before (no change needed)
curl -X POST /functions/myFunc/evaluation \
  -d '{"fnArguments": {"x": 5}}'

# After (opt-in to smaller response)
curl -X POST /functions/myFunc/evaluation \
  -H "X-L4-Trace: none" \
  -d '{"fnArguments": {"x": 5}}'
```

## Future Extensions

### Partial Trace

A `partial` trace level could return only the top N levels of the reasoning tree:

```
X-L4-Trace: partial
X-L4-Trace-Depth: 2
```

This would give a summary of reasoning without the full depth, useful for:
- Showing "here's why" without overwhelming detail
- Debugging without full trace overhead

### Trace Filtering

Filter trace to specific expressions or variables:

```
X-L4-Trace-Filter: variableName
```

### Streaming Trace

For very long-running evaluations, stream trace incrementally:

```
X-L4-Trace: stream
```

## References

- Issue #635: Critical L4 Decision Service Improvements (Item 1)
- `jl4-decision-service/src/Backend/Api.hs`: Response types
- `jl4-decision-service/src/Backend/Jl4.hs`: Trace generation
- `jl4-decision-service/src/Server.hs`: API handlers
