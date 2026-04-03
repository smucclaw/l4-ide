# Specification: Performance Optimization for Decision Service

## Executive Summary

The decision service currently re-parses and re-typechecks L4 source code on **every evaluation request**, even when the source hasn't changed. This is the primary performance bottleneck.

The fix is to **precompile L4 modules once** at load time and keep the compiled representation hot in memory, then evaluate queries against the cached compiled module.

## Current Architecture (Slow)

```
Request 1: evaluate myFunc(x=5)
  → Parse L4 source text           [~10ms]
  → Typecheck entire module        [~50ms]
  → Build function application AST [~1ms]
  → Evaluate                       [~5ms]
  Total: ~66ms

Request 2: evaluate myFunc(x=10)
  → Parse L4 source text           [~10ms]  ← WASTED
  → Typecheck entire module        [~50ms]  ← WASTED
  → Build function application AST [~1ms]
  → Evaluate                       [~5ms]
  Total: ~66ms
```

For 1000 requests against the same function: **66 seconds** of which 60 seconds is redundant parsing/typechecking.

## Proposed Architecture (Fast)

```
Load time (once):
  → Parse L4 source text           [~10ms]
  → Typecheck entire module        [~50ms]
  → Cache TypeCheckResult          [stored in RAM]

Request 1: evaluate myFunc(x=5)
  → Build Expr Resolved for call   [~1ms]
  → Evaluate against cached module [~5ms]
  Total: ~6ms

Request 2: evaluate myFunc(x=10)
  → Build Expr Resolved for call   [~1ms]
  → Evaluate against cached module [~5ms]
  Total: ~6ms
```

For 1000 requests against the same function: **6 seconds** - a **10x improvement**.

## Key Insight

The function `execEvalExprInContextOfModule` already exists in `L4.EvaluateLazy`:

```haskell
execEvalExprInContextOfModule
  :: EntityInfo
  -> Expr Resolved
  -> (Environment, Module Resolved)
  -> IO (Maybe EvalDirectiveResult)
```

This evaluates an expression against a pre-compiled module without re-parsing or re-typechecking. The decision service just needs to use it.

## Implementation

### Phase 1: Precompile at Load Time

#### Current `ValidatedFunction` Structure

```haskell
data ValidatedFunction = ValidatedFunction
  { fnImpl :: !Function                      -- Metadata
  , fnEvaluator :: !(Map EvalBackend RunFunction)  -- Closure over raw Text
  }
```

The `RunFunction` closure captures raw `Text` and re-parses on every call.

#### New `ValidatedFunction` Structure

```haskell
data ValidatedFunction = ValidatedFunction
  { fnImpl :: !Function
  , fnEvaluator :: !(Map EvalBackend RunFunction)
  , fnCompiled :: !(Maybe CompiledModule)  -- NEW: cached compilation
  }

-- | Pre-compiled L4 module ready for fast evaluation
data CompiledModule = CompiledModule
  { compiledModule :: !(Module Resolved)
  , compiledEnvironment :: !Environment
  , compiledEntityInfo :: !EntityInfo
  , compiledDecide :: !(Decide Resolved)  -- The target function
  , compiledRecordMap :: !RecordMap       -- For type-directed deserialization
  }
```

### Phase 2: Compile Once at Registration

**File:** `jl4-decision-service/src/Backend/Jl4.hs`

```haskell
-- | Precompile an L4 module for fast repeated evaluation
precompileModule
  :: FilePath
  -> Text
  -> ModuleContext
  -> RawName
  -> IO (Either Text CompiledModule)
precompileModule filepath source moduleContext funName = do
  -- Parse and typecheck once
  (errs, mTcRes) <- typecheckModule filepath source moduleContext
  case mTcRes of
    Nothing -> pure $ Left (mconcat errs)
    Just tcRes -> do
      -- Extract what we need for evaluation
      case runExcept (getFunctionDefinition funName tcRes.module') of
        Left err -> pure $ Left (show err)
        Right decide -> do
          -- Get environment from dependencies
          let env = buildEnvironmentFromDeps tcRes
          pure $ Right CompiledModule
            { compiledModule = tcRes.module'
            , compiledEnvironment = env
            , compiledEntityInfo = tcRes.entityInfo
            , compiledDecide = decide
            , compiledRecordMap = getAllRecords tcRes.module'
            }
```

### Phase 3: Fast Evaluation Path

**File:** `jl4-decision-service/src/Backend/Jl4.hs`

```haskell
-- | Evaluate using precompiled module (fast path)
evaluateWithCompiled
  :: CompiledModule
  -> [(Text, FnLiteral)]
  -> TraceLevel
  -> ExceptT EvaluatorError IO ResponseWithReason
evaluateWithCompiled compiled params traceLevel = do
  -- Build the function application expression (already Resolved)
  appliedExpr <- buildResolvedFunApp
    compiled.compiledDecide
    compiled.compiledRecordMap
    params

  -- Evaluate directly against cached module - NO RE-PARSING!
  result <- liftIO $ execEvalExprInContextOfModule
    compiled.compiledEntityInfo
    appliedExpr
    (compiled.compiledEnvironment, compiled.compiledModule)

  case result of
    Nothing -> throwError $ InterpreterError "Evaluation produced no result"
    Just evalResult -> handleEvalResult evalResult traceLevel

-- | Build function application as Resolved AST (not text!)
buildResolvedFunApp
  :: Decide Resolved
  -> RecordMap
  -> [(Text, FnLiteral)]
  -> ExceptT EvaluatorError IO (Expr Resolved)
buildResolvedFunApp decide recordMap params = do
  -- Similar to current buildEvalFunApp but produces Expr Resolved
  -- instead of Expr Name, avoiding need for re-typechecking
  ...
```

### Phase 4: Update `createFunction` to Use Fast Path

```haskell
createFunction ::
  FilePath ->
  FunctionDeclaration ->
  Text ->
  ModuleContext ->
  IO (Either Text ValidatedFunction)  -- Now returns IO for precompilation
createFunction filepath fnDecl fnImpl moduleContext = do
  -- Precompile at creation time
  compiledResult <- precompileModule filepath fnImpl moduleContext funRawName

  case compiledResult of
    Left err -> pure $ Left err
    Right compiled -> pure $ Right ValidatedFunction
      { fnImpl = fnDecl
      , fnEvaluator = Map.singleton JL4 $ RunFunction
          { runFunction = \params outputFilter traceLevel ->
              evaluateWithCompiled compiled params traceLevel
          }
      , fnCompiled = Just compiled
      }
 where
  funRawName = mkNormalName fnDecl.name
```

### Phase 5: Parallel Batch Evaluation

Once we have precompiled modules, batch evaluation can be parallelized:

**File:** `jl4-decision-service/src/Server.hs`

```haskell
batchFunctionHandler :: String -> BatchRequest -> AppM BatchResponse
batchFunctionHandler name' batchArgs = do
  functionsTVar <- asks (.functionDatabase)
  functions <- liftIO $ readTVarIO functionsTVar
  case Map.lookup name functions of
    Nothing -> throwError err404
    Just fnImpl -> do
      -- Parallel evaluation using precompiled module
      (execTime, responses) <- stopwatchM $
        forConcurrently batchArgs.cases $ \inputCase -> do  -- parallel!
          let args = Map.assocs $ fmap Just inputCase.attributes
          r <- runEvaluatorFor Nothing fnImpl args outputFilter TraceNone
          pure (inputCase.id, r)
      ...
```

## Memory Considerations

### What Gets Cached

Per function:

- `Module Resolved` - the full AST with resolved names (~100KB - 1MB depending on complexity)
- `Environment` - evaluation environment (~10KB - 100KB)
- `EntityInfo` - type information (~10KB - 50KB)
- `RecordMap` - record field mappings (~1KB - 10KB)

Estimated per-function memory: **~200KB - 2MB**

For 1000 functions: **~200MB - 2GB**

This is acceptable for a dedicated decision service.

### Cache Invalidation

The compiled module cache should be invalidated when:

1. The L4 source file changes (file watcher or explicit reload endpoint)
2. Any imported module changes (dependency tracking)
3. The function is deleted

**File:** `jl4-decision-service/src/Server.hs`

```haskell
-- | Reload a function, recompiling from source
reloadFunction :: Text -> AppM ()
reloadFunction name = do
  functionsTVar <- asks (.functionDatabase)
  -- Re-read source, recompile, update cache
  ...

-- | Endpoint for explicit reload
reloadHandler :: AppM ()
reloadHandler = do
  -- Reload all functions
  ...
```

## Alternative: Persistent Shake Session

Instead of manual caching, we could maintain a persistent Shake session:

```haskell
data AppEnv = MkAppEnv
  { functionDatabase :: TVar (Map Text ValidatedFunction)
  , baseUrl :: BaseUrl
  , manager :: Manager
  , shakeSession :: ShakeSession  -- NEW: persistent Shake DB
  }
```

Then use Shake's built-in caching for `TypeCheck` and `EvaluateLazy` rules.

**Pros:**

- Reuses existing Shake infrastructure
- Automatic dependency tracking
- Incremental recompilation

**Cons:**

- More complex integration
- Shake session management
- May need to modify rule definitions

## Migration Path

### Phase 1: Add Precompilation (Low Risk)

1. Add `fnCompiled` field to `ValidatedFunction`
2. Implement `precompileModule`
3. Try precompilation at load time; fall back to current behavior on failure
4. Add metrics to compare fast vs slow paths

### Phase 2: Fast Evaluation Path (Medium Risk)

1. Implement `evaluateWithCompiled`
2. Implement `buildResolvedFunApp`
3. Route requests through fast path when `fnCompiled` is available
4. Keep slow path as fallback

### Phase 3: Parallel Batch (Low Risk)

1. Switch `forM` to `forConcurrently` for batch evaluation
2. Add concurrency limits to prevent resource exhaustion

### Phase 4: Remove Slow Path (After Validation)

1. Remove text-concatenation approach
2. Require precompilation for all functions
3. Clean up dead code

## Benchmarking Plan

### Metrics to Track

1. **Request latency** (p50, p95, p99)
2. **Throughput** (requests/second)
3. **Memory usage** (per function, total)
4. **Batch processing time** (1000 cases)

### Benchmark Scenarios

1. **Single function, repeated calls**: Same function, varying inputs
2. **Many functions, single call each**: Load testing function lookup
3. **Batch evaluation**: 100, 1000, 10000 cases
4. **Complex vs simple**: Functions with many dependencies vs standalone

### Expected Improvements

| Scenario              | Current | After Optimization | Improvement |
| --------------------- | ------- | ------------------ | ----------- |
| Single eval (simple)  | ~50ms   | ~5ms               | 10x         |
| Single eval (complex) | ~200ms  | ~20ms              | 10x         |
| Batch 1000 cases      | ~50s    | ~5s (sequential)   | 10x         |
| Batch 1000 cases      | ~50s    | ~0.5s (parallel)   | 100x        |

## Summary

The core optimization is simple: **compile once, evaluate many times**.

The infrastructure already exists (`execEvalExprInContextOfModule`). The decision service just needs to:

1. Cache `TypeCheckResult` at function registration time
2. Build `Expr Resolved` directly (not via text concatenation)
3. Call `execEvalExprInContextOfModule` instead of re-parsing

This should yield a **10x improvement** for sequential requests and **100x improvement** for parallelized batch requests.

## References

- Issue #635: Critical L4 Decision Service Improvements (Item 8)
- `jl4-core/src/L4/EvaluateLazy.hs`: `execEvalExprInContextOfModule` (line 390)
- `jl4-lsp/src/LSP/L4/Rules.hs`: `TypeCheckResult` structure
- `jl4-decision-service/src/Backend/Jl4.hs`: Current (slow) implementation
