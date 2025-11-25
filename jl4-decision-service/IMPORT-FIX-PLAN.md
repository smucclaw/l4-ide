# IMPORT Resolution Fix for jl4-decision-service

## Problem

When evaluating L4 functions through the decision service REST API, IMPORT statements fail to resolve, even when all source files are provided via `--sourcePaths`.

**Error**:
```
I could not find a module with this name: anthropicClient
I have tried the following paths:
./anthropicClient.l4,
./anthropicClient.l4,
/Users/mengwong/src/smucclaw/l4-ide/./jl4-core/./libraries/anthropicClient.l4
```

## Root Cause

In `Backend/Jl4.hs`, the `typecheckModule` and `evaluateModule` functions use `Shake.addVirtualFile` to add only the function being evaluated as a virtual file. When the L4 compiler tries to resolve IMPORT statements, it cannot find the imported modules because:

1. Only the entrypoint file content is added as a virtual file
2. The imported modules (anthropicClient.l4, promptLibrary.l4) are not available to the Shake system
3. The module resolution looks for files in the filesystem, not in the decision service's in-memory function registry

## Solution Design

### Approach 1: Add All Source Files as Virtual Files (RECOMMENDED)

**Concept**: When typechecking/evaluating a function, add all source files from `--sourcePaths` as virtual files to the Shake system.

**Implementation**:
1. Modify `Examples.hs::loadL4File` to return file path along with content
2. Store a mapping of `FilePath -> Text` content in `ValidatedFunction` or global context
3. Modify `Backend/Jl4.hs::typecheckModule` to accept this mapping
4. Before typechecking, add all files as virtual files using `Shake.addVirtualFile`

**Changes Required**:
- `Examples.hs`: Store file paths with content
- `Backend/Jl4.hs`: Accept and use file content map
- `Backend/Api.hs`: Update `RunFunction` to include module context

**Benefits**:
- Preserves modular code structure
- Works with IMPORT statements as intended
- No need for standalone files
- Clean separation of concerns

### Approach 2: Inline All Imports (Current Workaround)

**Status**: Already implemented in `cosmetics-api-standalone.l4`

**Drawbacks**:
- Code duplication
- Hard to maintain
- Doesn't solve the root issue

## Implementation Plan

### Step 1: Store File Paths with Content

**File**: `jl4-decision-service/src/Examples.hs`

```haskell
-- Current:
loadL4File :: FilePath -> IO (Maybe (Text, Text, Function))
loadL4File path = do
  -- returns (name, content, function)

-- Proposed:
loadL4File :: FilePath -> IO (Maybe (FilePath, Text, Text, Function))
loadL4File path = do
  -- returns (path, name, content, function)
```

### Step 2: Build Module Context Map

**File**: `jl4-decision-service/src/Examples.hs`

```haskell
type ModuleContext = Map.Map FilePath Text

loadL4Functions :: [FilePath] -> IO (Map.Map Text ValidatedFunction, ModuleContext)
loadL4Functions paths = do
  files <- mapM loadL4File paths
  let validFiles = catMaybes files
      functions = Map.fromList [(name, createValidatedFunction path name content fn)
                                | (path, name, content, fn) <- validFiles]
      moduleContext = Map.fromList [(path, content)
                                    | (path, _, content, _) <- validFiles]
  return (functions, moduleContext)
```

### Step 3: Pass Module Context to Backend

**File**: `jl4-decision-service/src/Backend/Api.hs`

```haskell
-- Add module context to RunFunction
data RunFunction = RunFunction
  { runFunction :: [(Text, Maybe FnLiteral)]
                -> Maybe (Set Text)
                -> ExceptT EvaluatorError IO ResponseWithReason
  , moduleContext :: ModuleContext  -- NEW
  }
```

### Step 4: Update Typecheck/Evaluate to Use Context

**File**: `jl4-decision-service/src/Backend/Jl4.hs`

```haskell
typecheckModule :: (MonadIO m)
                => FilePath
                -> Text
                -> ModuleContext  -- NEW
                -> m ([Text], Maybe Rules.TypeCheckResult)
typecheckModule file input moduleCtx = do
  liftIO $ oneshotL4ActionAndErrors file \nfp -> do
    let uri = normalizedFilePathToUri nfp

    -- Add all module files as virtual files
    forM_ (Map.toList moduleCtx) $ \(path, content) -> do
      let modulePath = toNormalizedFilePath path
      Shake.addVirtualFile modulePath content

    -- Add the main file
    _ <- Shake.addVirtualFile nfp input
    Shake.use Rules.TypeCheck uri
```

Similar changes for `evaluateModule`.

### Step 5: Update createFunction

**File**: `jl4-decision-service/src/Backend/Jl4.hs`

```haskell
createFunction ::
  FunctionDeclaration ->
  Text ->
  ModuleContext ->  -- NEW
  RunFunction
createFunction fnDecl fnImpl moduleCtx =
  RunFunction
    { runFunction = \params' _outFilter -> do
        (initErrs, mTcRes) <- typecheckModule file fnImpl moduleCtx  -- Pass context
        -- ... rest of implementation
    , moduleContext = moduleCtx  -- Store for later use
    }
```

### Step 6: Update Application Startup

**File**: `jl4-decision-service/src/Application.hs`

```haskell
defaultMain :: IO ()
defaultMain = do
  Options{port, serverName, sourcePaths, crudServerName} <- execParser opts

  l4Files <- expandSourcePaths sourcePaths

  -- Get both functions AND module context
  (l4Functions, moduleCtx) <- Examples.loadL4Functions l4Files

  -- Store module context in application state for use during evaluation
  dbRef <- newTVarIO (Examples.functionSpecs <> l4Functions)
  ctxRef <- newTVarIO moduleCtx  -- NEW

  -- Pass to app
  let initialState = MkAppEnv dbRef ctxRef crudServerName mgr  -- Updated
  -- ...
```

## Testing Plan

1. **Unit Test**: Test `typecheckModule` with multiple virtual files
2. **Integration Test**: Test full API call with IMPORT statements
3. **Regression Test**: Ensure existing functionality still works

**Test Case**:
```bash
# Should succeed with modular files
curl -X POST "http://localhost:8080/functions/evaluate%20cosmetics%20claim/evaluation" \
  -H "Content-Type: application/json" \
  -d '{"fnEvalBackend":"jl4","fnArguments":{"claim_text":"test","test_type":3}}'

# Expected: Successful evaluation (not IMPORT error)
```

## Migration Path

1. Implement changes in branch `mengwong/decision-imports`
2. Run `cabal test all` to ensure no regressions
3. Test with thailand-cosmetics modular files
4. Update documentation
5. Submit PR for review

## Alternative Considered: Filesystem-Based Resolution

**Concept**: Write all source files to a temporary directory and use real filesystem paths.

**Rejected Because**:
- More complex (temp file management)
- Slower (filesystem I/O)
- Virtual files are the intended mechanism in Shake
- Doesn't align with LSP architecture

## Benefits of This Fix

1. ✅ IMPORT statements work as expected
2. ✅ Modular code organization supported
3. ✅ Code reusability across functions
4. ✅ Cleaner API (no need for standalone files)
5. ✅ Consistent with how jl4-cli handles imports
