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

**IDENTIFIED**: The module resolver in `jl4-lsp/src/LSP/L4/Rules.hs` (lines 304-308) uses `liftIO (doesFileExist pth)` to check if imported modules exist. This is a **direct filesystem check** that cannot find virtual files added via `Shake.addVirtualFile`.

Specifically:

1. Only the entrypoint file content is added as a virtual file
2. The imported modules (anthropicClient.l4, promptLibrary.l4) are added as virtual files but...
3. **The module resolver checks the filesystem with `doesFileExist`, not Shake's virtual file system**
4. Virtual files only exist in Shake's memory, so `doesFileExist` returns `False` for them

This is a fundamental architectural limitation: module resolution happens outside of Shake's awareness and uses direct filesystem I/O instead of Shake rules.

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

1. ✅ Implement changes in branch `mengwong/decision-imports`
2. ⏳ Run `cabal test all` to ensure no regressions
3. ✅ Test with multi-file modular L4 projects - **SUCCESS!**
4. ⏳ Update documentation
5. ⏳ Submit PR for review

## Fix Implementation Status - COMPLETE ✅

**Date**: 2025-11-26

### Changes Made

1. **Backend/Jl4.hs**:

   - Updated `createFunction` signature to accept `FilePath` as first parameter
   - Changed `typecheckModule` and `evaluateModule` calls to use actual filepath
   - Removed fake filename generation from WHERE clause

2. **Examples.hs**:

   - Modified `createValidatedFunction` to pass actual filepath to `Jl4.createFunction`
   - Updated builtin examples to pass fake filepaths ("compute_qualifies.l4", "vermin_and_rodent.l4")

3. **Server.hs**:
   - Updated `validateImplementation` to generate filepath from function name
   - Updated session function creation to use function name as filepath

### Test Results

Successfully tested with modular cosmetics-api.l4 using IMPORT statements:

```bash
# Server started with multiple modules:
--sourcePaths path/to/file1.l4
--sourcePaths path/to/file2.l4
--sourcePaths path/to/main.l4
```

**Result**: Full evaluation trace returned successfully. No IMPORT errors. All imported functions resolved correctly.

### Why It Works

By using the actual source file path instead of a fake filename derived from the function name:

1. `oneshotL4ActionAndErrors` sets `curDir` to the actual source directory
2. The module resolver looks for imports relative to that directory
3. Sibling files like `anthropicClient.l4` and `promptLibrary.l4` are found naturally via filesystem checks
4. System libraries (prelude, daydate) continue to work via library path fallback

## Alternative Considered: Filesystem-Based Resolution

**Concept**: Write all source files to a temporary directory and use real filesystem paths.

**Rejected Because**:

- More complex (temp file management)
- Slower (filesystem I/O)
- Virtual files are the intended mechanism in Shake
- Doesn't align with LSP architecture

## Actual Fix Required (Beyond Current Scope)

The proper fix requires modifying `jl4-lsp/src/LSP/L4/Rules.hs` to make module resolution Shake-aware:

```haskell
-- Current code (lines 304-308):
let guardExists pth = do
      guard =<< liftIO (doesFileExist pth)  -- ❌ Checks real filesystem only
      pure pth

asum $ guardExists <$> paths

-- Proposed fix:
let guardExists pth = do
      let nfp = toNormalizedFilePath pth
      -- First check if file exists as a virtual file in Shake
      mVirtual <- Shake.getVirtualFile nfp
      case mVirtual of
        Just _content -> pure pth  -- Found as virtual file
        Nothing -> do              -- Fall back to filesystem check
          guard =<< liftIO (doesFileExist pth)
          pure pth

asum $ guardExists <$> paths
```

This would require:

1. Access to Shake's virtual file system from within the Rules
2. Modifying the module resolution logic to check virtual files first
3. Testing to ensure system libraries (prelude, daydate) still resolve correctly

## Benefits of Proper Fix

1. ✅ IMPORT statements work as expected
2. ✅ Modular code organization supported
3. ✅ Code reusability across functions
4. ✅ Cleaner API (no need for standalone files)
5. ✅ Consistent with how jl4-cli handles imports
6. ✅ System libraries (prelude, daydate) continue to work

## Automatic Import Discovery Enhancement - COMPLETE ✅

**Date**: 2025-11-26

### Problem

While the filepath-based fix allowed IMPORT statements to work, it required users to explicitly list all dependencies on the command line:

```bash
--sourcePaths ~/src/.../anthropicClient.l4 \
--sourcePaths ~/src/.../promptLibrary.l4 \
--sourcePaths ~/src/.../cosmetics-api.l4
```

This is inconvenient and doesn't match the behavior of standard module systems in other programming languages.

### Solution: Recursive Import Discovery

Implemented automatic dependency resolution that parses IMPORT statements and recursively discovers all required files. Users can now specify only the main file:

```bash
--sourcePaths ~/src/.../cosmetics-api.l4
```

The system automatically discovers and loads `anthropicClient.l4` and `promptLibrary.l4`.

### Implementation Details

**File**: `jl4-decision-service/src/Examples.hs`

#### 1. Extract IMPORT Statements

```haskell
-- | Extract IMPORT statements from L4 file content
extractImports :: Text -> [Text]
extractImports content =
  [ T.strip $ T.drop 6 line  -- Remove "IMPORT" prefix and trim
  | line <- T.lines content
  , T.strip line /= ""
  , "IMPORT" `T.isPrefixOf` T.strip line
  ]
```

Parses L4 source files line-by-line to find `IMPORT` statements and extract module names.

#### 2. Recursive Dependency Discovery

```haskell
-- | Recursively discover all files needed (following IMPORT statements)
discoverAllFiles :: Set.Set FilePath -> [FilePath] -> IO (Set.Set FilePath)
discoverAllFiles discovered [] = return discovered
discoverAllFiles discovered (path:paths)
  | path `Set.member` discovered = discoverAllFiles discovered paths
  | otherwise = do
      putStrLn $ "* Discovering dependencies for: " <> path
      fileExists <- doesFileExist path
      if not fileExists
        then do
          putStrLn $ "  WARNING: File not found: " <> path
          discoverAllFiles discovered paths
        else do
          content <- TIO.readFile path
          let imports = extractImports content
              dir = takeDirectory path
              importPaths = [dir </> T.unpack imp <> ".l4" | imp <- imports]
          putStrLn $ "  Found imports: " <> show imports
          let newDiscovered = Set.insert path discovered
          discoverAllFiles newDiscovered (importPaths ++ paths)
```

Key features:

- Uses `Set.Set FilePath` to track discovered files and avoid duplicates
- Recursively processes each discovered import
- Resolves import paths relative to the importing file's directory
- Provides helpful debug output showing the discovery process
- Gracefully handles missing files with warnings

#### 3. Modified loadL4Functions

```haskell
loadL4Functions :: [FilePath] -> IO (Map.Map Text ValidatedFunction, ModuleContext)
loadL4Functions paths = do
  when (null paths) $ do
    putStrLn "* to load L4 functions from disk, run with --sourcePaths"
    putStrLn "  for example, --sourcePaths ../doc/tutorial-code/fruit.l4"
    putStrLn "  each .l4 file needs a matching .yaml definition"

  -- Automatically discover all files including imports
  allFiles <- discoverAllFiles Set.empty paths
  let allFilePaths = Set.toList allFiles
  putStrLn $ "* Auto-discovered " <> show (Set.size allFiles) <> " total files (including imports)"

  files <- mapM loadL4File allFilePaths
  unless (null files) $ putStrLn $ "* Loaded " <> show (length files) <> " .l4 files"
  let
    validFiles = catMaybes files
    functions = Map.fromList [(name, createValidatedFunction path name content fn moduleContext) | (path, name, content, fn) <- validFiles]
    moduleContext = Map.fromList [(path, content) | (path, _, content, _) <- validFiles]
  return (functions, moduleContext)
```

Now automatically discovers all dependencies before loading files.

### Test Results

Successfully tested with multi-file L4 projects:

```bash
# Server started with ONLY the main file
cd ~/src/smucclaw/l4-ide && cabal run jl4-decision-service-exe -- \
  --port 8080 \
  --serverName http://localhost:8080/ \
  --sourcePaths path/to/main.l4

# Server output:
* Discovering dependencies for: /path/to/main.l4
  Found imports: ["module1","module2"]
* Discovering dependencies for: /path/to/module1.l4
  Found imports: []
* Discovering dependencies for: /path/to/module2.l4
  Found imports: []
* Auto-discovered 3 total files (including imports)
** Loaded l4 functions from disk: 1
["my_function"]
```

**Result**: Full evaluation trace returned successfully. All imported functions resolved and executed correctly.

### Benefits

1. ✅ **Improved user experience**: Only specify entry point file
2. ✅ **Standard behavior**: Works like module systems in other languages (Python, Node.js, etc.)
3. ✅ **Automatic**: No need to manually track dependencies
4. ✅ **Robust**: Uses Set-based tracking to avoid duplicates
5. ✅ **Informative**: Debug output shows dependency discovery process
6. ✅ **Graceful degradation**: Warns about missing files rather than failing silently

### Usage Examples

**Before** (explicit dependency listing):

```bash
cabal run jl4-decision-service-exe -- \
  --sourcePaths path/to/module1.l4 \
  --sourcePaths path/to/module2.l4 \
  --sourcePaths path/to/main.l4
```

**After** (automatic discovery):

```bash
cabal run jl4-decision-service-exe -- \
  --sourcePaths path/to/main.l4
```

Both commands now work identically, with the second being more convenient.

### Future Enhancements

Potential improvements for consideration:

1. **Circular import detection**: Detect and warn about circular dependencies
2. **Import caching**: Cache parsed imports to avoid re-reading files
3. **Multi-root support**: Handle imports from multiple base directories
4. **Wildcard imports**: Support patterns like `--sourcePaths *.l4`
5. **Dependency graph**: Visualize import relationships
