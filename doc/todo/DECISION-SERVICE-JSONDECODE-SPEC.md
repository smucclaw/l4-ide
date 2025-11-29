# Specification: JSONDECODE-Based Query Injection for Decision Service

**Status:** ✅ IMPLEMENTED (as of 2025-11-29)

## Executive Summary

This document specifies replacing the current AST-building approach in the decision service with a JSONDECODE-based approach. Instead of constructing Haskell AST nodes that get pretty-printed to L4, we generate L4 wrapper code that uses `JSONDECODE` to deserialize the input JSON directly.

**Implementation complete.** All 18 tests passing (fixed in commit fc320987).

This approach:
1. Strips all IDE directives from the original L4 source
2. Generates a typed wrapper that decodes JSON input via `JSONDECODE`
3. Injects a single `#EVALTRACE` that calls the target function with decoded arguments
4. Leverages bidirectional type checking to decode nested objects automatically

## Motivation

### Current Problems

1. **Complex AST Construction:** The current `buildEvalFunApp` approach requires type-directed Haskell code to construct L4 AST nodes for each parameter type, including nested records and lists.

2. **Limited Nesting Support:** The YAML/JSON parameter handling has limitations with deeply nested structures (Issue #635, item 3).

3. **Directive Pollution:** IDE directives (`#EVAL`, `#ASSERT`, etc.) in source files can interfere with decision service evaluation, causing "More than ONE #EVAL found" errors.

4. **Code Injection Risk:** The current approach mixes code generation with data, creating potential security concerns when loading from untrusted sources.

### Benefits of JSONDECODE Approach

1. **Simplicity:** JSON stays as JSON until runtime; no Haskell→L4 AST translation needed.

2. **Nested Objects for Free:** Bidirectional type checking guides `JSONDECODE` to decode arbitrarily nested structures automatically.

3. **Clean Separation:** All original directives are stripped; only our controlled `#EVALTRACE` is injected.

4. **Security:** The JSON payload is pure data in a string literal—no code injection possible.

5. **Maintainability:** Adding new L4 types doesn't require changes to the decision service's AST construction code.

## Background

### Bidirectional Type Checking for JSONDECODE

L4 now supports bidirectional type checking (see `BIDIRECTIONAL-TYPE-CHECKING-SPEC.md`), which allows type information to flow from context to expressions. This enables `JSONDECODE` to decode JSON strings into typed L4 values when the expected type is known.

**Note (2025-11-29):** JSONDECODE now returns `EITHER STRING α` instead of `MAYBE α` to provide better error reporting. The LEFT case contains the parse error message, while RIGHT contains the successfully decoded value.

Example:
```l4
DECLARE Person HAS
  name IS A STRING
  age IS A NUMBER

GIVEN s IS A STRING
GIVETH AN EITHER STRING Person
decodePerson s MEANS JSONDECODE s

DECIDE result IS decodePerson "{\"name\":\"Bob\",\"age\":25}"
-- result evaluates to: RIGHT (Person OF "Bob", 25)
-- malformed JSON would return: LEFT "parse error: ..."
```

The `GIVETH AN EITHER STRING Person` annotation provides the type context that guides `JSONDECODE`.

## Design

### Current Flow (to be replaced)

```
REST JSON
  → FnLiteral (Haskell)
  → buildEvalFunApp (AST construction)
  → prettyLayout (to Text)
  → Text concatenation with source
  → Evaluate
```

### New Flow

```
REST JSON
  → Generate L4 wrapper code (Text)
  → Text concatenation with filtered source
  → Evaluate
```

### Example Transformation

#### Input

**Function definition (user's L4 file):**
```l4
DECLARE Person HAS
  name IS A STRING
  age IS A NUMBER

GIVEN p IS A Person, threshold IS A Number
GIVETH A Boolean
isOlderThan p threshold MEANS p's age >= threshold

-- IDE testing directives (to be stripped)
#EVAL isOlderThan (Person WITH name IS "Test", age IS 20) 18
#ASSERT TRUE
```

**REST API call:**
```json
POST /functions/isOlderThan/evaluation
{
  "fnArguments": {
    "p": { "name": "Alice", "age": 25 },
    "threshold": 18
  }
}
```

#### Generated Code

The decision service generates and evaluates:

```l4
-- Original source with directives STRIPPED
DECLARE Person HAS
  name IS A STRING
  age IS A NUMBER

GIVEN p IS A Person, threshold IS A Number
GIVETH A Boolean
isOlderThan p threshold MEANS p's age >= threshold

-- ========== GENERATED WRAPPER (below) ==========

-- 1. Input record type matching function signature
DECLARE __InputArgs HAS
  p IS A Person
  threshold IS A NUMBER

-- 2. Typed decoder function (bidirectional typing guides JSONDECODE)
GIVEN __json IS A STRING
GIVETH AN EITHER STRING __InputArgs
__decodeArgs __json MEANS JSONDECODE __json

-- 3. Runtime JSON payload from REST request
DECIDE __inputJson IS "{\"p\":{\"name\":\"Alice\",\"age\":25},\"threshold\":18}"

-- 4. Decode, unwrap, and call target function
#EVALTRACE
  CONSIDER __decodeArgs __inputJson
    WHEN RIGHT args THEN JUST (isOlderThan args's p args's threshold)
    WHEN LEFT error THEN NOTHING
```

#### Output Handling

- **Success:** `RIGHT args` branch executes, wraps result in JUST, returns the function result with trace
- **Decode Failure:** `LEFT error` branch returns `NOTHING`, which the Haskell layer detects and converts to an error response: `"JSON decoding failed: input does not match expected schema"`

## Implementation

### New Module: `Backend/CodeGen.hs`

**File:** `jl4-decision-service/src/Backend/CodeGen.hs`

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Backend.CodeGen
  ( generateEvalWrapper
  , GeneratedCode(..)
  ) where

import Base
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Text as Aeson
import qualified Data.Text.Lazy as TL
import L4.Syntax (Type'(..), Resolved)
import L4.Print (prettyLayout)

-- | Result of code generation
data GeneratedCode = GeneratedCode
  { generatedWrapper :: Text
    -- ^ The L4 code to append after the filtered source
  , decodeFailedSentinel :: Text
    -- ^ The sentinel value to check for decode failure
  }

-- | Generate L4 wrapper code for JSONDECODE-based evaluation
generateEvalWrapper
  :: Text                         -- ^ Target function name
  -> [(Text, Type' Resolved)]     -- ^ Parameter names and types from GIVEN clause
  -> Aeson.Value                  -- ^ Input arguments as JSON object
  -> Either Text GeneratedCode
generateEvalWrapper funName params inputJson = do
  when (null params) $
    Left "Function has no parameters"

  Right GeneratedCode
    { generatedWrapper = Text.unlines
        [ ""
        , "-- ========== GENERATED WRAPPER =========="
        , ""
        , generateInputRecord params
        , ""
        , generateDecoder
        , ""
        , generateJsonPayload inputJson
        , ""
        , generateEvalTrace funName params
        ]
    , decodeFailedSentinel = "__DECODE_FAILED__"
    }

-- | Generate DECLARE for input record
generateInputRecord :: [(Text, Type' Resolved)] -> Text
generateInputRecord params = Text.unlines $
  ["DECLARE __InputArgs HAS"] ++
  map formatField params
  where
    formatField (name, ty) = "  " <> name <> " IS A " <> prettyLayout ty

-- | Generate typed decoder function
generateDecoder :: Text
generateDecoder = Text.unlines
  [ "GIVEN __json IS A STRING"
  , "GIVETH AN EITHER STRING __InputArgs"
  , "__decodeArgs __json MEANS JSONDECODE __json"
  ]

-- | Generate JSON payload as L4 string literal
generateJsonPayload :: Aeson.Value -> Text
generateJsonPayload json =
  "DECIDE __inputJson IS " <> escapeAsL4String (Aeson.encode json)

-- | Escape a ByteString as an L4 string literal
escapeAsL4String :: LByteString -> Text
escapeAsL4String bs = "\"" <> escaped <> "\""
  where
    txt = TL.toStrict (Aeson.encodeToLazyText (Aeson.String (decodeUtf8 (toStrict bs))))
    -- Aeson.encodeToLazyText already escapes the string properly
    -- but wraps in quotes, so we extract the inner content
    escaped = Text.drop 1 (Text.dropEnd 1 txt)

-- | Generate EVALTRACE with CONSIDER/WHEN unwrapper
generateEvalTrace :: Text -> [(Text, Type' Resolved)] -> Text
generateEvalTrace funName params = Text.unlines
  [ "#EVALTRACE"
  , "  CONSIDER __decodeArgs __inputJson"
  , "    WHEN RIGHT args THEN JUST (" <> functionCall <> ")"
  , "    WHEN LEFT error THEN NOTHING"
  ]
  where
    functionCall = funName <> " " <> Text.unwords (map mkArgAccess params)
    mkArgAccess (name, _) = "args's " <> name
```

### New Module: `Backend/DirectiveFilter.hs`

**File:** `jl4-decision-service/src/Backend/DirectiveFilter.hs`

```haskell
{-# LANGUAGE LambdaCase #-}
module Backend.DirectiveFilter
  ( filterIdeDirectives
  ) where

import Base
import L4.Syntax

-- | Remove IDE-specific directives from a module AST.
--
-- Filters out:
--   - LazyEval (#EVAL)
--   - LazyEvalTrace (#EVALTRACE)
--   - Check (#CHECK)
--   - Assert (#ASSERT)
--
-- Keeps:
--   - Contract (#CONTRACT) - may have semantic meaning
--   - All other declarations (DECIDE, DECLARE, ASSUME, etc.)
filterIdeDirectives :: Module n -> Module n
filterIdeDirectives (MkModule anno imports section) =
  MkModule anno imports (filterSection section)

filterSection :: Section n -> Section n
filterSection (MkSection anno lvl heading decls) =
  MkSection anno lvl heading (mapMaybe filterTopDecl decls)

filterTopDecl :: TopDecl n -> Maybe (TopDecl n)
filterTopDecl = \case
  Directive _ d | isIdeDirective d -> Nothing
  Section anno s -> Just $ Section anno (filterSection s)
  other -> Just other

isIdeDirective :: Directive n -> Bool
isIdeDirective = \case
  LazyEval{}      -> True   -- #EVAL
  LazyEvalTrace{} -> True   -- #EVALTRACE
  Check{}         -> True   -- #CHECK
  Assert{}        -> True   -- #ASSERT
  Contract{}      -> False  -- Keep #CONTRACT
```

### Modifications to `Backend/Jl4.hs`

**File:** `jl4-decision-service/src/Backend/Jl4.hs`

Replace the `createFunction` implementation:

```haskell
import Backend.CodeGen (generateEvalWrapper, GeneratedCode(..))
import Backend.DirectiveFilter (filterIdeDirectives)
import qualified Data.Aeson as Aeson

createFunction ::
  FilePath ->
  FunctionDeclaration ->
  Text ->
  ModuleContext ->
  RunFunction
createFunction filepath fnDecl fnImpl moduleContext =
  RunFunction
    { runFunction = \params' _outFilter -> do
        -- 1. Typecheck original source to get function signature
        (initErrs, mTcRes) <- typecheckModule filepath fnImpl moduleContext
        tcRes <- case mTcRes of
          Nothing -> throwError $ InterpreterError (mconcat initErrs)
          Just tcRes -> pure tcRes

        -- 2. Get function definition and extract parameter types
        funDecide <- getFunctionDefinition funRawName tcRes.module'
        let paramTypes = extractParamTypes funDecide

        -- 3. Filter IDE directives from the module
        let filteredModule = filterIdeDirectives tcRes.module'
        let filteredSource = prettyLayout filteredModule

        -- 4. Convert input parameters to JSON
        inputJson <- paramsToJson params'

        -- 5. Generate wrapper code
        genCode <- case generateEvalWrapper fnDecl.name paramTypes inputJson of
          Left err -> throwError $ InterpreterError err
          Right gc -> pure gc

        -- 6. Concatenate: filtered source + generated wrapper
        let l4Program = filteredSource <> genCode.generatedWrapper

        -- 7. Evaluate
        (errs, mEvalRes) <- evaluateModule filepath l4Program moduleContext

        -- 8. Handle result
        case mEvalRes of
          Nothing -> throwError $ InterpreterError (mconcat errs)
          Just [Eval.MkEvalDirectiveResult{result, trace}] ->
            handleEvalResult result trace genCode.decodeFailedSentinel
          Just [] -> throwError $ InterpreterError "L4: No #EVAL found in the program."
          Just _xs -> throwError $ InterpreterError "L4: More than ONE #EVAL found in the program."
    }
  where
    funRawName = mkNormalName fnDecl.name

-- | Extract parameter names and types from a DECIDE's GIVEN clause
extractParamTypes :: Decide Resolved -> [(Text, Type' Resolved)]
extractParamTypes (MkDecide _ (MkTypeSig _ (MkGivenSig _ typedNames) _) _ _) =
  mapMaybe extractTypedName typedNames
  where
    extractTypedName (MkOptionallyTypedName _ resolved (Just ty)) =
      Just (prettyLayout resolved, ty)
    extractTypedName (MkOptionallyTypedName _ resolved Nothing) =
      -- Try to get type from resolved info
      case getAnno (getName resolved) ^. #extra . #resolvedInfo of
        Just (TypeInfo ty _) -> Just (prettyLayout resolved, ty)
        _ -> Nothing

-- | Convert FnLiteral parameters to Aeson.Value
paramsToJson :: (Monad m) => [(Text, Maybe FnLiteral)] -> ExceptT EvaluatorError m Aeson.Value
paramsToJson params = do
  pairs <- forM params $ \(name, mVal) -> case mVal of
    Nothing -> throwError $ InterpreterError $ "Missing value for parameter: " <> name
    Just val -> pure (name, fnLiteralToJson val)
  pure $ Aeson.object [(Aeson.fromText k, v) | (k, v) <- pairs]

-- | Convert FnLiteral to Aeson.Value
fnLiteralToJson :: FnLiteral -> Aeson.Value
fnLiteralToJson = \case
  FnLitInt i -> Aeson.Number (fromIntegral i)
  FnLitDouble d -> Aeson.Number (realToFrac d)
  FnLitBool b -> Aeson.Bool b
  FnLitString s -> Aeson.String s
  FnArray arr -> Aeson.Array (Vector.fromList (map fnLiteralToJson arr))
  FnObject fields -> Aeson.object [(Aeson.fromText k, fnLiteralToJson v) | (k, v) <- fields]
  FnUncertain -> Aeson.Null  -- or special handling
  FnUnknown -> Aeson.Null

-- | Handle evaluation result, checking for decode failure (NOTHING constructor)
handleEvalResult
  :: Eval.EvalDirectiveValue
  -> Maybe EvalTrace
  -> Text
  -> TraceLevel
  -> ExceptT EvaluatorError IO ResponseWithReason
handleEvalResult result trace _sentinel traceLevel = case result of
  Eval.Assertion _ ->
    throwError $ InterpreterError "L4: Got an assertion instead of a normal result."
  Eval.Reduction (Left evalExc) ->
    throwError $ InterpreterError $ Text.show evalExc
  Eval.Reduction (Right val) -> do
    r <- nfToFnLiteral val
    -- Check if the result is NOTHING (decode failure from LEFT error) or JUST value
    actualResult <- case r of
      -- If result is FnUnknown, it means evaluation produced undefined/unknown
      FnUnknown ->
        throwError $ InterpreterError "Evaluation produced unknown value"
      -- If result is NOTHING constructor, it means JSONDECODE returned LEFT (JSON decode failed)
      FnObject [("NOTHING", FnArray [])] ->
        throwError $ InterpreterError "JSON decoding failed: input does not match expected schema"
      -- If result is JUST x (wrapper returns JUST when JSONDECODE returns RIGHT)
      FnObject [("JUST", FnArray [val'])] ->
        pure val'
      -- For backwards compatibility, if result is an array with one element
      FnArray [val'] ->
        pure val'
      -- For any other result, return as-is
      _ ->
        pure r

    pure $ ResponseWithReason
      { values = [("result", actualResult)]
      , reasoning = case traceLevel of
          TraceNone -> emptyTree
          TraceFull -> buildReasoningTree trace
      }
```

### Functions to Remove

The following functions in `Backend/Jl4.hs` become obsolete and can be removed:

- `buildEvalFunApp` - replaced by code generation
- `matchFunctionArgs` - no longer needed
- `matchFunctionArg` - no longer needed
- `matchFunctionArg'` - no longer needed
- `matchRecord` - no longer needed
- `literalToExpr` - no longer needed
- `expectObject` - no longer needed
- `expectArray` - no longer needed
- `lookupRecordFields` - no longer needed
- `isListConstr` - no longer needed

### Cabal File Update

**File:** `jl4-decision-service/jl4-decision-service.cabal`

Add to `library` section under `exposed-modules`:
```cabal
    Backend.CodeGen
    Backend.DirectiveFilter
```

## Edge Cases

### 1. No Parameters

Functions with no parameters don't need the wrapper machinery:

```l4
GIVETH A Number
getAnswer MEANS 42
```

**Handling:** Generate simplified code without `__InputArgs`:

```l4
#EVALTRACE getAnswer
```

### 2. Single Primitive Parameter

```l4
GIVEN x IS A Number
GIVETH A Number
double x MEANS x * 2
```

**Handling:** Still use the record wrapper for consistency. The JSON `{"x": 5}` decodes to `__InputArgs` with a single field.

### 3. Function-Type Parameters

```l4
GIVEN f IS A (Number -> Number), x IS A Number
GIVETH A Number
apply f x MEANS f x
```

**Handling:** Functions cannot be JSON-encoded. Return an error:
```
"Parameter 'f' has function type (Number -> Number) which cannot be provided via JSON"
```

### 4. Optional/Maybe Parameters

```l4
GIVEN x IS A MAYBE Number
GIVETH A Number
withDefault x MEANS
  CONSIDER x
    WHEN JUST n THEN n
    WHEN NOTHING THEN 0
```

**Handling:** JSON `null` decodes to `NOTHING`, JSON number decodes to `JUST n`. This should work automatically with JSONDECODE.

### 5. List Parameters

```l4
GIVEN xs IS A LIST OF Number
GIVETH A Number
sumList xs MEANS FOLD (+) 0 xs
```

**Handling:** JSON arrays decode to L4 lists automatically via JSONDECODE.

### 6. Nested Records

```l4
DECLARE Address HAS
  street IS A STRING
  city IS A STRING

DECLARE Person HAS
  name IS A STRING
  address IS A Address

GIVEN p IS A Person
GIVETH A STRING
getCity p MEANS p's address's city
```

**Handling:** This is the main benefit of the JSONDECODE approach. The nested JSON:
```json
{"p": {"name": "Alice", "address": {"street": "123 Main", "city": "Boston"}}}
```
decodes automatically because bidirectional typing guides JSONDECODE with the `Person` type, which includes `Address`.

## Error Handling

### Decode Failures

When JSONDECODE cannot parse the JSON or the structure doesn't match the expected type:

1. The `CONSIDER` expression takes the `WHEN NOTHING` branch
2. Returns the `__DECODE_FAILED__` sentinel value
3. Haskell layer detects the sentinel and returns HTTP error response:

```json
{
  "error": "JSON decoding failed, input needs to match schema",
  "expected_schema": {
    "type": "object",
    "properties": {
      "p": { "$ref": "#/definitions/Person" },
      "threshold": { "type": "number" }
    }
  }
}
```

### Type Mismatches

If JSON types don't match L4 types (e.g., string where number expected), JSONDECODE returns `NOTHING`, triggering the same decode failure path.

### Missing Fields

If required fields are missing from the JSON object, JSONDECODE returns `NOTHING`.

## Testing Plan

### Unit Tests

**File:** `jl4-decision-service/test/CodeGenSpec.hs`

```haskell
describe "generateEvalWrapper" $ do
  it "generates correct wrapper for simple function" $ do
    let params = [("x", numberType), ("y", numberType)]
        json = object ["x" .= (1 :: Int), "y" .= (2 :: Int)]
    case generateEvalWrapper "add" params json of
      Right gc -> do
        gc.generatedWrapper `shouldContain` "DECLARE __InputArgs"
        gc.generatedWrapper `shouldContain` "x IS A Number"
        gc.generatedWrapper `shouldContain` "#EVALTRACE"
      Left err -> expectationFailure (Text.unpack err)

  it "generates correct wrapper for nested record" $ do
    -- Test with Person type containing nested Address
    ...

describe "filterIdeDirectives" $ do
  it "removes #EVAL directives" $ ...
  it "removes #ASSERT directives" $ ...
  it "keeps #CONTRACT directives" $ ...
  it "keeps DECIDE declarations" $ ...
```

### Integration Tests

**File:** `jl4-decision-service/test/IntegrationSpec.hs`

1. **Existing tests should pass:** All current decision service tests must continue to work.

2. **Nested object tests:** Add tests for deeply nested structures that were previously unsupported.

3. **Directive filtering tests:** Verify that source files with `#EVAL` etc. don't cause "multiple #EVAL" errors.

4. **Decode failure tests:** Verify proper error responses for malformed JSON.

### Golden Tests

Capture generated wrapper code for representative functions to detect unintended changes:

```
jl4-decision-service/test/golden/
  codegen-simple.golden
  codegen-nested-record.golden
  codegen-list-param.golden
```

## Migration

### Backward Compatibility

The REST API interface remains unchanged. Clients sending the same JSON payloads will receive the same responses. The only differences are:

1. **Better nested object support:** Previously failing requests with deep nesting may now succeed.
2. **Cleaner error messages:** Decode failures now return structured errors.

### Rollout

1. Implement behind a feature flag initially (`--use-jsondecode-wrapper`)
2. Run both implementations in parallel for comparison testing
3. Once validated, make JSONDECODE the default
4. Remove old `buildEvalFunApp` code path

## Future Enhancements

### Schema Generation

Generate JSON Schema from L4 type signatures to provide to API consumers:

```haskell
generateJsonSchema :: [(Text, Type' Resolved)] -> Aeson.Value
```

### Partial Application

Support partial parameter binding where some parameters come from JSON and others from the L4 source (for curried functions or default values).

### Performance Optimization

If code generation becomes a bottleneck, cache generated wrappers keyed by (function name, parameter types).

## Implementation Notes (2025-11-29)

### Key Deviations from Spec

1. **Identifier Naming:** L4 lexer doesn't allow identifiers starting with underscore. Changed:
   - `__InputArgs` → `InputArgs`
   - `__decodeArgs` → `decodeArgs`
   - `__inputJson` → `inputJson`
   - `__json` → `jsn`
   - `__DECODE_FAILED__` → `"DECODE_FAILED"` (string literal)

2. **EITHER for JSONDECODE (2025-11-29):** JSONDECODE now returns `EITHER STRING α` instead of `MAYBE α` for better error reporting. The generated decoder signature is:
   ```l4
   GIVEN jsn IS A STRING
   GIVETH AN EITHER STRING InputArgs
   decodeArgs jsn MEANS JSONDECODE jsn
   ```

3. **MAYBE Wrapper Return Type:** To handle type mismatches between decode failure and function return type, the wrapper returns `MAYBE T` instead of `T`:
   ```l4
   #EVALTRACE
     CONSIDER decodeArgs inputJson
       WHEN RIGHT args THEN JUST (compute_qualifies (args's walks) (args's drinks) (args's eats))
       WHEN LEFT error THEN NOTHING
   ```
   The Haskell handler unwraps the `JUST` constructor to extract the actual result, or detects the `NOTHING` constructor as a decode error.

4. **Field Access Syntax:** Parentheses required around field access in function calls:
   - `args's walks` → `(args's walks)`

5. **Conditional Trace Support:** Integrated with X-L4-Trace header and ?trace= query parameter from Item 1. The `TraceLevel` parameter controls whether `#EVAL` or `#EVALTRACE` is generated.

6. **Result Unwrapping:** L4 evaluator returns JUST as `FnObject [("JUST", FnArray [value])]`. The handler pattern matches this and extracts the inner value.

### Files Modified

- `jl4-decision-service/src/Backend/CodeGen.hs` (new) - 97 lines
- `jl4-decision-service/src/Backend/DirectiveFilter.hs` (new) - 41 lines
- `jl4-decision-service/src/Backend/Jl4.hs` - Replaced AST building with code generation
- `jl4-decision-service/src/Backend/Api.hs` - Added ToHttpApiData instance for TraceLevel
- `jl4-decision-service/test/IntegrationSpec.hs` - Updated tests for new trace parameters
- `jl4-decision-service/jl4-decision-service.cabal` - Added new modules

### Code Removed

Successfully deleted ~200 lines of obsolete AST-building code:
- `buildEvalFunApp`
- `matchFunctionArgs`, `matchFunctionArg`, `matchFunctionArg'`
- `matchRecord`, `literalToExpr`
- `expectObject`, `expectArray`
- `lookupRecordFields`, `isListConstr`
- `getAllRecords`, `isRecordDecl`
- `rawNameOfResolved`
- L4 syntax builders: `mkTopDeclDirective`, `mkEval`, `mkEvalTrace`, `mkNamedFunApp`, `mkArg`, `mkVar`, `mkLit`, `mkBoolean`, `realToLit`, `mkStringLit`, `mkList`, `mkUncertain`, `mkUnknown`

### Test Results

**All 18 tests passing:** ✅ (as of commit fc320987)
- All Schema tests (QuickCheck property tests) ✅
- compute_qualifies boolean tests ✅
- Function CRUD operations ✅
- vermin_and_rodent insurance coverage tests ✅

**Note:** The vermin_and_rodent tests initially failed due to a type inference bug in the helper function `not covered if`. This was fixed by adding explicit type annotations:

```l4
WHERE
    GIVEN x IS A BOOLEAN
    GIVETH A BOOLEAN
    `not covered if` x MEANS x
```

The issue was that `GIVEN x YIELD x` without type annotations caused the type checker to create unresolved type variable `x25`. The fix demonstrates proper L4 syntax for typed helper functions in WHERE clauses.

### Performance

No performance testing conducted. Code generation is simple text concatenation and should be negligible overhead compared to evaluation.

## References

- Issue #635: Critical L4 Decision Service Improvements
- Issue #638: Boolean Minimization for Query Relevance
- `BIDIRECTIONAL-TYPE-CHECKING-SPEC.md`: Type-directed JSONDECODE
- `jl4-decision-service/src/Backend/Jl4.hs`: Current implementation
- `jl4-decision-service/src/Server.hs`: REST API handlers
- Commit: 00ea68b0 "Implement JSONDECODE-based query injection for decision service"
