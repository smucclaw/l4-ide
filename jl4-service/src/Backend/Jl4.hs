{-# LANGUAGE ViewPatterns #-}
module Backend.Jl4 (createFunction, createRunFunctionFromCompiled, getFunctionDefinition, buildFunDecide, ModuleContext, CompiledModule(..), precompileModule, evaluateWithCompiled, evaluateWithCompiledDeontic, typecheckModule, buildImportEnvironment) where

import Base hiding (trace)
import qualified Base.DList as DList
import qualified Base.Map as Map
import qualified Base.Text as Text

import L4.Annotation
-- import qualified L4.Evaluate.Value as Eval
import qualified L4.Evaluate.ValueLazy as Eval
import qualified L4.EvaluateLazy as Eval
import L4.EvaluateLazy.Machine (EvalException, emptyEnvironment)
import qualified L4.Evaluate.ValueLazy as EvalEnv (Environment)
import L4.EvaluateLazy.Trace
import qualified L4.EvaluateLazy.GraphViz2 as GraphViz
import L4.TracePolicy (apiDefaultPolicy, TracePolicy(..))
import qualified L4.TracePolicy as TracePolicy
import L4.Names
import L4.Print
import qualified L4.Print as Print
import L4.Syntax
import qualified L4.TypeCheck as TypeCheck
import L4.TypeCheck.Types (EntityInfo, Environment)
import L4.Utils.Ratio
import qualified LSP.Core.Shake as Shake
import LSP.L4.Oneshot (oneshotL4ActionAndErrors)
import qualified LSP.L4.Rules as Rules
import Optics ((^.), (%))

import Language.LSP.Protocol.Types (normalizedFilePathToUri, toNormalizedFilePath)
import System.FilePath ((<.>), takeFileName)

import Backend.Api
import Backend.CodeGen (generateEvalWrapper, generateDeonticEvalWrapper, GeneratedCode(..))
import L4.Export (extractAssumeParamTypes)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Aeson
import qualified Data.Scientific as Scientific
import qualified Data.Vector as Vector

-- | Map from file path to file content for module resolution
type ModuleContext = Map FilePath Text

-- | Pre-compiled L4 module ready for fast evaluation
data CompiledModule = CompiledModule
  { compiledModule :: !(Module Resolved)
  , compiledEnvironment :: !Environment              -- ^ Typechecker environment (for type info)
  , compiledEntityInfo :: !EntityInfo
  , compiledDecide :: !(Decide Resolved)
  , compiledModuleContext :: !ModuleContext          -- ^ Context needed for IMPORT resolution
  , compiledImportEnv :: !EvalEnv.Environment        -- ^ Evaluator environment from imports
  , compiledSource :: !Text                          -- ^ Original source text (preserves layout)
  }
  deriving (Generic)

buildFunDecide :: Text -> FunctionDeclaration -> ExceptT EvaluatorError IO (Decide Resolved)
buildFunDecide fnImpl fnDecl = do
  (initErrs, mTcRes) <- typecheckModule file fnImpl Map.empty

  tcRes <- case mTcRes of
    Nothing -> throwError $ InterpreterError (mconcat initErrs)
    Just tcRes -> pure tcRes

  getFunctionDefinition funRawName tcRes.module'
 where
  file = Text.unpack fnDecl.name <.> "l4"
  funRawName = mkNormalName fnDecl.name

-- | Precompile an L4 module for fast repeated evaluation
precompileModule
  :: FilePath
  -> Text
  -> ModuleContext
  -> RawName
  -> IO (Either Text CompiledModule)
precompileModule filepath source moduleContext funName = runExceptT $ do
  -- Parse and typecheck once
  (errs, mTcRes) <- liftIO $ typecheckModule filepath source moduleContext
  tcRes <- case mTcRes of
    Nothing -> throwError $ mconcat errs
    Just tcRes -> pure tcRes

  -- Extract the function definition
  decide <- withExceptT evalErrorToText $ getFunctionDefinition funName tcRes.module'

  -- Pre-evaluate import dependencies to get the evaluator Environment
  -- This is needed because execEvalExprInContextOfModule expects the import
  -- environment to be pre-computed (it only evaluates the current module fresh)
  importEnv <- liftIO $ buildImportEnvironment filepath source moduleContext tcRes.entityInfo

  -- Build the compiled module
  pure CompiledModule
    { compiledModule = tcRes.module'
    , compiledEnvironment = tcRes.environment
    , compiledEntityInfo = tcRes.entityInfo
    , compiledDecide = decide
    , compiledModuleContext = moduleContext  -- Store context for IMPORT resolution
    , compiledImportEnv = importEnv
    , compiledSource = source                -- Preserve original source for layout-sensitive re-parsing
    }
 where
  evalErrorToText :: EvaluatorError -> Text
  evalErrorToText (InterpreterError t) = t
  evalErrorToText (RequiredParameterMissing pm) = "Required parameter missing: expected " <> Text.textShow pm.expected <> ", got " <> Text.textShow pm.actual
  evalErrorToText (UnknownArguments args) = "Unknown arguments: " <> Text.intercalate ", " args
  evalErrorToText (CannotHandleParameterType lit) = "Cannot handle parameter type: " <> Text.textShow lit
  evalErrorToText CannotHandleUnknownVars = "Cannot handle unknown variables"

-- ----------------------------------------------------------------------------
-- Direct AST evaluation (avoiding text round-trip)
-- ----------------------------------------------------------------------------

-- | Convert FnLiteral to Expr Resolved
-- This allows us to build function call expressions directly without going through text
-- Note: FnObject, FnUncertain, and FnUnknown require wrapper-based evaluation
-- and should be filtered out by requiresWrapperEvaluation before calling this
fnLiteralToExpr :: FnLiteral -> Expr Resolved
fnLiteralToExpr = \case
  FnLitInt i -> Lit emptyAnno (NumericLit emptyAnno (fromIntegral i))
  -- Use Scientific for exact decimal semantics matching JSONDECODE
  -- This preserves decimal values like 0.1 as exact rationals (1/10)
  -- Scientific has a Real instance so toRational works directly
  FnLitDouble d -> Lit emptyAnno (NumericLit emptyAnno (toRational (Scientific.fromFloatDigits d)))
  FnLitBool True -> App emptyAnno TypeCheck.trueRef []
  FnLitBool False -> App emptyAnno TypeCheck.falseRef []
  FnLitString s -> Lit emptyAnno (StringLit emptyAnno s)
  FnArray xs -> List emptyAnno (map fnLiteralToExpr xs)
  -- These cases should never be reached if requiresWrapperEvaluation is checked first
  FnObject _fields -> error "fnLiteralToExpr: FnObject requires wrapper-based evaluation"
  FnUncertain -> error "fnLiteralToExpr: FnUncertain requires wrapper-based evaluation"
  FnUnknown -> error "fnLiteralToExpr: FnUnknown requires wrapper-based evaluation"

-- | Get the function's Resolved name from the compiled decide
getFunctionResolved :: Decide Resolved -> Resolved
getFunctionResolved (MkDecide _ _ (MkAppForm _ funName _ _) _) = funName

-- | Build the function call expression from function name and argument expressions
-- Arguments must be in the correct order (matching the function signature)
buildFunctionCallExpr :: Resolved -> [Expr Resolved] -> Expr Resolved
buildFunctionCallExpr funName args =
  App emptyAnno funName args

-- | Check if any parameter value requires wrapper-based evaluation
-- Returns True for:
-- - Missing parameters (Nothing) - need wrapper to handle as UNKNOWN
-- - FnObject, FnUncertain, FnUnknown, FnLitString (enum coercion)
-- These types need JSONDECODE to handle properly
requiresWrapperEvaluation :: [(Text, Maybe FnLiteral)] -> Bool
requiresWrapperEvaluation = any (\(_, mVal) -> maybe True needsWrapper mVal)
  where
    needsWrapper :: FnLiteral -> Bool
    needsWrapper (FnObject _) = True
    needsWrapper FnUncertain = True
    needsWrapper FnUnknown = True
    needsWrapper (FnLitString _) = True  -- Strings may be enum constructors
    needsWrapper (FnArray xs) = any needsWrapper xs
    needsWrapper _ = False

-- | Evaluate using precompiled module (fast path) - direct AST evaluation
-- This avoids the text round-trip through prettyLayout and re-parsing
-- Falls back to wrapper-based evaluation for FnObject parameters or missing params
evaluateWithCompiled
  :: FilePath
  -> FunctionDeclaration
  -> CompiledModule
  -> [(Text, Maybe FnLiteral)]
  -> TraceLevel
  -> Bool
  -> ExceptT EvaluatorError IO ResponseWithReason
evaluateWithCompiled filepath fnDecl compiled params traceLevel includeGraphViz = do
  -- Fill in missing parameters with Nothing
  -- The input params may only contain provided parameters; we need explicit Nothing
  -- entries for missing parameters so requiresWrapperEvaluation can detect them
  let expectedParams = map fst (extractParamTypes compiled.compiledDecide)
      inputMap = Map.fromList params
      -- join flattens Maybe (Maybe FnLiteral) -> Maybe FnLiteral:
      -- - Nothing (not in input) -> Nothing
      -- - Just Nothing (explicit unknown) -> Nothing
      -- - Just (Just v) (provided value) -> Just v
      fullParams = [(name, join $ Map.lookup name inputMap) | name <- expectedParams]

  -- Check if we need to fall back to wrapper-based evaluation
  -- (for FnObject, FnUncertain, FnUnknown, missing params which require JSONDECODE)
  if requiresWrapperEvaluation fullParams
    then evaluateWithWrapper filepath fnDecl compiled fullParams traceLevel includeGraphViz
    else evaluateDirectAST compiled fullParams traceLevel includeGraphViz

-- | Evaluate a deontic function with startTime and events via EVALTRACE wrapper.
-- Always uses the wrapper path since events need to go through L4 typechecking.
evaluateWithCompiledDeontic
  :: FilePath
  -> FunctionDeclaration
  -> CompiledModule
  -> [(Text, Maybe FnLiteral)]
  -> Scientific.Scientific   -- ^ Start time for contract simulation
  -> [TraceEvent]             -- ^ Events to replay
  -> TraceLevel
  -> Bool
  -> ExceptT EvaluatorError IO ResponseWithReason
evaluateWithCompiledDeontic filepath fnDecl compiled params startTime traceEvents traceLevel includeGraphViz = do
  let givenParamTypes = extractParamTypes compiled.compiledDecide
      assumeParamTypes = extractAssumeParamTypes compiled.compiledModule compiled.compiledDecide

  -- Convert input parameters to JSON
  inputJson <- paramsToJson params

  -- Generate deontic wrapper code with EVALTRACE
  genCode <- case generateDeonticEvalWrapper fnDecl.name givenParamTypes assumeParamTypes inputJson startTime traceEvents traceLevel of
    Left err -> throwError $ InterpreterError err
    Right gc -> pure gc

  -- Evaluate the wrapper in the context of the precompiled module
  (errs, mEvalRes) <- liftIO $ evaluateWrapperInContext filepath genCode.generatedWrapper compiled

  -- Handle result
  case mEvalRes of
    Nothing -> throwError $ InterpreterError (mconcat errs)
    Just [Eval.MkEvalDirectiveResult{result, trace}] ->
      handleEvalResult compiled.compiledEntityInfo result trace genCode.decodeFailedSentinel traceLevel includeGraphViz compiled.compiledModule
    Just [] -> throwError $ InterpreterError "L4: No #EVAL found in the program."
    Just _xs -> throwError $ InterpreterError "L4: More than ONE #EVAL found in the program."

-- | Direct AST evaluation (fast path) - for simple types without FnObject
evaluateDirectAST
  :: CompiledModule
  -> [(Text, Maybe FnLiteral)]
  -> TraceLevel
  -> Bool
  -> ExceptT EvaluatorError IO ResponseWithReason
evaluateDirectAST compiled params traceLevel includeGraphViz = do
  -- Convert input parameters to a map for lookup
  let paramMap = Map.fromList [(name, val) | (name, Just val) <- params]

  -- Get parameter names in order from the function signature
  let paramTypes = extractParamTypes compiled.compiledDecide
      paramNames = map fst paramTypes

  -- Build argument expressions in the correct order
  argExprs <- forM paramNames $ \name -> case Map.lookup name paramMap of
    Nothing -> throwError $ InterpreterError $ "Missing value for parameter: " <> name
    Just val -> pure (fnLiteralToExpr val)

  -- Get the function's Resolved name from the compiled decide
  let funResolved = getFunctionResolved compiled.compiledDecide

  -- Build the function call expression directly as AST
  let callExpr = buildFunctionCallExpr funResolved argExprs

  -- Configure evaluation with tracing based on trace level
  let evalTracePolicy = case traceLevel of
        TraceNone -> apiDefaultPolicy
        TraceFull -> TracePolicy
          { evalDirectiveTrace = TracePolicy.CollectTrace (TracePolicy.TraceOptions TracePolicy.TextTrace TracePolicy.ApiResponse GraphViz.defaultGraphVizOptions)
          , evaltraceDirectiveTrace = TracePolicy.CollectTrace (TracePolicy.TraceOptions TracePolicy.TextTrace TracePolicy.ApiResponse GraphViz.defaultGraphVizOptions)
          }

  -- Evaluate the expression directly using the precompiled module
  fixedNow <- liftIO Eval.readFixedNowEnv
  evalConfig <- liftIO $ Eval.resolveEvalConfig fixedNow evalTracePolicy

  -- Pass the pre-computed import environment
  -- The module's own definitions are evaluated fresh each time, but imports
  -- need their References to be pre-allocated
  mResult <- liftIO $ Eval.execEvalExprInContextOfModule
    evalConfig
    compiled.compiledEntityInfo
    callExpr
    (compiled.compiledImportEnv, compiled.compiledModule)

  -- Handle result
  case mResult of
    Nothing -> throwError $ InterpreterError "L4: Expression evaluation failed."
    Just Eval.MkEvalDirectiveResult{result, trace} ->
      handleEvalResultDirect compiled.compiledEntityInfo result trace traceLevel includeGraphViz compiled.compiledModule

-- | Handle evaluation result (simplified version for direct evaluation)
handleEvalResultDirect
  :: EntityInfo
  -> Eval.EvalDirectiveValue
  -> Maybe EvalTrace
  -> TraceLevel
  -> Bool
  -> Module Resolved
  -> ExceptT EvaluatorError IO ResponseWithReason
handleEvalResultDirect ei result trace traceLevel includeGraphViz mModule = case result of
  Eval.Assertion _ -> throwError $ InterpreterError "L4: Got an assertion instead of a normal result."
  Eval.Reduction (Left evalExc) -> throwError $ InterpreterError $ Text.textShow evalExc
  Eval.Reduction (Right val) -> do
    r <- nfToFnLiteral ei val
    pure $ ResponseWithReason
      { fnResult = Map.singleton "value" r
      , reasoning = case traceLevel of
          TraceNone -> emptyTree
          TraceFull -> buildReasoningTree trace
      , graphviz =
          if includeGraphViz && traceLevel == TraceFull
            then
              fmap
                ( \tr ->
                    GraphVizResponse
                      { dot = GraphViz.traceToGraphViz GraphViz.defaultGraphVizOptions (Just mModule) tr
                      }
                )
                trace
            else Nothing
      }

-- | Wrapper-based evaluation (fallback for FnObject parameters)
-- Uses JSONDECODE to handle complex record types
evaluateWithWrapper
  :: FilePath
  -> FunctionDeclaration
  -> CompiledModule
  -> [(Text, Maybe FnLiteral)]
  -> TraceLevel
  -> Bool
  -> ExceptT EvaluatorError IO ResponseWithReason
evaluateWithWrapper filepath fnDecl compiled params traceLevel includeGraphViz = do
  -- Extract parameter types from the compiled function definition
  let givenParamTypes = extractParamTypes compiled.compiledDecide
      assumeParamTypes = extractAssumeParamTypes compiled.compiledModule compiled.compiledDecide

  -- Convert input parameters to JSON
  inputJson <- paramsToJson params

  -- Generate wrapper code using existing code generation
  genCode <- case generateEvalWrapper fnDecl.name givenParamTypes assumeParamTypes inputJson traceLevel of
    Left err -> throwError $ InterpreterError err
    Right gc -> pure gc

  -- The wrapper contains JSONDECODE and function application
  -- We evaluate the wrapper in the context of the precompiled module
  -- This avoids re-parsing and re-typechecking the main module
  (errs, mEvalRes) <- liftIO $ evaluateWrapperInContext filepath genCode.generatedWrapper compiled

  -- Handle result
  case mEvalRes of
    Nothing -> throwError $ InterpreterError (mconcat errs)
    Just [Eval.MkEvalDirectiveResult{result, trace}] ->
      handleEvalResult compiled.compiledEntityInfo result trace genCode.decodeFailedSentinel traceLevel includeGraphViz compiled.compiledModule
    Just [] -> throwError $ InterpreterError "L4: No #EVAL found in the program."
    Just _xs -> throwError $ InterpreterError "L4: More than ONE #EVAL found in the program."

-- | Evaluate wrapper code in the context of a precompiled module
-- This combines the wrapper (small) with the precompiled module and evaluates
evaluateWrapperInContext
  :: FilePath
  -> Text  -- ^ Wrapper code containing #EVAL directive
  -> CompiledModule
  -> IO ([Text], Maybe [Eval.EvalDirectiveResult])
evaluateWrapperInContext filepath wrapperCode compiled = do
  -- Use original source text to preserve layout-sensitive formatting
  -- L4 is layout-sensitive (like Python), so prettyLayout can break indentation
  -- We filter IDE directives (#EVAL, #TRACE, etc.) using text-based filtering
  let filteredSource = filterIdeDirectivesText compiled.compiledSource
      combinedProgram = filteredSource <> wrapperCode

  -- Evaluate the combined program using the original module context
  -- This ensures IMPORT statements can be resolved correctly
  evaluateModule filepath combinedProgram compiled.compiledModuleContext

-- | Filter IDE directives from source text while preserving layout
-- Removes lines starting with #EVAL, #TRACE, #EVALTRACE, #ASSERT, #CHECK
-- Also removes continuation lines for multiline directives (#TRACE ... WITH)
-- A continuation line is one that:
-- 1. Immediately follows a directive line or another continuation
-- 2. Is more indented than the directive line (or is a blank/comment line)
filterIdeDirectivesText :: Text -> Text
filterIdeDirectivesText = Text.unlines . filterLines . Text.lines
  where
    -- Filter lines, tracking whether we're inside a multiline directive
    filterLines :: [Text] -> [Text]
    filterLines = go Nothing
      where
        -- mIndent = Nothing: not in a directive
        -- mIndent = Just indent: in a directive with given base indentation
        go :: Maybe Int -> [Text] -> [Text]
        go _ [] = []
        go mIndent (line:rest)
          | isDirectiveLine line =
              -- Start of a directive - skip it and enter directive state
              let baseIndent = lineIndentation line
              in go (Just baseIndent) rest
          | Just baseIndent <- mIndent, isContinuationLine baseIndent line =
              -- Continuation of a multiline directive - skip it
              go mIndent rest
          | otherwise =
              -- Not a directive or continuation - keep it and reset state
              line : go Nothing rest

    -- Check if line starts a directive (with optional leading whitespace)
    isDirectiveLine :: Text -> Bool
    isDirectiveLine line =
      let stripped = Text.stripStart line
      in any (`Text.isPrefixOf` stripped)
           ["#EVAL", "#EVALTRACE", "#TRACE", "#ASSERT", "#CHECK"]

    -- Check if line is a continuation of a multiline directive
    -- Continuation lines are either:
    -- 1. More indented than the base directive
    -- 2. Empty or whitespace-only (blank lines within directive)
    -- 3. Comment-only lines (starting with --) that are more indented
    isContinuationLine :: Int -> Text -> Bool
    isContinuationLine baseIndent line
      | Text.all isSpace line = True  -- Blank line - include in directive block
      | lineIndentation line > baseIndent = True  -- More indented = continuation
      | otherwise = False

    -- Get the indentation (number of leading spaces/tabs) of a line
    lineIndentation :: Text -> Int
    lineIndentation = Text.length . Text.takeWhile isSpace

    isSpace :: Char -> Bool
    isSpace c = c == ' ' || c == '\t'

-- | Convert FnLiteral parameters to Aeson.Value
-- Missing parameters (Nothing) become FnUnknown -> Aeson.Null for partial evaluation
paramsToJson :: (Monad m) => [(Text, Maybe FnLiteral)] -> ExceptT EvaluatorError m Aeson.Value
paramsToJson params =
  let pairs = [(name, fnLiteralToJson (maybe FnUnknown id mVal)) | (name, mVal) <- params]
  in pure $ Aeson.object [(Aeson.fromText k, v) | (k, v) <- pairs]

-- | Convert FnLiteral to Aeson.Value
fnLiteralToJson :: FnLiteral -> Aeson.Value
fnLiteralToJson = \case
  FnLitInt i -> Aeson.Number (fromIntegral i)
  FnLitDouble d -> Aeson.Number (realToFrac d)
  FnLitBool b -> Aeson.Bool b
  FnLitString s -> Aeson.String s
  FnArray arr -> Aeson.Array (Vector.fromList (map fnLiteralToJson arr))
  FnObject fields -> Aeson.object [(Aeson.fromText k, fnLiteralToJson v) | (k, v) <- fields]
  FnUncertain -> Aeson.Null
  FnUnknown -> Aeson.Null

-- | Handle evaluation result, checking for decode failure sentinel
handleEvalResult
  :: EntityInfo
  -> Eval.EvalDirectiveValue
  -> Maybe EvalTrace
  -> Text
  -> TraceLevel
  -> Bool
  -> Module Resolved
  -> ExceptT EvaluatorError IO ResponseWithReason
handleEvalResult ei result trace _sentinel traceLevel includeGraphViz mModule = case result of
  Eval.Assertion _ -> throwError $ InterpreterError "L4: Got an assertion instead of a normal result."
  Eval.Reduction (Left evalExc) -> throwError $ InterpreterError $ Text.textShow evalExc
  Eval.Reduction (Right val) -> do
    r <- nfToFnLiteral ei val
    -- Check if the result is NOTHING (decode failure from LEFT error) or JUST value
    actualResult <- case r of
      -- If result is FnUnknown, it means evaluation produced undefined/unknown
      FnUnknown ->
        throwError $ InterpreterError "Evaluation produced unknown value"
      -- If result is NOTHING constructor, it means JSONDECODE returned LEFT (JSON decode failed)
      FnObject [("NOTHING", _)] ->
        throwError $ InterpreterError "JSON decoding failed: input does not match expected schema"
      -- If result is JUST x (wrapper returns JUST when JSONDECODE returns RIGHT)
      FnObject [("JUST", FnArray [val'])] ->
        pure val'
      FnObject [("JUST", FnObject [(_fieldName, val')])] ->
        pure val'
      -- For backwards compatibility, if result is an array with one element
      FnArray [val'] ->
        pure val'
      -- For any other result, return as-is
      _ ->
        pure r

    pure $ ResponseWithReason
      { fnResult = Map.singleton "value" actualResult
      , reasoning = case traceLevel of
          TraceNone -> emptyTree
          TraceFull -> buildReasoningTree trace
      , graphviz =
          if includeGraphViz && traceLevel == TraceFull
            then
              fmap
                ( \tr ->
                    GraphVizResponse
                      { dot = GraphViz.traceToGraphViz GraphViz.defaultGraphVizOptions (Just mModule) tr
                      }
                )
                trace
            else Nothing
      }

createFunction ::
  FilePath ->
  FunctionDeclaration ->
  Text ->
  ModuleContext ->
  IO (RunFunction, Maybe CompiledModule)
createFunction filepath fnDecl fnImpl moduleContext = do
  -- Try to precompile for fast path
  let funRawName = mkNormalName fnDecl.name
  precompileResult <- precompileModule filepath fnImpl moduleContext funRawName

  case precompileResult of
    Right compiled -> do
      -- Fast path: use precompiled module
      let runFn = RunFunction
            { runFunction = \params' _outFilter traceLevel includeGraphViz ->
                evaluateWithCompiled filepath fnDecl compiled params' traceLevel includeGraphViz
            }
      pure (runFn, Just compiled)

    Left _err -> do
      -- Slow path fallback: use original implementation
      let runFn = RunFunction
            { runFunction = \params' _outFilter traceLevel includeGraphViz -> do
                -- 1. Typecheck original source to get function signature
                (initErrs, mTcRes) <- typecheckModule filepath fnImpl moduleContext

                tcRes <- case mTcRes of
                  Nothing -> throwError $ InterpreterError (mconcat initErrs)
                  Just tcRes -> pure tcRes

                -- 2. Get function definition and extract parameter types
                funDecide <- getFunctionDefinition funRawName tcRes.module'
                let givenParamTypes = extractParamTypes funDecide
                    assumeParamTypes = extractAssumeParamTypes tcRes.module' funDecide

                -- 3. Filter IDE directives from the original source text
                -- L4 is layout-sensitive, so we must preserve the original formatting
                let filteredSource = filterIdeDirectivesText fnImpl

                -- 4. Convert input parameters to JSON
                inputJson <- paramsToJson params'

                -- 5. Generate wrapper code
                genCode <- case generateEvalWrapper fnDecl.name givenParamTypes assumeParamTypes inputJson traceLevel of
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
                    handleEvalResult tcRes.entityInfo result trace genCode.decodeFailedSentinel traceLevel includeGraphViz tcRes.module'
                  Just [] -> throwError $ InterpreterError "L4: No #EVAL found in the program."
                  Just _xs -> throwError $ InterpreterError "L4: More than ONE #EVAL found in the program."
            }
      pure (runFn, Nothing)

-- | Create a 'RunFunction' from an already-compiled module.
-- Used by the CBOR cache path to avoid re-typechecking.
createRunFunctionFromCompiled :: FilePath -> FunctionDeclaration -> CompiledModule -> RunFunction
createRunFunctionFromCompiled filepath fnDecl compiled =
  RunFunction
    { runFunction = \params' _outFilter traceLevel includeGraphViz ->
        evaluateWithCompiled filepath fnDecl compiled params' traceLevel includeGraphViz
    }

-- | Extract parameter names and types from a DECIDE's GIVEN clause
extractParamTypes :: Decide Resolved -> [(Text, Type' Resolved)]
extractParamTypes (MkDecide _ (MkTypeSig _ (MkGivenSig _ typedNames) _) _ _) =
  mapMaybe extractTypedName typedNames
  where
    extractTypedName (MkOptionallyTypedName _ resolved (Just ty)) =
      Just (rawNameToText (rawName $ getActual resolved), ty)
    extractTypedName (MkOptionallyTypedName _ resolved Nothing) =
      -- Try to get type from resolved info
      case getAnno (getName resolved) ^. #extra % #resolvedInfo of
        Just (TypeInfo ty _) -> Just (rawNameToText (rawName $ getActual resolved), ty)
        _ -> Nothing

-- | Find the first function with the given name in the module.
getFunctionDefinition :: (Monad m) => RawName -> Module Resolved -> ExceptT EvaluatorError m (Decide Resolved)
getFunctionDefinition name (MkModule _ _ sect) = case goSection sect of
  Nothing -> throwError $ InterpreterError $ "L4: No function with name " <> prettyLayout name <> " found."
  Just dec -> pure dec
 where
  goSection (MkSection _ _ _ decls) =
    listToMaybe $ mapMaybe (goTopDecl) decls

  rawNameOfDecide = rawNameToText . rawName . getActual . nameOf

  goTopDecl = \case
    Decide _ dec ->
      if rawNameOfDecide dec == rawNameToText name
        then Just dec
        else Nothing
    Declare _ _ -> Nothing
    Assume _ _ -> Nothing
    Directive _ _ -> Nothing
    Import _ _ -> Nothing
    Section _ s -> goSection s

  nameOf (MkDecide _ _ (MkAppForm _ n _ _) _) = n

-- | Build the evaluator Environment for imported modules
-- This evaluates all imports and returns their combined environment
-- so that execEvalExprInContextOfModule can reference imported definitions
buildImportEnvironment
  :: FilePath
  -> Text
  -> ModuleContext
  -> EntityInfo
  -> IO EvalEnv.Environment
buildImportEnvironment filepath source moduleContext _entityInfo = do
  fixedNow <- Eval.readFixedNowEnv
  evalConfig <- Eval.resolveEvalConfig fixedNow apiDefaultPolicy

  result <- oneshotL4ActionAndErrors evalConfig filepath $ \nfp -> do
    let uri = normalizedFilePathToUri nfp
    -- Add all module files as virtual files for IMPORT resolution
    forM_ (Map.toList moduleContext) $ \(path, content) -> do
      let modulePath = toNormalizedFilePath ("." <> takeFileName path)
      _ <- Shake.addVirtualFile modulePath content
      pure ()
    -- Add the main file
    _ <- Shake.addVirtualFile nfp source
    -- Use GetLazyEvaluationDependencies which returns (Environment, [EvalDirectiveResult])
    Shake.use (Shake.AttachCallStack [uri] Rules.GetLazyEvaluationDependencies) uri

  case result of
    (_errs, Just (importEnv, _dirResults)) ->
      -- Successfully built import environment
      pure importEnv
    (_errs, Nothing) ->
      -- Fall back to empty environment if evaluation fails
      pure emptyEnvironment

-- ----------------------------------------------------------------------------
-- Helpers and other non-generic functions
-- ----------------------------------------------------------------------------

nfToFnLiteral :: (Monad m) => EntityInfo -> Eval.NF -> ExceptT EvaluatorError m FnLiteral
nfToFnLiteral ei (Eval.MkNF v) = valueToFnLiteral ei v
nfToFnLiteral _  Eval.Omitted  = pure FnUnknown

valueToFnLiteral :: (Monad m) => EntityInfo -> Eval.Value Eval.NF -> ExceptT EvaluatorError m FnLiteral
valueToFnLiteral ei = \case
  Eval.ValNumber i ->
    pure $ case isInteger i of
      Just int -> FnLitInt int
      Nothing -> FnLitDouble $ fromRational i
  Eval.ValDate day ->
    pure $ FnLitString (Text.textShow day)
  Eval.ValString t -> pure $ FnLitString t
  Eval.ValNil -> pure $ FnArray []
  Eval.ValCons v1 v2 -> nfToFnLiteral ei v1 >>= \ l1 -> listToFnLiteral ei (DList.singleton l1) v2
  Eval.ValClosure{} -> throwError $ InterpreterError "#EVAL produced function closure."
  Eval.ValNullaryBuiltinFun{} -> throwError $ InterpreterError "#EVAL produced builtin closure."
  Eval.ValBinaryBuiltinFun{} -> throwError $ InterpreterError "#EVAL produced function closure."
  Eval.ValUnaryBuiltinFun{} -> throwError $ InterpreterError "#EVAL produced builtin closure."
  Eval.ValTernaryBuiltinFun{} -> throwError $ InterpreterError "#EVAL produced builtin closure."
  Eval.ValPartialTernary{} -> throwError $ InterpreterError "#EVAL produced partial closure."
  Eval.ValPartialTernary2{} -> throwError $ InterpreterError "#EVAL produced partial closure."
  -- Deontic values: serialize as structured JSON objects
  Eval.ValObligation _env party action due _followup _lest -> do
    partyLit <- serializeEitherValue ei party
    let actionLit = serializeRAction action
    dueLit <- serializeDue ei due
    pure $ FnObject
      [ ("OBLIGATION", FnObject
          [ ("party", partyLit)
          , ("modal", actionLit.modal)
          , ("action", actionLit.actionPat)
          , ("deadline", dueLit)
          ])
      ]

  Eval.ValBreached reason -> do
    reasonLit <- serializeBreachReason ei reason
    pure $ FnObject [("BREACH", reasonLit)]

  Eval.ValROp _env op left right -> do
    leftLit <- serializeEitherValue ei left
    rightLit <- serializeEitherValue ei right
    let opStr = case op of
          Eval.ValROr  -> "OR" :: Text
          Eval.ValRAnd -> "AND"
    pure $ FnObject
      [ (opStr, FnArray [leftLit, rightLit])
      ]

  Eval.ValEnvironment{} -> throwError $ InterpreterError "#EVAL produced environment."
  Eval.ValUnappliedConstructor name ->
    pure $ FnLitString $ prettyLayout name
  Eval.ValConstructor resolved [] ->
    -- Special case boolean constructors (preserve original casing for others)
    let name = prettyLayout $ getActual resolved
     in case Text.toUpper name of
          "TRUE" -> pure $ FnLitBool True
          "FALSE" -> pure $ FnLitBool False
          -- Other nullary constructors become strings (original casing preserved)
          _ -> pure $ FnLitString name
  Eval.ValConstructor resolved vals -> do
    lits <- traverse (nfToFnLiteral ei) vals
    let name = prettyLayout $ getActual resolved
        fieldNames = lookupFieldNames ei resolved
    pure $ case fieldNames of
      Just names | length names == length lits ->
        -- Constructor has named fields: produce an object with field names
        FnObject
          [ (name, FnObject (zip names lits))
          ]
      _ ->
        -- No field names available or count mismatch: fall back to array
        FnObject
          [ (name, FnArray lits)
          ]
  Eval.ValAssumed var ->
    throwError $ InterpreterError $ "#EVAL produced ASSUME: " <> prettyLayout var

-- | Look up field names for a constructor from the EntityInfo.
-- Returns 'Just' field names if the constructor has named fields, 'Nothing' otherwise.
-- First tries Unique-based lookup (fast, works for direct evaluation path),
-- then falls back to name-based lookup (for wrapper evaluation path where
-- re-typechecking generates different Uniques).
lookupFieldNames :: EntityInfo -> Resolved -> Maybe [Text]
lookupFieldNames ei resolved =
  case Map.lookup (getUnique resolved) ei of
    Just (_name, TypeCheck.KnownTerm conType Constructor) ->
      extractNonEmpty conType
    _ ->
      -- Fallback: search by name (wrapper path generates fresh Uniques)
      let targetName = nameToText (getActual resolved)
       in listToMaybe $ mapMaybe (matchByName targetName) (Map.elems ei)
  where
    extractNonEmpty conType =
      let names = extractFieldNamesFromType conType
       in if null names then Nothing else Just names
    matchByName target (name, TypeCheck.KnownTerm conType Constructor)
      | nameToText name == target = extractNonEmpty conType
    matchByName _ _ = Nothing

-- | Extract field names from a constructor's function type.
-- The constructor type is typically: forall args. (field1: Type1, field2: Type2, ...) -> ResultType
extractFieldNamesFromType :: Type' Resolved -> [Text]
extractFieldNamesFromType ty = case ty of
  Forall _ _ innerTy -> extractFieldNamesFromType innerTy
  Fun _ argTypes _resultTy -> mapMaybe getFieldName argTypes
  _ -> []
  where
    getFieldName :: OptionallyNamedType Resolved -> Maybe Text
    getFieldName (MkOptionallyNamedType _ maybeName _ty) =
      nameToText . TypeCheck.getName <$> maybeName

listToFnLiteral :: Monad m => EntityInfo -> DList FnLiteral -> Eval.NF -> ExceptT EvaluatorError m FnLiteral
listToFnLiteral _  acc Eval.Omitted                     = pure (FnArray (toList (DList.snoc acc FnUnknown)))
listToFnLiteral _  acc (Eval.MkNF Eval.ValNil)          = pure (FnArray (toList acc))
listToFnLiteral ei acc (Eval.MkNF (Eval.ValCons v1 v2)) = do
  l1 <- nfToFnLiteral ei v1
  listToFnLiteral ei (DList.snoc acc l1) v2
listToFnLiteral _  _acc (Eval.MkNF _)                   =
  throwError $ InterpreterError "#EVAL produced a type-incorrect list."

-- | Helper: serialized action info
data SerializedAction = SerializedAction
  { modal :: FnLiteral
  , actionPat :: FnLiteral
  }

-- | Serialize a regulative action (MUST/MAY/SHANT/DO action) to FnLiteral
serializeRAction :: RAction Resolved -> SerializedAction
serializeRAction (MkAction _ deonticModal pat _) =
  SerializedAction
    { modal = FnLitString $ case deonticModal of
        DMust    -> "MUST"
        DMay     -> "MAY"
        DMustNot -> "SHANT"
        DDo      -> "DO"
    , actionPat = FnLitString (Print.prettyLayout pat)
    }

-- | Serialize Either RExpr (Value NF) — used for obligation party and ROp operands
serializeEitherValue :: (Monad m) => EntityInfo -> Either (Expr Resolved) (Eval.Value Eval.NF) -> ExceptT EvaluatorError m FnLiteral
serializeEitherValue _  (Left expr)  = pure $ FnLitString (Print.prettyLayout expr)
serializeEitherValue ei (Right val)  = valueToFnLiteral ei val

-- | Serialize the deadline/due field of an obligation
serializeDue :: (Monad m) => EntityInfo -> Either (Maybe (Expr Resolved)) (Eval.Value Eval.NF) -> ExceptT EvaluatorError m FnLiteral
serializeDue _  (Left Nothing)     = pure FnUnknown
serializeDue _  (Left (Just expr)) = pure $ FnLitString (Print.prettyLayout expr)
serializeDue ei (Right val)        = valueToFnLiteral ei val

-- | Serialize a breach reason to FnLiteral
serializeBreachReason :: (Monad m) => EntityInfo -> Eval.ReasonForBreach Eval.NF -> ExceptT EvaluatorError m FnLiteral
serializeBreachReason ei = \case
  Eval.DeadlineMissed evParty evAction evTimestamp _oblParty oblAction oblDeadline -> do
    evPartyLit <- nfToFnLiteral ei evParty
    evActionLit <- nfToFnLiteral ei evAction
    let oblActionLit = serializeRAction oblAction
    pure $ FnObject
      [ ("reason", FnLitString "deadline_missed")
      , ("eventParty", evPartyLit)
      , ("eventAction", evActionLit)
      , ("timestamp", FnLitDouble $ fromRational evTimestamp)
      , ("obligationAction", oblActionLit.actionPat)
      , ("deadline", FnLitDouble $ fromRational oblDeadline)
      ]
  Eval.ExplicitBreach mParty mReason -> do
    partyLit <- case mParty of
      Just nf -> nfToFnLiteral ei nf
      Nothing -> pure FnUnknown
    reasonLit <- case mReason of
      Just nf -> nfToFnLiteral ei nf
      Nothing -> pure FnUnknown
    pure $ FnObject
      [ ("reason", FnLitString "explicit")
      , ("party", partyLit)
      , ("detail", reasonLit)
      ]

-- ----------------------------------------------------------------------------
-- L4 helpers
-- ----------------------------------------------------------------------------

typecheckModule :: (MonadIO m) => FilePath -> Text -> ModuleContext -> m ([Text], Maybe Rules.TypeCheckResult)
typecheckModule file input moduleContext = do
  fixedNow <- liftIO Eval.readFixedNowEnv
  evalConfig <- liftIO $ Eval.resolveEvalConfig fixedNow apiDefaultPolicy
  liftIO $ oneshotL4ActionAndErrors evalConfig file \nfp -> do
    let
      uri = normalizedFilePathToUri nfp
    -- Add all module files as virtual files for IMPORT resolution
    forM_ (Map.toList moduleContext) $ \(path, content) -> do
      let modulePath = toNormalizedFilePath ("./" <> takeFileName path)
      _ <- Shake.addVirtualFile modulePath content
      pure ()
    -- Add the main file
    _ <- Shake.addVirtualFile nfp input
    Shake.use Rules.TypeCheck uri

evaluateModule :: (MonadIO m) => FilePath -> Text -> ModuleContext -> m ([Text], Maybe [Eval.EvalDirectiveResult])
evaluateModule file input moduleContext = do
  fixedNow <- liftIO Eval.readFixedNowEnv
  evalConfig <- liftIO $ Eval.resolveEvalConfig fixedNow apiDefaultPolicy
  liftIO $ oneshotL4ActionAndErrors evalConfig file \nfp -> do
    let
      uri = normalizedFilePathToUri nfp
    -- Add all module files as virtual files for IMPORT resolution
    forM_ (Map.toList moduleContext) $ \(path, content) -> do
      let modulePath = toNormalizedFilePath ("./" <> takeFileName path)
      _ <- Shake.addVirtualFile modulePath content
      pure ()
    -- Add the main file
    _ <- Shake.addVirtualFile nfp input
    Shake.use Rules.EvaluateLazy uri

-- ----------------------------------------------------------------------------
-- L4 syntax builders
-- ----------------------------------------------------------------------------

mkNormalName :: Text -> RawName
mkNormalName = NormalName

-- ----------------------------------------------------------------------------
-- Trace builders
-- ----------------------------------------------------------------------------

buildReasoningTree :: Maybe EvalTrace -> Reasoning
buildReasoningTree xs =
  Reasoning
    { payload = toReasoningTree xs
    }

toReasoningTree :: Maybe EvalTrace -> ReasoningTree
toReasoningTree Nothing  = ReasoningTree (ReasonNode [] []) []
toReasoningTree (Just t) = toReasoningTree' t

toReasoningTree' :: EvalTrace -> ReasoningTree
toReasoningTree' (Trace lbl [] val) =
  ReasoningTree
    { payload =
        ReasonNode
          { exampleCode = labelExample lbl
          , explanation = [resultLine val]
          }
    , children = []
    }
toReasoningTree' (Trace lbl [(expr, children)] val) =
  ReasoningTree
    { payload =
        ReasonNode
          { exampleCode = labelExample lbl <> [Print.prettyLayout expr]
          , explanation = [resultLine val]
          }
    , children = fmap toReasoningTree' children
    }
toReasoningTree' (Trace lbl ((expr, children) : rest) val) =
  toReasoningTree' (Trace lbl [(expr, children ++ [Trace lbl rest val])] val)

labelExample :: Maybe Resolved -> [Text]
labelExample = maybe [] (\resolved -> [nameToText (getOriginal resolved)])

resultLine :: Either EvalException Eval.NF -> Text
resultLine val =
  "Result: " <> case val of
    Left exc -> Text.unlines (Eval.prettyEvalException exc)
    Right v -> Print.prettyLayout v
