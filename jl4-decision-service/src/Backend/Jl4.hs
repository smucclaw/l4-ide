{-# LANGUAGE ViewPatterns #-}
module Backend.Jl4 (createFunction, getFunctionDefinition, buildFunDecide, ModuleContext, CompiledModule(..), precompileModule, evaluateWithCompiled, typecheckModule) where

import Base hiding (trace)
import qualified Base.DList as DList
import qualified Base.Map as Map
import qualified Base.Text as Text

import L4.Annotation
-- import qualified L4.Evaluate.Value as Eval
import qualified L4.Evaluate.ValueLazy as Eval
import qualified L4.EvaluateLazy as Eval
import L4.EvaluateLazy.Machine (EvalException, emptyEnvironment)
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
import Backend.CodeGen (generateEvalWrapper, GeneratedCode(..))
import Backend.DirectiveFilter (filterIdeDirectives)
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
  , compiledEnvironment :: !Environment
  , compiledEntityInfo :: !EntityInfo
  , compiledDecide :: !(Decide Resolved)
  , compiledModuleContext :: !ModuleContext  -- ^ Context needed for IMPORT resolution
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

  -- Build the compiled module
  pure CompiledModule
    { compiledModule = tcRes.module'
    , compiledEnvironment = tcRes.environment
    , compiledEntityInfo = tcRes.entityInfo
    , compiledDecide = decide
    , compiledModuleContext = moduleContext  -- Store context for IMPORT resolution
    }
 where
  evalErrorToText :: EvaluatorError -> Text
  evalErrorToText (InterpreterError t) = t
  evalErrorToText (RequiredParameterMissing pm) = "Required parameter missing: expected " <> Text.show pm.expected <> ", got " <> Text.show pm.actual
  evalErrorToText (UnknownArguments args) = "Unknown arguments: " <> Text.intercalate ", " args
  evalErrorToText (CannotHandleParameterType lit) = "Cannot handle parameter type: " <> Text.show lit
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

  -- Pass empty environment - the module will be evaluated fresh each time
  -- The evaluator's Environment (Map Unique Reference) is different from
  -- the typechecker's Environment (Map RawName [Unique])
  mResult <- liftIO $ Eval.execEvalExprInContextOfModule
    evalConfig
    compiled.compiledEntityInfo
    callExpr
    (emptyEnvironment, compiled.compiledModule)

  -- Handle result
  case mResult of
    Nothing -> throwError $ InterpreterError "L4: Expression evaluation failed."
    Just Eval.MkEvalDirectiveResult{result, trace} ->
      handleEvalResultDirect result trace traceLevel includeGraphViz compiled.compiledModule

-- | Handle evaluation result (simplified version for direct evaluation)
handleEvalResultDirect
  :: Eval.EvalDirectiveValue
  -> Maybe EvalTrace
  -> TraceLevel
  -> Bool
  -> Module Resolved
  -> ExceptT EvaluatorError IO ResponseWithReason
handleEvalResultDirect result trace traceLevel includeGraphViz mModule = case result of
  Eval.Assertion _ -> throwError $ InterpreterError "L4: Got an assertion instead of a normal result."
  Eval.Reduction (Left evalExc) -> throwError $ InterpreterError $ Text.show evalExc
  Eval.Reduction (Right val) -> do
    r <- nfToFnLiteral val
    pure $ ResponseWithReason
      { values = [("result", r)]
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
                      , png = Nothing
                      , svg = Nothing
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
      handleEvalResult result trace genCode.decodeFailedSentinel traceLevel includeGraphViz compiled.compiledModule
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
  -- Import the precompiled module's declarations into the wrapper's context
  -- For simplicity, we concatenate the filtered module text with the wrapper
  -- But we skip re-typechecking the original module
  let filteredModule = filterIdeDirectives compiled.compiledModule
      filteredSource = prettyLayout filteredModule
      combinedProgram = filteredSource <> wrapperCode

  -- Evaluate the combined program using the original module context
  -- This ensures IMPORT statements can be resolved correctly
  evaluateModule filepath combinedProgram compiled.compiledModuleContext

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
  :: Eval.EvalDirectiveValue
  -> Maybe EvalTrace
  -> Text
  -> TraceLevel
  -> Bool
  -> Module Resolved
  -> ExceptT EvaluatorError IO ResponseWithReason
handleEvalResult result trace _sentinel traceLevel includeGraphViz mModule = case result of
  Eval.Assertion _ -> throwError $ InterpreterError "L4: Got an assertion instead of a normal result."
  Eval.Reduction (Left evalExc) -> throwError $ InterpreterError $ Text.show evalExc
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
      , graphviz =
          if includeGraphViz && traceLevel == TraceFull
            then
              fmap
                ( \tr ->
                    GraphVizResponse
                      { dot = GraphViz.traceToGraphViz GraphViz.defaultGraphVizOptions (Just mModule) tr
                      , png = Nothing
                      , svg = Nothing
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
                -- TODO: consider supporting output filters in slow path
                -- 1. Typecheck original source to get function signature
                (initErrs, mTcRes) <- typecheckModule filepath fnImpl moduleContext

                tcRes <- case mTcRes of
                  Nothing -> throwError $ InterpreterError (mconcat initErrs)
                  Just tcRes -> pure tcRes

                -- 2. Get function definition and extract parameter types
                funDecide <- getFunctionDefinition funRawName tcRes.module'
                let givenParamTypes = extractParamTypes funDecide
                    assumeParamTypes = extractAssumeParamTypes tcRes.module' funDecide

                -- 3. Filter IDE directives from the module
                let filteredModule = filterIdeDirectives tcRes.module'
                    filteredSource = prettyLayout filteredModule

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
                    handleEvalResult result trace genCode.decodeFailedSentinel traceLevel includeGraphViz tcRes.module'
                  Just [] -> throwError $ InterpreterError "L4: No #EVAL found in the program."
                  Just _xs -> throwError $ InterpreterError "L4: More than ONE #EVAL found in the program."
            }
      pure (runFn, Nothing)

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

-- ----------------------------------------------------------------------------
-- Helpers and other non-generic functions
-- ----------------------------------------------------------------------------

nfToFnLiteral :: (Monad m) => Eval.NF -> ExceptT EvaluatorError m FnLiteral
nfToFnLiteral (Eval.MkNF v) = valueToFnLiteral v
nfToFnLiteral Eval.Omitted  = pure FnUnknown

valueToFnLiteral :: (Monad m) => Eval.Value Eval.NF -> ExceptT EvaluatorError m FnLiteral
valueToFnLiteral = \case
  Eval.ValNumber i ->
    pure $ case isInteger i of
      Just int -> FnLitInt int
      Nothing -> FnLitDouble $ fromRational i
  Eval.ValDate day ->
    pure $ FnLitString (Text.show day)
  Eval.ValString t -> pure $ FnLitString t
  Eval.ValNil -> pure $ FnArray []
  Eval.ValCons v1 v2 -> nfToFnLiteral v1 >>= \ l1 -> listToFnLiteral (DList.singleton l1) v2
  Eval.ValClosure{} -> throwError $ InterpreterError "#EVAL produced function closure."
  Eval.ValNullaryBuiltinFun{} -> throwError $ InterpreterError "#EVAL produced builtin closure."
  Eval.ValBinaryBuiltinFun{} -> throwError $ InterpreterError "#EVAL produced function closure."
  Eval.ValUnaryBuiltinFun{} -> throwError $ InterpreterError "#EVAL produced builtin closure."
  Eval.ValTernaryBuiltinFun{} -> throwError $ InterpreterError "#EVAL produced builtin closure."
  Eval.ValPartialTernary{} -> throwError $ InterpreterError "#EVAL produced partial closure."
  Eval.ValPartialTernary2{} -> throwError $ InterpreterError "#EVAL produced partial closure."
  Eval.ValObligation{} -> throwError $ InterpreterError "#EVAL produced obligation."
  Eval.ValEnvironment{} -> throwError $ InterpreterError "#EVAL produced environment."
  Eval.ValROp{} -> throwError $ InterpreterError "#EVAL produced regulative operator."
  Eval.ValBreached{} -> throwError $ InterpreterError "#EVAL produced breach."
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
    lits <- traverse nfToFnLiteral vals
    pure $
      FnObject
        [ (prettyLayout $ getActual resolved, FnArray lits)
        ]
  Eval.ValAssumed var ->
    throwError $ InterpreterError $ "#EVAL produced ASSUME: " <> prettyLayout var

listToFnLiteral :: Monad m => DList FnLiteral -> Eval.NF -> ExceptT EvaluatorError m FnLiteral
listToFnLiteral acc Eval.Omitted                     = pure (FnArray (toList (DList.snoc acc FnUnknown)))
listToFnLiteral acc (Eval.MkNF Eval.ValNil)          = pure (FnArray (toList acc))
listToFnLiteral acc (Eval.MkNF (Eval.ValCons v1 v2)) = do
  l1 <- nfToFnLiteral v1
  listToFnLiteral (DList.snoc acc l1) v2
listToFnLiteral _acc (Eval.MkNF _)                   =
  throwError $ InterpreterError "#EVAL produced a type-incorrect list."

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
