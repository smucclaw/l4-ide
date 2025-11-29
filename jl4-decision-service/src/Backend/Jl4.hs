{-# LANGUAGE ViewPatterns #-}
module Backend.Jl4 (createFunction, getFunctionDefinition, buildFunDecide, ModuleContext) where

import Base hiding (trace)
import qualified Base.DList as DList
import qualified Base.Map as Map
import qualified Base.Text as Text

import L4.Annotation
-- import qualified L4.Evaluate.Value as Eval
import qualified L4.Evaluate.ValueLazy as Eval
import qualified L4.EvaluateLazy as Eval
import L4.EvaluateLazy.Trace
import L4.Names
import L4.Print
import qualified L4.Print as Print
import L4.Syntax
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
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Aeson
import qualified Data.Vector as Vector

-- | Map from file path to file content for module resolution
type ModuleContext = Map FilePath Text

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

createFunction ::
  FilePath ->
  FunctionDeclaration ->
  Text ->
  ModuleContext ->
  RunFunction
createFunction filepath fnDecl fnImpl moduleContext =
  RunFunction
    { runFunction = \params' _outFilter {- TODO: how to handle the outFilter? -} traceLevel -> do
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
            filteredSource = prettyLayout filteredModule

        -- 4. Convert input parameters to JSON
        inputJson <- paramsToJson params'

        -- 5. Generate wrapper code
        genCode <- case generateEvalWrapper fnDecl.name paramTypes inputJson traceLevel of
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
            handleEvalResult result trace genCode.decodeFailedSentinel traceLevel
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
      Just (rawNameToText (rawName $ getActual resolved), ty)
    extractTypedName (MkOptionallyTypedName _ resolved Nothing) =
      -- Try to get type from resolved info
      case getAnno (getName resolved) ^. #extra % #resolvedInfo of
        Just (TypeInfo ty _) -> Just (rawNameToText (rawName $ getActual resolved), ty)
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
  FnUncertain -> Aeson.Null
  FnUnknown -> Aeson.Null

-- | Handle evaluation result, checking for decode failure sentinel
handleEvalResult
  :: Eval.EvalDirectiveValue
  -> Maybe EvalTrace
  -> Text
  -> TraceLevel
  -> ExceptT EvaluatorError IO ResponseWithReason
handleEvalResult result trace _sentinel traceLevel = case result of
  Eval.Assertion _ -> throwError $ InterpreterError "L4: Got an assertion instead of a normal result."
  Eval.Reduction (Left evalExc) -> throwError $ InterpreterError $ Text.show evalExc
  Eval.Reduction (Right val) -> do
    r <- nfToFnLiteral val
    -- Check if the result is NOTHING (decode failure) or JUST value
    actualResult <- case r of
      -- If result is NOTHING, it means JSON decode failed
      FnUnknown ->
        throwError $ InterpreterError "JSON decoding failed: input does not match expected schema"
      -- If result is JUST x (represented as record/object with JUST constructor)
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
  Eval.ValString t -> pure $ FnLitString t
  Eval.ValNil -> pure $ FnArray []
  Eval.ValCons v1 v2 -> nfToFnLiteral v1 >>= \ l1 -> listToFnLiteral (DList.singleton l1) v2
  Eval.ValClosure{} -> throwError $ InterpreterError "#EVAL produced function closure."
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
    -- Constructors such as TRUE and FALSE
    pure $ FnLitString $ prettyLayout $ getActual resolved
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
  liftIO $ oneshotL4ActionAndErrors file \nfp -> do
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
evaluateModule file input moduleContext =
  liftIO $ oneshotL4ActionAndErrors file \nfp -> do
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
toReasoningTree' (Trace [] val) =
  ReasoningTree
    { payload =
        ReasonNode
          { exampleCode = []
          , explanation =
              [ "Result: " <> case val of
                  Left exc -> Text.unlines (Eval.prettyEvalException exc)
                  Right v -> Print.prettyLayout v
              ]
          }
    , children = []
    }
toReasoningTree' (Trace [(expr, children)] val) =
  ReasoningTree
    { payload =
        ReasonNode
          { exampleCode =
              [Print.prettyLayout expr]
          , explanation =
              [ "Result: " <> case val of
                  Left exc -> Text.unlines (Eval.prettyEvalException exc)
                  Right v -> Print.prettyLayout v
              ]
          }
    , children = fmap toReasoningTree' children
    }
toReasoningTree' (Trace ((expr, children) : rest) val) =
  toReasoningTree' (Trace [(expr, children ++ [Trace rest val])] val)

