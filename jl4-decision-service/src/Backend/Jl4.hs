module Backend.Jl4 (createFunction) where

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
import qualified L4.TypeCheck.Environment as TypeCheck
import L4.Utils.Ratio
import qualified LSP.Core.Shake as Shake
import LSP.L4.Oneshot (oneshotL4ActionAndErrors)
import qualified LSP.L4.Rules as Rules

import Language.LSP.Protocol.Types (normalizedFilePathToUri)
import System.FilePath ((<.>))

import Backend.Api

createFunction ::
  FunctionDeclaration ->
  Text ->
  RunFunction
createFunction fnDecl fnImpl =
  RunFunction
    { runFunction = \params' _outFilter {- TODO: how to handle the outFilter? -} -> do
        params <- assumeNoUnknowns params'
        (initErrs, mTcRes) <- typecheckModule file fnImpl
        funExpr <- case mTcRes of
          Nothing -> throwError $ InterpreterError (mconcat initErrs)
          Just tcRes -> do
            let
              recordMap = getAllRecords tcRes.module'
            l4fun <- getFunctionDefinition funRawName tcRes.module'
            buildEvalFunApp funRawName l4fun recordMap params
        let
          l4InputWithEval =
            Text.unlines
              [ fnImpl
              , prettyLayout $ mkTopDeclDirective $ mkEvalTrace funExpr
              ]

        (errs, mEvalRes) <- evaluateModule file l4InputWithEval
        case mEvalRes of
          Nothing -> throwError $ InterpreterError (mconcat errs)
          Just [Eval.MkEvalDirectiveResult{result, trace}] -> case result of
                Left evalExc -> throwError $ InterpreterError $ Text.show evalExc
                Right val -> do
                  r <- nfToFnLiteral val
                  pure $
                    ResponseWithReason
                      { values = [("result", r)]
                      , reasoning = buildReasoningTree trace
                      }
          Just [] -> throwError $ InterpreterError "L4: No #EVAL found in the program."
          Just _xs -> throwError $ InterpreterError "L4: More than ONE #EVAL found in the program."
    }
 where
  file = Text.unpack fnDecl.name <.> "l4"
  funRawName = mkNormalName fnDecl.name

  assumeNoUnknowns :: (Monad m) => [(Text, Maybe FnLiteral)] -> ExceptT EvaluatorError m [(Text, FnLiteral)]
  assumeNoUnknowns inputs = do
    traverse assumeKnown inputs

  assumeKnown :: (Monad m) => (Text, Maybe FnLiteral) -> ExceptT EvaluatorError m (Text, FnLiteral)
  assumeKnown (t, Just l) = pure (t, l)
  assumeKnown (t, Nothing) = throwError $ InterpreterError $ "L4: can't handle missing values for field: " <> t

type RecordMap = Map RawName [TypedName Resolved]

-- | Build the final function application for the given
--
-- See Note [Support for nested objects] for details.
buildEvalFunApp ::
  (Monad m) =>
  RawName ->
  Decide Resolved ->
  RecordMap ->
  [(Text, FnLiteral)] ->
  ExceptT EvaluatorError m (Expr Name)
buildEvalFunApp funName decide recordMap args = do
  exprArgs <- matchFunctionArgs recordMap args funArgs
  pure $ mkNamedFunApp (mkName funName) exprArgs
 where
  MkDecide _ (MkTypeSig _ given _) _ _ = decide
  MkGivenSig _ ns = given
  funArgs = mapMaybe typedNameToTuple ns
  typedNameToTuple (MkOptionallyTypedName _ n (Just ty)) = case ty of
    Type{} -> Nothing -- We don't care about TYPE
    TyApp{} -> Just (n, ty)
    Fun{} -> Just (n, ty)
    Forall{} -> Just (n, ty)
    InfVar{} -> Nothing -- Should never happen
  typedNameToTuple (MkOptionallyTypedName _ _ Nothing) = Nothing

matchFunctionArgs :: (Monad m) => RecordMap -> [(Text, FnLiteral)] -> [(Resolved, Type' Resolved)] -> ExceptT EvaluatorError m [NamedExpr Name]
matchFunctionArgs recordMap input parameters =
  zipWithM (matchFunctionArg recordMap) (sortOn fst input) (sortOn (resolvedRawName . fst) parameters)
 where
  resolvedRawName = rawName . getActual

-- | Try to match the given argument 'FnLiteral' with the expected type of the argument.
--
-- See Note [Support for nested objects] for details.
matchFunctionArg ::
  (Monad m) =>
  RecordMap ->
  (Text, FnLiteral) ->
  (Resolved, Type' Resolved) ->
  ExceptT EvaluatorError m (NamedExpr Name)
matchFunctionArg recordMap (inputName, inputValue) (r, ty)
  | inputName /= rawNameToText (rawNameOfResolved r) =
      throwError $ InterpreterError $ "L4: Unexpected parameter name, expected " <> rawNameToText (rawNameOfResolved r) <> ", but got " <> inputName
  | otherwise = do
      arguments <- matchFunctionArg' recordMap inputValue (r, ty)
      pure $ mkArg (mkNormalNameText inputName) arguments

-- | Worker function of 'matchFunctionArg'. Match the 'FnLiteral' with the
-- expected type given by @'Type'' 'Resolved'@.
matchFunctionArg' ::
  (Monad m) =>
  RecordMap ->
  FnLiteral ->
  (Resolved, Type' Resolved) ->
  ExceptT EvaluatorError m (Expr Name)
matchFunctionArg' recordMap inputValue (r, ty)
  | Just (constrName, fieldNames) <- lookupRecordFields recordMap ty = do
      matchRecord recordMap constrName fieldNames inputValue
  | Just nty <- isListConstr r ty = do
      -- We need to treat list of objects differently to
      vals <- expectArray inputValue
      case lookupRecordFields recordMap nty of
        Just (constrName, fieldNames) -> do
          l4Vals <- traverse (matchRecord recordMap constrName fieldNames) vals
          pure $ mkList l4Vals
        Nothing -> do
          l4Vals <- traverse literalToExpr vals
          pure $ mkList l4Vals
  | otherwise =
      literalToExpr inputValue

-- | Given the 'RawName' of a record and its fields, match the 'FnLiteral' with the
-- fields of the record.
-- The fields of the record may also be records, which we will recursively match.
matchRecord :: (Monad m) => RecordMap -> RawName -> [TypedName Resolved] -> FnLiteral -> ExceptT EvaluatorError m (Expr Name)
matchRecord recordMap constrName fieldNames inputValue = do
  inputObj <- expectObject inputValue
  arguments <- matchFunctionArgs recordMap inputObj (fmap toRecord fieldNames)
  pure (mkNamedFunApp (mkName constrName) arguments)
 where
  toRecord (MkTypedName _ n ns) = (n, ns)

isListConstr :: Resolved -> Type' Resolved -> Maybe (Type' Resolved)
isListConstr r ty
  | r == TypeCheck.listRef = case ty of
      TyApp _ _list [innerTy] -> Just innerTy
      _ -> Nothing
  | otherwise = Nothing

-- | Given a type, lookup the 'RawName' of the constructor and the fields of the record.
lookupRecordFields :: RecordMap -> Type' Resolved -> Maybe (RawName, [TypedName Resolved])
lookupRecordFields recordMap ty = do
  tyName <- tyNameOf ty
  fieldNames <- Map.lookup (rawNameOfResolved tyName) recordMap
  pure (rawNameOfResolved tyName, fieldNames)
 where
  tyNameOf = \case
    Type{} -> Nothing
    TyApp _ n _ -> Just n
    Fun{} -> Nothing
    Forall _ _ fty -> tyNameOf fty
    InfVar{} -> Nothing

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

-- | Find all records defined in this module.
getAllRecords :: Module Resolved -> RecordMap
getAllRecords (MkModule _ _ sect) = Map.fromList $ goSection sect
 where
  goSection (MkSection _ _ _ decls) =
    Base.concatMap goDecl decls

  goDecl = \case
    Decide _ _ -> []
    Declare _ decl -> goDeclare decl
    Assume _ _ -> []
    Directive _ _ -> []
    Import _ _ -> []
    Section _ s -> goSection s

  goDeclare (MkDeclare _ _ _ tyDecl) = maybeToList $ isRecordDecl tyDecl

isRecordDecl :: (HasName n) => TypeDecl n -> Maybe (RawName, [TypedName n])
isRecordDecl = \case
  RecordDecl _ (Just n) typedNames -> Just (rawName $ getName n, typedNames)
  RecordDecl _ Nothing _ -> Nothing
  EnumDecl{} -> Nothing
  SynonymDecl{} -> Nothing

{-
Note [Support for nested objects]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We support nested function arguments, including objects, arrays, and arrays of objects.
To support this, we perform type-directed deserialisation.

1. Collect all record names and their fields defined in the module.
2. Find the function that will be executed.
3. Extract the arguments of the function from the 'GivenSig'.
  * We ignore 'TYPE' variables, as we only generate an untyped AST node, it is up to the
    later stages to reject ill-typed programs.
4. Match the 'FnLiteral' with the arguments of the function.
  * Essentially zipping the arguments with the expected arguments.
5. For each 'FnLiteral' check whether the argument type matches.
  * For objects, the type of the function argument needs to be a record.
    We retrieve the fields of the record and match them with the fields of the object.
    Then, repeat step (5)
  * For arrays, we check whether a 'LIST ty' is expected.
    If it is, we use the type information of 'ty'
  * For all other types, L4 provides primitives to which we translate.
6. Build the final function application.
-}

-- ----------------------------------------------------------------------------
-- Helpers and other non-generic functions
-- ----------------------------------------------------------------------------

rawNameOfResolved :: Resolved -> RawName
rawNameOfResolved = rawName . getActual

expectObject :: (Monad m) => FnLiteral -> ExceptT EvaluatorError m [(Text, FnLiteral)]
expectObject = \case
  FnObject flds -> pure flds
  _ -> throwError $ InterpreterError "L4: expected object but got something else."

expectArray :: (Monad m) => FnLiteral -> ExceptT EvaluatorError m [FnLiteral]
expectArray = \case
  FnArray arr -> pure arr
  _ -> throwError $ InterpreterError "L4: expected list but got something else."

-- | Translate simple 'FnLiteral' to 'Expr Name'.
-- Does not work for nested objects, such as 'FnArray' and 'FnObject'.
literalToExpr :: (Monad m) => FnLiteral -> ExceptT EvaluatorError m (Expr Name)
literalToExpr = \case
  FnLitInt i -> pure . mkLit $ realToLit i
  FnLitDouble d -> pure . mkLit $ realToLit d
  FnLitBool b -> pure . mkVar $ mkBoolean b
  FnLitString s -> pure . mkLit $ mkStringLit s
  FnArray arr -> throwError $ CannotHandleParameterType $ FnArray arr
  FnObject obj -> throwError $ CannotHandleParameterType $ FnObject obj
  FnUncertain -> pure $ mkVar mkUncertain
  FnUnknown -> pure $ mkVar mkUnknown

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

typecheckModule :: (MonadIO m) => FilePath -> Text -> m ([Text], Maybe Rules.TypeCheckResult)
typecheckModule file input = do
  liftIO $ oneshotL4ActionAndErrors file \nfp -> do
    let
      uri = normalizedFilePathToUri nfp
    _ <- Shake.addVirtualFile nfp input
    Shake.use Rules.TypeCheck uri

evaluateModule :: (MonadIO m) => FilePath -> Text -> m ([Text], Maybe [Eval.EvalDirectiveResult])
evaluateModule file input =
  liftIO $ oneshotL4ActionAndErrors file \nfp -> do
    let
      uri = normalizedFilePathToUri nfp
    _ <- Shake.addVirtualFile nfp input
    Shake.use Rules.EvaluateLazy uri

-- ----------------------------------------------------------------------------
-- L4 syntax builders
-- ----------------------------------------------------------------------------

mkTopDeclDirective :: Directive n -> TopDecl n
mkTopDeclDirective = Directive emptyAnno

mkEvalTrace :: Expr n -> Directive n
mkEvalTrace = LazyEvalTrace emptyAnno

mkNamedFunApp :: n -> [NamedExpr n] -> Expr n
mkNamedFunApp con args =
  AppNamed emptyAnno con args Nothing

mkArg :: n -> Expr n -> NamedExpr n
mkArg =
  MkNamedExpr emptyAnno

mkName :: RawName -> Name
mkName =
  MkName emptyAnno

mkNormalName :: Text -> RawName
mkNormalName = NormalName

mkNormalNameText :: Text -> Name
mkNormalNameText = mkName . mkNormalName

mkVar :: n -> Expr n
mkVar =
  Var emptyAnno

mkLit :: Lit -> Expr n
mkLit =
  Lit emptyAnno

l4True :: Name
l4True =
  MkName emptyAnno $ NormalName "TRUE"

l4False :: Name
l4False =
  MkName emptyAnno $ NormalName "FALSE"

mkBoolean :: Bool -> Name
mkBoolean b =
  case b of
    True -> l4True
    False -> l4False

realToLit :: (Real a) => a -> Lit
realToLit =
  NumericLit emptyAnno . toRational

mkStringLit :: Text -> Lit
mkStringLit =
  StringLit emptyAnno

mkList :: [Expr n] -> Expr n
mkList =
  List emptyAnno

mkUncertain :: Name
mkUncertain = mkNormalNameText "uncertain"

mkUnknown :: Name
mkUnknown = mkNormalNameText "unknown"

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
