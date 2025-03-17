module Backend.Jl4 (createFunction) where

import Backend.Api
import Base.Text
import qualified Base.Text as Text
import Control.Monad.Trans.Except
import L4.Annotation
import L4.Evaluate
import qualified L4.Evaluate as Evaluate
import qualified L4.Evaluate.Value as Eval
import L4.Main
import L4.Print
import qualified L4.Print as Print
import L4.Syntax
import System.FilePath ((<.>))

createFunction ::
  (Monad m) =>
  FunctionDeclaration ->
  Text ->
  ExceptT EvaluatorError m RunFunction
createFunction fnDecl fnImpl =
  pure $
    RunFunction
      { runFunction = \params _outFilter {- TODO: how to handle the outFilter? -} -> do
          l4Params <- traverse (uncurry toL4Param) params
          let
            l4InputWithEval =
              Text.unlines
                [ fnImpl
                , prettyLayout $ evalStatement l4Params
                ]
          case parseAndCheck file l4InputWithEval of
            Left cliError -> do
              let
                errMsg = prettyCliError cliError
              throwE $ InterpreterError errMsg
            Right p -> do
              case Evaluate.doEvalProgram p of
                [(_srcRange, valEither, evalTrace)] -> case valEither of
                  Left evalExc -> throwE $ InterpreterError $ Text.show evalExc
                  Right val -> do
                    r <- valueToFnLiteral val
                    pure $
                      ResponseWithReason
                        { values = [("result", r)]
                        , reasoning = buildReasoningTree evalTrace
                        }
                [] -> throwE $ InterpreterError "L4 Internal Error: No #EVAL"
                _xs -> throwE $ InterpreterError "L4 Error: More than ONE #EVAL found"
      }
 where
  toL4Param _ Nothing = do
    throwE CannotHandleUnknownVars
  toL4Param nameText (Just fnLiteral) = do
    (mkName nameText,) <$> literalToExpr fnLiteral

  file = Text.unpack fnDecl.name <.> "l4"

  funName = mkName fnDecl.name

  inputName = mkName "Inputs"

  evalStatement args =
    mkTopDeclDirective $
      mkEval $
        mkFunApp
          funName
          [ mkInputs inputName $
              fmap (uncurry mkArg) args
          ]

literalToExpr :: (Monad m) => FnLiteral -> ExceptT EvaluatorError m (Expr Name)
literalToExpr = \case
  FnLitInt i -> pure . mkLit $ mkNumericLit $ fromIntegral i
  FnLitDouble d -> throwE $ CannotHandleParameterType $ FnLitDouble d
  FnLitBool b -> pure . mkVar $ mkBoolean b
  FnLitString s -> pure . mkLit $ mkStringLit s
  FnArray arr -> do
    es <- traverse literalToExpr arr
    pure $ mkList es
  FnObject obj -> throwE $ CannotHandleParameterType $ FnObject obj
  FnUncertain -> pure $ mkVar mkUncertain
  FnUnknown -> pure $ mkVar mkUnknown

valueToFnLiteral :: (Monad m) => Eval.Value -> ExceptT EvaluatorError m FnLiteral
valueToFnLiteral = \case
  Eval.ValNumber i -> pure $ FnLitInt $ fromIntegral i
  Eval.ValString t -> pure $ FnLitString t
  Eval.ValList vals -> do
    lits <- traverse valueToFnLiteral vals
    pure $ FnArray lits
  Eval.ValClosure _ _ _ -> do
    throwE $ InterpreterError $ "#EVAL produced function closure."
  Eval.ValUnappliedConstructor name ->
    pure $ FnLitString $ prettyLayout name
  Eval.ValConstructor resolved [] ->
    -- Constructors such as TRUE and FALSE
    pure $ FnLitString $ prettyLayout $ getActual resolved
  Eval.ValConstructor resolved vals -> do
    lits <- traverse valueToFnLiteral vals
    pure $
      FnObject
        [ (prettyLayout $ getActual resolved, FnArray lits)
        ]
  Eval.ValAssumed var ->
    throwE $ InterpreterError $ "#EVAL produced ASSUME: " <> prettyLayout var

buildReasoningTree :: EvalTrace -> Reasoning
buildReasoningTree xs =
  Reasoning
    { payload = toReasoningTree xs
    }

toReasoningTree :: EvalTrace -> ReasoningTree
toReasoningTree (Trace herald expr children val) =
  ReasoningTree
    { payload =
        ReasonNode
          { exampleCode =
              [Print.prettyLayout expr]
          , explanation =
              case herald of
                Nothing -> []
                Just h -> [h]
                <> [ "the result is: " <> case val of
                      Left exc -> Text.show exc
                      Right v -> Print.prettyLayout v
                   ]
          }
    , children = fmap toReasoningTree children
    }

-- ----------------------------------------------------------------------------
-- L4 syntax builders
-- ----------------------------------------------------------------------------

mkTopDeclDirective :: Directive n -> TopDecl n
mkTopDeclDirective = Directive emptyAnno

mkEval :: Expr n -> Directive n
mkEval = Eval emptyAnno

mkFunApp :: n -> [Expr n] -> Expr n
mkFunApp =
  App emptyAnno

mkInputs :: n -> [NamedExpr n] -> Expr n
mkInputs con args =
  AppNamed emptyAnno con args Nothing

mkArg :: n -> Expr n -> NamedExpr n
mkArg =
  MkNamedExpr emptyAnno

mkName :: Text -> Name
mkName =
  MkName emptyAnno . NormalName

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

mkNumericLit :: Int -> Lit
mkNumericLit =
  NumericLit emptyAnno

mkStringLit :: Text -> Lit
mkStringLit =
  StringLit emptyAnno

mkList :: [Expr n] -> Expr n
mkList =
  List emptyAnno

mkUncertain :: Name
mkUncertain = mkName "uncertain"

mkUnknown :: Name
mkUnknown = mkName "unknown"
