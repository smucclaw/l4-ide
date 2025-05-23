module L4.Desugar (
  doDesugarModule,
  DesugarError (..),
  prettyDesugarError,
)
where

import Base

import L4.Annotation
import L4.Print (prettyLayout)
import L4.Syntax
import qualified L4.TypeCheck.Environment as TypeCheck

import qualified Base.Text as Text
import Data.List.Split (splitWhen)
import Optics

newtype Desugar a = MkDesugar (Either DesugarError a)
  deriving newtype (Functor, Applicative, Monad)

data HoleInfo
  = HoleInfo {expected :: !Int, got :: !Int}

data DesugarError where
  InternalAnnoRewritingError :: Expr Name -> HoleInfo -> DesugarError

runDesugar :: Desugar a -> Either DesugarError a
runDesugar (MkDesugar a) = a

internalError :: DesugarError -> Desugar a
internalError = MkDesugar . Left

doDesugarModule :: Module Name -> Either DesugarError (Module Name)
doDesugarModule =
  runDesugar . dsModule

dsModule :: Module Name -> Desugar (Module Name)
dsModule (MkModule ann nuri s) =
  MkModule ann nuri
    <$> dsSection s

dsSection :: Section Name -> Desugar (Section Name)
dsSection (MkSection ann mName maka topdecl) =
  MkSection ann mName maka
    <$> traverse dsTopDecl topdecl

dsTopDecl :: TopDecl Name -> Desugar (TopDecl Name)
dsTopDecl = \ case
  Declare ann d -> Declare ann <$> dsDeclare d
  Decide ann d -> Decide ann <$> dsDecide d
  Assume ann d -> Assume ann <$> dsAssume d
  Directive ann d -> Directive ann <$> dsDirective d
  Import ann d -> Import ann <$> dsImport d
  Section ann d -> Section ann <$> dsSection d

dsImport :: Import Name -> Desugar (Import Name)
dsImport imp = pure imp

dsDirective :: Directive Name -> Desugar (Directive Name)
dsDirective = \ case
  StrictEval ann expr ->
    StrictEval ann <$> dsExpr expr
  LazyEval ann expr ->
    LazyEval ann <$> dsExpr expr
  Check ann expr ->
    Check ann <$> dsExpr expr
  Contract ann party msg events ->
    Contract ann
      <$> dsExpr party
      <*> dsExpr msg
      <*> traverse dsExpr events

dsAssume :: Assume Name -> Desugar (Assume Name)
dsAssume = \ case
  MkAssume ann tySig appForm mty ->
    pure $ MkAssume ann tySig appForm mty

dsDecide :: Decide Name -> Desugar (Decide Name)
dsDecide (MkDecide ann tySig appForm expr) =
  MkDecide ann tySig appForm
    <$> dsExpr expr

dsDeclare :: Declare Name -> Desugar (Declare Name)
dsDeclare (MkDeclare ann tySig appform tyDecl) =
  pure (MkDeclare ann tySig appform tyDecl)

dsExpr :: Expr Name -> Desugar (Expr Name)
dsExpr expr = case expr of
  And ann e1 e2 -> do
    e1' <- dsExpr e1
    e2' <- dsExpr e2
    pure $ And ann e1' e2'
  Or ann e1 e2 -> do
    e1' <- dsExpr e1
    e2' <- dsExpr e2
    pure $ Or ann e1' e2'
  RAnd ann e1 e2 -> do
    e1' <- dsExpr e1
    e2' <- dsExpr e2
    pure $ RAnd ann e1' e2'
  ROr ann e1 e2 -> do
    e1' <- dsExpr e1
    e2' <- dsExpr e2
    pure $ ROr ann e1' e2'
  Implies ann e1 e2 -> do
    e1' <- dsExpr e1
    e2' <- dsExpr e2
    pure $ Implies ann e1' e2'
  Equals ann e1 e2 -> do
    e1' <- dsExpr e1
    e2' <- dsExpr e2
    pure $ Equals ann e1' e2'
  Not ann e -> do
    e' <- dsExpr e
    pure $ Not ann e'
  Plus ann e1 e2 -> do
    args <- rewriteBinOpAnno expr e1 e2
    dsExpr $ App (annoNoFunName ann args) (MkName emptyAnno $ rawName TypeCheck.plusName) args
  Minus ann e1 e2 -> do
    args <- rewriteBinOpAnno expr e1 e2
    dsExpr $ App (annoNoFunName ann args) (MkName emptyAnno $ rawName TypeCheck.minusName) args
  Times ann e1 e2 -> do
    args <- rewriteBinOpAnno expr e1 e2
    dsExpr $ App (annoNoFunName ann args) (MkName emptyAnno $ rawName TypeCheck.timesName) args
  DividedBy ann e1 e2 -> do
    args <- rewriteBinOpAnno expr e1 e2
    dsExpr $ App (annoNoFunName ann args) (MkName emptyAnno $ rawName TypeCheck.divideName) args
  Modulo ann e1 e2 -> do
    args <- rewriteBinOpAnno expr e1 e2
    dsExpr $ App (annoNoFunName ann args) (MkName emptyAnno $ rawName TypeCheck.moduloName) args
  Cons ann e1 e2 -> do
    e1' <- dsExpr e1
    e2' <- dsExpr e2
    pure $ Cons ann e1' e2'
  Leq ann e1 e2 -> do
    args <- rewriteBinOpAnno expr e1 e2
    dsExpr $ App (annoNoFunName ann args) (MkName emptyAnno $ rawName TypeCheck.leqName) args
  Lt ann e1 e2 -> do
    args <- rewriteBinOpAnno expr e1 e2
    dsExpr $ App (annoNoFunName ann args) (MkName emptyAnno $ rawName TypeCheck.ltName) args
  Gt ann e1 e2 -> do
    args <- rewriteBinOpAnno expr e1 e2
    dsExpr $ App (annoNoFunName ann args) (MkName emptyAnno $ rawName TypeCheck.gtName) args
  Geq ann e1 e2 -> do
    args <- rewriteBinOpAnno expr e1 e2
    dsExpr $ App (annoNoFunName ann args) (MkName emptyAnno $ rawName TypeCheck.geqName) args
  Proj ann e1 n -> do
    e1' <- dsExpr e1
    pure $ Proj ann e1' n
  Var ann v -> do
    pure $ Var ann v
  Lam ann sig body -> do
    body' <- dsExpr body
    pure $ Lam ann sig body'
  App ann n ns -> do
    ns' <- traverse dsExpr ns
    pure $ App ann n ns'
  AppNamed ann n ns order -> do
    pure $ AppNamed ann n ns order
  IfThenElse ann b e1 e2 -> do
    b' <- dsExpr b
    e1' <- dsExpr e1
    e2' <- dsExpr e2
    pure $ IfThenElse ann b' e1' e2'
  Regulative ann r -> do
    r' <- dsObligation r
    pure $ Regulative ann r'
  Consider ann e branches -> do
    e' <- dsExpr e
    branches' <- traverse dsBranch branches
    pure $ Consider ann e' branches'
  e@Lit{} -> do
    pure e
  List ann es -> do
    es' <- traverse dsExpr es
    pure $ List ann es'
  Where ann e lcl -> do
    e' <- dsExpr e
    lcl' <- traverse dsLocalDecl lcl
    pure $ Where ann e' lcl'
  Event ann e -> Event ann <$> dsEvent e
 where
  annoNoFunName a as =
    fixAnnoSrcRange
      Anno
        { extra = a.extra
        , range = a.range
        , payload = [mkHoleWithSrcRangeHint Nothing, mkHoleWithSrcRange as]
        }

dsEvent :: Event Name -> Desugar (Event Name)
dsEvent = \ case
  MkEvent ann party action timestamp ->
    MkEvent ann
      <$> dsExpr party
      <*> dsExpr action
      <*> dsExpr timestamp

dsObligation :: Obligation Name -> Desugar (Obligation Name)
dsObligation = \ case
  MkObligation ann party action due hence lest ->
    MkObligation ann
      <$> dsExpr party
      <*> dsExpr action
      <*> traverse dsExpr due
      <*> traverse dsExpr hence
      <*> traverse dsExpr lest

dsLocalDecl :: LocalDecl Name -> Desugar (LocalDecl Name)
dsLocalDecl = \ case
  LocalDecide ann decide -> do
    d <- dsDecide decide
    pure $ LocalDecide ann d
  LocalAssume ann assume -> do
    pure $ LocalAssume ann assume

dsBranch :: Branch Name -> Desugar (Branch Name)
dsBranch = \ case
  When ann pat e -> do
    de <- dsExpr e
    pure $ When ann pat de
  Otherwise ann e -> do
    de <- dsExpr e
    pure $ Otherwise ann de

-- | Rewrite the 'Anno' of the given arguments @'NonEmpty' ('Expr' 'Name)'@ to
-- include the concrete syntax nodes of the 'Anno' in the @'Expr' 'Name'@.
--
-- Let's assume an 'Expr' for the code:
--
-- @
--   1 PLUS 2
-- @
--
-- We want to desugar this to use a prefix function:
--
-- @
--   \_\_PLUS\_\_ 1 2
-- @
--
-- To make exactprinting still faithful to the original sources, we need
-- to be very careful that it is printed the same way. First, let's look at the
-- 'Anno' for the @1 PLUS 2@ expression:
-- (a @_@ marks an 'AnnoHole'):
--
-- @
--   _ PLUS _
-- @
--
-- The 'Anno' of a function looks like '_ [OF] _', so two holes, one for the name of the function
-- and one for *all* arguments of the function. The first hole is easy to manage, just make
-- sure the 'Name' of the function has an 'emptyAnno', then it is basically skipped over.
-- For the second hole to accurately reproduce the same concrete syntax nodes, we need to be more
-- careful.
--
-- We need to massage now the concrete syntax nodes of '_ PLUS _' into the arguments
-- of the prefix function notation.
-- For each 'AnnoHole' in the original 'Anno', we get all the 'AnnoCsn' that come
-- afterwards and attach them to the 'Anno' of the argument.
-- Note, for the first element, we need to do the same thing for leading 'AnnoCsn' elements.
-- The finalised 'Anno' of each argument should then look like:
--
-- @
--   [[_, " PLUS "], [_]]
-- @
--
-- where the inner holes are then filled by the underlying @'Expr' 'Name'@.
--
-- This works also nicely for parenthesis, the expression @(1 PLUS 2)@
-- is then translated to:
--
-- @
--   [["(", _, " PLUS "], [_, ")"]]
-- @
rewriteBinOpAnno :: Expr Name -> Expr Name -> Expr Name -> Desugar [Expr Name]
rewriteBinOpAnno expr e1 e2 = do
  let

  case csnSlices of
    [beforeFirst, beforeSecond, after] ->
      pure
        [ updateAnnoOf e1 (surroundWithCsn beforeFirst beforeSecond)
        , updateAnnoOf e2 (surroundWithCsn [] after)
        ]
    _slices -> internalError $ InternalAnnoRewritingError expr (HoleInfo 2 numberOfAnnoHoles)
 where
  annoPieces = (getAnno expr).payload

  updateAnnoOf e f = e & annoOf %~ f

  csnSlices = splitWhen (isJust . preview #_AnnoHole) annoPieces
  numberOfAnnoHoles = length $ filter (isJust . preview #_AnnoHole) annoPieces

  surroundWithCsn :: [AnnoElement] -> [AnnoElement] -> Anno -> Anno
  surroundWithCsn before after a =
    fixAnnoSrcRange
      Anno
        { extra = a.extra
        , range = a.range
        , payload = before <> a.payload <> after
        }

prettyDesugarError :: DesugarError -> Text
prettyDesugarError = \ case
  InternalAnnoRewritingError context errorInfo ->
    Text.unlines
      [ "Internal Error:"
      , "While trying to desugar the expression"
      , ""
      , "```"
      , prettyLayout context
      , "```"
      , ""
      , "We ran into the error:"
      , "The source annotation are not matching the expected number of arguments."
      , "Expected " <> Text.show (errorInfo.expected) <> " holes but got: " <> Text.show (errorInfo.got)
      ]
