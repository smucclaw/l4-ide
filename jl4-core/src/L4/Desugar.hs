module L4.Desugar (
  -- * Caramelize Expressions
  --
  carameliseExpr,
  carameliseNode,
  ) where


import           Base
import qualified Data.Map.Strict          as Map
import           L4.Annotation            (HasAnno (..), emptyAnno)
import           L4.Names
import           L4.Syntax
import qualified L4.TypeCheck.Environment as TypeCheck
import Control.Category ((>>>))

-- ----------------------------------------------------------------------------
-- Caramelize
-- ----------------------------------------------------------------------------

carameliseExpr :: HasName n => Expr n -> Expr n
carameliseExpr = carameliseExprWithContext InertCtxNone

-- | Caramelize expression with context tracking for inert elements.
-- Inert elements evaluate to the identity for their containing operator:
-- - In AND context: True (AND identity)
-- - In OR context: False (OR identity)
carameliseExprWithContext :: HasName n => InertContext -> Expr n -> Expr n
carameliseExprWithContext ctx = carameliseNode >>> \ case
  Not        ann e -> Not ann (carameliseExprWithContext InertCtxNone e)
  -- For AND/OR, we propagate the context to children
  And        ann e1 e2 -> And       ann (carameliseExprWithContext InertCtxAnd e1) (carameliseExprWithContext InertCtxAnd e2)
  Or         ann e1 e2 -> Or        ann (carameliseExprWithContext InertCtxOr e1) (carameliseExprWithContext InertCtxOr e2)
  RAnd       ann e1 e2 -> RAnd      ann (carameliseExprWithContext InertCtxAnd e1) (carameliseExprWithContext InertCtxAnd e2)
  ROr        ann e1 e2 -> ROr       ann (carameliseExprWithContext InertCtxOr e1) (carameliseExprWithContext InertCtxOr e2)
  Implies    ann e1 e2 -> Implies   ann (carameliseExprWithContext InertCtxNone e1) (carameliseExprWithContext InertCtxNone e2)
  Equals     ann e1 e2 -> Equals    ann (carameliseExprWithContext InertCtxNone e1) (carameliseExprWithContext InertCtxNone e2)
  Plus       ann e1 e2 -> Plus      ann (carameliseExprWithContext InertCtxNone e1) (carameliseExprWithContext InertCtxNone e2)
  Minus      ann e1 e2 -> Minus     ann (carameliseExprWithContext InertCtxNone e1) (carameliseExprWithContext InertCtxNone e2)
  Times      ann e1 e2 -> Times     ann (carameliseExprWithContext InertCtxNone e1) (carameliseExprWithContext InertCtxNone e2)
  DividedBy  ann e1 e2 -> DividedBy ann (carameliseExprWithContext InertCtxNone e1) (carameliseExprWithContext InertCtxNone e2)
  Modulo     ann e1 e2 -> Modulo    ann (carameliseExprWithContext InertCtxNone e1) (carameliseExprWithContext InertCtxNone e2)
  Exponent   ann e1 e2 -> Exponent  ann (carameliseExprWithContext InertCtxNone e1) (carameliseExprWithContext InertCtxNone e2)
  Cons       ann e1 e2 -> Cons      ann (carameliseExprWithContext InertCtxNone e1) (carameliseExprWithContext InertCtxNone e2)
  Leq        ann e1 e2 -> Leq       ann (carameliseExprWithContext InertCtxNone e1) (carameliseExprWithContext InertCtxNone e2)
  Geq        ann e1 e2 -> Geq       ann (carameliseExprWithContext InertCtxNone e1) (carameliseExprWithContext InertCtxNone e2)
  Lt         ann e1 e2 -> Lt        ann (carameliseExprWithContext InertCtxNone e1) (carameliseExprWithContext InertCtxNone e2)
  Gt         ann e1 e2 -> Gt        ann (carameliseExprWithContext InertCtxNone e1) (carameliseExprWithContext InertCtxNone e2)
  Proj       ann e n   -> Proj ann (carameliseExprWithContext InertCtxNone e) n
  Var        ann n     -> Var  ann n
  Lam        ann sig e -> Lam ann sig (carameliseExprWithContext InertCtxNone e)
  App        ann n es  -> App ann n (fmap (carameliseExprWithContext InertCtxNone) es)
  AppNamed   ann n nes morder -> AppNamed ann n (fmap caramliseNamedExpr nes) morder
  IfThenElse ann b t e -> IfThenElse ann (carameliseExprWithContext InertCtxNone b) (carameliseExprWithContext InertCtxNone t) (carameliseExprWithContext InertCtxNone e)
  MultiWayIf ann es e -> MultiWayIf ann (map (\(MkGuardedExpr ann' a b) -> MkGuardedExpr ann' (carameliseExprWithContext InertCtxNone a) (carameliseExprWithContext InertCtxNone b)) es) (carameliseExprWithContext InertCtxNone e)
  Regulative ann o -> Regulative ann (carameliseDeonton o)
  Consider   ann e branches -> Consider ann (carameliseExprWithContext InertCtxNone e) (fmap carameliseBranch branches)
  Lit        ann l -> Lit ann l
  Percent    ann e -> Percent ann (carameliseExprWithContext InertCtxNone e)
  List       ann es -> List ann (fmap (carameliseExprWithContext InertCtxNone) es)
  Where      ann e ds -> Where ann (carameliseExprWithContext ctx e) (fmap carameliseLocalDecl ds)
  LetIn      ann ds e -> LetIn ann (fmap carameliseLocalDecl ds) (carameliseExprWithContext ctx e)
  Event      ann ev -> Event ann (carameliseEvent ev)
  Fetch      ann e -> Fetch ann (carameliseExprWithContext InertCtxNone e)
  Env        ann e -> Env ann (carameliseExprWithContext InertCtxNone e)
  Post       ann e1 e2 e3 -> Post ann (carameliseExprWithContext InertCtxNone e1) (carameliseExprWithContext InertCtxNone e2) (carameliseExprWithContext InertCtxNone e3)
  Concat     ann es -> Concat ann (fmap (carameliseExprWithContext InertCtxNone) es)
  AsString   ann e -> AsString ann (carameliseExprWithContext InertCtxNone e)
  Breach     ann mParty mReason -> Breach ann (fmap (carameliseExprWithContext InertCtxNone) mParty) (fmap (carameliseExprWithContext InertCtxNone) mReason)
  -- Inert elements: update the context based on the desugaring context.
  -- The evaluator (Machine.hs) will read this context to determine the value.
  Inert      ann txt _oldCtx -> Inert ann txt ctx

carameliseLocalDecl :: HasName n => LocalDecl n -> LocalDecl n
carameliseLocalDecl = \ case
  LocalDecide ann decide -> LocalDecide ann (carameliseDecide decide)
  LocalAssume ann assume -> LocalAssume ann assume

carameliseDecide :: HasName n => Decide n -> Decide n
carameliseDecide = \ case
  MkDecide ann tySig appForm expr ->
    MkDecide ann tySig appForm (carameliseExpr expr)

carameliseBranch :: HasName n => Branch n -> Branch n
carameliseBranch = \ case
  MkBranch ann (When ann' pat) e -> MkBranch ann (When ann' (caramelisePattern pat)) (carameliseExpr e)
  MkBranch ann (Otherwise ann') e -> MkBranch ann (Otherwise ann') (carameliseExpr e)

caramelisePattern :: HasName n => Pattern n -> Pattern n
caramelisePattern = \ case
  PatVar ann n -> PatVar ann n
  PatApp ann n ps -> PatApp ann n (fmap caramelisePattern ps)
  PatCons ann p1 p2 -> PatCons ann (caramelisePattern p1) (caramelisePattern p2)
  PatExpr ann e -> PatExpr ann (carameliseExpr e)
  PatLit ann l -> PatLit ann l

carameliseEvent :: HasName n => Event n -> Event n
carameliseEvent = \ case
  MkEvent { anno, party, action, timestamp, atFirst} ->
    MkEvent
      { anno
      , party = carameliseExpr party
      , action = carameliseExpr action
      , timestamp = carameliseExpr timestamp
      , atFirst
      }

carameliseDeonton :: HasName n => Deonton n -> Deonton n
carameliseDeonton = \ case
  MkDeonton { anno, party, action, due, hence, lest} ->
    MkDeonton
      { anno
      , party = carameliseExpr party
      , action = carameliseRAction action
      , due = fmap carameliseExpr due
      , hence = fmap carameliseExpr hence
      , lest = fmap carameliseExpr lest
      }

carameliseRAction :: HasName n => RAction n -> RAction n
carameliseRAction = \ case
  MkAction { anno, modal, action, provided } ->
    MkAction
      { anno
      , modal
      , action
      , provided = fmap carameliseExpr provided
      }

caramliseNamedExpr :: HasName n => NamedExpr n -> NamedExpr n
caramliseNamedExpr = \ case
  MkNamedExpr ann n e -> MkNamedExpr ann n (carameliseExpr e)

-- | We desugar expressions during typechecking, for example, @2 PLUS 3@ is
-- turned into the function @\_\_PLUS\_\_ 2 3@. This is less readable during natural
-- language generation, so we undo some of our desugaring and add the syntactic sugar
-- back.
--
-- We call this process "caramelise". Primarily, because fendor likes caramel,
-- and it feels like a good name for the opposite of "desugaring".
carameliseNode :: HasName n => Expr n -> Expr n
carameliseNode = \ case
  App ann n es
    | Just caramelise <- isBuiltinBinary (rawName $ getName n)
    , [e1, e2] <- es ->
        setAnno ann $ caramelise e1 e2
    | Just caramelise <- isBuiltinUnary (rawName $ getName n)
    , [e1] <- es ->
        setAnno ann $ caramelise e1
  expr -> expr

-- ----------------------------------------------------------------------------
-- Builtins
-- ----------------------------------------------------------------------------

-- | Operations such as Plus and Minus are desugared to prefix function
-- notation.
-- However, we don't want to show the prefix function name
-- but rather the infix version. So, we translate the prefix function
-- notation back to the infix one. In a way, we are undoing the
-- desugaring again.
isBuiltinBinary :: RawName -> Maybe (Expr n -> Expr n -> Expr n)
isBuiltinBinary r =
  Map.lookup (rawNameToText r) builtinBinFunctions

isBuiltinUnary :: RawName -> Maybe (Expr n -> Expr n)
isBuiltinUnary r =
  Map.lookup (rawNameToText r) builtinUnaryFunctions

builtinBinFunctions :: Map Text (Expr n -> Expr n -> Expr n)
builtinBinFunctions = Map.fromList
  [ (rawNameToText $ rawName TypeCheck.plusName, Plus emptyAnno)
  , (rawNameToText $ rawName TypeCheck.minusName, Minus emptyAnno)
  , (rawNameToText $ rawName TypeCheck.timesName, Times emptyAnno)
  , (rawNameToText $ rawName TypeCheck.divideName, DividedBy emptyAnno)
  , (rawNameToText $ rawName TypeCheck.moduloName, Modulo emptyAnno)
  , (rawNameToText $ rawName TypeCheck.ltName, Lt emptyAnno)
  , (rawNameToText $ rawName TypeCheck.leqName, Leq emptyAnno)
  , (rawNameToText $ rawName TypeCheck.gtName, Gt emptyAnno)
  , (rawNameToText $ rawName TypeCheck.geqName, Geq emptyAnno)
  , (rawNameToText $ rawName TypeCheck.andName, And emptyAnno)
  , (rawNameToText $ rawName TypeCheck.orName, Or emptyAnno)
  , (rawNameToText $ rawName TypeCheck.impliesName, Implies emptyAnno)
  , (rawNameToText $ rawName TypeCheck.consName, Cons emptyAnno)
  , (rawNameToText $ rawName TypeCheck.equalsName, Equals emptyAnno)
  ]

builtinUnaryFunctions :: Map Text (Expr n -> Expr n)
builtinUnaryFunctions = Map.fromList
  [ (rawNameToText $ rawName TypeCheck.notName, Not emptyAnno)
  ]
