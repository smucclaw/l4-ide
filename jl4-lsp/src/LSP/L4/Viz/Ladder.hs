{-# LANGUAGE ViewPatterns #-}
module LSP.L4.Viz.Ladder where

import Control.DeepSeq
import Control.Monad.Except
import Base
import qualified Base.Text as Text
import Optics.State.Operators ((<%=))

import qualified L4.TypeCheck as TC
-- import qualified L4.TypeCheck.Environment as TC (boolean)
import L4.Syntax
import LSP.L4.Viz.VizExpr
  ( ID (..), IRExpr,
    RenderAsLadderInfo (..),
  )
import qualified LSP.L4.Viz.VizExpr as V
import L4.Print (prettyLayout)
import qualified L4.Transform as Transform (simplify)

-- import Debug.Trace

------------------------------------------------------
-- Monad
------------------------------------------------------

newtype Viz a = MkViz {getVizE :: VizState -> (Either VizError a, VizState)}
  deriving
    (Functor, Applicative, Monad, MonadState VizState, MonadError VizError)
    via ExceptT VizError (State VizState)

data VizEnv = MkVizEnv
  { moduleUri :: !NormalizedUri
  , substitution :: TC.Substitution
  -- | Whether to simplify the expression
  , shouldSimplify :: Bool
  }
  deriving stock (Show, Generic, Eq)

data VizState =
  MkVizState
    { env :: !VizEnv
    , maxId :: !ID
    }
  deriving stock (Show, Generic, Eq)

getVizEnv :: Viz VizEnv
getVizEnv = use #env

getFresh :: Viz ID
getFresh = do
  #maxId <%= \(MkID n) -> MkID (n + 1)

mkInitialVizState :: VizEnv -> VizState
mkInitialVizState env =
  MkVizState
    { env = env
    , maxId = MkID 0
    }

------------------------------------------------------
-- VizError
------------------------------------------------------

data VizError
  = InvalidProgramNoDecidesFound
  | InvalidProgramDecidesMustNotHaveMoreThanOneGiven
  | Unimplemented
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

-- TODO: Incorporate context like the specific erroring rule and the src range too
prettyPrintVizError :: VizError -> Text
prettyPrintVizError = \case
  InvalidProgramNoDecidesFound ->
    "The program isn't the right sort for visualization: there are no DECIDE rules that can be visualized."
  InvalidProgramDecidesMustNotHaveMoreThanOneGiven ->
    "Visualization failed: DECIDE rules must reference no more than one GIVEN variable."
  Unimplemented -> "Unimplemented"

------------------------------------------------------
-- Entrypoint: Visualise
------------------------------------------------------

-- | Entrypoint: Generate boolean circuits of the given 'Decide'.
doVisualize :: Decide Resolved -> VizEnv -> Either VizError RenderAsLadderInfo
doVisualize decide env =
  case  (vizProgram decide).getVizE (mkInitialVizState env) of
    (result, _) -> result

vizProgram :: Decide Resolved -> Viz RenderAsLadderInfo
vizProgram = fmap MkRenderAsLadderInfo . translateDecide

------------------------------------------------------
-- translateDecide, translateExpr
------------------------------------------------------

-- | Turn a single clause into a boolean circuit.
-- We support only a tiny subset of possible representations, in particular
-- only statements of the form:
--
-- @
--  GIVEN <variable>
--  DECIDE <variable> IF <boolean expression>
-- @
--
-- Moreover, only 'AND', 'OR' and variables are allowed in the '<boolean expression>'.
--
-- These limitations are arbitrary, mostly to make sure we have something to show rather
-- than being complete. So, feel free to lift these limitations at your convenience :)
--
-- Simple implementation: Translate Decide iff <= 1 Given
translateDecide :: Decide Resolved -> Viz V.FunDecl
translateDecide (MkDecide _ (MkTypeSig _ givenSig _) (MkAppForm _ funResolved _ _) body) =
  -- traceShow ("decide AST" :: Text, clearAnno dec) $
  do
    vizEnv <- getVizEnv
    uid    <- getFresh
    vizBody <- translateExpr vizEnv.shouldSimplify body
    pure $ V.MkFunDecl
      uid
      -- didn't want a backtick'd name in the header
      (mkSimpleVizName funResolved)
      (paramNamesFromGivens givenSig)
      vizBody
      where
        paramNamesFromGivens :: GivenSig Resolved -> [V.Name]
        paramNamesFromGivens (MkGivenSig _ optionallyTypedNames) =
          mkSimpleVizName . getResolved <$> optionallyTypedNames

        -- TODO: I imagine there will be functionality for this kind of thing in a more central place soon;
        -- this can be replaced with that when that happens.
        getResolved :: OptionallyTypedName Resolved -> Resolved
        getResolved (MkOptionallyTypedName _ paramName _) = paramName

        mkSimpleVizName = mkVizNameWith prettyLayout

translateExpr :: Bool -> Expr Resolved -> Viz IRExpr
translateExpr True  =
  translateExpr False . Transform.simplify
translateExpr False = go
  where
    go e =
      case e of
        Not _ negand -> do
          uid <- getFresh
          V.Not uid <$> go negand
        And {} -> do
          uid <- getFresh
          V.And uid <$> traverse go (scanAnd e)
        Or {} -> do
          uid <- getFresh
          V.Or uid <$> traverse go (scanOr e)
        Where _ e' _ds -> go e' -- TODO: lossy

        -- TODO: Should handle BoolLits differently too
        -- 'var'
        App _ resolved [] ->
          leafFromVizName (mkVizNameWith prettyLayout resolved)
        -- TODO: Will be replacing this temporary version with a variant for App on the frontend
        App _ (getOriginal -> MkName _ fnName) args ->
          leaf "" $ Text.unwords (rawNameToText fnName : (prettyLayout <$> args))

        _ -> do
          leaf "" (prettyLayout e)

scanAnd :: Expr Resolved -> [Expr Resolved]
scanAnd (And _ e1 e2) =
  scanAnd e1 <> scanAnd e2
scanAnd e = [e]

scanOr :: Expr Resolved -> [Expr Resolved]
scanOr (Or _ e1 e2) =
  scanOr e1 <> scanOr e2
scanOr e = [e]

------------------------------------------------------
-- Leaf makers
------------------------------------------------------

defaultUBoolVarValue :: V.UBoolValue
defaultUBoolVarValue = V.UnknownV

leafFromVizName :: V.Name -> Viz IRExpr
leafFromVizName vname = do
  uid <- getFresh
  pure $ V.UBoolVar uid vname defaultUBoolVarValue

leaf :: Text -> Text -> Viz IRExpr
leaf subject complement = do
  uid <- getFresh
  tempUniqueTODO <- getFresh
  -- tempUniqueTODO: I'd like to defer properly handling the V.Name for `leaf` and the kinds of cases it's used for.
  -- I'll return to this when we explicitly/properly handle more cases in translateExpr
  -- (I'm currently focusing on state in the frontend in the simpler case of App with no args)
  pure $ V.UBoolVar uid (V.MkName tempUniqueTODO.id $ subject <> " " <> complement) defaultUBoolVarValue

------------------------------------------------------
-- Name helpers
------------------------------------------------------

-- It's not obvious to me that we want to be using
-- getOriginal (as getUniqueName does), instead of getActual,
-- but I'm going with this for now since it's what was used to translate the function name.
mkVizNameWith :: (Name -> Text) -> Resolved -> V.Name
mkVizNameWith printer (getUniqueName -> (uniq, name)) =
  case uniq of
    MkUnique {unique} -> V.MkName unique (printer name)
