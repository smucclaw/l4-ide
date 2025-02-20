{-# LANGUAGE ViewPatterns #-}
module LSP.L4.Viz.Ladder where

import Control.DeepSeq
import Control.Monad.Except
import Control.Monad.Identity (Identity (Identity))
import Control.Monad.State (MonadState, StateT (StateT))
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)
import Optics.State.Operators ((<%=))

import L4.Syntax (OptionallyTypedName (..), AppForm (..), Decide (..), Expr (..),
  GivenSig (..), Name (..), TypeSig (..), Resolved, getOriginal, Unique (..), getUnique)
import L4.TypeCheck (rawNameToText, simpleprint)
import LSP.L4.Viz.VizExpr
  ( ID (..), IRExpr,
    VisualizeDecisionLogicIRInfo (..),
  )
import qualified LSP.L4.Viz.VizExpr as V
import L4.Print (prettyLayout)
import qualified L4.Transform as Transform (simplify)

------------------------------------------------------
-- Monad
------------------------------------------------------

-- TODO: Would be better not to stop at the first error
newtype Viz a = MkViz {getVizE :: VizState -> (Either VizError a, VizState)}
  deriving
    (Functor, Applicative, Monad, MonadState VizState, MonadError VizError)
    via ExceptT VizError (StateT VizState Identity)

newtype VizState = MkVizState {maxId :: ID}
  deriving stock (Show, Generic)
  deriving newtype (Eq, Ord)

getFresh :: Viz ID
getFresh = do
    #maxId <%= \(MkID n) -> MkID (n + 1)

initialVizState :: VizState
initialVizState = MkVizState { maxId = MkID 0 }

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
doVisualize :: Decide Resolved -> Bool -> Either VizError VisualizeDecisionLogicIRInfo
doVisualize decide simplify =
  case  (vizProgram simplify decide).getVizE initialVizState of
    (result, _) -> result

vizProgram :: Bool -> Decide Resolved -> Viz VisualizeDecisionLogicIRInfo
vizProgram simplify = fmap MkVisualizeDecisionLogicIRInfo . translateDecide simplify

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
translateDecide :: Bool -> Decide Resolved -> Viz V.IRDecl
translateDecide simplify (MkDecide _ (MkTypeSig _ givenSig _) (MkAppForm _ funResolved _) body) =
  do
    uid <- getFresh
    vizBody <- translateExpr simplify body
    pure $ V.MkFunDecl
      uid
      -- didn't want a backtick'd name in the header
      (mkSimpleVizName funResolved)
      (paramNamesFromGivens givenSig)
      vizBody
      where
        paramNamesFromGivens :: GivenSig Resolved -> [V.Name]
        paramNamesFromGivens (MkGivenSig _ optionallyTypedNames) =
          mkSimpleVizName . getName <$> optionallyTypedNames

        getName (MkOptionallyTypedName _ paramName _) = paramName

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

defaultBoolVarValue :: V.BoolValue
defaultBoolVarValue = V.UnknownV

leafFromVizName :: V.Name -> Viz IRExpr
leafFromVizName vname = do
  uid <- getFresh
  pure $ V.BoolVar uid vname defaultBoolVarValue

leaf :: Text -> Text -> Viz IRExpr
leaf subject complement = do
  uid <- getFresh
  let tempUniqueTODO = negate uid.id
  -- The negate is just my very-lazy-person's way of finding some kind of unique for this
  -- that isn't likely to collide with other Uniques,
  -- because I'd like to defer properly handling `leaf` and the kinds of cases it's used for.
  -- I'll return to this when we explicitly handle more cases in translateExpr
  -- (I'm currently focusing on state in the frontend in the simpler case of App with no args)
  pure $ V.BoolVar uid (V.MkName tempUniqueTODO $ subject <> " " <> complement) defaultBoolVarValue

------------------------------------------------------
-- Name helpers
------------------------------------------------------

-- | Used for places where I don't want backticks appearing.
mkSimpleVizName :: Resolved -> V.Name
mkSimpleVizName = mkVizNameWith simpleprint

mkVizNameWith :: (Resolved -> Text) -> Resolved -> V.Name
mkVizNameWith printer resolved =
  case getUnique resolved of
    MkUnique _ uniq -> V.MkName uniq (printer resolved)

nameToText :: Name -> Text
nameToText (MkName _ rawName) = rawNameToText rawName
