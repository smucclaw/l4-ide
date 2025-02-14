{-# LANGUAGE ViewPatterns #-}
module LSP.L4.Viz.Ladder where

import Control.DeepSeq
import Control.Monad.Except
import Control.Monad.Identity (Identity (Identity))
import Control.Monad.State (MonadState, StateT (StateT))
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)
import qualified Optics
import Optics ((%), (^..))
import Optics.State.Operators ((<%=))

import L4.Syntax (AppForm (..), Decide (..), Expr (..), GivenSig (..), Name (..), OptionallyTypedName (..), TypeSig (..), Resolved, getOriginal)
import L4.TypeCheck (rawNameToText)
import LSP.L4.Viz.VizExpr
  ( ID (..), IRExpr,
    VisualizeDecisionLogicIRInfo (..),
  )
import qualified LSP.L4.Viz.VizExpr as V

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
  InvalidProgramNoDecidesFound -> "The program isn't the right sort for visualization: there are no DECIDE rules that can be visualized."
  InvalidProgramDecidesMustNotHaveMoreThanOneGiven -> "Visualization failed: DECIDE rules must reference no more than one GIVEN variable."
  Unimplemented -> "Unimplemented"

------------------------------------------------------
-- Entrypoint: Visualise
------------------------------------------------------

-- | Entrypoint: Generate boolean circuits of the given 'Decide'.
doVisualize :: Decide Resolved -> Either VizError VisualizeDecisionLogicIRInfo
doVisualize decide =
  case  (vizProgram decide).getVizE initialVizState of
    (result, _) -> result

vizProgram :: Decide Resolved -> Viz VisualizeDecisionLogicIRInfo
vizProgram = fmap MkVisualizeDecisionLogicIRInfo . translateDecide

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
translateDecide :: Decide Resolved -> Viz V.IRExpr
translateDecide (MkDecide _ (MkTypeSig _ givenSig _retSig) (MkAppForm _ (getOriginal -> MkName _ fnName) _args) body) =
  case givenSig of
    MkGivenSig _ [MkOptionallyTypedName _ (getOriginal -> MkName _ subject) _] ->
      translateExpr (rawNameToText subject) body
    MkGivenSig _ [] ->
      translateExpr (rawNameToText fnName) body
    -- DECIDEs with more than one GIVEN not currently supported
    MkGivenSig _ _xs -> throwError InvalidProgramDecidesMustNotHaveMoreThanOneGiven

translateExpr :: Text -> Expr Resolved -> Viz IRExpr
translateExpr subject e = case e of
{- TODO: Need to look more into whether we really want to be passing `subject` down.
-}
  Not _ negand -> do
    uid <- getFresh
    V.Not uid <$> translateExpr subject negand
  And {} -> do
    uid <- getFresh
    V.And uid <$> traverse (translateExpr subject) (scanAnd e)
  Or {} -> do
    uid <- getFresh
    V.Or uid <$> traverse (translateExpr subject) (scanOr e)
  ---- unimplemented --------------------------------
  Equals {} -> throwError Unimplemented -- Can't handle 'Is' yet

  -- A 'Var' can apparently be parsed as an App with no arguments ----------------
  Var _ (getOriginal -> MkName _ verb) ->
    leaf "" (rawNameToText verb)
  App _ (getOriginal -> MkName _ leafName) [] ->
    leaf "" (rawNameToText leafName)
  --------------------------------------------------------------------------------

  -- TODO: Will be replacing this temporary, hacky version with variants for Lam and App on the frontend
  App _ (getOriginal -> MkName _ fnName) args ->
    leaf subject $ rawNameToText fnName <> Text.unwords (getNames args)
  _ -> throwError Unimplemented
  where
    getNames args = args ^.. (Optics.gplate @Name) % Optics.to nameToText

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

leaf :: Text -> Text -> Viz IRExpr
leaf subject complement = do
  uid <- getFresh
  pure $ V.BoolVar uid (subject <> " " <> complement) defaultBoolVarValue

------------------------------------------------------
-- Name helpers
------------------------------------------------------

nameToText :: Name -> Text
nameToText (MkName _ rawName) = rawNameToText rawName
