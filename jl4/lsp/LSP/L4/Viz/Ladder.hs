{-# LANGUAGE DataKinds #-}

module LSP.L4.Viz.Ladder where

-- import           L4.Annotation      ()
import Control.DeepSeq
import Control.Monad ()
import Control.Monad.Except
import Control.Monad.Identity (Identity (Identity))
import Control.Monad.State (MonadState, StateT (StateT))
import Data.List.NonEmpty (toList)
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)
-- import           L4.Lexer             (PosToken, SrcRange)

------- Imports for testing -------------
import L4.Parser
import L4.Syntax (AppForm (..), Decide (..), Expr (..), GivenSig (..), Name (..), OptionallyTypedName (..), Program, TypeSig (..))
import qualified L4.Syntax as S ()
import L4.TypeCheck (rawNameToText)
import LSP.L4.Viz.VizExpr
  ( ID (..), IRExpr,
    VisualizeDecisionLogicIRInfo (..),
  )
import qualified LSP.L4.Viz.VizExpr as V
import Optics
import Optics.State.Operators ((<%=))
import Text.Pretty.Simple

-----------------------------------------

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

-- TODO in next version
-- data VizErrorWithContext =
--   MkVizErrorWithContext
--     { error   :: !VizError
--     , context :: !SrcRange
--     }
--   deriving stock (Eq, Generic, Show)

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

-- | Entrypoint: Generate boolean circuits of the 'Program'.
--
-- Simple version where we visualize the first Decide, if it exists.
doVisualize :: Program Name -> Either VizError VisualizeDecisionLogicIRInfo
-- doVisualize prog = (vizProgram prog).getVizE
doVisualize prog = 
  case  (vizProgram prog).getVizE initialVizState of
    (result, _) -> result

vizProgram :: Program Name -> Viz VisualizeDecisionLogicIRInfo
vizProgram prog =
  let decides = toListOf (gplate @(Decide Name)) prog
   in case decides of
        [x] -> MkVisualizeDecisionLogicIRInfo <$> translateDecide x
        [] -> throwError InvalidProgramNoDecidesFound
        _xs -> throwError Unimplemented -- TODO: Want to eventually translate every eligible Decide

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
translateDecide :: Decide Name -> Viz V.IRExpr
translateDecide (MkDecide _ (MkTypeSig _ givenSig _retSig) (MkAppForm _ (MkName _ fnName) _args) body) =
  case givenSig of
    MkGivenSig _ [MkOptionallyTypedName _ (MkName _ subject) _] ->
      translateExpr (rawNameToText subject) body
    MkGivenSig _ [] ->
      translateExpr (rawNameToText fnName) body
    -- DECIDEs with more than one GIVEN not currently supported
    MkGivenSig _ _xs -> throwError InvalidProgramDecidesMustNotHaveMoreThanOneGiven

translateExpr :: Text -> Expr Name -> Viz IRExpr
translateExpr subject e = case e of
  And {} -> do
    uid <- getFresh
    V.And uid <$> traverse (translateExpr subject) (scanAnd e)
  Or {} -> do
    uid <- getFresh
    V.Or uid <$> traverse (translateExpr subject) (scanOr e)
  ---- unimplemented --------------------------------
  Equals {} -> throwError Unimplemented -- Can't handle 'Is' yet
  Not {} -> throwError Unimplemented -- This will be done in the next or next next PR, at the same time that we add a visualization for this on the frontend

  -- A 'Var' can apparently be parsed as an App with no arguments ----------------
  Var _ (MkName _ verb) ->
    leaf "" (rawNameToText verb)
  App _ (MkName _ leafName) [] ->
    leaf "" (rawNameToText leafName)
  --------------------------------------------------------------------------------

  -- TODO: Will be replacing this temporary, hacky version with variants for Lam App on the frontend
  App _ (MkName _ fnName) args ->
    leaf subject $ rawNameToText fnName <> Text.unwords (getNames args)
  _ -> throwError Unimplemented
  where
    getNames args = args ^.. (gplate @Name) % to nameToText
-- error $ "[fallthru]\n" <> show x (Keeping comment around because useful for printf-style debugging)

scanAnd :: Expr Name -> [Expr Name]
scanAnd (And _ e1 e2) =
  scanAnd e1 <> scanAnd e2
scanAnd e = [e]

scanOr :: Expr Name -> [Expr Name]
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

------------------------------------------------------
-- Dev utils
------------------------------------------------------

parseAndVisualiseProgram :: Text -> Either VizError VisualizeDecisionLogicIRInfo
parseAndVisualiseProgram prog =
  case execParser program "" prog of
    Left errs -> error $ unlines $ fmap (Text.unpack . (.message)) (toList errs)
    Right x -> doVisualize x

vizTest :: Text -> IO ()
vizTest = pPrint . parseAndVisualiseProgram
