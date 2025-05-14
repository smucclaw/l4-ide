{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant lambda" #-}

module LSP.L4.Viz.Ladder (
  -- * Entrypoint
  doVisualize,

  -- * VizConfig, VizState
  VizConfig (..),
  mkVizConfig,
  VizState (..),

  -- * Viz State helpers
  lookupEvalAppMaker,
  getVizConfig,
  
  -- * Other helpers
  prettyPrintVizError,
  ) where

import Control.DeepSeq
import Control.Monad.Except
import Base
import qualified Base.Map as Map
import qualified Base.Text as Text
import Optics.State.Operators ((<%=), (%=))

import qualified Language.LSP.Protocol.Types as LSP

import qualified L4.TypeCheck as TC
import L4.Annotation
import L4.Syntax
import L4.Print (prettyLayout)
import LSP.L4.Viz.VizExpr
  ( ID (..), IRExpr,
    RenderAsLadderInfo (..),
  )
import qualified LSP.L4.Viz.VizExpr as V
import qualified LSP.L4.Viz.CustomProtocol as V (EvalAppRequestParams (..))
import qualified L4.Transform as Transform (simplify)

{- | Temporary hack to disable the translation to IRExpr App
while the frontend doesn't have proper UI for it -}
isDevMode :: Bool
isDevMode = False

------------------------------------------------------
-- Monad
------------------------------------------------------

newtype Viz a = MkViz {getViz :: VizEnv -> VizState -> (Either VizError a, VizState)}
  deriving
    (Functor, Applicative, Monad, MonadState VizState, MonadError VizError, MonadReader VizEnv)
    via ReaderT VizEnv (ExceptT VizError (State VizState))

-- | A 'local' env
newtype VizEnv = MkVizEnv { localDecls :: [LocalDecl Resolved]}

mkVizConfig :: LSP.VersionedTextDocumentIdentifier -> TC.Substitution -> Bool -> VizConfig
mkVizConfig verTxtDocId substitution shouldSimplify =
  let moduleUri = toNormalizedUri verTxtDocId._uri
  in MkVizConfig
    { moduleUri
    , verTxtDocId
    , substitution
    , shouldSimplify
    }

data VizConfig = MkVizConfig
  { moduleUri      :: !NormalizedUri
  , verTxtDocId    :: !LSP.VersionedTextDocumentIdentifier
  , substitution   :: !TC.Substitution  -- might be more futureproof to use TypeCheckResult
  , shouldSimplify :: !Bool             -- ^ whether to simplify the expression
  }
  deriving stock (Show, Generic, Eq)


data VizState = MkVizState
  { cfg           :: !VizConfig
  , maxId         :: !ID
  , evalAppMakers :: !(Map Int (V.EvalAppRequestParams -> TopDecl Resolved))
  -- ^ Map from Unique of V.ID to eval-app-directive maker
  }
  deriving stock (Generic)

instance Show VizState where
  show MkVizState{cfg, maxId, evalAppMakers} =
    "MkVizState { cfg = " <> show cfg <>
    ", maxId = " <> show maxId <>
    ", (keys of) evalAppMakers = " <> show (Map.keys evalAppMakers) <> " }"

mkInitialVizState :: VizConfig -> VizState
mkInitialVizState cfg =
  MkVizState
    { cfg = cfg
    , maxId = MkID 0
    , evalAppMakers = Map.empty
    }

-- Monad ops

-- | 'Internal' helper: This should only be used by other Viz monad ops
getVizCfg :: Viz VizConfig
getVizCfg = use #cfg

getVerTxtDocId :: Viz LSP.VersionedTextDocumentIdentifier
getVerTxtDocId = do
  cfg <- getVizCfg
  pure cfg.verTxtDocId

getExpandedType :: Type' Resolved -> Viz (Type' Resolved)
getExpandedType ty = do
  cfg <- getVizCfg
  pure $ TC.applyFinalSubstitution cfg.substitution cfg.moduleUri ty

getFresh :: Viz ID
getFresh = do
  #maxId <%= \(MkID n) -> MkID (n + 1)

getShouldSimplify :: Viz Bool
getShouldSimplify = do
  cfg <- getVizCfg
  pure cfg.shouldSimplify

{- | Assumes that the program is well-scoped and well-typed -}
prepEvalAppMaker :: V.ID -> Expr Resolved -> Viz ()
prepEvalAppMaker vid = \ case
  App appAnno appResolved _ -> do
    localDecls <- getLocalDecls
    let maker = \V.EvalAppRequestParams{args} ->
          Directive emptyAnno $ LazyEval emptyAnno $
            Where emptyAnno
              (App appAnno appResolved $ map toBoolExpr args)
              localDecls
    #evalAppMakers %= Map.insert vid.id maker
  _ -> pure ()

getLocalDecls :: Viz [LocalDecl Resolved]
getLocalDecls = do
  env <- ask
  pure env.localDecls

withLocalDecls :: [LocalDecl Resolved] -> Viz a -> Viz a
withLocalDecls newLocalDecls = local (\env -> env { localDecls = newLocalDecls <> env.localDecls })

-- Viz state helpers

lookupEvalAppMaker :: VizState -> V.ID -> Maybe (V.EvalAppRequestParams -> TopDecl Resolved)
lookupEvalAppMaker vs vid = Map.lookup vid.id vs.evalAppMakers

getVizConfig :: VizState -> VizConfig
getVizConfig vs = vs.cfg

------------------------------------------------------
-- VizError
------------------------------------------------------

data VizError
  = InvalidProgramNoDecidesFound
  | InvalidProgramDecidesMustNotHaveMoreThanOneGiven
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

-- TODO: Incorporate context like the specific erroring rule and the src range too
prettyPrintVizError :: VizError -> Text
prettyPrintVizError = \ case
  InvalidProgramNoDecidesFound ->
    "The program isn't the right sort for visualization: there are no DECIDE rules that can be visualized."
  InvalidProgramDecidesMustNotHaveMoreThanOneGiven ->
    "Visualization failed: DECIDE rules must reference no more than one GIVEN variable."

------------------------------------------------------
-- Entrypoint: Visualise
------------------------------------------------------

-- | Entrypoint: Generate boolean circuits of the given 'Decide'.
doVisualize :: Decide Resolved -> VizConfig -> Either VizError (RenderAsLadderInfo, VizState)
doVisualize decide cfg =
  let (result, vizState) = (vizProgram decide).getViz initialEnv initialVizState
  in case result of
    Left err         -> Left err
    Right ladderInfo -> Right (ladderInfo, vizState)
  where
    initialEnv = MkVizEnv { localDecls = [] }
    initialVizState = mkInitialVizState cfg

vizProgram :: Decide Resolved -> Viz RenderAsLadderInfo
vizProgram decide = MkRenderAsLadderInfo <$> getVerTxtDocId <*> translateDecide decide

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
  do
    shouldSimplify <- getShouldSimplify
    vid            <- getFresh
    vizBody        <- translateExpr shouldSimplify body
    pure $ V.MkFunDecl
      vid
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
          vid <- getFresh
          V.Not vid <$> go negand
        And {} -> do
          vid <- getFresh
          V.And vid <$> traverse go (scanAnd e)
        Or {} -> do
          vid <- getFresh
          V.Or vid <$> traverse go (scanOr e)
        Where _ e' ds ->
          withLocalDecls ds $
            go e' -- TODO: lossy

        -- TODO: Should handle BoolLits differently too
        -- 'var'
        App _ resolved [] -> do
          vid <- getFresh
          prepEvalAppMaker vid e
          leafFromVizName vid (mkVizNameWith prettyLayout resolved)

        App appAnno fnResolved args -> do
          fnOfAppIsFnFromBooleansToBoolean <- and <$> traverse hasBooleanType (appAnno : map getAnno args)
          -- for now, only translating App of boolean functions to V.App
          if isDevMode && fnOfAppIsFnFromBooleansToBoolean
            then do
              vid <- getFresh
              prepEvalAppMaker vid e
              V.App vid (mkVizNameWith nameToText fnResolved) <$> traverse go args
            else
              leaf "" $ Text.unwords $ (nameToText . getOriginal $ fnResolved) : (prettyLayout <$> args)

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

leafFromVizName :: V.ID -> V.Name -> Viz IRExpr
leafFromVizName vid vname = do
  pure $ V.UBoolVar vid vname defaultUBoolVarValue

leaf :: Text -> Text -> Viz IRExpr
leaf subject complement = do
  vid <- getFresh
  tempUniqueTODO <- getFresh
  -- tempUniqueTODO: I'd like to defer properly handling the V.Name for `leaf` and the kinds of cases it's used for.
  -- I'll return to this when we explicitly/properly handle more cases in translateExpr
  -- (I'm currently focusing on state in the frontend in the simpler case of App with no args)
  pure $ V.UBoolVar vid (V.MkName tempUniqueTODO.id $ subject <> " " <> complement) defaultUBoolVarValue

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

------------------------------------------------------
-- Helpers for checking if an Expr has Boolean type
------------------------------------------------------

hasBooleanType :: Anno_ t Extension -> Viz Bool
hasBooleanType (Anno {extra = Extension {resolvedInfo = Just (TypeInfo ty _)}}) =
  isBooleanType ty
hasBooleanType _ = pure False

-- | Returns True iff the (expanded) Type Resolved is that of a L4 BOOLEAN
isBooleanType :: Type' Resolved -> Viz Bool
isBooleanType ty = do
  type' <- getExpandedType ty
  pure $ case type' of
    TyApp _ (Ref _ uniq _) [] ->
      uniq == TC.booleanUnique
    _ -> False

------------------------------------------------------
-- UBoolValue x L4 True/False conversion helpers
------------------------------------------------------

toBoolExpr :: V.UBoolValue -> Expr Resolved
toBoolExpr = \ case
  V.FalseV   -> App emptyAnno TC.falseRef []
  V.TrueV    -> App emptyAnno TC.trueRef []
  V.UnknownV -> error "impossible for now"
