{-# LANGUAGE ViewPatterns, PatternSynonyms #-}

module LSP.L4.Viz.Ladder (
  -- * Viz Decide entrypoint
  doVisualize,

  -- * Inline Exprs (currently only inlines simple 'App of no args' exprs)
  inlineExprs,

  -- * VizConfig, VizState
  VizConfig (..),
  mkVizConfig,
  VizState,

  -- * Viz State helpers
  lookupAppExprMaker,
  getAtomDeps,
  InputRef (..),
  getAtomInputRefs,
  getVizConfig,

  -- * Other helpers
  prettyPrintVizError
  ) where

import Base
import Data.IntMap.Lazy (IntMap)
import qualified Data.IntMap.Lazy as Map
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import qualified Data.Set as Set
import qualified Data.List.NonEmpty as NE
import Optics.State.Operators ((<%=), (%=))
import Optics
import Control.Monad.Extra (unlessM)
import qualified Language.LSP.Protocol.Types as LSP

import qualified L4.TypeCheck as TC
import L4.Annotation
import L4.Syntax
import L4.Print (prettyLayout)
import qualified L4.Transform as Transform (simplify)
import LSP.L4.Viz.VizExpr
  ( ID (..), IRExpr,
    RenderAsLadderInfo (..),
  )
import qualified LSP.L4.Viz.VizExpr as V
import qualified LSP.L4.Viz.CustomProtocol as V (EvalAppRequestParams (..))
import L4.Desugar

------------------------------------------------------
-- Monad
------------------------------------------------------

newtype Viz a = MkViz {getViz :: VizEnv -> VizState -> (Either VizError a, VizState)}
  deriving
    (Functor, Applicative, Monad, MonadState VizState, MonadError VizError, MonadReader VizEnv)
    via ReaderT VizEnv (ExceptT VizError (State VizState))

-- | A 'local' env
newtype VizEnv = MkVizEnv { localDecls :: [LocalDecl Resolved]}

mkVizConfig :: LSP.VersionedTextDocumentIdentifier -> Module Resolved -> TC.Substitution -> Bool -> VizConfig
mkVizConfig verTxtDocId module' substitution shouldSimplify =
  let moduleUri = toNormalizedUri verTxtDocId._uri
  in MkVizConfig
    { moduleUri
    , module'
    , verTxtDocId
    , substitution
    , shouldSimplify
    }

data VizConfig = MkVizConfig
  { moduleUri      :: !NormalizedUri
  , module'        :: Module Resolved
  , verTxtDocId    :: !LSP.VersionedTextDocumentIdentifier
  , substitution   :: !TC.Substitution  -- might be more futureproof to use TypeCheckResult
  , shouldSimplify :: !Bool             -- ^ whether to simplify the expression
  }
  deriving stock (Show, Generic, Eq)


data VizState = MkVizState
  { cfg            :: !VizConfig
  , maxId          :: !ID
  , appExprMakers  :: IntMap (V.EvalAppRequestParams -> Expr Resolved)
  -- ^ Map from Unique of V.ID to eval-app-directive maker
  , defsForInlining :: IntMap (Expr Resolved)
  , atomDeps       :: IntMap IntSet
  , atomInputRefs  :: IntMap (Set InputRef)
  }
  deriving stock (Generic)

instance Show VizState where
  show MkVizState{cfg, maxId, appExprMakers, defsForInlining, atomDeps, atomInputRefs} =
    "MkVizState { cfg = " <> show cfg <>
    ", maxId = " <> show maxId <>
    ", (keys of) appExprMakers =   " <> show (Map.keys appExprMakers) <>
    ", defsForInlining =  " <> show defsForInlining <>
    ", (keys of) atomDeps = " <> show (Map.keys atomDeps) <>
    ", (keys of) atomInputRefs = " <> show (Map.keys atomInputRefs) <> " }"

mkInitialVizState :: VizConfig -> VizState
mkInitialVizState cfg =
  MkVizState
    { cfg
    , maxId = MkID 0
    , appExprMakers = Map.empty
    , defsForInlining = Map.empty
    , atomDeps = Map.empty
    , atomInputRefs = Map.empty
    }

------------------------------------------------------
-- Monad ops
------------------------------------------------------

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

{-# ANN prepEvalAppMaker ("HLINT: ignore Redundant lambda" :: String) #-}
{- | Assumes that the program is well-scoped and well-typed -}
prepEvalAppMaker :: V.ID -> Expr Resolved -> Viz ()
prepEvalAppMaker vid = \ case
  App appAnno appResolved _ -> do
    localDecls <- getLocalDecls
    let maker =
          \V.EvalAppRequestParams{args} ->
            Where emptyAnno
              (App appAnno appResolved $ map toBoolExpr args)
              localDecls
    #appExprMakers %= Map.insert vid.id maker
  _ -> pure ()

getLocalDecls :: Viz [LocalDecl Resolved]
getLocalDecls = do
  env <- ask
  pure env.localDecls

withLocalDecls :: [LocalDecl Resolved] -> Viz a -> Viz a
withLocalDecls newLocalDecls = local (\env -> env { localDecls = newLocalDecls <> env.localDecls })

recordAtomDeps :: V.Unique -> IntSet -> Viz ()
recordAtomDeps uniq deps =
  #atomDeps %= Map.insertWith (<>) uniq deps

data InputRef = MkInputRef
  { rootUnique :: !Int
  , path :: ![Text]
  }
  deriving stock (Eq, Ord, Show, Generic)

recordAtomInputRefs :: V.Unique -> Set InputRef -> Viz ()
recordAtomInputRefs uniq refs = do
  recordAtomDeps uniq (IntSet.fromList (map (.rootUnique) (Set.toList refs)))
  #atomInputRefs %= Map.insertWith (<>) uniq refs

lookupLocalDecideBody :: Int -> Viz (Maybe (Expr Resolved))
lookupLocalDecideBody target = do
  localDecls <- getLocalDecls
  pure $
    listToMaybe $
      flip mapMaybe localDecls $ \case
        LocalDecide _ (MkDecide _ _ (MkAppForm _ n _ _) body) ->
          if (getUnique n).unique == target then Just body else Nothing
        _ -> Nothing

freeInputRefsExpanded :: Set Int -> Expr Resolved -> Viz (Set InputRef)
freeInputRefsExpanded visited expr = do
  expanded <- traverse expandOne (Set.toList (freeInputRefs expr))
  pure (Set.unions expanded)
 where
  expandOne :: InputRef -> Viz (Set InputRef)
  expandOne r =
    case r.path of
      _ : _ -> pure (Set.singleton r)
      [] ->
        if Set.member r.rootUnique visited
          then pure (Set.singleton r)
          else do
            lookupLocalDecideBody r.rootUnique >>= \case
              Nothing -> pure (Set.singleton r)
              Just body -> freeInputRefsExpanded (Set.insert r.rootUnique visited) body

collectDefsForInlining :: Viz (IntMap (Expr Resolved))
collectDefsForInlining = do
  cfg <- getVizCfg
  pure $ toMap (foldTopLevelDecides tryExtractDef cfg.module')
    where
      tryExtractDef :: Decide Resolved -> [(Unique, Expr Resolved)]
      tryExtractDef = foldDecides $ \ case
        DefForInlining uniq' definiens -> [(uniq', definiens)]
        _ -> []

      toMap :: [(Unique, Expr Resolved)] -> IntMap (Expr Resolved)
      toMap = Map.fromList . map (\(MkUnique _ k _, v) -> (k, v))

hasDefForInlining :: Unique -> Viz Bool
hasDefForInlining (MkUnique _ uniq uniqUri) = do
  cfg             <- getVizCfg
  defsForInlining <- use #defsForInlining
  -- We don't want, e.g., to pick up on builtin Uniques, which can have the same Int unique as a user-defined Unique
  pure $ uniqUri == cfg.moduleUri && Map.member uniq defsForInlining

------------------------------------------------------
-- Viz state helpers
------------------------------------------------------

{- Consumers should use the following helpers to work with VizState.
I.e., I'm trying to hide the implementational details of VizState
(e.g. how VizConfig is related to VizState). -}

lookupAppExprMaker :: VizState -> V.ID -> Maybe (V.EvalAppRequestParams -> Expr Resolved)
lookupAppExprMaker vs vid = Map.lookup vid.id vs.appExprMakers

getAtomDeps :: VizState -> IntMap IntSet
getAtomDeps vs = vs.atomDeps

getAtomInputRefs :: VizState -> IntMap (Set InputRef)
getAtomInputRefs vs = vs.atomInputRefs

getVizConfig :: VizState -> VizConfig
getVizConfig vs = vs.cfg

------------------------------------------------------
-- VizError
------------------------------------------------------

data VizError
  = InvalidProgramNoDecidesFound
  | InvalidDecideMustHaveBoolRetType
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

-- TODO: Incorporate context like the specific erroring rule and the src range too
prettyPrintVizError :: VizError -> Text
prettyPrintVizError = \ case
  InvalidProgramNoDecidesFound ->
    "The program isn't the right sort for visualization: there are no DECIDE rules that can be visualized."
  InvalidDecideMustHaveBoolRetType ->
    "Can only visualize, as a ladder diagram, a DECIDE that returns a boolean."

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
translateDecide (MkDecide _ (MkTypeSig _ givenSig _) (MkAppForm _ funResolved appArgs _) body) =
  do
    unlessM (hasBooleanType (getAnno body)) $
      throwError InvalidDecideMustHaveBoolRetType

    assign #defsForInlining =<< collectDefsForInlining
    shouldSimplify <- getShouldSimplify
    vid            <- getFresh
    vizBody        <- translateExpr shouldSimplify (carameliseExpr body)
    pure $ V.MkFunDecl
      vid
      -- didn't want a backtick'd name in the header
      (mkPrettyVizName funResolved)
      (paramNamesFromGivens givenSig appArgs)
      vizBody
      where
        paramNamesFromGivens :: GivenSig Resolved -> [Resolved] -> [V.Name]
        paramNamesFromGivens (MkGivenSig _ optionallyTypedNames) args =
          let
            fromGivens = mkPrettyVizName . getResolved <$> optionallyTypedNames
            fromAppArgs = mkPrettyVizName <$> args
           in
            Map.elems $ Map.fromList [(p.unique, p) | p <- fromGivens <> fromAppArgs]

        -- TODO: I imagine there will be functionality for this kind of thing in a more central place soon;
        -- this can be replaced with that when that happens.
        getResolved :: OptionallyTypedName Resolved -> Resolved
        getResolved (MkOptionallyTypedName _ paramName _) = paramName

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

        -- 'var'
        App _ resolved [] -> do
          vid <- getFresh
          let vname = mkPrettyVizName resolved
          case getUnique resolved of
            u | u == TC.trueUnique  -> pure $ V.TrueE vid vname
              | u == TC.falseUnique -> pure $ V.FalseE vid vname
            _ -> varLeaf vid vname resolved
            -- TODO: Check how exactly a function of no args, as opposed to a var, would be represented?
            -- There was some discussion of this at a meeting, but can't remember exactly what was said

        App appAnno _fnResolved args -> do
          fnOfAppIsFnFromBooleansToBoolean <- and <$> traverse hasBooleanType (appAnno : map getAnno args)
          -- for now, only translating App of boolean functions to V.App
          if fnOfAppIsFnFromBooleansToBoolean
            then do
              vid <- getFresh
              prepEvalAppMaker vid e
              let uniq = vid.id
                  vname = V.MkName uniq (prettyLayout e)
              recordAtomInputRefs uniq =<< freeInputRefsExpanded Set.empty e
              V.App vid vname <$> traverse go args <*> pure ""
            else
              leafFromExpr e

        _ -> do
          leafFromExpr e

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

defaultUBoolVarCanInline :: Bool
defaultUBoolVarCanInline = False

varLeaf :: V.ID -> V.Name -> Resolved -> Viz IRExpr
varLeaf vid vname resolved = do
  canInline <- case resolved of
    Ref _ uniq _ -> hasDefForInlining uniq
    Def uniq _ -> hasDefForInlining uniq
    _ -> pure False
  -- TODO: Prob need to do this `canInline` thing for things that aren't App of no args too?
  let target = (getUnique resolved).unique
  refs <-
    lookupLocalDecideBody target >>= \case
      Nothing -> pure (Set.singleton (MkInputRef vname.unique []))
      Just body -> freeInputRefsExpanded (Set.singleton vname.unique) body
  recordAtomInputRefs vname.unique refs
  pure $ V.UBoolVar vid vname defaultUBoolVarValue canInline ""

leafFromExpr :: Expr Resolved -> Viz IRExpr
leafFromExpr expr = do
  vid <- getFresh
  tempUniqueTODO <- getFresh
  let uniq = tempUniqueTODO.id
  recordAtomInputRefs uniq =<< freeInputRefsExpanded Set.empty expr
  pure $
    V.UBoolVar
      vid
      (V.MkName uniq (prettyLayout expr))
      defaultUBoolVarValue
      defaultUBoolVarCanInline
      ""

------------------------------------------------------
-- Name helpers
------------------------------------------------------

mkPrettyVizName :: Resolved -> V.Name
mkPrettyVizName = mkVizNameWith prettyLayout

-- It's not obvious to me that we want to be using
-- getOriginal (as getUniqueName does), instead of getActual,
-- but I'm going with this for now since it's what was used to translate the function name.
mkVizNameWith :: (Name -> Text) -> Resolved -> V.Name
mkVizNameWith printer (getUniqueName -> (MkUnique {unique}, name)) =
  V.MkName unique (printer name)

------------------------------------------------------
-- Crude dependency tracking
------------------------------------------------------

freeInputRefs :: Expr Resolved -> Set InputRef
freeInputRefs expr =
  refsFromVars expr <> refsFromProjections expr
 where
  refsFromVars :: Expr Resolved -> Set InputRef
  refsFromVars =
    foldMapOf (Optics.gplate @(Expr Resolved)) $ \case
      App _ (Ref _ uniq _) [] -> Set.singleton (MkInputRef uniq.unique [])
      _ -> Set.empty

  refsFromProjections :: Expr Resolved -> Set InputRef
  refsFromProjections =
    foldMapOf (Optics.gplate @(Expr Resolved)) $ \case
      p@(Proj _ _ _) ->
        case projChain [] p of
          Nothing -> Set.empty
          Just r -> Set.singleton r
      _ -> Set.empty

  projChain :: [Text] -> Expr Resolved -> Maybe InputRef
  projChain acc = \case
    Proj _ base fieldResolved ->
      projChain (projectionSegments fieldResolved <> acc) base
    App _ (Ref _ uniq _) [] ->
      Just (MkInputRef uniq.unique acc)
    _ -> Nothing

  projectionSegments :: Resolved -> [Text]
  projectionSegments resolved =
    case rawName (getActual resolved) of
      QualifiedName qs n -> NE.toList qs <> [n]
      NormalName n -> [n]
      PreDef n -> [n]

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

------------------------------------------------------
-- Inline Exprs
------------------------------------------------------

{- | We only really need a *uni*directional pattern synonym,
but the other direction might be useful for testing. -}
pattern DefForInlining :: Unique -> Expr Resolved -> Decide Resolved
pattern DefForInlining unique definiens <-
  MkDecide _ _ (MkAppForm _ (Def unique _) _ _) definiens
  where
    DefForInlining unique definiens =
      MkDecide emptyAnno
      (MkTypeSig emptyAnno (MkGivenSig emptyAnno []) Nothing)
      (MkAppForm emptyAnno (Def unique TC.emptyName) [] Nothing)
      definiens

{- | Given a @VizState@, a top-level @Decide Resolved@, and a bunch of Uniques,
inline refs to those Uniques in the Decide.

Note:
- Currently only inlines 'App of no args' exprs
- Currently requires that the definiens be in the same module as the ref.
Lifting this restriction is not hard, but it's also not totally obvious that that'd be good UX.
-}
inlineExprs :: VizState -> Decide Resolved -> [Int] -> Decide Resolved
inlineExprs vs = foldr (inlineExpr vs)

inlineExpr :: VizState -> Int -> Decide Resolved -> Decide Resolved
inlineExpr vs target = over decideBody $ transformOf (Optics.gplate @(Expr Resolved)) replace
  where
    replace :: Expr Resolved -> Expr Resolved
    replace expr =
      if isRefOfTarget expr
      then
      case Map.lookup target vs.defsForInlining of
        Just definiens -> definiens
        Nothing -> error "Programmer error: either isRefOfTarget has false positives or we aren't recording all the definienda"
      else expr

    isRefOfTarget :: Expr Resolved -> Bool
    isRefOfTarget = \ case
      App _ resolved _args ->
        case resolved of
          Ref _ uniq _ -> uniq.unique == target
          _            -> False
      -- TODO: Look into whether we should handle other cases too
      _                    -> False
