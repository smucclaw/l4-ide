{-# LANGUAGE ViewPatterns, PatternSynonyms #-}
-- | Ladder diagram visualization for WASM.
--
-- This module provides a simplified version of the ladder visualization
-- for use in WASM builds. It converts L4 DECIDE rules into a format
-- suitable for rendering as ladder diagrams.
--
-- @since 0.1
module L4.Viz.Ladder
  ( -- * Main entry points
    visualize
  , visualizeByName
  , findAllVisualizableDecides
  , VisualizableDecide(..)
  , VizError(..)
  , prettyPrintVizError
  ) where

import Base
import qualified Base.Text as Text
import Data.IntMap.Lazy (IntMap)
import qualified Data.IntMap.Lazy as Map

import qualified Data.Set as Set
import qualified Data.List.NonEmpty as NE
import qualified Data.List as List
import Optics.State.Operators ((<%=), (%=))
import Optics
import Control.Monad.Extra (unlessM)
import qualified Data.Text.Encoding as TextEncoding

import qualified L4.Crypto.UUID5 as UUID5

import qualified L4.TypeCheck as TC
import L4.Annotation
import L4.Parser.SrcSpan (SrcRange(..), SrcPos(..))
import L4.Syntax
import L4.Print (prettyLayout)
import qualified L4.Transform as Transform (simplify)
import L4.Desugar
import L4.Viz.VizExpr (RenderAsLadderInfo(..), VersionedDocId(..), FunDecl(..), IRExpr, InertContext(..), ID(..), UBoolValue(..))
import qualified L4.Viz.VizExpr as VizExpr

------------------------------------------------------
-- Errors
------------------------------------------------------

data VizError
  = InvalidProgramNoDecidesFound
  | InvalidDecideMustHaveBoolRetType
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

prettyPrintVizError :: VizError -> Text
prettyPrintVizError = \case
  InvalidProgramNoDecidesFound ->
    "The program isn't the right sort for visualization: there are no DECIDE rules that can be visualized."
  InvalidDecideMustHaveBoolRetType ->
    "Can only visualize, as a ladder diagram, a DECIDE that returns a boolean."

------------------------------------------------------
-- Monad
------------------------------------------------------

newtype Viz a = MkViz { getViz :: VizEnv -> VizState -> (Either VizError a, VizState) }
  deriving
    (Functor, Applicative, Monad, MonadState VizState, MonadError VizError, MonadReader VizEnv)
    via ReaderT VizEnv (ExceptT VizError (State VizState))

newtype VizEnv = MkVizEnv { localDecls :: [LocalDecl Resolved] }

data VizState = MkVizState
  { moduleUri      :: !NormalizedUri
  , module'        :: !(Module Resolved)
  , substitution   :: !TC.Substitution
  , shouldSimplify :: !Bool
  , maxId          :: !ID
  , functionName   :: !Text
  , defsForInlining :: IntMap (Expr Resolved)
  , atomInputRefs  :: IntMap (Set InputRef)
  }
  deriving stock (Generic)

data InputRef = MkInputRef
  { rootUnique :: !Int
  , path       :: ![Text]
  }
  deriving stock (Eq, Ord, Show, Generic)

mkInitialState :: NormalizedUri -> Module Resolved -> TC.Substitution -> Bool -> VizState
mkInitialState uri mod' subst simp = MkVizState
  { moduleUri = uri
  , module' = mod'
  , substitution = subst
  , shouldSimplify = simp
  , maxId = MkID 0
  , functionName = ""
  , defsForInlining = Map.empty
  , atomInputRefs = Map.empty
  }

------------------------------------------------------
-- Monad operations
------------------------------------------------------

getFresh :: Viz ID
getFresh = #maxId <%= \(MkID n) -> MkID (n + 1)

getShouldSimplify :: Viz Bool
getShouldSimplify = use #shouldSimplify

getSubstitution :: Viz TC.Substitution
getSubstitution = use #substitution

getModuleUri :: Viz NormalizedUri
getModuleUri = use #moduleUri

getLocalDecls :: Viz [LocalDecl Resolved]
getLocalDecls = asks (.localDecls)

withLocalDecls :: [LocalDecl Resolved] -> Viz a -> Viz a
withLocalDecls newDecls = local (\env -> env { localDecls = newDecls <> env.localDecls })

getExpandedType :: Type' Resolved -> Viz (Type' Resolved)
getExpandedType ty = do
  subst <- getSubstitution
  uri <- getModuleUri
  pure $ TC.applyFinalSubstitution subst uri ty

recordAtomInputRefs :: Int -> Set InputRef -> Viz ()
recordAtomInputRefs uniq refs =
  #atomInputRefs %= Map.insertWith (<>) uniq refs

lookupLocalDecideBody :: Int -> Viz (Maybe (Expr Resolved))
lookupLocalDecideBody target = do
  localDecls <- getLocalDecls
  pure $ listToMaybe $ flip mapMaybe localDecls $ \case
    LocalDecide _ (MkDecide _ _ (MkAppForm _ n _ _) body) ->
      if (getUnique n).unique == target then Just body else Nothing
    _ -> Nothing

collectDefsForInlining :: Viz (IntMap (Expr Resolved))
collectDefsForInlining = do
  mod' <- use #module'
  uri <- getModuleUri
  pure $ toMap (foldTopLevelDecides (tryExtractDef uri) mod')
  where
    tryExtractDef :: NormalizedUri -> Decide Resolved -> [(L4.Syntax.Unique, Expr Resolved)]
    tryExtractDef uri = foldDecides $ \case
      DefForInlining uri' uniq' definiens | uri == uri' -> [(uniq', definiens)]
      _ -> []

    toMap :: [(L4.Syntax.Unique, Expr Resolved)] -> IntMap (Expr Resolved)
    toMap = Map.fromList . map (\(MkUnique _ k _, v) -> (k, v))

hasDefForInlining :: L4.Syntax.Unique -> Viz Bool
hasDefForInlining (MkUnique _ uniq uniqUri) = do
  uri <- getModuleUri
  defsForInlining <- use #defsForInlining
  pure $ uniqUri == uri && Map.member uniq defsForInlining

------------------------------------------------------
-- Main entry point
------------------------------------------------------

-- | Information about a visualizable DECIDE rule.
data VisualizableDecide = MkVisualizableDecide
  { vdName     :: !Text       -- ^ The function name
  , vdStartLine :: !Int       -- ^ 1-indexed start line
  , vdStartCol  :: !Int       -- ^ 1-indexed start column
  }
  deriving stock (Eq, Show, Generic)

-- | Visualize a module by finding the first visualizable DECIDE.
-- Returns the RenderAsLadderInfo or an error.
visualize :: NormalizedUri -> Text -> Int -> Module Resolved -> TC.Substitution -> Bool -> Either VizError RenderAsLadderInfo
visualize uri uriText version mod' subst simplify =
  case findVisualizableDecide mod' of
    Nothing -> Left InvalidProgramNoDecidesFound
    Just decide -> visualizeDecide uri uriText version mod' subst simplify decide

-- | Visualize a specific DECIDE by function name.
-- Returns Nothing if the function is not found or not visualizable.
visualizeByName :: NormalizedUri -> Text -> Int -> Module Resolved -> TC.Substitution -> Bool -> Text -> Either VizError RenderAsLadderInfo
visualizeByName uri uriText version mod' subst simplify targetName =
  case findDecideByName mod' targetName of
    Nothing -> Left InvalidProgramNoDecidesFound
    Just decide -> visualizeDecide uri uriText version mod' subst simplify decide

-- | Internal: visualize a specific Decide
visualizeDecide :: NormalizedUri -> Text -> Int -> Module Resolved -> TC.Substitution -> Bool -> Decide Resolved -> Either VizError RenderAsLadderInfo
visualizeDecide uri uriText version mod' subst simplify decide =
  let initialEnv = MkVizEnv { localDecls = [] }
      initialState = mkInitialState uri mod' subst simplify
      verDocId = MkVersionedDocId uriText version
      (result, _) = (vizProgram verDocId decide).getViz initialEnv initialState
  in result

-- | Find all DECIDE rules that can be visualized.
-- Rather than just checking types, we try to visualize each DECIDE
-- and only include those that succeed. This matches the native LSP behavior
-- and is more robust for edge cases.
findAllVisualizableDecides :: NormalizedUri -> Module Resolved -> TC.Substitution -> [VisualizableDecide]
findAllVisualizableDecides uri mod' subst =
  foldTopLevelDecides (tryVisualize uri subst) mod'
  where
    tryVisualize :: NormalizedUri -> TC.Substitution -> Decide Resolved -> [VisualizableDecide]
    tryVisualize nuri sub d@(MkDecide _ _ (MkAppForm _ funResolved _ _) _body) =
      -- Try to actually visualize the decide - if it succeeds, it's visualizable
      let initialEnv = MkVizEnv { localDecls = [] }
          initialState = mkInitialState nuri mod' sub False  -- simplify=False for checking
          dummyVerDocId = MkVersionedDocId "" 0  -- Doesn't matter for checking
          (result, _) = (vizProgram dummyVerDocId d).getViz initialEnv initialState
      in case result of
        Right _ ->
          -- Visualization succeeded - include this decide
          case rangeOf d of
            Just (MkSrcRange (MkSrcPos startLine startCol) _ _ _) ->
              [MkVisualizableDecide
                { vdName = prettyLayout (getActual funResolved)
                , vdStartLine = startLine
                , vdStartCol = startCol
                }]
            Nothing -> []
        Left _ ->
          -- Visualization failed (e.g., non-boolean type) - skip
          []

-- | Find the first DECIDE with a boolean return type.
findVisualizableDecide :: Module Resolved -> Maybe (Decide Resolved)
findVisualizableDecide mod' =
  listToMaybe $ foldTopLevelDecides selectBoolDecide mod'
  where
    selectBoolDecide :: Decide Resolved -> [Decide Resolved]
    selectBoolDecide d@(MkDecide anno _ _ _body) =
      case anno.extra.resolvedInfo of
        Just (TypeInfo ty _) | isBoolTypeSimple ty -> [d]
        _ -> []

    isBoolTypeSimple :: Type' Resolved -> Bool
    isBoolTypeSimple (TyApp _ (Ref _ uniq _) []) = uniq == TC.booleanUnique
    isBoolTypeSimple _ = False

-- | Find a DECIDE by its function name.
-- Note: Does not filter by type - the visualization will validate
-- that the return type is boolean and fail with an appropriate error if not.
findDecideByName :: Module Resolved -> Text -> Maybe (Decide Resolved)
findDecideByName mod' targetName =
  listToMaybe $ foldTopLevelDecides matchByName mod'
  where
    matchByName :: Decide Resolved -> [Decide Resolved]
    matchByName d@(MkDecide _ _ (MkAppForm _ funResolved _ _) _body) =
      let funName = prettyLayout (getActual funResolved)
      in [d | funName == targetName]

------------------------------------------------------
-- Visualization
------------------------------------------------------

vizProgram :: VersionedDocId -> Decide Resolved -> Viz RenderAsLadderInfo
vizProgram verDocId decide = MkRenderAsLadderInfo verDocId <$> translateDecide decide

translateDecide :: Decide Resolved -> Viz FunDecl
translateDecide (MkDecide _ (MkTypeSig _ givenSig _) (MkAppForm _ funResolved appArgs _) body) = do
  unlessM (hasBooleanType (getAnno body)) $
    throwError InvalidDecideMustHaveBoolRetType

  let funName = mkPrettyVizName funResolved
  assign #functionName funName.label
  assign #defsForInlining =<< collectDefsForInlining
  shouldSimplify <- getShouldSimplify
  vid <- getFresh
  vizBody <- translateExpr shouldSimplify (carameliseExpr body)
  pure $ MkFunDecl vid funName (paramNamesFromGivens givenSig appArgs) vizBody
  where
    paramNamesFromGivens :: GivenSig Resolved -> [Resolved] -> [VizExpr.Name]
    paramNamesFromGivens (MkGivenSig _ optionallyTypedNames) args =
      let fromGivens = mkPrettyVizName . getResolved <$> optionallyTypedNames
          fromAppArgs = mkPrettyVizName <$> args
      in Map.elems $ Map.fromList [(p.unique, p) | p <- fromGivens <> fromAppArgs]

    getResolved :: OptionallyTypedName Resolved -> Resolved
    getResolved (MkOptionallyTypedName _ paramName _) = paramName

translateExpr :: Bool -> Expr Resolved -> Viz IRExpr
translateExpr True = translateExpr False . Transform.simplify
translateExpr False = go
  where
    go :: Expr Resolved -> Viz IRExpr
    go e = case e of
      Not _ negand -> do
        vid <- getFresh
        VizExpr.Not vid <$> go negand

      And {} -> do
        vid <- getFresh
        VizExpr.And vid <$> traverse go (scanAnd e)

      Or {} -> do
        vid <- getFresh
        VizExpr.Or vid <$> traverse go (scanOr e)

      Where _ e' ds ->
        withLocalDecls ds $ go e'

      Inert _ txt coreCtx -> do
        vid <- getFresh
        let inertCtx = case coreCtx of
              InertCtxAnd  -> InertAnd
              InertCtxOr   -> InertOr
              InertCtxNone -> InertAnd
        pure $ VizExpr.InertE vid txt inertCtx

      App _ resolved [] -> do
        vid <- getFresh
        let vname = mkPrettyVizName resolved
        case getUnique resolved of
          u | u == TC.trueUnique  -> pure $ VizExpr.TrueE vid vname
            | u == TC.falseUnique -> pure $ VizExpr.FalseE vid vname
          _ -> varLeaf vid vname resolved

      App appAnno _fnResolved args -> do
        fnOfAppIsFnFromBooleansToBoolean <- and <$> traverse hasBooleanType (appAnno : map getAnno args)
        if fnOfAppIsFnFromBooleansToBoolean
          then do
            vid <- getFresh
            let uniq = vid.id
                label = prettyLayout e
                vname = VizExpr.MkName uniq label
            refs <- freeInputRefsExpanded Set.empty e
            recordAtomInputRefs uniq refs
            functionName <- use #functionName
            let atomId = generateAtomId functionName label refs
            VizExpr.App vid vname <$> traverse go args <*> pure atomId
          else
            leafFromExpr e

      _ -> leafFromExpr e

scanAnd :: Expr Resolved -> [Expr Resolved]
scanAnd (And _ e1 e2) = scanAnd e1 <> scanAnd e2
scanAnd e = [e]

scanOr :: Expr Resolved -> [Expr Resolved]
scanOr (Or _ e1 e2) = scanOr e1 <> scanOr e2
scanOr e = [e]

------------------------------------------------------
-- Leaf makers
------------------------------------------------------

varLeaf :: ID -> VizExpr.Name -> Resolved -> Viz IRExpr
varLeaf vid vname resolved = do
  canInline <- case resolved of
    Ref _ uniq _ -> hasDefForInlining uniq
    Def uniq _ -> hasDefForInlining uniq
    _ -> pure False
  let target = (getUnique resolved).unique
  refs <- lookupLocalDecideBody target >>= \case
    Nothing -> pure (Set.singleton (MkInputRef vname.unique []))
    Just body -> freeInputRefsExpanded (Set.singleton vname.unique) body
  recordAtomInputRefs vname.unique refs
  functionName <- use #functionName
  let atomId = generateAtomId functionName vname.label refs
  pure $ VizExpr.UBoolVar vid vname UnknownV canInline atomId

leafFromExpr :: Expr Resolved -> Viz IRExpr
leafFromExpr expr = do
  vid <- getFresh
  tempUniqueTODO <- getFresh
  let uniq = tempUniqueTODO.id
  refs <- freeInputRefsExpanded Set.empty expr
  recordAtomInputRefs uniq refs
  functionName <- use #functionName
  let label = prettyLayout expr
      atomId = generateAtomId functionName label refs
  pure $ VizExpr.UBoolVar vid (VizExpr.MkName uniq label) UnknownV False atomId

------------------------------------------------------
-- Name helpers
------------------------------------------------------

mkPrettyVizName :: Resolved -> VizExpr.Name
mkPrettyVizName = mkVizNameWith prettyLayout

mkVizNameWith :: (L4.Syntax.Name -> Text) -> Resolved -> VizExpr.Name
mkVizNameWith printer (getUniqueName -> (MkUnique {unique}, name)) =
  VizExpr.MkName unique (printer name)

------------------------------------------------------
-- Free variable tracking
------------------------------------------------------

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
-- AtomId generation
------------------------------------------------------

generateAtomId :: Text -> Text -> Set InputRef -> Text
generateAtomId functionName label refs =
  let renderInputRef :: InputRef -> Text
      renderInputRef ref =
        let rootTxt = Text.pack (show ref.rootUnique)
            pathTxt = case ref.path of
              [] -> ""
              xs -> "." <> Text.intercalate "." xs
        in rootTxt <> pathTxt

      sortedRefs = List.sort
        [ renderInputRef ref
        | ref <- Set.toList refs
        ]

      canonical = Text.intercalate "|"
        ( [functionName, label]
          <> ["refs=" <> Text.intercalate ";" sortedRefs | not (null sortedRefs)]
        )
  in UUID5.toText (UUID5.generateNamed UUID5.namespaceURL (TextEncoding.encodeUtf8 canonical))

------------------------------------------------------
-- Boolean type checking
------------------------------------------------------

hasBooleanType :: Anno_ t Extension -> Viz Bool
hasBooleanType (Anno {extra = Extension {resolvedInfo = Just (TypeInfo ty _)}}) =
  isBooleanType ty
hasBooleanType _ = pure False

isBooleanType :: Type' Resolved -> Viz Bool
isBooleanType ty = do
  type' <- getExpandedType ty
  pure $ case type' of
    TyApp _ (Ref _ uniq _) [] -> uniq == TC.booleanUnique
    _ -> False

------------------------------------------------------
-- Patterns
------------------------------------------------------

pattern DefForInlining :: NormalizedUri -> L4.Syntax.Unique -> Expr Resolved -> Decide Resolved
pattern DefForInlining uri unique definiens <-
  MkDecide _ _ (MkAppForm _ (Def unique@(MkUnique _ _ uri) _) _ _) definiens
