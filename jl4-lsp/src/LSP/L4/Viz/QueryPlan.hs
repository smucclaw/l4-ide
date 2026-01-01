{-# LANGUAGE LambdaCase #-}

module LSP.L4.Viz.QueryPlan (
  buildQueryPlanCache,
  annotateLadderWithAtomIds,
  queryPlanFromLadder,
) where

import Base
import Data.IntMap.Lazy (IntMap)
import Data.IntSet (IntSet)
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text

import qualified L4.Decision.BooleanDecisionQuery as BDQ
import qualified L4.Decision.QueryPlan as QP

import qualified LSP.L4.Viz.Ladder as LadderViz
import qualified LSP.L4.Viz.VizExpr as VizExpr

annotateLadderWithAtomIds ::
  VizExpr.RenderAsLadderInfo ->
  LadderViz.VizState ->
  VizExpr.RenderAsLadderInfo
annotateLadderWithAtomIds ladderInfo vizState =
  let
    funName :: Text
    funName = ladderInfo.funDecl.fnName.label

    paramsByUnique :: Map Int Text
    paramsByUnique =
      Map.fromList
        [ (p.unique, p.label)
        | p <- ladderInfo.funDecl.params
        ]

    cache = buildQueryPlanCache ladderInfo vizState
    atomIds = QP.atomIdByUnique funName paramsByUnique cache

    annotateExpr :: VizExpr.IRExpr -> VizExpr.IRExpr
    annotateExpr = \case
      VizExpr.And uid xs ->
        VizExpr.And uid (map annotateExpr xs)
      VizExpr.Or uid xs ->
        VizExpr.Or uid (map annotateExpr xs)
      VizExpr.Not uid x ->
        VizExpr.Not uid (annotateExpr x)
      VizExpr.TrueE uid nm ->
        VizExpr.TrueE uid nm
      VizExpr.FalseE uid nm ->
        VizExpr.FalseE uid nm
      VizExpr.UBoolVar uid nm val canInline _oldAtomId ->
        VizExpr.UBoolVar uid nm val canInline (Map.findWithDefault (Text.pack (show nm.unique)) nm.unique atomIds)
      VizExpr.App uid nm args _oldAtomId ->
        VizExpr.App uid nm (map annotateExpr args) (Map.findWithDefault (Text.pack (show nm.unique)) nm.unique atomIds)
   in
    ladderInfo
      { VizExpr.funDecl =
          ladderInfo.funDecl
            { VizExpr.body = annotateExpr ladderInfo.funDecl.body
            }
      }

buildQueryPlanCache :: VizExpr.RenderAsLadderInfo -> LadderViz.VizState -> QP.CachedDecisionQuery
buildQueryPlanCache ladderInfo vizState =
  let
    (boolExpr, labels, order) = vizExprToBoolExpr ladderInfo.funDecl.body
    compiled = BDQ.compileDecisionQuery order boolExpr
    deps :: IntMap IntSet
    deps = LadderViz.getAtomDeps vizState
    inputRefs = LadderViz.getAtomInputRefs vizState
   in
    QP.CachedDecisionQuery
      { varLabelByUnique = labels
      , varDepsByUnique = deps
      , varInputRefsByUnique =
          fmap
            (Set.map (\ref -> QP.MkInputRef ref.rootUnique ref.path))
            inputRefs
      , compiled
      }

queryPlanFromLadder ::
  Text ->
  -- | Parameter labels keyed by unique.
  Map Int Text ->
  VizExpr.RenderAsLadderInfo ->
  LadderViz.VizState ->
  [(Text, Bool)] ->
  QP.QueryPlanResponse
queryPlanFromLadder funName paramsByUnique ladderInfo vizState flattenedLabelBindings =
  QP.queryPlan funName paramsByUnique (buildQueryPlanCache ladderInfo vizState) flattenedLabelBindings

vizExprToBoolExpr ::
  VizExpr.IRExpr ->
  (BDQ.BoolExpr Int, Map Int Text, [Int])
vizExprToBoolExpr expr =
  let (e, labels, order0) = go expr
   in (e, labels, List.nub order0)
 where
  go :: VizExpr.IRExpr -> (BDQ.BoolExpr Int, Map Int Text, [Int])
  go = \case
    VizExpr.TrueE _ _ -> (BDQ.BTrue, mempty, [])
    VizExpr.FalseE _ _ -> (BDQ.BFalse, mempty, [])
    VizExpr.UBoolVar _ nm _ _ _ ->
      let u = nm.unique
       in (BDQ.BVar u, Map.singleton u nm.label, [u])
    VizExpr.App _ nm _args _ ->
      let u = nm.unique
       in (BDQ.BVar u, Map.singleton u nm.label, [u])
    VizExpr.Not _ x ->
      let (ex, m, o) = go x
       in (BDQ.BNot ex, m, o)
    VizExpr.And _ xs ->
      let (es, ms, os) = unzip3 (map go xs)
       in (BDQ.BAnd es, mconcat ms, mconcat os)
    VizExpr.Or _ xs ->
      let (es, ms, os) = unzip3 (map go xs)
       in (BDQ.BOr es, mconcat ms, mconcat os)
