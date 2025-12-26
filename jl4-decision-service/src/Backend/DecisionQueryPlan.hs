{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Backend.DecisionQueryPlan (
  CachedDecisionQuery (..),
  decisionQueryCacheKey,
  getOrBuildDecisionQueryCache,
  buildDecisionQueryCache,
  QueryAtom (..),
  QueryOutcome (..),
  QueryImpact (..),
  QueryInput (..),
  QueryAsk (..),
  QueryPlanResponse (..),
  queryPlan,
) where

import Base
import qualified Backend.BooleanDecisionQuery as BDQ
import Backend.Jl4 as Jl4
import Control.Concurrent.STM
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Servant (ServerError (..), err400, err404, err500)

import qualified Data.UUID as UUID
import qualified Data.UUID.V5 as UUIDV5

import qualified LSP.L4.Rules as Rules
import qualified LSP.L4.Viz.Ladder as LadderViz
import qualified LSP.L4.Viz.VizExpr as VizExpr
import qualified Language.LSP.Protocol.Types as LSP

import Backend.Api (EvalBackend (..), FnArguments (..), FnLiteral (..))
import L4.Syntax (RawName (..))
import qualified L4.Decision.QueryPlan as QP
import L4.Decision.QueryPlan (QueryAsk (..), QueryAtom (..), QueryImpact (..), QueryInput (..), QueryOutcome (..))

data CachedDecisionQuery = CachedDecisionQuery
  { cacheKey :: !Text
  , ladderInfo :: !VizExpr.RenderAsLadderInfo
  , core :: !QP.CachedDecisionQuery
  }

decisionQueryCacheKey :: Text -> Map EvalBackend Text -> Text
decisionQueryCacheKey funName sources =
  let
    sourcesTxt =
      Text.intercalate
        "\n\n"
        [ Text.pack (show b) <> "\n" <> src
        | (b, src) <- Map.toAscList sources
        ]
    canonical =
      Text.intercalate
        "\n\n"
        [ "function=" <> funName
        , "sources=" <> sourcesTxt
        ]
  in UUID.toText (UUIDV5.generateNamed UUIDV5.namespaceURL (BS.unpack (Text.encodeUtf8 canonical)))

getOrBuildDecisionQueryCache ::
  (MonadIO m) =>
  (MonadError ServerError m) =>
  TVar (Map Text fn) ->
  Text ->
  (fn -> Maybe CachedDecisionQuery) ->
  (fn -> Text) ->
  (CachedDecisionQuery -> fn -> fn) ->
  (Text -> fn -> m CachedDecisionQuery) ->
  m CachedDecisionQuery
getOrBuildDecisionQueryCache functionsTVar name getCache cacheKeyNow setCache buildCache =
  go (2 :: Int)
 where
  go retriesRemaining = do
    fn <- do
      functions <- liftIO $ readTVarIO functionsTVar
      case Map.lookup name functions of
        Nothing -> throwError err404
        Just f -> pure f

    let keyNow = cacheKeyNow fn
    case getCache fn of
      Just cached | cached.cacheKey == keyNow -> pure cached
      _ -> do
        cached <- buildCache name fn
        stored <-
          liftIO $
            atomically $
              stateTVar functionsTVar $ \functions ->
                case Map.lookup name functions of
                  Nothing -> (False, functions)
                  Just fn0 ->
                    if cacheKeyNow fn0 == cached.cacheKey
                      then (True, Map.insert name (setCache cached fn0) functions)
                      else (False, functions)
        if stored
          then pure cached
          else
            if retriesRemaining <= 0
              then pure cached
              else go (retriesRemaining - 1)

buildDecisionQueryCache ::
  (MonadIO m) =>
  (MonadError ServerError m) =>
  Text ->
  Map EvalBackend Text ->
  m CachedDecisionQuery
buildDecisionQueryCache funName sources = do
  source <- case Map.lookup JL4 sources of
    Nothing -> throwError err400 {errBody = "No JL4 source available for query-plan"}
    Just s -> pure s

  let fileName = Text.unpack funName <> ".l4"
  (errs, mTcRes) <- liftIO $ Jl4.typecheckModule fileName source Map.empty
  tcRes <- case mTcRes of
    Nothing -> throwError err500 {errBody = encodeTextLBS (Text.intercalate "; " errs)}
    Just r -> pure r
  let Rules.TypeCheckResult{module' = resolvedModule, substitution = subst} = tcRes

  decide <- runExceptT (Jl4.getFunctionDefinition (NormalName funName) resolvedModule) >>= \case
    Left e -> throwError err500 {errBody = encodeTextLBS (Text.pack (show e))}
    Right d -> pure d

  let verDocId = mkVerDocId fileName
  let vizCfg = LadderViz.mkVizConfig verDocId resolvedModule subst True
  (ladderInfo, vizState) <- case LadderViz.doVisualize decide vizCfg of
    Left e -> throwError err500 {errBody = encodeTextLBS (LadderViz.prettyPrintVizError e)}
    Right x -> pure x

  let (boolExpr, labels, order) = vizExprToBoolExpr ladderInfo.funDecl.body
      compiled = BDQ.compileDecisionQuery order boolExpr
      deps = LadderViz.getAtomDeps vizState
      inputRefs = LadderViz.getAtomInputRefs vizState

  pure
    CachedDecisionQuery
      { cacheKey = decisionQueryCacheKey funName sources
      , ladderInfo
      , core =
          QP.CachedDecisionQuery
            { varLabelByUnique = labels
            , varDepsByUnique = deps
            , varInputRefsByUnique =
                fmap
                  (Set.map (\ref -> QP.MkInputRef ref.rootUnique ref.path))
                  inputRefs
            , compiled
            }
      }

encodeTextLBS :: Text -> LBS.ByteString
encodeTextLBS = LBS.fromStrict . Text.encodeUtf8

mkVerDocId :: FilePath -> LSP.VersionedTextDocumentIdentifier
mkVerDocId fileName =
  LSP.VersionedTextDocumentIdentifier
    { _uri = LSP.filePathToUri fileName
    , _version = 1
    }

vizExprToBoolExpr ::
  VizExpr.IRExpr ->
  (BDQ.BoolExpr Int, Map Int Text, [Int])
vizExprToBoolExpr expr =
  let (e, labels, order0) = go expr
   in (e, labels, List.nub order0)
 where
  go :: VizExpr.IRExpr -> (BDQ.BoolExpr Int, Map Int Text, [Int])
  go = \case
    VizExpr.TrueE _ _ -> (BDQ.BTrue, Map.empty, [])
    VizExpr.FalseE _ _ -> (BDQ.BFalse, Map.empty, [])
    VizExpr.UBoolVar _ nm _ _ ->
      let u = nm.unique
       in (BDQ.BVar u, Map.singleton u nm.label, [u])
    VizExpr.App _ nm _args ->
      let u = nm.unique
       in (BDQ.BVar u, Map.singleton u nm.label, [u])
    VizExpr.Not _ x ->
      let (ex, m, o) = go x
       in (BDQ.BNot ex, m, o)
    VizExpr.And _ xs ->
      let (es, ms, os) = unzip3 (map go xs)
       in (BDQ.BAnd es, Map.unions ms, concat os)
    VizExpr.Or _ xs ->
      let (es, ms, os) = unzip3 (map go xs)
       in (BDQ.BOr es, Map.unions ms, concat os)

data QueryPlanResponse = QueryPlanResponse
  { determined :: !(Maybe Bool)
  , stillNeeded :: ![QueryAtom]
  , ranked :: ![QueryAtom]
  , inputs :: ![QueryInput]
  , asks :: ![QueryAsk]
  , impact :: !(Map Int QueryImpact)
  , impactByAtomId :: !(Map Text QueryImpact)
  , note :: !Text
  , ladder :: !(Maybe VizExpr.RenderAsLadderInfo)
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

queryPlan :: Text -> CachedDecisionQuery -> FnArguments -> QueryPlanResponse
queryPlan name cached args =
  let
    paramsByUnique :: Map Int Text
    paramsByUnique =
      Map.fromList [(p.unique, p.label) | p <- cached.ladderInfo.funDecl.params]

    flattenBoolBindings :: Text -> FnLiteral -> [(Text, Bool)]
    flattenBoolBindings prefix = \case
      FnLitBool b -> [(prefix, b)]
      FnObject kvs ->
        concat
          [ flattenBoolBindings (prefix <> "." <> k) v
          | (k, v) <- kvs
          ]
      _ -> []

    flattenedLabelBindings :: [(Text, Bool)]
    flattenedLabelBindings =
      concat
        [ case mv of
            Nothing -> []
            Just v -> flattenBoolBindings k v
        | (k, mv) <- Map.toList args.fnArguments
        ]
    QP.QueryPlanResponse{determined, stillNeeded, ranked, inputs, asks = asksRanked, impact, impactByAtomId, note} =
      QP.queryPlan name paramsByUnique cached.core flattenedLabelBindings
   in
    QueryPlanResponse
      { determined
      , stillNeeded
      , ranked
      , inputs
      , asks = asksRanked
      , impact
      , impactByAtomId
      , note
      , ladder = Just cached.ladderInfo
      }
