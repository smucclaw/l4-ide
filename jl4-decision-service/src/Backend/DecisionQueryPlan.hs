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
import Control.Applicative ((<|>))
import Control.Concurrent.STM
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.IntMap.Lazy as IntMap
import Data.IntMap.Lazy (IntMap)
import qualified Data.IntSet as IntSet
import Data.IntSet (IntSet)
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import Data.Ord (Down (..))
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Text.Read (readMaybe)
import Servant (ServerError (..), err400, err404, err500)

import qualified Data.UUID as UUID
import qualified Data.UUID.V5 as UUIDV5

import qualified LSP.L4.Rules as Rules
import qualified LSP.L4.Viz.Ladder as LadderViz
import qualified LSP.L4.Viz.VizExpr as VizExpr
import qualified Language.LSP.Protocol.Types as LSP

import Backend.Api (EvalBackend (..), FnArguments (..), FnLiteral (..))
import L4.Syntax (RawName (..))

data CachedDecisionQuery = CachedDecisionQuery
  { cacheKey :: !Text
  , ladderInfo :: !VizExpr.RenderAsLadderInfo
  , varLabelByUnique :: !(Map Int Text)
  , varDepsByUnique :: !(IntMap IntSet)
  , varInputRefsByUnique :: !(IntMap (Set LadderViz.InputRef))
  , compiled :: !(BDQ.CompiledDecisionQuery Int)
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
      , varLabelByUnique = labels
      , varDepsByUnique = deps
      , varInputRefsByUnique = inputRefs
      , compiled
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

data QueryAtom = QueryAtom
  { unique :: !Int
  , atomId :: !Text
  , label :: !Text
  }
  deriving stock (Show, Read, Ord, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data QueryOutcome = QueryOutcome
  { determined :: !(Maybe Bool)
  , support :: ![QueryAtom]
  }
  deriving stock (Show, Read, Ord, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data QueryImpact = QueryImpact
  { ifTrue :: !QueryOutcome
  , ifFalse :: !QueryOutcome
  }
  deriving stock (Show, Read, Ord, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data QueryInput = QueryInput
  { inputUnique :: !Int
  , inputLabel :: !Text
  , score :: !Double
  , atoms :: ![QueryAtom]
  }
  deriving stock (Show, Read, Ord, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data QueryAsk = QueryAsk
  { container :: !Text
  , key :: !(Maybe Text)
  , label :: !Text
  , score :: !Double
  , atoms :: ![QueryAtom]
  }
  deriving stock (Show, Read, Ord, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

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

    paramUniqueByLabel :: Map Text Int
    paramUniqueByLabel =
      Map.fromList
        [ (lbl, u)
        | (u, lbl) <- Map.toList paramsByUnique
        ]

    renderInputRef :: LadderViz.InputRef -> Text
    renderInputRef ref =
      let
        rootLbl =
          Maybe.fromMaybe
            (Text.pack (show ref.rootUnique))
            ( Map.lookup ref.rootUnique paramsByUnique
                <|> Map.lookup ref.rootUnique cached.varLabelByUnique
            )
        pathTxt =
          case ref.path of
            [] -> ""
            xs -> "." <> Text.intercalate "." xs
       in rootLbl <> pathTxt

    stableAtomId :: Int -> Text -> Text
    stableAtomId u lbl =
      let
        refs =
          List.sort
            [ renderInputRef ref
            | ref <- Set.toList (IntMap.findWithDefault Set.empty u cached.varInputRefsByUnique)
            ]
        canonical =
          Text.intercalate
            "|"
            ( [name, lbl]
                <> ["refs=" <> Text.intercalate ";" refs | not (null refs)]
            )
       in UUID.toText (UUIDV5.generateNamed UUIDV5.namespaceURL (BS.unpack (Text.encodeUtf8 canonical)))

    atomIdByUnique :: Map Int Text
    atomIdByUnique =
      Map.fromList
        [ (u, stableAtomId u lbl)
        | (u, lbl) <- Map.toList cached.varLabelByUnique
        ]

    uniqueByAtomId :: Map Text Int
    uniqueByAtomId =
      Map.fromList
        [ (aid, u)
        | (u, aid) <- Map.toList atomIdByUnique
        ]

    parseUniqueKey t = readMaybe (Text.unpack t) :: Maybe Int
    stripBackticks t =
      Maybe.fromMaybe t $
        Text.stripPrefix "`" t >>= \t1 ->
          Text.stripSuffix "`" t1

    parseProjectionLabel :: Text -> Maybe (Text, Text)
    parseProjectionLabel lbl = do
      (container0, rest0) <- Text.breakOn "'s " lbl & \case
        (c, r) | not (Text.null r) -> Just (c, Text.drop 3 r)
        _ -> Nothing
      let field = stripBackticks (Text.strip rest0)
      guard (not (Text.null (Text.strip container0)))
      guard (not (Text.null field))
      pure (Text.strip container0, field)

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

    answeredAskKeys :: Set (Text, Maybe Text)
    answeredAskKeys =
      Set.fromList $
        concat
          [ case Text.breakOn "." k of
              (containerLabel, rest)
                | Text.null rest -> [(containerLabel, Nothing)]
                | otherwise -> [(containerLabel, Just (Text.drop 1 rest))]
          | (k, _b) <- flattenedLabelBindings
          ]

    leafUniquesByInputRef :: Map LadderViz.InputRef [Int]
    leafUniquesByInputRef =
      Map.fromListWith (<>)
        [ (ref, [u])
        | (u, refs) <- IntMap.toList cached.varInputRefsByUnique
        , [ref] <- [Set.toList refs]
        ]

    candidateInputRefsForKey :: Text -> [LadderViz.InputRef]
    candidateInputRefsForKey k =
      case Text.breakOn "." k of
        (containerLabel, rest) ->
          case Map.lookup containerLabel paramUniqueByLabel of
            Nothing -> []
            Just rootUnique ->
              if Text.null rest
                then [LadderViz.MkInputRef rootUnique []]
                else
                  let
                    raw = Text.drop 1 rest
                    singletonRef = LadderViz.MkInputRef rootUnique [raw]
                    segmented =
                      let segs = filter (not . Text.null) (Text.splitOn "." raw)
                       in LadderViz.MkInputRef rootUnique segs
                   in if singletonRef == segmented then [singletonRef] else [singletonRef, segmented]

    labelToUniques0 :: Map Text [Int]
    labelToUniques0 =
      Map.fromListWith (<>)
        [ (lbl, [u])
        | (u, lbl) <- Map.toList cached.varLabelByUnique
        ]

    projectionLabelToUniques :: Map Text [Int]
    projectionLabelToUniques =
      Map.fromListWith (<>)
        [ (container <> "." <> field, [u])
        | (u, lbl) <- Map.toList cached.varLabelByUnique
        , Just (container, field) <- [parseProjectionLabel lbl]
        ]

    labelToUniques :: Map Text [Int]
    labelToUniques =
      labelToUniques0 <> projectionLabelToUniques

    knownBindings =
      Map.fromList
        [ (u, b)
        | (k, b) <- flattenedLabelBindings
        , u <-
            Maybe.fromMaybe []
              ( Map.lookup k labelToUniques
                  <|> (pure <$> parseUniqueKey k)
                  <|> (pure <$> Map.lookup k uniqueByAtomId)
              )
        , Map.member u cached.varLabelByUnique
        ]
        <>
        Map.fromList
          [ (u, b)
          | (k, b) <- flattenedLabelBindings
          , ref <- candidateInputRefsForKey k
          , u <- Map.findWithDefault [] ref leafUniquesByInputRef
          , Map.member u cached.varLabelByUnique
          ]

    res = BDQ.queryDecision cached.compiled knownBindings
    atomOf u =
      let lbl = Map.findWithDefault (Text.pack (show u)) u cached.varLabelByUnique
          aid = Map.findWithDefault (stableAtomId u lbl) u atomIdByUnique
       in QueryAtom u aid lbl
    atomsOfSet s = map atomOf (Set.toList s)

    outcomeToJson o =
      QueryOutcome
        { determined = o.determined
        , support = atomsOfSet o.support
        }

    impactJson =
      Map.fromList
        [ ( u
          , QueryImpact
              { ifTrue = outcomeToJson imp.ifTrue
              , ifFalse = outcomeToJson imp.ifFalse
              }
          )
        | (u, imp) <- Map.toList res.impact
        ]
    impactByAtomIdJson =
      Map.fromList
        [ ( (atomOf u).atomId
          , QueryImpact
              { ifTrue = outcomeToJson imp.ifTrue
              , ifFalse = outcomeToJson imp.ifFalse
              }
          )
        | (u, imp) <- Map.toList res.impact
        ]

    impactScoreFor :: Int -> Double
    impactScoreFor u =
      case Map.lookup u res.impact of
        Nothing -> 0
        Just imp ->
          let bump o = if Maybe.isJust o.determined then (1 :: Int) else 0
           in (fromIntegral (bump imp.ifTrue + bump imp.ifFalse) :: Double)

    atomParamDeps :: Int -> [Int]
    atomParamDeps atomUniq =
      case IntMap.lookup atomUniq cached.varDepsByUnique of
        Nothing -> []
        Just deps ->
          [p | p <- IntSet.toList deps, Map.member p paramsByUnique]

    stillNeededAtoms = atomsOfSet res.support

    inputAtoms :: [(Int, [QueryAtom])]
    inputAtoms =
      [ (pUniq, [a | a <- stillNeededAtoms, pUniq `elem` atomParamDeps a.unique])
      | (pUniq, _lbl) <- Map.toList paramsByUnique
      ]

    inputScores :: [(Int, Double)]
    inputScores =
      [ ( pUniq
        , sum
            [ let deps = atomParamDeps a.unique
                  w = 1 / max 1 (fromIntegral (length deps))
               in w * (1 + impactScoreFor a.unique)
            | a <- as
            ]
        )
      | (pUniq, as) <- inputAtoms
      ]

    inputsRanked =
      [ QueryInput
          { inputUnique = pUniq
          , inputLabel = Map.findWithDefault (Text.pack (show pUniq)) pUniq paramsByUnique
          , score = sc
          , atoms = Map.findWithDefault [] pUniq (Map.fromList inputAtoms)
          }
      | (pUniq, sc) <-
          List.sortOn
            (\(u, sc0) -> (Down sc0, Map.findWithDefault "" u paramsByUnique))
            inputScores
      , sc > 0
      ]

    askKeyDepsForAtom :: Int -> Set (Text, Maybe Text)
    askKeyDepsForAtom atomUniq =
      let
        refs =
          Maybe.fromMaybe Set.empty (IntMap.lookup atomUniq cached.varInputRefsByUnique)
        fallback = do
          lbl <- Map.lookup atomUniq cached.varLabelByUnique
          (c, f) <- parseProjectionLabel lbl
          pure (Set.singleton (c, Just f))
       in
        Set.fromList
          [ (containerLabel, keyMaybe)
          | ref <- Set.toList refs
          , containerLabel <-
              Maybe.maybeToList
                ( Map.lookup ref.rootUnique paramsByUnique
                    <|> Map.lookup ref.rootUnique cached.varLabelByUnique
                )
          , let
              keyMaybe =
                case ref.path of
                  [] -> Nothing
                  xs -> Just (Text.intercalate "." xs)
          ]
          <> Maybe.fromMaybe Set.empty fallback

    askAtomsMap0 :: Map (Text, Maybe Text) [QueryAtom]
    askAtomsMap0 =
      Map.fromListWith (<>)
        [ (askKey, [a])
        | a <- stillNeededAtoms
        , askKey <- Set.toList (askKeyDepsForAtom a.unique)
        ]

    askAtomsMap :: Map (Text, Maybe Text) [QueryAtom]
    askAtomsMap =
      let
        containers = Set.fromList [c | ((c, _), _) <- Map.toList askAtomsMap0]
        atomsByUniques = fmap (Set.fromList . map (.unique)) askAtomsMap0
       in
        List.foldl'
          ( \m containerLabel ->
              let
                emptyKey = (containerLabel, Nothing)
                otherKeys = [k | k@(_, Just _) <- Map.keys m, fst k == containerLabel]
               in
                case Map.lookup emptyKey atomsByUniques of
                  Nothing -> m
                  Just emptyAtoms ->
                    if null otherKeys
                      then m
                      else
                        let otherAtoms =
                              Set.unions
                                [ Map.findWithDefault Set.empty k atomsByUniques
                                | k <- otherKeys
                                ]
                         in if emptyAtoms `Set.isSubsetOf` otherAtoms then Map.delete emptyKey m else m
          )
          askAtomsMap0
          (Set.toList containers)

    askDepsSize :: QueryAtom -> Double
    askDepsSize a =
      max 1 (fromIntegral (Set.size (askKeyDepsForAtom a.unique)))

    askScores :: Map (Text, Maybe Text) Double
    askScores =
      Map.mapWithKey
        (\_ atoms -> sum [(1 / askDepsSize a) * (1 + impactScoreFor a.unique) | a <- atoms])
        askAtomsMap

    asksRanked =
      [ QueryAsk
          { container = containerLabel
          , key = keyMaybe
          , label =
              case keyMaybe of
                Nothing -> containerLabel
                Just k -> containerLabel <> "." <> k
          , score = sc
          , atoms = Map.findWithDefault [] (containerLabel, keyMaybe) askAtomsMap
          }
      | ((containerLabel, keyMaybe), sc) <-
          List.sortOn
            (\((c, k), sc0) -> (Down sc0, c, k))
            (Map.toList askScores)
      , sc > 0
      , (containerLabel, keyMaybe) `Set.notMember` answeredAskKeys
      ]

    noteTxt =
      "StillNeeded/ranked are boolean atoms from ladder visualization; these may be derived predicates rather than original user inputs. Bindings are matched by atom label (including dotted labels for nested fields), atom unique (as a decimal string), or atomId (a stable UUIDv5 derived from function name, atom label, and input refs). `inputs` ranks function parameters using a simple dependency-based heuristic."
        <> " `asks` ranks askable keys; for record parameters this includes projected field paths discovered via `Proj`."
   in
    QueryPlanResponse
      { determined = res.determined
      , stillNeeded = atomsOfSet res.support
      , ranked = map atomOf res.ranked
      , inputs = inputsRanked
      , asks = asksRanked
      , impact = impactJson
      , impactByAtomId = impactByAtomIdJson
      , note = noteTxt
      , ladder = Just cached.ladderInfo
      }
