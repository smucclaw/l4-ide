{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module L4.Decision.QueryPlan (
  InputRef (..),
  CachedDecisionQuery (..),
  QueryAtom (..),
  QueryOutcome (..),
  QueryImpact (..),
  QueryInput (..),
  QueryAsk (..),
  QueryPlanResponse (..),
  queryPlan,
) where

import Base
import qualified L4.Decision.BooleanDecisionQuery as BDQ
import Control.Applicative ((<|>))
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.ByteString as BS
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

import qualified Data.UUID as UUID
import qualified Data.UUID.V5 as UUIDV5

data InputRef = MkInputRef
  { rootUnique :: !Int
  , path :: ![Text]
  }
  deriving stock (Show, Read, Ord, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data CachedDecisionQuery = CachedDecisionQuery
  { varLabelByUnique :: !(Map Int Text)
  , varDepsByUnique :: !(IntMap IntSet)
  , varInputRefsByUnique :: !(IntMap (Set InputRef))
  , compiled :: !(BDQ.CompiledDecisionQuery Int)
  }

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
  , path :: ![Text]
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
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

queryPlan ::
  Text ->
  -- | Parameter labels keyed by unique.
  Map Int Text ->
  CachedDecisionQuery ->
  -- | Flattened boolean bindings keyed by label (including dotted keys), plus optionally `unique` (decimal) or `atomId`.
  [(Text, Bool)] ->
  QueryPlanResponse
queryPlan name paramsByUnique cached flattenedLabelBindings =
  let
    paramUniqueByLabel :: Map Text Int
    paramUniqueByLabel =
      Map.fromList
        [ (lbl, u)
        | (u, lbl) <- Map.toList paramsByUnique
        ]

    renderInputRef :: InputRef -> Text
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

    answeredAskKeys :: Set (Text, [Text])
    answeredAskKeys =
      Set.fromList $
        concat
          [ case Text.breakOn "." k of
              (containerLabel, rest)
                | Text.null rest -> [(containerLabel, [])]
                | otherwise ->
                    let
                      raw = Text.drop 1 rest
                      singletonPath = [raw]
                      segmentedPath = filter (not . Text.null) (Text.splitOn "." raw)
                     in if singletonPath == segmentedPath
                          then [(containerLabel, singletonPath)]
                          else [(containerLabel, singletonPath), (containerLabel, segmentedPath)]
          | (k, _b) <- flattenedLabelBindings
          ]

    leafUniquesByInputRef :: Map InputRef [Int]
    leafUniquesByInputRef =
      Map.fromListWith (<>)
        [ (ref, [u])
        | (u, refs) <- IntMap.toList cached.varInputRefsByUnique
        , [ref] <- [Set.toList refs]
        ]

    candidateInputRefsForKey :: Text -> [InputRef]
    candidateInputRefsForKey k =
      case Text.breakOn "." k of
        (containerLabel, rest) ->
          case Map.lookup containerLabel paramUniqueByLabel of
            Nothing -> []
            Just rootUnique ->
              if Text.null rest
                then [MkInputRef rootUnique []]
                else
                  let
                    raw = Text.drop 1 rest
                    singletonRef = MkInputRef rootUnique [raw]
                    segmented =
                      let segs = filter (not . Text.null) (Text.splitOn "." raw)
                       in MkInputRef rootUnique segs
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

    keyToLeafUniques :: Text -> [Int]
    keyToLeafUniques k =
      List.nub $
        concat
          [ Map.findWithDefault [] ref leafUniquesByInputRef
          | ref <- candidateInputRefsForKey k
          ]

    -- Apply boolean bindings to atoms by:
    -- - exact label match (including dotted keys)
    -- - `unique` as decimal string
    -- - atomId (stable UUIDv5)
    -- - for record parameters, treat `i.someField = true/false` as binding any leaf atom that depends solely on that input ref.
    knownBindings :: Map Int Bool
    knownBindings =
      Map.fromList $
        concat
          [ [ (u, b)
            | u <-
                Maybe.fromMaybe []
                  ( Map.lookup k labelToUniques
                      <|> (pure <$> parseUniqueKey k)
                      <|> (pure <$> Map.lookup k uniqueByAtomId)
                  )
            ]
              <> [ (u, b) | u <- keyToLeafUniques k ]
          | (k, b) <- flattenedLabelBindings
          ]

    res = BDQ.queryDecision cached.compiled knownBindings

    atomOf :: Int -> QueryAtom
    atomOf u =
      QueryAtom
        { unique = u
        , atomId = Map.findWithDefault (Text.pack (show u)) u atomIdByUnique
        , label = Map.findWithDefault (Text.pack (show u)) u cached.varLabelByUnique
        }

    atomsOfSet :: Set Int -> [QueryAtom]
    atomsOfSet s = map atomOf (Set.toList s)

    impactScoreFor :: Int -> Double
    impactScoreFor atomUniq =
      let
        byUnique =
          Maybe.fromMaybe
            (0, False)
            ( do
                vi <- Map.lookup atomUniq res.impact
                let
                  impacts =
                    [ (vi.ifTrue.determined, vi.ifTrue.support)
                    , (vi.ifFalse.determined, vi.ifFalse.support)
                    ]
                  canDetermine = any (\(d, _s) -> d /= Nothing) impacts
                  shrink =
                    sum
                      [ fromIntegral (Set.size res.support - Set.size s)
                      | (_d, s) <- impacts
                      ]
                pure (shrink, canDetermine)
            )
       in (if snd byUnique then 2 else 0) + fst byUnique / max 1 (fromIntegral (Set.size res.support))

    impactJson :: Map Int QueryImpact
    impactJson =
      Map.map
        ( \vi ->
            QueryImpact
              { ifTrue = QueryOutcome vi.ifTrue.determined (atomsOfSet vi.ifTrue.support)
              , ifFalse = QueryOutcome vi.ifFalse.determined (atomsOfSet vi.ifFalse.support)
              }
        )
        res.impact

    impactByAtomIdJson :: Map Text QueryImpact
    impactByAtomIdJson =
      Map.fromList
        [ (atomIdByUnique Map.! u, imp)
        | (u, imp) <- Map.toList impactJson
        , Map.member u atomIdByUnique
        ]

    depAtoms :: Int -> [Int]
    depAtoms atomUniq =
      IntSet.toList (IntMap.findWithDefault IntSet.empty atomUniq cached.varDepsByUnique)

    atomParamDeps :: Int -> [Int]
    atomParamDeps atomUniq =
      List.nub $
        concat
          [ [pUniq]
          | a <- atomUniq : depAtoms atomUniq
          , ref <- Set.toList (IntMap.findWithDefault Set.empty a cached.varInputRefsByUnique)
          , pUniq <- [ref.rootUnique]
          , Map.member pUniq paramsByUnique
          ]

    stillNeededAtoms :: [QueryAtom]
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

    askKeyDepsForAtom :: Int -> Set (Text, [Text])
    askKeyDepsForAtom atomUniq =
      let
        refs =
          Maybe.fromMaybe Set.empty (IntMap.lookup atomUniq cached.varInputRefsByUnique)
        fallback :: Maybe (Set (Text, [Text]))
        fallback = do
          lbl <- Map.lookup atomUniq cached.varLabelByUnique
          (c, f) <- parseProjectionLabel lbl
          let singletonPath = [f]
          let segmentedPath = filter (not . Text.null) (Text.splitOn "." f)
          pure $
            Set.fromList $
              if singletonPath == segmentedPath
                then [(c, singletonPath)]
                else [(c, singletonPath), (c, segmentedPath)]
       in
        Set.fromList
          [ (containerLabel, ref.path)
          | ref <- Set.toList refs
          , containerLabel <-
              Maybe.maybeToList
                ( Map.lookup ref.rootUnique paramsByUnique
                    <|> Map.lookup ref.rootUnique cached.varLabelByUnique
                )
          ]
          <> Maybe.fromMaybe Set.empty fallback

    askAtomsMap0 :: Map (Text, [Text]) [QueryAtom]
    askAtomsMap0 =
      Map.fromListWith (<>)
        [ (askKey, [a])
        | a <- stillNeededAtoms
        , askKey <- Set.toList (askKeyDepsForAtom a.unique)
        ]

    askAtomsMap :: Map (Text, [Text]) [QueryAtom]
    askAtomsMap =
      let
        containers = Set.fromList [c | ((c, _), _) <- Map.toList askAtomsMap0]
        atomsByUniques = fmap (Set.fromList . map (.unique)) askAtomsMap0
       in
        List.foldl'
          ( \m containerLabel ->
              let
                emptyKey = (containerLabel, [])
                otherKeys = [k | k@(_, (_ : _)) <- Map.keys m, fst k == containerLabel]
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

    askScores :: Map (Text, [Text]) Double
    askScores =
      Map.mapWithKey
        (\_ atoms -> sum [(1 / askDepsSize a) * (1 + impactScoreFor a.unique) | a <- atoms])
        askAtomsMap

    asksRanked =
      [ QueryAsk
          { container = containerLabel
          , key =
              case pathSegments of
                [] -> Nothing
                xs -> Just (Text.intercalate "." xs)
          , path = pathSegments
          , label =
              case pathSegments of
                [] -> containerLabel
                xs -> containerLabel <> "." <> Text.intercalate "." xs
          , score = sc
          , atoms = Map.findWithDefault [] (containerLabel, pathSegments) askAtomsMap
          }
      | ((containerLabel, pathSegments), sc) <-
          List.sortOn
            (\((c, k), sc0) -> (Down sc0, c, k))
            (Map.toList askScores)
      , sc > 0
      , (containerLabel, pathSegments) `Set.notMember` answeredAskKeys
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
      }
