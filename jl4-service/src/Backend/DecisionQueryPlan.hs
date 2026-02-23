{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Backend.DecisionQueryPlan (
  CachedDecisionQuery (..),
  decisionQueryCacheKey,
  buildDecisionQueryCacheFromCompiled,
  orderAsksForElicitation,
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
import Backend.FunctionSchema (Parameter (..), Parameters (..), parametersFromDecideWithErrors)
import Backend.Jl4 (CompiledModule (..))
import Data.Aeson (FromJSON, ToJSON, (.=), object)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Ord (Down (..))
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Text.Read (readMaybe)
import Servant (ServerError (..), err400)

import qualified Data.UUID as UUID
import qualified Data.UUID.V5 as UUIDV5

import qualified LSP.L4.Viz.Ladder as LadderViz
import qualified LSP.L4.Viz.VizExpr as VizExpr
import qualified Language.LSP.Protocol.Types as LSP

import Backend.Api (EvalBackend (..), FnArguments (..), FnLiteral (..))
import qualified L4.Decision.QueryPlan as QP
import L4.Decision.QueryPlan (QueryAtom (..), QueryImpact (..), QueryInput (..), QueryOutcome (..))

data CachedDecisionQuery = CachedDecisionQuery
  { cacheKey :: !Text
  , ladderInfo :: !VizExpr.RenderAsLadderInfo
  , core :: !QP.CachedDecisionQuery
  , paramSchema :: !Parameters
  }

data QueryAsk = QueryAsk
  { container :: !Text
  , key :: !(Maybe Text)
  , path :: ![Text]
  , label :: !Text
  , score :: !Double
  , atoms :: ![QueryAtom]
  , schema :: !(Maybe Parameter)
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON)

-- | Strip backticks from L4 quoted identifiers when serializing to JSON.
-- Backticks are L4's syntax for allowing spaces in identifiers, but in JSON
-- we already have double quotes for string keys, so backticks shouldn't leak through.
stripBackticks :: Text -> Text
stripBackticks t = fromMaybe t $ Text.stripPrefix "`" t >>= Text.stripSuffix "`"

instance ToJSON QueryAsk where
  toJSON qa = object
    [ "container" .= stripBackticks qa.container
    , "key" .= qa.key
    , "path" .= qa.path
    , "label" .= stripBackticks qa.label
    , "score" .= qa.score
    , "atoms" .= qa.atoms  -- QueryAtom has its own ToJSON that strips backticks
    , "schema" .= qa.schema
    ]

data PathSortKey
  = KSField !Int !Text
  | KSIndex !Int
  | KSWildcard
  | KSUnknown !Text
  deriving stock (Show, Eq, Ord)

orderAsksForElicitation :: Parameters -> [QueryAsk] -> [QueryAsk]
orderAsksForElicitation params =
  let
    rootSchemaFor :: QueryAsk -> Maybe Parameter
    rootSchemaFor a = Map.lookup a.container params.parameterMap

    fieldOrderIndex :: Parameter -> Map Text Int
    fieldOrderIndex p =
      let
        props = Map.keys (fromMaybe Map.empty p.parameterProperties)
        ord = fromMaybe (List.sort props) p.parameterPropertyOrder
       in Map.fromList (zip ord [0 ..])

    consumePropertyWithKey :: Map Text Parameter -> [Text] -> Maybe (Text, Parameter, [Text])
    consumePropertyWithKey props segs =
      let
        n = length segs
        tryLen k =
          let key0 = Text.intercalate "." (take k segs)
           in (\p1 -> (key0, p1, drop k segs)) <$> Map.lookup key0 props
       in
        listToMaybe (mapMaybe tryLen [n, n - 1 .. 1])

    pathSortKeyFromSchema :: Parameter -> [Text] -> [PathSortKey]
    pathSortKeyFromSchema p0 = go p0
     where
      go p segs =
        case segs of
          [] -> []
          (seg : rest) ->
            case p.parameterItems of
              Just items ->
                case seg of
                  "*" -> KSWildcard : go items rest
                  "[]" -> KSWildcard : go items rest
                  _ | Text.all Char.isDigit seg ->
                        case readMaybe (Text.unpack seg) of
                          Just idx -> KSIndex idx : go items rest
                          Nothing -> KSUnknown seg : go items rest
                  _ -> KSUnknown seg : go items rest
              Nothing ->
                case p.parameterProperties of
                  Just props ->
                    case consumePropertyWithKey props segs of
                      Nothing -> KSUnknown seg : go p rest
                      Just (fieldKey, p1, rest') ->
                        let
                          idx =
                            Map.findWithDefault maxBound fieldKey (fieldOrderIndex p)
                         in KSField idx fieldKey : go p1 rest'
                  Nothing -> KSUnknown seg : go p rest

    pathSortKey :: QueryAsk -> [PathSortKey]
    pathSortKey a =
      case rootSchemaFor a of
        Nothing -> map KSUnknown a.path
        Just root -> pathSortKeyFromSchema root a.path

    sortKey a = (Down a.score, a.container, pathSortKey a, a.label)
   in
    List.sortOn sortKey

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

-- | Build the decision query cache from an already-compiled module.
-- Uses the pre-compiled AST and resolved module directly, avoiding re-typechecking.
buildDecisionQueryCacheFromCompiled ::
  (MonadError ServerError m) =>
  Text ->
  CompiledModule ->
  Map EvalBackend Text ->
  m CachedDecisionQuery
buildDecisionQueryCacheFromCompiled funName compiled sources = do
  let resolvedModule = compiled.compiledModule
      decide = compiled.compiledDecide
      fileName = Text.unpack funName <> ".l4"
      verDocId = mkVerDocId fileName
      -- The module is already fully resolved; an empty substitution is sufficient.
      vizCfg = LadderViz.mkVizConfig verDocId resolvedModule Map.empty True

  (ladderInfo, vizState) <- case LadderViz.doVisualize decide vizCfg of
    Left e -> throwError err400 {errBody = Aeson.encode $ object ["error" .= LadderViz.prettyPrintVizError e]}
    Right x -> pure x

  let (boolExpr, labels, order) = vizExprToBoolExpr ladderInfo.funDecl.body
      bddCompiled = BDQ.compileDecisionQuery order boolExpr
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
            , compiled = bddCompiled
            }
      , paramSchema = parametersFromDecideWithErrors resolvedModule decide []
      }

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
       in (BDQ.BAnd es, Map.unions ms, concat os)
    VizExpr.Or _ xs ->
      let (es, ms, os) = unzip3 (map go xs)
       in (BDQ.BOr es, Map.unions ms, concat os)
    -- Inert elements evaluate to identity for their context: AND→True, OR→False
    VizExpr.InertE _ _ VizExpr.InertAnd -> (BDQ.BTrue, Map.empty, [])
    VizExpr.InertE _ _ VizExpr.InertOr -> (BDQ.BFalse, Map.empty, [])

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
      FnArray xs ->
        concat
          [ flattenBoolBindings (prefix <> "." <> Text.pack (show (idx :: Int))) v
          | (idx, v) <- zip [0 ..] xs
          ]
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

    askSchemaFor :: QP.QueryAsk -> Maybe Parameter
    askSchemaFor qa = do
      root <- Map.lookup qa.container cached.paramSchema.parameterMap
      schemaAtPathVariants root qa.path

    schemaAtPathVariants :: Parameter -> [Text] -> Maybe Parameter
    schemaAtPathVariants root segs =
      let
        splitSegments =
          concatMap
            (\s -> filter (not . Text.null) (Text.splitOn "." s))
            segs
        variants = List.nub [segs, splitSegments]
       in
        listToMaybe (mapMaybe (schemaAtPath root) variants)

    schemaAtPath :: Parameter -> [Text] -> Maybe Parameter
    schemaAtPath p0 = \case
      [] -> Just p0
      segs@(seg : rest) ->
        let
          descendItems =
            p0.parameterItems
         in
          case seg of
            "*" -> descendItems >>= \p1 -> schemaAtPath p1 rest
            "[]" -> descendItems >>= \p1 -> schemaAtPath p1 rest
            _ | Text.all Char.isDigit seg -> descendItems >>= \p1 -> schemaAtPath p1 rest
            _ ->
              p0.parameterProperties >>= \props ->
                consumeProperty props segs >>= \(p1, rest') ->
                  schemaAtPath p1 rest'

    consumeProperty :: Map Text Parameter -> [Text] -> Maybe (Parameter, [Text])
    consumeProperty props segs =
      let
        n = length segs
        tryLen k =
          let key0 = Text.intercalate "." (take k segs)
           in (,drop k segs) <$> Map.lookup key0 props
       in
        listToMaybe (mapMaybe tryLen [n, n - 1 .. 1])

    asksEnriched =
      [ QueryAsk
          { container = a.container
          , key = a.key
          , path = a.path
          , label = a.label
          , score = a.score
          , atoms = a.atoms
          , schema = askSchemaFor a
          }
      | a <- asksRanked
      ]
    asksOrdered = orderAsksForElicitation cached.paramSchema asksEnriched
   in
    QueryPlanResponse
      { determined
      , stillNeeded
      , ranked
      , inputs
      , asks = asksOrdered
      , impact
      , impactByAtomId
      , note
      , ladder = Just cached.ladderInfo
      }
