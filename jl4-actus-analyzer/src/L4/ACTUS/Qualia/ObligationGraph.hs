{-# LANGUAGE StrictData #-}

-- | Obligation Graph extraction for qualia-based contract classification.
--
-- This module builds a directed graph of obligations between parties,
-- independent of naming conventions. The graph captures WHO owes WHAT to WHOM,
-- enabling structural classification that works even when contracts use
-- non-standard terminology, different languages, or obfuscated variable names.
--
-- Key insight: A loan is not a loan because it contains a variable named
-- "borrower" — it's a loan because one party has a net negative obligation
-- flow with temporal decay toward another party.
module L4.ACTUS.Qualia.ObligationGraph (
  -- * Core Types
  ObligationGraph (..),
  PartyNode (..),
  ObligationEdge (..),
  RecursionPattern (..),

  -- * Flow Analysis
  FlowRole (..),
  flowRole,

  -- * Value Types
  ValueType (..),
  TemporalPattern (..),
  DecayPattern (..),

  -- * Extraction
  extractObligationGraph,

  -- * Analysis
  partyCount,
  hasRecursion,
  isUnidirectional,
  isBidirectional,
) where

import Data.Hashable (hash)
import Data.List (nub)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import L4.Syntax
  ( DeonticModal (..)
  , Deonton (..)
  , Expr (..)
  , Module
  , Pattern (..)
  , RAction (..)
  , Resolved
  , nameToText
  , getOriginal
  )
import Optics (foldMapOf, gplate)

--------------------------------------------------------------------------------
-- Core Types
--------------------------------------------------------------------------------

-- | An obligation graph captures WHO owes WHAT to WHOM.
--
-- This is the primary data structure for qualia-based analysis. It represents
-- the structural essence of a contract's obligations without relying on
-- naming conventions.
data ObligationGraph = ObligationGraph
  { ogParties :: [PartyNode]
  -- ^ All parties identified in the contract (by structural role, not name)
  , ogObligations :: [ObligationEdge]
  -- ^ All obligation edges between parties
  , ogRecursions :: [RecursionPattern]
  -- ^ Self-referential structures (e.g., amortization)
  }
  deriving stock (Eq, Show)

-- | A party identified by structural role, not name.
--
-- Parties are identified by hashing their expression representation,
-- so the same structural party expression will map to the same ID
-- regardless of what name is used in the source code.
data PartyNode = PartyNode
  { pnId :: Int
  -- ^ Unique identifier for this party
  , pnExprHash :: Int
  -- ^ Hash of the party expression (for structural identity)
  , pnExprText :: Text
  -- ^ Pretty-printed representation (for debugging/display)
  , pnOutDegree :: Int
  -- ^ Number of outgoing MUST/DO obligations (this party as obligor)
  , pnInDegree :: Int
  -- ^ Number of incoming obligations (this party as beneficiary)
  , pnMayCount :: Int
  -- ^ Number of MAY clauses (discretionary rights)
  , pnShantCount :: Int
  -- ^ Number of SHANT/MUST NOT clauses (prohibitions)
  }
  deriving stock (Eq, Show)

-- | Computed role based on obligation flow direction.
--
-- This is the key qualia for determining contract type:
-- - A loan has one NetDebtor and one NetCreditor
-- - An exchange has two Symmetric parties
-- - An option has one AsymmetricWithRights
data FlowRole
  = NetDebtor
  -- ^ Party with more outgoing than incoming obligations
  | NetCreditor
  -- ^ Party with more incoming than outgoing obligations
  | Symmetric
  -- ^ Party with balanced obligations (exchange, swap)
  | Uninvolved
  -- ^ Party with no obligations
  deriving stock (Eq, Show)

-- | Compute the flow role of a party based on their obligation balance.
flowRole :: PartyNode -> FlowRole
flowRole pn
  | pn.pnOutDegree > pn.pnInDegree = NetDebtor
  | pn.pnInDegree > pn.pnOutDegree = NetCreditor
  | pn.pnOutDegree == pn.pnInDegree && pn.pnOutDegree > 0 = Symmetric
  | otherwise = Uninvolved

-- | An edge representing a deontic obligation between parties.
data ObligationEdge = ObligationEdge
  { oeFrom :: Int
  -- ^ Obligor party ID (who has the obligation)
  , oeTo :: Int
  -- ^ Beneficiary party ID (who receives/benefits)
  , oeModal :: DeonticModal
  -- ^ MUST/MAY/SHANT - the deontic modality
  , oeValueType :: ValueType
  -- ^ What kind of value flows (money, asset, service)
  , oeTiming :: TemporalPattern
  -- ^ When/how often the obligation occurs
  , oeActionText :: Text
  -- ^ The action description (for debugging/display)
  , oeHasHence :: Bool
  -- ^ Whether there's a HENCE clause (success path)
  , oeHasLest :: Bool
  -- ^ Whether there's a LEST clause (failure path)
  }
  deriving stock (Eq, Show)

-- | What kind of value flows in an obligation.
data ValueType
  = MoneyValue
  -- ^ Currency/money transfer (pay, deliver currency)
  | AssetValue
  -- ^ Non-fungible asset transfer
  | ServiceValue
  -- ^ Action/service performance
  | InformationValue
  -- ^ Information/notice provision
  | UnknownValue
  -- ^ Could not determine value type
  deriving stock (Eq, Show)

-- | Temporal patterns in obligations.
data TemporalPattern
  = PointInTime
  -- ^ Single occurrence (spot, forward)
  | Periodic
  -- ^ Repeating at regular intervals
  | Recursive DecayPattern
  -- ^ Self-referential with decay (amortization)
  | Continuous
  -- ^ Ongoing without specific timing
  | Unknown
  -- ^ Could not determine pattern
  deriving stock (Eq, Show)

-- | How recursive patterns decay over time.
data DecayPattern
  = LinearDecay
  -- ^ Fixed reduction each iteration
  | ExponentialDecay
  -- ^ Compound/exponential reduction
  | UntilExhaustion
  -- ^ Continue until value reaches zero
  | Unbounded
  -- ^ No natural termination
  deriving stock (Eq, Show)

-- | A recursion pattern detected in the contract.
--
-- Recursion is a key structural qualia for debt instruments:
-- amortizing loans have recursive payment structures that
-- decrease until the principal is exhausted.
data RecursionPattern = RecursionPattern
  { rpDepth :: Int
  -- ^ How deep is the recursion?
  , rpDecay :: DecayPattern
  -- ^ How does value change?
  , rpTermination :: Maybe Text
  -- ^ Termination condition (if identifiable)
  , rpSourceExpr :: Text
  -- ^ Source expression text (for debugging)
  }
  deriving stock (Eq, Show)

--------------------------------------------------------------------------------
-- Extraction
--------------------------------------------------------------------------------

-- | Extract an obligation graph from an L4 module.
--
-- This traverses the module, collects all deontic particles (Deontons),
-- identifies unique parties, and builds the obligation graph.
extractObligationGraph :: Module Resolved -> ObligationGraph
extractObligationGraph mod' =
  let deontons = collectDeontons mod'
      (parties, partyMap) = identifyParties deontons
      edges = mapMaybe (deontonToEdge partyMap) deontons
      recursions = detectRecursions mod'

      -- Update party degrees based on edges
      updatedParties = updatePartyDegrees parties edges
  in ObligationGraph
       { ogParties = updatedParties
       , ogObligations = edges
       , ogRecursions = recursions
       }

-- | Collect all Deonton nodes from a module using optics.
collectDeontons :: Module Resolved -> [Deonton Resolved]
collectDeontons = foldMapOf (gplate @(Deonton Resolved)) (: [])

-- | Identify unique parties from deontons.
--
-- Parties are identified by hashing their expression representation.
-- Returns the list of party nodes and a map from expression hash to party ID.
identifyParties :: [Deonton Resolved] -> ([PartyNode], Map Int Int)
identifyParties deontons =
  let -- Extract all party expressions
      partyExprs = nub $ map extractPartyExpr deontons

      -- Create party nodes with unique IDs
      nodes = zipWith mkPartyNode [0 ..] partyExprs

      -- Build hash-to-ID map
      hashMap = Map.fromList [(pn.pnExprHash, pn.pnId) | pn <- nodes]
  in (nodes, hashMap)
  where
    extractPartyExpr :: Deonton Resolved -> (Int, Text)
    extractPartyExpr d =
      let txt = exprToText d.party
          h = hash txt
      in (h, txt)

    mkPartyNode :: Int -> (Int, Text) -> PartyNode
    mkPartyNode nodeId (exprHash, exprText) =
      PartyNode
        { pnId = nodeId
        , pnExprHash = exprHash
        , pnExprText = exprText
        , pnOutDegree = 0
        , pnInDegree = 0
        , pnMayCount = 0
        , pnShantCount = 0
        }

-- | Convert a Deonton to an obligation edge.
deontonToEdge :: Map Int Int -> Deonton Resolved -> Maybe ObligationEdge
deontonToEdge partyMap d = do
  -- Get the obligor (party with the obligation)
  let partyText = exprToText d.party
      partyHash = hash partyText
  fromId <- Map.lookup partyHash partyMap

  -- For now, we assume a bilateral structure where the "other" party
  -- is the beneficiary. This is a simplification that works for most
  -- debt and exchange instruments.
  let toId = inferBeneficiary partyMap fromId

  -- Extract fields from RAction (pattern match to avoid NoFieldSelectors issues)
  let MkAction _ deonticModal actionPattern _ = d.action
      actionText = patternToText actionPattern
      valueType = inferValueType actionText

  -- Determine temporal pattern
  let timing = inferTemporalPattern d

  pure
    ObligationEdge
      { oeFrom = fromId
      , oeTo = toId
      , oeModal = deonticModal
      , oeValueType = valueType
      , oeTiming = timing
      , oeActionText = actionText
      , oeHasHence = d.hence /= Nothing
      , oeHasLest = d.lest /= Nothing
      }

-- | Infer the beneficiary party.
--
-- For bilateral contracts, this is simply the "other" party.
-- For multilateral contracts, this would need deeper analysis.
inferBeneficiary :: Map Int Int -> Int -> Int
inferBeneficiary partyMap fromId =
  -- Find any party that isn't the obligor
  let otherIds = [pid | pid <- Map.elems partyMap, pid /= fromId]
  in case otherIds of
       (pid : _) -> pid
       [] -> fromId -- Self-obligation (unusual but possible)

-- | Infer value type from action text.
inferValueType :: Text -> ValueType
inferValueType actionText =
  let lower = T.toLower actionText
  in if any (`T.isInfixOf` lower) moneyKeywords
       then MoneyValue
       else
         if any (`T.isInfixOf` lower) assetKeywords
           then AssetValue
           else
             if any (`T.isInfixOf` lower) serviceKeywords
               then ServiceValue
               else
                 if any (`T.isInfixOf` lower) infoKeywords
                   then InformationValue
                   else UnknownValue
  where
    moneyKeywords =
      ["pay", "currency", "money", "amount", "price", "fee", "premium"]
    assetKeywords = ["deliver", "transfer", "asset", "property", "goods"]
    serviceKeywords = ["perform", "execute", "provide", "render", "service"]
    infoKeywords = ["notify", "inform", "disclose", "report", "notice"]

-- | Infer temporal pattern from the deonton.
inferTemporalPattern :: Deonton Resolved -> TemporalPattern
inferTemporalPattern d =
  case d.hence of
    Just henceExpr ->
      if isRecursiveCall henceExpr
        then Recursive UntilExhaustion
        else
          if d.due /= Nothing
            then PointInTime
            else Unknown
    Nothing ->
      if d.due /= Nothing
        then PointInTime
        else Unknown

-- | Check if an expression is a recursive call (self-reference).
isRecursiveCall :: Expr Resolved -> Bool
isRecursiveCall expr = case expr of
  Regulative _ obl ->
    -- Check if the HENCE leads to another obligation (chained/recursive)
    obl.hence /= Nothing
  App {} -> True -- Function application could be recursive
  Where _ e _ -> isRecursiveCall e
  LetIn _ _ e -> isRecursiveCall e
  _ -> False

-- | Detect recursion patterns in the module.
detectRecursions :: Module Resolved -> [RecursionPattern]
detectRecursions mod' =
  let deontons = collectDeontons mod'
      recursives = filter hasRecursiveHence deontons
  in map analyzeRecursion recursives
  where
    hasRecursiveHence :: Deonton Resolved -> Bool
    hasRecursiveHence d = case d.hence of
      Just (Regulative _ _) -> True
      _ -> False

    analyzeRecursion :: Deonton Resolved -> RecursionPattern
    analyzeRecursion d =
      let MkAction _ _ actionPattern _ = d.action
      in RecursionPattern
           { rpDepth = 1 -- Would need deeper analysis for actual depth
           , rpDecay = UntilExhaustion -- Default assumption
           , rpTermination = Nothing -- Would need condition analysis
           , rpSourceExpr = exprToText d.party <> " -> " <> patternToText actionPattern
           }

-- | Update party degrees based on edges.
updatePartyDegrees :: [PartyNode] -> [ObligationEdge] -> [PartyNode]
updatePartyDegrees parties edges =
  map updateNode parties
  where
    -- Count outgoing obligations per party
    outCounts =
      Map.fromListWith (+) [(e.oeFrom, 1 :: Int) | e <- edges, e.oeModal `elem` [DMust, DDo]]
    -- Count incoming obligations per party
    inCounts = Map.fromListWith (+) [(e.oeTo, 1 :: Int) | e <- edges, e.oeModal `elem` [DMust, DDo]]
    -- Count MAY clauses per party
    mayCounts = Map.fromListWith (+) [(e.oeFrom, 1 :: Int) | e <- edges, e.oeModal == DMay]
    -- Count SHANT clauses per party
    shantCounts = Map.fromListWith (+) [(e.oeFrom, 1 :: Int) | e <- edges, e.oeModal == DMustNot]

    updateNode pn =
      pn
        { pnOutDegree = Map.findWithDefault 0 pn.pnId outCounts
        , pnInDegree = Map.findWithDefault 0 pn.pnId inCounts
        , pnMayCount = Map.findWithDefault 0 pn.pnId mayCounts
        , pnShantCount = Map.findWithDefault 0 pn.pnId shantCounts
        }

--------------------------------------------------------------------------------
-- Analysis Helpers
--------------------------------------------------------------------------------

-- | Get the number of parties in the graph.
partyCount :: ObligationGraph -> Int
partyCount og = length og.ogParties

-- | Check if the graph has any recursion patterns.
hasRecursion :: ObligationGraph -> Bool
hasRecursion og = not $ null og.ogRecursions

-- | Check if obligations flow in only one direction.
isUnidirectional :: ObligationGraph -> Bool
isUnidirectional og =
  let edges = og.ogObligations
      forwardPairs = [(e.oeFrom, e.oeTo) | e <- edges]
      reversePairs = [(e.oeTo, e.oeFrom) | e <- edges]
  in null [p | p <- forwardPairs, p `elem` reversePairs]

-- | Check if obligations flow in both directions (exchange/swap pattern).
isBidirectional :: ObligationGraph -> Bool
isBidirectional og = not $ isUnidirectional og

--------------------------------------------------------------------------------
-- Utility Functions
--------------------------------------------------------------------------------

-- | Convert a Resolved name to Text.
resolvedToText :: Resolved -> Text
resolvedToText = nameToText . getOriginal

-- | Convert an expression to a text representation (for hashing/display).
exprToText :: Expr Resolved -> Text
exprToText = \case
  App _ name [] -> resolvedToText name
  App _ name args -> resolvedToText name <> "(" <> T.intercalate ", " (map exprToText args) <> ")"
  Proj _ base field -> exprToText base <> "." <> resolvedToText field
  Lit _ lit -> T.pack $ show lit
  _ -> "<expr>"

-- | Convert a pattern to text.
patternToText :: Pattern Resolved -> Text
patternToText = \case
  PatVar _ n -> resolvedToText n
  PatApp _ n args -> resolvedToText n <> if null args then "" else " ..."
  PatCons _ h _ -> patternToText h <> " ..."
  PatExpr _ e -> exprToText e
  PatLit _ lit -> T.pack $ show lit
