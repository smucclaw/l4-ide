{-# LANGUAGE StrictData #-}

-- | ACTUS Archetype definitions and structural matching for qualia-based classification.
--
-- This module defines ACTUS contract types (PAM, ANN, FXOUT, SWAPS, OPTNS, etc.)
-- as structural archetypes rather than symbolic patterns. Classification is performed
-- by computing the structural distance between a contract's essence and each archetype.
--
-- Key insight: Instead of asking "does this contract mention 'borrower'?", we ask
-- "does this contract have asymmetric obligations with recursive decay?" — which is
-- the structural essence of a loan regardless of terminology.
module L4.ACTUS.Qualia.Archetypes (
  -- * Archetype Definition
  ACTUSArchetype (..),
  actusArchetypes,

  -- * Distance Computation
  DistanceWeights (..),
  defaultWeights,
  structuralDistance,
  scoreAgainstArchetype,

  -- * Classification
  classifyByEssence,
  bestMatch,

  -- * Individual Distance Functions
  symmetryDistance,
  topologyDistance,
  temporalDistance,
  breachDistance,
  partyCountDistance,
  recursionDistance,
  optionalityDistance,
) where

import Data.List (sortBy)
import Data.Maybe (isJust)
import Data.Ord (comparing)
import Data.Text (Text)
import L4.ACTUS.Qualia.Essence

--------------------------------------------------------------------------------
-- Archetype Definition
--------------------------------------------------------------------------------

-- | An ACTUS archetype defined by structural expectations.
--
-- Each archetype represents a class of financial contracts with
-- characteristic structural properties. Classification is performed
-- by measuring how closely a contract's essence matches these expectations.
data ACTUSArchetype = ACTUSArchetype
  { archActusCode :: Text
  -- ^ ACTUS code (e.g., "PAM", "ANN", "FXOUT")
  , archLabel :: Text
  -- ^ Human-readable name
  , archDescription :: Text
  -- ^ Brief description of the contract type
  , archExpectedSymmetry :: Symmetry
  -- ^ Expected party symmetry pattern
  , archExpectedTopology :: FlowTopology
  -- ^ Expected value flow direction
  , archExpectedTemporal :: TemporalShape
  -- ^ Expected temporal structure
  , archExpectedBreach :: BreachTopology
  -- ^ Expected breach handling
  , archPartyCount :: Int
  -- ^ Expected number of parties
  , archRequiresRecursion :: Bool
  -- ^ Must have recursive structure?
  , archMinOptionality :: Double
  -- ^ Minimum MAY degree (0.0 - 1.0)
  , archMaxOptionality :: Double
  -- ^ Maximum MAY degree (0.0 - 1.0)
  }
  deriving stock (Eq, Show)

-- | All ACTUS archetypes defined structurally.
--
-- These definitions capture the structural essence of each contract type
-- without relying on naming conventions or symbolic patterns.
actusArchetypes :: [ACTUSArchetype]
actusArchetypes =
  [ -- =========================================================================
    -- DEBT INSTRUMENTS
    -- =========================================================================

    -- Principal at Maturity (PAM)
    -- Simple bullet loan: borrow now, repay principal + interest at maturity
    ACTUSArchetype
      { archActusCode = "PAM"
      , archLabel = "Principal at Maturity"
      , archDescription = "Bullet loan with single principal repayment at maturity"
      , archExpectedSymmetry = FullyAsymmetric
      , archExpectedTopology = Unidirectional
      , archExpectedTemporal = PointToPoint
      , archExpectedBreach = SimpleBreach
      , archPartyCount = 2
      , archRequiresRecursion = False
      , archMinOptionality = 0.0
      , archMaxOptionality = 0.1
      }

    -- Annuity (ANN)
    -- Fixed periodic payments that include both principal and interest
  , ACTUSArchetype
      { archActusCode = "ANN"
      , archLabel = "Annuity"
      , archDescription = "Fixed periodic payments blending principal and interest"
      , archExpectedSymmetry = FullyAsymmetric
      , archExpectedTopology = Unidirectional
      , archExpectedTemporal = RecursiveDecay
      , archExpectedBreach = EscalationLadder 2
      , archPartyCount = 2
      , archRequiresRecursion = True
      , archMinOptionality = 0.0
      , archMaxOptionality = 0.1
      }

    -- Linear Amortizer (LAM)
    -- Linear principal reduction with interest on remaining balance
  , ACTUSArchetype
      { archActusCode = "LAM"
      , archLabel = "Linear Amortizer"
      , archDescription = "Linear principal reduction with diminishing interest"
      , archExpectedSymmetry = FullyAsymmetric
      , archExpectedTopology = Unidirectional
      , archExpectedTemporal = RecursiveDecay
      , archExpectedBreach = EscalationLadder 2
      , archPartyCount = 2
      , archRequiresRecursion = True
      , archMinOptionality = 0.0
      , archMaxOptionality = 0.1
      }

    -- Negative Amortizer (NAM)
    -- Payments less than interest, so principal grows
  , ACTUSArchetype
      { archActusCode = "NAM"
      , archLabel = "Negative Amortizer"
      , archDescription = "Payments below interest causing principal growth"
      , archExpectedSymmetry = FullyAsymmetric
      , archExpectedTopology = Unidirectional
      , archExpectedTemporal = PeriodicUntilTerm
      , archExpectedBreach = EscalationLadder 2
      , archPartyCount = 2
      , archRequiresRecursion = False
      , archMinOptionality = 0.0
      , archMaxOptionality = 0.1
      }

    -- =========================================================================
    -- EXCHANGE INSTRUMENTS
    -- =========================================================================

    -- Foreign Exchange Outright (FXOUT)
    -- Bilateral currency exchange at a single point
  , ACTUSArchetype
      { archActusCode = "FXOUT"
      , archLabel = "Foreign Exchange Outright"
      , archDescription = "Bilateral exchange of currencies at single settlement"
      , archExpectedSymmetry = Symmetric
      , archExpectedTopology = Bidirectional
      , archExpectedTemporal = PointToPoint
      , archExpectedBreach = SimpleBreach
      , archPartyCount = 2
      , archRequiresRecursion = False
      , archMinOptionality = 0.0
      , archMaxOptionality = 0.2
      }

    -- Swap (SWAPS)
    -- Periodic bilateral exchanges over a term
  , ACTUSArchetype
      { archActusCode = "SWAPS"
      , archLabel = "Swap"
      , archDescription = "Periodic bilateral payment exchanges"
      , archExpectedSymmetry = Symmetric
      , archExpectedTopology = Bidirectional
      , archExpectedTemporal = PeriodicUntilTerm
      , archExpectedBreach = SimpleBreach
      , archPartyCount = 2
      , archRequiresRecursion = False
      , archMinOptionality = 0.0
      , archMaxOptionality = 0.3
      }

    -- =========================================================================
    -- CONTINGENT INSTRUMENTS
    -- =========================================================================

    -- Option (OPTNS)
    -- One party has the right (not obligation) to exercise
  , ACTUSArchetype
      { archActusCode = "OPTNS"
      , archLabel = "Option"
      , archDescription = "Right but not obligation to buy/sell at strike price"
      , archExpectedSymmetry = AsymmetricWithRights
      , archExpectedTopology = Unidirectional
      , archExpectedTemporal = EventDriven
      , archExpectedBreach = NoBreachPath
      , archPartyCount = 2
      , archRequiresRecursion = False
      , archMinOptionality = 0.4
      , archMaxOptionality = 1.0
      }

    -- Future (FUTUR)
    -- Standardized forward with margin requirements
  , ACTUSArchetype
      { archActusCode = "FUTUR"
      , archLabel = "Future"
      , archDescription = "Standardized forward contract with daily settlement"
      , archExpectedSymmetry = Symmetric
      , archExpectedTopology = Bidirectional
      , archExpectedTemporal = PointToPoint
      , archExpectedBreach = SimpleBreach
      , archPartyCount = 2
      , archRequiresRecursion = False
      , archMinOptionality = 0.0
      , archMaxOptionality = 0.2
      }

    -- =========================================================================
    -- CREDIT INSTRUMENTS
    -- =========================================================================

    -- Credit Default Swap (CDSWP)
    -- Protection buyer pays premium, seller pays on credit event
  , ACTUSArchetype
      { archActusCode = "CDSWP"
      , archLabel = "Credit Default Swap"
      , archDescription = "Credit protection with premium payments and contingent payout"
      , archExpectedSymmetry = AsymmetricWithRights
      , archExpectedTopology = Unidirectional
      , archExpectedTemporal = EventDriven
      , archExpectedBreach = NoBreachPath
      , archPartyCount = 2
      , archRequiresRecursion = False
      , archMinOptionality = 0.3
      , archMaxOptionality = 0.7
      }

    -- Credit Enhancement Guarantee (CEG)
    -- Guarantor promises to pay if primary obligor defaults
  , ACTUSArchetype
      { archActusCode = "CEG"
      , archLabel = "Credit Enhancement Guarantee"
      , archDescription = "Third-party guarantee of primary obligation"
      , archExpectedSymmetry = AsymmetricWithRights
      , archExpectedTopology = Unidirectional
      , archExpectedTemporal = EventDriven
      , archExpectedBreach = NoBreachPath
      , archPartyCount = 2
      , archRequiresRecursion = False
      , archMinOptionality = 0.3
      , archMaxOptionality = 0.8
      }

    -- =========================================================================
    -- EQUITY INSTRUMENTS
    -- =========================================================================

    -- Stock (STK)
    -- Ownership share with dividend rights
  , ACTUSArchetype
      { archActusCode = "STK"
      , archLabel = "Stock"
      , archDescription = "Equity ownership with dividend and voting rights"
      , archExpectedSymmetry = AsymmetricWithRights
      , archExpectedTopology = Unidirectional
      , archExpectedTemporal = PeriodicIndefinite
      , archExpectedBreach = NoBreachPath
      , archPartyCount = 2
      , archRequiresRecursion = False
      , archMinOptionality = 0.5
      , archMaxOptionality = 1.0
      }

    -- =========================================================================
    -- MASTER AGREEMENTS
    -- =========================================================================

    -- Master Agreement (container type)
  , ACTUSArchetype
      { archActusCode = "MasterAgreement"
      , archLabel = "Master Agreement"
      , archDescription = "Framework agreement governing multiple transactions"
      , archExpectedSymmetry = Symmetric
      , archExpectedTopology = Bidirectional
      , archExpectedTemporal = PeriodicIndefinite
      , archExpectedBreach = EscalationLadder 3
      , archPartyCount = 2
      , archRequiresRecursion = False
      , archMinOptionality = 0.2
      , archMaxOptionality = 0.6
      }
  ]

--------------------------------------------------------------------------------
-- Distance Computation
--------------------------------------------------------------------------------

-- | Weights for each distance component.
--
-- These weights determine the relative importance of each structural
-- dimension in the overall distance calculation.
data DistanceWeights = DistanceWeights
  { wSymmetry :: Double
  -- ^ Weight for party symmetry (who owes whom)
  , wTopology :: Double
  -- ^ Weight for flow direction
  , wTemporal :: Double
  -- ^ Weight for time structure
  , wBreach :: Double
  -- ^ Weight for failure handling
  , wPartyCount :: Double
  -- ^ Weight for number of parties
  , wRecursion :: Double
  -- ^ Weight for self-reference
  , wOptionality :: Double
  -- ^ Weight for MAY vs MUST
  }
  deriving stock (Eq, Show)

-- | Default weights prioritizing the most discriminative features.
defaultWeights :: DistanceWeights
defaultWeights =
  DistanceWeights
    { wSymmetry = 0.25 -- Most important: debtor/creditor vs exchange
    , wTopology = 0.20 -- Direction of flows
    , wTemporal = 0.20 -- Time structure (crucial for PAM vs ANN)
    , wBreach = 0.10 -- Failure handling
    , wPartyCount = 0.10 -- Number of parties
    , wRecursion = 0.10 -- Self-reference (key for amortization)
    , wOptionality = 0.05 -- MAY vs MUST (key for options)
    }

-- | Compute structural distance between essence and archetype.
--
-- Lower distance = better match. Distance is normalized to [0, 1].
structuralDistance :: DistanceWeights -> ContractEssence -> ACTUSArchetype -> Double
structuralDistance weights essence archetype =
  let components =
        [ (weights.wSymmetry, symmetryDistance essence.ceSymmetry archetype.archExpectedSymmetry)
        , (weights.wTopology, topologyDistance essence.ceFlowTopology archetype.archExpectedTopology)
        , (weights.wTemporal, temporalDistance essence.ceTemporalShape archetype.archExpectedTemporal)
        , (weights.wBreach, breachDistance essence.ceBreachTopology archetype.archExpectedBreach)
        , (weights.wPartyCount, partyCountDistance essence.cePartyCount archetype.archPartyCount)
        , (weights.wRecursion, recursionDistance essence archetype)
        , (weights.wOptionality, optionalityDistance essence archetype)
        ]
      weightedSum = sum [w * d | (w, d) <- components]
      totalWeight = sum [w | (w, _) <- components]
  in if totalWeight > 0 then weightedSum / totalWeight else 0.0

-- | Score essence against archetype.
--
-- Returns (ACTUS code, confidence) where confidence is 1 - distance.
scoreAgainstArchetype :: DistanceWeights -> ContractEssence -> ACTUSArchetype -> (Text, Double)
scoreAgainstArchetype weights essence archetype =
  let distance = structuralDistance weights essence archetype
      confidence = max 0.0 (1.0 - distance)
  in (archetype.archActusCode, confidence)

--------------------------------------------------------------------------------
-- Classification
--------------------------------------------------------------------------------

-- | Classify contract by structural essence.
--
-- Returns all archetypes sorted by confidence (highest first).
classifyByEssence :: ContractEssence -> [(Text, Double)]
classifyByEssence essence =
  classifyByEssenceWithWeights defaultWeights essence

-- | Classify with custom weights.
classifyByEssenceWithWeights :: DistanceWeights -> ContractEssence -> [(Text, Double)]
classifyByEssenceWithWeights weights essence =
  sortBy (comparing (negate . snd)) $
    map (scoreAgainstArchetype weights essence) actusArchetypes

-- | Get the best matching archetype.
--
-- Returns Nothing if no archetype has confidence above the threshold.
bestMatch :: Double -> ContractEssence -> Maybe (Text, Double)
bestMatch minConfidence essence =
  case classifyByEssence essence of
    ((code, conf) : _) | conf >= minConfidence -> Just (code, conf)
    _ -> Nothing

--------------------------------------------------------------------------------
-- Individual Distance Functions
--------------------------------------------------------------------------------

-- | Distance between symmetry types.
--
-- Same symmetry = 0.0, opposite symmetry = 1.0
symmetryDistance :: Symmetry -> Symmetry -> Double
symmetryDistance s1 s2
  | s1 == s2 = 0.0
  | otherwise = case (s1, s2) of
      -- Close: both involve asymmetry
      (FullyAsymmetric, AsymmetricWithRights) -> 0.3
      (AsymmetricWithRights, FullyAsymmetric) -> 0.3
      -- Far: symmetric vs asymmetric
      (Symmetric, FullyAsymmetric) -> 1.0
      (FullyAsymmetric, Symmetric) -> 1.0
      (Symmetric, AsymmetricWithRights) -> 0.7
      (AsymmetricWithRights, Symmetric) -> 0.7
      -- Multi-party is special
      (MultiPartyHub, _) -> 0.6
      (_, MultiPartyHub) -> 0.6
      -- Default
      _ -> 0.5

-- | Distance between topology types.
topologyDistance :: FlowTopology -> FlowTopology -> Double
topologyDistance t1 t2
  | t1 == t2 = 0.0
  | otherwise = case (t1, t2) of
      -- Unidirectional vs Bidirectional is a key distinction
      (Unidirectional, Bidirectional) -> 0.8
      (Bidirectional, Unidirectional) -> 0.8
      -- Circular is complex
      (Circular, Unidirectional) -> 0.6
      (Unidirectional, Circular) -> 0.6
      (Circular, Bidirectional) -> 0.4
      (Bidirectional, Circular) -> 0.4
      -- Star pattern
      (StarPattern _, Bidirectional) -> 0.5
      (Bidirectional, StarPattern _) -> 0.5
      (StarPattern _, Unidirectional) -> 0.7
      (Unidirectional, StarPattern _) -> 0.7
      -- Star patterns with different sizes
      (StarPattern n1, StarPattern n2) -> min 1.0 (fromIntegral (abs (n1 - n2)) / 5.0)
      -- Default
      _ -> 0.5

-- | Distance between temporal shapes.
temporalDistance :: TemporalShape -> TemporalShape -> Double
temporalDistance ts1 ts2
  | ts1 == ts2 = 0.0
  | otherwise = case (ts1, ts2) of
      -- Point-to-point vs periodic
      (PointToPoint, PeriodicUntilTerm) -> 0.5
      (PeriodicUntilTerm, PointToPoint) -> 0.5
      (PointToPoint, PeriodicIndefinite) -> 0.6
      (PeriodicIndefinite, PointToPoint) -> 0.6
      -- Periodic types are related
      (PeriodicUntilTerm, PeriodicIndefinite) -> 0.3
      (PeriodicIndefinite, PeriodicUntilTerm) -> 0.3
      -- Recursive decay is similar to periodic
      (RecursiveDecay, PeriodicUntilTerm) -> 0.3
      (PeriodicUntilTerm, RecursiveDecay) -> 0.3
      (RecursiveDecay, PeriodicIndefinite) -> 0.4
      (PeriodicIndefinite, RecursiveDecay) -> 0.4
      -- Event-driven is different from others
      (EventDriven, PointToPoint) -> 0.5
      (PointToPoint, EventDriven) -> 0.5
      (EventDriven, PeriodicUntilTerm) -> 0.7
      (PeriodicUntilTerm, EventDriven) -> 0.7
      (EventDriven, RecursiveDecay) -> 0.8
      (RecursiveDecay, EventDriven) -> 0.8
      -- Default
      _ -> 0.5

-- | Distance between breach topologies.
breachDistance :: BreachTopology -> BreachTopology -> Double
breachDistance b1 b2
  | b1 == b2 = 0.0
  | otherwise = case (b1, b2) of
      -- Simple vs escalation
      (SimpleBreach, EscalationLadder _) -> 0.4
      (EscalationLadder _, SimpleBreach) -> 0.4
      -- Escalation ladders of different sizes
      (EscalationLadder n1, EscalationLadder n2) ->
        min 1.0 (fromIntegral (abs (n1 - n2)) / 3.0)
      -- Cure branch is between simple and escalation
      (CureBranch, SimpleBreach) -> 0.3
      (SimpleBreach, CureBranch) -> 0.3
      (CureBranch, EscalationLadder _) -> 0.3
      (EscalationLadder _, CureBranch) -> 0.3
      -- No breach path is very different
      (NoBreachPath, SimpleBreach) -> 0.7
      (SimpleBreach, NoBreachPath) -> 0.7
      (NoBreachPath, EscalationLadder _) -> 0.8
      (EscalationLadder _, NoBreachPath) -> 0.8
      (NoBreachPath, CureBranch) -> 0.6
      (CureBranch, NoBreachPath) -> 0.6
      -- Default
      _ -> 0.5

-- | Distance based on party count.
partyCountDistance :: Int -> Int -> Double
partyCountDistance actual expected
  | actual == expected = 0.0
  | otherwise = min 1.0 (fromIntegral (abs (actual - expected)) / 3.0)

-- | Distance based on recursion requirement.
recursionDistance :: ContractEssence -> ACTUSArchetype -> Double
recursionDistance essence archetype =
  let hasRecursion' = isJust essence.ceRecursionDepth
      requiresRecursion = archetype.archRequiresRecursion
  in case (hasRecursion', requiresRecursion) of
       (True, True) -> 0.0 -- Both recursive
       (False, False) -> 0.0 -- Both non-recursive
       (True, False) -> 0.3 -- Has recursion but not required (mild penalty)
       (False, True) -> 1.0 -- Missing required recursion (strong penalty)

-- | Distance based on optionality level.
optionalityDistance :: ContractEssence -> ACTUSArchetype -> Double
optionalityDistance essence archetype =
  let opt = essence.ceOptionalityDegree
      minOpt = archetype.archMinOptionality
      maxOpt = archetype.archMaxOptionality
  in if opt >= minOpt && opt <= maxOpt
       then 0.0
       else
         let belowMin = max 0 (minOpt - opt)
             aboveMax = max 0 (opt - maxOpt)
         in min 1.0 (belowMin + aboveMax)
