{-# LANGUAGE StrictData #-}

-- | Hybrid classification combining symbolic and qualia-based analysis.
--
-- This module integrates the qualia-based structural analysis with the
-- existing symbolic pattern matching to provide best-of-both-worlds
-- contract classification.
--
-- == Strategy Options
--
-- * 'SymbolicOnly' - Fast path using existing pattern matching
-- * 'QualiaOnly' - Pure structural analysis (name-agnostic)
-- * 'HybridSymbolicFirst' - Try symbolic, fall back to qualia if low confidence
-- * 'HybridQualiaFirst' - Try qualia, validate with symbolic
-- * 'Ensemble' - Run both, combine scores with configurable weights
--
-- == Typical Usage
--
-- @
-- config <- defaultHybridConfig
-- result <- classifyHybrid config features mod'
-- @
module L4.ACTUS.Qualia.Hybrid (
  -- * Strategy
  ClassificationStrategy (..),

  -- * Configuration
  HybridConfig (..),
  defaultHybridConfig,

  -- * Classification
  classifyHybrid,
  classifyQualia,
  classifySymbolic,

  -- * Result Combination
  selectBest,
  validateWithSymbolic,
  combineResults,

  -- * Evidence Generation
  generateQualiaEvidence,
) where

import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Text (Text)
import qualified Data.Text as T
import L4.ACTUS.FeatureExtractor (L4Features)
import L4.ACTUS.Matching.ACTUS (inferFIBOClass)
import L4.ACTUS.Matching.Rules (MatchingRule, applyRules, defaultRules)
import L4.ACTUS.Ontology.Types
  ( ClassificationResult (..)
  , Evidence (..)
  )
import L4.ACTUS.Qualia.Archetypes
  ( ACTUSArchetype (..)
  , classifyByEssence
  , actusArchetypes
  )
import L4.ACTUS.Qualia.Essence
  ( ContractEssence (..)
  , Symmetry (..)
  , FlowTopology (..)
  , TemporalShape (..)
  , BreachTopology (..)
  , extractEssence
  )
import L4.ACTUS.Qualia.ObligationGraph (extractObligationGraph)
import L4.StateGraph (StateGraph (..), extractStateGraphs)
import L4.Syntax (Module, Resolved)

--------------------------------------------------------------------------------
-- Strategy
--------------------------------------------------------------------------------

-- | Classification strategy determining how symbolic and qualia analyses
-- are combined.
data ClassificationStrategy
  = SymbolicOnly
  -- ^ Fast path: use existing pattern matching only
  | QualiaOnly
  -- ^ Pure structural analysis (name-agnostic)
  | HybridSymbolicFirst
  -- ^ Try symbolic first, fall back to qualia if confidence < threshold
  | HybridQualiaFirst
  -- ^ Try qualia first, validate/boost with symbolic agreement
  | Ensemble
  -- ^ Run both analyses, combine scores with weighted average
  deriving stock (Eq, Show)

--------------------------------------------------------------------------------
-- Configuration
--------------------------------------------------------------------------------

-- | Configuration for hybrid classification.
data HybridConfig = HybridConfig
  { hcStrategy :: ClassificationStrategy
  -- ^ Which combination strategy to use
  , hcSymbolicThreshold :: Double
  -- ^ Use symbolic result if confidence >= this (for HybridSymbolicFirst)
  , hcQualiaWeight :: Double
  -- ^ Weight for qualia score in ensemble (0.0 - 1.0)
  , hcSymbolicWeight :: Double
  -- ^ Weight for symbolic score in ensemble (0.0 - 1.0)
  , hcMinConfidence :: Double
  -- ^ Minimum confidence to report a classification
  , hcBoostOnAgreement :: Double
  -- ^ Confidence boost when both methods agree (multiplier)
  , hcPenaltyOnDisagreement :: Double
  -- ^ Confidence penalty when methods disagree (subtraction)
  , hcMatchingRules :: [MatchingRule]
  -- ^ Rules for symbolic matching
  }
  deriving stock (Show)

-- Eq instance ignoring matching rules (which may not have Eq)
instance Eq HybridConfig where
  c1 == c2 =
    c1.hcStrategy == c2.hcStrategy
      && c1.hcSymbolicThreshold == c2.hcSymbolicThreshold
      && c1.hcQualiaWeight == c2.hcQualiaWeight
      && c1.hcSymbolicWeight == c2.hcSymbolicWeight
      && c1.hcMinConfidence == c2.hcMinConfidence
      && c1.hcBoostOnAgreement == c2.hcBoostOnAgreement
      && c1.hcPenaltyOnDisagreement == c2.hcPenaltyOnDisagreement

-- | Default configuration favoring hybrid approach.
--
-- Uses HybridSymbolicFirst strategy: fast symbolic matching with
-- qualia fallback for low-confidence results.
defaultHybridConfig :: HybridConfig
defaultHybridConfig =
  HybridConfig
    { hcStrategy = HybridSymbolicFirst
    , hcSymbolicThreshold = 0.85 -- High bar for trusting symbolic alone
    , hcQualiaWeight = 0.6 -- Slightly favor structural analysis
    , hcSymbolicWeight = 0.4
    , hcMinConfidence = 0.3 -- Report anything above 30%
    , hcBoostOnAgreement = 1.1 -- 10% boost when methods agree
    , hcPenaltyOnDisagreement = 0.05 -- 5% penalty when they disagree
    , hcMatchingRules = defaultRules
    }

--------------------------------------------------------------------------------
-- Classification
--------------------------------------------------------------------------------

-- | Intermediate result before final aggregation
data IntermediateResult = IntermediateResult
  { irActusType :: Maybe Text
  , irConfidence :: Double
  , irEvidence :: [Evidence]
  , irMethod :: Text
  }
  deriving stock (Eq, Show)

-- | Run hybrid classification using the configured strategy.
classifyHybrid ::
  HybridConfig ->
  L4Features ->
  Module Resolved ->
  ClassificationResult
classifyHybrid config features mod' =
  case config.hcStrategy of
    SymbolicOnly ->
      toClassificationResult $ classifySymbolicInternal config features

    QualiaOnly ->
      toClassificationResult $ classifyQualiaInternal mod'

    HybridSymbolicFirst ->
      let symbolic = classifySymbolicInternal config features
       in if symbolic.irConfidence >= config.hcSymbolicThreshold
            then toClassificationResult symbolic
            else
              let qualia = classifyQualiaInternal mod'
               in toClassificationResult $ selectBestInternal symbolic qualia

    HybridQualiaFirst ->
      let qualia = classifyQualiaInternal mod'
          symbolic = classifySymbolicInternal config features
       in toClassificationResult $ validateWithSymbolicInternal config qualia symbolic

    Ensemble ->
      let symbolic = classifySymbolicInternal config features
          qualia = classifyQualiaInternal mod'
       in toClassificationResult $ combineResultsInternal config symbolic qualia

-- | Pure qualia-based classification.
--
-- Extracts structural essence and matches against ACTUS archetypes.
-- This is name-agnostic and works even with obfuscated variable names.
classifyQualia :: Module Resolved -> ClassificationResult
classifyQualia = toClassificationResult . classifyQualiaInternal

-- | Internal qualia classification returning intermediate result.
-- | Empty state graph for when no DECIDE rules are found.
emptyStateGraph :: StateGraph
emptyStateGraph =
  StateGraph
    { sgName = "empty"
    , sgStates = []
    , sgTransitions = []
    , sgInitialState = 0
    }

classifyQualiaInternal :: Module Resolved -> IntermediateResult
classifyQualiaInternal mod' =
  let og = extractObligationGraph mod'
      sgs = extractStateGraphs mod'
      -- Use first state graph if available, otherwise create empty one
      sg = case sgs of
        (g : _) -> g
        [] -> emptyStateGraph
      essence = extractEssence og sg
      scores = classifyByEssence essence
      evidence = generateQualiaEvidence essence
   in case scores of
        ((bestType, bestConf) : _) ->
          IntermediateResult
            { irActusType = Just bestType
            , irConfidence = bestConf
            , irEvidence = evidence
            , irMethod = "qualia"
            }
        [] ->
          IntermediateResult
            { irActusType = Nothing
            , irConfidence = 0.0
            , irEvidence = evidence
            , irMethod = "qualia"
            }

-- | Pure symbolic classification.
--
-- Uses pattern matching on type names, field names, and keywords.
-- Fast but requires standard naming conventions.
classifySymbolic :: HybridConfig -> L4Features -> ClassificationResult
classifySymbolic config = toClassificationResult . classifySymbolicInternal config

-- | Internal symbolic classification.
classifySymbolicInternal :: HybridConfig -> L4Features -> IntermediateResult
classifySymbolicInternal config features =
  let rules = config.hcMatchingRules
      matches = applyRules rules features
      sorted = sortBy (comparing (\(_, s, _) -> negate s)) matches
   in case sorted of
        ((actusType, score, evidence) : _) ->
          IntermediateResult
            { irActusType = Just actusType
            , irConfidence = min 1.0 (max 0.0 score)
            , irEvidence = evidence
            , irMethod = "symbolic"
            }
        [] ->
          IntermediateResult
            { irActusType = Nothing
            , irConfidence = 0.0
            , irEvidence = []
            , irMethod = "symbolic"
            }

--------------------------------------------------------------------------------
-- Result Combination
--------------------------------------------------------------------------------

-- | Select the best result between symbolic and qualia.
--
-- Simply returns the result with higher confidence.
selectBest :: IntermediateResult -> IntermediateResult -> IntermediateResult
selectBest = selectBestInternal

selectBestInternal :: IntermediateResult -> IntermediateResult -> IntermediateResult
selectBestInternal ir1 ir2
  | ir1.irConfidence >= ir2.irConfidence = ir1
  | otherwise = ir2

-- | Validate qualia result using symbolic analysis.
--
-- If both methods agree on the type, boost confidence.
-- If they disagree, add a note but keep the qualia result.
validateWithSymbolic ::
  HybridConfig ->
  IntermediateResult ->
  IntermediateResult ->
  IntermediateResult
validateWithSymbolic = validateWithSymbolicInternal

validateWithSymbolicInternal ::
  HybridConfig ->
  IntermediateResult ->
  IntermediateResult ->
  IntermediateResult
validateWithSymbolicInternal config qualia symbolic =
  case (qualia.irActusType, symbolic.irActusType) of
    (Just qt, Just st) | qt == st ->
      -- Agreement: boost confidence
      qualia
        { irConfidence = min 1.0 (qualia.irConfidence * config.hcBoostOnAgreement)
        , irEvidence = qualia.irEvidence ++ [agreementNote st symbolic.irConfidence]
        , irMethod = "qualia-validated"
        }
    (Just _, Just st) ->
      -- Disagreement: note it but keep qualia
      qualia
        { irConfidence = max 0.0 (qualia.irConfidence - config.hcPenaltyOnDisagreement)
        , irEvidence = qualia.irEvidence ++ [disagreementNote st symbolic.irConfidence]
        , irMethod = "qualia-unvalidated"
        }
    _ ->
      -- One or both have no result
      qualia

-- | Combine results using weighted ensemble.
--
-- The final type is chosen based on which method has higher confidence,
-- but the final confidence is a weighted average of both scores.
combineResults ::
  HybridConfig ->
  IntermediateResult ->
  IntermediateResult ->
  IntermediateResult
combineResults = combineResultsInternal

combineResultsInternal ::
  HybridConfig ->
  IntermediateResult ->
  IntermediateResult ->
  IntermediateResult
combineResultsInternal config symbolic qualia =
  let wSym = config.hcSymbolicWeight
      wQual = config.hcQualiaWeight
      totalWeight = wSym + wQual
      combinedConf =
        if totalWeight > 0
          then (wSym * symbolic.irConfidence + wQual * qualia.irConfidence) / totalWeight
          else 0.0
      -- Choose type from higher-confidence method
      (bestType, method) =
        if qualia.irConfidence >= symbolic.irConfidence
          then (qualia.irActusType, "ensemble-qualia")
          else (symbolic.irActusType, "ensemble-symbolic")
      -- Combine evidence
      allEvidence = qualia.irEvidence ++ symbolic.irEvidence
   in IntermediateResult
        { irActusType = bestType
        , irConfidence = combinedConf
        , irEvidence = allEvidence
        , irMethod = method
        }

--------------------------------------------------------------------------------
-- Evidence Generation
--------------------------------------------------------------------------------

-- | Generate human-readable evidence from contract essence.
--
-- Creates evidence entries for each structural dimension analyzed.
generateQualiaEvidence :: ContractEssence -> [Evidence]
generateQualiaEvidence essence =
  [ Evidence
      { evidenceFeature = "Party structure"
      , evidenceLocation = Nothing
      , evidenceMappedTo = describeSymmetry essence.ceSymmetry
      , evidenceWeight = 0.25
      }
  , Evidence
      { evidenceFeature = "Flow topology"
      , evidenceLocation = Nothing
      , evidenceMappedTo = describeTopology essence.ceFlowTopology
      , evidenceWeight = 0.20
      }
  , Evidence
      { evidenceFeature = "Temporal shape"
      , evidenceLocation = Nothing
      , evidenceMappedTo = describeTemporal essence.ceTemporalShape
      , evidenceWeight = 0.20
      }
  , Evidence
      { evidenceFeature = "Breach handling"
      , evidenceLocation = Nothing
      , evidenceMappedTo = describeBreach essence.ceBreachTopology
      , evidenceWeight = 0.10
      }
  , Evidence
      { evidenceFeature = "Party count"
      , evidenceLocation = Nothing
      , evidenceMappedTo = T.pack (show essence.cePartyCount) <> " parties"
      , evidenceWeight = 0.10
      }
  , Evidence
      { evidenceFeature = "Optionality"
      , evidenceLocation = Nothing
      , evidenceMappedTo = describeOptionality essence.ceOptionalityDegree
      , evidenceWeight = 0.05
      }
  ]
    ++ recursionEvidence essence

-- | Generate recursion evidence if present.
recursionEvidence :: ContractEssence -> [Evidence]
recursionEvidence essence =
  case essence.ceRecursionDepth of
    Nothing -> []
    Just depth ->
      [ Evidence
          { evidenceFeature = "Recursion"
          , evidenceLocation = Nothing
          , evidenceMappedTo = "Self-referential at depth " <> T.pack (show depth)
          , evidenceWeight = 0.10
          }
      ]

-- | Agreement note for evidence.
agreementNote :: Text -> Double -> Evidence
agreementNote symType symConf =
  Evidence
    { evidenceFeature = "Symbolic agreement"
    , evidenceLocation = Nothing
    , evidenceMappedTo =
        "Symbolic analysis also suggests " <> symType
          <> " (confidence: "
          <> T.pack (show (round (symConf * 100) :: Int))
          <> "%)"
    , evidenceWeight = 0.05
    }

-- | Disagreement note for evidence.
disagreementNote :: Text -> Double -> Evidence
disagreementNote symType symConf =
  Evidence
    { evidenceFeature = "Symbolic disagreement"
    , evidenceLocation = Nothing
    , evidenceMappedTo =
        "Symbolic analysis suggests " <> symType
          <> " (confidence: "
          <> T.pack (show (round (symConf * 100) :: Int))
          <> "%)"
    , evidenceWeight = -0.05
    }

--------------------------------------------------------------------------------
-- Description Helpers
--------------------------------------------------------------------------------

describeSymmetry :: Symmetry -> Text
describeSymmetry = \case
  FullyAsymmetric -> "Fully asymmetric (debtor/creditor pattern)"
  AsymmetricWithRights -> "Asymmetric with rights (option-like)"
  Symmetric -> "Symmetric (balanced mutual obligations)"
  MultiPartyHub -> "Multi-party hub (central counterparty)"

describeTopology :: FlowTopology -> Text
describeTopology = \case
  Unidirectional -> "Unidirectional (one-way value flow)"
  Bidirectional -> "Bidirectional (exchange pattern)"
  Circular -> "Circular (complex netting)"
  StarPattern n -> "Star pattern (" <> T.pack (show n) <> " parties)"

describeTemporal :: TemporalShape -> Text
describeTemporal = \case
  PointToPoint -> "Point-to-point (single event)"
  PeriodicUntilTerm -> "Periodic until maturity"
  PeriodicIndefinite -> "Perpetual periodic"
  RecursiveDecay -> "Recursive decay (amortization)"
  EventDriven -> "Event-driven (contingent)"

describeBreach :: BreachTopology -> Text
describeBreach = \case
  SimpleBreach -> "Simple breach (direct terminal)"
  EscalationLadder n -> T.pack (show n) <> "-step escalation"
  CureBranch -> "Cure opportunity before breach"
  NoBreachPath -> "No breach path (fully discretionary)"

describeOptionality :: Double -> Text
describeOptionality opt
  | opt < 0.1 = "Very low (mostly MUST obligations)"
  | opt < 0.3 = "Low (some discretionary elements)"
  | opt < 0.6 = "Moderate (mixed MUST/MAY)"
  | opt < 0.9 = "High (mostly discretionary)"
  | otherwise = "Very high (almost fully optional)"

--------------------------------------------------------------------------------
-- Conversion to Final Result
--------------------------------------------------------------------------------

-- | Convert intermediate result to final ClassificationResult.
toClassificationResult :: IntermediateResult -> ClassificationResult
toClassificationResult ir =
  ClassificationResult
    { sourceFiles = []
    , containerType = Nothing
    , containerFiboClass = Nothing
    , primaryActusType = ir.irActusType
    , primaryActusLabel = ir.irActusType >>= lookupArchetypeLabel
    , primaryConfidence = ir.irConfidence
    , primaryFiboClass = ir.irActusType >>= inferFIBOClass
    , containedTypes = []
    , evidence = ir.irEvidence
    , analyzedAt = ""
    }

-- | Look up label from archetypes.
lookupArchetypeLabel :: Text -> Maybe Text
lookupArchetypeLabel code =
  case filter (\a -> a.archActusCode == code) actusArchetypes of
    (arch : _) -> Just arch.archLabel
    [] -> Nothing
