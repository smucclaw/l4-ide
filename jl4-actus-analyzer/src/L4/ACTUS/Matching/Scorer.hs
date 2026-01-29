-- | Confidence scoring and classification aggregation.
--
-- This module computes final confidence scores from rule matches
-- and determines the primary classification.
module L4.ACTUS.Matching.Scorer (
  -- * Scoring
  computeConfidence,
  aggregateScores,

  -- * Classification
  selectPrimaryClassification,
  determineContainerType,
  detectContainedTypes,

  -- * Score Types
  ScoredMatch (..),
) where

import Data.List (sortBy)
import qualified Data.Map.Strict as Map
import Data.Ord (comparing)
import Data.Text (Text)
import L4.ACTUS.FeatureExtractor
import L4.ACTUS.Matching.Rules
import L4.ACTUS.Ontology.Types

-- | A scored match with confidence and evidence.
data ScoredMatch = ScoredMatch
  { matchActusType :: Text
  , matchConfidence :: Double
  , matchEvidence :: [Evidence]
  }
  deriving stock (Eq, Show)

-- | Compute confidence scores for all applicable rules.
computeConfidence :: [MatchingRule] -> L4Features -> [ScoredMatch]
computeConfidence rules features =
  let rawMatches = applyRules rules features
   in map toScoredMatch rawMatches
 where
  toScoredMatch (actusType, score, evidence) =
    ScoredMatch
      { matchActusType = actusType
      , matchConfidence = normalizeScore score
      , matchEvidence = evidence
      }

-- | Normalize a raw score to 0.0 - 1.0 range.
normalizeScore :: Double -> Double
normalizeScore = max 0.0 . min 1.0

-- | Aggregate scores, combining evidence for the same ACTUS type.
--
-- Groups matches by ACTUS type and takes the best score for each,
-- merging evidence from all matches of the same type.
aggregateScores :: [ScoredMatch] -> [ScoredMatch]
aggregateScores matches =
  -- Group by ACTUS type and take the best score for each, merging evidence
  let grouped = Map.toList $ foldr groupByType Map.empty matches
      merged = map mergeGroup grouped
   in sortBy (comparing (negate . getConfidence)) merged
 where
  getConfidence m = m.matchConfidence

  -- Group matches by ACTUS type, keeping all matches for each type
  groupByType :: ScoredMatch -> Map.Map Text [ScoredMatch] -> Map.Map Text [ScoredMatch]
  groupByType m = Map.insertWith (++) m.matchActusType [m]

  -- Merge a group of matches: take best confidence, combine all evidence
  mergeGroup :: (Text, [ScoredMatch]) -> ScoredMatch
  mergeGroup (actusType, groupMatches) =
    let bestConf = maximum $ map getConfidence groupMatches
        allEvidence = concatMap (\m -> m.matchEvidence) groupMatches
     in ScoredMatch
          { matchActusType = actusType
          , matchConfidence = bestConf
          , matchEvidence = allEvidence
          }

-- | Select the primary classification from scored matches.
--
-- Returns Nothing if no match exceeds the minimum threshold.
-- The threshold should be passed from AnalyzerConfig.minConfidence.
selectPrimaryClassification ::
  -- | Minimum confidence threshold (from AnalyzerConfig.minConfidence)
  Double ->
  [ScoredMatch] ->
  Maybe ScoredMatch
selectPrimaryClassification minConfidence matches =
  case sortBy (comparing (negate . getConfidence)) matches of
    (best : _) | best.matchConfidence >= minConfidence -> Just best
    _ -> Nothing
 where
  getConfidence m = m.matchConfidence

-- | Determine if this is a container type (master agreement).
--
-- A container wraps other contract types. If we detect master agreement
-- structure, we return it as the container and look for contained types.
-- The threshold should be passed from AnalyzerConfig.minConfidence.
determineContainerType ::
  -- | Minimum confidence threshold (from AnalyzerConfig.minConfidence)
  Double ->
  [ScoredMatch] ->
  Maybe ScoredMatch
determineContainerType minConfidence matches =
  let masterMatches = filter isMasterAgreement matches
   in case masterMatches of
        (m : _) | m.matchConfidence >= minConfidence -> Just m
        _ -> Nothing
 where
  isMasterAgreement m = m.matchActusType == "MasterAgreement"

-- | Detect contained contract types within a master agreement.
--
-- These are transaction types that the master agreement governs.
detectContainedTypes :: [ScoredMatch] -> [ContainedType]
detectContainedTypes matches =
  let nonMaster = filter (not . isMasterAgreement) matches
      sorted = sortBy (comparing (negate . getConfidence)) nonMaster
   in map toContainedType sorted
 where
  isMasterAgreement m = m.matchActusType == "MasterAgreement"
  getConfidence m = m.matchConfidence
  toContainedType m =
    ContainedType
      { containedActusType = m.matchActusType
      , containedConfidence = m.matchConfidence
      , containedEvidence = m.matchEvidence
      }
