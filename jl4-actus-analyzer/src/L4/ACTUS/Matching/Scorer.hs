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
aggregateScores :: [ScoredMatch] -> [ScoredMatch]
aggregateScores matches =
  -- Group by ACTUS type and take the best score for each
  -- (In practice, we shouldn't have duplicates, but this handles edge cases)
  sortBy (comparing (negate . getConfidence)) matches
 where
  getConfidence m = m.matchConfidence

-- | Select the primary classification from scored matches.
--
-- Returns Nothing if no match exceeds the minimum threshold.
selectPrimaryClassification :: [ScoredMatch] -> Maybe ScoredMatch
selectPrimaryClassification matches =
  case sortBy (comparing (negate . getConfidence)) matches of
    (best : _) | best.matchConfidence >= 0.3 -> Just best
    _ -> Nothing
 where
  getConfidence m = m.matchConfidence

-- | Determine if this is a container type (master agreement).
--
-- A container wraps other contract types. If we detect master agreement
-- structure, we return it as the container and look for contained types.
determineContainerType :: [ScoredMatch] -> Maybe ScoredMatch
determineContainerType matches =
  let masterMatches = filter isMasterAgreement matches
   in case masterMatches of
        (m : _) | m.matchConfidence >= 0.4 -> Just m
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
