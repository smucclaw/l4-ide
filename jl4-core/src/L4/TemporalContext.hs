{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module L4.TemporalContext
  ( TemporalContext (..)
  , EvalClause (..)
  , initialTemporalContext
  , applyEvalClauses
  )
where

import Base
import qualified Base.Text as Text
import Data.Time (Day, UTCTime (..), secondsToDiffTime)

-- | Multi-axis temporal context carried during evaluation.
-- Currently this is a lightweight container; evaluator wiring will
-- thread it through and update the fields as EVAL clauses are applied.
data TemporalContext = TemporalContext
  { tcValidTime :: !(Maybe Day)
  , tcSystemTime :: !UTCTime
  , tcRuleVersionTime :: !(Maybe Day)
  , tcRuleValidTime :: !(Maybe Day)
  , tcRuleEncodingTime :: !(Maybe UTCTime)
  , tcRuleCommit :: !(Maybe Text.Text)
  , tcDecisionTime :: !(Maybe UTCTime)
  , tcDocumentTimezone :: !(Maybe Text.Text)  -- ^ IANA timezone name (e.g. "Asia/Singapore")
  }
  deriving stock (Eq, Show, Generic)

-- | Clauses supported by the runtime EVAL construct.
data EvalClause
  = UnderValidTime Day
  | AsOfSystemTime UTCTime
  | UnderRulesEffectiveAt Day
  | UnderRulesEncodedAt UTCTime
  deriving stock (Eq, Show, Generic)

-- | Seed an initial temporal context using the wall-clock time for
-- system/decision defaults. Other axes start unset so callers can opt in.
initialTemporalContext :: UTCTime -> TemporalContext
initialTemporalContext now =
  TemporalContext
    { tcValidTime = Nothing
    , tcSystemTime = now
    , tcRuleVersionTime = Nothing
    , tcRuleValidTime = Nothing
    , tcRuleEncodingTime = Nothing
    , tcRuleCommit = Nothing
    , tcDecisionTime = Just now
    , tcDocumentTimezone = Nothing
    }

-- | Apply a list of clauses to a context, left-to-right.
-- This is pure so it can be reused by different evaluator frontends.
applyEvalClauses :: [EvalClause] -> TemporalContext -> TemporalContext
applyEvalClauses clauses ctx0 =
  foldl' applyClause ctx0 clauses
  where
    applyClause ctx = \case
      UnderValidTime d ->
        ctx { tcValidTime = Just d }
      AsOfSystemTime t ->
        ctx { tcSystemTime = t }
      UnderRulesEffectiveAt d ->
        ctx
          { tcRuleValidTime = Just d
          , tcRuleVersionTime = Just d
          , tcRuleEncodingTime =
              case ctx.tcRuleEncodingTime of
                Just t -> Just t
                Nothing -> Just (coerceDay d)
          }
        where
          -- default encoding snapshot to the target day when unspecified
          coerceDay day = UTCTime day (secondsToDiffTime 0)
      UnderRulesEncodedAt t ->
        ctx { tcRuleEncodingTime = Just t }
