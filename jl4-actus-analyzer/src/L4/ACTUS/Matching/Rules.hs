-- | Pattern matching rules for ACTUS classification.
--
-- This module defines the rules that map L4 features to ACTUS contract types.
-- Rules are derived from the ontology definitions and coverage descriptions.
module L4.ACTUS.Matching.Rules (
  -- * Rule Types
  MatchingRule (..),
  FeatureMatcher (..),

  -- * Rule Construction
  buildRulesFromOntology,
  defaultRules,

  -- * Rule Application
  applyRules,
  matchFeature,
) where

import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import L4.ACTUS.FeatureExtractor
import L4.ACTUS.Ontology.Types

-- | A rule for matching L4 features to an ACTUS type.
data MatchingRule = MatchingRule
  { ruleActusType :: Text
  -- ^ The ACTUS type code this rule matches
  , ruleMatchers :: [FeatureMatcher]
  -- ^ Feature matchers with weights
  , ruleMinConfidence :: Double
  -- ^ Minimum total score to consider a match
  }
  deriving stock (Eq, Show)

-- | A feature matcher with a weight.
data FeatureMatcher = FeatureMatcher
  { matcherName :: Text
  -- ^ Description of what this matches
  , matcherWeight :: Double
  -- ^ Weight contribution (0.0 - 1.0)
  , matcherFunction :: L4Features -> Double
  -- ^ Function to compute match score (0.0 - 1.0)
  }

instance Eq FeatureMatcher where
  m1 == m2 = m1.matcherName == m2.matcherName && m1.matcherWeight == m2.matcherWeight

instance Show FeatureMatcher where
  show m = "FeatureMatcher " ++ T.unpack m.matcherName ++ " (" ++ show m.matcherWeight ++ ")"

-- | Build matching rules from the loaded ontology.
--
-- This extracts patterns from coverage descriptions and type definitions
-- to create matching rules.
buildRulesFromOntology :: OntologyGraph -> [MatchingRule]
buildRulesFromOntology _graph =
  -- For now, use default rules enhanced with ontology data
  -- Future: Parse coverage descriptions to generate rules dynamically
  defaultRules

-- | Default rules for common ACTUS types.
--
-- These are handcrafted rules based on ACTUS type definitions.
defaultRules :: [MatchingRule]
defaultRules =
  [ fxOutrightRule
  , swapRule
  , optionRule
  , loanRule
  , bondRule
  , futureRule
  , masterAgreementRule
  ]

-- | Rule for FXOUT (Foreign Exchange Outright).
fxOutrightRule :: MatchingRule
fxOutrightRule =
  MatchingRule
    { ruleActusType = "FXOUT"
    , ruleMatchers =
        [ FeatureMatcher "Currency types" 0.25 hasCurrencyTypes
        , FeatureMatcher "FX transaction type" 0.20 hasFXTransactionType
        , FeatureMatcher "Two currency exchange" 0.15 hasTwoCurrencyExchange
        , FeatureMatcher "Value date" 0.15 hasValueDateFeature
        , FeatureMatcher "Settlement provisions" 0.10 hasSettlementProvisions
        , FeatureMatcher "Bilateral parties" 0.10 hasBilateralParties
        , FeatureMatcher "Master agreement structure" 0.05 hasMasterAgreementStructure
        ]
    , ruleMinConfidence = 0.3
    }

-- | Rule for SWAPS (Plain Vanilla Swap).
swapRule :: MatchingRule
swapRule =
  MatchingRule
    { ruleActusType = "SWAPS"
    , ruleMatchers =
        [ FeatureMatcher "Swap type" 0.30 hasSwapType
        , FeatureMatcher "Payment legs" 0.20 hasPaymentLegs
        , FeatureMatcher "Notional amount" 0.15 hasNotionalAmount
        , FeatureMatcher "Payment schedule" 0.15 hasPaymentSchedule
        , FeatureMatcher "Bilateral parties" 0.10 hasBilateralParties
        , FeatureMatcher "Interest rate features" 0.10 hasInterestRateFeatures
        ]
    , ruleMinConfidence = 0.3
    }

-- | Rule for OPTNS (Options).
optionRule :: MatchingRule
optionRule =
  MatchingRule
    { ruleActusType = "OPTNS"
    , ruleMatchers =
        [ FeatureMatcher "Option type" 0.30 hasOptionType
        , FeatureMatcher "Strike price" 0.20 hasStrikePrice
        , FeatureMatcher "Expiration date" 0.20 hasExpirationDate
        , FeatureMatcher "Underlying asset" 0.15 hasUnderlyingAsset
        , FeatureMatcher "Premium" 0.15 hasPremiumFeature
        ]
    , ruleMinConfidence = 0.3
    }

-- | Rule for loans (PAM, ANN, LAM, NAM).
loanRule :: MatchingRule
loanRule =
  MatchingRule
    { ruleActusType = "PAM"
    , ruleMatchers =
        [ FeatureMatcher "Loan type" 0.25 hasLoanType
        , FeatureMatcher "Principal amount" 0.20 hasPrincipalAmount
        , FeatureMatcher "Interest rate" 0.20 hasInterestRate
        , FeatureMatcher "Repayment schedule" 0.15 hasRepaymentSchedule
        , FeatureMatcher "Maturity date" 0.10 hasMaturityDate
        , FeatureMatcher "Debtor/creditor structure" 0.10 hasDebtorCreditor
        ]
    , ruleMinConfidence = 0.3
    }

-- | Rule for bonds (similar to PAM but with coupon focus).
bondRule :: MatchingRule
bondRule =
  MatchingRule
    { ruleActusType = "PAM"
    , ruleMatchers =
        [ FeatureMatcher "Bond type" 0.25 hasBondType
        , FeatureMatcher "Face value" 0.20 hasFaceValue
        , FeatureMatcher "Coupon rate" 0.20 hasCouponRate
        , FeatureMatcher "Coupon schedule" 0.15 hasCouponSchedule
        , FeatureMatcher "Maturity date" 0.10 hasMaturityDate
        , FeatureMatcher "Issuer/holder structure" 0.10 hasIssuerHolder
        ]
    , ruleMinConfidence = 0.3
    }

-- | Rule for futures (FUTUR).
futureRule :: MatchingRule
futureRule =
  MatchingRule
    { ruleActusType = "FUTUR"
    , ruleMatchers =
        [ FeatureMatcher "Future type" 0.30 hasFutureType
        , FeatureMatcher "Contract size" 0.20 hasContractSize
        , FeatureMatcher "Settlement date" 0.20 hasSettlementDate
        , FeatureMatcher "Underlying asset" 0.15 hasUnderlyingAsset
        , FeatureMatcher "Margin requirements" 0.15 hasMarginRequirements
        ]
    , ruleMinConfidence = 0.3
    }

-- | Rule for master agreements (container type).
masterAgreementRule :: MatchingRule
masterAgreementRule =
  MatchingRule
    { ruleActusType = "MasterAgreement"
    , ruleMatchers =
        [ FeatureMatcher "Master agreement structure" 0.25 hasMasterAgreementStructure
        , FeatureMatcher "Multiple transaction types" 0.20 hasMultipleTransactionTypes
        , FeatureMatcher "Netting provisions" 0.20 hasNettingProvisions
        , FeatureMatcher "Close-out provisions" 0.15 hasCloseoutProvisions
        , FeatureMatcher "Bilateral parties" 0.10 hasBilateralParties
        , FeatureMatcher "Credit support" 0.10 hasCreditSupportFeatures
        ]
    , ruleMinConfidence = 0.3
    }

-- | Apply all rules to features and return scored matches.
applyRules :: [MatchingRule] -> L4Features -> [(Text, Double, [Evidence])]
applyRules rules features =
  mapMaybe (applyRule features) rules

-- | Apply a single rule and return result if it meets minimum confidence.
applyRule :: L4Features -> MatchingRule -> Maybe (Text, Double, [Evidence])
applyRule features rule =
  let (score, evidence) = scoreRule features rule
   in if score >= rule.ruleMinConfidence
        then Just (rule.ruleActusType, score, evidence)
        else Nothing

-- | Score a rule against features.
scoreRule :: L4Features -> MatchingRule -> (Double, [Evidence])
scoreRule features rule =
  let results = map (matchFeature features) rule.ruleMatchers
      totalScore = sum [w * s | (w, s, _) <- results]
      evidenceList =
        [ Evidence
          { evidenceFeature = name
          , evidenceLocation = Nothing
          , evidenceMappedTo = name <> " → " <> rule.ruleActusType
          , evidenceWeight = w * s
          }
        | (w, s, name) <- results
        , s > 0
        ]
   in (totalScore, evidenceList)

-- | Match a single feature and return (weight, score, name).
matchFeature :: L4Features -> FeatureMatcher -> (Double, Double, Text)
matchFeature features matcher =
  let score = matcher.matcherFunction features
   in (matcher.matcherWeight, score, matcher.matcherName)

-- Matcher functions

hasCurrencyTypes :: L4Features -> Double
hasCurrencyTypes features =
  let indicators = [i | i@(CurrencyInd _) <- features.domainIndicators]
   in if null indicators then 0.0 else 1.0

hasFXTransactionType :: L4Features -> Double
hasFXTransactionType features =
  let indicators = [i | i@(FXTransactionInd _) <- features.domainIndicators]
   in if null indicators then 0.0 else 1.0

hasTwoCurrencyExchange :: L4Features -> Double
hasTwoCurrencyExchange features =
  let indicators = [d | FXTransactionInd d <- features.domainIndicators, d.fxHasTwoCurrencies]
   in if null indicators then 0.0 else 1.0

hasValueDateFeature :: L4Features -> Double
hasValueDateFeature features =
  let indicators = [d | FXTransactionInd d <- features.domainIndicators, d.fxHasValueDate]
   in if null indicators then 0.0 else 1.0

hasSettlementProvisions :: L4Features -> Double
hasSettlementProvisions features =
  let indicators = [i | i@(SettlementInd _) <- features.domainIndicators]
   in if null indicators then 0.0 else 1.0

hasBilateralParties :: L4Features -> Double
hasBilateralParties features =
  case features.partyStructure of
    Bilateral -> 1.0
    _ -> 0.5 -- Partial credit for unknown

hasMasterAgreementStructure :: L4Features -> Double
hasMasterAgreementStructure features =
  let indicators = [i | i@(MasterAgreementInd _) <- features.domainIndicators]
   in if null indicators then 0.0 else 1.0

hasSwapType :: L4Features -> Double
hasSwapType features =
  let indicators = [i | i@(SwapInd _) <- features.domainIndicators]
   in if null indicators then 0.0 else 1.0

hasPaymentLegs :: L4Features -> Double
hasPaymentLegs features =
  let indicators = [d | SwapInd d <- features.domainIndicators, d.swHasLegs]
   in if null indicators then 0.0 else 1.0

hasNotionalAmount :: L4Features -> Double
hasNotionalAmount features =
  let indicators = [d | SwapInd d <- features.domainIndicators, d.swHasNotional]
   in if null indicators then 0.0 else 1.0

hasPaymentSchedule :: L4Features -> Double
hasPaymentSchedule features =
  let indicators = [d | SwapInd d <- features.domainIndicators, d.swHasPaymentSchedule]
   in if null indicators then 0.0 else 1.0

hasInterestRateFeatures :: L4Features -> Double
hasInterestRateFeatures features =
  let patterns = [p | p <- features.typePatterns, "rate" `T.isInfixOf` T.toLower p.patternName]
   in if null patterns then 0.0 else 1.0

hasOptionType :: L4Features -> Double
hasOptionType features =
  let indicators = [i | i@(OptionInd _) <- features.domainIndicators]
   in if null indicators then 0.0 else 1.0

hasStrikePrice :: L4Features -> Double
hasStrikePrice features =
  let indicators = [d | OptionInd d <- features.domainIndicators, d.optHasStrike]
   in if null indicators then 0.0 else 1.0

hasExpirationDate :: L4Features -> Double
hasExpirationDate features =
  let indicators = [d | OptionInd d <- features.domainIndicators, d.optHasExpiry]
   in if null indicators then 0.0 else 1.0

hasUnderlyingAsset :: L4Features -> Double
hasUnderlyingAsset features =
  let indicators = [d | OptionInd d <- features.domainIndicators, d.optHasUnderlying]
   in if null indicators then 0.0 else 1.0

hasPremiumFeature :: L4Features -> Double
hasPremiumFeature features =
  let patterns = [p | p <- features.typePatterns, "premium" `T.isInfixOf` T.toLower p.patternName]
   in if null patterns then 0.0 else 1.0

hasLoanType :: L4Features -> Double
hasLoanType features =
  let indicators = [i | i@(LoanInd _) <- features.domainIndicators]
   in if null indicators then 0.0 else 1.0

hasPrincipalAmount :: L4Features -> Double
hasPrincipalAmount features =
  let indicators = [d | LoanInd d <- features.domainIndicators, d.lnHasPrincipal]
   in if null indicators then 0.0 else 1.0

hasInterestRate :: L4Features -> Double
hasInterestRate features =
  let indicators = [d | LoanInd d <- features.domainIndicators, d.lnHasInterestRate]
   in if null indicators then 0.0 else 1.0

hasRepaymentSchedule :: L4Features -> Double
hasRepaymentSchedule features =
  let patterns = [p | p <- features.typePatterns, "repayment" `T.isInfixOf` T.toLower p.patternName]
   in if null patterns then 0.0 else 1.0

hasMaturityDate :: L4Features -> Double
hasMaturityDate features =
  let patterns = [p | p <- features.typePatterns, "maturity" `T.isInfixOf` T.toLower p.patternName]
   in if null patterns then 0.0 else 1.0

hasDebtorCreditor :: L4Features -> Double
hasDebtorCreditor features =
  let patterns =
        [ p
        | p <- features.typePatterns
        , "debtor" `T.isInfixOf` T.toLower p.patternName
            || "creditor" `T.isInfixOf` T.toLower p.patternName
        ]
   in if null patterns then 0.0 else 1.0

hasBondType :: L4Features -> Double
hasBondType features =
  let patterns = [p | p <- features.typePatterns, "bond" `T.isInfixOf` T.toLower p.patternName]
   in if null patterns then 0.0 else 1.0

hasFaceValue :: L4Features -> Double
hasFaceValue features =
  let patterns =
        [ p
        | p <- features.typePatterns
        , "face" `T.isInfixOf` T.toLower p.patternName
            || "par" `T.isInfixOf` T.toLower p.patternName
        ]
   in if null patterns then 0.0 else 1.0

hasCouponRate :: L4Features -> Double
hasCouponRate features =
  let patterns = [p | p <- features.typePatterns, "coupon" `T.isInfixOf` T.toLower p.patternName]
   in if null patterns then 0.0 else 1.0

hasCouponSchedule :: L4Features -> Double
hasCouponSchedule features =
  let patterns = [p | p <- features.typePatterns, "coupon" `T.isInfixOf` T.toLower p.patternName]
   in if null patterns then 0.0 else 1.0

hasIssuerHolder :: L4Features -> Double
hasIssuerHolder features =
  let patterns =
        [ p
        | p <- features.typePatterns
        , "issuer" `T.isInfixOf` T.toLower p.patternName
            || "holder" `T.isInfixOf` T.toLower p.patternName
        ]
   in if null patterns then 0.0 else 1.0

hasFutureType :: L4Features -> Double
hasFutureType features =
  let patterns = [p | p <- features.typePatterns, "future" `T.isInfixOf` T.toLower p.patternName]
   in if null patterns then 0.0 else 1.0

hasContractSize :: L4Features -> Double
hasContractSize features =
  let patterns =
        [ p
        | p <- features.typePatterns
        , "size" `T.isInfixOf` T.toLower p.patternName
            || "quantity" `T.isInfixOf` T.toLower p.patternName
        ]
   in if null patterns then 0.0 else 1.0

hasSettlementDate :: L4Features -> Double
hasSettlementDate features =
  let patterns = [p | p <- features.typePatterns, "settlement" `T.isInfixOf` T.toLower p.patternName]
   in if null patterns then 0.0 else 1.0

hasMarginRequirements :: L4Features -> Double
hasMarginRequirements features =
  let indicators = [d | CreditSupportInd d <- features.domainIndicators, d.csHasMargin]
   in if null indicators then 0.0 else 1.0

hasMultipleTransactionTypes :: L4Features -> Double
hasMultipleTransactionTypes features =
  let transactionPatterns =
        [ p
        | p <- features.typePatterns
        , p.patternKind == TransactionRecordType
        ]
   in if length transactionPatterns >= 2 then 1.0 else 0.5

hasNettingProvisions :: L4Features -> Double
hasNettingProvisions features =
  let indicators = [d | SettlementInd d <- features.domainIndicators, d.stHasNetting]
   in if null indicators then 0.0 else 1.0

hasCloseoutProvisions :: L4Features -> Double
hasCloseoutProvisions features =
  let indicators = [d | MasterAgreementInd d <- features.domainIndicators, d.maHasCloseoutNetting]
   in if null indicators then 0.0 else 1.0

hasCreditSupportFeatures :: L4Features -> Double
hasCreditSupportFeatures features =
  let indicators = [i | i@(CreditSupportInd _) <- features.domainIndicators]
   in if null indicators then 0.0 else 1.0
