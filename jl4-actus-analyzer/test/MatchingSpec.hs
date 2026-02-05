{-# LANGUAGE OverloadedRecordDot #-}

module MatchingSpec (spec) where

import qualified Data.Set as Set
import L4.ACTUS.FeatureExtractor
import L4.ACTUS.Matching.ACTUS
import L4.ACTUS.Matching.Rules
import L4.ACTUS.Matching.Scorer
import L4.ACTUS.Ontology.Types (ContainedType(..), Evidence(..))
import Test.Hspec

spec :: Spec
spec = do
  describe "FX Outright detection" $ do
    it "detects FX features correctly" $ do
      let features = mkFXFeatures
      isLikelyFXOutright features `shouldBe` True

    it "scores FX features highly" $ do
      let features = mkFXFeatures
          scores = computeConfidence defaultRules features
          fxScores = filter (\s -> s.matchActusType == "FXOUT") scores
      case fxScores of
        (s : _) -> s.matchConfidence `shouldSatisfy` (> 0.5)
        [] -> expectationFailure "Expected FXOUT match"

  describe "Swap detection" $ do
    it "detects swap features correctly" $ do
      let features = mkSwapFeatures
      isLikelySwap features `shouldBe` True

  describe "Option detection" $ do
    it "detects option features correctly" $ do
      let features = mkOptionFeatures
      isLikelyOption features `shouldBe` True

  describe "FIBO class mapping" $ do
    it "maps FXOUT to ForeignExchangeForwardAgreement" $ do
      let mClass = inferFIBOClass "FXOUT"
      mClass `shouldSatisfy` maybe False (const True)

    it "maps SWAPS to Swap" $ do
      let mClass = inferFIBOClass "SWAPS"
      mClass `shouldSatisfy` maybe False (const True)

    it "maps OPTNS to Option" $ do
      let mClass = inferFIBOClass "OPTNS"
      mClass `shouldSatisfy` maybe False (const True)

  describe "Confidence scoring" $ do
    it "normalizes scores to 0.0-1.0 range" $ do
      let features = mkFXFeatures
          scores = computeConfidence defaultRules features
      all (\s -> s.matchConfidence >= 0.0 && s.matchConfidence <= 1.0) scores
        `shouldBe` True

    it "aggregates scores correctly" $ do
      let scores =
            [ ScoredMatch "FXOUT" 0.8 []
            , ScoredMatch "SWAPS" 0.3 []
            ]
          aggregated = aggregateScores scores
      length aggregated `shouldBe` 2

    it "deduplicates same ACTUS type from multiple rules" $ do
      -- Simulates loan and bond rules both emitting PAM
      let ev1 = Evidence "Loan type" Nothing "Loan type -> PAM" 0.25
          ev2 = Evidence "Bond type" Nothing "Bond type -> PAM" 0.20
          scores =
            [ ScoredMatch "PAM" 0.5 [ev1]
            , ScoredMatch "PAM" 0.6 [ev2]
            , ScoredMatch "FXOUT" 0.8 []
            ]
          aggregated = aggregateScores scores
      -- Should have only 2 unique types, not 3
      length aggregated `shouldBe` 2
      -- PAM should have the best confidence (0.6) and merged evidence
      let pamMatches = filter (\s -> s.matchActusType == "PAM") aggregated
      case pamMatches of
        (pam : _) -> do
          pam.matchConfidence `shouldBe` 0.6
          length pam.matchEvidence `shouldBe` 2
        [] -> expectationFailure "Expected PAM match"

  describe "Primary classification selection" $ do
    it "selects highest confidence match" $ do
      let scores =
            [ ScoredMatch "FXOUT" 0.8 []
            , ScoredMatch "SWAPS" 0.5 []
            ]
      case selectPrimaryClassification 0.3 scores of
        Just primary -> primary.matchActusType `shouldBe` "FXOUT"
        Nothing -> expectationFailure "Expected primary classification"

    it "returns Nothing for low confidence matches" $ do
      let scores = [ScoredMatch "FXOUT" 0.2 []]
      selectPrimaryClassification 0.3 scores `shouldBe` Nothing

    it "honors custom minConfidence threshold" $ do
      let scores = [ScoredMatch "FXOUT" 0.4 []]
      -- With default 0.3 threshold, should match
      selectPrimaryClassification 0.3 scores `shouldSatisfy` maybe False (const True)
      -- With higher 0.5 threshold, should not match
      selectPrimaryClassification 0.5 scores `shouldBe` Nothing

  describe "Container type detection" $ do
    it "detects master agreement as container" $ do
      let scores =
            [ ScoredMatch "MasterAgreement" 0.6 []
            , ScoredMatch "FXOUT" 0.8 []
            ]
      case determineContainerType 0.3 scores of
        Just container -> container.matchActusType `shouldBe` "MasterAgreement"
        Nothing -> expectationFailure "Expected container type"

    it "honors custom minConfidence threshold for container detection" $ do
      let scores =
            [ ScoredMatch "MasterAgreement" 0.4 []
            , ScoredMatch "FXOUT" 0.8 []
            ]
      -- With 0.3 threshold, should detect container
      determineContainerType 0.3 scores `shouldSatisfy` maybe False (const True)
      -- With 0.5 threshold, should not detect container
      determineContainerType 0.5 scores `shouldBe` Nothing

    it "detects contained types" $ do
      let scores =
            [ ScoredMatch "MasterAgreement" 0.6 []
            , ScoredMatch "FXOUT" 0.8 []
            , ScoredMatch "SWAPS" 0.4 []
            ]
          contained = detectContainedTypes scores
      length contained `shouldBe` 2
      case contained of
        (first' : _) -> first'.containedActusType `shouldBe` "FXOUT"
        [] -> expectationFailure "Expected contained types"

-- Test data factories

mkFXFeatures :: L4Features
mkFXFeatures =
  L4Features
    { domainIndicators =
        [ CurrencyInd
            CurrencyIndicatorData
              { ciCurrencyNames = Set.fromList ["USD", "EUR", "GBP", "JPY"]
              , ciLocation = Nothing
              }
        , FXTransactionInd
            FXTransactionIndicatorData
              { fxHasTwoCurrencies = True
              , fxHasValueDate = True
              , fxLocation = Nothing
              }
        , SettlementInd
            SettlementIndicatorData
              { stHasNetting = True
              , stHasDelivery = True
              , stLocation = Nothing
              }
        , MasterAgreementInd
            MasterAgreementIndicatorData
              { maHasSchedule = True
              , maHasMultipleTransactionTypes = False
              , maHasCloseoutNetting = True
              , maLocation = Nothing
              }
        ]
    , deonticPatterns =
        [ DeonticPattern
            { modal = DMust
            , partyRole = Just "obligor"
            , actionType = Just "deliver"
            , hasConsequence = True
            , location = Nothing
            }
        ]
    , stateTransitions = []
    , temporalConstraints = []
    , partyStructure = Bilateral
    , typePatterns =
        [ TypePattern
            { patternName = "Currency"
            , patternKind = CurrencyEnumType
            , relevantFields = ["USD", "EUR", "GBP"]
            , location = Nothing
            }
        , TypePattern
            { patternName = "FXTransaction"
            , patternKind = TransactionRecordType
            , relevantFields = ["boughtCurrency", "soldCurrency"]
            , location = Nothing
            }
        ]
    , sourceFile = "test.l4"
    }

mkSwapFeatures :: L4Features
mkSwapFeatures =
  L4Features
    { domainIndicators =
        [ SwapInd
            SwapIndicatorData
              { swHasLegs = True
              , swHasNotional = True
              , swHasPaymentSchedule = True
              , swLocation = Nothing
              }
        ]
    , deonticPatterns = []
    , stateTransitions = []
    , temporalConstraints = []
    , partyStructure = Bilateral
    , typePatterns =
        [ TypePattern
            { patternName = "InterestRateSwap"
            , patternKind = TransactionRecordType
            , relevantFields = ["notional", "payLeg", "receiveLeg"]
            , location = Nothing
            }
        ]
    , sourceFile = "test.l4"
    }

mkOptionFeatures :: L4Features
mkOptionFeatures =
  L4Features
    { domainIndicators =
        [ OptionInd
            OptionIndicatorData
              { optHasStrike = True
              , optHasExpiry = True
              , optHasUnderlying = True
              , optLocation = Nothing
              }
        ]
    , deonticPatterns = []
    , stateTransitions = []
    , temporalConstraints = []
    , partyStructure = Bilateral
    , typePatterns =
        [ TypePattern
            { patternName = "CallOption"
            , patternKind = TransactionRecordType
            , relevantFields = ["strike", "expiry", "underlying"]
            , location = Nothing
            }
        ]
    , sourceFile = "test.l4"
    }
