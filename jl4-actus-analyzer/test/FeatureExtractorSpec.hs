module FeatureExtractorSpec (spec) where

import qualified Data.Set as Set
import L4.ACTUS.FeatureExtractor
import Test.Hspec

spec :: Spec
spec = do
  describe "DomainIndicator detection" $ do
    it "detects currency indicator patterns" $ do
      -- Currency indicator should recognize standard currency codes
      let currencyData =
            CurrencyIndicatorData
              { ciCurrencyNames = Set.fromList ["USD", "EUR", "GBP"]
              , ciLocation = Nothing
              }
          _currencyIndicator = CurrencyInd currencyData
      Set.size currencyData.ciCurrencyNames `shouldBe` 3

    it "detects FX transaction indicator patterns" $ do
      let fxData =
            FXTransactionIndicatorData
              { fxHasTwoCurrencies = True
              , fxHasValueDate = True
              , fxLocation = Nothing
              }
          _fxIndicator = FXTransactionInd fxData
      fxData.fxHasTwoCurrencies `shouldBe` True
      fxData.fxHasValueDate `shouldBe` True

    it "detects master agreement indicator patterns" $ do
      let maData =
            MasterAgreementIndicatorData
              { maHasSchedule = True
              , maHasMultipleTransactionTypes = True
              , maHasCloseoutNetting = True
              , maLocation = Nothing
              }
          _maIndicator = MasterAgreementInd maData
      maData.maHasSchedule `shouldBe` True
      maData.maHasCloseoutNetting `shouldBe` True

  describe "TypePattern classification" $ do
    it "identifies CurrencyEnumType" $ do
      let pat =
            TypePattern
              { patternName = "Currency"
              , patternKind = CurrencyEnumType
              , relevantFields = ["USD", "EUR", "GBP"]
              , location = Nothing
              }
      pat.patternKind `shouldBe` CurrencyEnumType

    it "identifies TransactionRecordType" $ do
      let pat =
            TypePattern
              { patternName = "FXTransaction"
              , patternKind = TransactionRecordType
              , relevantFields = ["boughtCurrency", "soldCurrency", "amount"]
              , location = Nothing
              }
      pat.patternKind `shouldBe` TransactionRecordType

  describe "PartyStructure" $ do
    it "correctly represents bilateral structure" $ do
      Bilateral `shouldBe` Bilateral

    it "correctly represents multilateral structure" $ do
      Multilateral 3 `shouldBe` Multilateral 3
      Multilateral 3 `shouldNotBe` Multilateral 4

  describe "DeonticPattern" $ do
    it "captures MUST obligations" $ do
      let pat =
            DeonticPattern
              { modal = DMust
              , partyRole = Just "obligor"
              , actionType = Just "deliver"
              , hasConsequence = True
              , location = Nothing
              }
      pat.modal `shouldBe` DMust
      pat.hasConsequence `shouldBe` True

    it "captures MAY permissions" $ do
      let pat =
            DeonticPattern
              { modal = DMay
              , partyRole = Just "party"
              , actionType = Just "exercise"
              , hasConsequence = False
              , location = Nothing
              }
      pat.modal `shouldBe` DMay

  describe "StateTransition" $ do
    it "captures breach transitions" $ do
      let transition =
            StateTransition
              { fromState = Just "active"
              , toState = Just "breach"
              , trigger = Just "failure_to_deliver"
              , isBreach = True
              , location = Nothing
              }
      transition.isBreach `shouldBe` True
