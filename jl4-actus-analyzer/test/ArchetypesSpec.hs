module ArchetypesSpec (spec) where

import Control.Monad (forM_)
import qualified Data.Text as T
import L4.ACTUS.Qualia.Archetypes
import L4.ACTUS.Qualia.Essence
import Test.Hspec

spec :: Spec
spec = describe "Archetypes" $ do
  describe "Distance functions" $ do
    describe "symmetryDistance" $ do
      it "returns 0.0 for same symmetry" $ do
        symmetryDistance FullyAsymmetric FullyAsymmetric `shouldBe` 0.0
        symmetryDistance Symmetric Symmetric `shouldBe` 0.0

      it "returns low distance for related asymmetric types" $ do
        symmetryDistance FullyAsymmetric AsymmetricWithRights `shouldBe` 0.3

      it "returns high distance for opposite types" $ do
        symmetryDistance Symmetric FullyAsymmetric `shouldBe` 1.0
        symmetryDistance FullyAsymmetric Symmetric `shouldBe` 1.0

    describe "topologyDistance" $ do
      it "returns 0.0 for same topology" $ do
        topologyDistance Unidirectional Unidirectional `shouldBe` 0.0
        topologyDistance Bidirectional Bidirectional `shouldBe` 0.0

      it "returns high distance for opposite flow directions" $ do
        topologyDistance Unidirectional Bidirectional `shouldBe` 0.8

      it "handles star patterns" $ do
        topologyDistance (StarPattern 3) (StarPattern 3) `shouldBe` 0.0
        let d = topologyDistance (StarPattern 3) (StarPattern 5)
        d `shouldSatisfy` (> 0.0)
        d `shouldSatisfy` (< 1.0)

    describe "temporalDistance" $ do
      it "returns 0.0 for same shape" $ do
        temporalDistance PointToPoint PointToPoint `shouldBe` 0.0
        temporalDistance RecursiveDecay RecursiveDecay `shouldBe` 0.0

      it "returns low distance for related periodic types" $ do
        temporalDistance PeriodicUntilTerm PeriodicIndefinite `shouldBe` 0.3

      it "returns high distance for event-driven vs recursive" $ do
        temporalDistance EventDriven RecursiveDecay `shouldBe` 0.8

    describe "breachDistance" $ do
      it "returns 0.0 for same breach type" $ do
        breachDistance SimpleBreach SimpleBreach `shouldBe` 0.0
        breachDistance NoBreachPath NoBreachPath `shouldBe` 0.0

      it "handles escalation ladders" $ do
        breachDistance (EscalationLadder 2) (EscalationLadder 2) `shouldBe` 0.0
        let d = breachDistance (EscalationLadder 2) (EscalationLadder 4)
        d `shouldSatisfy` (> 0.0)
        d `shouldSatisfy` (<= 1.0)

      it "returns high distance for no breach vs breach" $ do
        breachDistance NoBreachPath SimpleBreach `shouldBe` 0.7
        breachDistance NoBreachPath (EscalationLadder 2) `shouldBe` 0.8

    describe "partyCountDistance" $ do
      it "returns 0.0 for same count" $ do
        partyCountDistance 2 2 `shouldBe` 0.0
        partyCountDistance 5 5 `shouldBe` 0.0

      it "scales with difference" $ do
        partyCountDistance 2 3 `shouldSatisfy` (> 0.0)
        partyCountDistance 2 5 `shouldSatisfy` (>= partyCountDistance 2 3)

      it "caps at 1.0" $ do
        partyCountDistance 1 100 `shouldBe` 1.0

    describe "recursionDistance" $ do
      it "returns 0.0 when both recursive" $ do
        let essence = mockEssenceWithRecursion (Just 2)
            arch = mockArchetypeWithRecursion True
        recursionDistance essence arch `shouldBe` 0.0

      it "returns 0.0 when both non-recursive" $ do
        let essence = mockEssenceWithRecursion Nothing
            arch = mockArchetypeWithRecursion False
        recursionDistance essence arch `shouldBe` 0.0

      it "penalizes missing required recursion" $ do
        let essence = mockEssenceWithRecursion Nothing
            arch = mockArchetypeWithRecursion True
        recursionDistance essence arch `shouldBe` 1.0

      it "mildly penalizes extra recursion" $ do
        let essence = mockEssenceWithRecursion (Just 1)
            arch = mockArchetypeWithRecursion False
        recursionDistance essence arch `shouldBe` 0.3

    describe "optionalityDistance" $ do
      it "returns 0.0 when within range" $ do
        let essence = mockEssenceWithOptionality 0.5
            arch = mockArchetypeWithOptionality 0.3 0.7
        optionalityDistance essence arch `shouldBe` 0.0

      it "penalizes below minimum" $ do
        let essence = mockEssenceWithOptionality 0.1
            arch = mockArchetypeWithOptionality 0.4 0.8
        optionalityDistance essence arch `shouldSatisfy` (> 0.0)

      it "penalizes above maximum" $ do
        let essence = mockEssenceWithOptionality 0.9
            arch = mockArchetypeWithOptionality 0.0 0.3
        optionalityDistance essence arch `shouldSatisfy` (> 0.0)

  describe "Classification" $ do
    it "classifies debt pattern as PAM or similar" $ do
      let essence = mockDebtEssence
          results = classifyByEssence essence
      -- Should match a debt instrument
      case results of
        ((topCode, topConf) : _) -> do
          topConf `shouldSatisfy` (> 0.5)
          -- Top result should be a debt-related type
          topCode `shouldSatisfy` (`elem` ["PAM", "ANN", "LAM", "NAM"])
        [] -> expectationFailure "Expected non-empty results"

    it "classifies exchange pattern as FXOUT or similar" $ do
      let essence = mockExchangeEssence
          results = classifyByEssence essence
      case results of
        ((topCode, topConf) : _) -> do
          topConf `shouldSatisfy` (> 0.5)
          -- Top result should be an exchange-related type
          topCode `shouldSatisfy` (`elem` ["FXOUT", "FUTUR", "SWAPS"])
        [] -> expectationFailure "Expected non-empty results"

    it "classifies option pattern as OPTNS" $ do
      let essence = mockOptionEssence
          results = classifyByEssence essence
      case results of
        ((topCode, topConf) : _) -> do
          topConf `shouldSatisfy` (> 0.4)
          -- Top result should be option-like
          topCode `shouldSatisfy` (`elem` ["OPTNS", "CDSWP", "CEG", "STK"])
        [] -> expectationFailure "Expected non-empty results"

    it "classifies amortizing pattern as ANN or LAM" $ do
      let essence = mockAmortizingEssence
          results = classifyByEssence essence
      case results of
        ((topCode, topConf) : _) -> do
          topConf `shouldSatisfy` (> 0.5)
          -- Top result should be amortizing type
          topCode `shouldSatisfy` (`elem` ["ANN", "LAM"])
        [] -> expectationFailure "Expected non-empty results"

  describe "bestMatch" $ do
    it "returns best match above threshold" $ do
      let essence = mockDebtEssence
          result = bestMatch 0.5 essence
      result `shouldSatisfy` (/= Nothing)

    it "returns Nothing below threshold" $ do
      -- Use a weird essence that doesn't match anything well
      let weirdEssence = baseEssence
            { ceSymmetry = MultiPartyHub -- unusual
            , ceFlowTopology = Circular -- unusual
            , ceTemporalShape = PeriodicIndefinite
            , ceBreachTopology = CureBranch
            , cePartyCount = 7 -- unusual
            }
          result = bestMatch 0.95 weirdEssence -- High threshold
      result `shouldBe` Nothing

  describe "actusArchetypes" $ do
    it "contains expected archetypes" $ do
      let codes = map (\a -> a.archActusCode) actusArchetypes
      codes `shouldSatisfy` ("PAM" `elem`)
      codes `shouldSatisfy` ("ANN" `elem`)
      codes `shouldSatisfy` ("FXOUT" `elem`)
      codes `shouldSatisfy` ("SWAPS" `elem`)
      codes `shouldSatisfy` ("OPTNS" `elem`)

    it "all archetypes have valid structure" $ do
      forM_ actusArchetypes $ \arch -> do
        T.length arch.archActusCode `shouldSatisfy` (> 0)
        T.length arch.archLabel `shouldSatisfy` (> 0)
        arch.archPartyCount `shouldSatisfy` (>= 2)
        arch.archMinOptionality `shouldSatisfy` (>= 0.0)
        arch.archMaxOptionality `shouldSatisfy` (<= 1.0)
        arch.archMinOptionality `shouldSatisfy` (<= arch.archMaxOptionality)

  describe "defaultWeights" $ do
    it "sums to approximately 1.0" $ do
      let w = defaultWeights
          total = w.wSymmetry + w.wTopology + w.wTemporal + w.wBreach
                + w.wPartyCount + w.wRecursion + w.wOptionality
      total `shouldSatisfy` (\t -> abs (t - 1.0) < 0.01)

--------------------------------------------------------------------------------
-- Mock Data
--------------------------------------------------------------------------------

-- | Base essence for modifications
baseEssence :: ContractEssence
baseEssence =
  ContractEssence
    { cePartyCount = 2
    , ceSymmetry = FullyAsymmetric
    , ceFlowTopology = Unidirectional
    , ceTemporalShape = PointToPoint
    , ceOptionalityDegree = 0.0
    , ceRecursionDepth = Nothing
    , ceBreachTopology = SimpleBreach
    , ceStateCount = 3
    , ceTerminalModes = 2
    }

-- | Create essence with specific recursion depth
mockEssenceWithRecursion :: Maybe Int -> ContractEssence
mockEssenceWithRecursion depth =
  baseEssence {ceRecursionDepth = depth}

-- | Create essence with specific optionality
mockEssenceWithOptionality :: Double -> ContractEssence
mockEssenceWithOptionality opt =
  baseEssence {ceOptionalityDegree = opt}

-- | Mock archetype for recursion testing
mockArchetypeWithRecursion :: Bool -> ACTUSArchetype
mockArchetypeWithRecursion requires =
  ACTUSArchetype
    { archActusCode = "TEST"
    , archLabel = "Test"
    , archDescription = "Test archetype"
    , archExpectedSymmetry = FullyAsymmetric
    , archExpectedTopology = Unidirectional
    , archExpectedTemporal = PointToPoint
    , archExpectedBreach = SimpleBreach
    , archPartyCount = 2
    , archRequiresRecursion = requires
    , archMinOptionality = 0.0
    , archMaxOptionality = 1.0
    }

-- | Mock archetype for optionality testing
mockArchetypeWithOptionality :: Double -> Double -> ACTUSArchetype
mockArchetypeWithOptionality minOpt maxOpt =
  (mockArchetypeWithRecursion False)
    { archMinOptionality = minOpt
    , archMaxOptionality = maxOpt
    }

-- | Debt pattern essence (matches PAM, ANN, LAM)
mockDebtEssence :: ContractEssence
mockDebtEssence =
  ContractEssence
    { cePartyCount = 2
    , ceSymmetry = FullyAsymmetric
    , ceFlowTopology = Unidirectional
    , ceTemporalShape = PointToPoint
    , ceOptionalityDegree = 0.0
    , ceRecursionDepth = Nothing
    , ceBreachTopology = SimpleBreach
    , ceStateCount = 3
    , ceTerminalModes = 2
    }

-- | Exchange pattern essence (matches FXOUT, FUTUR)
mockExchangeEssence :: ContractEssence
mockExchangeEssence =
  ContractEssence
    { cePartyCount = 2
    , ceSymmetry = Symmetric
    , ceFlowTopology = Bidirectional
    , ceTemporalShape = PointToPoint
    , ceOptionalityDegree = 0.1
    , ceRecursionDepth = Nothing
    , ceBreachTopology = SimpleBreach
    , ceStateCount = 3
    , ceTerminalModes = 2
    }

-- | Option pattern essence (matches OPTNS)
mockOptionEssence :: ContractEssence
mockOptionEssence =
  ContractEssence
    { cePartyCount = 2
    , ceSymmetry = AsymmetricWithRights
    , ceFlowTopology = Unidirectional
    , ceTemporalShape = EventDriven
    , ceOptionalityDegree = 0.6
    , ceRecursionDepth = Nothing
    , ceBreachTopology = NoBreachPath
    , ceStateCount = 3
    , ceTerminalModes = 1
    }

-- | Amortizing pattern essence (matches ANN, LAM)
mockAmortizingEssence :: ContractEssence
mockAmortizingEssence =
  ContractEssence
    { cePartyCount = 2
    , ceSymmetry = FullyAsymmetric
    , ceFlowTopology = Unidirectional
    , ceTemporalShape = RecursiveDecay
    , ceOptionalityDegree = 0.0
    , ceRecursionDepth = Just 1
    , ceBreachTopology = EscalationLadder 2
    , ceStateCount = 4
    , ceTerminalModes = 2
    }
