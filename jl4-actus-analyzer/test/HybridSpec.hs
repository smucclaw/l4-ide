module HybridSpec (spec) where

import qualified Data.Text as T
import L4.ACTUS.Ontology.Types (Evidence (..))
import L4.ACTUS.Qualia.Essence
import L4.ACTUS.Qualia.Hybrid
import Test.Hspec

spec :: Spec
spec = describe "Hybrid" $ do
  describe "ClassificationStrategy" $ do
    it "has all expected strategies" $ do
      let strategies =
            [ SymbolicOnly
            , QualiaOnly
            , HybridSymbolicFirst
            , HybridQualiaFirst
            , Ensemble
            ]
      length strategies `shouldBe` 5

  describe "defaultHybridConfig" $ do
    it "uses HybridSymbolicFirst strategy" $ do
      defaultHybridConfig.hcStrategy `shouldBe` HybridSymbolicFirst

    it "has reasonable thresholds" $ do
      defaultHybridConfig.hcSymbolicThreshold `shouldSatisfy` (> 0.5)
      defaultHybridConfig.hcSymbolicThreshold `shouldSatisfy` (< 1.0)
      defaultHybridConfig.hcMinConfidence `shouldSatisfy` (> 0.0)
      defaultHybridConfig.hcMinConfidence `shouldSatisfy` (< 0.5)

    it "has weights summing to 1.0" $ do
      let total = defaultHybridConfig.hcQualiaWeight + defaultHybridConfig.hcSymbolicWeight
      total `shouldBe` 1.0

  describe "Evidence generation" $ do
    it "generates evidence for all essence dimensions" $ do
      let essence = mockDebtEssence
          evidence = generateQualiaEvidence essence
      -- Should have evidence for: symmetry, topology, temporal, breach, party count, optionality
      length evidence `shouldSatisfy` (>= 6)

    it "includes party structure evidence" $ do
      let essence = mockDebtEssence
          evidence = generateQualiaEvidence essence
          hasParty = any (\e -> e.evidenceFeature == "Party structure") evidence
      hasParty `shouldBe` True

    it "includes flow topology evidence" $ do
      let essence = mockExchangeEssence
          evidence = generateQualiaEvidence essence
          hasFlow = any (\e -> e.evidenceFeature == "Flow topology") evidence
      hasFlow `shouldBe` True

    it "includes temporal shape evidence" $ do
      let essence = mockAmortizingEssence
          evidence = generateQualiaEvidence essence
          hasTemporal = any (\e -> e.evidenceFeature == "Temporal shape") evidence
      hasTemporal `shouldBe` True

    it "includes recursion evidence when present" $ do
      let essence = mockAmortizingEssence -- Has recursion
          evidence = generateQualiaEvidence essence
          hasRecursion = any (\e -> e.evidenceFeature == "Recursion") evidence
      hasRecursion `shouldBe` True

    it "excludes recursion evidence when not present" $ do
      let essence = mockDebtEssence -- No recursion
          evidence = generateQualiaEvidence essence
          hasRecursion = any (\e -> e.evidenceFeature == "Recursion") evidence
      hasRecursion `shouldBe` False

  describe "Symmetry descriptions" $ do
    it "describes FullyAsymmetric" $ do
      let essence = mockDebtEssence {ceSymmetry = FullyAsymmetric}
          evidence = generateQualiaEvidence essence
          symEvidence = filter (\e -> e.evidenceFeature == "Party structure") evidence
      case symEvidence of
        (e : _) -> e.evidenceMappedTo `shouldSatisfy` ("asymmetric" `T.isInfixOf`)
        [] -> expectationFailure "Missing symmetry evidence"

    it "describes Symmetric" $ do
      let essence = mockExchangeEssence {ceSymmetry = Symmetric}
          evidence = generateQualiaEvidence essence
          symEvidence = filter (\e -> e.evidenceFeature == "Party structure") evidence
      case symEvidence of
        (e : _) -> e.evidenceMappedTo `shouldSatisfy` ("Symmetric" `T.isInfixOf`)
        [] -> expectationFailure "Missing symmetry evidence"

  describe "Topology descriptions" $ do
    it "describes Unidirectional" $ do
      let essence = mockDebtEssence {ceFlowTopology = Unidirectional}
          evidence = generateQualiaEvidence essence
          topEvidence = filter (\e -> e.evidenceFeature == "Flow topology") evidence
      case topEvidence of
        (e : _) -> e.evidenceMappedTo `shouldSatisfy` ("Unidirectional" `T.isInfixOf`)
        [] -> expectationFailure "Missing topology evidence"

    it "describes Bidirectional" $ do
      let essence = mockExchangeEssence {ceFlowTopology = Bidirectional}
          evidence = generateQualiaEvidence essence
          topEvidence = filter (\e -> e.evidenceFeature == "Flow topology") evidence
      case topEvidence of
        (e : _) -> e.evidenceMappedTo `shouldSatisfy` ("Bidirectional" `T.isInfixOf`)
        [] -> expectationFailure "Missing topology evidence"

  describe "Temporal descriptions" $ do
    it "describes RecursiveDecay" $ do
      let essence = mockAmortizingEssence {ceTemporalShape = RecursiveDecay}
          evidence = generateQualiaEvidence essence
          tempEvidence = filter (\e -> e.evidenceFeature == "Temporal shape") evidence
      case tempEvidence of
        (e : _) -> e.evidenceMappedTo `shouldSatisfy` ("decay" `T.isInfixOf`)
        [] -> expectationFailure "Missing temporal evidence"

    it "describes EventDriven" $ do
      let essence = mockOptionEssence {ceTemporalShape = EventDriven}
          evidence = generateQualiaEvidence essence
          tempEvidence = filter (\e -> e.evidenceFeature == "Temporal shape") evidence
      case tempEvidence of
        (e : _) -> e.evidenceMappedTo `shouldSatisfy` ("Event" `T.isInfixOf`)
        [] -> expectationFailure "Missing temporal evidence"

  describe "Optionality descriptions" $ do
    it "describes low optionality" $ do
      let essence = mockDebtEssence {ceOptionalityDegree = 0.05}
          evidence = generateQualiaEvidence essence
          optEvidence = filter (\e -> e.evidenceFeature == "Optionality") evidence
      case optEvidence of
        (e : _) -> e.evidenceMappedTo `shouldSatisfy` ("low" `T.isInfixOf`)
        [] -> expectationFailure "Missing optionality evidence"

    it "describes high optionality" $ do
      let essence = mockOptionEssence {ceOptionalityDegree = 0.8}
          evidence = generateQualiaEvidence essence
          optEvidence = filter (\e -> e.evidenceFeature == "Optionality") evidence
      case optEvidence of
        (e : _) -> e.evidenceMappedTo `shouldSatisfy` ("High" `T.isInfixOf`)
        [] -> expectationFailure "Missing optionality evidence"

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

-- | Debt pattern essence
mockDebtEssence :: ContractEssence
mockDebtEssence = baseEssence

-- | Exchange pattern essence
mockExchangeEssence :: ContractEssence
mockExchangeEssence =
  baseEssence
    { ceSymmetry = Symmetric
    , ceFlowTopology = Bidirectional
    , ceOptionalityDegree = 0.1
    }

-- | Option pattern essence
mockOptionEssence :: ContractEssence
mockOptionEssence =
  baseEssence
    { ceSymmetry = AsymmetricWithRights
    , ceTemporalShape = EventDriven
    , ceOptionalityDegree = 0.6
    , ceBreachTopology = NoBreachPath
    }

-- | Amortizing pattern essence
mockAmortizingEssence :: ContractEssence
mockAmortizingEssence =
  baseEssence
    { ceTemporalShape = RecursiveDecay
    , ceRecursionDepth = Just 1
    , ceBreachTopology = EscalationLadder 2
    }
