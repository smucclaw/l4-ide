module EssenceSpec (spec) where

import qualified Data.Text as T
import L4.ACTUS.Qualia.Essence
import L4.ACTUS.Qualia.ObligationGraph hiding (Symmetric)
import L4.StateGraph
import L4.Syntax (DeonticModal (..))
import Test.Hspec

spec :: Spec
spec = describe "Essence" $ do
  describe "Symmetry computation" $ do
    it "returns FullyAsymmetric for debt pattern (one debtor, one creditor)" $ do
      let og = mockDebtGraph
      computeSymmetry og `shouldBe` FullyAsymmetric

    it "returns Symmetric for exchange pattern" $ do
      let og = mockExchangeGraph
      computeSymmetry og `shouldBe` Symmetric

    it "returns AsymmetricWithRights when creditor has MAY rights" $ do
      let og = mockOptionGraph
      computeSymmetry og `shouldBe` AsymmetricWithRights

  describe "FlowTopology computation" $ do
    it "returns Unidirectional for one-way flow" $ do
      let og = mockDebtGraph
      computeFlowTopology og `shouldBe` Unidirectional

    it "returns Bidirectional for two-way flow" $ do
      let og = mockExchangeGraph
      computeFlowTopology og `shouldBe` Bidirectional

  describe "TemporalShape computation" $ do
    it "returns RecursiveDecay when graph has recursion" $ do
      let og = mockAmortizingGraph
          sg = mockEmptyStateGraph
      computeTemporalShape og sg `shouldBe` RecursiveDecay

    it "returns PointToPoint for simple forward" $ do
      let og = mockDebtGraph
          sg = mockEmptyStateGraph
      computeTemporalShape og sg `shouldBe` PointToPoint

  describe "BreachTopology computation" $ do
    it "returns SimpleBreach for single breach path" $ do
      let sg = mockSimpleBreachStateGraph
      computeBreachTopology sg `shouldBe` SimpleBreach

    it "returns EscalationLadder for multi-step escalation" $ do
      let sg = mockEscalationStateGraph
      computeBreachTopology sg `shouldBe` EscalationLadder 2

    it "returns NoBreachPath when no breach states" $ do
      let sg = mockNoBreachStateGraph
      computeBreachTopology sg `shouldBe` NoBreachPath

  describe "Optionality computation" $ do
    it "returns 0.0 for all MUST obligations" $ do
      let og = mockDebtGraph
      computeOptionality og `shouldBe` 0.0

    it "returns 1.0 for all MAY obligations" $ do
      let og = mockAllMayGraph
      computeOptionality og `shouldBe` 1.0

    it "returns 0.5 for mixed MUST/MAY" $ do
      let og = mockMixedGraph
      computeOptionality og `shouldBe` 0.5

  describe "RecursionDepth detection" $ do
    it "returns Nothing for non-recursive graph" $ do
      let og = mockDebtGraph
      detectRecursionDepth og `shouldBe` Nothing

    it "returns Just depth for recursive graph" $ do
      let og = mockAmortizingGraph
      detectRecursionDepth og `shouldBe` Just 1

  describe "extractEssence" $ do
    it "extracts complete essence for debt pattern" $ do
      let og = mockDebtGraph
          sg = mockSimpleBreachStateGraph
          essence = extractEssence og sg
      essence.cePartyCount `shouldBe` 2
      essence.ceSymmetry `shouldBe` FullyAsymmetric
      essence.ceFlowTopology `shouldBe` Unidirectional
      essence.ceBreachTopology `shouldBe` SimpleBreach

    it "extracts complete essence for exchange pattern" $ do
      let og = mockExchangeGraph
          sg = mockNoBreachStateGraph
          essence = extractEssence og sg
      essence.cePartyCount `shouldBe` 2
      essence.ceSymmetry `shouldBe` Symmetric
      essence.ceFlowTopology `shouldBe` Bidirectional

  describe "describeEssence" $ do
    it "produces human-readable output" $ do
      let og = mockDebtGraph
          sg = mockSimpleBreachStateGraph
          essence = extractEssence og sg
          desc = describeEssence essence
      desc `shouldSatisfy` ("Contract Essence:" `T.isInfixOf`)

--------------------------------------------------------------------------------
-- Mock Data
--------------------------------------------------------------------------------

-- | Mock debt graph: Debtor (outgoing) → Creditor (incoming)
mockDebtGraph :: ObligationGraph
mockDebtGraph =
  ObligationGraph
    { ogParties =
        [ PartyNode 0 100 "Debtor" 1 0 0 0
        , PartyNode 1 200 "Creditor" 0 1 0 0
        ]
    , ogObligations =
        [ ObligationEdge 0 1 DMust MoneyValue PointInTime "pay" True False
        ]
    , ogRecursions = []
    }

-- | Mock exchange graph: Both parties have obligations to each other
mockExchangeGraph :: ObligationGraph
mockExchangeGraph =
  ObligationGraph
    { ogParties =
        [ PartyNode 0 100 "PartyA" 1 1 0 0
        , PartyNode 1 200 "PartyB" 1 1 0 0
        ]
    , ogObligations =
        [ ObligationEdge 0 1 DMust MoneyValue PointInTime "deliver USD" True False
        , ObligationEdge 1 0 DMust MoneyValue PointInTime "deliver EUR" True False
        ]
    , ogRecursions = []
    }

-- | Mock option graph: Creditor has MAY rights
mockOptionGraph :: ObligationGraph
mockOptionGraph =
  ObligationGraph
    { ogParties =
        [ PartyNode 0 100 "Writer" 1 0 0 0
        , PartyNode 1 200 "Holder" 0 1 1 0  -- Has MAY right
        ]
    , ogObligations =
        [ ObligationEdge 0 1 DMust AssetValue PointInTime "deliver underlying" True False
        ]
    , ogRecursions = []
    }

-- | Mock amortizing graph: Has recursion pattern
mockAmortizingGraph :: ObligationGraph
mockAmortizingGraph =
  ObligationGraph
    { ogParties =
        [ PartyNode 0 100 "Borrower" 1 0 0 0
        , PartyNode 1 200 "Lender" 0 1 0 0
        ]
    , ogObligations =
        [ ObligationEdge 0 1 DMust MoneyValue (Recursive UntilExhaustion) "pay installment" True False
        ]
    , ogRecursions =
        [ RecursionPattern 1 UntilExhaustion Nothing "Borrower -> pay"
        ]
    }

-- | Mock all-MAY graph
mockAllMayGraph :: ObligationGraph
mockAllMayGraph =
  ObligationGraph
    { ogParties =
        [ PartyNode 0 100 "PartyA" 1 0 1 0
        , PartyNode 1 200 "PartyB" 0 1 0 0
        ]
    , ogObligations =
        [ ObligationEdge 0 1 DMay MoneyValue PointInTime "may pay" False False
        ]
    , ogRecursions = []
    }

-- | Mock mixed MUST/MAY graph
mockMixedGraph :: ObligationGraph
mockMixedGraph =
  ObligationGraph
    { ogParties =
        [ PartyNode 0 100 "PartyA" 2 0 1 0
        , PartyNode 1 200 "PartyB" 0 2 0 0
        ]
    , ogObligations =
        [ ObligationEdge 0 1 DMust MoneyValue PointInTime "must pay" True False
        , ObligationEdge 0 1 DMay MoneyValue PointInTime "may pay extra" False False
        ]
    , ogRecursions = []
    }

-- | Empty state graph (no states)
mockEmptyStateGraph :: StateGraph
mockEmptyStateGraph =
  StateGraph
    { sgName = "empty"
    , sgStates = []
    , sgTransitions = []
    , sgInitialState = 0
    }

-- | Simple breach state graph
mockSimpleBreachStateGraph :: StateGraph
mockSimpleBreachStateGraph =
  StateGraph
    { sgName = "simple_breach"
    , sgStates =
        [ ContractState 0 "initial" InitialState
        , ContractState 1 "Fulfilled" TerminalFulfilled
        , ContractState 2 "Breach" TerminalBreach
        ]
    , sgTransitions =
        [ Transition 0 1 (TransitionLabel Nothing Nothing "pay" Nothing Nothing) HenceTransition
        , Transition 0 2 (TransitionLabel Nothing Nothing "timeout" Nothing Nothing) LestTransition
        ]
    , sgInitialState = 0
    }

-- | Escalation state graph (penalty → breach)
mockEscalationStateGraph :: StateGraph
mockEscalationStateGraph =
  StateGraph
    { sgName = "escalation"
    , sgStates =
        [ ContractState 0 "initial" InitialState
        , ContractState 1 "penalty" IntermediateState
        , ContractState 2 "Fulfilled" TerminalFulfilled
        , ContractState 3 "Breach" TerminalBreach
        ]
    , sgTransitions =
        [ Transition 0 2 (TransitionLabel Nothing Nothing "pay" Nothing Nothing) HenceTransition
        , Transition 0 1 (TransitionLabel Nothing Nothing "timeout" Nothing Nothing) LestTransition
        , Transition 1 3 (TransitionLabel Nothing Nothing "timeout" Nothing Nothing) LestTransition
        ]
    , sgInitialState = 0
    }

-- | No breach path state graph
mockNoBreachStateGraph :: StateGraph
mockNoBreachStateGraph =
  StateGraph
    { sgName = "no_breach"
    , sgStates =
        [ ContractState 0 "initial" InitialState
        , ContractState 1 "Fulfilled" TerminalFulfilled
        ]
    , sgTransitions =
        [ Transition 0 1 (TransitionLabel Nothing Nothing "complete" Nothing Nothing) HenceTransition
        ]
    , sgInitialState = 0
    }
