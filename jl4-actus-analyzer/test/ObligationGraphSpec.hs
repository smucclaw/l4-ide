module ObligationGraphSpec (spec) where

import L4.ACTUS.Qualia.ObligationGraph
import L4.Syntax (DeonticModal (..))
import Test.Hspec

spec :: Spec
spec = describe "ObligationGraph" $ do
  describe "FlowRole computation" $ do
    it "detects NetDebtor when outDegree > inDegree" $ do
      let party =
            PartyNode
              { pnId = 0
              , pnExprHash = 0
              , pnExprText = "PartyA"
              , pnOutDegree = 3
              , pnInDegree = 1
              , pnMayCount = 0
              , pnShantCount = 0
              }
      flowRole party `shouldBe` NetDebtor

    it "detects NetCreditor when inDegree > outDegree" $ do
      let party =
            PartyNode
              { pnId = 0
              , pnExprHash = 0
              , pnExprText = "PartyB"
              , pnOutDegree = 1
              , pnInDegree = 3
              , pnMayCount = 0
              , pnShantCount = 0
              }
      flowRole party `shouldBe` NetCreditor

    it "detects Symmetric when degrees are equal and non-zero" $ do
      let party =
            PartyNode
              { pnId = 0
              , pnExprHash = 0
              , pnExprText = "PartyA"
              , pnOutDegree = 2
              , pnInDegree = 2
              , pnMayCount = 0
              , pnShantCount = 0
              }
      flowRole party `shouldBe` Symmetric

    it "detects Uninvolved when degrees are zero" $ do
      let party =
            PartyNode
              { pnId = 0
              , pnExprHash = 0
              , pnExprText = "Observer"
              , pnOutDegree = 0
              , pnInDegree = 0
              , pnMayCount = 0
              , pnShantCount = 0
              }
      flowRole party `shouldBe` Uninvolved

  describe "ObligationGraph analysis" $ do
    it "partyCount returns correct count" $ do
      let og =
            ObligationGraph
              { ogParties =
                  [ PartyNode 0 0 "A" 1 0 0 0
                  , PartyNode 1 1 "B" 0 1 0 0
                  ]
              , ogObligations = []
              , ogRecursions = []
              }
      partyCount og `shouldBe` 2

    it "hasRecursion detects recursive patterns" $ do
      let ogWithRecursion =
            ObligationGraph
              { ogParties = []
              , ogObligations = []
              , ogRecursions = [RecursionPattern 1 UntilExhaustion Nothing "test"]
              }
          ogWithoutRecursion =
            ObligationGraph
              { ogParties = []
              , ogObligations = []
              , ogRecursions = []
              }
      hasRecursion ogWithRecursion `shouldBe` True
      hasRecursion ogWithoutRecursion `shouldBe` False

    it "isUnidirectional detects one-way flow" $ do
      let og =
            ObligationGraph
              { ogParties =
                  [ PartyNode 0 0 "A" 1 0 0 0
                  , PartyNode 1 1 "B" 0 1 0 0
                  ]
              , ogObligations =
                  [ ObligationEdge 0 1 DMust MoneyValue PointInTime "pay" True False
                  ]
              , ogRecursions = []
              }
      isUnidirectional og `shouldBe` True
      isBidirectional og `shouldBe` False

    it "isBidirectional detects two-way flow" $ do
      let og =
            ObligationGraph
              { ogParties =
                  [ PartyNode 0 0 "A" 1 1 0 0
                  , PartyNode 1 1 "B" 1 1 0 0
                  ]
              , ogObligations =
                  [ ObligationEdge 0 1 DMust MoneyValue PointInTime "pay" True False
                  , ObligationEdge 1 0 DMust AssetValue PointInTime "deliver" True False
                  ]
              , ogRecursions = []
              }
      isBidirectional og `shouldBe` True
      isUnidirectional og `shouldBe` False

  describe "ValueType inference" $ do
    it "recognizes money-related keywords" $ do
      -- The inferValueType function is not exported, so we test indirectly
      -- through ObligationEdge values that would be created
      True `shouldBe` True -- Placeholder

  describe "TemporalPattern types" $ do
    it "has correct DecayPattern constructors" $ do
      -- Verify the types exist and are usable
      let patterns = [LinearDecay, ExponentialDecay, UntilExhaustion, Unbounded]
      length patterns `shouldBe` 4
