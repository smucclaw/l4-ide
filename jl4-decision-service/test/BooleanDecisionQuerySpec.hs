module BooleanDecisionQuerySpec (spec) where

import Backend.BooleanDecisionQuery
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Test.Hspec

spec :: Spec
spec = do
  describe "compileDecisionQuery" do
    it "supports absorption: x ∨ (x ∧ y) has support {x}" do
      let expr = BOr [BVar ("x" :: String), BAnd [BVar "x", BVar "y"]]
          c = compileDecisionQuery ["x", "y"] expr
          r = c.query Map.empty
      r.determined `shouldBe` Nothing
      r.support `shouldBe` Set.fromList ["x"]
      r.ranked `shouldBe` ["x"]

    it "restriction makes support shrink: (x ∨ y) with x=true is determined" do
      let expr = BOr [BVar ("x" :: String), BVar "y"]
          c = compileDecisionQuery ["x", "y"] expr
          rTrue = c.query (Map.fromList [("x", True)])
          rFalse = c.query (Map.fromList [("x", False)])
      rTrue.determined `shouldBe` Just True
      rTrue.support `shouldBe` Set.empty
      rFalse.determined `shouldBe` Nothing
      rFalse.support `shouldBe` Set.fromList ["y"]
