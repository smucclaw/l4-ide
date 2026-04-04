module DecisionQueryCacheKeySpec (spec) where

import Test.Hspec

import qualified Data.Text as Text

import Backend.DecisionQueryPlan (decisionQueryCacheKey)

spec :: Spec
spec = describe "decisionQueryCacheKey" do
  it "produces the same key for identical source text" do
    let
      src =
        Text.unlines
          [ "GIVETH A BOOLEAN"
          , "DECIDE f IS TRUE"
          ]
    decisionQueryCacheKey "f" src `shouldBe` decisionQueryCacheKey "f" src

  it "changes when the function source changes" do
    let
      src0 =
        Text.unlines
          [ "GIVETH A BOOLEAN"
          , "DECIDE f IS TRUE"
          ]
      src1 =
        Text.unlines
          [ "GIVETH A BOOLEAN"
          , "DECIDE f IS FALSE"
          ]
    decisionQueryCacheKey "f" src0 `shouldNotBe` decisionQueryCacheKey "f" src1
