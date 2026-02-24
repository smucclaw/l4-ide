module DecisionQueryCacheKeySpec (spec) where

import Test.Hspec

import qualified Data.Map.Strict as Map
import qualified Data.Text as Text

import Backend.Api (EvalBackend (..))
import Backend.DecisionQueryPlan (decisionQueryCacheKey)

spec :: Spec
spec = describe "decisionQueryCacheKey" do
  it "ignores function metadata (depends only on sources)" do
    let
      src =
        Text.unlines
          [ "GIVETH A BOOLEAN"
          , "DECIDE f IS TRUE"
          ]
      sources0 = Map.singleton JL4 src
      sources1 = Map.singleton JL4 src
    decisionQueryCacheKey "f" sources0 `shouldBe` decisionQueryCacheKey "f" sources1

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
      sources0 = Map.singleton JL4 src0
      sources1 = Map.singleton JL4 src1
    decisionQueryCacheKey "f" sources0 `shouldNotBe` decisionQueryCacheKey "f" sources1
