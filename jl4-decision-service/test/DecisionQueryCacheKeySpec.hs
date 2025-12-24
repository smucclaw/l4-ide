module DecisionQueryCacheKeySpec (spec) where

import Test.Hspec

import qualified Data.Map.Strict as Map
import qualified Data.Text as Text

import Server

spec :: Spec
spec = describe "decisionQueryCacheKey" do
  it "ignores function metadata (depends only on sources)" do
    let
      src =
        Text.unlines
          [ "GIVETH A BOOLEAN"
          , "DECIDE f IS TRUE"
          ]
      fn0 = mkFn "desc0" src
      fn1 = mkFn "desc1" src
    decisionQueryCacheKey "f" fn0 `shouldBe` decisionQueryCacheKey "f" fn1

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
      fn0 = mkFn "desc" src0
      fn1 = mkFn "desc" src1
    decisionQueryCacheKey "f" fn0 `shouldNotBe` decisionQueryCacheKey "f" fn1

mkFn :: Text.Text -> Text.Text -> ValidatedFunction
mkFn desc src =
  let
    decl =
      Function
        { name = "f"
        , description = desc
        , parameters = MkParameters {parameterMap = Map.empty, required = []}
        , supportedEvalBackend = [JL4]
        }
  in
    ValidatedFunction
      { fnImpl = decl
      , fnEvaluator = Map.empty
      , fnCompiled = Nothing
      , fnSources = Map.singleton JL4 src
      , fnDecisionQueryCache = Nothing
      }

