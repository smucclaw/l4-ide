module TemporalContextSpec (spec) where

import Data.Time (UTCTime (..), fromGregorian, secondsToDiffTime)
import L4.TemporalContext (EvalClause (..), TemporalContext (..), applyEvalClauses, initialTemporalContext)
import Test.Hspec

spec :: Spec
spec = describe "applyEvalClauses with UnderRulesEffectiveAt" $ do
  let targetDay = fromGregorian 2024 5 5
      systemNow = UTCTime (fromGregorian 2024 1 1) (secondsToDiffTime 0)

  it "preserves an existing encoding timestamp" $ do
    let pinnedEncoding = UTCTime (fromGregorian 2023 12 31) (secondsToDiffTime 3600)
        ctxWithEncoding =
          (initialTemporalContext systemNow)
            { tcRuleEncodingTime = Just pinnedEncoding
            }
        updated = applyEvalClauses [UnderRulesEffectiveAt targetDay] ctxWithEncoding
    updated.tcRuleEncodingTime `shouldBe` Just pinnedEncoding
    updated.tcRuleValidTime `shouldBe` Just targetDay
    updated.tcRuleVersionTime `shouldBe` Just targetDay

  it "defaults encoding time to the target day when missing" $ do
    let updated = applyEvalClauses [UnderRulesEffectiveAt targetDay] (initialTemporalContext systemNow)
        expectedEncoding = UTCTime targetDay (secondsToDiffTime 0)
    updated.tcRuleEncodingTime `shouldBe` Just expectedEncoding
