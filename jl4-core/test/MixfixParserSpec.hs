{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module MixfixParserSpec (spec) where

import Base hiding (lift)
import qualified Data.Text as T
import Language.Haskell.TH.Syntax (lift, runIO)
import L4.Parser (buildMixfixHintRegistry, execProgramParserWithHintPass)
import Test.Hspec hiding (runIO)

spec :: Spec
spec =
  describe "Mixfix parser hint pass" $
    it "parses a simple mixfix module with registry hints" $ do
      let uri = toNormalizedUri (Uri "file:///mixfix-parser-spec")
          parsed = execProgramParserWithHintPass uri mixfixFixture
      case parsed of
        Left errs ->
          expectationFailure $
            "Parser failed with: " <> show errs
        Right (moduleAst, hints, _warnings) ->
          hints `shouldBe` buildMixfixHintRegistry moduleAst

mixfixFixture :: T.Text
mixfixFixture =
  T.pack
    $(do
        contents <- runIO (readFile "../jl4/examples/ok/mixfix-with-variables.l4")
        lift contents)
