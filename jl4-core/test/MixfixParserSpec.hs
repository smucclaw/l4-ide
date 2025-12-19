{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
module MixfixParserSpec (spec) where

import Base hiding (lift, listToMaybe)
import Data.Maybe (listToMaybe)
import qualified Data.Text as T
import Language.Haskell.TH.Syntax (lift, runIO)
import L4.Parser (buildMixfixHintRegistry, execProgramParserWithHintPass)
import L4.Syntax (AppForm (..), Decide (..), Expr (..), Module (..), Name, Section (..), TopDecl (..), pattern Var, rawName, rawNameToText)
import Test.Hspec hiding (runIO)

spec :: Spec
spec =
  describe "Mixfix parser hint pass" $ do
    it "parses a simple mixfix module with registry hints" $ do
      let uri = toNormalizedUri (Uri "file:///mixfix-parser-spec")
          parsed = execProgramParserWithHintPass uri mixfixFixture
      case parsed of
        Left errs ->
          expectationFailure $
            "Parser failed with: " <> show errs
        Right (moduleAst, hints, _warnings) ->
          hints `shouldBe` buildMixfixHintRegistry moduleAst

    it "accepts multiline mixfix and postfix invocations when aligned" $ do
      let uri = toNormalizedUri (Uri "file:///mixfix-multiline-spec")
          parsed = execProgramParserWithHintPass uri multilineFixture
      case parsed of
        Left errs ->
          expectationFailure $
            "Parser failed with: " <> show errs
        Right (moduleAst, _hints, _warnings) -> do
          postfixBody <- expectBody "demo_multiline_postfix" moduleAst
          mixfixBody <- expectBody "demo_multiline_mixfix" moduleAst
          chainBody <- expectBody "demo_multiline_chain" moduleAst
          assertPostfix postfixBody
          assertBinary mixfixBody
          assertChain chainBody

assertPostfix :: Expr Name -> Expectation
assertPostfix expr =
  case expr of
    App _ funcName [Var _ radiusName] -> do
      rawNameText funcName `shouldBe` "squared"
      rawNameText radiusName `shouldBe` "radius"
    _ -> expectationFailure $ "Expected postfix App but got: " <> show expr

assertBinary :: Expr Name -> Expectation
assertBinary expr =
  case expr of
    App _ funcName [Var _ baseName, Var _ exponentName] -> do
      rawNameText funcName `shouldBe` "raised to"
      rawNameText baseName `shouldBe` "base"
      rawNameText exponentName `shouldBe` "exponent"
    _ -> expectationFailure $ "Expected binary mixfix App but got: " <> show expr

assertChain :: Expr Name -> Expectation
assertChain expr =
  case expr of
    App _ funcName [Var _ payerName, Var _ payeeName, Var _ keywordName, Var _ ledgerName] -> do
      rawNameText funcName `shouldBe` "remits"
      rawNameText payerName `shouldBe` "payer"
      rawNameText payeeName `shouldBe` "payee"
      rawNameText keywordName `shouldBe` "to settle"
      rawNameText ledgerName `shouldBe` "ledger"
    _ -> expectationFailure $ "Expected ternary mixfix chain but got: " <> show expr

expectBody :: T.Text -> Module Name -> IO (Expr Name)
expectBody target modu =
  case lookupDecideBody target modu of
    Nothing -> do
      let names = decideNames modu
      expectationFailure ("Unable to find Decide named " <> T.unpack target <> ". Available: " <> show (map T.unpack names))
      fail "unreachable"
    Just body -> pure body

decideNames :: Module Name -> [T.Text]
decideNames (MkModule _ _ (MkSection _ _ _ decls)) =
  [ rawNameText fn
  | Decide _ (MkDecide _ _ (MkAppForm _ fn _ _) _) <- decls
  ]

lookupDecideBody :: T.Text -> Module Name -> Maybe (Expr Name)
lookupDecideBody target (MkModule _ _ (MkSection _ _ _ decls)) =
  listToMaybe
    [ body
    | Decide _ (MkDecide _ _ (MkAppForm _ fn _ _) body) <- decls
    , rawNameText fn == target
    ]

rawNameText :: Name -> T.Text
rawNameText = rawNameToText . rawName

mixfixFixture :: T.Text
mixfixFixture =
  T.pack
    $(do
        contents <- runIO (readFile "../jl4/examples/ok/mixfix-with-variables.l4")
        lift contents)

multilineFixture :: T.Text
multilineFixture =
  T.unlines
    [ "IMPORT prelude"
    , ""
    , "GIVEN n IS A NUMBER"
    , "n squared MEANS n * n"
    , ""
    , "GIVEN radius IS A NUMBER"
    , "demo_multiline_postfix radius MEANS"
    , "  radius"
    , "  `squared`"
    , ""
    , "GIVEN base IS A NUMBER"
    , "      exponent IS A NUMBER"
    , "base `raised to` exponent MEANS base * exponent"
    , ""
    , "demo_multiline_mixfix base exponent MEANS"
    , "  base"
    , "  `raised to`"
    , "  exponent"
    , ""
    , "GIVEN payer IS A NUMBER"
    , "      payee IS A NUMBER"
    , "      ledger IS A NUMBER"
    , "payer `remits` payee `to settle` ledger MEANS payer + payee + ledger"
    , ""
    , "demo_multiline_chain payer payee ledger MEANS"
    , "  payer"
    , "  `remits`"
    , "  payee"
    , "  `to settle`"
    , "  ledger"
    ]
