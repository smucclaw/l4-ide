{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
module MixfixParserSpec (spec) where

import Base hiding (listToMaybe)
import Data.Maybe (listToMaybe)
import qualified Data.Text as T
import L4.Parser (buildMixfixHintRegistry, execProgramParserWithHintPass)
import L4.Syntax (AppForm (..), Decide (..), Expr (..), Module (..), Name, Section (..), TopDecl (..), pattern Var, rawName, rawNameToText)
import Test.Hspec

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
  T.unlines
    [ "§ `Mixfix Operators With Variables`"
    , ""
    , "-- Regression tests ensuring mixfix operators (beyond pure postfix) accept bare"
    , "-- variables as operands, both at top level and within LET...IN blocks."
    , "--"
    , "-- These tests complement postfix coverage in postfix-with-variables.l4."
    , ""
    , "IMPORT prelude"
    , ""
    , "-- Simple binary mixfix operator."
    , "GIVEN left IS A NUMBER, right IS A NUMBER"
    , "left `mixadd` right MEANS left + right"
    , ""
    , "-- Ternary mixfix operator with two keywords."
    , "GIVEN base IS A NUMBER, delta IS A NUMBER, cap IS A NUMBER"
    , "base `climb` delta `toward` cap MEANS base + delta + cap"
    , ""
    , "-- Another ternary operator whose keywords can be written without backticks."
    , "GIVEN start IS A NUMBER, pivot IS A NUMBER, finish IS A NUMBER"
    , "start `via` pivot `onto` finish MEANS start + pivot + finish"
    , ""
    , "-- ====================================================================="
    , "-- Tests: binary mixfix with bare variables"
    , "-- ====================================================================="
    , ""
    , "GIVEN x IS A NUMBER"
    , "mixfixVarInfixBacktick MEANS x `mixadd` 5"
    , ""
    , "#EVAL mixfixVarInfixBacktick OF 7"
    , "-- Expected: 12"
    , ""
    , "-- Mixfix analogue of the original postfix bug:"
    , "-- Previously, bare variables like `x mixadd 5` could be mis-parsed similarly to `x squared`."
    , "GIVEN x_bug IS A NUMBER"
    , "mixfixOriginalShape MEANS x_bug mixadd 5"
    , ""
    , "#EVAL mixfixOriginalShape OF 3"
    , "-- Expected: 8"
    , ""
    , "-- LET...IN scope still sees bare variables."
    , "GIVEN z IS A NUMBER"
    , "mixfixVarInfixLet MEANS"
    , "  LET tmp BE z + 1"
    , "  IN tmp mixadd 2"
    , ""
    , "#EVAL mixfixVarInfixLet OF 4"
    , "-- Expected: 7"
    , ""
    , "-- ====================================================================="
    , "-- Tests: ternary mixfix with bare variables"
    , "-- ====================================================================="
    , ""
    , "GIVEN a IS A NUMBER, b IS A NUMBER"
    , "mixfixVarMultiBacktick MEANS a `climb` b `toward` 10"
    , ""
    , "#EVAL mixfixVarMultiBacktick OF 2, 3"
    , "-- Expected: 15"
    , ""
    , "GIVEN c IS A NUMBER, d IS A NUMBER"
    , "mixfixVarMultiBare MEANS c via d onto 1"
    , ""
    , "#EVAL mixfixVarMultiBare OF 5, 6"
    , "-- Expected: 12"
    ]

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
