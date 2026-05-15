{-# LANGUAGE OverloadedStrings #-}
module NumberLiteralSpec (spec) where

import Base
import Data.Ratio ((%))
import L4.Parser (execProgramParserWithHintPass)
import L4.Syntax
  ( Decide (..)
  , Expr (..)
  , Lit (..)
  , Module (..)
  , Section (..)
  , TopDecl (..)
  )
import Test.Hspec

-- | Parse a source snippet and return the @Lit@ literal that appears as the
-- body of the first @DECIDE@. Fails the assertion if the body is anything
-- other than a single literal.
firstDecideLit :: Text -> IO Lit
firstDecideLit src = do
  let uri = toNormalizedUri (Uri "file:///number-literal-spec")
  case execProgramParserWithHintPass uri src of
    Left errs -> do
      expectationFailure $ "Parser failed: " <> show errs
      error "unreachable"
    Right (MkModule _ _ (MkSection _ _ _ decls), _, _) ->
      case decls of
        Decide _ (MkDecide _ _ _ (Lit _ lit)) : _ -> pure lit
        _ -> do
          expectationFailure $
            "Expected a DECIDE whose body is a literal, got: " <> show decls
          error "unreachable"

spec :: Spec
spec = describe "Number literals with `_` as thousand separator" $ do
  it "parses 100_000 as 100000" $ do
    lit <- firstDecideLit "DECIDE n IS 100_000"
    case lit of
      NumericLit _ r -> r `shouldBe` (100000 % 1)
      _ -> expectationFailure $ "Expected numeric literal, got: " <> show lit

  it "parses 1_000_000 as 1000000" $ do
    lit <- firstDecideLit "DECIDE n IS 1_000_000"
    case lit of
      NumericLit _ r -> r `shouldBe` (1000000 % 1)
      _ -> expectationFailure $ "Expected numeric literal, got: " <> show lit

  it "parses 100_000.50 as 100000.5" $ do
    lit <- firstDecideLit "DECIDE n IS 100_000.50"
    case lit of
      NumericLit _ r -> r `shouldBe` (200001 % 2)
      _ -> expectationFailure $ "Expected numeric literal, got: " <> show lit

  it "parses 1.50_5 as 1.505" $ do
    lit <- firstDecideLit "DECIDE n IS 1.50_5"
    case lit of
      NumericLit _ r -> r `shouldBe` (301 % 200)
      _ -> expectationFailure $ "Expected numeric literal, got: " <> show lit

  it "still parses plain 1000 as 1000" $ do
    lit <- firstDecideLit "DECIDE n IS 1000"
    case lit of
      NumericLit _ r -> r `shouldBe` (1000 % 1)
      _ -> expectationFailure $ "Expected numeric literal, got: " <> show lit

  it "rejects 1000_ (trailing underscore)" $ do
    let uri = toNormalizedUri (Uri "file:///number-literal-spec-bad")
    case execProgramParserWithHintPass uri "DECIDE n IS 1000_" of
      Left _ -> pure ()
      Right _ ->
        expectationFailure "Expected parse error for trailing underscore"
