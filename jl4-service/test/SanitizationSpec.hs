{-# LANGUAGE OverloadedStrings #-}

module SanitizationSpec (spec) where

import Test.Hspec

import Backend.Api (FnLiteral (..), EvaluatorError (..), ParameterMismatch (..), prettyEvaluatorError)
import L4.FunctionSchema (Parameters (..), Parameter (..))
import Shared (sanitizePropertyName, buildPropertyReverseMap, remapFnLiteralKeys, remapArguments, validateNoSanitizationCollisions, sanitizeFieldNamesInText)

import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text

spec :: SpecWith ()
spec = describe "Sanitization" $ do
  describe "sanitizePropertyName" $ do
    it "replaces spaces with hyphens" $
      sanitizePropertyName "function or purpose" `shouldBe` "function-or-purpose"

    it "replaces backticks with hyphens" $
      sanitizePropertyName "`some field`" `shouldBe` "some-field"

    it "collapses consecutive hyphens" $
      sanitizePropertyName "a  b" `shouldBe` "a-b"

    it "strips leading and trailing hyphens" $
      sanitizePropertyName " hello " `shouldBe` "hello"

    it "preserves existing hyphens" $
      sanitizePropertyName "Non-Profit type" `shouldBe` "Non-Profit-type"

    it "preserves dots and underscores" $
      sanitizePropertyName "Loss or Damage.caused by insects" `shouldBe` "Loss-or-Damage.caused-by-insects"

    it "returns _unnamed for empty result" $
      sanitizePropertyName "``" `shouldBe` "_unnamed"

  describe "buildPropertyReverseMap" $ do
    it "maps sanitized names back to originals" $ do
      let params = MkParameters
            (Map.fromList
              [ ("function or purpose", simpleParam "string")
              , ("name", simpleParam "string")
              ])
            ["function or purpose", "name"]
      let revMap = buildPropertyReverseMap params
      Map.lookup "function-or-purpose" revMap `shouldBe` Just "function or purpose"
      -- "name" doesn't change, so it's not in the reverse map
      Map.lookup "name" revMap `shouldBe` Nothing

    it "includes nested object property names" $ do
      let nestedProps = Map.fromList
            [ ("country of manufacture", simpleParam "string")
            , ("batch number or lot number", simpleParam "string")
            ]
          params = MkParameters
            (Map.fromList
              [ ("product", (simpleParam "object") { parameterProperties = Just nestedProps })
              ])
            ["product"]
      let revMap = buildPropertyReverseMap params
      Map.lookup "country-of-manufacture" revMap `shouldBe` Just "country of manufacture"
      Map.lookup "batch-number-or-lot-number" revMap `shouldBe` Just "batch number or lot number"

    it "includes array item property names" $ do
      let itemProps = Map.fromList
            [ ("type category", simpleParam "string")
            ]
          itemParam = (simpleParam "object") { parameterProperties = Just itemProps }
          params = MkParameters
            (Map.fromList
              [ ("ingredients", (simpleParam "array") { parameterItems = Just itemParam })
              ])
            ["ingredients"]
      let revMap = buildPropertyReverseMap params
      Map.lookup "type-category" revMap `shouldBe` Just "type category"

  describe "remapFnLiteralKeys" $ do
    it "remaps keys in FnObject" $ do
      let revMap = Map.fromList [("function-or-purpose", "function or purpose")]
          input = FnObject [("function-or-purpose", FnLitString "moisturizing")]
          expected = FnObject [("function or purpose", FnLitString "moisturizing")]
      remapFnLiteralKeys revMap input `shouldBe` expected

    it "remaps keys recursively in nested objects" $ do
      let revMap = Map.fromList
            [ ("country-of-manufacture", "country of manufacture")
            , ("batch-number", "batch number")
            ]
          input = FnObject
            [ ("product", FnObject
                [ ("country-of-manufacture", FnLitString "SG")
                , ("batch-number", FnLitString "LOT1")
                ])
            ]
          expected = FnObject
            [ ("product", FnObject
                [ ("country of manufacture", FnLitString "SG")
                , ("batch number", FnLitString "LOT1")
                ])
            ]
      remapFnLiteralKeys revMap input `shouldBe` expected

    it "remaps keys in objects inside arrays" $ do
      let revMap = Map.fromList [("type-category", "type category")]
          input = FnArray
            [ FnObject [("type-category", FnLitString "solvent")]
            , FnObject [("type-category", FnLitString "active")]
            ]
          expected = FnArray
            [ FnObject [("type category", FnLitString "solvent")]
            , FnObject [("type category", FnLitString "active")]
            ]
      remapFnLiteralKeys revMap input `shouldBe` expected

    it "leaves non-mapped keys unchanged" $ do
      let revMap = Map.fromList [("foo-bar", "foo bar")]
          input = FnObject [("name", FnLitString "test"), ("foo-bar", FnLitBool True)]
          expected = FnObject [("name", FnLitString "test"), ("foo bar", FnLitBool True)]
      remapFnLiteralKeys revMap input `shouldBe` expected

  describe "remapArguments" $ do
    it "remaps both top-level keys and nested FnObject keys" $ do
      let revMap = Map.fromList
            [ ("my-arg", "my arg")
            , ("inner-field", "inner field")
            ]
          input = [("my-arg", Just (FnObject [("inner-field", FnLitBool True)]))]
          expected = [("my arg", Just (FnObject [("inner field", FnLitBool True)]))]
      remapArguments revMap input `shouldBe` expected

  describe "validateNoSanitizationCollisions" $ do
    it "returns empty list when no collisions" $ do
      let params = MkParameters
            (Map.fromList
              [ ("foo bar", simpleParam "string")
              , ("baz qux", simpleParam "string")
              ])
            []
      validateNoSanitizationCollisions "test_fn" params `shouldBe` []

    it "detects collision between spaced and hyphenated names" $ do
      let params = MkParameters
            (Map.fromList
              [ ("foo bar", simpleParam "string")
              , ("foo-bar", simpleParam "string")
              ])
            []
      let collisions = validateNoSanitizationCollisions "test_fn" params
      length collisions `shouldBe` 1
      case collisions of
        (c:_) -> do
          c `shouldSatisfy` Text.isInfixOf "foo-bar"
          c `shouldSatisfy` Text.isInfixOf "foo bar"
        [] -> expectationFailure "Expected at least one collision"

    it "detects collisions in nested properties" $ do
      let nestedProps = Map.fromList
            [ ("inner field", simpleParam "string")
            , ("inner-field", simpleParam "string")
            ]
          params = MkParameters
            (Map.fromList
              [ ("product", (simpleParam "object") { parameterProperties = Just nestedProps })
              ])
            []
      let collisions = validateNoSanitizationCollisions "test_fn" params
      length collisions `shouldBe` 1

    it "detects collisions in array item properties" $ do
      let itemProps = Map.fromList
            [ ("item name", simpleParam "string")
            , ("item-name", simpleParam "string")
            ]
          itemParam = (simpleParam "object") { parameterProperties = Just itemProps }
          params = MkParameters
            (Map.fromList
              [ ("items", (simpleParam "array") { parameterItems = Just itemParam })
              ])
            []
      let collisions = validateNoSanitizationCollisions "test_fn" params
      length collisions `shouldBe` 1

  describe "sanitizeFieldNamesInText" $ do
    it "replaces original L4 field names with sanitized equivalents" $ do
      let revMap = Map.fromList
            [ ("years-of-service", "years of service")
            , ("performance-rating", "performance rating")
            ]
      sanitizeFieldNamesInText revMap "Missing value for parameter: years of service"
        `shouldBe` "Missing value for parameter: years-of-service"

    it "replaces multiple field names in the same text" $ do
      let revMap = Map.fromList
            [ ("years-of-service", "years of service")
            , ("performance-rating", "performance rating")
            ]
      sanitizeFieldNamesInText revMap "Expected years of service and performance rating"
        `shouldBe` "Expected years-of-service and performance-rating"

    it "does nothing when no names differ" $ do
      let revMap = Map.fromList [("name", "name")]
      sanitizeFieldNamesInText revMap "Missing: name" `shouldBe` "Missing: name"

    it "handles empty reverse map" $ do
      sanitizeFieldNamesInText Map.empty "some error text" `shouldBe` "some error text"

    it "replaces longer names first to avoid partial matches" $ do
      let revMap = Map.fromList
            [ ("is-a-citizen", "is a citizen")
            , ("is-a", "is a")
            ]
      sanitizeFieldNamesInText revMap "Check if is a citizen"
        `shouldBe` "Check if is-a-citizen"

  describe "prettyEvaluatorError" $ do
    it "formats InterpreterError as plain text" $
      prettyEvaluatorError (InterpreterError "something went wrong")
        `shouldBe` "something went wrong"

    it "formats RequiredParameterMissing" $
      prettyEvaluatorError (RequiredParameterMissing (ParameterMismatch 3 1))
        `shouldBe` "Required parameter missing: expected 3 parameter(s), but got 1"

    it "formats UnknownArguments" $
      prettyEvaluatorError (UnknownArguments ["foo", "bar"])
        `shouldBe` "Unknown argument(s): foo, bar"

    it "formats CannotHandleUnknownVars" $
      prettyEvaluatorError CannotHandleUnknownVars
        `shouldBe` "Cannot handle unknown variables in input"

-- | Helper to create a simple Parameter with no nested properties.
simpleParam :: Text -> Parameter
simpleParam ty = Parameter ty Nothing Nothing [] "" Nothing Nothing Nothing

