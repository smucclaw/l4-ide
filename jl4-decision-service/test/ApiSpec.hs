{-# LANGUAGE OverloadedStrings #-}

module ApiSpec (spec) where

import Backend.Api
import Data.Aeson (decode)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Types as Aeson
import Test.Hspec

spec :: Spec
spec = do
  describe "FnLiteral JSON parsing" $ do
    it "should preserve JSON String type (not coerce to number)" $ do
      -- Bug report: "5" was being parsed as FnLitInt 5
      let jsonString = "{\"value\": \"5\"}"
      let parsed = decode jsonString :: Maybe Aeson.Object
      case parsed of
        Nothing -> expectationFailure "Failed to parse JSON"
        Just obj -> do
          case KeyMap.lookup "value" obj of
            Nothing -> expectationFailure "Missing 'value' field"
            Just val -> do
              let literal = Aeson.parseMaybe Aeson.parseJSON val :: Maybe FnLiteral
              literal `shouldBe` Just (FnLitString "5")

    it "should parse JSON Number as FnLitInt" $ do
      let jsonNumber = "{\"value\": 5}"
      let parsed = decode jsonNumber :: Maybe Aeson.Object
      case parsed of
        Nothing -> expectationFailure "Failed to parse JSON"
        Just obj -> do
          case KeyMap.lookup "value" obj of
            Nothing -> expectationFailure "Missing 'value' field"
            Just val -> do
              let literal = Aeson.parseMaybe Aeson.parseJSON val :: Maybe FnLiteral
              literal `shouldBe` Just (FnLitInt 5)

    it "should keep 'five' as FnLitString" $ do
      let jsonString = "{\"value\": \"five\"}"
      let parsed = decode jsonString :: Maybe Aeson.Object
      case parsed of
        Nothing -> expectationFailure "Failed to parse JSON"
        Just obj -> do
          case KeyMap.lookup "value" obj of
            Nothing -> expectationFailure "Missing 'value' field"
            Just val -> do
              let literal = Aeson.parseMaybe Aeson.parseJSON val :: Maybe FnLiteral
              literal `shouldBe` Just (FnLitString "five")

    it "should parse JSON Number with decimal as FnLitDouble" $ do
      let jsonNumber = "{\"value\": 5.5}"
      let parsed = decode jsonNumber :: Maybe Aeson.Object
      case parsed of
        Nothing -> expectationFailure "Failed to parse JSON"
        Just obj -> do
          case KeyMap.lookup "value" obj of
            Nothing -> expectationFailure "Missing 'value' field"
            Just val -> do
              let literal = Aeson.parseMaybe Aeson.parseJSON val :: Maybe FnLiteral
              literal `shouldBe` Just (FnLitDouble 5.5)

    it "should keep '5.5' string as FnLitString (not convert to double)" $ do
      let jsonString = "{\"value\": \"5.5\"}"
      let parsed = decode jsonString :: Maybe Aeson.Object
      case parsed of
        Nothing -> expectationFailure "Failed to parse JSON"
        Just obj -> do
          case KeyMap.lookup "value" obj of
            Nothing -> expectationFailure "Missing 'value' field"
            Just val -> do
              let literal = Aeson.parseMaybe Aeson.parseJSON val :: Maybe FnLiteral
              literal `shouldBe` Just (FnLitString "5.5")

    it "should preserve large integers beyond Int bounds" $ do
      -- Test that integers larger than maxBound :: Int are preserved
      let largeInt = 9999999999999999999 :: Integer
      let jsonNumber = "{\"value\": 9999999999999999999}"
      let parsed = decode jsonNumber :: Maybe Aeson.Object
      case parsed of
        Nothing -> expectationFailure "Failed to parse JSON"
        Just obj -> do
          case KeyMap.lookup "value" obj of
            Nothing -> expectationFailure "Missing 'value' field"
            Just val -> do
              let literal = Aeson.parseMaybe Aeson.parseJSON val :: Maybe FnLiteral
              literal `shouldBe` Just (FnLitInt largeInt)

  describe "FnLiteral FromHttpApiData (query parameters)" $ do
    it "should infer type from query parameter string" $ do
      -- Query parameters don't have type information, so inference is needed
      parseTextAsFnLiteral "5" `shouldBe` FnLitInt 5
      parseTextAsFnLiteral "5.5" `shouldBe` FnLitDouble 5.5
      parseTextAsFnLiteral "true" `shouldBe` FnLitBool True
      parseTextAsFnLiteral "false" `shouldBe` FnLitBool False
      parseTextAsFnLiteral "five" `shouldBe` FnLitString "five"

    it "should parse quoted strings as FnLitString" $ do
      parseTextAsFnLiteral "\"five\"" `shouldBe` FnLitString "five"
      parseTextAsFnLiteral "\"5\"" `shouldBe` FnLitString "5"
