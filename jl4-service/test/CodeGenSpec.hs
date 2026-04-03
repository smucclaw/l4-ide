{-# LANGUAGE OverloadedStrings #-}

module CodeGenSpec (spec) where

import Test.Hspec

import qualified Data.Aeson as Aeson
import Backend.CodeGen (inputFieldName, transformJsonKeys)

spec :: SpecWith ()
spec = describe "CodeGen" $ do
  describe "inputFieldName" $ do
    it "adds (input) suffix to simple names" $ do
      inputFieldName "x" `shouldBe` "x (input)"
      inputFieldName "value" `shouldBe` "value (input)"

    it "adds (input) suffix to names with spaces" $ do
      inputFieldName "musk service role" `shouldBe` "musk service role (input)"
      inputFieldName "first value" `shouldBe` "first value (input)"

    it "handles empty string" $ do
      inputFieldName "" `shouldBe` " (input)"

  describe "transformJsonKeys" $ do
    it "transforms top-level object keys" $ do
      let input = Aeson.object
            [ ("x", Aeson.Number 1)
            , ("y", Aeson.Number 2)
            ]
          expected = Aeson.object
            [ ("x (input)", Aeson.Number 1)
            , ("y (input)", Aeson.Number 2)
            ]
      transformJsonKeys input `shouldBe` expected

    it "transforms keys with spaces" $ do
      let input = Aeson.object
            [ ("musk service role", Aeson.String "CEO")
            , ("board approved", Aeson.Bool True)
            ]
          expected = Aeson.object
            [ ("musk service role (input)", Aeson.String "CEO")
            , ("board approved (input)", Aeson.Bool True)
            ]
      transformJsonKeys input `shouldBe` expected

    it "does not transform nested object keys" $ do
      let input = Aeson.object
            [ ("person", Aeson.object
                [ ("name", Aeson.String "Alice")
                , ("age", Aeson.Number 30)
                ])
            ]
          expected = Aeson.object
            [ ("person (input)", Aeson.object
                [ ("name", Aeson.String "Alice")
                , ("age", Aeson.Number 30)
                ])
            ]
      transformJsonKeys input `shouldBe` expected

    it "passes through non-object values unchanged" $ do
      transformJsonKeys (Aeson.String "test") `shouldBe` Aeson.String "test"
      transformJsonKeys (Aeson.Number 42) `shouldBe` Aeson.Number 42
      transformJsonKeys (Aeson.Bool True) `shouldBe` Aeson.Bool True
      transformJsonKeys Aeson.Null `shouldBe` Aeson.Null

    it "passes through arrays unchanged" $ do
      let input = Aeson.toJSON [1 :: Int, 2, 3]
      transformJsonKeys input `shouldBe` input
