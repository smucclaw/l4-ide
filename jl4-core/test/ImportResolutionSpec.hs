{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
module ImportResolutionSpec (spec) where

import Test.Hspec
import Data.Text (Text)
import qualified Data.Text as Text
import L4.Wasm.Import
import qualified Data.Map.Strict as Map

spec :: Spec
spec = describe "Import Resolution" $ do
  describe "Embedded Libraries" $ do
    it "has embedded libraries" $ do
      Map.size embeddedLibraries `shouldSatisfy` (> 0)
    
    it "includes prelude" $ do
      lookupEmbeddedLibrary "prelude" `shouldSatisfy` \case
        Just _ -> True
        Nothing -> False
    
    it "includes math" $ do
      lookupEmbeddedLibrary "math" `shouldSatisfy` \case
        Just _ -> True
        Nothing -> False

  describe "Type checking without imports" $ do
    it "type-checks a simple declaration" $ do
      let result = checkWithImports emptyVFS "DECLARE x IS A NUMBER"
      case result of
        Left errs -> fail $ "Type check failed: " ++ show errs
        Right r -> r.tcdSuccess `shouldBe` True

  describe "Type checking with imports" $ do
    it "resolves prelude import" $ do
      let result = checkWithImports emptyVFS "IMPORT prelude\nDECLARE x IS A NUMBER"
      case result of
        Left errs -> fail $ "Type check failed: " ++ show errs
        Right r -> do
          r.tcdSuccess `shouldBe` True
          length r.tcdResolvedImports `shouldBe` 1

    it "uses functions from prelude" $ do
      let source :: Text
          source = Text.unlines
            [ "IMPORT prelude"
            , ""
            , "GIVEN xs IS A LIST OF NUMBER"
            , "GIVETH A NUMBER"
            , "total xs MEANS sum xs"
            ]
      let result = checkWithImports emptyVFS source
      case result of
        Left errs -> fail $ "Type check failed: " ++ show errs
        Right r -> r.tcdSuccess `shouldBe` True

    it "resolves transitive imports (math -> prelude)" $ do
      let source :: Text
          source = Text.unlines
            [ "IMPORT math"
            , ""
            , "DECIDE x IS exp 1"
            ]
      let result = checkWithImports emptyVFS source
      case result of
        Left errs -> fail $ "Type check failed: " ++ show errs
        Right r -> do
          r.tcdSuccess `shouldBe` True
          -- Should have resolved both math and prelude
          length r.tcdResolvedImports `shouldSatisfy` (>= 2)

    it "resolves diamond imports (currency -> jurisdiction, prelude; jurisdiction -> prelude)" $ do
      let source :: Text
          source = Text.unlines
            [ "IMPORT currency"
            , ""
            , "DECIDE x IS 42"
            ]
      let result = checkWithImports emptyVFS source
      case result of
        Left errs -> fail $ "Type check failed: " ++ show errs
        Right r -> do
          r.tcdSuccess `shouldBe` True
          -- Should have resolved currency, jurisdiction, prelude (no duplicates)
          length r.tcdResolvedImports `shouldSatisfy` (>= 3)

  describe "VFS lookup" $ do
    it "finds files in VFS" $ do
      let vfs = vfsFromList [("helper", "DECLARE y IS A NUMBER")]
      vfsLookup "helper" vfs `shouldSatisfy` \case
        Just _ -> True
        Nothing -> False

    it "resolves imports from VFS" $ do
      let vfs = vfsFromList [("helper", "DECLARE y IS A NUMBER")]
      let source = "IMPORT helper\nDECLARE x IS A NUMBER"
      let result = checkWithImports vfs source
      case result of
        Left errs -> fail $ "Type check failed: " ++ show errs
        Right r -> do
          r.tcdSuccess `shouldBe` True
          length r.tcdResolvedImports `shouldBe` 1

  describe "Error handling" $ do
    it "reports missing module errors" $ do
      let result = checkWithImports emptyVFS "IMPORT nonexistent\nDECLARE x IS A NUMBER"
      case result of
        Left errs -> errs `shouldSatisfy` \es -> any (Text.isInfixOf "not found") es
        Right _ -> fail "Expected an error for missing module"
