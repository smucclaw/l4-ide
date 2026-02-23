{-# LANGUAGE OverloadedStrings #-}

module BundleStoreSpec (spec) where

import Test.Hspec

import BundleStore
import qualified Data.Map.Strict as Map
import System.Directory (removeDirectoryRecursive, doesDirectoryExist)

spec :: Spec
spec = describe "BundleStore" do
  around withTempStore do
    describe "initStore" do
      it "creates the store directory" \store -> do
        exists <- doesDirectoryExist store.storePath
        exists `shouldBe` True

    describe "saveBundle and loadBundle" do
      it "round-trips sources and metadata" \store -> do
        let sources = Map.fromList
              [ ("main.l4", "DECIDE f IS TRUE")
              , ("helper.l4", "DECIDE g IS FALSE")
              ]
            meta = StoredMetadata
              { smFunctions = [StoredFunctionSummary "f" "a function"]
              , smVersion = "abc123"
              , smCreatedAt = "2025-01-01T00:00:00Z"
              }
        saveBundle store "test-deploy" sources meta
        (loadedSources, loadedMeta) <- loadBundle store "test-deploy"
        loadedSources `shouldBe` sources
        loadedMeta.smVersion `shouldBe` "abc123"
        loadedMeta.smCreatedAt `shouldBe` "2025-01-01T00:00:00Z"
        length loadedMeta.smFunctions `shouldBe` 1

      it "overwrites existing deployment atomically" \store -> do
        let sources1 = Map.singleton "main.l4" "DECIDE f IS TRUE"
            sources2 = Map.singleton "main.l4" "DECIDE f IS FALSE"
            meta1 = StoredMetadata [] "v1" "2025-01-01T00:00:00Z"
            meta2 = StoredMetadata [] "v2" "2025-01-02T00:00:00Z"
        saveBundle store "deploy-overwrite" sources1 meta1
        saveBundle store "deploy-overwrite" sources2 meta2
        (loadedSources, loadedMeta) <- loadBundle store "deploy-overwrite"
        loadedSources `shouldBe` sources2
        loadedMeta.smVersion `shouldBe` "v2"

    describe "listDeployments" do
      it "lists saved deployments" \store -> do
        let meta = StoredMetadata [] "v1" "2025-01-01T00:00:00Z"
        saveBundle store "deploy-a" (Map.singleton "a.l4" "DECIDE a IS TRUE") meta
        saveBundle store "deploy-b" (Map.singleton "b.l4" "DECIDE b IS TRUE") meta
        deployIds <- listDeployments store
        length deployIds `shouldBe` 2
        deployIds `shouldContain` ["deploy-a"]
        deployIds `shouldContain` ["deploy-b"]

      it "returns empty list for fresh store" \store -> do
        deployIds <- listDeployments store
        deployIds `shouldBe` []

    describe "deleteBundle" do
      it "removes a deployment from disk" \store -> do
        let meta = StoredMetadata [] "v1" "2025-01-01T00:00:00Z"
        saveBundle store "to-delete" (Map.singleton "main.l4" "DECIDE f IS TRUE") meta
        deleteBundle store "to-delete"
        deployIds <- listDeployments store
        deployIds `shouldNotContain` ["to-delete"]

      it "is idempotent for non-existent deployments" \store -> do
        -- Should not throw
        deleteBundle store "nonexistent"

-- | Create a temp store, run the test, then clean up.
withTempStore :: (BundleStore -> IO ()) -> IO ()
withTempStore action = do
  let tmpPath = "/tmp/jl4-service-test-store"
  -- Clean up any previous test run
  exists <- doesDirectoryExist tmpPath
  if exists then removeDirectoryRecursive tmpPath else pure ()
  store <- initStore tmpPath
  action store
  -- Clean up after test
  removeDirectoryRecursive tmpPath
