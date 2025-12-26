{-# LANGUAGE OverloadedStrings #-}

module DiskLoadingSpec (spec) where

import Test.Hspec

import Control.Exception (bracket)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (createDirectory, getTemporaryDirectory, removeDirectoryRecursive, removeFile)
import System.FilePath ((</>))
import System.IO (hClose, openTempFile)

import qualified Examples

spec :: Spec
spec = describe "disk loading" do
  it "prefers YAML sidecar over implicit default export" do
    withTempDir \dir -> do
      let
        l4Path = dir </> "example.l4"
        yamlPath = dir </> "example.yaml"

      TIO.writeFile l4Path $
        T.unlines
          [ "GIVETH A BOOLEAN"
          , "DECIDE first IS TRUE"
          , ""
          , "GIVETH A BOOLEAN"
          , "DECIDE chosen IS FALSE"
          ]

      TIO.writeFile yamlPath $
        T.unlines
          [ "type: function"
          , "function:"
          , "  supportedBackends: [jl4]"
          , "  name: chosen"
          , "  description: Prefer YAML over implicit export"
          , "  parameters:"
          , "    type: object"
          , "    required: []"
          , "    properties: {}"
          ]

      (functions, _ctx) <- Examples.loadL4Functions [l4Path]
      Map.keys functions `shouldBe` ["chosen"]

  it "loads only explicit @export functions when no YAML exists" do
    withTempDir \dir -> do
      let
        exportedPath = dir </> "exported.l4"
        implicitPath = dir </> "implicit.l4"

      TIO.writeFile exportedPath $
        T.unlines
          [ "@export default Exported entry"
          , "GIVETH A BOOLEAN"
          , "DECIDE exported IS TRUE"
          ]

      TIO.writeFile implicitPath $
        T.unlines
          [ "GIVETH A BOOLEAN"
          , "DECIDE implicit IS TRUE"
          ]

      (functions, _ctx) <- Examples.loadL4Functions [exportedPath, implicitPath]
      Map.keys functions `shouldBe` ["exported"]

  describe "@export placement" do
    it "works when @export is before GIVEN" do
      withTempDir \dir -> do
        let l4Path = dir </> "export-before-given.l4"
        TIO.writeFile l4Path $
          T.unlines
            [ "@export default Export before GIVEN"
            , "GIVEN x IS A BOOLEAN"
            , "GIVETH A BOOLEAN"
            , "DECIDE test_before_given IS x"
            ]
        (functions, _ctx) <- Examples.loadL4Functions [l4Path]
        Map.keys functions `shouldBe` ["test_before_given"]

    it "works when @export is before GIVETH (no GIVEN clause)" do
      withTempDir \dir -> do
        let l4Path = dir </> "export-before-giveth.l4"
        TIO.writeFile l4Path $
          T.unlines
            [ "@export default Export before GIVETH"
            , "GIVETH A BOOLEAN"
            , "DECIDE test_before_giveth IS TRUE"
            ]
        (functions, _ctx) <- Examples.loadL4Functions [l4Path]
        Map.keys functions `shouldBe` ["test_before_giveth"]

    it "works when @export is before MEANS" do
      withTempDir \dir -> do
        let l4Path = dir </> "export-before-means.l4"
        TIO.writeFile l4Path $
          T.unlines
            [ "@export default Export before MEANS"
            , "GIVEN x IS A BOOLEAN"
            , "test_with_means x MEANS x"
            ]
        (functions, _ctx) <- Examples.loadL4Functions [l4Path]
        Map.keys functions `shouldBe` ["test_with_means"]

    it "does NOT work when @export is between GIVEN and GIVETH" do
      withTempDir \dir -> do
        let l4Path = dir </> "export-between-given-giveth.l4"
        TIO.writeFile l4Path $
          T.unlines
            [ "GIVEN x IS A BOOLEAN"
            , "@export default Export between GIVEN and GIVETH"
            , "GIVETH A BOOLEAN"
            , "DECIDE test_between IS x"
            ]
        (functions, _ctx) <- Examples.loadL4Functions [l4Path]
        Map.keys functions `shouldBe` []

    it "does NOT work when @export is between GIVETH and DECIDE" do
      withTempDir \dir -> do
        let l4Path = dir </> "export-after-giveth.l4"
        TIO.writeFile l4Path $
          T.unlines
            [ "GIVEN x IS A BOOLEAN"
            , "GIVETH A BOOLEAN"
            , "@export default Export after GIVETH"
            , "DECIDE test_after_giveth IS x"
            ]
        (functions, _ctx) <- Examples.loadL4Functions [l4Path]
        Map.keys functions `shouldBe` []

    it "does NOT work when @export is right before DECIDE (after GIVETH)" do
      withTempDir \dir -> do
        let l4Path = dir </> "export-before-decide.l4"
        TIO.writeFile l4Path $
          T.unlines
            [ "GIVEN x IS A BOOLEAN"
            , "GIVETH A BOOLEAN"
            , "@export default Export right before DECIDE"
            , "DECIDE test_before_decide IS x"
            ]
        (functions, _ctx) <- Examples.loadL4Functions [l4Path]
        Map.keys functions `shouldBe` []

withTempDir :: (FilePath -> IO a) -> IO a
withTempDir k = do
  tmp <- getTemporaryDirectory
  (path, h) <- openTempFile tmp "jl4-decision-service-test"
  hClose h
  removeFile path
  createDirectory path
  bracket (pure path) removeDirectoryRecursive k
