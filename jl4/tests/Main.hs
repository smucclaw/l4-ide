module Main where

import Base
import L4.Main

import System.FilePath
import System.FilePath.Glob
import System.IO.Silently
import Test.Hspec
import Test.Hspec.Golden

import Paths_jl4

main :: IO ()
main = do
  dataDir <- getDataDir
  exampleSimalaFiles <- globDir1 (compile "*.l4") (dataDir </> "examples")
  hspec $ forM_ exampleSimalaFiles $ \ inputFile -> do
    describe inputFile $
      it "compiles with correct output" $ do
        l4Golden (dataDir </> "examples") inputFile
        

l4Golden :: String -> String -> IO (Golden String)
l4Golden dir inputFile = do
  firstLine <- take 1 . lines <$> readFile inputFile
  let
    extraFiles =
      case firstLine of
        []                        -> []
        [l] | take 5 l == "-- ! " -> ((dir </>) <$> words (drop 5 l))
        _                         -> []
  (output_, _) <- capture (parseFiles (extraFiles ++ [inputFile]))
  pure
    Golden
      { output = output_
      , encodePretty = show
      , writeToFile = writeFile
      , readFromFile = readFile
      , goldenFile = dir </> "tests" </> (takeFileName inputFile -<.> "golden")
      , actualFile = Just (dir </> "tests" </> (takeFileName inputFile -<.> "actual"))
      , failFirstTime = False
      }
