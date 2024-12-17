module Main where

import Base
import L4.Main
import qualified L4.Parser as Parser
import qualified L4.ExactPrint as JL4
import Paths_jl4

import System.FilePath
import System.FilePath.Glob
import System.IO.Silently
import Test.Hspec
import Test.Hspec.Golden
import qualified Data.Text.IO as Text
import qualified Data.Text as Text

main :: IO ()
main = do
  dataDir <- getDataDir
  exampleSimalaFiles <- globDir1 (compile "*.l4") (dataDir </> "examples")
  hspec $ do
    forM_ exampleSimalaFiles $ \ inputFile -> do
      describe (takeFileName inputFile) $
        it "compiles with correct output" $ do
          l4Golden (dataDir </> "examples") inputFile

    forM_ exampleSimalaFiles $ \ inputFile -> do
      describe inputFile $
        it "exactprints" $ do
          jl4ExactPrintGolden (dataDir </> "examples") inputFile

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

jl4ExactPrintGolden :: String -> String -> IO (Golden Text)
jl4ExactPrintGolden dir inputFile = do
  input <- Text.readFile inputFile
  let output_ = case Parser.execParser Parser.program inputFile input of
        Left err -> Text.unlines $ fmap (.message) $ toList err
        Right prog -> case JL4.exactprint prog of
          Left epError -> JL4.prettyEPError epError
          Right ep -> ep
  pure
    Golden
      { output = output_
      , encodePretty = Text.unpack
      , writeToFile = Text.writeFile
      , readFromFile = Text.readFile
      , goldenFile = dir </> "tests" </> (takeFileName inputFile -<.> "ep.golden")
      , actualFile = Just (dir </> "tests" </> (takeFileName inputFile -<.> "ep.actual"))
      , failFirstTime = False
      }
