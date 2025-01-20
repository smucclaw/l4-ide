module Main where

import Base
import qualified L4.Parser as Parser
import qualified L4.ExactPrint as JL4
import qualified L4.TypeCheck as JL4
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
      describe (takeFileName inputFile) $ do
        it "parses and checks" $
          l4Golden (dataDir </> "examples") inputFile
        it "exactprints" $
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
  let output_ = JL4.exactprintFile (takeFileName inputFile) input
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

-- ----------------------------------------------------------------------------
-- Test helpers
-- ----------------------------------------------------------------------------

parseFile :: String -> Text -> IO ()
parseFile file input =
  case Parser.execParser Parser.program (takeFileName file) input of
    Left errs -> Text.putStr $ Text.unlines $ fmap (.message) (toList errs)
    Right prog -> do
      Text.putStrLn "Parsing successful"
      case JL4.doCheckProgram prog of
        ([], _p, _s) -> Text.putStrLn "Typechecking successful"
        (errs, _p, _s) ->
          Text.putStr (Text.unlines (map (\ err -> JL4.prettySrcRange (JL4.rangeOf err) <> ":\n" <> JL4.prettyCheckErrorWithContext err) errs))

parseFiles :: [FilePath] -> IO ()
parseFiles =
  traverse_ (\ file -> parseFile file =<< Text.readFile file)
