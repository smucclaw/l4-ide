module Main where

import Base
import qualified L4.Annotation as JL4
import qualified L4.Evaluate as JL4
import qualified L4.Parser.SrcSpan as JL4
import qualified L4.Main as JL4
import L4.Parser (execProgramParser)
import qualified L4.Parser as Parser
import qualified L4.Parser.ResolveAnnotation as Parser
import qualified L4.Print as Print
import L4.Syntax
import L4.TypeCheck (CheckResult (CheckResult))
import qualified L4.TypeCheck as JL4
import Paths_jl4

import qualified Base.Text as Text
import Optics
import System.FilePath
import System.FilePath.Glob
import System.IO.Silently
import Test.Hspec
import Test.Hspec.Golden

main :: IO ()
main = do
  dataDir <- getDataDir
  let examplesRoot = dataDir </> "examples"
  exampleFiles <- sort <$> globDir1 (compile "**/*.l4") examplesRoot
  hspec $ do
    forM_ exampleFiles $ \ inputFile -> do
      let testCase  = makeRelative examplesRoot inputFile
      let goldenDir = takeDirectory inputFile </> "tests"
      describe testCase $ do
        it "parses and checks" $
          l4Golden goldenDir inputFile
        it "exactprints" $
          jl4ExactPrintGolden goldenDir inputFile
        it "natural language annotations" $
          jl4NlgAnnotationsGolden goldenDir inputFile

l4Golden :: String -> String -> IO (Golden String)
l4Golden dir inputFile = do
  (output_, _) <- capture (readAndParseFile inputFile)
  pure
    Golden
      { output = output_
      , encodePretty = show
      , writeToFile = writeFile
      , readFromFile = readFile
      , goldenFile = dir </> (takeFileName inputFile -<.> "golden")
      , actualFile = Just (dir </> (takeFileName inputFile -<.> "actual"))
      , failFirstTime = False
      }

jl4ExactPrintGolden :: String -> String -> IO (Golden Text)
jl4ExactPrintGolden dir inputFile = do
  input <- Text.readFile inputFile
  let output_ = JL4.exactprintProgram (takeFileName inputFile) input
  pure
    Golden
      { output = output_
      , encodePretty = Text.unpack
      , writeToFile = Text.writeFile
      , readFromFile = Text.readFile
      , goldenFile = dir </> (takeFileName inputFile -<.> "ep.golden")
      , actualFile = Just (dir </> (takeFileName inputFile -<.> "ep.actual"))
      , failFirstTime = False
      }

jl4NlgAnnotationsGolden :: String -> String -> IO (Golden Text)
jl4NlgAnnotationsGolden dir inputFile = do
  input <- Text.readFile inputFile
  let output_ = case execProgramParser (takeFileName inputFile) input of
        Left _err -> "Failed to parse"
        Right (prog, warns) -> prettyNlgOutput prog warns
  pure
    Golden
      { output = output_
      , encodePretty = Text.unpack
      , writeToFile = Text.writeFile
      , readFromFile = Text.readFile
      , goldenFile = dir </> (takeFileName inputFile -<.> "nlg.golden")
      , actualFile = Just (dir </> (takeFileName inputFile -<.> "nlg.actual"))
      , failFirstTime = False
      }

-- ----------------------------------------------------------------------------
-- Test helpers
-- ----------------------------------------------------------------------------

-- TODO: This function should be unified / merged with checkAndExactPrintFile from L4.TypeCheck
parseFile :: String -> Text -> IO ()
parseFile file input =
  case Parser.execProgramParser fp input of
    Left errs -> Text.putStr $ Text.unlines $ fmap (.message) (toList errs)
    Right (prog, _) -> do
      Text.putStrLn "Parsing successful"
      case JL4.doCheckProgram prog of
        CheckResult {errors, program}
          | all ((== JL4.SInfo) . JL4.severity) errors -> do
            Text.putStrLn "Typechecking successful"
            let results = JL4.doEvalProgram program
            let msgs = (typeErrorToMessage <$> errors) ++ (evalResultToMessage <$> results)
            Text.putStr (Text.unlines (renderMessage <$> sortOn fst msgs))
          | otherwise -> do
            let msgs = typeErrorToMessage <$> errors
            Text.putStr (Text.unlines (renderMessage <$> sortOn fst msgs))
  where
    fp = takeFileName file
    typeErrorToMessage err = (JL4.rangeOf err, JL4.prettyCheckErrorWithContext err)
    evalResultToMessage (r, res) = (Just r, [either Text.show Print.prettyLayout res])
    renderMessage (r, txt) = JL4.cliErrorMessage fp r txt

readAndParseFile :: FilePath -> IO ()
readAndParseFile file = do
  input <- Text.readFile file
  parseFile file input

prettyNlgOutput :: Program Name -> [Parser.Warning] -> Text
prettyNlgOutput p warns = Text.unlines $
  [ prettyNlgName n nlg
  | n <- toListOf gplate p
  , Just nlg <- [n ^. annoOf % annNlg]
  ]
  <>
  [prettyWarning warning | warning <- warns]
  where
    prettyNlgName :: Name -> Nlg -> Text
    prettyNlgName name nlg = mconcat
      [ prettyName name
      , "\n  "
      , prettyNlg nlg
      ]

    prettyName name = mconcat
      [ prettyMaybeSrcRange (JL4.rangeOf name)
      , " "
      , Print.prettyLayout name
      ]

    prettyMaybeSrcRange :: Maybe JL4.SrcRange -> Text
    prettyMaybeSrcRange srcRange = "[" <> JL4.prettySrcRange Nothing srcRange <> "]"

    prettyNlg :: Nlg -> Text
    prettyNlg n = mconcat
      [ prettyMaybeSrcRange (JL4.rangeOf n)
      , " "
      , Print.prettyLayout n
      ]

    prettyWarning = \case
      Parser.NotAttached nlg ->
        "Not attached to any node:\n  " <> prettyNlg nlg.payload
      Parser.UnknownLocation nlg ->
        "Annotation without location:\n  " <> prettyNlg nlg
      Parser.Ambiguous n nlgs -> Text.unlines
        [ "Too many annotations:"
        , Text.replicate 2 " " <> "Name: " <> prettyName n
        , Text.unlines  $
          [ Text.replicate 4 " " <> prettyNlg nlg.payload
          | nlg <- nlgs
          ]
        ]
