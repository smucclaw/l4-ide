{-# LANGUAGE ViewPatterns #-}
module Main where

import Base
import Control.Monad.Trans.Maybe
import qualified L4.Annotation as JL4
import qualified L4.Evaluate as JL4
import L4.Parser (execProgramParser)
import qualified L4.Parser as Parser
import qualified L4.Parser.ResolveAnnotation as Parser
import qualified L4.Parser.SrcSpan as JL4
import qualified L4.Print as Print
import L4.Syntax
import L4.TypeCheck (CheckResult(..))
import qualified L4.TypeCheck as JL4
import Paths_jl4

import qualified Base.Text as Text
import qualified LSP.Core.Shake as Shake
import LSP.L4.Oneshot (oneshotL4ActionAndErrors)
import qualified LSP.L4.Rules as Rules
import Language.LSP.Protocol.Types
import Optics
import System.FilePath
import System.FilePath.Glob
import System.IO.Silently
import Test.Hspec
import Test.Hspec.Golden
import qualified Regex.Text as RE
import qualified Data.CharSet as CharSet
import qualified System.OsPath as OsPath
import LSP.L4.Rules

main :: IO ()
main = do
  dataDir <- getDataDir
  let examplesRoot = dataDir </> "examples"
  okFiles <- sort <$> globDir1 (compile "ok/**/*.l4") examplesRoot
  legalFiles <- sort <$> globDir1 (compile "legal/**/*.l4") examplesRoot
  tcFailsFiles <- sort <$> globDir1 (compile "not-ok/tc/**/*.l4") examplesRoot
  nlgFailsFiles <- sort <$> globDir1 (compile "not-ok/nlg/**/*.l4") examplesRoot
  hspec do
    describe "ok files" $ tests (True, True) (okFiles <> legalFiles) examplesRoot
    describe "tc fails" $ tests (False, True) tcFailsFiles examplesRoot
    describe "nlg fails" $ tests (True, False) nlgFailsFiles examplesRoot
  where
    tests (tcOk, nlgOk) files root =
      forM_ files $ \inputFile -> do
        let testCase = makeRelative root inputFile
        let goldenDir = takeDirectory inputFile </> "tests"
        describe testCase $ do
          it "parses and checks" $
            l4Golden tcOk goldenDir inputFile
          it "exactprints" $
            jl4ExactPrintGolden goldenDir inputFile
          it "natural language annotations" $
            jl4NlgAnnotationsGolden nlgOk goldenDir inputFile

l4Golden :: Bool -> String -> String -> IO (Golden String)
l4Golden isOk dir inputFile = do
  (output_, _) <- capture (checkFile isOk inputFile)
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
  (errs, moutput) <- oneshotL4ActionAndErrors inputFile \nfp -> do
    let uri = normalizedFilePathToUri nfp
    _ <- Shake.addVirtualFileFromFS nfp
    Shake.use Rules.ExactPrint uri

  -- NOTE: we sort the output, because the traces are concurrent and might not be in order
  let output = fromMaybe (sanitizeFilePaths $ mconcat errs) moutput

  pure
    Golden
      { output
      , encodePretty = Text.unpack
      , writeToFile = Text.writeFile
      , readFromFile = Text.readFile
      , goldenFile = dir </> (takeFileName inputFile -<.> "ep.golden")
      , actualFile = Just (dir </> (takeFileName inputFile -<.> "ep.actual"))
      , failFirstTime = False
      }

jl4NlgAnnotationsGolden :: Bool -> String -> FilePath -> IO (Golden Text)
jl4NlgAnnotationsGolden isOk dir inputFile = do
  input <- Text.readFile inputFile
  output_ <- case execProgramParser (toNormalizedUri $ filePathToUri inputFile) input of
    Left _err -> pure "Failed to parse"
    Right (prog, warns) -> do
      null warns `shouldBe` isOk
      pure $ prettyNlgOutput prog warns
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

sanitizeFilePaths :: Text -> Text
sanitizeFilePaths = RE.replaceAll regex
  where
  mkFileName (Text.unpack -> fp) = Text.pack $ ' ' : case OsPath.decodeUtf . OsPath.takeFileName =<< OsPath.encodeUtf fp of
    Just p | not (null p) -> p
    _ -> fp

  regex = fmap mkFileName $
    RE.manyTextOf CharSet.space *> RE.text "file://" *>
      RE.manyTextOf (CharSet.not $ CharSet.space `CharSet.union` CharSet.singleton ':')

checkFile :: Bool -> FilePath -> IO ()
checkFile isOk file = do
  (errs, isJust ->  success) <- oneshotL4ActionAndErrors file \nfp -> runMaybeT do
      let uri = normalizedFilePathToUri nfp
      _       <- lift   $ Shake.addVirtualFileFromFS nfp
      _       <- MaybeT $ Shake.use GetParsedAst uri        <* liftIO (Text.putStrLn "Parsing successful")
      checked <- MaybeT $ Shake.use SuccessfulTypeCheck uri <* liftIO (Text.putStrLn "Typechecking successful")
      results <- MaybeT $ Shake.use Evaluate uri            <* liftIO (Text.putStrLn "Evaluation successful")
      let msgs = map typeErrorToMessage checked.infos <> map evalDirectiveResultToMessage results
          formatted = foldMap (sanitizeFilePaths . renderMessage) $ sortOn fst msgs
      liftIO $ Text.putStr formatted
  -- NOTE: if we're okay, we don't expect any errors, if we are not, we do expect them
  success `shouldBe` isOk
  unless success do
    Text.putStr $ foldMap sanitizeFilePaths errs

 where
  fp = takeFileName file
  typeErrorToMessage err = (JL4.rangeOf err, JL4.prettyCheckErrorWithContext err)
  evalDirectiveResultToMessage (JL4.MkEvalDirectiveResult r res _) = (Just r, [either Text.show Print.prettyLayout res])
  renderMessage (r, txt) = cliErrorMessage fp r txt

data CliError
  = CliParserError FilePath (NonEmpty Parser.PError)
  | CliCheckError FilePath CheckResult
  deriving stock (Show, Eq)

prettyCliError :: CliError -> Text
prettyCliError = \case
  CliParserError file perrors ->
    "While parsing "
      <> Text.pack file
      <> ":"
      <> Text.unlines (((.message)) <$> toList perrors)
  CliCheckError file MkCheckResult{errors} ->
    Text.unlines (map (\err -> cliErrorMessage file (JL4.rangeOf err) (JL4.prettyCheckErrorWithContext err)) errors)

cliErrorMessage :: FilePath -> Maybe JL4.SrcRange -> [Text] -> Text
cliErrorMessage fp mrange msg =
  Text.unlines
    ( JL4.prettySrcRange (Just fp) mrange <> ":"
        : map ("  " <>) msg
    )

prettyNlgOutput :: Module Name -> [Parser.Warning] -> Text
prettyNlgOutput p warns =
  Text.unlines $
    [ prettyNlgName n nlg
    | n <- toListOf gplate p
    , Just nlg <- [n ^? JL4.annoOf % #extra % #nlg % _Just]
    ]
      <> [prettyWarning warning | warning <- warns]
 where
  prettyNlgName :: Name -> Nlg -> Text
  prettyNlgName name nlg =
    mconcat
      [ prettyName name
      , "\n  "
      , prettyNlg nlg
      ]

  prettyName name =
    mconcat
      [ prettyMaybeSrcRange (JL4.rangeOf name)
      , " "
      , Print.prettyLayout name
      ]

  prettyMaybeSrcRange :: Maybe JL4.SrcRange -> Text
  prettyMaybeSrcRange srcRange = "[" <> JL4.prettySrcRange Nothing srcRange <> "]"

  prettyNlg :: Nlg -> Text
  prettyNlg n =
    mconcat
      [ prettyMaybeSrcRange (JL4.rangeOf n)
      , " "
      , Print.prettyLayout n
      ]

  prettyWarning = \case
    Parser.NotAttached nlg ->
      "Not attached to any node:\n  " <> prettyNlg nlg.payload
    Parser.UnknownLocation nlg ->
      "Annotation without location:\n  " <> prettyNlg nlg
    Parser.Ambiguous n nlgs ->
      Text.unlines
        [ "Too many annotations:"
        , Text.replicate 2 " " <> "Name: " <> prettyName n
        , Text.unlines $
            [ Text.replicate 4 " " <> prettyNlg nlg.payload
            | nlg <- nlgs
            ]
        ]
