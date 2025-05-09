{-# LANGUAGE ViewPatterns #-}
module Main (main) where

import Base
import Control.Monad.Trans.Maybe
import qualified L4.Annotation as JL4
import qualified L4.Evaluate as JL4
import qualified L4.EvaluateLazy as JL4Lazy
import qualified L4.Nlg as Nlg
import qualified L4.Parser as Parser
import qualified L4.Parser.SrcSpan as JL4
import qualified L4.Print as Print
import L4.Syntax
import L4.TypeCheck (CheckResult(..))
import qualified L4.TypeCheck as JL4

import qualified Paths_jl4
import qualified Paths_jl4_core

import qualified Base.Text as Text
import qualified Data.List as List
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
  dataDir <- Paths_jl4.getDataDir
  dataDirCore <- Paths_jl4_core.getDataDir
  let examplesRoot = dataDir </> "examples"
  okFiles <- sort <$> globDir1 (compile "ok/**/*.l4") examplesRoot
  librariesFiles <- sort <$> globDir1 (compile "*.l4") (dataDirCore </> "libraries")
  legalFiles <- sort <$> globDir1 (compile "legal/**/*.l4") examplesRoot
  tcFailsFiles <- sort <$> globDir1 (compile "not-ok/tc/**/*.l4") examplesRoot
  nlgFailsFiles <- sort <$> globDir1 (compile "not-ok/nlg/**/*.l4") examplesRoot
  hspec do
    describe "ok files" $ tests (True, True) (okFiles <> legalFiles <> librariesFiles) examplesRoot
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
  (errs, moutput) <- oneshotL4ActionAndErrors inputFile \nfp -> do
    let uri = normalizedFilePathToUri nfp
    _ <- Shake.addVirtualFileFromFS nfp
    Shake.use Rules.SuccessfulTypeCheck uri
  let output_ = case moutput of
        Nothing -> "Cannot linearize module that doesn't typecheck\n"
        Just checkResult ->
          let
            mod' = checkResult.module'
            directives = toListOf (gplate @(Directive Resolved)) mod'
          in
            Text.unlines $ fmap Nlg.simpleLinearizer directives
  let output =
        if isOk
          then output_
          else output_ <> "\n" <> Text.unlines (fmap (Text.strip . sanitizeFilePaths) errs)
  pure
    Golden
      { output
      , encodePretty = Text.unpack
      , writeToFile = Text.writeFile
      , readFromFile = Text.readFile
      , goldenFile = dir </> takeFileName inputFile -<.> "nlg.golden"
      , actualFile = Just (dir </> takeFileName inputFile -<.> "nlg.actual")
      , failFirstTime = False
      }

-- ----------------------------------------------------------------------------
-- Test helpers
-- ----------------------------------------------------------------------------

sanitizeFilePaths :: Text -> Text
sanitizeFilePaths = RE.replaceAll regex
  where
  mkFileName (Text.null -> hadNoWhiteSpace) (Text.unpack -> fp)
    = Text.pack $ (if hadNoWhiteSpace then id else (' ' :)) case OsPath.decodeUtf . OsPath.takeFileName =<< OsPath.encodeUtf fp of
      Just p | not (null p) -> p
      _ -> fp

  regex = mkFileName <$> RE.manyTextOf CharSet.space <* RE.text "file://" <*>
      RE.manyTextOf (CharSet.not $ CharSet.space `CharSet.union` CharSet.singleton ':')

checkFile :: Bool -> FilePath -> IO ()
checkFile isOk file = do
  (errs, isJust ->  success) <- oneshotL4ActionAndErrors file \nfp -> runMaybeT do
      let uri = normalizedFilePathToUri nfp
      _        <- lift   $ Shake.addVirtualFileFromFS nfp
      _        <- MaybeT $ Shake.use GetParsedAst uri        <* liftIO (Text.putStrLn "Parsing successful")
      checked  <- MaybeT $ Shake.use SuccessfulTypeCheck uri <* liftIO (Text.putStrLn "Typechecking successful")
      results  <- MaybeT $ Shake.use Evaluate uri
      when (not (null results)) $ liftIO $ Text.putStrLn "Eager evaluation successful"
      results' <- MaybeT $ Shake.use EvaluateLazy uri
      when (not (null results')) $ liftIO $ Text.putStrLn "Evaluation successful"
      let msgs  =    map typeErrorToMessage checked.infos
                  <> map evalDirectiveResultToMessage results
                  <> map evalLazyDirectiveResultToMessage results'
          formatted = foldMap (sanitizeFilePaths . renderMessage) $ sortOn fst msgs
      liftIO $ Text.putStr formatted
  -- NOTE: if we're okay, we don't expect any errors, if we are not, we do expect them
  success `shouldBe` isOk
  unless success do
    Text.putStr $ foldMap sanitizeFilePaths errs

 where
  typeErrorToMessage err = (JL4.rangeOf err, JL4.prettyCheckErrorWithContext err)
  evalDirectiveResultToMessage (JL4.MkEvalDirectiveResult r res _) = (Just r, either JL4.prettyEvalException (List.singleton . Print.prettyLayout) res)
  evalLazyDirectiveResultToMessage (JL4Lazy.MkEvalDirectiveResult r res) = (r, either JL4Lazy.prettyEvalException (List.singleton . Print.prettyLayout) res)
  renderMessage (r, txt) = cliErrorMessage r txt

data CliError
  = CliParserError FilePath (NonEmpty Parser.PError)
  | CliCheckError FilePath CheckResult
  deriving stock (Show, Eq)

cliErrorMessage :: Maybe JL4.SrcRange -> [Text] -> Text
cliErrorMessage mrange msg =
  Text.unlines
    ( JL4.prettySrcRangeM  mrange <> ":"
        : map ("  " <>) msg
    )
