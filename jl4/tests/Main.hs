{-# LANGUAGE ViewPatterns #-}
module Main (main) where

import Base
import Control.Monad.Trans.Maybe
import qualified Data.Aeson.Encode.Pretty as AP
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified L4.Annotation as JL4
import qualified L4.EvaluateLazy as JL4Lazy
import L4.TracePolicy (lspDefaultPolicy)
import L4.EvaluateLazy.GraphVizOptions (defaultGraphVizOptions)
import L4.Export (ExportedFunction (..))
import qualified L4.Export as Export
import L4.JsonSchema (SchemaContext (..))
import qualified L4.JsonSchema as JsonSchema
import qualified L4.Nlg as Nlg
import qualified L4.Parser.SrcSpan as JL4
import L4.Syntax
import qualified L4.TypeCheck as JL4

import qualified Paths_jl4
import qualified Paths_jl4_core

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

import qualified Hover
import qualified SemanticTokens

main :: IO ()
main = do
  dataDir <- Paths_jl4.getDataDir
  dataDirCore <- Paths_jl4_core.getDataDir
  envFixed <- JL4Lazy.readFixedNowEnv
  let fallbackNow =
        fromMaybe (error "Internal: invalid fallback timestamp for JL4 tests")
                  (JL4Lazy.parseFixedNow "2025-01-31T15:45:30Z")
  let chosenNow = maybe (Just fallbackNow) Just envFixed
  let tracePolicy = lspDefaultPolicy defaultGraphVizOptions
  evalConfig <- JL4Lazy.resolveEvalConfig chosenNow tracePolicy
  let examplesRoot = dataDir </> "examples"
  okFiles <- sort <$> globDir1 (compile "ok/**/*.l4") examplesRoot
  librariesFiles <- sort <$> globDir1 (compile "*.l4") (dataDirCore </> "libraries")
  legalFiles <- sort <$> globDir1 (compile "legal/**/*.l4") examplesRoot
  tcFailsFiles <- sort <$> globDir1 (compile "not-ok/tc/**/*.l4") examplesRoot
  nlgFailsFiles <- sort <$> globDir1 (compile "not-ok/nlg/**/*.l4") examplesRoot
  semanticTokenFiles <- sort <$> globDir1 (compile "lsp/semantic-tokens/**/*.l4") examplesRoot
  hoverFiles <- sort <$> globDir1 (compile "lsp/hover/**/*.l4") examplesRoot
  hspec do
    describe "ok files" $ tests evalConfig (True, True) (okFiles <> legalFiles <> librariesFiles) examplesRoot
    describe "tc fails" $ tests evalConfig (False, True) tcFailsFiles examplesRoot
    describe "nlg fails" $ tests evalConfig (True, False) nlgFailsFiles examplesRoot
    describe "lsp" $ SemanticTokens.semanticTokenTests evalConfig semanticTokenFiles examplesRoot
    describe "lsp hover" $ Hover.hoverTests evalConfig hoverFiles examplesRoot
  where
    tests evalConfig (tcOk, nlgOk) files root =
      forM_ files $ \inputFile -> do
        let testCase = makeRelative root inputFile
        let goldenDir = takeDirectory inputFile </> "tests"
        describe testCase $ do
          it "parses and checks" $
            l4Golden evalConfig tcOk goldenDir inputFile
          it "exactprints" $
            jl4ExactPrintGolden evalConfig goldenDir inputFile
          it "natural language annotations" $
            jl4NlgAnnotationsGolden evalConfig nlgOk goldenDir inputFile
          it "json schema" $
            jl4JsonSchemaGolden evalConfig goldenDir inputFile

l4Golden :: JL4Lazy.EvalConfig -> Bool -> String -> String -> IO (Golden String)
l4Golden evalConfig isOk dir inputFile = do
  (output, _) <- capture (checkFile evalConfig isOk inputFile)
  pure
    Golden
      { output
      , encodePretty = show
      , writeToFile = writeFile
      , readFromFile = readFile
      , goldenFile = dir </> (takeFileName inputFile -<.> "golden")
      , actualFile = Just (dir </> (takeFileName inputFile -<.> "actual"))
      , failFirstTime = True
      }

jl4ExactPrintGolden :: JL4Lazy.EvalConfig -> String -> String -> IO (Golden Text)
jl4ExactPrintGolden evalConfig dir inputFile = do
  (errs, moutput) <- oneshotL4ActionAndErrors evalConfig inputFile \nfp -> do
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
      , failFirstTime = True
      }

jl4NlgAnnotationsGolden :: JL4Lazy.EvalConfig -> Bool -> String -> FilePath -> IO (Golden Text)
jl4NlgAnnotationsGolden evalConfig isOk dir inputFile = do
  (errs, moutput) <- oneshotL4ActionAndErrors evalConfig inputFile \nfp -> do
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
      , failFirstTime = True
      }

jl4JsonSchemaGolden :: JL4Lazy.EvalConfig -> String -> FilePath -> IO (Golden String)
jl4JsonSchemaGolden evalConfig dir inputFile = do
  (_, moutput) <- oneshotL4ActionAndErrors evalConfig inputFile \nfp -> do
    let uri = normalizedFilePathToUri nfp
    _ <- Shake.addVirtualFileFromFS nfp
    Shake.use Rules.SuccessfulTypeCheck uri
  let output = case moutput of
        Nothing -> "Cannot generate schema for module that doesn't typecheck\n"
        Just checkResult ->
          let exports = Export.getExportedFunctions checkResult.module'
              defaultExport = selectDefaultExport exports
          in case defaultExport of
               Nothing -> "No @export annotations found in file\n"
               Just export ->
                 let ctx = buildSchemaContext checkResult.module'
                     schema = JsonSchema.generateJsonSchema ctx export
                 in BL.unpack (AP.encodePretty schema) ++ "\n"
  pure
    Golden
      { output
      , encodePretty = id
      , writeToFile = writeFile
      , readFromFile = readFile
      , goldenFile = dir </> takeFileName inputFile -<.> "schema.golden"
      , actualFile = Just (dir </> takeFileName inputFile -<.> "schema.actual")
      , failFirstTime = True
      }

selectDefaultExport :: [ExportedFunction] -> Maybe ExportedFunction
selectDefaultExport exports =
  case List.find (.exportIsDefault) exports of
    Just e -> Just e
    Nothing -> listToMaybe exports

buildSchemaContext :: Module Resolved -> SchemaContext
buildSchemaContext (MkModule _ _ section) =
  JsonSchema.emptyContext{ctxDeclares = collectDeclares section}
 where
  collectDeclares :: Section Resolved -> Map.Map Text (Declare Resolved)
  collectDeclares (MkSection _ _ _ decls) =
    Map.fromList $ concatMap collectDecl decls

  collectDecl :: TopDecl Resolved -> [(Text, Declare Resolved)]
  collectDecl = \case
    Declare _ decl@(MkDeclare _ _ appForm _) ->
      [(getDeclName appForm, decl)]
    Section _ sub -> Map.toList $ collectDeclares sub
    _ -> []

  getDeclName (MkAppForm _ name _ _) = rawNameToText (rawName (getActual name))

-- ----------------------------------------------------------------------------
-- Test helpers
-- ----------------------------------------------------------------------------

sanitizeFilePaths :: Text -> Text
sanitizeFilePaths = stripAnsiCodes . RE.replaceAll regex
  where
  mkFileName (Text.null -> hadNoWhiteSpace) (Text.unpack -> fp)
    = Text.pack $ (if hadNoWhiteSpace then id else (' ' :)) case OsPath.decodeUtf . OsPath.takeFileName =<< OsPath.encodeUtf fp of
      Just p | not (null p) -> p
      _ -> fp

  regex = mkFileName <$> RE.manyTextOf CharSet.space <* RE.text "file://" <*>
      RE.manyTextOf (CharSet.not $ CharSet.space `CharSet.union` CharSet.singleton ':')

-- | Strip ANSI color codes from text output
stripAnsiCodes :: Text -> Text
stripAnsiCodes = Text.pack . go . Text.unpack
  where
  go [] = []
  go ('\x1b':'[':rest) = go (drop 1 $ dropWhile (/= 'm') rest)
  go (c:cs) = c : go cs

checkFile :: JL4Lazy.EvalConfig -> Bool -> FilePath -> IO ()
checkFile evalConfig isOk file = do
  (errs, isJust ->  success) <- oneshotL4ActionAndErrors evalConfig file \nfp -> runMaybeT do
      let uri = normalizedFilePathToUri nfp
      _        <- lift   $ Shake.addVirtualFileFromFS nfp
      _        <- MaybeT $ Shake.use GetParsedAst uri        <* liftIO (Text.putStrLn "Parsing successful")
      checked  <- MaybeT $ Shake.use SuccessfulTypeCheck uri <* liftIO (Text.putStrLn "Typechecking successful")
      results' <- MaybeT $ Shake.use EvaluateLazy uri
      when (not (null results')) $ liftIO $ Text.putStrLn "Evaluation successful"
      let msgs  =    map typeErrorToMessage checked.infos
                  <> map evalLazyDirectiveResultToMessage results'
          formatted = foldMap (sanitizeFilePaths . renderMessage) $ sortOn fst msgs
      liftIO $ Text.putStr formatted
  -- NOTE: if we're okay, we don't expect any errors, if we are not, we do expect them
  success `shouldBe` isOk
  unless success do
    Text.putStr $ foldMap sanitizeFilePaths errs

 where
  typeErrorToMessage err = (JL4.rangeOf err, JL4.prettyCheckErrorWithContext err)
  evalLazyDirectiveResultToMessage res@(JL4Lazy.MkEvalDirectiveResult r _ _) =
    (r, Text.lines (JL4Lazy.prettyEvalDirectiveResult res))
  renderMessage (r, txt) = cliErrorMessage r txt

cliErrorMessage :: Maybe JL4.SrcRange -> [Text] -> Text
cliErrorMessage mrange msg =
  Text.unlines
    ( JL4.prettySrcRangeM  mrange <> ":"
        : map ("  " <>) msg
    )
