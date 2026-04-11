-- | @l4 batch FILE --inputs data.{json,yaml,csv} [--entrypoint fn]@
--
-- Evaluates a single @\@export@ function against many input rows,
-- streaming one NDJSON result object per line as each row completes.
-- Stays buffer-free so large CSV batches can be piped through @jq@
-- without running the evaluator dry on memory.
module L4.Cli.Batch
  ( BatchOptions(..)
  , batchOptionsParser
  , batchCmd
  ) where

import Base (NonEmpty, forM)
import Base.Text (Text)
import qualified Base.Text as Text
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as Text.Lazy
import qualified Data.Text.Lazy.Encoding as Text.Lazy.Encoding
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSL8
import qualified Data.Csv as Csv
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as List
import qualified Data.Vector as Vector
import qualified Data.Yaml as Yaml
import Data.Text.Encoding (decodeUtf8)
import Options.Applicative
import System.Exit (exitFailure, exitSuccess)
import System.IO (hFlush, stdout, stdin)

import qualified LSP.Core.Shake as Shake
import qualified LSP.L4.Rules as Rules
import Language.LSP.Protocol.Types (normalizedFilePathToUri, toNormalizedFilePath)

import L4.Export (ExportedFunction(..), ExportedParam(..), getDefaultFunction, getExportedFunctions)
import L4.DirectiveFilter (filterIdeDirectives)
import L4.Print (prettyLayout)
import L4.Syntax (Module, Resolved, Type')

import L4.Cli.Common

-- NonEmpty is re-exported from Base but we import it only for a type sig.
_unusedNE :: NonEmpty Int -> Int
_unusedNE _ = 0

----------------------------------------------------------------------------
-- Options
----------------------------------------------------------------------------

data BatchOptions = BatchOptions
  { batchFile        :: FilePath
  , batchInputs      :: FilePath      -- path or "-" for stdin
  , batchInputFormat :: Maybe Text    -- inferred from extension if omitted
  , batchEntrypoint  :: Maybe Text
  , batchFixedNow    :: FixedNowOpt
  }

batchOptionsParser :: Parser BatchOptions
batchOptionsParser = BatchOptions
  <$> strArgument (metavar "FILE" <> help "Path to the .l4 file containing the @export function")
  <*> strOption
        ( long "inputs"
        <> short 'i'
        <> metavar "PATH"
        <> help "Input rows file (.json/.yaml/.csv); '-' reads from stdin (requires --input-format)"
        )
  <*> optional (strOption
        ( long "input-format"
        <> metavar "FORMAT"
        <> help "Override input format (json|yaml|csv); inferred from extension by default"
        ))
  <*> optional (strOption
        ( long "entrypoint"
        <> short 'e'
        <> metavar "FUNCTION"
        <> help "Name of the @export function to call (defaults to @export default or the first one)"
        ))
  <*> fixedNowParser

----------------------------------------------------------------------------
-- Entry point
----------------------------------------------------------------------------

batchCmd :: BatchOptions -> IO ()
batchCmd opts = do
  evalConfig <- makeEvalConfig opts.batchFixedNow

  -- Step 1: read & parse the input rows.
  let inferredFormat = case opts.batchInputs of
        "-"  -> Nothing
        path | ".json" `List.isSuffixOf` path -> Just "json"
             | ".yaml" `List.isSuffixOf` path -> Just "yaml"
             | ".yml"  `List.isSuffixOf` path -> Just "yaml"
             | ".csv"  `List.isSuffixOf` path -> Just "csv"
             | otherwise -> Nothing
      format = opts.batchInputFormat <|> inferredFormat
  fmt <- case format of
    Just f  -> pure f
    Nothing -> do
      TIO.putStrLn "Error: --input-format is required when the extension can't be inferred (e.g. reading from stdin)"
      exitFailure
  rowsBytes <- if opts.batchInputs == "-"
    then BSL.hGetContents stdin
    else BSL.readFile opts.batchInputs
  inputs <- case parseBatchInput fmt rowsBytes of
    Right rs -> pure rs
    Left err -> do
      TIO.putStrLn $ "Error: failed to parse inputs: " <> Text.pack err
      exitFailure

  -- Step 2: typecheck the source file, find the target @export function.
  (initErrs, mTc) <- runOneshot evalConfig opts.batchFile \nfp -> do
    let uri = normalizedFilePathToUri nfp
    _ <- Shake.addVirtualFileFromFS nfp
    Shake.use Rules.SuccessfulTypeCheck uri
  tcRes <- case mTc of
    Just tc | tc.success -> pure tc
    _ -> do
      putDiagnostics initErrs
      TIO.putStrLn "Error: type checking failed"
      exitFailure
  exportFn <- case findExportFunction opts.batchEntrypoint tcRes.module' of
    Right ef -> pure ef
    Left err -> do
      TIO.putStrLn $ "Error: " <> err
      exitFailure
  let filteredModule = filterIdeDirectives tcRes.module'
      filteredSource = prettyLayout filteredModule
      params = extractParamsFromExport exportFn

  -- Step 3: for each input, generate wrapper, eval, emit one NDJSON line.
  anyFailure <- fmap or $ forM (zip [1 :: Int ..] inputs) \(idx, input) -> do
    let wrapperCode     = generateBatchWrapper exportFn.exportName params input
        combinedProgram = filteredSource <> wrapperCode
        virtualPath     = opts.batchFile ++ ".batch" ++ show idx ++ ".l4"
    (evalErrs, mEval) <- runOneshot evalConfig virtualPath \nfp -> do
      let uri = normalizedFilePathToUri nfp
      _ <- Shake.addVirtualFile (toNormalizedFilePath virtualPath) combinedProgram
      Shake.use Rules.EvaluateLazy uri
    let (status, outputJson, diags) = case mEval of
          Nothing ->
            ("error" :: Text, Aeson.Null, Aeson.toJSON evalErrs)
          Just evalResults ->
            ("success" :: Text, Aeson.toJSON evalResults, Aeson.Array mempty)
        line = Aeson.object
          [ Key.fromString "input"       Aeson..= input
          , Key.fromString "output"      Aeson..= outputJson
          , Key.fromString "status"      Aeson..= status
          , Key.fromString "diagnostics" Aeson..= diags
          ]
    BSL8.putStrLn (Aeson.encode line)
    hFlush stdout
    pure (status == "error")

  if anyFailure then exitFailure else exitSuccess

----------------------------------------------------------------------------
-- Input parsing
----------------------------------------------------------------------------

parseBatchInput :: Text -> BSL.ByteString -> Either String [Aeson.Value]
parseBatchInput fmt bytes = case Text.toLower fmt of
  "json" -> Aeson.eitherDecode' bytes
  "yaml" -> case Yaml.decodeEither' (BSL.toStrict bytes) of
    Left err -> Left (Yaml.prettyPrintParseException err)
    Right val -> case val of
      Aeson.Array arr -> Right (Vector.toList arr)
      single          -> Right [single]
  "csv" -> case Csv.decodeByName bytes of
    Left err -> Left err
    Right (_, rows) -> Right (map rowToJson (Vector.toList rows))
      where
        rowToJson :: Csv.NamedRecord -> Aeson.Value
        rowToJson record = Aeson.Object $ KeyMap.fromList $
          map (\(k, v) -> (Key.fromText (decodeUtf8 k), Aeson.String (decodeUtf8 v))) $
          HashMap.toList record
  other -> Left ("Unsupported format: " ++ Text.unpack other)

----------------------------------------------------------------------------
-- Wrapper code generation
----------------------------------------------------------------------------

generateBatchWrapper
  :: Text
  -> [(Text, Maybe (Type' Resolved))]
  -> Aeson.Value
  -> Text
generateBatchWrapper funName params inputJson
  | null params =
      Text.unlines
        [ ""
        , "-- ========== GENERATED WRAPPER =========="
        , ""
        , "#EVAL " <> funName
        ]
  | otherwise =
      Text.unlines
        [ ""
        , "-- ========== GENERATED WRAPPER =========="
        , ""
        , generateInputRecord params
        , ""
        , generateDecoder
        , ""
        , generateJsonPayload inputJson
        , ""
        , generateEvalDirective funName params
        ]

generateInputRecord :: [(Text, Maybe (Type' Resolved))] -> Text
generateInputRecord params = Text.unlines $
  ["DECLARE InputArgs HAS"] ++
  map formatField (zip [0 :: Int ..] params)
  where
    formatField (idx, (name, mty)) =
      let fieldIndent = if idx == 0 then "  " else ", "
          tyText      = maybe "A NUMBER" prettyLayout mty
      in fieldIndent <> name <> " IS " <> tyText

generateDecoder :: Text
generateDecoder = Text.unlines
  [ "GIVEN jsn IS A STRING"
  , "GIVETH AN EITHER STRING InputArgs"
  , "decodeArgs jsn MEANS JSONDECODE jsn"
  ]

generateJsonPayload :: Aeson.Value -> Text
generateJsonPayload json =
  "DECIDE inputJson IS " <> escapeAsL4String json

escapeAsL4String :: Aeson.Value -> Text
escapeAsL4String val =
  let jsonText = Text.Lazy.toStrict $ Text.Lazy.Encoding.decodeUtf8 $ Aeson.encode val
      escaped  = Text.replace "\"" "\\\"" jsonText
  in "\"" <> escaped <> "\""

generateEvalDirective :: Text -> [(Text, Maybe (Type' Resolved))] -> Text
generateEvalDirective funName params = Text.unlines
  [ "#EVAL"
  , "  CONSIDER decodeArgs inputJson"
  , "    WHEN RIGHT args THEN JUST (" <> functionCall <> ")"
  , "    WHEN LEFT error THEN NOTHING"
  ]
  where
    functionCall = funName <> " " <> Text.unwords (map mkArgAccess params)
    mkArgAccess (name, _) = "(args's " <> name <> ")"

extractParamsFromExport :: ExportedFunction -> [(Text, Maybe (Type' Resolved))]
extractParamsFromExport ef =
  [(p.paramName, p.paramType) | p <- ef.exportParams]

findExportFunction :: Maybe Text -> Module Resolved -> Either Text ExportedFunction
findExportFunction mName m =
  let exports = getExportedFunctions m
  in case mName of
       Just name ->
         case List.find (\e -> e.exportName == name) exports of
           Just ef -> Right ef
           Nothing -> Left $ "no @export function named '" <> name <> "' found"
       Nothing ->
         case getDefaultFunction m of
           Just ef -> Right ef
           Nothing -> case exports of
             (ef:_) -> Right ef
             []     -> Left "no @export functions found in module"
