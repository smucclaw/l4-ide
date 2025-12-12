module Main where

import Base (NonEmpty, for_, when, unless, liftIO, forM)
import Base.Text (Text)
import qualified Base.Text as Text
import qualified Data.Text.Lazy as Text.Lazy
import Control.Applicative ((<|>))
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (some1)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Csv as Csv
import qualified Data.Yaml as Yaml
import qualified Data.Vector as Vector
import qualified Data.HashMap.Strict as HashMap
import Data.Text.Encoding (decodeUtf8)
import Options.Applicative (ReadM, eitherReader, fullDesc, header, footer, helper, info, metavar, option, optional, strArgument, help, short, long, switch, progDesc)
import qualified Options.Applicative as Options
import System.Directory (getCurrentDirectory)
import System.Exit (exitSuccess, exitFailure)
import System.IO (stdin, stdout, hIsTerminalDevice)
import Text.Pretty.Simple (pShow, pShowNoColor)

import qualified LSP.Core.Shake as Shake
import LSP.Logger
import Language.LSP.Protocol.Types

import qualified LSP.L4.Rules as Rules
import LSP.L4.Oneshot (oneshotL4Action)
import qualified LSP.L4.Oneshot as Oneshot

import L4.EvaluateLazy (parseFixedNow, readFixedNowEnv, resolveEvalConfig, execEvalModuleWithJSON)
import Data.Time (UTCTime)

-- | Parse batch input based on format (json, yaml, or csv)
parseBatchInput :: Text -> BSL.ByteString -> Either String [Aeson.Value]
parseBatchInput fmt bytes = case Text.toLower fmt of
  "json" -> Aeson.eitherDecode' bytes
  
  "yaml" -> case Yaml.decodeEither' (BSL.toStrict bytes) of
    Left err -> Left (Yaml.prettyPrintParseException err)
    Right val -> case val of
      Aeson.Array arr -> Right $ Vector.toList arr
      single -> Right [single]  -- Single object becomes single-element list
  
  "csv" -> case Csv.decodeByName bytes of
    Left err -> Left err
    Right (_, rows) -> Right $ map rowToJson $ Vector.toList rows
      where
        rowToJson :: Csv.NamedRecord -> Aeson.Value
        rowToJson record = Aeson.Object $ KeyMap.fromList $ 
          map (\(k, v) -> (Key.fromText $ decodeUtf8 k, Aeson.String $ decodeUtf8 v)) $
          HashMap.toList record
  
  _ -> Left $ "Unsupported format: " ++ Text.unpack fmt

data Log
  = IdeLog Oneshot.Log
  | CheckFailed !NormalizedUri
  | ExactPrint !Text
  | ShowAst !Text.Lazy.Text
  | SuccessOnly
  | BatchProcessing !Text

instance Pretty Log where
  pretty = \ case
    IdeLog l -> "Ide:" <+> pretty l
    CheckFailed uri -> "Checking" <+> pretty uri <+> "failed."
    ExactPrint ep -> nest 2 $ vsep [pretty SuccessOnly, pretty ep]
    ShowAst ast -> pretty ast
    SuccessOnly -> "Checking succeeded."
    BatchProcessing msg -> "Batch:" <+> pretty msg

main :: IO ()
main = do
  curDir   <- getCurrentDirectory
  recorder <- cmapWithPrio pretty <$> makeDefaultStderrRecorder Nothing
  options  <- Options.execParser optionsConfig
  envFixed <- readFixedNowEnv
  evalConfig <- resolveEvalConfig (options.fixedNow <|> envFixed)

  (getErrs, errRecorder) <- fmap (cmapWithPrio pretty) <$> makeRefRecorder

  case options.batchFile of
    Just batchPath -> do
      -- Batch processing mode
      -- Validate format requirement for stdin
      when (batchPath == "-" && options.batchFormat == Nothing) $ do
        logWith recorder Error $ BatchProcessing "Error: --format is required when reading from stdin (--batch -)"  
        exitFailure
      
      -- Determine format: explicit flag or infer from file extension
      let inferredFormat = case batchPath of
            "-" -> Nothing  -- Must use explicit format
            path
              | ".json" `Text.isSuffixOf` Text.pack path -> Just "json"
              | ".yaml" `Text.isSuffixOf` Text.pack path -> Just "yaml"
              | ".yml" `Text.isSuffixOf` Text.pack path -> Just "yaml"
              | ".csv" `Text.isSuffixOf` Text.pack path -> Just "csv"
              | otherwise -> Nothing
          format = options.batchFormat <|> inferredFormat
      
      batchBytes <- if batchPath == "-"
        then BSL.hGetContents stdin
        else BSL.readFile batchPath
      
      case format of
        Nothing -> do
          logWith recorder Error $ BatchProcessing "Error: Could not determine input format. Use --format to specify."
          exitFailure
        Just fmt -> do
          let parseResult = parseBatchInput fmt batchBytes
          case parseResult of
            Left err -> do
              logWith recorder Error $ BatchProcessing $ Text.pack $ "Failed to parse batch file: " ++ err
              exitFailure
            Right inputs -> do
              logWith recorder Info $ BatchProcessing $ Text.pack $ "Processing " ++ show (length inputs) ++ " inputs from " ++ batchPath
              
              -- Batch processing: parse/typecheck L4 file ONCE, then evaluate for each input
              let l4File = NE.head options.files
              let nfp = toNormalizedFilePath l4File
              let uri = normalizedFilePathToUri nfp
              
              -- Parse and typecheck once using oneshot
              results <- oneshotL4Action (cmapWithPrio IdeLog recorder) evalConfig curDir \_ -> do
                _ <- Shake.addVirtualFileFromFS nfp
                mtc <- Shake.use Rules.SuccessfulTypeCheck uri
                
                case mtc of
                  Nothing -> do
                    logWith recorder Error $ BatchProcessing "Type checking failed"
                    pure []
                  Just tcRes | not tcRes.success -> do
                    logWith recorder Error $ BatchProcessing "Type checking failed"  
                    pure []
                  Just tcRes -> do
                    -- Type check succeeded once, now evaluate each input in IO
                    -- This avoids re-typechecking for every input
                    liftIO $ forM inputs $ \input -> do
                      -- Phase 4: JSON → L4 Environment integration
                      -- JSON values are bound to ASSUME'd L4 variables by name matching
                      (_env, evalResults) <- execEvalModuleWithJSON 
                        evalConfig 
                        tcRes.entityInfo 
                        input 
                        tcRes.module'
                      
                      -- Build result object
                      pure $ Aeson.object
                        [ "input" Aeson..= input
                        , "output" Aeson..= Aeson.toJSON (map (Text.pack . show) evalResults)
                        , "status" Aeson..= ("success" :: Text)
                        , "diagnostics" Aeson..= Aeson.Array mempty
                        ]
              
              BSL.putStr $ Aeson.encode results
              exitSuccess

    Nothing -> do
      -- Normal oneshot processing mode
      oneshotL4Action (cmapWithPrio IdeLog recorder) evalConfig curDir \_ ->
        for_ options.files \fp -> do
          let nfp = toNormalizedFilePath fp
              uri = normalizedFilePathToUri nfp
          _ <- Shake.addVirtualFileFromFS nfp
          mtc <- Shake.use Rules.SuccessfulTypeCheck  uri
          _ <- Shake.use Rules.EvaluateLazy uri
          mep <- Shake.use Rules.ExactPrint uri
          case (mtc, mep) of
            (Just tcRes, Just ep)
              | tcRes.success -> do
                  when options.verbose $ logWith recorder Info $ ExactPrint ep
                  when options.showAst $ do
                    mast <- Shake.use Rules.GetParsedAst uri
                    case mast of
                      Just ast -> do
                        isTTY <- liftIO $ hIsTerminalDevice stdout
                        let showFn = if isTTY then pShow else pShowNoColor
                        logWith recorder Info $ ShowAst (showFn ast)
                      Nothing -> pure ()
                  unless (options.verbose || options.showAst) $ logWith recorder Info SuccessOnly
            (_, _)            -> do
              logWith    recorder Error $ CheckFailed uri
              logWith errRecorder Error $ CheckFailed uri

      errs <- getErrs
      if null errs
      then exitSuccess
      else exitFailure

data Options = MkOptions
  { files :: NonEmpty FilePath
  , verbose :: Bool
  , showAst :: Bool
  , fixedNow :: Maybe UTCTime
  , batchFile :: Maybe FilePath
  , batchFormat :: Maybe Text
  }

optionsDescription :: Options.Parser Options
optionsDescription = MkOptions
  <$> some1 (strArgument (metavar "L4FILE"))
  <*> switch (long "verbose" <> short 'v' <> help "Enable verbose output: reformats and prints the input files")
  <*> switch (long "ast" <> short 'a' <> help "Show abstract syntax tree")
  <*> optional (option fixedNowReader (long "fixed-now" <> metavar "ISO8601" <> help "Pin evaluation clock (e.g. 2025-01-31T15:45:30Z) so NOW/TODAY stay deterministic"))
  <*> optional (Options.strOption (long "batch" <> short 'b' <> metavar "BATCH_FILE" <> help "Batch input file (JSON/YAML/CSV); use '-' for stdin"))
  <*> optional (Options.strOption (long "format" <> short 'f' <> metavar "FORMAT" <> help "Input/output format (json|yaml|csv); required when reading from stdin"))

fixedNowReader :: ReadM UTCTime
fixedNowReader =
  eitherReader \s ->
    maybe (Left "Unable to parse --fixed-now; expected ISO8601 UTC like 2025-01-31T15:45:30Z")
          Right
          (parseFixedNow (Text.pack s))

optionsConfig :: Options.ParserInfo Options
optionsConfig = info
  (helper <*> optionsDescription)
  (fullDesc
    <> progDesc "Parse the given FILES and reports warnings, errors, and eval outputs. Returns with exitcode 0 (success) if parse succeeds. Returns exitcode 1 if parse fails."
    <> header "## jl4-cli"
    <> footer "## Part of the L4 tool family from Legalese.com"
  )
