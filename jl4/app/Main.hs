module Main where

import Base (NonEmpty, for_, forM_, when, unless, liftIO, forM)
import Base.Text (Text)
import qualified Base.Text as Text
import qualified Data.Text.IO as Text.IO
import qualified Data.Text.Lazy as Text.Lazy
import qualified Data.Text.Lazy.Encoding as Text.Lazy.Encoding
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
import Data.Maybe (catMaybes, isJust, fromMaybe)
import Options.Applicative (ReadM, eitherReader, fullDesc, header, footer, helper, info, metavar, option, optional, strArgument, help, short, long, switch, progDesc, value, showDefaultWith)
import qualified Options.Applicative as Options
import System.Directory (getCurrentDirectory, createDirectoryIfMissing)
import System.Exit (exitSuccess, exitFailure)
import System.IO (stdin, stdout, hIsTerminalDevice)
import System.FilePath (takeBaseName, (</>))
import System.Process (callCommand)
import Text.Pretty.Simple (pShow, pShowNoColor)

import qualified LSP.Core.Shake as Shake
import LSP.Logger
import Language.LSP.Protocol.Types

import qualified LSP.L4.Rules as Rules
import LSP.L4.Oneshot (oneshotL4Action, oneshotL4ActionAndErrors)
import qualified LSP.L4.Oneshot as Oneshot

import L4.EvaluateLazy (parseFixedNow, readFixedNowEnv, resolveEvalConfig, EvalDirectiveResult(..), EvalDirectiveValue(..), prettyEvalException)
import qualified L4.EvaluateLazy.GraphViz2 as GraphViz2
import L4.Export (getExportedFunctions, getDefaultFunction, ExportedFunction(..), ExportedParam(..))
import L4.Syntax (Type'(..), Resolved, Module(..))
import L4.Print (prettyLayout)
import L4.DirectiveFilter (filterIdeDirectives)
import L4.Parser.SrcSpan (prettySrcRange)
import qualified L4.TracePolicy as TracePolicy
import L4.TracePolicy (TracePolicy(..), TraceLevel(..), TraceOptions(..), TraceDestination(..))
import Data.Time (UTCTime)
import qualified Data.List as List

-- | Parse batch input based on format (json, yaml, or csv)
parseBatchInput :: Text -> BSL.ByteString -> Either String [Aeson.Value]
parseBatchInput fmt bytes = case Text.toLower fmt of
  "json" -> Aeson.eitherDecode' bytes
  
  "yaml" -> case Yaml.decodeEither' (BSL.toStrict bytes) of
    Left err -> Left (Yaml.prettyPrintParseException err)
    Right val -> case val of
      Aeson.Array arr -> Right $ Vector.toList arr
      single -> Right [single]
  
  "csv" -> case Csv.decodeByName bytes of
    Left err -> Left err
    Right (_, rows) -> Right $ map rowToJson $ Vector.toList rows
      where
        rowToJson :: Csv.NamedRecord -> Aeson.Value
        rowToJson record = Aeson.Object $ KeyMap.fromList $ 
          map (\(k, v) -> (Key.fromText $ decodeUtf8 k, Aeson.String $ decodeUtf8 v)) $
          HashMap.toList record
  
  _ -> Left $ "Unsupported format: " ++ Text.unpack fmt

----------------------------------------------------------------------------
-- Wrapper code generation (adapted from jl4-decision-service/Backend/CodeGen.hs)
----------------------------------------------------------------------------

-- | Generate L4 wrapper code for JSONDECODE-based batch evaluation
generateBatchWrapper
  :: Text                                -- ^ Target function name
  -> [(Text, Maybe (Type' Resolved))]    -- ^ Parameter names and types
  -> Aeson.Value                         -- ^ Input arguments as JSON object
  -> Text                                -- ^ Generated L4 wrapper code
generateBatchWrapper funName params inputJson =
  if null params
    then Text.unlines
      [ ""
      , "-- ========== GENERATED WRAPPER =========="
      , ""
      , "#EVAL " <> funName
      ]
    else Text.unlines
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

-- | Generate DECLARE for input record
generateInputRecord :: [(Text, Maybe (Type' Resolved))] -> Text
generateInputRecord params = Text.unlines $
  ["DECLARE InputArgs HAS"] ++
  map formatField (zip [0::Int ..] params)
  where
    formatField (idx, (name, mty)) =
      let fieldIndent = if idx == 0 then "  " else ", "
          tyText = maybe "A NUMBER" prettyLayout mty
      in fieldIndent <> name <> " IS " <> tyText

-- | Generate typed decoder function
generateDecoder :: Text
generateDecoder = Text.unlines
  [ "GIVEN jsn IS A STRING"
  , "GIVETH AN EITHER STRING InputArgs"
  , "decodeArgs jsn MEANS JSONDECODE jsn"
  ]

-- | Generate JSON payload as L4 string literal
generateJsonPayload :: Aeson.Value -> Text
generateJsonPayload json =
  "DECIDE inputJson IS " <> escapeAsL4String json

-- | Escape JSON value as an L4 string literal
escapeAsL4String :: Aeson.Value -> Text
escapeAsL4String val =
  let jsonText = Text.Lazy.toStrict $ Text.Lazy.Encoding.decodeUtf8 $ Aeson.encode val
      escaped = Text.replace "\"" "\\\"" jsonText
  in "\"" <> escaped <> "\""

-- | Generate EVAL with CONSIDER/WHEN unwrapper
generateEvalDirective :: Text -> [(Text, Maybe (Type' Resolved))] -> Text
generateEvalDirective funName params = Text.unlines $
  [ "#EVAL"
  , "  CONSIDER decodeArgs inputJson"
  , "    WHEN RIGHT args THEN JUST (" <> functionCall <> ")"
  , "    WHEN LEFT error THEN NOTHING"
  ]
  where
    functionCall = funName <> " " <> Text.unwords (map mkArgAccess params)
    mkArgAccess (name, _) = "(args's " <> name <> ")"

-- | Extract parameter info from ExportedFunction
extractParamsFromExport :: ExportedFunction -> [(Text, Maybe (Type' Resolved))]
extractParamsFromExport ef = 
  [(p.paramName, p.paramType) | p <- ef.exportParams]

-- | Find export function by name or default
findExportFunction :: Maybe Text -> Module Resolved -> Either Text ExportedFunction
findExportFunction mName m = 
  let exports = getExportedFunctions m
  in case mName of
    Just name -> 
      case List.find (\e -> e.exportName == name) exports of
        Just ef -> Right ef
        Nothing -> Left $ "No @export function named '" <> name <> "' found"
    Nothing ->
      case getDefaultFunction m of
        Just ef -> Right ef
        Nothing -> 
          case exports of
            (ef:_) -> Right ef
            [] -> Left "No @export functions found in module"

----------------------------------------------------------------------------
-- Logging
----------------------------------------------------------------------------

data Log
  = IdeLog Oneshot.Log
  | CheckFailed !NormalizedUri
  | ExactPrint !Text
  | ShowAst !Text.Lazy.Text
  | SuccessOnly
  | BatchProcessing !Text
  | EvalOutput !Text
  | CliMessage !Text  -- General CLI messages (info, warnings)

instance Pretty Log where
  pretty = \ case
    IdeLog l -> "Ide:" <+> pretty l
    CheckFailed uri -> "Checking" <+> pretty uri <+> "failed."
    ExactPrint ep -> nest 2 $ vsep [pretty SuccessOnly, pretty ep]
    ShowAst ast -> pretty ast
    SuccessOnly -> "Checking succeeded."
    BatchProcessing msg -> "Batch:" <+> pretty msg
    EvalOutput txt -> pretty txt
    CliMessage msg -> pretty msg  -- No prefix for general CLI messages

----------------------------------------------------------------------------
-- Main
----------------------------------------------------------------------------

-- | Convert CLI options to unified TracePolicy
-- Implements the CLI behavior from TRACE-GRAPHVIZ-ARCHITECTURE.md
--
-- NOTE: This function creates a TracePolicy representing the unified architecture.
-- Currently this is informational - the actual trace behavior is still controlled
-- by the direct Option fields (traceText, graphvizFormat, outputDir).
-- Future work: Thread TracePolicy through the evaluation pipeline to fully unify
-- trace behavior across CLI/REPL/API.
optionsToTracePolicy :: Options -> TracePolicy
optionsToTracePolicy opts =
  let gvOpts = GraphViz2.GraphVizOptions
        { GraphViz2.includeValues = True
        , GraphViz2.showUnevaluated = True
        , GraphViz2.simplifyTrivial = opts.graphVizOptimize
        , GraphViz2.maxDepth = Nothing
        , GraphViz2.collapseFunctionLookups = opts.graphVizOptimize
        , GraphViz2.collapseSimplePaths = opts.graphVizOptimize
        , GraphViz2.deduplicateBindings = not opts.noDeduplicateBindings
        }

      -- Determine trace level for #EVALTRACE directives
      evaltraceLevel = case (opts.traceText, opts.graphvizFormat) of
        (TraceTextNone, Nothing) -> NoTrace
        (TraceTextFull, Nothing) -> CollectTrace (TraceOptions TracePolicy.TextTrace dest gvOpts)
        (_, Just format) -> CollectTrace (TraceOptions (graphVizFormatToTraceFormat format) dest gvOpts)

      -- Destination based on output directory
      dest = case opts.outputDir of
        Nothing -> Stdout
        Just dir -> FilesOnly dir

      -- CLI default: #EVAL directives don't produce traces (author's hint respected)
      evalLevel = NoTrace

  in TracePolicy
       { evalDirectiveTrace = evalLevel
       , evaltraceDirectiveTrace = evaltraceLevel
       }

graphVizFormatToTraceFormat :: GraphVizFormat -> TracePolicy.TraceFormat
graphVizFormatToTraceFormat DotFormat = TracePolicy.DotFormat
graphVizFormatToTraceFormat PngFormat = TracePolicy.PngFormat
graphVizFormatToTraceFormat SvgFormat = TracePolicy.SvgFormat

-- | Check if graphviz output is enabled (old or new way) and emit deprecation warnings
isGraphVizEnabled :: Options -> Recorder (WithPriority Log) -> IO Bool
isGraphVizEnabled opts recorder = do
  when opts.outputGraphViz $ do
    logWith recorder Warning $ CliMessage
      "WARNING: --graphviz is deprecated. Use --graphviz-format=dot instead."
  when opts.outputGraphViz2 $ do
    logWith recorder Warning $ CliMessage
      "WARNING: --graphviz2 is deprecated. Use --graphviz-format=dot instead."
  pure $ opts.outputGraphViz || opts.outputGraphViz2 || isJust opts.graphvizFormat

main :: IO ()
main = do
  curDir   <- getCurrentDirectory
  options  <- Options.execParser optionsConfig
  baseRecorder <- makeDefaultStderrRecorder Nothing
  let recorder = cmapWithPrio pretty baseRecorder

  -- Check if graphviz is enabled and emit deprecation warnings
  graphvizEnabled <- isGraphVizEnabled options recorder

  let -- In graphviz mode, suppress LSP diagnostics to keep output clean
      oneshotRecorder = if graphvizEnabled
                          then cmapWithPrio IdeLog mempty  -- Null recorder
                          else cmapWithPrio IdeLog recorder
  envFixed <- readFixedNowEnv
  evalConfig <- resolveEvalConfig (options.fixedNow <|> envFixed)

  -- Create unified TracePolicy from options (TRACE-GRAPHVIZ-ARCHITECTURE.md)
  -- NOTE: Currently informational - actual behavior still controlled by Option fields
  -- Future: Thread this through evaluation pipeline for complete unification
  let _tracePolicy = optionsToTracePolicy options

  (getErrs, errRecorder) <- fmap (cmapWithPrio pretty) <$> makeRefRecorder

  case options.batchFile of
    Just batchPath -> do
      -- Batch processing mode
      when (batchPath == "-" && options.batchFormat == Nothing) $ do
        logWith recorder Error $ BatchProcessing "Error: --format is required when reading from stdin (--batch -)"  
        exitFailure
      
      let inferredFormat = case batchPath of
            "-" -> Nothing
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
              logWith recorder Info $ BatchProcessing $ Text.pack $ "Processing " ++ show (length inputs) ++ " inputs"
              
              let l4File = NE.head options.files
              
              -- Read source file (need for combining with wrapper)
              _sourceText <- Text.IO.readFile l4File
              
              -- Phase 1: Parse and typecheck to get exports
              (initErrs, mTcRes) <- oneshotL4ActionAndErrors evalConfig l4File $ \_ -> do
                let nfp = toNormalizedFilePath l4File
                let uri = normalizedFilePathToUri nfp
                _ <- Shake.addVirtualFileFromFS nfp
                Shake.use Rules.SuccessfulTypeCheck uri
              
              case mTcRes of
                Nothing -> do
                  logWith recorder Error $ BatchProcessing $ "Type checking failed: " <> Text.concat initErrs
                  exitFailure
                Just tcRes | not tcRes.success -> do
                  logWith recorder Error $ BatchProcessing "Type checking failed"
                  exitFailure
                Just tcRes -> do
                  -- Phase 2: Find target export function
                  case findExportFunction options.entrypoint tcRes.module' of
                    Left err -> do
                      logWith recorder Error $ BatchProcessing err
                      exitFailure
                    Right exportFn -> do
                      logWith recorder Info $ BatchProcessing $ "Using @export function: " <> exportFn.exportName
                      
                      -- Phase 3: Filter IDE directives from source module
                      let filteredModule = filterIdeDirectives tcRes.module'
                          filteredSource = prettyLayout filteredModule
                          params = extractParamsFromExport exportFn
                      
                      -- Phase 4: For each input, generate wrapper and evaluate
                      results <- forM (zip [1::Int ..] inputs) $ \(idx, input) -> do
                        let wrapperCode = generateBatchWrapper exportFn.exportName params input
                            combinedProgram = filteredSource <> wrapperCode
                        
                        -- Evaluate combined program
                        let virtualPath = l4File ++ ".batch" ++ show idx ++ ".l4"
                        (evalErrs, mEvalRes) <- oneshotL4ActionAndErrors evalConfig virtualPath $ \_ -> do
                          let nfp = toNormalizedFilePath virtualPath
                          let uri = normalizedFilePathToUri nfp
                          _ <- Shake.addVirtualFile nfp combinedProgram
                          mEval <- Shake.use Rules.EvaluateLazy uri
                          pure mEval
                        
                        let (status, output, diagnostics) = case mEvalRes of
                              Nothing -> 
                                ("error" :: Text, Aeson.Null, Aeson.toJSON evalErrs)
                              Just evalResults ->
                                ("success", Aeson.toJSON (map (Text.pack . show) evalResults), Aeson.Array mempty)
                        
                        pure $ Aeson.object
                          [ "input" Aeson..= input
                          , "output" Aeson..= output
                          , "status" Aeson..= status
                          , "diagnostics" Aeson..= diagnostics
                          ]
                      
                      BSL.putStr $ Aeson.encode results
                      exitSuccess

    Nothing -> do
      -- Normal oneshot processing mode
      oneshotL4Action oneshotRecorder evalConfig curDir \_ ->
        for_ options.files \fp -> do
          let nfp = toNormalizedFilePath fp
              uri = normalizedFilePathToUri nfp
          _ <- Shake.addVirtualFileFromFS nfp
          mtc <- Shake.use Rules.SuccessfulTypeCheck  uri
          mEval <- Shake.use Rules.EvaluateLazy uri
          mep <- Shake.use Rules.ExactPrint uri
          forM_ mEval $ \evalResults -> do
            if graphvizEnabled
              then do
                -- GraphViz mode - generate trace visualizations
                let mModule = mtc >>= \tc -> if tc.success then Just tc.module' else Nothing
                    gvOpts = GraphViz2.defaultGraphVizOptions
                      { GraphViz2.collapseFunctionLookups = options.graphVizOptimize
                      , GraphViz2.collapseSimplePaths = options.graphVizOptimize
                      , GraphViz2.deduplicateBindings = not options.noDeduplicateBindings
                      }
                    -- Determine format: new flag takes precedence, fallback to DOT for old flags
                    format = fromMaybe DotFormat options.graphvizFormat

                case (options.outputDir, format) of
                  (Nothing, DotFormat) ->
                    -- No output dir, DOT format: write to stdout
                    for_ evalResults $ \result ->
                      case result.trace of
                        Just tr -> liftIO $ Text.IO.putStrLn $ GraphViz2.traceToGraphViz gvOpts mModule tr
                        Nothing -> pure ()

                  (Just outDir, _) -> do
                    -- Output dir specified: write files based on format
                    liftIO $ createDirectoryIfMissing True outDir
                    let baseName = takeBaseName fp
                    for_ (zip [1 :: Int ..] evalResults) $ \(idx, result) ->
                      case result.trace of
                        Just tr -> liftIO $ do
                          let dotContent = GraphViz2.traceToGraphViz gvOpts mModule tr
                              dotFile = outDir </> baseName <> "-eval" <> show idx <> ".dot"
                          -- Always write .dot file
                          Text.IO.writeFile dotFile dotContent
                          -- Generate additional formats if requested
                          case format of
                            DotFormat -> do
                              logWith recorder Info $ BatchProcessing $
                                "Generated: " <> Text.pack dotFile
                            PngFormat -> do
                              let pngFile = outDir </> baseName <> "-eval" <> show idx <> ".png"
                              callCommand $ "dot -Tpng " <> dotFile <> " > " <> pngFile
                              logWith recorder Info $ BatchProcessing $
                                "Generated: " <> Text.pack dotFile <> " and " <> Text.pack pngFile
                            SvgFormat -> do
                              let svgFile = outDir </> baseName <> "-eval" <> show idx <> ".svg"
                              callCommand $ "dot -Tsvg " <> dotFile <> " > " <> svgFile
                              logWith recorder Info $ BatchProcessing $
                                "Generated: " <> Text.pack dotFile <> " and " <> Text.pack svgFile
                        Nothing -> pure ()

                  (Nothing, _) -> do
                    -- PNG/SVG without output dir doesn't make sense
                    liftIO $ logWith recorder Error $ BatchProcessing
                      "Error: --graphviz-format=png/svg requires --output-dir to be specified"
                    liftIO exitFailure
                else do
                  -- In normal mode, log evaluation results
                  for_ (zip [1 :: Int ..] evalResults) $ \(idx, evalResult) -> do
                    let rendered = renderEvalOutput options.traceText idx evalResult
                    logWith recorder Info $ EvalOutput rendered
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
                  -- Don't log success message in graphviz mode (keeps output clean)
                  unless (options.verbose || options.showAst || options.outputGraphViz || options.outputGraphViz2) $
                    logWith recorder Info SuccessOnly
            (_, _)            -> do
              logWith    recorder Error $ CheckFailed uri
              logWith errRecorder Error $ CheckFailed uri

      errs <- getErrs
      if null errs
      then exitSuccess
      else exitFailure

data TraceTextMode
  = TraceTextNone
  | TraceTextFull
  deriving (Eq, Show)

renderTraceTextMode :: TraceTextMode -> String
renderTraceTextMode mode = case mode of
  TraceTextNone -> "none"
  TraceTextFull -> "full"

traceTextModeReader :: ReadM TraceTextMode
traceTextModeReader = eitherReader \input ->
  case Text.toLower (Text.pack input) of
    "none" -> Right TraceTextNone
    "off"  -> Right TraceTextNone
    "full" -> Right TraceTextFull
    "text" -> Right TraceTextFull
    "ascii" -> Right TraceTextFull
    other -> Left $ "Invalid trace MODE: " <> Text.unpack other <> " (expected none|full)"

-- | Output format for graphviz traces
data GraphVizFormat
  = DotFormat  -- ^ Output DOT format to stdout
  | PngFormat  -- ^ Generate PNG files
  | SvgFormat  -- ^ Generate SVG files
  deriving (Eq, Show)

graphVizFormatReader :: ReadM GraphVizFormat
graphVizFormatReader = eitherReader \input ->
  case Text.toLower (Text.pack input) of
    "dot" -> Right DotFormat
    "png" -> Right PngFormat
    "svg" -> Right SvgFormat
    other -> Left $ "Invalid format: " <> Text.unpack other <> " (expected dot|png|svg)"

data Options = MkOptions
  { files :: NonEmpty FilePath
  , verbose :: Bool
  , showAst :: Bool
  , traceText :: TraceTextMode
  , outputGraphViz :: Bool          -- DEPRECATED: old v1
  , outputGraphViz2 :: Bool         -- DEPRECATED: use graphvizFormat instead
  , graphvizFormat :: Maybe GraphVizFormat  -- NEW: unified format control
  , graphVizOptimize :: Bool        -- Enable GraphViz2 optimizations (collapse trivial nodes)
  , noDeduplicateBindings :: Bool   -- DISABLE binding deduplication (enabled by default)
  , outputDir :: Maybe FilePath     -- Directory for auto-split graph files
  , fixedNow :: Maybe UTCTime
  , batchFile :: Maybe FilePath
  , batchFormat :: Maybe Text
  , entrypoint :: Maybe Text
  }

optionsDescription :: Options.Parser Options
optionsDescription = MkOptions
  <$> some1 (strArgument (metavar "L4FILE"))
  <*> switch (long "verbose" <> short 'v' <> help "Enable verbose output: reformats and prints the input files")
  <*> switch (long "ast" <> short 'a' <> help "Show abstract syntax tree")
  <*> option traceTextModeReader (long "trace" <> short 't' <> metavar "MODE" <> value TraceTextFull <> showDefaultWith renderTraceTextMode <> help "Trace text output: none | full (default full)")
  <*> switch (long "graphviz" <> short 'g' <> help "[DEPRECATED: use --graphviz-format=dot] Output as GraphViz DOT (old v1)")
  <*> switch (long "graphviz2" <> help "[DEPRECATED: use --graphviz-format=dot] Output as GraphViz DOT (v2)")
  <*> optional (option graphVizFormatReader (long "graphviz-format" <> metavar "FORMAT" <> help "Output GraphViz trace (enables tracing): dot | png | svg"))
  <*> switch (long "optimize" <> help "Enable GraphViz optimizations (collapse function lookups and simple paths)")
  <*> switch (long "no-deduplicate-bindings" <> help "Disable binding deduplication (enabled by default to show shared WHERE/LET bindings)")
  <*> optional (Options.strOption (long "output-dir" <> short 'o' <> metavar "DIR" <> help "Output directory for graph files (auto-splits multiple graphs, generates .dot and .png)"))
  <*> optional (option fixedNowReader (long "fixed-now" <> metavar "ISO8601" <> help "Pin evaluation clock (e.g. 2025-01-31T15:45:30Z) so NOW/TODAY stay deterministic"))
  <*> optional (Options.strOption (long "batch" <> short 'b' <> metavar "BATCH_FILE" <> help "Batch input file (JSON/YAML/CSV); use '-' for stdin"))
  <*> optional (Options.strOption (long "format" <> short 'f' <> metavar "FORMAT" <> help "Input/output format (json|yaml|csv); required when reading from stdin"))
  <*> optional (Options.strOption (long "entrypoint" <> short 'e' <> metavar "FUNCTION" <> help "Name of @export function to call (defaults to @export default or first @export)"))

fixedNowReader :: ReadM UTCTime
fixedNowReader =
  eitherReader \s ->
    maybe (Left "Unable to parse --fixed-now; expected ISO8601 UTC like 2025-01-31T15:45:30Z")
          Right
          (parseFixedNow (Text.pack s))

renderEvalOutput :: TraceTextMode -> Int -> EvalDirectiveResult -> Text
renderEvalOutput traceMode idx MkEvalDirectiveResult{range = mRange, result, trace = mTrace} =
  Text.intercalate "\n\n" $ catMaybes
    [ Just headerLine
    , Just ("Result:\n" <> indentBlockText (renderEvalValue result))
    , traceSection
    ]
  where
    headerLine =
      let idxText = Text.pack (show idx)
          rangeText = maybe "" (\rng -> " @ " <> prettySrcRange rng) mRange
      in "Evaluation[" <> idxText <> "]" <> rangeText
    traceSection = case traceMode of
      TraceTextNone -> Nothing
      TraceTextFull ->
        let traceBody = maybe "(no trace captured; add #EVALTRACE to the directive)" prettyLayout mTrace
        in Just ("Trace:\n" <> indentBlockText traceBody)

renderEvalValue :: EvalDirectiveValue -> Text
renderEvalValue = \case
  Assertion True -> "assertion satisfied"
  Assertion False -> "assertion failed"
  Reduction (Left exc) -> Text.unlines (prettyEvalException exc)
  Reduction (Right val) -> prettyLayout val

indentBlockText :: Text -> Text
indentBlockText txt =
  let ls = case Text.lines txt of
             [] -> [""]
             xs -> xs
  in Text.unlines (map ("  " <>) ls)

optionsConfig :: Options.ParserInfo Options
optionsConfig = info
  (helper <*> optionsDescription)
  (fullDesc
    <> progDesc "Parse the given FILES and reports warnings, errors, and eval outputs. Returns with exitcode 0 (success) if parse succeeds. Returns exitcode 1 if parse fails."
    <> header "## jl4-cli"
    <> footer "## Part of the L4 tool family from Legalese.com"
  )
