-- | Shared helpers for the l4 CLI subcommands.
--
-- Everything here is neutral to a specific command so individual
-- @L4.Cli.*@ modules stay narrow and readable.
module L4.Cli.Common
  ( -- * Shared options
    FixedNowOpt(..)
  , fixedNowParser
  , parseFixedNowText

    -- * Evaluation config
  , makeEvalConfig
  , makeTracePolicyForEval
  , makeTracePolicyForTrace

    -- * Running the LSP pipeline
  , runOneshot
  , runOneshotVerbose

    -- * Rendering
  , TraceTextMode(..)
  , traceTextModeReader
  , renderTraceTextMode
  , renderEvalOutput
  , renderEvalValue
  , indentBlockText

    -- * GraphViz format
  , GraphVizFormat(..)
  , graphVizFormatReader
  , graphVizFormatExt
  , graphVizFormatToTraceFormat

    -- * Diagnostics output
  , putDiagnostics
  , diagnosticsToJson
  ) where

import Base.Text (Text)
import qualified Base.Text as Text
import qualified Data.Aeson as Aeson
import Data.Maybe (catMaybes)
import Data.Time (UTCTime)
import Options.Applicative (ReadM, eitherReader, long, metavar, help, option, optional)
import qualified Options.Applicative as Options
import System.IO (hPutStrLn, stderr)
import Control.Applicative ((<|>))

import LSP.L4.Oneshot (oneshotL4Action)
import qualified LSP.L4.Oneshot as Oneshot
import LSP.Logger
  ( Priority(..)
  , Recorder
  , WithPriority(..)
  , cfilter
  , cmapWithPrio
  , makeRefRecorder
  , pretty
  )
import Language.LSP.Protocol.Types (NormalizedFilePath)
import Development.IDE.Graph (Action)

import L4.EvaluateLazy
  ( EvalConfig
  , EvalDirectiveResult(..)
  , EvalDirectiveValue(..)
  , parseFixedNow
  , prettyEvalException
  , readFixedNowEnv
  , resolveEvalConfig
  )
import qualified L4.EvaluateLazy.GraphViz2 as GraphViz2
import L4.Parser.SrcSpan (prettySrcRange)
import L4.Print (prettyLayout)
import qualified L4.TracePolicy as TracePolicy
import L4.TracePolicy
  ( TraceDestination(..)
  , TraceLevel(..)
  , TraceOptions(..)
  , TracePolicy(..)
  )

----------------------------------------------------------------------------
-- Shared options
----------------------------------------------------------------------------

newtype FixedNowOpt = FixedNowOpt { unFixedNow :: Maybe UTCTime }

parseFixedNowText :: String -> Either String UTCTime
parseFixedNowText s =
  maybe
    (Left "Unable to parse --fixed-now; expected ISO8601 UTC like 2025-01-31T15:45:30Z")
    Right
    (parseFixedNow (Text.pack s))

fixedNowReader :: ReadM UTCTime
fixedNowReader = eitherReader parseFixedNowText

fixedNowParser :: Options.Parser FixedNowOpt
fixedNowParser = fmap FixedNowOpt $ optional $
  option fixedNowReader
    ( long "fixed-now"
    <> metavar "ISO8601"
    <> help "Pin evaluation clock (e.g. 2025-01-31T15:45:30Z) so NOW/TODAY stay deterministic"
    )

----------------------------------------------------------------------------
-- Evaluation config
----------------------------------------------------------------------------

-- | Build an `EvalConfig` honoring either the `--fixed-now` flag or the
-- `L4_NOW` environment variable. Use this for `run`, `check`, `batch`.
makeEvalConfig :: FixedNowOpt -> IO EvalConfig
makeEvalConfig (FixedNowOpt mClock) = do
  envFixed <- readFixedNowEnv
  resolveEvalConfig (mClock <|> envFixed) makeTracePolicyForEval

makeTracePolicyForEval :: TracePolicy
makeTracePolicyForEval =
  TracePolicy
    { evalDirectiveTrace = NoTrace
    , evaltraceDirectiveTrace = NoTrace
    }

-- | Trace policy used by `l4 trace`. `#EVAL` stays silent so the user can
-- still author `#EVAL` directives without being drowned in trace output;
-- `#EVALTRACE` directives do emit.
makeTracePolicyForTrace :: GraphVizFormat -> Maybe FilePath -> TracePolicy
makeTracePolicyForTrace fmt outputDir =
  let gvOpts = GraphViz2.GraphVizOptions
        { GraphViz2.includeValues = True
        , GraphViz2.showUnevaluated = True
        , GraphViz2.simplifyTrivial = False
        , GraphViz2.maxDepth = Nothing
        , GraphViz2.collapseFunctionLookups = False
        , GraphViz2.collapseSimplePaths = False
        , GraphViz2.deduplicateBindings = True
        }
      dest = case outputDir of
        Nothing -> Stdout
        Just dir -> FilesOnly dir
      traceFmt = graphVizFormatToTraceFormat fmt
  in TracePolicy
       { evalDirectiveTrace = NoTrace
       , evaltraceDirectiveTrace = CollectTrace (TraceOptions traceFmt dest gvOpts)
       }

----------------------------------------------------------------------------
-- Running the LSP pipeline
----------------------------------------------------------------------------

-- | Run a one-shot Shake action against `fp` and collect both the inner
-- action's result *and* any Warning/Error priority diagnostic lines the
-- LSP rules emitted. Info/Debug chatter (e.g. import-resolution progress)
-- is filtered out so CLI output stays clean — callers that want verbose
-- progress can use 'runOneshotVerbose' with their own recorder.
runOneshot
  :: EvalConfig
  -> FilePath
  -> (NormalizedFilePath -> Action b)
  -> IO ([Text], b)
runOneshot evalConfig fp act = do
  (getLog, refRecorder) <- makeRefRecorder
  let prettyRecorder = cmapWithPrio pretty refRecorder
      -- Drop Debug-priority messages (very verbose Shake internals) but
      -- keep Info/Warning/Error: real parse/type diagnostics come through
      -- at Info priority via the LSP publishDiagnostics hook, so we can't
      -- just filter by priority alone.  We scrub progress chatter out of
      -- the rendered Text list below with `isProgressChatter`.
      keep wp = wp.priority /= Debug
      filteredRec = cfilter keep prettyRecorder
  res <- oneshotL4Action filteredRec evalConfig fp act
  rawErrs <- getLog
  -- Post-filter: drop lines matching known progress-chatter prefixes.
  pure (filter (not . isProgressChatter) rawErrs, res)

-- | Returns True for rendered log lines that represent internal
-- bookkeeping (import resolution, VFS lookups, Shake db progress) rather
-- than user-facing diagnostics.
--
-- The rendered format puts priority + content inline, e.g.
-- @"Info | [Import Resolution] Found on filesystem: ..."@, so we look
-- past the @"Priority | "@ prefix.
isProgressChatter :: Text -> Bool
isProgressChatter txt =
  let body    = dropPriorityPrefix txt
      starts p = p `Text.isPrefixOf` body
  in  starts "[Import Resolution]"
   || starts "[VFS"
   || starts "Ide: Shake"
   || starts "Shake:"
   || starts "Finishing writing Shake database"
   || starts "Opening Shake database"
   || Text.null (Text.strip body)

dropPriorityPrefix :: Text -> Text
dropPriorityPrefix txt =
  case Text.breakOn " | " txt of
    (_, rest) | not (Text.null rest) -> Text.stripStart (Text.drop 3 rest)
    _                                -> txt

-- | Like 'runOneshot', but routes LSP diagnostics through a caller-supplied
-- recorder (e.g. one backed by stderr) and returns only the action's result.
-- Used by @l4 trace@ / @l4 state-graph@ where the caller wants interleaved
-- progress visible.
runOneshotVerbose
  :: Recorder (WithPriority Oneshot.Log)
  -> EvalConfig
  -> FilePath
  -> (NormalizedFilePath -> Action b)
  -> IO b
runOneshotVerbose recorder evalConfig fp act =
  oneshotL4Action recorder evalConfig fp act

----------------------------------------------------------------------------
-- Trace text rendering
----------------------------------------------------------------------------

data TraceTextMode
  = TraceTextNone
  | TraceTextFull
  deriving (Eq, Show)

renderTraceTextMode :: TraceTextMode -> String
renderTraceTextMode = \case
  TraceTextNone -> "none"
  TraceTextFull -> "full"

traceTextModeReader :: ReadM TraceTextMode
traceTextModeReader = eitherReader \input ->
  case Text.toLower (Text.pack input) of
    "none" -> Right TraceTextNone
    "off" -> Right TraceTextNone
    "full" -> Right TraceTextFull
    "text" -> Right TraceTextFull
    "ascii" -> Right TraceTextFull
    other -> Left $ "Invalid trace MODE: " <> Text.unpack other <> " (expected none|full)"

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

----------------------------------------------------------------------------
-- GraphViz format
----------------------------------------------------------------------------

data GraphVizFormat
  = DotFormat
  | PngFormat
  | SvgFormat
  deriving (Eq, Show)

graphVizFormatExt :: GraphVizFormat -> String
graphVizFormatExt = \case
  DotFormat -> "dot"
  PngFormat -> "png"
  SvgFormat -> "svg"

graphVizFormatReader :: ReadM GraphVizFormat
graphVizFormatReader = eitherReader \input ->
  case Text.toLower (Text.pack input) of
    "dot" -> Right DotFormat
    "png" -> Right PngFormat
    "svg" -> Right SvgFormat
    other -> Left $ "Invalid format: " <> Text.unpack other <> " (expected dot|png|svg)"

graphVizFormatToTraceFormat :: GraphVizFormat -> TracePolicy.TraceFormat
graphVizFormatToTraceFormat = \case
  DotFormat -> TracePolicy.DotFormat
  PngFormat -> TracePolicy.PngFormat
  SvgFormat -> TracePolicy.SvgFormat

----------------------------------------------------------------------------
-- Diagnostic dump helpers
----------------------------------------------------------------------------

-- | Write collected diagnostic lines to stderr, one per line.
putDiagnostics :: [Text] -> IO ()
putDiagnostics = mapM_ (hPutStrLn stderr . Text.unpack)

-- | Convert a list of rendered diagnostic lines to a JSON array of strings.
-- A richer structured-diagnostics shape is a future upgrade; for now the
-- JSON envelope quotes the already-prettified LSP log text.
diagnosticsToJson :: [Text] -> Aeson.Value
diagnosticsToJson = Aeson.toJSON
