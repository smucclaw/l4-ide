-- | @l4 run FILE@ — typecheck and evaluate an L4 file, printing
-- diagnostics and the results of every @#EVAL@ / @#EVALTRACE@ directive.
--
-- Text output by default; @--json@ flips to a machine-readable envelope
-- suitable for editors, CI, and the writing-l4-rules skill.
module L4.Cli.Run
  ( RunOptions(..)
  , runOptionsParser
  , runCmd
  ) where

import Base (for_)
import Base.Text (Text)
import qualified Base.Text as Text
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.ByteString.Lazy.Char8 as BSL8
import Options.Applicative
import System.Exit (exitFailure, exitSuccess)
import System.IO (stderr)

import qualified LSP.Core.Shake as Shake
import qualified LSP.L4.Rules as Rules
import Language.LSP.Protocol.Types (normalizedFilePathToUri)

import L4.EvaluateLazy (EvalDirectiveResult(..), EvalDirectiveValue(..))
import L4.Parser.SrcSpan (prettySrcRange)

import L4.Cli.Common

----------------------------------------------------------------------------
-- Options
----------------------------------------------------------------------------

data RunOptions = RunOptions
  { runFile      :: FilePath
  , runJsonOut   :: Bool
  , runTrace     :: TraceTextMode
  , runFixedNow  :: FixedNowOpt
  }

runOptionsParser :: Parser RunOptions
runOptionsParser = RunOptions
  <$> strArgument (metavar "FILE" <> help "Path to the .l4 file to evaluate")
  <*> switch (long "json" <> help "Emit a JSON envelope instead of human text")
  <*> option traceTextModeReader
        ( long "trace"
        <> short 't'
        <> metavar "MODE"
        <> value TraceTextFull
        <> showDefaultWith renderTraceTextMode
        <> help "Trace text mode for #EVALTRACE: none | full"
        )
  <*> fixedNowParser

----------------------------------------------------------------------------
-- Entry point
----------------------------------------------------------------------------

runCmd :: RunOptions -> IO ()
runCmd opts = do
  evalConfig <- makeEvalConfig opts.runFixedNow
  (errs, (mTc, mEval)) <- runOneshot evalConfig opts.runFile \nfp -> do
    let uri = normalizedFilePathToUri nfp
    _ <- Shake.addVirtualFileFromFS nfp
    mtc   <- Shake.use Rules.SuccessfulTypeCheck uri
    meval <- Shake.use Rules.EvaluateLazy uri
    pure (mtc, meval)

  let typecheckOk = case mTc of
        Just tc | tc.success -> True
        _                    -> False
      evalResults = case mEval of
        Just rs -> rs
        Nothing -> []
      -- Exit code is based on Shake rule results, NOT on the log stream.
      -- The LSP rule publishes #EVAL results as Information-severity
      -- diagnostics via the logger, so a file with passing `#EVAL`s still
      -- produces non-empty `errs` — treating that as failure would be a
      -- false positive.
      overallOk = typecheckOk

  if opts.runJsonOut
    then do
      BSL8.putStrLn $ Aeson.encode $ Aeson.object
        [ Key.fromString "file"        Aeson..= opts.runFile
        , Key.fromString "ok"          Aeson..= overallOk
        , Key.fromString "diagnostics" Aeson..= diagnosticsToJson errs
        , Key.fromString "results"     Aeson..= map evalResultToJson evalResults
        ]
      if overallOk then exitSuccess else exitFailure
    else do
      -- Plain text: diagnostics first (stderr), then eval results (stdout).
      putDiagnostics errs
      for_ (zip [1 :: Int ..] evalResults) \(idx, r) ->
        Text.putStrLn (renderEvalOutput opts.runTrace idx r <> "\n")
      if overallOk
        then do
          -- Only say "success" when there are no eval results, so output
          -- stays grep-friendly and CI-readable.
          if null evalResults
            then Text.putStrLn "Checking succeeded."
            else pure ()
          exitSuccess
        else do
          case errs of
            [] | null evalResults ->
                 Text.hPutStrLn stderr "Run failed (file may be empty, unparseable, or not found)"
            _ -> pure ()
          exitFailure

----------------------------------------------------------------------------
-- JSON shape
----------------------------------------------------------------------------

evalResultToJson :: EvalDirectiveResult -> Aeson.Value
evalResultToJson MkEvalDirectiveResult{range = mRange, result, trace = _} =
  Aeson.object
    [ Key.fromString "range" Aeson..= fmap prettySrcRange mRange
    , Key.fromString "kind"  Aeson..= kindText
    , Key.fromString "value" Aeson..= valueJson
    ]
  where
    (kindText, valueJson) = case result of
      Assertion b             -> ("assertion" :: Text, Aeson.Bool b)
      Reduction (Left exc)    -> ("error" :: Text, Aeson.String (Text.unlines ["evaluation exception:", Text.pack (show exc)]))
      Reduction (Right val)   -> ("value" :: Text, Aeson.String (renderEvalValue (Reduction (Right val))))
