-- | @l4 check FILE@ — fast typecheck without evaluating @#EVAL@ directives.
--
-- Intended for CI lint, pre-commit hooks, and editor "is this valid?"
-- checks. Skips the `EvaluateLazy` Shake rule entirely, so files with
-- expensive @#EVAL@ bodies still check in milliseconds.
module L4.Cli.Check
  ( CheckOptions(..)
  , checkOptionsParser
  , checkCmd
  ) where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.ByteString.Lazy.Char8 as BSL8
import qualified Base.Text as Text
import Options.Applicative
import System.Exit (exitFailure, exitSuccess)
import System.IO (stderr)

import qualified LSP.Core.Shake as Shake
import qualified LSP.L4.Rules as Rules
import Language.LSP.Protocol.Types (normalizedFilePathToUri)

import L4.Cli.Common

----------------------------------------------------------------------------
-- Options
----------------------------------------------------------------------------

data CheckOptions = CheckOptions
  { checkFile     :: FilePath
  , checkJsonOut  :: Bool
  , checkFixedNow :: FixedNowOpt
  }

checkOptionsParser :: Parser CheckOptions
checkOptionsParser = CheckOptions
  <$> strArgument (metavar "FILE" <> help "Path to the .l4 file to typecheck")
  <*> switch (long "json" <> help "Emit a JSON envelope instead of human text")
  <*> fixedNowParser

----------------------------------------------------------------------------
-- Entry point
----------------------------------------------------------------------------

checkCmd :: CheckOptions -> IO ()
checkCmd opts = do
  evalConfig <- makeEvalConfig opts.checkFixedNow
  (errs, mTc) <- runOneshot evalConfig opts.checkFile \nfp -> do
    let uri = normalizedFilePathToUri nfp
    _ <- Shake.addVirtualFileFromFS nfp
    Shake.use Rules.SuccessfulTypeCheck uri

  let typecheckOk = case mTc of
        Just tc | tc.success -> True
        _                    -> False
      -- Exit code depends on the Shake rule, not the log stream: see
      -- the same comment in L4.Cli.Run for why.
      overallOk = typecheckOk

  if opts.checkJsonOut
    then do
      BSL8.putStrLn $ Aeson.encode $ Aeson.object
        [ Key.fromString "file"        Aeson..= opts.checkFile
        , Key.fromString "ok"          Aeson..= overallOk
        , Key.fromString "diagnostics" Aeson..= diagnosticsToJson errs
        ]
      if overallOk then exitSuccess else exitFailure
    else do
      putDiagnostics errs
      if overallOk
        then do
          Text.putStrLn "Check succeeded."
          exitSuccess
        else do
          -- Failed typecheck with no captured diagnostics: make sure the
          -- user sees *something* before we exit non-zero.
          case errs of
            [] -> Text.hPutStrLn stderr "Check failed (file may be empty, unparseable, or not found)"
            _  -> pure ()
          exitFailure
