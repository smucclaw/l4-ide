-- | @l4 fmt FILE@ — exact-print / reformat an L4 file to stdout.
--
-- Mirrors the @gofmt@ / @rustfmt@ convention: no flags means "print the
-- reformatted source to stdout". Redirect with @>@ to write in place.
-- Exits 0 on success, 1 if the file fails to parse/typecheck.
module L4.Cli.Fmt
  ( FmtOptions(..)
  , fmtOptionsParser
  , fmtCmd
  ) where

import qualified Base.Text as Text
import Options.Applicative
import System.Exit (exitFailure, exitSuccess)

import qualified LSP.Core.Shake as Shake
import qualified LSP.L4.Rules as Rules
import Language.LSP.Protocol.Types (normalizedFilePathToUri)

import L4.Cli.Common

----------------------------------------------------------------------------
-- Options
----------------------------------------------------------------------------

newtype FmtOptions = FmtOptions
  { fmtFile :: FilePath
  }

fmtOptionsParser :: Parser FmtOptions
fmtOptionsParser = FmtOptions
  <$> strArgument (metavar "FILE" <> help "Path to the .l4 file to reformat")

----------------------------------------------------------------------------
-- Entry point
----------------------------------------------------------------------------

fmtCmd :: FmtOptions -> IO ()
fmtCmd opts = do
  evalConfig <- makeEvalConfig (FixedNowOpt Nothing)
  (errs, mEp) <- runOneshot evalConfig opts.fmtFile \nfp -> do
    let uri = normalizedFilePathToUri nfp
    _ <- Shake.addVirtualFileFromFS nfp
    Shake.use Rules.ExactPrint uri

  case mEp of
    Just ep -> do
      Text.putStr ep
      exitSuccess
    Nothing -> do
      putDiagnostics errs
      exitFailure
