module Main where

import Base (NonEmpty, for_)
import Base.Text (Text)
import Data.List.NonEmpty (some1)
import Options.Applicative (fullDesc, header, helper, info, metavar, strArgument, help, short, long, switch)
import qualified Options.Applicative as Options
import System.Directory (getCurrentDirectory)
import System.Exit (exitSuccess, exitFailure)

import qualified LSP.Core.Shake as Shake
import LSP.Logger
import Language.LSP.Protocol.Types

import qualified LSP.L4.Rules as Rules
import LSP.L4.Oneshot (oneshotL4Action)
import qualified LSP.L4.Oneshot as Oneshot

data Log
  = IdeLog Oneshot.Log
  | CheckFailed !NormalizedUri
  | ExactPrint !Text
  | SuccessOnly

instance Pretty Log where
  pretty = \ case
    IdeLog l -> "Ide:" <+> pretty l
    CheckFailed uri -> "Checking" <+> pretty uri <+> "failed."
    ExactPrint ep -> nest 2 $ vsep [pretty SuccessOnly, pretty ep]
    SuccessOnly -> "Checking succeeded."

main :: IO ()
main = do
  curDir   <- getCurrentDirectory
  recorder <- cmapWithPrio pretty <$> makeDefaultStderrRecorder Nothing
  options  <- Options.execParser optionsConfig

  (getErrs, errRecorder) <- fmap (cmapWithPrio pretty) <$> makeRefRecorder

  oneshotL4Action (cmapWithPrio IdeLog recorder) curDir \_ ->
    for_ options.files \fp -> do
      let nfp = toNormalizedFilePath fp
          uri = normalizedFilePathToUri nfp
      _ <- Shake.addVirtualFileFromFS nfp
      mtc <- Shake.use Rules.SuccessfulTypeCheck  uri
      _ <- Shake.use Rules.Evaluate uri
      _ <- Shake.use Rules.EvaluateLazy uri
      mep <- Shake.use Rules.ExactPrint uri
      case (mtc, mep) of
        (Just tcRes, Just ep)
          | tcRes.success -> logWith recorder Info  $ if options.verbose
                                                      then ExactPrint ep
                                                      else SuccessOnly
        (_, _)            -> do
          logWith    recorder Error $ CheckFailed uri
          logWith errRecorder Error $ CheckFailed uri

  errs <- getErrs
  if null errs
  then exitSuccess
  else exitFailure

data Options = MkOptions
  { files :: NonEmpty FilePath
  , verbose :: Bool }

optionsDescription :: Options.Parser Options
optionsDescription = MkOptions
  <$> some1 (strArgument (metavar "FILES..."))
  <*> switch (long "verbose" <> short 'v' <> help "Enable verbose output")


optionsConfig :: Options.ParserInfo Options
optionsConfig = info
  (helper <*> optionsDescription)
  (fullDesc <> header "jl4")
