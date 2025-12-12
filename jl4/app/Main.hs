module Main where

import Base (NonEmpty, for_, when, unless, liftIO)
import Base.Text (Text)
import qualified Base.Text as Text
import qualified Data.Text.Lazy as Text.Lazy
import Control.Applicative ((<|>))
import Data.List.NonEmpty (some1)
import Options.Applicative (ReadM, eitherReader, fullDesc, header, footer, helper, info, metavar, option, optional, strArgument, help, short, long, switch, progDesc)
import qualified Options.Applicative as Options
import System.Directory (getCurrentDirectory)
import System.Exit (exitSuccess, exitFailure)
import System.IO (stdout, hIsTerminalDevice)
import Text.Pretty.Simple (pShow, pShowNoColor)

import qualified LSP.Core.Shake as Shake
import LSP.Logger
import Language.LSP.Protocol.Types

import qualified LSP.L4.Rules as Rules
import LSP.L4.Oneshot (oneshotL4Action)
import qualified LSP.L4.Oneshot as Oneshot

import L4.EvaluateLazy (parseFixedNow, readFixedNowEnv, resolveEvalConfig)
import Data.Time (UTCTime)

data Log
  = IdeLog Oneshot.Log
  | CheckFailed !NormalizedUri
  | ExactPrint !Text
  | ShowAst !Text.Lazy.Text
  | SuccessOnly

instance Pretty Log where
  pretty = \ case
    IdeLog l -> "Ide:" <+> pretty l
    CheckFailed uri -> "Checking" <+> pretty uri <+> "failed."
    ExactPrint ep -> nest 2 $ vsep [pretty SuccessOnly, pretty ep]
    ShowAst ast -> pretty ast
    SuccessOnly -> "Checking succeeded."

main :: IO ()
main = do
  curDir   <- getCurrentDirectory
  recorder <- cmapWithPrio pretty <$> makeDefaultStderrRecorder Nothing
  options  <- Options.execParser optionsConfig
  envFixed <- readFixedNowEnv
  evalConfig <- resolveEvalConfig (options.fixedNow <|> envFixed)

  (getErrs, errRecorder) <- fmap (cmapWithPrio pretty) <$> makeRefRecorder

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
  }

optionsDescription :: Options.Parser Options
optionsDescription = MkOptions
  <$> some1 (strArgument (metavar "L4FILE"))
  <*> switch (long "verbose" <> short 'v' <> help "Enable verbose output: reformats and prints the input files")
  <*> switch (long "ast" <> short 'a' <> help "Show abstract syntax tree")
  <*> optional (option fixedNowReader (long "fixed-now" <> metavar "ISO8601" <> help "Pin evaluation clock (e.g. 2025-01-31T15:45:30Z) so NOW/TODAY stay deterministic"))

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
