module Main where

import Base (NonEmpty, for_, when, unless)
import Base.Text (Text)
import Data.List.NonEmpty (some1)
import Options.Applicative (fullDesc, header, footer, helper, info, metavar, strArgument, help, short, long, switch, progDesc)
import qualified Options.Applicative as Options
import System.Directory (getCurrentDirectory)
import System.Exit (exitSuccess, exitFailure)
import Text.Pretty.Simple (pShow)

import qualified LSP.Core.Shake as Shake
import LSP.Logger
import Language.LSP.Protocol.Types

import qualified LSP.L4.Rules as Rules
import LSP.L4.Oneshot (oneshotL4Action)
import qualified LSP.L4.Oneshot as Oneshot

import L4.Syntax (Module, Name)

data Log
  = IdeLog Oneshot.Log
  | CheckFailed !NormalizedUri
  | ExactPrint !Text
  | ShowAst !(Module Name)
  | SuccessOnly

instance Pretty Log where
  pretty = \ case
    IdeLog l -> "Ide:" <+> pretty l
    CheckFailed uri -> "Checking" <+> pretty uri <+> "failed."
    ExactPrint ep -> nest 2 $ vsep [pretty SuccessOnly, pretty ep]
    ShowAst ast -> pretty $ pShow ast
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
      _ <- Shake.use Rules.EvaluateLazy uri
      mep <- Shake.use Rules.ExactPrint uri
      case (mtc, mep) of
        (Just tcRes, Just ep)
          | tcRes.success -> do
              when options.verbose $ logWith recorder Info $ ExactPrint ep
              when options.showAst $ do
                mast <- Shake.use Rules.GetParsedAst uri
                case mast of
                  Just ast -> logWith recorder Info $ ShowAst ast
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
  , showAst :: Bool }

optionsDescription :: Options.Parser Options
optionsDescription = MkOptions
  <$> some1 (strArgument (metavar "L4FILE"))
  <*> switch (long "verbose" <> short 'v' <> help "Enable verbose output: reformats and prints the input files")
  <*> switch (long "ast" <> short 'a' <> help "Show abstract syntax tree")


optionsConfig :: Options.ParserInfo Options
optionsConfig = info
  (helper <*> optionsDescription)
  (fullDesc
    <> progDesc "Parse the given FILES and reports warnings, errors, and eval outputs. Returns with exitcode 0 (success) if parse succeeds. Returns exitcode 1 if parse fails."
    <> header "## jl4-cli"
    <> footer "## Part of the L4 tool family from Legalese.com"
  )
