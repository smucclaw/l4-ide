module Main where

import Base (NonEmpty, for_)
import Base.Text (Text)
import Data.List.NonEmpty (some1)
import Options.Applicative (fullDesc, header, helper, info, metavar, strArgument)
import qualified Options.Applicative as Options
import System.Directory (getCurrentDirectory)

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

instance Pretty Log where
  pretty = \case
    IdeLog l -> "Ide:" <+> pretty l
    CheckFailed uri -> "Checking" <+> pretty uri <+> "failed."
    ExactPrint ep -> nest 2 $ vsep ["Checking succeeded.", pretty ep]

main :: IO ()
main = do
  curDir   <- getCurrentDirectory
  recorder <- cmapWithPrio pretty <$> makeDefaultStderrRecorder Nothing
  options  <- Options.execParser optionsConfig
  oneshotL4Action (cmapWithPrio IdeLog recorder) curDir \_ ->
    for_ options.files \fp -> do
      let nfp = toNormalizedFilePath fp
          uri = normalizedFilePathToUri nfp
      _ <- Shake.addVirtualFileFromFS nfp
      mtc <- Shake.use Rules.TypeCheck  uri
      mep <- Shake.use Rules.ExactPrint uri
      case (mtc, mep) of
        (Just tcRes, Just ep) | tcRes.success -> logWith recorder Info $ ExactPrint ep
        (_, _) -> logWith recorder Error $ CheckFailed uri

newtype Options = MkOptions {files :: NonEmpty FilePath}

optionsDescription :: Options.Parser Options
optionsDescription = MkOptions <$> some1 (strArgument (metavar "FILES..."))

optionsConfig :: Options.ParserInfo Options
optionsConfig = info
  (helper <*> optionsDescription)
  (fullDesc <> header "jl4")
