module Main where

import Base (NonEmpty)
import Base.Text (Text)
import Data.List.NonEmpty (some1)
import Data.Traversable
import Development.IDE.Graph.Database
import Options.Applicative (fullDesc, header, helper, info, metavar, strArgument)
import qualified Options.Applicative as Options
import System.Directory

import qualified LSP.Core.FileStore as Store
import LSP.Core.Shake (IdeState (..))
import qualified LSP.Core.Shake as Shake
import LSP.Logger
import Language.LSP.Protocol.Types

import qualified LSP.L4.Rules as Rules

data Log
  = ShakeLog Shake.Log
  | RulesLog Rules.Log
  | StoreLog Store.Log
  | CheckFailed !NormalizedUri
  | ExactPrint !Text
  deriving stock (Show)

instance Pretty Log where
  pretty = \case
    ShakeLog l -> "Shake:" <+> pretty l
    RulesLog l -> "Rules:" <+> pretty l
    StoreLog l -> "Store:" <+> pretty l
    CheckFailed uri -> "Checking" <+> pretty uri <+> "failed."
    ExactPrint ep -> nest 2 $ vsep ["Checking succeeded.", pretty ep]

main :: IO ()
main = do
  curDir   <- getCurrentDirectory
  recorder <- cmapWithPrio pretty <$> makeDefaultStderrRecorder Nothing
  options  <- Options.execParser optionsConfig
  state    <- Shake.oneshotIdeState (cmapWithPrio ShakeLog recorder) curDir do
    Store.fileStoreRules (cmapWithPrio StoreLog recorder) (const $ pure False)
    Rules.jl4Rules (cmapWithPrio RulesLog recorder)

  () <$ shakeRunDatabase
      state.shakeDb
      [ for options.files \fp -> do
          let uri = normalizedFilePathToUri $ toNormalizedFilePath fp
          Shake.addVirtualFileFromFS (toNormalizedFilePath fp)
          mtc <- Shake.use Rules.TypeCheck uri
          mep <- Shake.use Rules.ExactPrint uri
          case (mtc, mep) of
            (Just tcRes, Just ep) | tcRes.success -> logWith recorder Info $ ExactPrint ep
            (_, _) -> logWith recorder Error $ CheckFailed uri
      ]

newtype Options = MkOptions {files :: NonEmpty FilePath}

optionsDescription :: Options.Parser Options
optionsDescription = MkOptions <$> some1 (strArgument (metavar "FILES..."))

optionsConfig :: Options.ParserInfo Options
optionsConfig = info
  (helper <*> optionsDescription)
  (fullDesc <> header "jl4")
