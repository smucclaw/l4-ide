module L4.Main where

import Base
import qualified Base.Text as Text

import Options.Applicative as Options

import L4.Parser
import L4.TypeCheck

data Options =
  MkOptions
    { files :: [FilePath]
    }

progName :: String
progName = "l4"

optionsDescription :: Options.Parser Options
optionsDescription =
  MkOptions
  <$> many (strArgument (metavar "FILES..."))

optionsConfig :: Options.ParserInfo Options
optionsConfig =
  info (optionsDescription <**> helper)
    (  fullDesc
    <> header progName
    )

main :: IO ()
main = do
  options <- Options.execParser optionsConfig
  if null options.files
    then do
      hPutStrLn stderr (progName <> ": no input files given; use --help for help")
    else
      exactprintFiles options.files

exactprintFiles :: [FilePath] -> IO ()
exactprintFiles =
  traverse_ $ \ file -> do
    input <- Text.readFile file
    Text.putStr (checkAndExactPrintFile file input)

parseFiles :: [FilePath] -> IO ()
parseFiles =
  traverse_ (\ file -> parseFile program file =<< Text.readFile file)
