module L4.Main where

import Base
import qualified Base.Text as Text

import Options.Applicative as Options

import L4.ExactPrint
import L4.Parser

data Options =
  MkOptions
    { files :: [FilePath]
    }

optionsDescription :: Options.Parser Options
optionsDescription =
  MkOptions
  <$> many (strArgument (metavar "FILES..."))

optionsConfig :: Options.ParserInfo Options
optionsConfig =
  info (optionsDescription <**> helper)
    (  fullDesc
    <> header "l4"
    )

main :: IO ()
main = do
  options <- Options.execParser optionsConfig
  if null options.files
    then do
      hPutStrLn stderr "l4: no input files given; use --help for help"
    else
      exactprintFiles options.files

exactprintFiles :: [FilePath] -> IO ()
exactprintFiles =
  traverse_ $ \ file -> do
    input <- Text.readFile file
    Text.putStr (exactprintFile file input)

parseFiles :: [FilePath] -> IO ()
parseFiles =
  traverse_ (\ file -> parseFile program file =<< Text.readFile file)
