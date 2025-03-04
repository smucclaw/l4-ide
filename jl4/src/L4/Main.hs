module L4.Main where

import Base
import qualified Base.Text as Text

import Options.Applicative as Options

import L4.Lexer (SrcRange, prettySrcRange)
import L4.Parser
import L4.TypeCheck
import L4.ExactPrint
import L4.Annotation

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

-- | Parse, typecheck and exact-print a program.
checkAndExactPrintFile :: String -> Text -> Text
checkAndExactPrintFile file input =
  case execProgramParser file input of
    Left errs -> Text.unlines $ fmap (.message) $ toList errs
    Right (prog, _) ->
      "Parsing successful\n\n"
      <>
      case doCheckProgram prog of
        CheckResult {errors = []} ->
          "Typechecking successful\n\n"
          <> case exactprint prog of
               Left epError -> prettyTraverseAnnoError epError
               Right ep -> ep
        CheckResult {errors} ->
          Text.unlines (map (\ err -> cliErrorMessage file (rangeOf err) (prettyCheckErrorWithContext err)) errors)

cliErrorMessage :: FilePath -> Maybe SrcRange -> [Text] -> Text
cliErrorMessage fp mrange msg =
  Text.unlines
    ( prettySrcRange (Just fp) mrange <> ":"
    : map ("  " <>) msg
    )

-- | Parse a source file and exact-print the result.
exactprintProgram :: String -> Text -> Text
exactprintProgram file input =
  case execProgramParser file input of
    Left errs -> Text.unlines $ fmap (.message) $ toList errs
    Right (prog, _) ->
      case exactprint prog of
        Left epError -> prettyTraverseAnnoError epError
        Right ep -> ep
