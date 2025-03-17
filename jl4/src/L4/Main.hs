module L4.Main where

import Base
import qualified Base.Text as Text

import Options.Applicative as Options

import L4.Parser.SrcSpan (SrcRange, prettySrcRange)
import L4.Parser
import L4.TypeCheck
import L4.ExactPrint
import L4.Annotation
import qualified Generics.SOP as SOP
import L4.Syntax
import L4.Evaluate
import qualified L4.Print as Print

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

parseAndCheck :: FilePath -> Text -> Either CliError (Program Resolved)
parseAndCheck file input =
  case execProgramParser file input of
    Left errs -> Left $ CliParserError file errs
    Right (prog, _) ->
      case doCheckProgram prog of
        CheckResult {errors = [], program = p} -> Right p
        ch -> Left $ CliCheckError file ch

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

-- | Parse a source file and exact-print the result.
exactprintProgram :: String -> Text -> Text
exactprintProgram file input =
  case execProgramParser file input of
    Left errs -> Text.unlines $ fmap (.message) $ toList errs
    Right (prog, _) ->
      case exactprint prog of
        Left epError -> prettyTraverseAnnoError epError
        Right ep -> ep

loadProgram :: FilePath -> IO (Either CliError (Program Resolved))
loadProgram file = do
  input <- Text.readFile file
  pure $ parseAndCheck file input

evaluateAndTrace :: FilePath -> IO ()
evaluateAndTrace file =
  loadProgram file >>= \case
    Left err -> Text.putStrLn $ "Failed to check " <> Text.show file <> ":\n" <> prettyCliError err
    Right prog -> case doEvalProgram prog of
      evals -> do
        Text.putStr $
          Text.unlines $
            fmap
              (uncurry (cliErrorMessage file) . evalResultToMessage)
              evals

evalResultToMessage :: EvalResult -> (Maybe SrcRange, [Text])
evalResultToMessage (r, res, _t) = (Just r, [either Text.show Print.prettyLayout res])

-- ----------------------------------------------------------------------------
-- Error Handling
-- ----------------------------------------------------------------------------

data CliError
  = CliParserError FilePath (NonEmpty PError)
  | CliCheckError FilePath CheckResult
  deriving stock (Show, Eq, Generic)
  deriving anyclass (SOP.Generic)

prettyCliError :: CliError -> Text
prettyCliError = \case
  CliParserError file perrors ->
    "While parsing " <> Text.pack file <> ":" <>
    Text.intercalate "\n" (fmap (.message) $ toList perrors)
  CliCheckError file CheckResult{errors} ->
    Text.intercalate "\n" (map (\ err -> cliErrorMessage file (rangeOf err) (prettyCheckErrorWithContext err)) errors)

cliErrorMessage :: FilePath -> Maybe SrcRange -> [Text] -> Text
cliErrorMessage fp mrange msg =
  Text.intercalate "\n"
    ( prettySrcRange (Just fp) mrange <> ":"
    : map ("  " <>) msg
    )
