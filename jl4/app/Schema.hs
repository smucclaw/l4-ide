module Main where

import Base.Text (Text)
import qualified Base.Text as Text
import qualified Data.Aeson.Encode.Pretty as AesonPretty
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Language.LSP.Protocol.Types (normalizedFilePathToUri, toNormalizedFilePath)
import Options.Applicative
import System.Directory (getCurrentDirectory)
import System.Exit (ExitCode (..), exitWith)
import System.IO (hPutStrLn, stderr)

import qualified LSP.Core.Shake as Shake
import qualified LSP.L4.Rules as Rules
import LSP.L4.Oneshot (oneshotL4Action)
import LSP.Logger

import L4.EvaluateLazy (resolveEvalConfig)
import L4.Export (ExportedFunction (..), getExportedFunctions)
import L4.JsonSchema (SchemaContext (..), emptyContext, generateJsonSchema)
import L4.Syntax (TopDecl (..), Declare (..), Module (..), Resolved, Section (..), AppForm (..), rawNameToText, rawName, getActual)

data Options = MkOptions
  { file :: !FilePath
  , functionName :: !(Maybe Text)
  , output :: !(Maybe FilePath)
  }

optionsParser :: Parser Options
optionsParser =
  MkOptions
    <$> strArgument (metavar "L4FILE" <> help "L4 source file to generate schema from")
    <*> optional (strOption (long "function" <> short 'f' <> metavar "NAME" <> help "Specific exported function to generate schema for"))
    <*> optional (strOption (long "output" <> short 'o' <> metavar "FILE" <> help "Output file (stdout if omitted)"))

optionsConfig :: ParserInfo Options
optionsConfig =
  info
    (helper <*> optionsParser)
    ( fullDesc
        <> progDesc "Generate JSON Schema from an L4 program's exported functions"
        <> header "jl4-schema - JSON Schema generator for L4"
    )

main :: IO ()
main = do
  options <- execParser optionsConfig
  curDir <- getCurrentDirectory
  recorder <- cmapWithPrio pretty <$> makeDefaultStderrRecorder Nothing
  evalConfig <- resolveEvalConfig Nothing

  result <- oneshotL4Action recorder evalConfig curDir \_ -> do
    let nfp = toNormalizedFilePath options.file
        uri = normalizedFilePathToUri nfp
    _ <- Shake.addVirtualFileFromFS nfp
    mtc <- Shake.use Rules.SuccessfulTypeCheck uri
    case mtc of
      Nothing -> pure Nothing
      Just tcRes
        | not tcRes.success -> pure Nothing
        | otherwise -> pure $ Just tcRes.module'

  case result of
    Nothing -> do
      hPutStrLn stderr "Error: Failed to parse or typecheck L4 file"
      exitWith (ExitFailure 1)
    Just resolvedModule -> do
      let exports = getExportedFunctions resolvedModule
      if null exports
        then do
          hPutStrLn stderr "Error: No @export annotations found in file"
          exitWith (ExitFailure 2)
        else do
          let selectedExport = selectExport options.functionName exports
          case selectedExport of
            Nothing -> do
              hPutStrLn stderr $ "Error: Export not found: " <> maybe "(default)" Text.unpack options.functionName
              hPutStrLn stderr $ "Available exports: " <> List.intercalate ", " (map (Text.unpack . (.exportName)) exports)
              exitWith (ExitFailure 2)
            Just export -> do
              let ctx = buildContext resolvedModule
                  schema = generateJsonSchema ctx export
                  jsonOutput = AesonPretty.encodePretty schema
              case options.output of
                Nothing -> BL.putStrLn jsonOutput
                Just outFile -> BL.writeFile outFile jsonOutput

selectExport :: Maybe Text -> [ExportedFunction] -> Maybe ExportedFunction
selectExport Nothing exports =
  case List.find (.exportIsDefault) exports of
    Just e -> Just e
    Nothing -> case exports of
      [] -> Nothing
      (x : _) -> Just x
selectExport (Just name) exports =
  List.find (\e -> e.exportName == name) exports

buildContext :: Module Resolved -> SchemaContext
buildContext (MkModule _ _ section) =
  emptyContext{ctxDeclares = collectDeclares section}
 where
  collectDeclares :: Section Resolved -> Map.Map Text (Declare Resolved)
  collectDeclares (MkSection _ _ _ decls) =
    Map.fromList $ concatMap collectDecl decls

  collectDecl :: TopDecl Resolved -> [(Text, Declare Resolved)]
  collectDecl = \case
    Declare _ decl@(MkDeclare _ _ appForm _) ->
      [(getDeclName appForm, decl)]
    Section _ sub -> Map.toList $ collectDeclares sub
    _ -> []

  getDeclName (MkAppForm _ name _ _) = rawNameToText (rawName (getActual name))

data Log = IdeLog Rules.Log

instance Pretty Log where
  pretty (IdeLog l) = "Ide:" <+> pretty l
