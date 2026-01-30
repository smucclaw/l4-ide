{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}

-- | CLI for the L4 ACTUS/FIBO static analyzer.
--
-- Usage:
--   jl4-actus [OPTIONS] FILE...
--
-- Options:
--   --fibo PATH      Path to FIBO repository
--   --json           Output as JSON
--   --rdf            Output as RDF/Turtle
--   --markdown       Output as Markdown (default)
--   --rebuild-cache  Rebuild the ontology cache
--   --no-cache       Don't use the binary cache
--   --min-confidence Minimum confidence threshold (0.0-1.0)
--   -h, --help       Show help
module Main (main) where

import Control.Monad (when)
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import L4.ACTUS.Analyzer
import L4.ACTUS.Ontology.Cache (invalidateCache)
import qualified L4.ACTUS.Report.JSON as JSON
import qualified L4.ACTUS.Report.Markdown as Markdown
import qualified L4.ACTUS.Report.RDF as RDF
import Options.Applicative
import System.Exit (exitFailure, exitSuccess)

-- | Output format.
data OutputFormat = FormatJSON | FormatRDF | FormatMarkdown
  deriving stock (Eq, Show)

-- | Command-line options.
data Options = Options
  { optFiboPath :: Maybe FilePath
  , optCachePath :: Maybe FilePath
  , optOutputFormat :: OutputFormat
  , optRebuildCache :: Bool
  , optNoCache :: Bool
  , optMinConfidence :: Double
  , optFiles :: [FilePath]
  }
  deriving stock (Show)

-- | Parse command-line options.
optionsParser :: Parser Options
optionsParser = do
  optFiboPath <-
    optional $
      strOption
        ( long "fibo"
            <> metavar "PATH"
            <> help "Path to FIBO repository"
        )

  optCachePath <-
    optional $
      strOption
        ( long "cache"
            <> metavar "PATH"
            <> help "Path to cache directory"
        )

  optOutputFormat <-
    flag' FormatJSON (long "json" <> help "Output as JSON")
      <|> flag' FormatRDF (long "rdf" <> help "Output as RDF/Turtle")
      <|> flag' FormatMarkdown (long "markdown" <> help "Output as Markdown")
      <|> pure FormatMarkdown

  optRebuildCache <-
    switch
      ( long "rebuild-cache"
          <> help "Rebuild the ontology cache"
      )

  optNoCache <-
    switch
      ( long "no-cache"
          <> help "Don't use the binary cache"
      )

  optMinConfidence <-
    option
      auto
      ( long "min-confidence"
          <> metavar "FLOAT"
          <> value 0.3
          <> help "Minimum confidence threshold (0.0-1.0)"
      )

  optFiles <-
    many $
      argument str (metavar "FILE..." <> help "L4 files to analyze")

  pure Options {..}

-- | CLI description.
optionsInfo :: ParserInfo Options
optionsInfo =
  info
    (optionsParser <**> helper)
    ( fullDesc
        <> progDesc "Classify L4 contracts by ACTUS type and FIBO class"
        <> header "jl4-actus - L4 ACTUS/FIBO Static Analyzer"
    )

main :: IO ()
main = do
  opts <- execParser optionsInfo

  -- Handle cache operations
  when opts.optRebuildCache $ do
    invalidateCache opts.optCachePath
    putStrLn "Cache invalidated."

  -- Check for files
  when (null opts.optFiles) $ do
    putStrLn "Error: No input files specified."
    putStrLn "Usage: jl4-actus [OPTIONS] FILE..."
    exitFailure

  -- Build config
  let config =
        defaultConfig
          { fiboPath = opts.optFiboPath
          , cachePath = opts.optCachePath
          , useCache = not opts.optNoCache
          , minConfidence = opts.optMinConfidence
          }

  -- Analyze files
  result <-
    case opts.optFiles of
      [singleFile] -> analyzeFile config singleFile
      _ -> analyzeFiles config opts.optFiles

  case result of
    Left err -> do
      putStrLn $ "Error: " ++ show err
      exitFailure
    Right classification -> do
      let output = formatOutput opts.optOutputFormat classification
      TIO.putStrLn output
      exitSuccess

-- | Format output according to requested format.
formatOutput :: OutputFormat -> ClassificationResult -> Text
formatOutput FormatJSON = JSON.toJSONPretty
formatOutput FormatRDF = RDF.toTurtle
formatOutput FormatMarkdown = Markdown.toMarkdown
