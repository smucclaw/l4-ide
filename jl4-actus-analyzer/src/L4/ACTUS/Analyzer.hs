{-# LANGUAGE StrictData #-}

-- | Main entry point for the L4 ACTUS/FIBO static analyzer.
--
-- This module provides the high-level API for classifying L4 contracts
-- by ACTUS type and FIBO class.
--
-- == Usage
--
-- @
-- import L4.ACTUS.Analyzer
--
-- main :: IO ()
-- main = do
--   result <- analyzeFile "contract.l4"
--   case result of
--     Left err -> print err
--     Right classification -> print classification
-- @
module L4.ACTUS.Analyzer (
  -- * Analysis Functions
  analyzeFile,
  analyzeFiles,
  analyzeModule,

  -- * Configuration
  AnalyzerConfig (..),
  defaultConfig,

  -- * Results
  AnalysisResult,
  AnalysisError (..),

  -- * Re-exports
  module L4.ACTUS.Ontology.Types,
  module L4.ACTUS.FeatureExtractor,
) where

import Control.Monad (forM)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import L4.ACTUS.FeatureExtractor
import L4.ACTUS.Matching.ACTUS (inferFIBOClass)
import L4.ACTUS.Matching.Rules (buildRulesFromOntology, defaultRules)
import L4.ACTUS.Matching.Scorer
import L4.ACTUS.Ontology.Cache
import L4.ACTUS.Ontology.Loader
import L4.ACTUS.Ontology.Types
import L4.API.VirtualFS (VFS, checkWithImports, vfsFromList, TypeCheckWithDepsResult(..))
import L4.Syntax (Module, Resolved)
import System.Directory (doesFileExist, getModificationTime, listDirectory)
import System.FilePath (takeBaseName, takeDirectory, (</>))

-- | Configuration for the analyzer.
data AnalyzerConfig = AnalyzerConfig
  { fiboPath :: Maybe FilePath
  -- ^ Path to FIBO repository (default: ~/src/edmcouncil/fibo)
  , cachePath :: Maybe FilePath
  -- ^ Path to cache directory (default: .cache/jl4-actus)
  , useCache :: Bool
  -- ^ Whether to use binary cache for ontology (default: True)
  , minConfidence :: Double
  -- ^ Minimum confidence threshold (default: 0.3)
  }
  deriving stock (Eq, Show)

-- | Default configuration.
defaultConfig :: AnalyzerConfig
defaultConfig =
  AnalyzerConfig
    { fiboPath = Nothing
    , cachePath = Nothing
    , useCache = True
    , minConfidence = 0.3
    }

-- | Analysis error type.
data AnalysisError
  = OntologyError OntologyLoadError
  | L4ParseError FilePath String
  | FileNotFoundError FilePath
  deriving stock (Eq, Show)

-- | Analysis result.
type AnalysisResult = Either AnalysisError ClassificationResult

-- | Analyze a single L4 file.
analyzeFile :: AnalyzerConfig -> FilePath -> IO AnalysisResult
analyzeFile config path = do
  exists <- doesFileExist path
  if not exists
    then pure $ Left $ FileNotFoundError path
    else do
      -- Load ontology
      ontologyResult <- loadOntologyWithCache config
      case ontologyResult of
        Left err -> pure $ Left $ OntologyError err
        Right ontology -> do
          -- Build VFS from all L4 files in the same directory
          vfs <- buildVFSFromDirectory path
          -- Read and parse L4 file
          source <- TIO.readFile path
          case checkWithImports vfs source of
            Left errs -> pure $ Left $ L4ParseError path (T.unpack $ T.unlines errs)
            Right tcdResult -> do
              -- Analyze
              result <- analyzeModuleInternal config ontology [path] tcdResult.tcdModule
              pure $ Right result

-- | Analyze multiple L4 files together.
analyzeFiles :: AnalyzerConfig -> [FilePath] -> IO AnalysisResult
analyzeFiles config paths = do
  -- Check all files exist
  existences <- forM paths $ \p -> do
    exists <- doesFileExist p
    pure (p, exists)
  case filter (not . snd) existences of
    ((p, _) : _) -> pure $ Left $ FileNotFoundError p
    [] -> do
      -- Load ontology
      ontologyResult <- loadOntologyWithCache config
      case ontologyResult of
        Left err -> pure $ Left $ OntologyError err
        Right ontology -> do
          -- Build VFS from all provided files
          vfs <- buildVFSFromFiles paths
          -- Parse all L4 files and extract features
          allFeatures <- forM paths $ \p -> do
            source <- TIO.readFile p
            case checkWithImports vfs source of
              Left errs -> pure $ Left $ L4ParseError p (T.unpack $ T.unlines errs)
              Right tcdResult -> pure $ Right $ extractFeatures p tcdResult.tcdModule

          case sequence allFeatures of
            Left err -> pure $ Left err
            Right features -> do
              -- Combine features and analyze
              let combined = combineFeatures features
              result <- classifyFeatures config ontology paths combined
              pure $ Right result

-- | Analyze an already-parsed L4 module.
analyzeModule :: AnalyzerConfig -> FilePath -> Module Resolved -> IO AnalysisResult
analyzeModule config path mod' = do
  -- Load ontology
  ontologyResult <- loadOntologyWithCache config
  case ontologyResult of
    Left err -> pure $ Left $ OntologyError err
    Right ontology -> do
      result <- analyzeModuleInternal config ontology [path] mod'
      pure $ Right result

-- Internal implementation

-- | Load ontology with caching support.
loadOntologyWithCache :: AnalyzerConfig -> IO (Either OntologyLoadError OntologyGraph)
loadOntologyWithCache config
  | config.useCache = do
      cached <- loadFromCache config.cachePath
      case cached of
        Just graph -> pure $ Right graph
        Nothing -> loadAndCache config
  | otherwise = loadACTUSControlledVocabulary config.fiboPath

-- | Load ontology and save to cache.
loadAndCache :: AnalyzerConfig -> IO (Either OntologyLoadError OntologyGraph)
loadAndCache config = do
  let fiboPath' = fromMaybe defaultFiboPath config.fiboPath
      acvPath = fiboPath' <> "/" <> actusControlledVocabularyPath
  result <- loadACTUSControlledVocabulary config.fiboPath
  case result of
    Right graph -> do
      modTime <- getModificationTime acvPath
      saveToCache config.cachePath [(acvPath, modTime)] graph
      pure $ Right graph
    Left err -> pure $ Left err

-- | Analyze a module with loaded ontology.
analyzeModuleInternal ::
  AnalyzerConfig ->
  OntologyGraph ->
  [FilePath] ->
  Module Resolved ->
  IO ClassificationResult
analyzeModuleInternal config ontology paths mod' = do
  let firstPath = case paths of (p:_) -> p; [] -> ""
      features = extractFeatures firstPath mod'
  classifyFeatures config ontology paths features

-- | Classify extracted features.
classifyFeatures ::
  AnalyzerConfig ->
  OntologyGraph ->
  [FilePath] ->
  L4Features ->
  IO ClassificationResult
classifyFeatures _config ontology paths features = do
  let rules = buildRulesFromOntology ontology ++ defaultRules
      scores = computeConfidence rules features
      aggregated = aggregateScores scores

  -- Determine container type
  let mContainer = determineContainerType aggregated
      getActusType m = m.matchActusType
      containerActusType = fmap getActusType mContainer
      containerFibo = containerActusType >>= inferFIBOClass

  -- Determine primary type (excluding container if present)
  let nonContainer =
        case mContainer of
          Just c -> filter (\m -> m.matchActusType /= c.matchActusType) aggregated
          Nothing -> aggregated
      mPrimary = selectPrimaryClassification nonContainer
      primaryActus = fmap getActusType mPrimary
      primaryFibo = primaryActus >>= inferFIBOClass

  -- Get contained types
  let contained = detectContainedTypes aggregated

  -- Collect all evidence
  let getEvidence m = m.matchEvidence
      allEvidence = concatMap getEvidence aggregated

  -- Get timestamp
  now <- getCurrentTime
  let timestamp = T.pack $ iso8601Show now

  pure
    ClassificationResult
      { sourceFiles = paths
      , containerType = containerActusType
      , containerFiboClass = containerFibo
      , primaryActusType = primaryActus
      , primaryActusLabel = primaryActus >>= lookupLabel ontology
      , primaryConfidence = maybe 0.0 (\m -> m.matchConfidence) mPrimary
      , primaryFiboClass = primaryFibo
      , containedTypes = contained
      , evidence = allEvidence
      , analyzedAt = timestamp
      }

-- | Look up the label for an ACTUS type.
lookupLabel :: OntologyGraph -> Text -> Maybe Text
lookupLabel ontology code =
  fmap (\t -> t.actusLabel) (lookupACTUSTypeByCode ontology code)
 where
  lookupACTUSTypeByCode graph c =
    case filter (\t -> t.actusCode == c) (allACTUSTypes graph) of
      (t : _) -> Just t
      [] -> Nothing

  allACTUSTypes graph =
    let pairs = Map.toList graph.actusContractTypes
     in map snd pairs

-- | Combine features from multiple files.
combineFeatures :: [L4Features] -> L4Features
combineFeatures [] = emptyFeatures
combineFeatures [f1] = f1
combineFeatures features@(first':_) =
  L4Features
    { domainIndicators = concatMap (\f -> f.domainIndicators) features
    , deonticPatterns = concatMap (\f -> f.deonticPatterns) features
    , stateTransitions = concatMap (\f -> f.stateTransitions) features
    , temporalConstraints = concatMap (\f -> f.temporalConstraints) features
    , partyStructure = combinePartyStructures (map (\f -> f.partyStructure) features)
    , typePatterns = concatMap (\f -> f.typePatterns) features
    , sourceFile = first'.sourceFile
    }

-- | Empty features.
emptyFeatures :: L4Features
emptyFeatures =
  L4Features
    { domainIndicators = []
    , deonticPatterns = []
    , stateTransitions = []
    , temporalConstraints = []
    , partyStructure = Unknown
    , typePatterns = []
    , sourceFile = ""
    }

-- | Combine party structures.
combinePartyStructures :: [PartyStructure] -> PartyStructure
combinePartyStructures structures =
  let known = filter (/= Unknown) structures
   in case known of
        [] -> Unknown
        (s : _) -> s -- Take first known structure

-- | Build a VFS from all L4 files in the same directory as the given file.
buildVFSFromDirectory :: FilePath -> IO VFS
buildVFSFromDirectory path = do
  let dir = takeDirectory path
  files <- listDirectory dir
  let l4Files = filter (\f -> ".l4" `isSuffixOf` f) files
  contents <- forM l4Files $ \f -> do
    let fullPath = dir </> f
        moduleName = T.pack $ takeBaseName f
    content <- TIO.readFile fullPath
    pure (moduleName, content)
  pure $ vfsFromList contents
 where
  isSuffixOf :: String -> String -> Bool
  isSuffixOf suffix str = suffix == reverse (take (length suffix) (reverse str))

-- | Build a VFS from a list of file paths.
buildVFSFromFiles :: [FilePath] -> IO VFS
buildVFSFromFiles paths = do
  contents <- forM paths $ \p -> do
    let moduleName = T.pack $ takeBaseName p
    content <- TIO.readFile p
    pure (moduleName, content)
  pure $ vfsFromList contents
