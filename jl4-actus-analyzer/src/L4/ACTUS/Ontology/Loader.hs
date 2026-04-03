{-# LANGUAGE StrictData #-}

-- | Load ACTUS/FIBO ontologies from RDF/XML files using rdf4h.
--
-- This module parses RDF/XML files from the FIBO repository and extracts
-- ACTUS contract types and FIBO class definitions using the rdf4h library
-- for proper RDF semantics.
module L4.ACTUS.Ontology.Loader (
  -- * Loading Ontologies
  loadOntology,
  loadOntologyFromDir,
  loadACTUSControlledVocabulary,

  -- * Default Paths
  defaultFiboPath,
  actusControlledVocabularyPath,

  -- * Errors
  OntologyLoadError (..),
) where

import Control.Exception (Exception, try)
import Control.Monad (foldM, forM)
import Data.List (isSuffixOf)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.RDF (RDF, TList)
import qualified Data.RDF as RDF
import Data.Text (Text)
import qualified Data.Text as T
import L4.ACTUS.Ontology.Types
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath ((</>))

-- | Errors that can occur when loading ontologies.
data OntologyLoadError
  = FileNotFound FilePath
  | ParseError FilePath String
  | DirectoryNotFound FilePath
  deriving stock (Eq, Show)

instance Exception OntologyLoadError

-- | Default path to the FIBO repository.
defaultFiboPath :: FilePath
defaultFiboPath = "/Users/mengwong/src/edmcouncil/fibo"

-- | Path to the ACTUS Controlled Vocabulary relative to FIBO root.
actusControlledVocabularyPath :: FilePath
actusControlledVocabularyPath = "ACTUS/ACTUSControlledVocabulary.rdf"

-- | Load the ACTUS Controlled Vocabulary from the default or specified path.
loadACTUSControlledVocabulary :: Maybe FilePath -> IO (Either OntologyLoadError OntologyGraph)
loadACTUSControlledVocabulary mFiboPath = do
  let fiboPath = fromMaybe defaultFiboPath mFiboPath
      acvPath = fiboPath </> actusControlledVocabularyPath
  loadOntology acvPath

-- | Load a single RDF file into an ontology graph.
loadOntology :: FilePath -> IO (Either OntologyLoadError OntologyGraph)
loadOntology path = do
  exists <- doesFileExist path
  if not exists
    then pure $ Left (FileNotFound path)
    else do
      result <- try $ RDF.parseFile (RDF.XmlParser Nothing Nothing) path
      case result of
        Left (e :: IOError) -> pure $ Left (ParseError path (show e))
        Right (Left err) -> pure $ Left (ParseError path (show err))
        Right (Right rdfGraph) -> pure $ Right $ parseRDFGraph path rdfGraph

-- | Load multiple RDF files from a directory.
loadOntologyFromDir :: FilePath -> IO (Either OntologyLoadError OntologyGraph)
loadOntologyFromDir dirPath = do
  exists <- doesDirectoryExist dirPath
  if not exists
    then pure $ Left (DirectoryNotFound dirPath)
    else do
      files <- listDirectory dirPath
      let rdfFiles = filter (".rdf" `isSuffixOf`) files
      graphs <- forM rdfFiles $ \f -> loadOntology (dirPath </> f)
      pure $ foldM mergeGraphs emptyOntologyGraph (mapMaybe eitherToMaybe graphs)
 where
  eitherToMaybe (Right x) = Just x
  eitherToMaybe (Left _) = Nothing

-- | Merge two ontology graphs.
mergeGraphs :: OntologyGraph -> OntologyGraph -> Either OntologyLoadError OntologyGraph
mergeGraphs g1 g2 =
  Right
    OntologyGraph
      { actusContractTypes = Map.union g1.actusContractTypes g2.actusContractTypes
      , actusCashFlowFamilies = Map.union g1.actusCashFlowFamilies g2.actusCashFlowFamilies
      , actusCashFlowCategories = Map.union g1.actusCashFlowCategories g2.actusCashFlowCategories
      , fiboClasses = Map.union g1.fiboClasses g2.fiboClasses
      , fiboProperties = Map.union g1.fiboProperties g2.fiboProperties
      , loadedFiles = g1.loadedFiles ++ g2.loadedFiles
      }

-- | Parse an RDF graph into ontology structures using rdf4h.
parseRDFGraph :: FilePath -> RDF TList -> OntologyGraph
parseRDFGraph path rdfGraph =
  let -- Find all subjects that are ACTUSContractType individuals
      allTriples = RDF.triplesOf rdfGraph

      -- Extract contract types, families, and categories
      contractTypes = extractContractTypes allTriples
      families = extractCashFlowFamilies allTriples
      categories = extractCashFlowCategories allTriples
   in OntologyGraph
        { actusContractTypes = Map.fromList [(t.actusCode, t) | t <- contractTypes]
        , actusCashFlowFamilies = Map.fromList [(f.familyCode, f) | f <- families]
        , actusCashFlowCategories = Map.fromList [(c.categoryCode, c) | c <- categories]
        , fiboClasses = Map.empty -- Loaded from FIBO files if needed
        , fiboProperties = Map.empty
        , loadedFiles = [path]
        }

-- | Extract ACTUS contract types from RDF triples.
extractContractTypes :: [RDF.Triple] -> [ACTUSContractType]
extractContractTypes triples =
  let -- Find subjects that represent ACTUSContractType individuals
      contractTypeSubjects = findContractTypeSubjects triples
   in mapMaybe (buildContractType triples) contractTypeSubjects

-- | Find all subjects that are ACTUS contract types.
findContractTypeSubjects :: [RDF.Triple] -> [RDF.Node]
findContractTypeSubjects triples =
  [ RDF.subjectOf t
  | t <- triples
  , isContractTypeTriple t
  ]
 where
  isContractTypeTriple t =
    case RDF.objectOf t of
      RDF.UNode uri -> "ACTUSContractType" `T.isInfixOf` uri
      _ -> False

-- | Build an ACTUSContractType from triples about a given subject.
buildContractType :: [RDF.Triple] -> RDF.Node -> Maybe ACTUSContractType
buildContractType triples subj = do
  let subjTriples = filter (\t -> RDF.subjectOf t == subj) triples
      code = getTextValue "hasTextValue" subjTriples
      label = getTextValue "label" subjTriples
      definition = getTextValue "definition" subjTriples
      coverage = getTextValue "hasCoverageDescription" subjTriples
      family = getResourceLocalName "isNarrowerThan" "ACTUSCashFlowFamily" subjTriples
      category = getResourceLocalName "isNarrowerThan" "ACTUSCashflowCategory" subjTriples
      refersTo = getResources "refersTo" subjTriples
      uri = subjectToURI subj

  -- Only return if we have a code
  case code of
    Just c | not (T.null c) ->
      Just
        ACTUSContractType
          { actusCode = c
          , actusLabel = cleanLabel (fromMaybe "" label)
          , actusDefinition = fromMaybe "" definition
          , actusCoverageDescription = coverage
          , actusCashFlowFamily = family
          , actusCashFlowCategory = category
          , actusRefersTo = map mkURI refersTo
          , actusUri = uri
          }
    _ -> Nothing

-- | Extract ACTUS cash flow families from RDF triples.
extractCashFlowFamilies :: [RDF.Triple] -> [ACTUSCashFlowFamily]
extractCashFlowFamilies triples =
  let familySubjects = findSubjectsByUriPattern "ACTUSCashFlowFamily-" triples
   in mapMaybe (buildCashFlowFamily triples) familySubjects

-- | Build an ACTUSCashFlowFamily from triples.
buildCashFlowFamily :: [RDF.Triple] -> RDF.Node -> Maybe ACTUSCashFlowFamily
buildCashFlowFamily triples subj = do
  let subjTriples = filter (\t -> RDF.subjectOf t == subj) triples
      code = getTextValue "hasTextValue" subjTriples
      label = getTextValue "label" subjTriples
      definition = getTextValue "definition" subjTriples
      uri = subjectToURI subj

  case code of
    Just c | not (T.null c) ->
      Just
        ACTUSCashFlowFamily
          { familyCode = c
          , familyLabel = cleanLabel (fromMaybe "" label)
          , familyDefinition = fromMaybe "" definition
          , familyUri = uri
          }
    _ -> Nothing

-- | Extract ACTUS cash flow categories from RDF triples.
extractCashFlowCategories :: [RDF.Triple] -> [ACTUSCashFlowCategory]
extractCashFlowCategories triples =
  let categorySubjects = findSubjectsByUriPattern "ACTUSCashflowCategory-" triples
   in mapMaybe (buildCashFlowCategory triples) categorySubjects

-- | Build an ACTUSCashFlowCategory from triples.
buildCashFlowCategory :: [RDF.Triple] -> RDF.Node -> Maybe ACTUSCashFlowCategory
buildCashFlowCategory triples subj = do
  let subjTriples = filter (\t -> RDF.subjectOf t == subj) triples
      code = getTextValue "hasTextValue" subjTriples
      label = getTextValue "label" subjTriples
      definition = getTextValue "definition" subjTriples
      uri = subjectToURI subj

  case code of
    Just c | not (T.null c) ->
      Just
        ACTUSCashFlowCategory
          { categoryCode = c
          , categoryLabel = cleanLabel (fromMaybe "" label)
          , categoryDefinition = fromMaybe "" definition
          , categoryUri = uri
          }
    _ -> Nothing

-- | Find subjects by URI pattern.
findSubjectsByUriPattern :: Text -> [RDF.Triple] -> [RDF.Node]
findSubjectsByUriPattern pattern triples =
  [ subj
  | t <- triples
  , let subj = RDF.subjectOf t
  , case subj of
      RDF.UNode uri -> pattern `T.isInfixOf` uri
      _ -> False
  ]

-- | Get a text value from triples by predicate local name.
getTextValue :: Text -> [RDF.Triple] -> Maybe Text
getTextValue predLocalName triples =
  case filter (hasPredLocalName predLocalName) triples of
    (t : _) -> extractLiteralValue (RDF.objectOf t)
    [] -> Nothing

-- | Get a resource URI's local name from triples, filtering by object pattern.
getResourceLocalName :: Text -> Text -> [RDF.Triple] -> Maybe Text
getResourceLocalName predLocalName objPattern triples =
  case filter (\t -> hasPredLocalName predLocalName t && hasObjectPattern objPattern t) triples of
    (t : _) -> extractLocalName (RDF.objectOf t)
    [] -> Nothing

-- | Get all resource URIs for a predicate.
getResources :: Text -> [RDF.Triple] -> [Text]
getResources predLocalName triples =
  [ uri
  | t <- triples
  , hasPredLocalName predLocalName t
  , RDF.UNode uri <- [RDF.objectOf t]
  ]

-- | Check if a triple's predicate ends with the given local name.
hasPredLocalName :: Text -> RDF.Triple -> Bool
hasPredLocalName localName t =
  case RDF.predicateOf t of
    RDF.UNode uri ->
      localName `T.isSuffixOf` uri
        || ("/" <> localName) `T.isSuffixOf` uri
        || ("#" <> localName) `T.isSuffixOf` uri
    _ -> False

-- | Check if a triple's object contains the given pattern.
hasObjectPattern :: Text -> RDF.Triple -> Bool
hasObjectPattern pattern t =
  case RDF.objectOf t of
    RDF.UNode uri -> pattern `T.isInfixOf` uri
    _ -> False

-- | Extract literal value from an RDF node.
extractLiteralValue :: RDF.Node -> Maybe Text
extractLiteralValue (RDF.LNode (RDF.PlainL txt)) = Just txt
extractLiteralValue (RDF.LNode (RDF.PlainLL txt _)) = Just txt
extractLiteralValue (RDF.LNode (RDF.TypedL txt _)) = Just txt
extractLiteralValue _ = Nothing

-- | Extract local name from a URI node.
extractLocalName :: RDF.Node -> Maybe Text
extractLocalName (RDF.UNode uri) =
  let afterSlash = case T.breakOnEnd "/" uri of
        (_, local) | not (T.null local) -> local
        _ -> uri
      afterHash = case T.breakOnEnd "#" afterSlash of
        (_, local) | not (T.null local) -> local
        _ -> afterSlash
   in Just afterHash
extractLocalName _ = Nothing

-- | Convert a subject node to a URI.
subjectToURI :: RDF.Node -> URI
subjectToURI (RDF.UNode uri) = mkURI uri
subjectToURI (RDF.BNode nodeId) = mkURI ("_:" <> nodeId)
subjectToURI (RDF.BNodeGen n) = mkURI ("_:b" <> T.pack (show n))
subjectToURI (RDF.LNode _) = mkURI ""

-- | Clean up a label (remove "ACTUS contract type - " prefix).
cleanLabel :: Text -> Text
cleanLabel label =
  let prefixes = ["ACTUS contract type - ", "ACTUS cashflow family - ", "ACTUS cashflow category - "]
   in foldr removePrefix label prefixes
 where
  removePrefix prefix t =
    if prefix `T.isPrefixOf` t
      then T.drop (T.length prefix) t
      else t
