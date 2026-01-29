{-# LANGUAGE StrictData #-}

-- | Core types for representing ACTUS/FIBO ontology concepts.
--
-- This module defines the Haskell representation of ACTUS contract types
-- and FIBO classes as parsed from RDF ontology files.
module L4.ACTUS.Ontology.Types (
  -- * Ontology Graph
  OntologyGraph (..),
  emptyOntologyGraph,

  -- * ACTUS Types
  ACTUSContractType (..),
  ACTUSCashFlowFamily (..),
  ACTUSCashFlowCategory (..),

  -- * FIBO Types
  FIBOClass (..),
  FIBOProperty (..),

  -- * Classification Results
  ClassificationResult (..),
  ContainedType (..),
  Evidence (..),
  SourceLocation (..),

  -- * URIs and Identifiers
  URI,
  mkURI,
  uriToText,
) where

import Data.Binary (Binary)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import GHC.Generics (Generic)

-- | A URI identifier for ontology resources.
newtype URI = MkURI Text
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (Binary)

mkURI :: Text -> URI
mkURI = MkURI

uriToText :: URI -> Text
uriToText (MkURI t) = t

-- | The loaded ontology graph containing ACTUS and FIBO definitions.
data OntologyGraph = OntologyGraph
  { actusContractTypes :: Map Text ACTUSContractType
  -- ^ Contract types indexed by their short code (e.g., "FXOUT")
  , actusCashFlowFamilies :: Map Text ACTUSCashFlowFamily
  -- ^ Cash flow families (Basic, Combined, Credit Enhancement)
  , actusCashFlowCategories :: Map Text ACTUSCashFlowCategory
  -- ^ Cash flow categories within families
  , fiboClasses :: Map URI FIBOClass
  -- ^ FIBO classes indexed by URI
  , fiboProperties :: Map URI FIBOProperty
  -- ^ FIBO properties indexed by URI
  , loadedFiles :: [FilePath]
  -- ^ List of RDF files that were loaded
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Binary)

emptyOntologyGraph :: OntologyGraph
emptyOntologyGraph =
  OntologyGraph
    { actusContractTypes = Map.empty
    , actusCashFlowFamilies = Map.empty
    , actusCashFlowCategories = Map.empty
    , fiboClasses = Map.empty
    , fiboProperties = Map.empty
    , loadedFiles = []
    }

-- | An ACTUS contract type from the controlled vocabulary.
--
-- Example: FXOUT (Foreign Exchange Outright), SWAPS (Plain Vanilla Swap)
data ACTUSContractType = ACTUSContractType
  { actusCode :: Text
  -- ^ The short code (e.g., "FXOUT", "SWAPS")
  , actusLabel :: Text
  -- ^ Human-readable label
  , actusDefinition :: Text
  -- ^ SKOS definition describing the contract type
  , actusCoverageDescription :: Maybe Text
  -- ^ Description of what contracts this type covers
  , actusCashFlowFamily :: Maybe Text
  -- ^ Parent cash flow family (Basic, Combined, Credit Enhancement)
  , actusCashFlowCategory :: Maybe Text
  -- ^ Cash flow category within the family
  , actusRefersTo :: [URI]
  -- ^ FIBO classes this type maps to
  , actusUri :: URI
  -- ^ The full URI of this contract type
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Binary)

-- | ACTUS Cash Flow Family - top-level classification.
data ACTUSCashFlowFamily = ACTUSCashFlowFamily
  { familyCode :: Text
  , familyLabel :: Text
  , familyDefinition :: Text
  , familyUri :: URI
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Binary)

-- | ACTUS Cash Flow Category - classification within a family.
data ACTUSCashFlowCategory = ACTUSCashFlowCategory
  { categoryCode :: Text
  , categoryLabel :: Text
  , categoryDefinition :: Text
  , categoryUri :: URI
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Binary)

-- | A FIBO ontology class.
data FIBOClass = FIBOClass
  { fiboClassUri :: URI
  , fiboClassLabel :: Text
  , fiboClassDefinition :: Maybe Text
  , fiboClassSuperClasses :: [URI]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Binary)

-- | A FIBO ontology property.
data FIBOProperty = FIBOProperty
  { fiboPropertyUri :: URI
  , fiboPropertyLabel :: Text
  , fiboPropertyDomain :: Maybe URI
  , fiboPropertyRange :: Maybe URI
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Binary)

-- | Source location within an L4 file.
data SourceLocation = SourceLocation
  { sourceFile :: FilePath
  , startLine :: Int
  , endLine :: Int
  }
  deriving stock (Eq, Show, Generic)

-- | Evidence supporting a classification decision.
data Evidence = Evidence
  { evidenceFeature :: Text
  -- ^ The detected feature (e.g., "Currency enum type")
  , evidenceLocation :: Maybe SourceLocation
  -- ^ Where in the L4 source this was found
  , evidenceMappedTo :: Text
  -- ^ What concept this maps to (e.g., "CurrencyIndicator → FXOUT")
  , evidenceWeight :: Double
  -- ^ How much this evidence contributes to the score
  }
  deriving stock (Eq, Show, Generic)

-- | A contained contract type within a master agreement.
data ContainedType = ContainedType
  { containedActusType :: Text
  -- ^ ACTUS type code
  , containedConfidence :: Double
  -- ^ Confidence score (0.0 - 1.0)
  , containedEvidence :: [Evidence]
  -- ^ Supporting evidence
  }
  deriving stock (Eq, Show, Generic)

-- | The complete classification result for an L4 file or module.
data ClassificationResult = ClassificationResult
  { sourceFiles :: [FilePath]
  -- ^ L4 files that were analyzed
  , containerType :: Maybe Text
  -- ^ Container type (e.g., "MasterAgreement") if applicable
  , containerFiboClass :: Maybe URI
  -- ^ FIBO class for the container
  , primaryActusType :: Maybe Text
  -- ^ Primary ACTUS type code
  , primaryActusLabel :: Maybe Text
  -- ^ Human-readable primary type
  , primaryConfidence :: Double
  -- ^ Confidence score for primary classification
  , primaryFiboClass :: Maybe URI
  -- ^ FIBO class for primary type
  , containedTypes :: [ContainedType]
  -- ^ Additional contract types within a master agreement
  , evidence :: [Evidence]
  -- ^ All evidence supporting the classification
  , analyzedAt :: Text
  -- ^ ISO 8601 timestamp of analysis
  }
  deriving stock (Eq, Show, Generic)
