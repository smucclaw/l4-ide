-- | Query operations on the loaded ontology graph.
--
-- This module provides functions to look up ACTUS types and FIBO classes
-- from the ontology graph, and to find related concepts.
module L4.ACTUS.Ontology.Query (
  -- * ACTUS Queries
  lookupACTUSType,
  lookupACTUSTypeByCode,
  allACTUSTypes,
  getACTUSTypesByFamily,
  getACTUSTypesByCategory,

  -- * FIBO Queries
  lookupFIBOClass,

  -- * Text Matching
  findACTUSTypesByKeyword,
  findACTUSTypesByCoverage,
) where

import Data.List (sortBy)
import qualified Data.Map.Strict as Map
import Data.Ord (comparing)
import Data.Text (Text)
import qualified Data.Text as T
import L4.ACTUS.Ontology.Types

-- | Look up an ACTUS contract type by its short code.
lookupACTUSTypeByCode :: OntologyGraph -> Text -> Maybe ACTUSContractType
lookupACTUSTypeByCode graph code =
  Map.lookup code graph.actusContractTypes

-- | Look up an ACTUS contract type by URI.
lookupACTUSType :: OntologyGraph -> URI -> Maybe ACTUSContractType
lookupACTUSType graph uri =
  let matching =
        [ ct
        | ct <- Map.elems graph.actusContractTypes
        , ct.actusUri == uri
        ]
   in case matching of
        (ct : _) -> Just ct
        [] -> Nothing

-- | Get all ACTUS contract types.
allACTUSTypes :: OntologyGraph -> [ACTUSContractType]
allACTUSTypes graph = Map.elems graph.actusContractTypes

-- | Get ACTUS types belonging to a specific cash flow family.
getACTUSTypesByFamily :: OntologyGraph -> Text -> [ACTUSContractType]
getACTUSTypesByFamily graph familyCode =
  [ ct
  | ct <- Map.elems graph.actusContractTypes
  , ct.actusCashFlowFamily == Just familyCode
  ]

-- | Get ACTUS types belonging to a specific cash flow category.
getACTUSTypesByCategory :: OntologyGraph -> Text -> [ACTUSContractType]
getACTUSTypesByCategory graph categoryCode =
  [ ct
  | ct <- Map.elems graph.actusContractTypes
  , ct.actusCashFlowCategory == Just categoryCode
  ]

-- | Look up a FIBO class by URI.
lookupFIBOClass :: OntologyGraph -> URI -> Maybe FIBOClass
lookupFIBOClass graph uri = Map.lookup uri graph.fiboClasses

-- | Find ACTUS types whose label or definition contains a keyword.
findACTUSTypesByKeyword :: OntologyGraph -> Text -> [ACTUSContractType]
findACTUSTypesByKeyword graph keyword =
  let lowerKw = T.toLower keyword
      matches ct =
        lowerKw `T.isInfixOf` T.toLower ct.actusLabel
          || lowerKw `T.isInfixOf` T.toLower ct.actusDefinition
   in filter matches (Map.elems graph.actusContractTypes)

-- | Find ACTUS types whose coverage description mentions specific terms.
--
-- Returns types sorted by relevance (number of matching terms).
findACTUSTypesByCoverage :: OntologyGraph -> [Text] -> [(ACTUSContractType, Int)]
findACTUSTypesByCoverage graph keywords =
  let countMatches ct =
        case ct.actusCoverageDescription of
          Nothing -> 0
          Just desc ->
            let lowerDesc = T.toLower desc
             in length [k | k <- keywords, T.toLower k `T.isInfixOf` lowerDesc]
      scored =
        [ (ct, score)
        | ct <- Map.elems graph.actusContractTypes
        , let score = countMatches ct
        , score > 0
        ]
   in sortBy (comparing (negate . snd)) scored
