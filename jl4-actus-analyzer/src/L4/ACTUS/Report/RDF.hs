-- | RDF/Turtle report generation for classification results.
--
-- Generates RDF triples in Turtle format that link the analyzed
-- L4 file to ACTUS and FIBO concepts.
module L4.ACTUS.Report.RDF (
  -- * RDF Generation
  toTurtle,

  -- * Namespaces
  actusNS,
  fiboNS,
  l4NS,
) where

import Data.Text (Text)
import qualified Data.Text as T
import L4.ACTUS.Ontology.Types

-- | ACTUS namespace prefix.
actusNS :: Text
actusNS = "https://spec.edmcouncil.org/fibo/ontology/ACTUS/ACTUSControlledVocabulary/"

-- | FIBO namespace prefix.
fiboNS :: Text
fiboNS = "https://spec.edmcouncil.org/fibo/ontology/"

-- | L4 namespace for analyzed contracts.
l4NS :: Text
l4NS = "https://l4.legalese.com/contracts/"

-- | Generate Turtle RDF representation.
toTurtle :: ClassificationResult -> Text
toTurtle result =
  T.unlines $
    [ "@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> ."
    , "@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> ."
    , "@prefix xsd: <http://www.w3.org/2001/XMLSchema#> ."
    , "@prefix fibo-actus: <" <> actusNS <> "> ."
    , "@prefix fibo: <" <> fiboNS <> "> ."
    , "@prefix l4: <" <> l4NS <> "> ."
    , ""
    , "# L4 Contract Classification"
    , "# Generated at: " <> result.analyzedAt
    , ""
    ]
      ++ contractTriples result
      ++ classificationTriples result
      ++ evidenceTriples result

-- | Generate triples for the contract itself.
contractTriples :: ClassificationResult -> [Text]
contractTriples result =
  let contractUri = contractURI result
      file = case result.sourceFiles of
        (f:_) -> f
        [] -> ""
   in [ contractUri <> " a l4:AnalyzedContract ;"
      , "    l4:sourceFile \"" <> escapeString (T.pack file) <> "\" ;"
      , "    l4:analyzedAt \"" <> result.analyzedAt <> "\"^^xsd:dateTime ."
      , ""
      ]

-- | Generate triples for the classification.
classificationTriples :: ClassificationResult -> [Text]
classificationTriples result =
  let contractUri = contractURI result
   in case result.primaryActusType of
        Just actusType ->
          [ contractUri <> " l4:hasACTUSClassification ["
          , "    a l4:ACTUSClassification ;"
          , "    l4:actusType \"" <> actusType <> "\" ;"
          , "    l4:confidence \"" <> T.pack (show result.primaryConfidence) <> "\"^^xsd:decimal ;"
          ]
            ++ case result.primaryFiboClass of
              Just fiboClass ->
                ["    l4:mappedToFIBO <" <> uriToText fiboClass <> "> ;"]
              Nothing -> []
            ++ [ "] ."
               , ""
               ]
        Nothing ->
          [ "# No primary classification determined"
          , ""
          ]

-- | Generate triples for evidence.
evidenceTriples :: ClassificationResult -> [Text]
evidenceTriples result =
  if null result.evidence
    then []
    else
      [ "# Evidence supporting classification"
      ]
        ++ concatMap evidenceTriple (zip [1 ..] result.evidence)
 where
  evidenceTriple (i, ev) =
    let evUri = contractURI result <> "_evidence_" <> T.pack (show (i :: Int))
     in [ evUri <> " a l4:ClassificationEvidence ;"
        , "    l4:feature \"" <> escapeString ev.evidenceFeature <> "\" ;"
        , "    l4:mappedTo \"" <> escapeString ev.evidenceMappedTo <> "\" ;"
        , "    l4:weight \"" <> T.pack (show ev.evidenceWeight) <> "\"^^xsd:decimal ."
        , ""
        ]

-- | Generate a URI for the contract.
contractURI :: ClassificationResult -> Text
contractURI result =
  let file = case result.sourceFiles of
        (f:_) -> f
        [] -> "unknown"
      safeName = T.replace "/" "_" $ T.replace " " "_" $ T.pack file
   in "<" <> l4NS <> safeName <> ">"

-- | Escape a string for Turtle.
escapeString :: Text -> Text
escapeString =
  T.replace "\"" "\\\"" . T.replace "\\" "\\\\" . T.replace "\n" "\\n"
