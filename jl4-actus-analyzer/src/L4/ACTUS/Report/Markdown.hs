-- | Markdown report generation for classification results.
--
-- Generates human-readable reports in Markdown format,
-- suitable for documentation or review.
module L4.ACTUS.Report.Markdown (
  -- * Markdown Generation
  toMarkdown,
  toMarkdownBrief,
) where

import Data.Text (Text)
import qualified Data.Text as T
import L4.ACTUS.Ontology.Types
import Text.Printf (printf)

-- | Generate a full Markdown report.
toMarkdown :: ClassificationResult -> Text
toMarkdown result =
  T.unlines $
    [ "# ACTUS Classification Report"
    , ""
    , "## Source"
    , ""
    , "**Files analyzed:** " <> T.intercalate ", " (map (T.pack . show) result.sourceFiles)
    , ""
    , "**Analysis timestamp:** " <> result.analyzedAt
    , ""
    ]
      ++ containerSection result
      ++ primarySection result
      ++ containedSection result
      ++ evidenceSection result

-- | Generate a brief one-line summary.
toMarkdownBrief :: ClassificationResult -> Text
toMarkdownBrief result =
  case result.primaryActusType of
    Just actusType ->
      let label = maybe actusType id result.primaryActusLabel
          conf = formatPercent result.primaryConfidence
       in actusType <> " (" <> label <> ") - " <> conf <> " confidence"
    Nothing ->
      "No classification determined"

-- Container section
containerSection :: ClassificationResult -> [Text]
containerSection result =
  case result.containerType of
    Just ct ->
      [ "## Container Type"
      , ""
      , "This appears to be a **" <> ct <> "** that governs multiple transaction types."
      , ""
      ]
        ++ case result.containerFiboClass of
          Just fiboClass ->
            ["**FIBO Class:** `" <> uriToText fiboClass <> "`", ""]
          Nothing -> []
    Nothing -> []

-- Primary classification section
primarySection :: ClassificationResult -> [Text]
primarySection result =
  [ "## Primary Classification"
  , ""
  ]
    ++ case result.primaryActusType of
      Just actusType ->
        let label = maybe actusType id result.primaryActusLabel
         in [ "| Property | Value |"
            , "|----------|-------|"
            , "| ACTUS Type | **" <> actusType <> "** |"
            , "| Label | " <> label <> " |"
            , "| Confidence | " <> formatPercent result.primaryConfidence <> " |"
            ]
              ++ case result.primaryFiboClass of
                Just fiboClass ->
                  ["| FIBO Class | `" <> uriToText fiboClass <> "` |"]
                Nothing -> []
              ++ [""]
      Nothing ->
        [ "No primary classification could be determined from the analyzed L4 source."
        , ""
        , "This may indicate:"
        , "- The contract type is not in the ACTUS taxonomy"
        , "- The L4 encoding doesn't contain sufficient type information"
        , "- A custom or hybrid contract structure"
        , ""
        ]

-- Contained types section
containedSection :: ClassificationResult -> [Text]
containedSection result
  | null result.containedTypes = []
  | otherwise =
      [ "## Contained Transaction Types"
      , ""
      , "The following transaction types were detected within this master agreement:"
      , ""
      , "| ACTUS Type | Confidence |"
      , "|------------|------------|"
      ]
        ++ map formatContained result.containedTypes
        ++ [""]
 where
  formatContained ct =
    "| " <> ct.containedActusType <> " | " <> formatPercent ct.containedConfidence <> " |"

-- Evidence section
evidenceSection :: ClassificationResult -> [Text]
evidenceSection result
  | null result.evidence = []
  | otherwise =
      [ "## Supporting Evidence"
      , ""
      , "The following features were detected in the L4 source:"
      , ""
      , "| Feature | Mapped To | Weight |"
      , "|---------|-----------|--------|"
      ]
        ++ map formatEvidence result.evidence
        ++ [""]
 where
  formatEvidence ev =
    "| "
      <> ev.evidenceFeature
      <> " | "
      <> ev.evidenceMappedTo
      <> " | "
      <> formatPercent ev.evidenceWeight
      <> " |"

-- | Format a number as percentage.
formatPercent :: Double -> Text
formatPercent d = T.pack $ printf "%.1f%%" (d * 100)
