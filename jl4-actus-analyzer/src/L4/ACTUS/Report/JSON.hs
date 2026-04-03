{-# LANGUAGE StrictData #-}

-- | JSON report generation for classification results.
module L4.ACTUS.Report.JSON (
  -- * JSON Generation
  resultToJSON,
  toJSONText,
  toJSONPretty,

  -- * JSON Types
  ClassificationJSON (..),
  ContainerJSON (..),
  ContainedTypeJSON (..),
  EvidenceJSON (..),
) where

import Data.Aeson (ToJSON (..), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Aeson
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import GHC.Generics (Generic)
import L4.ACTUS.Ontology.Types

-- | JSON representation of classification result.
data ClassificationJSON = ClassificationJSON
  { sourceFile :: Text
  , container :: Maybe ContainerJSON
  , primaryClassification :: Maybe PrimaryClassificationJSON
  , containedTypes :: [ContainedTypeJSON]
  , evidence :: [EvidenceJSON]
  , analyzedAt :: Text
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON ClassificationJSON where
  toJSON c =
    Aeson.object
      [ "sourceFile" .= c.sourceFile
      , "container" .= c.container
      , "primaryClassification" .= c.primaryClassification
      , "containedTypes" .= c.containedTypes
      , "evidence" .= c.evidence
      , "analyzedAt" .= c.analyzedAt
      ]

-- | Container (master agreement) classification.
data ContainerJSON = ContainerJSON
  { containerType :: Text
  , fiboClass :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON ContainerJSON where
  toJSON c =
    Aeson.object
      [ "type" .= c.containerType
      , "fiboClass" .= c.fiboClass
      ]

-- | Primary classification.
data PrimaryClassificationJSON = PrimaryClassificationJSON
  { actusType :: Text
  , actusLabel :: Text
  , confidence :: Double
  , fiboClass :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON PrimaryClassificationJSON where
  toJSON p =
    Aeson.object
      [ "actusType" .= p.actusType
      , "actusLabel" .= p.actusLabel
      , "confidence" .= p.confidence
      , "fiboClass" .= p.fiboClass
      ]

-- | Contained type classification.
data ContainedTypeJSON = ContainedTypeJSON
  { actusType :: Text
  , confidence :: Double
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON ContainedTypeJSON where
  toJSON c =
    Aeson.object
      [ "actusType" .= c.actusType
      , "confidence" .= c.confidence
      ]

-- | Evidence item.
data EvidenceJSON = EvidenceJSON
  { feature :: Text
  , sourceLocation :: Maybe Text
  , mappedTo :: Text
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON EvidenceJSON where
  toJSON e =
    Aeson.object
      [ "feature" .= e.feature
      , "sourceLocation" .= e.sourceLocation
      , "mappedTo" .= e.mappedTo
      ]

-- | Convert classification result to JSON value.
resultToJSON :: ClassificationResult -> ClassificationJSON
resultToJSON result =
  let file = case result.sourceFiles of
        (f:_) -> f
        [] -> ""
   in ClassificationJSON
    { sourceFile = T.pack file
    , container = mkContainer result
    , primaryClassification = mkPrimary result
    , containedTypes = map mkContained result.containedTypes
    , evidence = map mkEvidence result.evidence
    , analyzedAt = result.analyzedAt
    }
 where
  mkContainer r = case r.containerType of
    Just ct ->
      Just
        ContainerJSON
          { containerType = ct
          , fiboClass = fmap uriToText r.containerFiboClass
          }
    Nothing -> Nothing

  mkPrimary r = case r.primaryActusType of
    Just at ->
      Just
        PrimaryClassificationJSON
          { actusType = at
          , actusLabel = maybe at id r.primaryActusLabel
          , confidence = r.primaryConfidence
          , fiboClass = fmap uriToText r.primaryFiboClass
          }
    Nothing -> Nothing

  mkContained ct =
    ContainedTypeJSON
      { actusType = ct.containedActusType
      , confidence = ct.containedConfidence
      }

  mkEvidence e =
    EvidenceJSON
      { feature = e.evidenceFeature
      , sourceLocation = fmap formatLocation e.evidenceLocation
      , mappedTo = e.evidenceMappedTo
      }

  formatLocation loc =
    T.pack loc.sourceFile <> ":" <> T.pack (show loc.startLine) <> "-" <> T.pack (show loc.endLine)

-- | Convert to JSON text (compact).
toJSONText :: ClassificationResult -> Text
toJSONText =
  TL.toStrict . TLE.decodeUtf8 . Aeson.encode . resultToJSON

-- | Convert to JSON text (pretty-printed).
toJSONPretty :: ClassificationResult -> Text
toJSONPretty =
  TL.toStrict . TLE.decodeUtf8 . Aeson.encodePretty . resultToJSON
