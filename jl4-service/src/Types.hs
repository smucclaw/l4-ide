{-# LANGUAGE DataKinds #-}

module Types (
  DeploymentId (..),
  DeploymentState (..),
  DeploymentMetadata (..),
  FunctionSummary (..),
  ValidatedFunction (..),
  Function (..),
  SimpleFunction (..),
  SimpleResponse (..),
  -- * Batch types
  BatchRequest (..),
  BatchResponse (..),
  InputCase (..),
  OutputCase (..),
  OutputSummary (..),
  Outcomes (..),
  OutcomeObject (..),
  OutcomeStyle (..),
  -- * Environment
  AppEnv (..),
  AppM,
) where

import Backend.Api (EvalBackend, FnLiteral, RunFunction, EvaluatorError, ResponseWithReason, GraphVizResponse)
import Backend.DecisionQueryPlan (CachedDecisionQuery)
import Backend.FunctionSchema (Parameters)
import Backend.Jl4 (CompiledModule)
import BundleStore (BundleStore)
import Control.Concurrent.STM (TVar)
import Control.Monad.Trans.Reader (ReaderT)
import Data.Aeson as Aeson
import Data.Aeson.Types (Parser)
import qualified Data.Aeson.Key as Aeson.Key
import qualified Data.Aeson.KeyMap as Aeson.KeyMap
import Data.Fixed (Centi, Fixed, E2)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Scientific as Scientific
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Servant (FromHttpApiData, Handler, ToHttpApiData)

-- | Unique identifier for a deployment.
newtype DeploymentId = DeploymentId { unDeploymentId :: Text }
  deriving newtype (Eq, Ord, Show, FromJSON, ToJSON, FromHttpApiData, ToHttpApiData)

-- | Runtime state of a deployment.
data DeploymentState
  = DeploymentPending
  -- ^ Bundle is being compiled in a background thread.
  | DeploymentReady !(Map Text ValidatedFunction) !DeploymentMetadata
  -- ^ Compiled and ready to serve evaluation requests.
  | DeploymentFailed !Text
  -- ^ Compilation failed; the error message is stored.

-- | Metadata persisted alongside a deployment bundle.
data DeploymentMetadata = DeploymentMetadata
  { metaFunctions :: ![FunctionSummary]
  , metaVersion   :: !Text
  -- ^ SHA-256 hex digest of all source contents (sorted by path).
  , metaCreatedAt :: !UTCTime
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Summary of a single exported function within a deployment.
data FunctionSummary = FunctionSummary
  { fsName        :: !Text
  , fsDescription :: !Text
  , fsParameters  :: !Parameters
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | A compiled and ready-to-evaluate function.
data ValidatedFunction = ValidatedFunction
  { fnImpl                :: !Function
  , fnEvaluator           :: !(Map EvalBackend RunFunction)
  , fnCompiled            :: !(Maybe CompiledModule)
  , fnSources             :: !(Map EvalBackend Text)
  , fnDecisionQueryCache  :: !(Maybe CachedDecisionQuery)
  }

-- | Lightweight function listing (name + description only).
data SimpleFunction = SimpleFunction
  { simpleName :: !Text
  , simpleDescription :: !Text
  }
  deriving stock (Show, Read, Ord, Eq, Generic)

instance ToJSON SimpleFunction where
  toJSON (SimpleFunction n desc) =
    Aeson.object
      [ "type" .= Aeson.String "function"
      , "function"
          .= Aeson.object
            [ "name" .= Aeson.String n
            , "description" .= Aeson.String desc
            ]
      ]

instance FromJSON SimpleFunction where
  parseJSON = Aeson.withObject "Function" $ \o -> do
    "function" :: Text <- o .: "type"
    props <- o .: "function"
    Aeson.withObject
      "function body"
      ( \p -> do
          SimpleFunction
            <$> p .: "name"
            <*> p .: "description"
      )
      props

-- | Full function description including parameter schema.
data Function = Function
  { name                  :: !Text
  , description           :: !Text
  , parameters            :: !Parameters
  , supportedEvalBackend  :: [EvalBackend]
  }
  deriving stock (Show, Read, Ord, Eq, Generic)

instance ToJSON Function where
  toJSON (Function n desc params backends) =
    Aeson.object
      [ "type" .= Aeson.String "function"
      , "function"
          .= Aeson.object
            [ "name" .= Aeson.String n
            , "description" .= Aeson.String desc
            , "parameters" .= params
            , "supportedBackends" .= backends
            ]
      ]

instance FromJSON Function where
  parseJSON = Aeson.withObject "Function" $ \o -> do
    fnType <- o .: "type"
    case fnType :: Text of
      "function" -> pure ()
      e -> fail $ "Expected \"function\" but got " <> Text.unpack e
    props <- o .: "function"
    Aeson.withObject
      "function body"
      ( \p -> do
          Function
            <$> p .: "name"
            <*> p .: "description"
            <*> p .: "parameters"
            <*> p .: "supportedBackends"
      )
      props

-- | Evaluation response: either a result with reasoning, or an error.
data SimpleResponse
  = SimpleResponse !ResponseWithReason
  | SimpleError !EvaluatorError
  deriving stock (Show, Read, Ord, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- ----------------------------------------------------------------------------
-- Batch types
-- ----------------------------------------------------------------------------

type Id = Int

data BatchRequest = BatchRequest
  { outcomes :: [Outcomes]
  , cases :: [InputCase]
  }
  deriving stock (Show, Eq, Ord)

data Outcomes
  = OutcomeAttribute Text
  | OutcomePropertyObject OutcomeObject
  deriving stock (Show, Eq, Ord)

data OutcomeObject = OutcomeObject
  { id :: Text
  , showSilent :: Maybe Bool
  , showInvisible :: Maybe Bool
  , resolveIndecisionRelationships :: Maybe Bool
  , knownOutcomeStyle :: Maybe OutcomeStyle
  , unknownOutcomeStyle :: Maybe OutcomeStyle
  }
  deriving stock (Show, Eq, Ord)

data OutcomeStyle
  = ValueOnly
  | DecisionReport
  | BaseAttributes
  deriving stock (Show, Ord, Eq, Enum, Bounded)

data InputCase = InputCase
  { id :: Id
  , attributes :: Map Text FnLiteral
  }
  deriving stock (Show, Eq, Ord)

data OutputCase = OutputCase
  { id :: Id
  , attributes :: Map Text FnLiteral
  , graphviz :: Maybe GraphVizResponse
  }
  deriving stock (Show, Eq, Ord)

data BatchResponse = BatchResponse
  { cases :: [OutputCase]
  , summary :: OutputSummary
  }
  deriving stock (Show, Eq, Ord)

data OutputSummary = OutputSummary
  { casesRead :: Int
  , casesProcessed :: Int
  , casesIgnored :: Int
  , processorDurationSec :: Fixed E2
  , casesPerSec :: Fixed E2
  , processorQueuedSec :: Fixed E2
  }
  deriving stock (Show, Eq, Ord)

-- ----------------------------------------------------------------------------
-- Batch JSON instances
-- ----------------------------------------------------------------------------

instance FromJSON OutcomeStyle where
  parseJSON = Aeson.withText "OutcomeStyle" $ \case
    "value-only" -> pure ValueOnly
    "decision-report" -> pure DecisionReport
    "base-attributes" -> pure BaseAttributes
    val -> fail $ "Unknown OutcomeStyle: " <> Text.unpack val

instance ToJSON OutcomeStyle where
  toJSON = \case
    ValueOnly -> Aeson.String "value-only"
    DecisionReport -> Aeson.String "decision-report"
    BaseAttributes -> Aeson.String "base-attributes"

instance FromJSON Outcomes where
  parseJSON (Aeson.String t) = pure (OutcomeAttribute t)
  parseJSON v = Aeson.withObject "Outcomes" (\o ->
    OutcomePropertyObject <$> (OutcomeObject
      <$> o .: "@id"
      <*> o .:? "showSilent"
      <*> o .:? "showInvisible"
      <*> o .:? "resolveIndecisionRelationships"
      <*> o .:? "knownOutcomeStyle"
      <*> o .:? "unknownOutcomeStyle")) v

instance ToJSON Outcomes where
  toJSON (OutcomeAttribute t) = Aeson.String t
  toJSON (OutcomePropertyObject o) =
    Aeson.object
      [ "@id" .= o.id
      , "showSilent" .= o.showSilent
      , "showInvisible" .= o.showInvisible
      , "resolveIndecisionRelationships" .= o.resolveIndecisionRelationships
      , "knownOutcomeStyle" .= o.knownOutcomeStyle
      , "unknownOutcomeStyle" .= o.unknownOutcomeStyle
      ]

instance FromJSON InputCase where
  parseJSON = Aeson.withObject "InputCase" $ \o -> do
    caseId <- o .: "@id"
    let attrs = Aeson.KeyMap.toMapText $ Aeson.KeyMap.delete "@id" (Aeson.KeyMap.map id o)
    parsedAttrs <- traverse parseFnLiteral attrs
    pure $ InputCase caseId parsedAttrs
   where
    parseFnLiteral :: Aeson.Value -> Parser FnLiteral
    parseFnLiteral = parseJSON

instance ToJSON InputCase where
  toJSON ic =
    Aeson.object $
      [ "@id" .= ic.id
      ] <> [(Aeson.Key.fromText k, Aeson.toJSON v) | (k, v) <- Map.toList ic.attributes]

instance FromJSON BatchRequest where
  parseJSON = Aeson.withObject "BatchRequest" $ \o ->
    BatchRequest
      <$> o .: "outcomes"
      <*> o .: "cases"

instance ToJSON BatchRequest where
  toJSON br =
    Aeson.object
      [ "outcomes" .= br.outcomes
      , "cases" .= br.cases
      ]

instance FromJSON OutputCase where
  parseJSON = Aeson.withObject "OutputCase" $ \o -> do
    caseId <- o .: "@id"
    graphvizVal <- o .:? "@graphviz"
    let attrs = Aeson.KeyMap.toMapText $
          Aeson.KeyMap.delete "@graphviz" $
          Aeson.KeyMap.delete "@id" (Aeson.KeyMap.map id o)
    parsedAttrs <- traverse parseJSON attrs
    pure $ OutputCase caseId parsedAttrs graphvizVal

instance ToJSON OutputCase where
  toJSON oc =
    Aeson.object $
      [ "@id" .= oc.id
      ] <> maybe [] (\gv -> ["@graphviz" .= gv]) oc.graphviz
        <> [(Aeson.Key.fromText k, Aeson.toJSON v) | (k, v) <- Map.toList oc.attributes]

instance FromJSON OutputSummary where
  parseJSON = Aeson.withObject "OutputSummary" $ \o ->
    OutputSummary
      <$> o .: "casesRead"
      <*> o .: "casesProcessed"
      <*> o .: "casesIgnored"
      <*> fmap toFixed (o .: "processorDurationSec")
      <*> fmap toFixed (o .: "processorCasesPerSec")
      <*> fmap toFixed (o .: "processorQueuedSec")
   where
    toFixed :: Scientific.Scientific -> Centi
    toFixed = realToFrac

instance ToJSON OutputSummary where
  toJSON os =
    Aeson.object
      [ "casesRead" .= os.casesRead
      , "casesProcessed" .= os.casesProcessed
      , "casesIgnored" .= os.casesIgnored
      , "processorDurationSec" .= toSci os.processorDurationSec
      , "processorCasesPerSec" .= toSci os.casesPerSec
      , "processorQueuedSec" .= toSci os.processorQueuedSec
      ]
   where
    toSci :: Centi -> Scientific.Scientific
    toSci = realToFrac

instance FromJSON BatchResponse where
  parseJSON = Aeson.withObject "BatchResponse" $ \o ->
    BatchResponse
      <$> o .: "cases"
      <*> o .: "summary"

instance ToJSON BatchResponse where
  toJSON br =
    Aeson.object
      [ "cases" .= br.cases
      , "summary" .= br.summary
      ]

-- | Shared application environment threaded through all handlers.
data AppEnv = MkAppEnv
  { deploymentRegistry :: TVar (Map DeploymentId DeploymentState)
  , bundleStore        :: BundleStore
  , serverName         :: Maybe Text
  }

-- | The handler monad for all Servant routes.
type AppM = ReaderT AppEnv Handler
