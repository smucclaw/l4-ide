{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module ControlPlane (
  ControlPlaneApi,
  DeploymentStatusResponse (..),
  controlPlaneHandler,
) where

import qualified BundleStore
import Compiler (compileBundle, computeVersion)
import Types

import Control.Concurrent.Async (async)
import Control.Concurrent.STM (atomically, modifyTVar', readTVarIO)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (asks)
import qualified Codec.Archive.Zip as Zip
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.ByteString.Lazy as LBS
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding
import Data.Time (getCurrentTime)
import Data.UUID.V4 (nextRandom)
import qualified Data.UUID as UUID
import GHC.Generics (Generic)
import Servant
import Servant.Multipart

-- | Status returned in GET responses.
data DeploymentStatusResponse = DeploymentStatusResponse
  { dsId       :: !Text
  , dsStatus   :: !Text    -- "compiling" | "ready" | "failed"
  , dsMetadata :: !(Maybe DeploymentMetadata)
  , dsError    :: !(Maybe Text)
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | The control plane API for managing deployments.
type ControlPlaneApi =
       "deployments" :> MultipartForm Mem (MultipartData Mem) :> Verb 'POST 202 '[JSON] DeploymentStatusResponse
  :<|> "deployments" :> Get '[JSON] [DeploymentStatusResponse]
  :<|> "deployments" :> Capture "deploymentId" Text :> Get '[JSON] DeploymentStatusResponse
  :<|> "deployments" :> Capture "deploymentId" Text :> MultipartForm Mem (MultipartData Mem) :> Verb 'PUT 202 '[JSON] DeploymentStatusResponse
  :<|> "deployments" :> Capture "deploymentId" Text :> DeleteNoContent

-- | Combined handler for the control plane.
controlPlaneHandler :: ServerT ControlPlaneApi AppM
controlPlaneHandler =
       postDeploymentHandler
  :<|> getDeploymentsHandler
  :<|> getDeploymentHandler
  :<|> putDeploymentHandler
  :<|> deleteDeploymentHandler

-- | POST /deployments — deploy a new bundle
postDeploymentHandler :: MultipartData Mem -> AppM DeploymentStatusResponse
postDeploymentHandler multipart = do
  env <- asks id

  -- Extract deployment ID (from multipart field or generate UUID)
  deployId <- case lookupInput "id" multipart of
    Right idText | not (Text.null idText) -> pure (DeploymentId idText)
    _ -> liftIO $ DeploymentId . Text.pack . UUID.toString <$> nextRandom

  -- Extract zip from multipart file
  sourceMap <- extractSourcesFromMultipart multipart

  -- Compute version and save to store
  now <- liftIO getCurrentTime
  let version = computeVersion sourceMap
      storedMeta = BundleStore.StoredMetadata
        { BundleStore.smFunctions = []
        , BundleStore.smVersion = version
        , BundleStore.smCreatedAt = Text.pack (show now)
        }

  liftIO $ BundleStore.saveBundle env.bundleStore (deployId.unDeploymentId) sourceMap storedMeta

  -- Register as pending
  liftIO $ atomically $ modifyTVar' env.deploymentRegistry $
    Map.insert deployId DeploymentPending

  -- Launch async compilation
  _ <- liftIO $ async $ do
    result <- compileBundle sourceMap
    case result of
      Right (fns, meta) ->
        atomically $ modifyTVar' env.deploymentRegistry $
          Map.insert deployId (DeploymentReady fns meta)
      Left err ->
        atomically $ modifyTVar' env.deploymentRegistry $
          Map.insert deployId (DeploymentFailed err)

  pure DeploymentStatusResponse
    { dsId = deployId.unDeploymentId
    , dsStatus = "compiling"
    , dsMetadata = Nothing
    , dsError = Nothing
    }

-- | GET /deployments — list all deployments
getDeploymentsHandler :: AppM [DeploymentStatusResponse]
getDeploymentsHandler = do
  registry <- asks (.deploymentRegistry) >>= liftIO . readTVarIO
  pure [stateToResponse did state | (did, state) <- Map.toList registry]

-- | GET /deployments/{id} — get deployment status
getDeploymentHandler :: Text -> AppM DeploymentStatusResponse
getDeploymentHandler deployIdText = do
  let deployId = DeploymentId deployIdText
  registry <- asks (.deploymentRegistry) >>= liftIO . readTVarIO
  case Map.lookup deployId registry of
    Nothing -> throwError err404
    Just state -> pure (stateToResponse deployId state)

-- | PUT /deployments/{id} — replace a deployment bundle
putDeploymentHandler :: Text -> MultipartData Mem -> AppM DeploymentStatusResponse
putDeploymentHandler deployIdText multipart = do
  env <- asks id
  let deployId = DeploymentId deployIdText

  -- Check deployment exists
  registry <- liftIO $ readTVarIO env.deploymentRegistry
  case Map.lookup deployId registry of
    Nothing -> throwError err404
    Just _ -> pure ()

  -- Extract sources
  sourceMap <- extractSourcesFromMultipart multipart

  -- Save to store
  now <- liftIO getCurrentTime
  let version = computeVersion sourceMap
      storedMeta = BundleStore.StoredMetadata
        { BundleStore.smFunctions = []
        , BundleStore.smVersion = version
        , BundleStore.smCreatedAt = Text.pack (show now)
        }

  liftIO $ BundleStore.saveBundle env.bundleStore (deployId.unDeploymentId) sourceMap storedMeta

  -- Compile in background; old version stays active until new compilation completes
  _ <- liftIO $ async $ do
    result <- compileBundle sourceMap
    case result of
      Right (fns, meta) ->
        atomically $ modifyTVar' env.deploymentRegistry $
          Map.insert deployId (DeploymentReady fns meta)
      Left err -> do
        putStrLn $ "PUT compilation failed for " <> show deployId <> ": " <> Text.unpack err
        -- Keep old version active, don't overwrite with Failed

  pure DeploymentStatusResponse
    { dsId = deployId.unDeploymentId
    , dsStatus = "compiling"
    , dsMetadata = Nothing
    , dsError = Nothing
    }

-- | DELETE /deployments/{id} — remove a deployment
deleteDeploymentHandler :: Text -> AppM NoContent
deleteDeploymentHandler deployIdText = do
  env <- asks id
  let deployId = DeploymentId deployIdText

  -- Remove from registry
  registry <- liftIO $ readTVarIO env.deploymentRegistry
  case Map.lookup deployId registry of
    Nothing -> throwError err404
    Just _ -> do
      liftIO $ atomically $ modifyTVar' env.deploymentRegistry $
        Map.delete deployId
      liftIO $ BundleStore.deleteBundle env.bundleStore (deployId.unDeploymentId)
      pure NoContent

-- ----------------------------------------------------------------------------
-- Helpers
-- ----------------------------------------------------------------------------

-- | Convert DeploymentState to a response.
stateToResponse :: DeploymentId -> DeploymentState -> DeploymentStatusResponse
stateToResponse (DeploymentId did) = \case
  DeploymentPending -> DeploymentStatusResponse did "compiling" Nothing Nothing
  DeploymentReady _ meta -> DeploymentStatusResponse did "ready" (Just meta) Nothing
  DeploymentFailed err -> DeploymentStatusResponse did "failed" Nothing (Just err)

-- | Extract .l4 sources from a multipart zip upload.
extractSourcesFromMultipart :: MultipartData Mem -> AppM (Map FilePath Text)
extractSourcesFromMultipart multipart = do
  case lookupFile "sources" multipart of
    Left _ -> throwError $ err400 { errBody = "Missing 'sources' file field (zip archive)" }
    Right (fileData :: FileData Mem) -> do
      let zipBytes = fdPayload fileData :: LBS.ByteString
          archive = Zip.toArchive zipBytes
          entries = Zip.zEntries archive
          l4Entries =
            [ (Zip.eRelativePath entry, Text.Encoding.decodeUtf8 $ LBS.toStrict (Zip.fromEntry entry))
            | entry <- entries
            , not (null (Zip.eRelativePath entry))
            , last (Zip.eRelativePath entry) /= '/'  -- Skip directories
            -- Accept all files, not just .l4, to support imports from any extension
            ]
      if null l4Entries
        then throwError $ err400 { errBody = "Zip archive contains no files" }
        else pure (Map.fromList l4Entries)
