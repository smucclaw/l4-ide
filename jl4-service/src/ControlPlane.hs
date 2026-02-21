{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module ControlPlane (
  ControlPlaneApi,
  DeploymentStatusResponse (..),
  controlPlaneHandler,
) where

import qualified BundleStore
import Compiler (compileBundle, computeVersion)
import Logging (logInfo, logWarn, logError)
import Options (Options (..))
import Types

import Control.Concurrent.Async (async)
import Control.Concurrent.STM (atomically, modifyTVar', readTVarIO)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (asks)
import qualified Codec.Archive.Zip as Zip
import Data.Aeson (FromJSON, ToJSON, toJSON)
import qualified Data.ByteString.Lazy as LBS
import Data.Char (isAlphaNum)
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
import System.FilePath (splitDirectories)
import System.Timeout (timeout)

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
    Right idText | not (Text.null idText) -> do
      validateDeploymentId idText
      pure (DeploymentId idText)
    _ -> liftIO $ DeploymentId . Text.pack . UUID.toString <$> nextRandom

  -- Extract zip from multipart file
  sourceMap <- extractSourcesFromMultipart multipart

  -- Compute version hash and check for existing deployment with same sources
  let version = computeVersion sourceMap

  registry <- liftIO $ readTVarIO env.deploymentRegistry
  let existingMatch =
        [ (did, meta)
        | (did, DeploymentReady _ meta) <- Map.toList registry
        , meta.metaVersion == version
        ]

  case existingMatch of
    ((existingId, existingMeta):_) ->
      -- Return existing deployment immediately — no recompile needed
      pure DeploymentStatusResponse
        { dsId = existingId.unDeploymentId
        , dsStatus = "ready"
        , dsMetadata = Just existingMeta
        , dsError = Nothing
        }
    [] -> do
      -- Check deployment count limit
      let cfg = env.options
      when (Map.size registry >= cfg.maxDeployments) $
        throwError err400 { errBody = "Maximum deployment limit reached" }

      -- Normal path: save, register as pending, compile async
      now <- liftIO getCurrentTime
      let storedMeta = BundleStore.StoredMetadata
            { BundleStore.smFunctions = []
            , BundleStore.smVersion = version
            , BundleStore.smCreatedAt = Text.pack (show now)
            }

      liftIO $ BundleStore.saveBundle env.bundleStore (deployId.unDeploymentId) sourceMap storedMeta

      liftIO $ atomically $ modifyTVar' env.deploymentRegistry $
        Map.insert deployId DeploymentPending

      let compileTimeoutMicros = cfg.compileTimeout * 1_000_000

      _ <- liftIO $ async $ do
        mResult <- timeout compileTimeoutMicros $ compileBundle env.logger sourceMap
        case mResult of
          Nothing -> do
            logError env.logger "Compilation timed out"
              [("deploymentId", toJSON deployId.unDeploymentId)]
            atomically $ modifyTVar' env.deploymentRegistry $
              Map.insert deployId (DeploymentFailed "Compilation timed out")
          Just (Right (fns, meta, bundles)) -> do
            mapM_ (BundleStore.saveBundleCbor env.bundleStore (deployId.unDeploymentId)) bundles
            atomically $ modifyTVar' env.deploymentRegistry $
              Map.insert deployId (DeploymentReady fns meta)
          Just (Left err) -> do
            logWarn env.logger "Compilation failed"
              [ ("deploymentId", toJSON deployId.unDeploymentId)
              , ("error", toJSON err)
              ]
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
  env <- asks id
  registry <- liftIO $ readTVarIO env.deploymentRegistry
  let debugMode = env.options.debug
  pure [stateToResponse debugMode did state | (did, state) <- Map.toList registry]

-- | GET /deployments/{id} — get deployment status
getDeploymentHandler :: Text -> AppM DeploymentStatusResponse
getDeploymentHandler deployIdText = do
  env <- asks id
  let deployId = DeploymentId deployIdText
  registry <- liftIO $ readTVarIO env.deploymentRegistry
  case Map.lookup deployId registry of
    Nothing -> throwError err404
    Just state -> pure (stateToResponse env.options.debug deployId state)

-- | PUT /deployments/{id} — replace a deployment bundle
putDeploymentHandler :: Text -> MultipartData Mem -> AppM DeploymentStatusResponse
putDeploymentHandler deployIdText multipart = do
  env <- asks id
  let deployId = DeploymentId deployIdText

  validateDeploymentId deployIdText

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

  let compileTimeoutMicros = env.options.compileTimeout * 1_000_000

  -- Compile in background; old version stays active until new compilation completes
  _ <- liftIO $ async $ do
    mResult <- timeout compileTimeoutMicros $ compileBundle env.logger sourceMap
    case mResult of
      Nothing ->
        logError env.logger "PUT compilation timed out"
          [("deploymentId", toJSON deployId.unDeploymentId)]
      Just (Right (fns, meta, bundles)) -> do
        -- Save CBOR cache for fast restart
        mapM_ (BundleStore.saveBundleCbor env.bundleStore (deployId.unDeploymentId)) bundles
        atomically $ modifyTVar' env.deploymentRegistry $
          Map.insert deployId (DeploymentReady fns meta)
      Just (Left err) ->
        logWarn env.logger "PUT compilation failed"
          [ ("deploymentId", toJSON deployId.unDeploymentId)
          , ("error", toJSON err)
          ]

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

      liftIO $ logInfo env.logger "Deployment deleted"
        [("deploymentId", toJSON deployId.unDeploymentId)]

      pure NoContent

-- ----------------------------------------------------------------------------
-- Helpers
-- ----------------------------------------------------------------------------

-- | Convert DeploymentState to a response.
-- In non-debug mode, error details are hidden.
stateToResponse :: Bool -> DeploymentId -> DeploymentState -> DeploymentStatusResponse
stateToResponse debugMode (DeploymentId did) = \case
  DeploymentPending -> DeploymentStatusResponse did "compiling" Nothing Nothing
  DeploymentReady _ meta -> DeploymentStatusResponse did "ready" (Just meta) Nothing
  DeploymentFailed err ->
    let errorMsg = if debugMode then Just err else Just "Compilation failed"
    in DeploymentStatusResponse did "failed" Nothing errorMsg

-- | Validate a deployment ID.
-- Must be <= 36 characters (UUID length), alphanumeric + hyphens + underscores,
-- and must not contain ".." sequences.
validateDeploymentId :: Text -> AppM ()
validateDeploymentId deployId = do
  when (Text.length deployId > 36) $
    throwError err400 { errBody = "Deployment ID exceeds maximum length of 36 characters" }
  when (Text.isInfixOf ".." deployId) $
    throwError err400 { errBody = "Deployment ID contains invalid sequence" }
  when (not $ Text.all isValidIdChar deployId) $
    throwError err400 { errBody = "Deployment ID contains invalid characters (allowed: a-z, A-Z, 0-9, -, _)" }
 where
  isValidIdChar c = isAlphaNum c || c == '-' || c == '_'

-- | Check if a zip entry path is safe (no path traversal).
isPathSafe :: FilePath -> Bool
isPathSafe path = ".." `notElem` splitDirectories path

-- | Extract sources from a multipart zip upload with validation.
extractSourcesFromMultipart :: MultipartData Mem -> AppM (Map FilePath Text)
extractSourcesFromMultipart multipart = do
  cfg <- asks (.options)

  case lookupFile "sources" multipart of
    Left _ -> throwError $ err400 { errBody = "Missing 'sources' file field (zip archive)" }
    Right (fileData :: FileData Mem) -> do
      let zipBytes = fdPayload fileData :: LBS.ByteString

      -- Validate zip file size
      when (LBS.length zipBytes > fromIntegral cfg.maxZipSize) $
        throwError $ err400 { errBody = "Zip archive exceeds maximum size" }

      let archive = Zip.toArchive zipBytes
          entries = Zip.zEntries archive

      -- Validate file count
      when (length entries > cfg.maxFileCount) $
        throwError $ err400 { errBody = "Zip archive exceeds maximum file count" }

      -- Extract entries with path traversal check
      let l4Entries =
            [ (Zip.eRelativePath entry, Text.Encoding.decodeUtf8 $ LBS.toStrict (Zip.fromEntry entry))
            | entry <- entries
            , not (null (Zip.eRelativePath entry))
            , last (Zip.eRelativePath entry) /= '/'  -- Skip directories
            -- Accept all files, not just .l4, to support imports from any extension
            ]
          unsafePaths = [Zip.eRelativePath entry | entry <- entries, not (isPathSafe (Zip.eRelativePath entry))]

      -- Reject if any paths contain ".."
      when (not (null unsafePaths)) $
        throwError $ err400 { errBody = "Zip entry contains path traversal" }

      if null l4Entries
        then throwError $ err400 { errBody = "Zip archive contains no files" }
        else pure (Map.fromList l4Entries)

