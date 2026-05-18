{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module ControlPlane (
  ControlPlaneApi,
  DeploymentStatusResponse (..),
  controlPlaneHandler,
  -- Individual handlers (re-exported for short routes)
  getDeploymentHandler,
  putDeploymentHandler,
  deleteDeploymentHandler,
) where

import qualified BundleStore
import L4.FunctionSchema (Parameters (..))
import Compatibility (FnIface, ifaceFromFunction, ifaceFromSummary, detectBreakingChanges)
import Compiler (compileBundle, computeVersion)
import DeploymentLoader (triggerCompilationIfPending)
import Logging (logInfo, logWarn, logError)
import Options (Options (..))
import Shared (jsonError)
import Types

import Control.Applicative ((<|>))
import Control.Concurrent.Async (async)
import Control.Concurrent.STM (atomically, modifyTVar', readTVarIO)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (asks)
import qualified Codec.Archive.Zip as Zip
import Data.Aeson (FromJSON, ToJSON, toJSON, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.Char (isAlphaNum)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding
import Data.Time (addUTCTime, getCurrentTime)
import Data.UUID.V4 (nextRandom)
import qualified Data.UUID as UUID
import GHC.Generics (Generic)
import Servant
import Servant.Multipart
import System.FilePath (splitDirectories)
import Control.Exception (SomeException, catch, displayException)
import Data.Int (Int64)
import GHC.Conc (setAllocationCounter, enableAllocationLimit)
import GHC.IO.Exception (AllocationLimitExceeded (..))
import System.Timeout (timeout)

-- | Status returned in GET responses.
data DeploymentStatusResponse = DeploymentStatusResponse
  { dsId       :: !Text
  , dsStatus   :: !Text    -- "pending" | "compiling" | "ready" | "failed"
  , dsMetadata :: !(Maybe DeploymentMetadata)
  , dsError    :: !(Maybe Text)
  }
  deriving stock (Show, Generic)

instance ToJSON DeploymentStatusResponse where
  toJSON ds = Aeson.object $
    [ "id"       .= ds.dsId
    , "status"   .= ds.dsStatus
    ] <> maybe [] (\m -> ["metadata" .= m]) ds.dsMetadata
      <> maybe [] (\e -> ["error" .= e]) ds.dsError

instance FromJSON DeploymentStatusResponse where
  parseJSON = Aeson.withObject "DeploymentStatusResponse" $ \o ->
    DeploymentStatusResponse
      <$> (o Aeson..: "id"       <|> o Aeson..: "dsId")
      <*> (o Aeson..: "status"   <|> o Aeson..: "dsStatus")
      <*> (o Aeson..:? "metadata" <|> o Aeson..:? "dsMetadata")
      <*> (o Aeson..:? "error"    <|> o Aeson..:? "dsError")

-- | The control plane API for managing deployments.
type ControlPlaneApi =
       "deployments" :> MultipartForm Mem (MultipartData Mem) :> Verb 'POST 202 '[JSON] DeploymentStatusResponse
  :<|> "deployments" :> QueryParam "functions" Text :> QueryParam "scope" Text :> Get '[JSON] [DeploymentStatusResponse]
  :<|> "deployments" :> Capture "deploymentId" Text :> Get '[JSON] DeploymentStatusResponse
  :<|> "deployments" :> Capture "deploymentId" Text :> MultipartForm Mem (MultipartData Mem) :> Verb 'PUT 202 '[JSON] DeploymentStatusResponse
  :<|> "deployments" :> Capture "deploymentId" Text :> DeleteNoContent

-- | Combined handler for the control plane.
controlPlaneHandler :: Visibility -> ServerT ControlPlaneApi AppM
controlPlaneHandler vis =
       postDeploymentHandler
  :<|> getDeploymentsHandler vis
  :<|> getDeploymentHandler vis
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

  liftIO $ logInfo env.logger "Deployment created"
    [("deploymentId", toJSON deployId.unDeploymentId)]

  -- Extract zip from multipart file
  sourceMap <- extractSourcesFromMultipart multipart
  let mDesc = extractDescription multipart

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
        throwError err400 { errBody = jsonError "Maximum deployment limit reached" }

      -- Normal path: save, register as pending, compile async
      now <- liftIO getCurrentTime
      let storedMeta = BundleStore.StoredMetadata
            { BundleStore.smVersion = version
            , BundleStore.smCreatedAt = Text.pack (show now)
            , BundleStore.smDescription = mDesc
            }

      liftIO $ BundleStore.saveBundle env.bundleStore (deployId.unDeploymentId) sourceMap storedMeta

      liftIO $ atomically $ modifyTVar' env.deploymentRegistry $
        Map.insert deployId DeploymentCompiling

      let compileTimeoutMicros = cfg.compileTimeout * 1_000_000
          compileMemLimitBytes = fromIntegral cfg.maxCompileMemoryMb * 1024 * 1024 :: Int64

      _ <- liftIO $ async $ do
        let compileLimited = do
              setAllocationCounter compileMemLimitBytes
              enableAllocationLimit
              compileBundle env.logger deployId.unDeploymentId sourceMap
        mResult <- (timeout compileTimeoutMicros compileLimited)
          `catch` \AllocationLimitExceeded -> do
            logError env.logger "Compilation exceeded memory limit"
              [ ("deploymentId", toJSON deployId.unDeploymentId)
              , ("maxCompileMemoryMb", toJSON cfg.maxCompileMemoryMb)
              ]
            atomically $ modifyTVar' env.deploymentRegistry $
              Map.insert deployId (DeploymentFailed "Compilation exceeded memory limit")
            pure Nothing
        case mResult of
          Nothing -> do
            logError env.logger "Compilation timed out"
              [("deploymentId", toJSON deployId.unDeploymentId)]
            atomically $ modifyTVar' env.deploymentRegistry $
              Map.insert deployId (DeploymentFailed "Compilation timed out")
          Just (Right (fns, meta0, bundles)) -> do
            let meta = meta0 { metaDescription = mDesc }
            BundleStore.saveBundleCbor env.bundleStore (deployId.unDeploymentId) bundles
            BundleStore.saveMetadataCache env.bundleStore (deployId.unDeploymentId) (Aeson.encode meta)
              `catch` \(e :: SomeException) ->
                logWarn env.logger "Failed to save metadata cache (non-fatal)"
                  [ ("deploymentId", toJSON deployId.unDeploymentId)
                  , ("error", toJSON (displayException e))
                  ]
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

-- | GET /deployments — list all deployments.
-- ?functions=simple → include function name + description in metadata (like GET .../functions)
-- ?functions=full   → include full function details (parameters, returnType, section) in metadata
-- ?scope=pattern    → filter by deployment pattern (e.g. "classics", "classics,test-rules")
-- All function data gated by X-Include-Functions header — if false, ?functions is ignored.
getDeploymentsHandler :: Visibility -> Maybe Text -> Maybe Text -> AppM [DeploymentStatusResponse]
getDeploymentsHandler vis mFunctions mScope = do
  env <- asks id
  liftIO $ logInfo env.logger "Deployments listed"
    [("functions", toJSON mFunctions), ("scope", toJSON mScope)]
  registry <- liftIO $ readTVarIO env.deploymentRegistry
  let debugMode = env.options.debug
      fnMode = if not vis.showFunctions then FnNone
               else case mFunctions of
                 Just "full" -> FnFull
                 Just "none" -> FnNone
                 _           -> FnSimple  -- default: name + description

  let allResponses = map (\(did, state) ->
        stateToResponse debugMode vis fnMode did state
        ) (Map.toList registry)

  pure $ case mScope of
    Nothing -> allResponses
    Just scope -> filter (matchesDeploymentScope scope) allResponses
 where
  matchesDeploymentScope scope resp =
    let patterns = Text.splitOn "," scope
    in any (\pat ->
      let trimmed = Text.strip pat
          (depPat, _rest) = Text.breakOn "/" trimmed
      in depPat == "*" || depPat == resp.dsId
      ) patterns

-- | Function detail level in deployment responses.
data FnMode = FnNone | FnSimple | FnFull

-- | Strip parameters from a FunctionSummary (for ?functions=simple).
simplify :: FunctionSummary -> FunctionSummary
simplify fs = fs
  { fsParameters = MkParameters Map.empty []
  , fsSourceFile = Nothing
  }

-- | GET /deployments/{id} — get deployment status.
-- If the deployment is Pending (lazy-load), compiles it synchronously
-- before returning the response.
getDeploymentHandler :: Visibility -> Text -> AppM DeploymentStatusResponse
getDeploymentHandler vis deployIdText = do
  env <- asks id
  let deployId = DeploymentId deployIdText
  liftIO $ logInfo env.logger "Deployment retrieved"
    [("deploymentId", toJSON deployIdText)]
  registry <- liftIO $ readTVarIO env.deploymentRegistry
  baseResp <- case Map.lookup deployId registry of
    Nothing -> throwError err404
    Just (DeploymentPending _) -> do
      triggerCompilationIfPending deployId
      -- Re-read: compilation is synchronous, state is now Ready or Failed
      registry' <- liftIO $ readTVarIO env.deploymentRegistry
      case Map.lookup deployId registry' of
        Nothing -> throwError err404
        Just state' -> pure (stateToResponse env.options.debug vis FnSimple deployId state')
    Just state -> pure (stateToResponse env.options.debug vis FnSimple deployId state)
  applyPendingUpdate env deployId baseResp

-- | Overlay the outcome of an in-flight async PUT onto the status
-- response. The live (old) deployment keeps its real state in the
-- registry; this only changes what the status endpoint reports:
--
--   * 'UpdateCompiling' → "compiling" (so a deploy poll keeps waiting
--     instead of seeing the still-live old version as "ready").
--   * 'UpdateRejected' → "failed" + reason for every poller until the
--     TTL lapses, then the (still-stale) marker self-clears and the true
--     live state is reported again. The deployment list endpoint never
--     consults this map, so it always reflects the true live state.
applyPendingUpdate
  :: AppEnv -> DeploymentId -> DeploymentStatusResponse -> AppM DeploymentStatusResponse
applyPendingUpdate env deployId resp = do
  pending <- liftIO $ readTVarIO env.pendingUpdates
  case Map.lookup deployId pending of
    Nothing -> pure resp
    Just UpdateCompiling ->
      pure resp { dsStatus = "compiling", dsMetadata = Nothing, dsError = Nothing }
    Just (UpdateRejected msg expiry) -> do
      now <- liftIO getCurrentTime
      if now < expiry
        then pure resp { dsStatus = "failed", dsMetadata = Nothing, dsError = Just msg }
        else do
          -- Stale: drop it (only if it is still the same expired
          -- rejection — a concurrent new PUT may have replaced it with
          -- 'UpdateCompiling', which must be preserved) and report the
          -- real, still-live deployment state.
          liftIO $ atomically $ modifyTVar' env.pendingUpdates $
            Map.update
              (\case
                  UpdateRejected _ e | e <= now -> Nothing
                  keep -> Just keep)
              deployId
          pure resp

-- | PUT /deployments/{id} — replace a deployment bundle
putDeploymentHandler :: Text -> MultipartData Mem -> AppM DeploymentStatusResponse
putDeploymentHandler deployIdText multipart = do
  env <- asks id
  let deployId = DeploymentId deployIdText

  liftIO $ logInfo env.logger "Deployment update requested"
    [("deploymentId", toJSON deployIdText)]

  validateDeploymentId deployIdText

  -- Check deployment exists; capture its current description (so a
  -- source-only PUT that omits the field preserves it) and its current
  -- interface for the backwards-compatibility guard.
  registry <- liftIO $ readTVarIO env.deploymentRegistry
  (existingDesc, mOldIfaces) <- case Map.lookup deployId registry of
    Nothing -> throwError err404
    Just st -> pure (stateDescription st, currentIfaces st)

  -- Extract sources
  sourceMap <- extractSourcesFromMultipart multipart
  let mDesc = extractDescription multipart <|> existingDesc

  now <- liftIO getCurrentTime
  let cfg = env.options
      version = computeVersion sourceMap
      storedMeta = BundleStore.StoredMetadata
        { BundleStore.smVersion = version
        , BundleStore.smCreatedAt = Text.pack (show now)
        , BundleStore.smDescription = mDesc
        }
      compileTimeoutMicros = cfg.compileTimeout * 1_000_000
      compileMemLimitBytes = fromIntegral cfg.maxCompileMemoryMb * 1024 * 1024 :: Int64

  -- Mark the update in progress so the status endpoint reports
  -- "compiling" rather than the still-live old version's "ready".
  liftIO $ atomically $ modifyTVar' env.pendingUpdates $
    Map.insert deployId UpdateCompiling

  -- Compile + compatibility-check in the background. The request returns
  -- 202 immediately (proxy-friendly for large bundles); the old version
  -- keeps serving until a compatible bundle is registered. Outcome is
  -- surfaced via GET /deployments/{id} (see 'applyPendingUpdate').
  let reject msg = do
        logWarn env.logger "PUT update rejected"
          [ ("deploymentId", toJSON deployId.unDeploymentId)
          , ("reason", toJSON msg)
          ]
        -- Keep the rejection visible to pollers for a bounded window
        -- (120s), then it self-clears in 'applyPendingUpdate'. Also
        -- superseded eagerly by the next PUT or a successful update.
        rejectedAt <- getCurrentTime
        let expiry = addUTCTime 120 rejectedAt
        atomically $ modifyTVar' env.pendingUpdates $
          Map.insert deployId (UpdateRejected msg expiry)
      compileLimited = do
        setAllocationCounter compileMemLimitBytes
        enableAllocationLimit
        compileBundle env.logger deployId.unDeploymentId sourceMap

  _ <- liftIO $ async $ do
    mResult <- timeout compileTimeoutMicros compileLimited
      `catch` \AllocationLimitExceeded -> pure Nothing
    case mResult of
      Nothing ->
        reject "Update rejected: compilation timed out or exceeded the memory limit"
      Just (Left err) ->
        reject ("Update rejected: compilation failed: " <> err)
      Just (Right (fns, meta0, bundles)) -> do
        let newIfaces = map (ifaceFromFunction . (.fnImpl)) (Map.elems fns)
            breaking = maybe [] (`detectBreakingChanges` newIfaces) mOldIfaces
        case breaking of
          (_:_) ->
            reject $ "Update rejected — it would break existing integrations: "
                       <> Text.intercalate "; " breaking
          [] -> do
            let meta = meta0 { metaDescription = mDesc }
            BundleStore.saveBundle env.bundleStore (deployId.unDeploymentId) sourceMap storedMeta
            BundleStore.saveBundleCbor env.bundleStore (deployId.unDeploymentId) bundles
            BundleStore.saveMetadataCache env.bundleStore (deployId.unDeploymentId) (Aeson.encode meta)
              `catch` \(e :: SomeException) ->
                logWarn env.logger "Failed to save metadata cache (non-fatal)"
                  [ ("deploymentId", toJSON deployId.unDeploymentId)
                  , ("error", toJSON (displayException e))
                  ]
            atomically $ do
              modifyTVar' env.deploymentRegistry $
                Map.insert deployId (DeploymentReady fns meta)
              modifyTVar' env.pendingUpdates $ Map.delete deployId
            logInfo env.logger "Deployment updated (interface-compatible)"
              [("deploymentId", toJSON deployId.unDeploymentId)]

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
      ok <- liftIO $ BundleStore.deleteBundle env.bundleStore (deployId.unDeploymentId)
      if ok
        then do
          liftIO $ atomically $ modifyTVar' env.deploymentRegistry $
            Map.delete deployId
          liftIO $ logInfo env.logger "Deployment deleted"
            [("deploymentId", toJSON deployId.unDeploymentId)]
          pure NoContent
        else do
          liftIO $ logWarn env.logger "Deployment directory not fully removed, retry later"
            [("deploymentId", toJSON deployId.unDeploymentId)]
          throwError err409 { errBody = jsonError "Could not remove deployment files — retry later" }

-- ----------------------------------------------------------------------------
-- Helpers
-- ----------------------------------------------------------------------------

-- | Convert DeploymentState to a response.
-- In non-debug mode, error details are hidden.
-- Visibility controls which fields are included in the response.
stateToResponse :: Bool -> Visibility -> FnMode -> DeploymentId -> DeploymentState -> DeploymentStatusResponse
stateToResponse debugMode vis fnMode (DeploymentId did) = \case
  DeploymentPending mCachedMeta ->
    let meta = case mCachedMeta of
          Just m -> Just m { metaFunctions = case fnMode of FnNone -> []; FnSimple -> map simplify m.metaFunctions; FnFull -> m.metaFunctions, metaFiles = if vis.showFiles then m.metaFiles else [] }
          Nothing -> Nothing
    in DeploymentStatusResponse did "pending" meta Nothing
  DeploymentCompiling -> DeploymentStatusResponse did "compiling" Nothing Nothing
  DeploymentReady _ meta ->
    let filteredMeta = meta
          { metaFunctions = case fnMode of
              FnNone -> []
              FnSimple -> map simplify meta.metaFunctions
              FnFull -> meta.metaFunctions
          , metaFiles = if vis.showFiles then meta.metaFiles else []}
    in DeploymentStatusResponse did "ready" (Just filteredMeta) Nothing
  DeploymentFailed err ->
    let errorMsg = if debugMode then Just err else Just "Compilation failed"
    in DeploymentStatusResponse did "failed" Nothing errorMsg

-- | Validate a deployment ID.
-- Must be <= 36 characters (UUID length), alphanumeric + hyphens + underscores,
-- and must not contain ".." sequences.
-- | Reserved words that cannot be used as deployment IDs.
-- These correspond to top-level route segments in the API.
reservedWords :: [Text]
reservedWords = ["health", "deployments", "openapi.json"]

validateDeploymentId :: Text -> AppM ()
validateDeploymentId deployId = do
  when (deployId `elem` reservedWords) $
    throwError err400 { errBody = jsonError "Deployment ID is a reserved word" }
  when (Text.isPrefixOf "." deployId) $
    throwError err400 { errBody = jsonError "Deployment ID must not start with a dot" }
  when (Text.length deployId > 36) $
    throwError err400 { errBody = jsonError "Deployment ID exceeds maximum length of 36 characters" }
  when (Text.isInfixOf ".." deployId) $
    throwError err400 { errBody = jsonError "Deployment ID contains invalid sequence" }
  when (not $ Text.all isValidIdChar deployId) $
    throwError err400 { errBody = jsonError "Deployment ID contains invalid characters (allowed: a-z, A-Z, 0-9, -, _)" }
 where
  isValidIdChar c = isAlphaNum c || c == '-' || c == '_'

-- | Check if a zip entry path is safe (no path traversal).
isPathSafe :: FilePath -> Bool
isPathSafe path = ".." `notElem` splitDirectories path

-- | Maximum stored length of the operator-supplied deployment description.
-- Bounds the downstream system-prompt token budget and the
-- prompt-injection surface.
maxDescriptionLength :: Int
maxDescriptionLength = 4000

-- | Extract and sanitize the optional operator-supplied deployment
-- description ("Intended use") from the multipart form.
-- Trimmed, length-capped, and 'Nothing' when absent or blank.
extractDescription :: MultipartData Mem -> Maybe Text
extractDescription multipart =
  case lookupInput "description" multipart of
    Right raw ->
      let trimmed = Text.strip raw
      in if Text.null trimmed
           then Nothing
           else Just (Text.take maxDescriptionLength trimmed)
    Left _ -> Nothing

-- | The operator-supplied description currently associated with a
-- deployment state, if any. Used to preserve the description across a
-- source-only PUT that omits the field.
stateDescription :: DeploymentState -> Maybe Text
stateDescription (DeploymentReady _ meta)        = meta.metaDescription
stateDescription (DeploymentPending (Just meta)) = meta.metaDescription
stateDescription _                               = Nothing

-- | The currently-deployed interface for the backwards-compatibility
-- guard. Uses the live in-memory functions when ready (full schema,
-- including the structured return shape) or the cached metadata when
-- only that is available. 'Nothing' (compiling / failed / no cache)
-- means there is no prior interface to break, so the guard is skipped.
currentIfaces :: DeploymentState -> Maybe [FnIface]
currentIfaces (DeploymentReady fns _)        =
  Just (map (ifaceFromFunction . (.fnImpl)) (Map.elems fns))
currentIfaces (DeploymentPending (Just meta)) =
  Just (map ifaceFromSummary meta.metaFunctions)
currentIfaces _                              = Nothing

-- | Extract sources from a multipart zip upload with validation.
extractSourcesFromMultipart :: MultipartData Mem -> AppM (Map FilePath Text)
extractSourcesFromMultipart multipart = do
  cfg <- asks (.options)

  case lookupFile "sources" multipart of
    Left _ -> throwError $ err400 { errBody = jsonError "Missing 'sources' file field (zip archive)" }
    Right (fileData :: FileData Mem) -> do
      let zipBytes = fdPayload fileData :: LBS.ByteString

      -- Validate zip file size
      when (LBS.length zipBytes > fromIntegral cfg.maxZipSize) $
        throwError $ err400 { errBody = jsonError "Zip archive exceeds maximum size" }

      let archive = Zip.toArchive zipBytes
          entries = Zip.zEntries archive

      -- Validate file count
      when (length entries > cfg.maxFileCount) $
        throwError $ err400 { errBody = jsonError "Zip archive exceeds maximum file count" }

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
        throwError $ err400 { errBody = jsonError "Zip entry contains path traversal" }

      if null l4Entries
        then throwError $ err400 { errBody = jsonError "Zip archive contains no files" }
        else pure (Map.fromList l4Entries)


