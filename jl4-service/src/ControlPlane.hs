{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module ControlPlane (
  ControlPlaneApi,
  DeploymentStatusResponse (..),
  UpdateStatusResponse (..),
  controlPlaneHandler,
  -- Individual handlers (re-exported for short routes)
  getDeploymentHandler,
  putDeploymentHandler,
  getUpdateStatusHandler,
  deleteDeploymentHandler,
) where

import qualified BundleStore
import L4.FunctionSchema (Parameters (..))
import Compatibility (FnIface, ifaceFromFunction, ifaceFromSummary, detectBreakingChanges)
import Compiler (compileBundle, computeVersion)
import DeploymentLoader (triggerCompilationIfPending)
import Logging (logInfo, logWarn)
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
import Data.Maybe (fromMaybe)
import Data.Time (UTCTime, diffUTCTime, getCurrentTime)
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
  , dsUpdateId :: !(Maybe Text)
  -- ^ Set only by POST/PUT: the id of the async deploy/update job to
  -- poll via @GET \/deployments\/{id}\/updates\/{updateId}@.
  }
  deriving stock (Show, Generic)

-- | A status response with no associated update job (GET / list).
mkStatus :: Text -> Text -> Maybe DeploymentMetadata -> Maybe Text -> DeploymentStatusResponse
mkStatus i s m e = DeploymentStatusResponse i s m e Nothing

instance ToJSON DeploymentStatusResponse where
  toJSON ds = Aeson.object $
    [ "id"       .= ds.dsId
    , "status"   .= ds.dsStatus
    ] <> maybe [] (\m -> ["metadata" .= m]) ds.dsMetadata
      <> maybe [] (\e -> ["error" .= e]) ds.dsError
      <> maybe [] (\u -> ["updateId" .= u]) ds.dsUpdateId

instance FromJSON DeploymentStatusResponse where
  parseJSON = Aeson.withObject "DeploymentStatusResponse" $ \o ->
    DeploymentStatusResponse
      <$> (o Aeson..: "id"       <|> o Aeson..: "dsId")
      <*> (o Aeson..: "status"   <|> o Aeson..: "dsStatus")
      <*> (o Aeson..:? "metadata" <|> o Aeson..:? "dsMetadata")
      <*> (o Aeson..:? "error"    <|> o Aeson..:? "dsError")
      <*> (o Aeson..:? "updateId" <|> o Aeson..:? "dsUpdateId")

-- | Status of an async deploy/update job.
data UpdateStatusResponse = UpdateStatusResponse
  { usUpdateId     :: !Text
  , usDeploymentId :: !Text
  , usStatus       :: !Text   -- "compiling" | "applied" | "rejected"
  , usError        :: !(Maybe Text)
  }
  deriving stock (Show, Generic)

instance ToJSON UpdateStatusResponse where
  toJSON u = Aeson.object $
    [ "updateId"     .= u.usUpdateId
    , "deploymentId" .= u.usDeploymentId
    , "status"       .= u.usStatus
    ] <> maybe [] (\e -> ["error" .= e]) u.usError

instance FromJSON UpdateStatusResponse where
  parseJSON = Aeson.withObject "UpdateStatusResponse" $ \o ->
    UpdateStatusResponse
      <$> o Aeson..: "updateId"
      <*> o Aeson..: "deploymentId"
      <*> o Aeson..: "status"
      <*> o Aeson..:? "error"

-- | The control plane API for managing deployments.
type ControlPlaneApi =
       "deployments" :> MultipartForm Mem (MultipartData Mem) :> Verb 'POST 202 '[JSON] DeploymentStatusResponse
  :<|> "deployments" :> QueryParam "functions" Text :> QueryParam "scope" Text :> Get '[JSON] [DeploymentStatusResponse]
  :<|> "deployments" :> Capture "deploymentId" Text :> "updates" :> Capture "updateId" Text :> Get '[JSON] UpdateStatusResponse
  :<|> "deployments" :> Capture "deploymentId" Text :> QueryParam "functions" Text :> Get '[JSON] DeploymentStatusResponse
  :<|> "deployments" :> Capture "deploymentId" Text :> MultipartForm Mem (MultipartData Mem) :> Verb 'PUT 202 '[JSON] DeploymentStatusResponse
  :<|> "deployments" :> Capture "deploymentId" Text :> DeleteNoContent

-- | Combined handler for the control plane.
controlPlaneHandler :: Visibility -> ServerT ControlPlaneApi AppM
controlPlaneHandler vis =
       postDeploymentHandler
  :<|> getDeploymentsHandler vis
  :<|> getUpdateStatusHandler
  :<|> getDeploymentHandler vis
  :<|> putDeploymentHandler
  :<|> deleteDeploymentHandler

-- | POST /deployments — create or overwrite a deployment.
--
-- Ungated (no backwards-compatibility check — that is PUT's job) but
-- non-destructive: the work runs as an async job and the old version
-- (if the id already exists) keeps serving until the job applies; a
-- brand-new id simply does not exist until then. Returns 202 with an
-- @updateId@ to poll, or "ready" immediately on a content-hash dedupe.
postDeploymentHandler :: MultipartData Mem -> AppM DeploymentStatusResponse
postDeploymentHandler multipart = do
  env <- asks id

  -- Extract deployment ID (from multipart field or generate UUID)
  deployId <- case lookupInput "id" multipart of
    Right idText | not (Text.null idText) -> do
      validateDeploymentId idText
      pure (DeploymentId idText)
    _ -> liftIO $ DeploymentId . Text.pack . UUID.toString <$> nextRandom

  liftIO $ logInfo env.logger "Deployment requested"
    [("deploymentId", toJSON deployId.unDeploymentId)]

  sourceMap <- extractSourcesFromMultipart multipart
  let mDesc = extractDescription multipart
      version = computeVersion sourceMap

  registry <- liftIO $ readTVarIO env.deploymentRegistry
  let existingMatch =
        [ (did, meta)
        | (did, DeploymentReady _ meta) <- Map.toList registry
        , meta.metaVersion == version
        ]

  case existingMatch of
    ((existingId, existingMeta):_) ->
      -- Identical sources already deployed — no recompile, no job.
      pure (mkStatus existingId.unDeploymentId "ready" (Just existingMeta) Nothing)
    [] -> do
      let cfg = env.options
          isNew = not (Map.member deployId registry)
      when (isNew && Map.size registry >= cfg.maxDeployments) $
        throwError err400 { errBody = jsonError "Maximum deployment limit reached" }

      now <- liftIO getCurrentTime
      let storedMeta = BundleStore.StoredMetadata
            { BundleStore.smVersion = version
            , BundleStore.smCreatedAt = Text.pack (show now)
            , BundleStore.smDescription = mDesc
            }
      jobId <- liftIO $ newJob env deployId
      _ <- liftIO $ async $
        runDeployJob env deployId jobId sourceMap storedMeta mDesc Nothing
      pure DeploymentStatusResponse
        { dsId = deployId.unDeploymentId
        , dsStatus = "compiling"
        , dsMetadata = Nothing
        , dsError = Nothing
        , dsUpdateId = Just jobId
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
      fnMode = parseFnMode vis mFunctions

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

-- | Parse the @?functions@ query parameter, honouring the visibility gate.
-- Defaults to 'FnSimple' (name + description) when unset or unrecognised.
parseFnMode :: Visibility -> Maybe Text -> FnMode
parseFnMode vis mFunctions
  | not vis.showFunctions = FnNone
  | otherwise = case mFunctions of
      Just "full" -> FnFull
      Just "none" -> FnNone
      _           -> FnSimple

-- | Strip parameters from a FunctionSummary (for ?functions=simple).
simplify :: FunctionSummary -> FunctionSummary
simplify fs = fs
  { fsParameters = MkParameters Map.empty []
  , fsSourceFile = Nothing
  }

-- | GET /deployments/{id} — get deployment status.
-- If the deployment is Pending (lazy-load), compiles it synchronously
-- before returning the response.
-- ?functions=full → include full function details (parameters, returnType, section)
-- ?functions=none → omit functions from metadata
-- default        → simple function list (name + description)
getDeploymentHandler :: Visibility -> Text -> Maybe Text -> AppM DeploymentStatusResponse
getDeploymentHandler vis deployIdText mFunctions = do
  env <- asks id
  let deployId = DeploymentId deployIdText
      fnMode = parseFnMode vis mFunctions
  liftIO $ logInfo env.logger "Deployment retrieved"
    [("deploymentId", toJSON deployIdText), ("functions", toJSON mFunctions)]
  registry <- liftIO $ readTVarIO env.deploymentRegistry
  case Map.lookup deployId registry of
    Nothing -> throwError err404
    Just (DeploymentPending _) -> do
      triggerCompilationIfPending deployId
      -- Re-read: compilation is synchronous, state is now Ready or Failed
      registry' <- liftIO $ readTVarIO env.deploymentRegistry
      case Map.lookup deployId registry' of
        Nothing -> throwError err404
        Just state' -> pure (stateToResponse env.options.debug vis fnMode deployId state')
    Just state -> pure (stateToResponse env.options.debug vis fnMode deployId state)

-- | GET /deployments/{id}/updates/{updateId} — poll an async
-- deploy/update job. Independent of the live deployment: a job's
-- progress or failure never changes what GET /deployments/{id} reports.
getUpdateStatusHandler :: Text -> Text -> AppM UpdateStatusResponse
getUpdateStatusHandler deployIdText jobId = do
  env <- asks id
  now <- liftIO getCurrentTime
  liftIO $ atomically $ modifyTVar' env.updateJobs (pruneJobs now)
  jobs <- liftIO $ readTVarIO env.updateJobs
  case Map.lookup jobId jobs of
    Just j | j.ujDeploymentId == deployIdText ->
      pure UpdateStatusResponse
        { usUpdateId = jobId
        , usDeploymentId = j.ujDeploymentId
        , usStatus = case j.ujStatus of
            JobCompiling  -> "compiling"
            JobApplied    -> "applied"
            JobRejected _ -> "rejected"
        , usError = case j.ujStatus of
            JobRejected m -> Just m
            _             -> Nothing
        }
    _ -> throwError err404

-- | PUT /deployments/{id} — update an existing deployment.
--
-- Enforces the backwards-compatibility gate. Runs as an async job (202
-- + updateId to poll); the old version keeps serving and nothing is
-- persisted or swapped unless the new bundle compiles and is compatible.
putDeploymentHandler :: Text -> MultipartData Mem -> AppM DeploymentStatusResponse
putDeploymentHandler deployIdText multipart = do
  env <- asks id
  let deployId = DeploymentId deployIdText

  liftIO $ logInfo env.logger "Deployment update requested"
    [("deploymentId", toJSON deployIdText)]

  validateDeploymentId deployIdText

  -- Must already exist. Capture its description (so a source-only PUT
  -- preserves it) and its current interface (for the compat gate).
  registry <- liftIO $ readTVarIO env.deploymentRegistry
  (existingDesc, mOldIfaces) <- case Map.lookup deployId registry of
    Nothing -> throwError err404
    Just st -> pure (stateDescription st, currentIfaces st)

  sourceMap <- extractSourcesFromMultipart multipart
  let mDesc = extractDescription multipart <|> existingDesc

  now <- liftIO getCurrentTime
  let version = computeVersion sourceMap
      storedMeta = BundleStore.StoredMetadata
        { BundleStore.smVersion = version
        , BundleStore.smCreatedAt = Text.pack (show now)
        , BundleStore.smDescription = mDesc
        }
  jobId <- liftIO $ newJob env deployId
  _ <- liftIO $ async $
    runDeployJob env deployId jobId sourceMap storedMeta mDesc
      (Just (fromMaybe [] mOldIfaces))
  pure DeploymentStatusResponse
    { dsId = deployId.unDeploymentId
    , dsStatus = "compiling"
    , dsMetadata = Nothing
    , dsError = Nothing
    , dsUpdateId = Just jobId
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
-- Async deploy/update jobs
-- ----------------------------------------------------------------------------

-- | Retention for terminal (applied/rejected) jobs, in seconds. Long
-- enough for any client poll loop; in-flight ('JobCompiling') jobs are
-- never pruned.
jobRetentionSeconds :: Double
jobRetentionSeconds = 600

-- | Drop terminal jobs older than the retention window. Bounds the map
-- to in-flight jobs plus recently-finished ones.
pruneJobs :: UTCTime -> Map Text UpdateJob -> Map Text UpdateJob
pruneJobs now = Map.filter keep
 where
  keep j = case j.ujStatus of
    JobCompiling -> True
    _            -> realToFrac (diffUTCTime now j.ujUpdatedAt) < jobRetentionSeconds

-- | Register a fresh 'JobCompiling' job and return its id.
newJob :: AppEnv -> DeploymentId -> IO Text
newJob env deployId = do
  jobId <- Text.pack . UUID.toString <$> nextRandom
  now <- getCurrentTime
  atomically $ modifyTVar' env.updateJobs $
    pruneJobs now . Map.insert jobId (UpdateJob deployId.unDeploymentId JobCompiling now)
  pure jobId

-- | Transition a job to a terminal (or any) status.
setJob :: AppEnv -> Text -> UpdateJobStatus -> IO ()
setJob env jobId st = do
  now <- getCurrentTime
  atomically $ modifyTVar' env.updateJobs $
    pruneJobs now . Map.adjust (\j -> j { ujStatus = st, ujUpdatedAt = now }) jobId

-- | The background worker shared by POST and PUT: compile (bounded by
-- the configured time + memory limits), optionally enforce the
-- backwards-compatibility gate, and on success persist + swap the live
-- deployment. Nothing is persisted or swapped on rejection, so the old
-- version (if any) keeps serving untouched.
--
-- @mGate@: 'Just' old interfaces ⇒ enforce the compat gate (PUT);
-- 'Nothing' ⇒ ungated (POST create/overwrite).
runDeployJob
  :: AppEnv
  -> DeploymentId
  -> Text                        -- ^ job id
  -> Map FilePath Text           -- ^ sources
  -> BundleStore.StoredMetadata
  -> Maybe Text                  -- ^ description to stamp into metadata
  -> Maybe [FnIface]             -- ^ compat gate (PUT) or Nothing (POST)
  -> IO ()
runDeployJob env deployId jobId sourceMap storedMeta mDesc mGate = do
  let cfg = env.options
      compileTimeoutMicros = cfg.compileTimeout * 1_000_000
      compileMemLimitBytes = fromIntegral cfg.maxCompileMemoryMb * 1024 * 1024 :: Int64
      compileLimited = do
        setAllocationCounter compileMemLimitBytes
        enableAllocationLimit
        compileBundle env.logger deployId.unDeploymentId sourceMap
      reject msg = do
        logWarn env.logger "Deploy job rejected"
          [ ("deploymentId", toJSON deployId.unDeploymentId)
          , ("jobId", toJSON jobId)
          , ("reason", toJSON msg)
          ]
        setJob env jobId (JobRejected msg)
  mResult <- timeout compileTimeoutMicros compileLimited
    `catch` \AllocationLimitExceeded -> pure Nothing
  case mResult of
    Nothing ->
      reject "Update rejected: compilation timed out or exceeded the memory limit"
    Just (Left err) ->
      reject ("Update rejected: compilation failed: " <> err)
    Just (Right (fns, meta0, bundles)) -> do
      let newIfaces = map (ifaceFromFunction . (.fnImpl)) (Map.elems fns)
          breaking = maybe [] (`detectBreakingChanges` newIfaces) mGate
      case breaking of
        (_:_) ->
          reject $ "Update rejected — it would break existing integrations: "
                     <> Text.intercalate "; " breaking
        [] -> do
          let meta = meta0 { metaDescription = mDesc }
          BundleStore.saveBundle env.bundleStore deployId.unDeploymentId sourceMap storedMeta
          BundleStore.saveBundleCbor env.bundleStore deployId.unDeploymentId bundles
          BundleStore.saveMetadataCache env.bundleStore deployId.unDeploymentId (Aeson.encode meta)
            `catch` \(e :: SomeException) ->
              logWarn env.logger "Failed to save metadata cache (non-fatal)"
                [ ("deploymentId", toJSON deployId.unDeploymentId)
                , ("error", toJSON (displayException e))
                ]
          atomically $ modifyTVar' env.deploymentRegistry $
            Map.insert deployId (DeploymentReady fns meta)
          setJob env jobId JobApplied
          logInfo env.logger "Deploy job applied"
            [ ("deploymentId", toJSON deployId.unDeploymentId)
            , ("jobId", toJSON jobId)
            ]

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
    in mkStatus did "pending" meta Nothing
  DeploymentCompiling -> mkStatus did "compiling" Nothing Nothing
  DeploymentReady _ meta ->
    let filteredMeta = meta
          { metaFunctions = case fnMode of
              FnNone -> []
              FnSimple -> map simplify meta.metaFunctions
              FnFull -> meta.metaFunctions
          , metaFiles = if vis.showFiles then meta.metaFiles else []}
    in mkStatus did "ready" (Just filteredMeta) Nothing
  DeploymentFailed err ->
    let errorMsg = if debugMode then Just err else Just "Compilation failed"
    in mkStatus did "failed" Nothing errorMsg

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


