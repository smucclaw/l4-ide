-- | Shared deployment loading and compilation logic.
--
-- Used by Application (eager/lazy startup) and DataPlane (compile-on-first-request).
module DeploymentLoader (
  loadAndRegister,
  triggerCompilationIfPending,
  tryCompileWithTimeout,
  CompilationResult (..),
) where

import qualified BundleStore
import BundleStore (BundleStore)
import Compiler (compileBundle, buildFromCborBundle)
import Logging (Logger, logInfo, logWarn, logError, logDebug)
import Options (Options (..))
import Types

import Control.Concurrent.Async (async)
import Control.Concurrent.STM (TVar, atomically, modifyTVar', readTVar, retry)
import Control.Exception (SomeException, catch, displayException)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (asks, ask)
import qualified Data.Aeson as Aeson
import Data.Aeson (toJSON)
import Data.Int (Int64)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import GHC.Conc (setAllocationCounter, enableAllocationLimit)
import GHC.IO.Exception (AllocationLimitExceeded (..))
import System.Timeout (timeout)

-- | Load a deployment from the store and register it.
-- Tries the fast CBOR cache path first; falls back to full recompilation.
loadAndRegister :: Logger -> Options -> TVar (Map.Map DeploymentId DeploymentState) -> BundleStore -> Text -> IO ()
loadAndRegister logger options registry store deployId = do
  -- Load sources and metadata (always needed)
  (sources, storedMeta) <- BundleStore.loadBundle store deployId

  let compileTimeoutMicros = options.compileTimeout * 1_000_000
      compileMemLimitMb = options.maxCompileMemoryMb

  -- Try fast path: load from CBOR cache
  mCbor <- BundleStore.loadBundleCbor logger store deployId
  result <- case mCbor of
    Just bundles -> do
      logDebug logger "Loading deployment from CBOR cache"
        [("deploymentId", toJSON deployId)]
      cborResult <- buildFromCborBundle logger deployId bundles sources storedMeta
      case cborResult of
        Right ok@(fns, _meta)
          | Map.null fns -> do
              logWarn logger "CBOR rebuild produced no functions, deleting cache and recompiling"
                [("deploymentId", toJSON deployId)]
              BundleStore.deleteBundleCbor store deployId
              compileFreshAndCache logger compileTimeoutMicros compileMemLimitMb store deployId sources
          | otherwise -> pure (Right ok)
        Left err -> do
          logWarn logger "CBOR rebuild failed, deleting cache and recompiling from source"
            [ ("deploymentId", toJSON deployId)
            , ("error", toJSON err)
            ]
          BundleStore.deleteBundleCbor store deployId
          compileFreshAndCache logger compileTimeoutMicros compileMemLimitMb store deployId sources
    Nothing -> do
      logDebug logger "Compiling deployment"
        [("deploymentId", toJSON deployId)]
      compileFreshAndCache logger compileTimeoutMicros compileMemLimitMb store deployId sources

  case result of
    Right (fns, meta) -> do
      atomically $ modifyTVar' registry $
        Map.insert (DeploymentId deployId) (DeploymentReady fns meta)
      -- Cache the full metadata to disk so it survives restarts.
      -- This is optional — a write failure must not fail the deployment.
      BundleStore.saveMetadataCache store deployId (Aeson.encode meta)
        `catch` \(e :: SomeException) ->
          logWarn logger "Failed to save metadata cache (non-fatal)"
            [ ("deploymentId", toJSON deployId)
            , ("error", toJSON (displayException e))
            ]
      logInfo logger "Deployment ready"
        [("deploymentId", toJSON deployId)]
    Left err -> do
      atomically $ modifyTVar' registry $
        Map.insert (DeploymentId deployId) (DeploymentFailed err)
      logError logger "Deployment failed"
        [ ("deploymentId", toJSON deployId)
        , ("error", toJSON err)
        ]

-- | If the deployment is in Pending state, compile it synchronously.
-- Atomically transitions Pending → Compiling, then blocks until compilation
-- finishes (updating to Ready or Failed). This makes the first request to a
-- lazy-loaded deployment slower but avoids requiring the client to poll.
-- Concurrent callers that arrive while compilation is already in progress
-- will see DeploymentCompiling and are not affected.
triggerCompilationIfPending :: DeploymentId -> AppM ()
triggerCompilationIfPending deployId = do
  env <- asks id
  wasPending <- liftIO $ atomically $ do
    registry <- readTVar env.deploymentRegistry
    case Map.lookup deployId registry of
      Just (DeploymentPending _) -> do
        modifyTVar' env.deploymentRegistry $
          Map.insert deployId DeploymentCompiling
        pure True
      _ -> pure False
  liftIO $ case wasPending of
    True -> do
      let DeploymentId did = deployId
      loadAndRegister env.logger env.options env.deploymentRegistry env.bundleStore did
        `catch` \(e :: SomeException) -> do
          logError env.logger "Compilation threw an exception"
            [ ("deploymentId", toJSON did)
            , ("error", toJSON (displayException e))
            ]
          atomically $ modifyTVar' env.deploymentRegistry $
            Map.insert deployId (DeploymentFailed "Compilation failed unexpectedly")
    False -> pure ()

-- | Result of an optimistic compilation attempt.
data CompilationResult
  = CompilationReady !(Map.Map Text ValidatedFunction) !DeploymentMetadata
  -- ^ Compilation finished in time (or was already ready).
  | CompilationInProgress
  -- ^ Compilation started but did not finish within the timeout.
  -- The compilation continues in the background; the client should retry.
  | CompilationError !Text
  -- ^ Compilation failed or the deployment was not found.

-- | Try to compile a deployment within a timeout (microseconds).
-- If already Ready, returns immediately. If Pending, starts background
-- compilation and waits up to the timeout. If Compiling (by another request),
-- waits up to the timeout for it to finish. Returns 'CompilationInProgress'
-- if the timeout expires before compilation completes.
tryCompileWithTimeout :: DeploymentId -> Int -> AppM CompilationResult
tryCompileWithTimeout deployId timeoutMicros = do
  env <- ask
  let registryTVar = env.deploymentRegistry
  state <- liftIO $ atomically $ Map.lookup deployId <$> readTVar registryTVar
  case state of
    Just (DeploymentReady fns meta) -> pure (CompilationReady fns meta)
    Just (DeploymentFailed err) -> pure (CompilationError err)
    Nothing -> pure (CompilationError "Deployment not found")
    Just (DeploymentPending _) -> do
      -- Atomically transition Pending → Compiling, or detect another thread did it
      wasPending <- liftIO $ atomically $ do
        reg <- readTVar registryTVar
        case Map.lookup deployId reg of
          Just (DeploymentPending _) -> do
            modifyTVar' registryTVar $ Map.insert deployId DeploymentCompiling
            pure True
          _ -> pure False
      if wasPending
        then do
          -- We won the race: start compilation in the background
          let DeploymentId did = deployId
          _ <- liftIO $ async $
            loadAndRegister env.logger env.options registryTVar env.bundleStore did
              `catch` \(e :: SomeException) -> do
                logError env.logger "Compilation threw an exception"
                  [ ("deploymentId", toJSON did)
                  , ("error", toJSON (displayException e))
                  ]
                atomically $ modifyTVar' registryTVar $
                  Map.insert deployId (DeploymentFailed "Compilation failed unexpectedly")
          waitForResult registryTVar deployId timeoutMicros
        else
          -- Another thread transitioned it; wait for the result
          waitForResult registryTVar deployId timeoutMicros
    Just DeploymentCompiling ->
      waitForResult registryTVar deployId timeoutMicros

-- | Wait for a deployment to leave 'DeploymentCompiling' state, with a timeout.
-- Uses STM retry to block efficiently until the TVar changes.
waitForResult :: TVar (Map.Map DeploymentId DeploymentState) -> DeploymentId -> Int -> AppM CompilationResult
waitForResult registryTVar deployId timeoutMicros = do
  mResult <- liftIO $ timeout timeoutMicros $ atomically $ do
    reg <- readTVar registryTVar
    case Map.lookup deployId reg of
      Just DeploymentCompiling -> retry  -- block until state changes
      Just (DeploymentReady fns meta) -> pure (CompilationReady fns meta)
      Just (DeploymentFailed err) -> pure (CompilationError err)
      _ -> pure (CompilationError "Deployment disappeared during compilation")
  pure $ case mResult of
    Nothing -> CompilationInProgress
    Just r  -> r

-- | Compile from source with timeout and memory limit, and save CBOR cache for next restart.
compileFreshAndCache
  :: Logger
  -> Int  -- ^ timeout in microseconds
  -> Int  -- ^ memory limit in MB
  -> BundleStore
  -> Text
  -> Map.Map FilePath Text
  -> IO (Either Text (Map.Map Text ValidatedFunction, DeploymentMetadata))
compileFreshAndCache logger timeoutMicros memLimitMb store deployId sources = do
  let memLimitBytes = fromIntegral memLimitMb * 1024 * 1024 :: Int64
      compileLimited = do
        setAllocationCounter memLimitBytes
        enableAllocationLimit
        compileBundle logger deployId sources
  mResult <- (timeout timeoutMicros compileLimited)
    `catch` \AllocationLimitExceeded -> do
      logError logger "Compilation exceeded memory limit"
        [ ("deploymentId", toJSON deployId)
        , ("maxCompileMemoryMb", toJSON memLimitMb)
        ]
      pure Nothing
  case mResult of
    Nothing -> do
      logError logger "Compilation timed out or exceeded memory limit"
        [("deploymentId", toJSON deployId)]
      pure $ Left "Compilation timed out or exceeded memory limit"
    Just (Right (fns, meta, bundles)) -> do
      -- Save CBOR caches for fast restart
      BundleStore.saveBundleCbor store deployId bundles
      pure $ Right (fns, meta)
    Just (Left err) ->
      pure $ Left err
