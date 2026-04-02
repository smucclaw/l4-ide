-- | Shared deployment loading and compilation logic.
--
-- Used by Application (eager/lazy startup) and DataPlane (compile-on-first-request).
module DeploymentLoader (
  loadAndRegister,
  triggerCompilationIfPending,
) where

import qualified BundleStore
import BundleStore (BundleStore)
import Compiler (compileBundle, buildFromCborBundle)
import Logging (Logger, logInfo, logWarn, logError, logDebug)
import Options (Options (..))
import Types

import Control.Concurrent.STM (TVar, atomically, modifyTVar', readTVar)
import Control.Exception (SomeException, catch, displayException)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (asks)
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
