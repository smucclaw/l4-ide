{-# LANGUAGE DataKinds #-}

module Application (defaultMain, app) where

import BundleStore (BundleStore (..))
import qualified BundleStore
import Compiler (compileBundle, buildFromCborBundle)
import ControlPlane (ControlPlaneApi, controlPlaneHandler)
import DataPlane (DataPlaneApi, dataPlaneHandler)
import Logging (Logger, logInfo, logWarn, logError, logDebug, newLogger)
import Options (Options (..), buildOpts)
import Types

import Data.Aeson (toJSON)
import Data.Text (Text)
import qualified Data.Text.Encoding as Text.Encoding
import Control.Concurrent.Async (mapConcurrently_)
import Control.Concurrent.STM (TVar, atomically, modifyTVar', newTVarIO, readTVarIO)
import Control.Exception (finally)
import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT (..), asks)
import Data.IORef (newIORef, atomicModifyIORef')
import qualified Data.Map.Strict as Map
import Data.Time (getCurrentTime, diffUTCTime)
import Network.HTTP.Types.Status (statusCode)
import Network.Wai (Middleware, requestMethod, rawPathInfo, responseStatus, pathInfo, responseLBS)
import Network.HTTP.Types (status503)
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setHost, setPort)
import Network.Wai.Middleware.Cors (cors, simpleCorsResourcePolicy, corsMethods, corsRequestHeaders)
import Options.Applicative (execParser)
import Servant
import System.Timeout (timeout)

-- | Combined service API.
type ServiceApi = HealthApi :<|> ControlPlaneApi :<|> DataPlaneApi

-- | Health check endpoint.
type HealthApi = "health" :> Get '[JSON] HealthResponse

-- | Main entry point.
defaultMain :: IO ()
defaultMain = do
  optsInfo <- buildOpts
  options@Options{port, storePath, lazyLoad, debug} <- execParser optsInfo

  logger <- newLogger debug

  -- Log effective configuration
  logInfo logger "Starting jl4-service"
    [ ("port", toJSON port)
    , ("storePath", toJSON storePath)
    , ("debug", toJSON debug)
    , ("lazyLoad", toJSON lazyLoad)
    , ("maxZipSize", toJSON options.maxZipSize)
    , ("maxFileCount", toJSON options.maxFileCount)
    , ("maxDeployments", toJSON options.maxDeployments)
    , ("maxConcurrentRequests", toJSON options.maxConcurrentRequests)
    , ("maxEvalMemoryMb", toJSON options.maxEvalMemoryMb)
    , ("evalTimeout", toJSON options.evalTimeout)
    , ("compileTimeout", toJSON options.compileTimeout)
    ]

  logInfo logger "Initializing bundle store"
    [("path", toJSON storePath)]
  store <- BundleStore.initStore storePath
  registry <- newTVarIO Map.empty
  let env = MkAppEnv registry store options.serverName logger options

  -- Scan existing deployments and register them
  deployIds <- BundleStore.listDeployments store
  logInfo logger "Found existing deployments"
    [("count", toJSON (length deployIds))]

  if lazyLoad
    then do
      -- Lazy mode: register all as Pending, compile on first request
      forM_ deployIds $ \did ->
        atomically $ modifyTVar' registry $
          Map.insert (DeploymentId did) DeploymentPending
      logInfo logger "Lazy loading enabled" []
    else do
      -- Eager mode: compile all in parallel
      logInfo logger "Compiling existing deployments" []
      mapConcurrently_ (loadAndRegister logger options registry store) deployIds

  -- Build middleware stack
  concLimiter <- concurrencyLimiter options.maxConcurrentRequests
  let middleware = concLimiter . requestLogMiddleware logger . corsMiddleware
      settings = setHost "*" $ setPort port defaultSettings

  logInfo logger "Server ready"
    [("port", toJSON port)]
  runSettings settings (middleware $ app env)

-- | Load a deployment from the store and register it.
-- Tries the fast CBOR cache path first; falls back to full recompilation.
loadAndRegister :: Logger -> Options -> TVar (Map.Map DeploymentId DeploymentState) -> BundleStore -> Text -> IO ()
loadAndRegister logger options registry store deployId = do
  -- Load sources and metadata (always needed)
  (sources, storedMeta) <- BundleStore.loadBundle store deployId

  let compileTimeoutMicros = options.compileTimeout * 1_000_000

  -- Try fast path: load from CBOR cache
  mCbor <- BundleStore.loadBundleCbor logger store deployId
  result <- case mCbor of
    Just bundle -> do
      logDebug logger "Loading deployment from CBOR cache"
        [("deploymentId", toJSON deployId)]
      cborResult <- buildFromCborBundle logger bundle sources storedMeta
      case cborResult of
        Right ok -> pure (Right ok)
        Left err -> do
          logWarn logger "CBOR rebuild failed, recompiling from source"
            [ ("deploymentId", toJSON deployId)
            , ("error", toJSON err)
            ]
          compileFreshAndCache logger compileTimeoutMicros store deployId sources
    Nothing -> do
      logDebug logger "Compiling deployment"
        [("deploymentId", toJSON deployId)]
      compileFreshAndCache logger compileTimeoutMicros store deployId sources

  case result of
    Right (fns, meta) -> do
      atomically $ modifyTVar' registry $
        Map.insert (DeploymentId deployId) (DeploymentReady fns meta)
      logInfo logger "Deployment ready"
        [("deploymentId", toJSON deployId)]
    Left err -> do
      atomically $ modifyTVar' registry $
        Map.insert (DeploymentId deployId) (DeploymentFailed err)
      logError logger "Deployment failed"
        [ ("deploymentId", toJSON deployId)
        , ("error", toJSON err)
        ]

-- | Compile from source with timeout, and save CBOR cache for next restart.
compileFreshAndCache
  :: Logger
  -> Int  -- ^ timeout in microseconds
  -> BundleStore
  -> Text
  -> Map.Map FilePath Text
  -> IO (Either Text (Map.Map Text ValidatedFunction, DeploymentMetadata))
compileFreshAndCache logger timeoutMicros store deployId sources = do
  mResult <- timeout timeoutMicros $ compileBundle logger sources
  case mResult of
    Nothing -> do
      logError logger "Compilation timed out"
        [("deploymentId", toJSON deployId)]
      pure $ Left "Compilation timed out"
    Just (Right (fns, meta, bundles)) -> do
      -- Save CBOR caches for fast restart
      mapM_ (BundleStore.saveBundleCbor store deployId) bundles
      pure $ Right (fns, meta)
    Just (Left err) ->
      pure $ Left err

-- | CORS middleware — same policy as jl4-decision-service.
corsMiddleware :: Middleware
corsMiddleware = cors (const $ Just simpleCorsResourcePolicy
  { corsMethods = ["GET", "POST", "PUT", "DELETE", "OPTIONS", "PATCH"]
  , corsRequestHeaders = ["content-type", "authorization"]
  })

-- | Structured request logging middleware.
requestLogMiddleware :: Logger -> Middleware
requestLogMiddleware logger baseApp req sendResp = do
  start <- getCurrentTime
  baseApp req $ \res -> do
    elapsed <- diffUTCTime <$> getCurrentTime <*> pure start
    let durationMs = realToFrac elapsed * 1000 :: Double
    logInfo logger "http_request"
      [ ("method", toJSON (Text.Encoding.decodeUtf8 (requestMethod req)))
      , ("path", toJSON (Text.Encoding.decodeUtf8 (rawPathInfo req)))
      , ("status", toJSON (statusCode (responseStatus res)))
      , ("duration_ms", toJSON durationMs)
      ]
    sendResp res

-- | Concurrency limiter middleware.
-- Returns 503 immediately when max concurrent requests is reached.
-- Health endpoint is exempt from the limit.
concurrencyLimiter :: Int -> IO Middleware
concurrencyLimiter maxConcurrent = do
  counter <- newIORef (0 :: Int)
  pure $ \baseApp req sendResp ->
    -- Exempt health endpoint from concurrency limit
    case pathInfo req of
      ["health"] -> baseApp req sendResp
      _ -> do
        acquired <- atomicModifyIORef' counter $ \n ->
          if n >= maxConcurrent
            then (n, False)
            else (n + 1, True)
        if acquired
          then baseApp req sendResp `finally`
                 atomicModifyIORef' counter (\n -> (n - 1, ()))
          else sendResp $ responseLBS status503 [] "Service at capacity"

-- | WAI Application.
app :: AppEnv -> Application
app env = serve (Proxy @ServiceApi) (serverT env)

serverT :: AppEnv -> Server ServiceApi
serverT env =
  hoistServer (Proxy @ServiceApi) (nt env) (healthHandler :<|> controlPlaneHandler :<|> dataPlaneHandler)
 where
  nt :: AppEnv -> AppM a -> Handler a
  nt s x = runReaderT x s

-- | GET /health — health check handler.
healthHandler :: ServerT HealthApi AppM
healthHandler = do
  registry <- asks (.deploymentRegistry) >>= liftIO . readTVarIO
  let states = Map.elems registry
      nReady = length [() | DeploymentReady _ _ <- states]
      nCompiling = length [() | DeploymentPending <- states]
      nFailed = length [() | DeploymentFailed _ <- states]
      nTotal = length states
  pure HealthResponse
    { hrStatus = "healthy"
    , hrDeployments = HealthDeploymentCounts
        { hdTotal = nTotal
        , hdReady = nReady
        , hdCompiling = nCompiling
        , hdFailed = nFailed
        }
    }
