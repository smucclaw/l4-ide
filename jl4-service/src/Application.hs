{-# LANGUAGE DataKinds #-}

module Application (defaultMain, app) where

import qualified BundleStore
import ControlPlane (ControlPlaneApi, controlPlaneHandler)
import DataPlane (DataPlaneApi, dataPlaneHandler, ShortRoutes, shortRoutesHandler)
import DeploymentLoader (loadAndRegister)
import Logging (Logger, logInfo, logDebug, logError, newLogger)
import Options (Options (..), buildOpts)
import Types

import Data.Aeson (toJSON, (.=))
import qualified Data.Aeson as Aeson
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding
import Control.Concurrent.Async (mapConcurrently_)
import Control.Concurrent.STM (atomically, modifyTVar', newTVarIO, readTVarIO)
import Control.Exception (SomeException, finally, displayException)
import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT (..), ask)
import Data.IORef (newIORef, atomicModifyIORef')
import qualified Data.Map.Strict as Map
import Data.Time (getCurrentTime, diffUTCTime)
import Network.HTTP.Types.Status (statusCode)
import Network.Wai (Middleware, Request, requestMethod, rawPathInfo, responseStatus, pathInfo, responseLBS, requestHeaders)
import Network.HTTP.Types (status503, mkStatus, ok200)
import qualified Data.ByteString as BS
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setHost, setPort, setOnException, setOnExceptionResponse)
import Network.Wai.Handler.Warp (defaultShouldDisplayException)
import Network.Wai.Middleware.Cors (cors, simpleCorsResourcePolicy, corsMethods, corsRequestHeaders)
import Options.Applicative (execParser)
import Servant

import WebMCPPage (RawJs, JavaScript, renderExplorerPageBS, renderOrgWebMCPScript)

-- | Combined service API.
type ServiceApi = HealthApi :<|> WellKnownApi :<|> OrgOpenApiRoute :<|> WebMCPApi :<|> ControlPlaneApi :<|> DataPlaneApi :<|> ShortRoutes

-- | Health check endpoint.
type HealthApi = "health" :> Get '[JSON] HealthResponse

-- | .well-known/webmcp discovery manifest.
type WellKnownApi = ".well-known" :> "webmcp" :> Get '[JSON] Aeson.Value

-- | Org-wide OpenAPI metadata (all deployments, optionally filtered by scope).
type OrgOpenApiRoute = "openapi.json" :> QueryParam "scope" Text :> Get '[JSON] Aeson.Value

-- | Org-wide WebMCP script endpoint.
type WebMCPApi = "webmcp.js" :> Get '[JavaScript] RawJs

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
    , ("maxCompileMemoryMb", toJSON options.maxCompileMemoryMb)
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
  let middleware = concLimiter . requestLogMiddleware logger . corsMiddleware . explorerMiddleware
      onExc :: Maybe Request -> SomeException -> IO ()
      onExc _req exc =
        if defaultShouldDisplayException exc || debug
        then logError logger "Unhandled exception"
               [("error", toJSON (displayException exc))]
        else pure ()
      onExcResponse _exc =
               responseLBS (mkStatus 500 "Internal Server Error")
                      [("Content-Type", "application/json")]
                      "{\"error\":\"Internal server error\"}"
      settings = setHost "*"
               $ setPort port
               $ setOnException onExc
               $ setOnExceptionResponse onExcResponse
               $ defaultSettings

  logInfo logger "Server ready"
    [("port", toJSON port)]
  runSettings settings (middleware $ app env)

-- | CORS middleware — same policy as jl4-decision-service.
corsMiddleware :: Middleware
corsMiddleware = cors (const $ Just simpleCorsResourcePolicy
  { corsMethods = ["GET", "POST", "PUT", "DELETE", "OPTIONS", "PATCH"]
  , corsRequestHeaders = ["content-type", "authorization"]
  })

-- | Structured request logging middleware (debug level only).
requestLogMiddleware :: Logger -> Middleware
requestLogMiddleware logger baseApp req sendResp = do
  start <- getCurrentTime
  baseApp req $ \res -> do
    elapsed <- diffUTCTime <$> getCurrentTime <*> pure start
    let durationMs = realToFrac elapsed * 1000 :: Double
        fields =
          [ ("method", toJSON (Text.Encoding.decodeUtf8 (requestMethod req)))
          , ("path", toJSON (Text.Encoding.decodeUtf8 (rawPathInfo req)))
          , ("status", toJSON (statusCode (responseStatus res)))
          , ("duration_ms", toJSON durationMs)
          ]
    logDebug logger "http_request" fields
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
  hoistServer (Proxy @ServiceApi) (nt env) (healthHandler :<|> wellKnownHandler :<|> orgOpenApiHandler :<|> webmcpHandler :<|> controlPlaneHandler :<|> dataPlaneHandler :<|> shortRoutesHandler)
 where
  nt :: AppEnv -> AppM a -> Handler a
  nt s x = runReaderT x s

-- | GET /health — health check handler.
healthHandler :: ServerT HealthApi AppM
healthHandler = do
  env <- ask
  registry <- liftIO . readTVarIO $ env.deploymentRegistry
  let states = Map.elems registry
      nReady = length [() | DeploymentReady _ _ <- states]
      nPending = length [() | DeploymentPending <- states]
      nCompiling = length [() | DeploymentCompiling <- states]
      nFailed = length [() | DeploymentFailed _ <- states]
      nTotal = length states
  pure HealthResponse
    { hrStatus = "healthy"
    , hrDeployments = HealthDeploymentCounts
        { hdTotal = nTotal
        , hdReady = nReady
        , hdPending = nPending
        , hdCompiling = nCompiling
        , hdFailed = nFailed
        }
    , hrInstanceToken = env.options.instanceToken
    }

-- | GET /.well-known/webmcp — discovery manifest for WebMCP crawlers.
wellKnownHandler :: ServerT WellKnownApi AppM
wellKnownHandler = do
  env <- ask
  registry <- liftIO . readTVarIO $ env.deploymentRegistry
  let readyDeployments =
        [ Aeson.object
            [ "id" .= did.unDeploymentId
            , "functions" .= length meta.metaFunctions
            ]
        | (did, DeploymentReady _ meta) <- Map.toList registry
        ]
  pure $ Aeson.object
    [ "version" .= ("draft" :: String)
    , "script" .= ("/webmcp.js" :: String)
    , "deployments" .= readyDeployments
    ]

-- | GET /openapi.json — org-wide metadata across all deployments.
-- Optional ?scope= parameter filters by deployment/function.
-- Serves from in-memory registry for ready deployments, disk cache for pending ones.
orgOpenApiHandler :: ServerT OrgOpenApiRoute AppM
orgOpenApiHandler mScope = do
  env <- ask
  registry <- liftIO . readTVarIO $ env.deploymentRegistry
  let store = env.bundleStore

  -- Collect metadata for all deployments (ready from memory, pending from cache)
  allEntries <- liftIO $ fmap concat $ mapM (\(did, state) -> do
    mMeta <- case state of
      DeploymentReady _ meta -> pure (Just meta)
      _ -> do
        mBytes <- BundleStore.loadMetadataCache store did.unDeploymentId
        case mBytes of
          Just bytes -> case Aeson.eitherDecode bytes of
            Right meta -> pure (Just meta)
            Left _ -> pure Nothing
          Nothing -> pure Nothing
    pure $ case mMeta of
      Nothing -> []
      Just meta ->
        [ Aeson.object
          [ "deployment" .= did.unDeploymentId
          , "name" .= fn.fsName
          , "description" .= fn.fsDescription
          , "parameters" .= fn.fsParameters
          , "returnType" .= fn.fsReturnType
          , "isDeontic" .= fn.fsIsDeontic
          ]
        | fn <- meta.metaFunctions
        , matchesScope mScope did.unDeploymentId fn.fsName
        ]
    ) (Map.toList registry)

  pure $ Aeson.object
    [ "functions" .= allEntries
    ]

-- | Check if a deployment/function matches the scope filter.
matchesScope :: Maybe Text -> Text -> Text -> Bool
matchesScope Nothing _ _ = True
matchesScope (Just scope) deployId fnName =
  any matchPattern (Text.splitOn "," scope)
 where
  matchPattern pat =
    let trimmed = Text.strip pat
        (depPat, rest) = Text.breakOn "/" trimmed
        fnPat = if Text.null rest then "*" else Text.drop 1 rest
    in (depPat == "*" || depPat == deployId)
       && (fnPat == "*" || fnPat == fnName)

-- | GET /webmcp.js — org-wide WebMCP script.
webmcpHandler :: ServerT WebMCPApi AppM
webmcpHandler = pure renderOrgWebMCPScript

-- | Middleware: serve the deployment explorer page on GET / with Accept: text/html.
explorerMiddleware :: Middleware
explorerMiddleware baseApp req sendResp
  | requestMethod req == "GET"
  , rawPathInfo req == "/"
  , acceptsHtml (requestHeaders req)
  = sendResp $ responseLBS ok200
      [("Content-Type", "text/html; charset=utf-8")]
      renderExplorerPageBS
  | otherwise = baseApp req sendResp
 where
  acceptsHtml headers =
    case lookup "Accept" headers of
      Just accept -> "text/html" `BS.isInfixOf` accept
      Nothing -> False
