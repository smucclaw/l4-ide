{-# LANGUAGE DataKinds #-}

module Application (defaultMain, app) where

import qualified BundleStore
import ControlPlane (ControlPlaneApi, controlPlaneHandler)
import DataPlane (DataPlaneApi, dataPlaneHandler, ShortRoutes, shortRoutesHandler)
import McpServer (mcpHandler)
import DeploymentLoader (loadAndRegister)
import Logging (Logger, logInfo, logDebug, logError, newLogger)
import Options (Options (..), buildOpts)
import Shared (collectMetadataEntries)
import Types

import Data.Aeson (toJSON, (.=))
import qualified Data.Aeson as Aeson
import Data.Text (Text)
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

import ExplorerPage (renderExplorerPageBS)
import WebMCPPage (RawJs, JavaScript, renderOrgWebMCPScript)

-- | Combined service API.
type ServiceApi = HealthApi :<|> WellKnownApi :<|> McpDiscoveryApi :<|> McpManifestApi :<|> OrgOpenApiRoute :<|> WebMCPApi :<|> McpApi :<|> McpScopedApi :<|> McpScopedLongApi :<|> ControlPlaneApi :<|> DataPlaneApi :<|> ShortRoutes

-- | Health check endpoint.
type HealthApi = "health" :> Get '[JSON] HealthResponse

-- | .well-known/webmcp discovery manifest.
type WellKnownApi = ".well-known" :> "webmcp" :> Get '[JSON] Aeson.Value

-- | Org-wide OpenAPI metadata (all deployments, optionally filtered by scope).
type OrgOpenApiRoute = "openapi.json" :> QueryParam "scope" Text :> Get '[JSON] Aeson.Value

-- | Org-wide WebMCP script endpoint.
-- RESERVED_SEGMENTS: .webmcp is a reserved path prefix (do not allow as deployment ID).
type WebMCPApi = ".webmcp" :> "embed.js" :> Get '[JavaScript] RawJs

-- | MCP discovery endpoint (/.well-known/mcp).
-- Primary discovery endpoint per MCP spec - returns server metadata and capabilities.
type McpDiscoveryApi = ".well-known" :> "mcp" :> Get '[JSON] Aeson.Value

-- | MCP manifest endpoint (/.well-known/mcp/manifest).
-- Legacy/alternative discovery endpoint.
type McpManifestApi = ".well-known" :> "mcp" :> "manifest" :> Get '[JSON] Aeson.Value

-- | MCP JSON-RPC endpoint (org-wide, no deployment scope).
-- POST handles JSON-RPC; GET returns 405 per MCP Streamable HTTP spec.
type McpApi = ".mcp" :> (ReqBody '[JSON] Aeson.Value :> Post '[JSON] Aeson.Value :<|> Get '[JSON] Aeson.Value)

-- | MCP JSON-RPC endpoint scoped to a deployment (short route).
type McpScopedApi = Capture "deploymentId" Text :> ".mcp" :> (ReqBody '[JSON] Aeson.Value :> Post '[JSON] Aeson.Value :<|> Get '[JSON] Aeson.Value)

-- | MCP JSON-RPC endpoint scoped to a deployment (long route).
type McpScopedLongApi = "deployments" :> Capture "deploymentId" Text :> ".mcp" :> (ReqBody '[JSON] Aeson.Value :> Post '[JSON] Aeson.Value :<|> Get '[JSON] Aeson.Value)

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
  BundleStore.cleanupStore logger store
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
  let middleware = concLimiter . requestLogMiddleware logger . corsMiddleware . explorerMiddleware logger
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
  hoistServer (Proxy @ServiceApi) (nt env) (healthHandler :<|> wellKnownHandler :<|> mcpDiscoveryHandler :<|> mcpManifestHandler :<|> orgOpenApiHandler :<|> webmcpHandler :<|> mcpRootHandler :<|> mcpScopedHandler :<|> mcpScopedLongHandler :<|> controlPlaneHandler :<|> dataPlaneHandler :<|> shortRoutesHandler)
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
  liftIO $ logInfo env.logger "WebMCP manifest served" []
  pure $ Aeson.object
    [ "version" .= ("draft" :: String)
    , "script" .= ("/.webmcp/embed.js" :: String)
    , "deployments" .= readyDeployments
    ]

-- | GET /openapi.json — org-wide metadata across all deployments.
-- Optional ?scope= parameter filters by deployment/function.
-- Serves from in-memory registry for ready deployments, disk cache for pending ones.
orgOpenApiHandler :: ServerT OrgOpenApiRoute AppM
orgOpenApiHandler mScope = do
  env <- ask
  liftIO $ logInfo env.logger "OpenAPI schema requested"
    [("scope", toJSON mScope)]

  entries <- collectMetadataEntries mScope
  let allEntries =
        [ Aeson.object
          [ "deployment" .= deployId
          , "name" .= fn.fsName
          , "description" .= fn.fsDescription
          , "parameters" .= fn.fsParameters
          , "returnType" .= fn.fsReturnType
          , "isDeontic" .= fn.fsIsDeontic
          ]
        | (deployId, fn) <- entries
        ]

  pure $ Aeson.object
    [ "functions" .= allEntries
    ]

-- | GET /.webmcp/embed.js — org-wide WebMCP script.
webmcpHandler :: ServerT WebMCPApi AppM
webmcpHandler = do
  env <- ask
  liftIO $ logInfo env.logger "WebMCP script served" []
  pure renderOrgWebMCPScript

-- | GET /.well-known/mcp — MCP discovery endpoint.
-- Returns server metadata, capabilities, and endpoint information per MCP spec.
mcpDiscoveryHandler :: ServerT McpDiscoveryApi AppM
mcpDiscoveryHandler = do
  env <- ask
  liftIO $ logInfo env.logger "MCP discovery endpoint accessed" []
  pure $ Aeson.object
    [ "name" .= ("L4 Rules Engine" :: Text)
    , "version" .= ("1.0.0" :: Text)
    , "protocol_version" .= ("2025-03-26" :: Text)
    , "capabilities" .= Aeson.object
        [ "tools" .= Aeson.object []
        ]
    , "endpoints" .= Aeson.object
        [ "mcp" .= ("/.mcp" :: Text)
        , "manifest" .= ("/.well-known/mcp/manifest" :: Text)
        ]
    ]

-- | GET /.well-known/mcp/manifest — MCP manifest (legacy/alternative).
mcpManifestHandler :: ServerT McpManifestApi AppM
mcpManifestHandler = do
  pure $ Aeson.object
    [ "version" .= ("2025-03-26" :: Text)
    , "capabilities" .= Aeson.object [ "tools" .= True ]
    , "endpoints" .= Aeson.object [ "mcp" .= ("/.mcp" :: Text) ]
    ]

-- | POST /.mcp — org-wide MCP JSON-RPC endpoint (no deployment scope).
-- GET /.mcp returns 405 per MCP Streamable HTTP spec (POST-only).
mcpRootHandler :: ServerT McpApi AppM
mcpRootHandler = mcpHandler Nothing :<|> throwError err405

-- | POST /{deploymentId}/.mcp — deployment-scoped MCP JSON-RPC endpoint.
mcpScopedHandler :: ServerT McpScopedApi AppM
mcpScopedHandler deployIdText = mcpHandler (Just deployIdText) :<|> throwError err405

-- | POST /deployments/{deploymentId}/.mcp — deployment-scoped MCP JSON-RPC endpoint (long route).
mcpScopedLongHandler :: ServerT McpScopedLongApi AppM
mcpScopedLongHandler deployIdText = mcpHandler (Just deployIdText) :<|> throwError err405

-- | Middleware: serve the deployment explorer page on GET / with Accept: text/html.
explorerMiddleware :: Logger -> Middleware
explorerMiddleware logger baseApp req sendResp
  | requestMethod req == "GET"
  , rawPathInfo req == "/"
  , acceptsHtml (requestHeaders req)
  = do
      logInfo logger "Explorer page served" []
      sendResp $ responseLBS ok200
        [("Content-Type", "text/html; charset=utf-8")]
        renderExplorerPageBS
  | otherwise = baseApp req sendResp
 where
  acceptsHtml headers =
    case lookup "Accept" headers of
      Just accept -> "text/html" `BS.isInfixOf` accept
      Nothing -> False
