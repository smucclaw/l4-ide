{-# LANGUAGE DataKinds #-}

module Application (defaultMain, app) where

import BundleStore (BundleStore (..))
import qualified BundleStore
import Compiler (compileBundle)
import ControlPlane (ControlPlaneApi, controlPlaneHandler)
import DataPlane (DataPlaneApi, dataPlaneHandler)
import Options (Options (..), opts)
import Types (AppEnv (..), AppM, DeploymentId (..), DeploymentState (..))

import Data.Text (Text)
import Control.Concurrent.Async (mapConcurrently_)
import Control.Concurrent.STM (TVar, atomically, modifyTVar', newTVarIO)
import Control.Monad (forM_)
import Control.Monad.Trans.Reader (ReaderT (..))
import qualified Data.Map.Strict as Map
import Network.Wai (Middleware)
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setHost, setPort, setLogger)
import Network.Wai.Logger (withStdoutLogger)
import Network.Wai.Middleware.Cors (cors, simpleCorsResourcePolicy, corsMethods, corsRequestHeaders)
import Options.Applicative (execParser)
import Servant

-- | Combined service API.
type ServiceApi = ControlPlaneApi :<|> DataPlaneApi

-- | Main entry point.
defaultMain :: IO ()
defaultMain = do
  Options{port, storePath, serverName, lazyLoad} <- execParser opts

  putStrLn $ "Initializing bundle store at: " <> storePath
  store <- BundleStore.initStore storePath
  registry <- newTVarIO Map.empty
  let env = MkAppEnv registry store serverName

  -- Scan existing deployments and register them
  deployIds <- BundleStore.listDeployments store
  putStrLn $ "Found " <> show (length deployIds) <> " existing deployment(s)"

  if lazyLoad
    then do
      -- Lazy mode: register all as Pending, compile on first request
      forM_ deployIds $ \did ->
        atomically $ modifyTVar' registry $
          Map.insert (DeploymentId did) DeploymentPending
      putStrLn "Lazy loading enabled: deployments will compile on first request"
    else do
      -- Eager mode: compile all in parallel
      putStrLn "Compiling existing deployments..."
      mapConcurrently_ (loadAndRegister registry store) deployIds

  putStrLn $ "Starting jl4-service on port " <> show port
  withStdoutLogger $ \logger -> do
    let settings = setHost "*" $ setPort port $ setLogger logger defaultSettings
    runSettings settings (corsMiddleware $ app env)

-- | Load a deployment from the store and register it.
loadAndRegister :: TVar (Map.Map DeploymentId DeploymentState) -> BundleStore -> Text -> IO ()
loadAndRegister registry store deployId = do
  putStrLn $ "  Compiling deployment: " <> show deployId
  result <- do
    (sources, _meta) <- BundleStore.loadBundle store deployId
    compileBundle sources
  case result of
    Right (fns, meta) -> do
      atomically $ modifyTVar' registry $
        Map.insert (DeploymentId deployId) (DeploymentReady fns meta)
      putStrLn $ "  Ready: " <> show deployId
    Left err -> do
      atomically $ modifyTVar' registry $
        Map.insert (DeploymentId deployId) (DeploymentFailed err)
      putStrLn $ "  FAILED: " <> show deployId <> " — " <> show err

-- | CORS middleware — same policy as jl4-decision-service.
corsMiddleware :: Middleware
corsMiddleware = cors (const $ Just simpleCorsResourcePolicy
  { corsMethods = ["GET", "POST", "PUT", "DELETE", "OPTIONS", "PATCH"]
  , corsRequestHeaders = ["content-type", "authorization"]
  })

-- | WAI Application.
app :: AppEnv -> Application
app env = serve (Proxy @ServiceApi) (serverT env)

serverT :: AppEnv -> Server ServiceApi
serverT env =
  hoistServer (Proxy @ServiceApi) (nt env) (controlPlaneHandler :<|> dataPlaneHandler)
 where
  nt :: AppEnv -> AppM a -> Handler a
  nt s x = runReaderT x s
