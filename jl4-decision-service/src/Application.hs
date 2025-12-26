{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Application (defaultMain, app) where

import Control.Concurrent.STM (newTVarIO)
import Control.Monad.Trans.Reader (ReaderT (..))
import Control.Monad (when, unless)
import qualified Examples
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Logger
import Options
import Options.Applicative as Opts
import Schema
import Servant
import Servant.Swagger.UI (SwaggerSchemaUI, swaggerSchemaUIServer)
import Server
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (takeExtension, (</>))
import qualified Data.Map as Map
import Network.Wai.Middleware.Cors (cors, simpleCorsResourcePolicy, corsMethods, corsRequestHeaders)
import Network.HTTP.Client (newManager, defaultManagerSettings)

-- ----------------------------------------------------------------------------
-- Option Parser
-- ----------------------------------------------------------------------------

opts :: ParserInfo Options
opts =
  Opts.info
    (optionsParser <**> helper)
    ( fullDesc
        <> progDesc "Serve a Web Service for interacting with the L4 evaluator"
        <> header "L4 explainable - A web server for L4"
    )

-- ----------------------------------------------------------------------------
-- Main Application and wiring
-- ----------------------------------------------------------------------------

defaultMain :: IO ()
defaultMain = do
  Options{port, serverName, sourcePaths, crudServerName} <- execParser opts

  l4Files <- expandSourcePaths sourcePaths
  when (null sourcePaths) $ putStrLn $ "sourcePaths expanded to empty: " <> show sourcePaths
  unless (null sourcePaths) $ putStrLn $ "Scanning .l4 files from: " <> show l4Files

  (l4Functions, _moduleContext) <- Examples.loadL4Functions l4Files
  unless (null sourcePaths) $ putStrLn $ "** Loaded l4 functions from disk: " <> show (length l4Functions)
  unless (null l4Functions) $ print $ Map.keys l4Functions

  exampleFunctions <- Examples.functionSpecs
  dbRef <- newTVarIO (exampleFunctions <> l4Functions)
  mgr <- newManager defaultManagerSettings
  putStrLn $ "will contact crud server on following base url: " <> show crudServerName
  let
    initialState = MkAppEnv dbRef crudServerName mgr
  putStrLn $ "Application started on port: " <> show port
  withStdoutLogger $ \aplogger -> do
    let
      settings = setPort port $ setLogger aplogger defaultSettings
    runSettings settings (corsMiddleware $ app initialState serverName)

corsMiddleware :: Middleware
corsMiddleware = cors (const $ Just simpleCorsResourcePolicy
  { corsMethods = ["GET", "POST", "PUT", "DELETE", "OPTIONS", "PATCH"]
  , corsRequestHeaders = ["content-type", "authorization"]
  })

expandSourcePaths :: [FilePath] -> IO [FilePath]
expandSourcePaths paths = do
  files <- concat <$> mapM expandPath paths
  return $ filter (\f -> takeExtension f == ".l4") files

expandPath :: FilePath -> IO [FilePath]
expandPath path = do
  isDir <- doesDirectoryExist path
  if isDir
    then do
      contents <- listDirectory path
      concat <$> mapM (expandPath . (path </>)) contents
    else return [path]

type ApiWithSwagger =
  SwaggerSchemaUI "swagger-ui" "swagger.json"
    :<|> Api

appWithSwagger :: AppEnv -> Maybe ServerName -> Servant.Server ApiWithSwagger
appWithSwagger initialDb mServerName =
  swaggerSchemaUIServer (serverOpenApi mServerName)
    :<|> hoistServer (Proxy @Api) (nt initialDb) handler
 where
  nt :: AppEnv -> AppM a -> Handler a
  nt s x = runReaderT x s

app :: AppEnv -> Maybe ServerName -> Application
app initialDb mServerName = do
  serve (Proxy @ApiWithSwagger) (appWithSwagger initialDb mServerName)
