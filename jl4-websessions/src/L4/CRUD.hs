{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}
module L4.CRUD (mkApp, withEnv, createDB, Api (..), HandlerEnv (..), JL4Program (..)) where

import GHC.Exts (withDict)
import GHC.Generics
import System.Environment
import Text.Read

import Control.Exception (catch)
import Control.Monad (unless, void)
import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson (FromJSON, ToJSON, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as Text
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import qualified Database.SQLite.Simple as SQLite
import Database.SQLite.Simple.QQ (sql)
import qualified Database.SQLite.Simple.ToField as SQLite
import qualified Database.SQLite.Simple.ToRow as SQLite
import System.Directory
import Network.Wai.Logger (ApacheLogger, withStdoutLogger)
import qualified Network.HTTP.Client as HTTP

import Servant
import Servant.Server.Generic

data JL4Program
  = MkJL4Program
  { sessionid :: UUID
  , jl4program :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance SQLite.ToRow JL4Program where
  toRow = withToFieldUUID $ SQLite.gtoRow . from

mkApp :: HandlerEnv -> Application
mkApp env = genericServeT (runHandlerM env) server

withEnv :: (Int -> ApacheLogger -> HandlerEnv -> IO r) -> IO r
withEnv k = do
  args <- getArgs
  case args of
    [readMaybe -> Just port, dbPath] -> do
      -- No decision service URL provided - run without push
      createDB dbPath
      mgr <- HTTP.newManager HTTP.defaultManagerSettings
      SQLite.withConnection dbPath \dbConn ->
        withStdoutLogger \logger -> do
          let env = MkHandlerEnv{dbConn, httpManager = mgr, decisionServiceUrl = Nothing}
          k port logger env
    [readMaybe -> Just port, dbPath, dsUrl] -> do
      -- Decision service URL provided - push on save
      createDB dbPath
      mgr <- HTTP.newManager HTTP.defaultManagerSettings
      SQLite.withConnection dbPath \dbConn ->
        withStdoutLogger \logger -> do
          let env = MkHandlerEnv{dbConn, httpManager = mgr, decisionServiceUrl = Just dsUrl}
          putStrLn $ "Will push saved programs to decision service at: " <> dsUrl
          k port logger env
    _ -> error "Usage: jl4-websessions <port> <db-path> [decision-service-url]"

data Api mode
  = MkApi
  { createSession :: mode :- ReqBody '[JSON] Text                       :> Post '[JSON] UUID
  , readSession   :: mode :- QueryParam' '[Required, Strict] "id" UUID  :> Get '[JSON] Text
  -- NOTE: all sessions are persistant once they're created
  -- and not supposed to be updated
  -- , updateSession :: mode :- ReqBody '[JSON] JL4Program                 :> PutNoContent
  -- NOTE: listSessions deliberately not exposed for security reasons (unlisted UUIDs)
  }
  deriving stock (Generic)

data HandlerEnv = MkHandlerEnv
  { dbConn :: SQLite.Connection
  , httpManager :: HTTP.Manager
  , decisionServiceUrl :: Maybe String  -- e.g., "http://localhost:8081"
  }

newtype HandlerM a
  = MkHandlerM {unHandlerM :: ReaderT HandlerEnv Handler a}
  deriving newtype
  ( Functor
  , Applicative
  , Monad
  , MonadIO
  , MonadReader HandlerEnv
  , MonadError ServerError
  )

runHandlerM :: HandlerEnv -> HandlerM a -> Handler a
runHandlerM env = flip runReaderT env . (.unHandlerM)

server :: ServerT (NamedRoutes Api) HandlerM
server = MkApi{createSession, readSession}

createSession :: Text -> HandlerM UUID
createSession jl4program = do
  env <- ask
  sessionid <- liftIO UUID.nextRandom
  -- Save to SQLite
  liftIO $ SQLite.execute env.dbConn createStmt MkJL4Program{sessionid, jl4program}
  -- Push to decision service if configured
  liftIO $ pushToDecisionService env sessionid jl4program
  pure sessionid

createStmt :: SQLite.Query
createStmt = [sql| INSERT INTO sessions (sessionid, jl4program) VALUES (?, ?) |]

_updateSession :: JL4Program -> HandlerM NoContent
_updateSession pl = do
  conn <- asks (.dbConn)
  liftIO
    $ withToFieldUUID
    $ SQLite.execute conn updateStmt (pl.jl4program, pl.sessionid)
  pure NoContent

updateStmt :: SQLite.Query
updateStmt = [sql| UPDATE sessions SET jl4program = ? WHERE sessionid = ? |]

readSession :: UUID -> HandlerM Text
readSession sessionid = do
  conn <- asks (.dbConn)
  res <-
    liftIO
    $ withToFieldUUID
    $ SQLite.query conn readStmt
    $ SQLite.Only sessionid
  case res of
    [SQLite.Only t] -> pure t
    [] -> throwError err404
    _ -> throwError err500

readStmt :: SQLite.Query
readStmt = [sql| SELECT jl4program FROM sessions WHERE  sessionid = ? |]

-- | Push the saved program to the decision service so it's immediately available
pushToDecisionService :: HandlerEnv -> UUID -> Text -> IO ()
pushToDecisionService env sessionid jl4program = case env.decisionServiceUrl of
  Nothing -> pure ()  -- No decision service configured
  Just baseUrl -> do
    let
      uuidText = UUID.toText sessionid
      -- Note: Decision service applies filterIdeDirectives (AST-level filtering)
      -- when it parses and evaluates the program, so we send it as-is
      -- Build the FunctionImplementation JSON that the decision service expects
      -- Note: implementation uses array format [["jl4", "program"]] for Map EvalBackend Text
      functionImpl = Aeson.object
        [ "declaration" .= Aeson.object
            [ "type" .= ("function" :: Text)
            , "function" .= Aeson.object
                [ "name" .= uuidText
                , "description" .= jl4program
                , "parameters" .= Aeson.object
                    [ "type" .= ("object" :: Text)
                    , "properties" .= Aeson.object []
                    , "required" .= ([] :: [Text])
                    ]
                , "supportedBackends" .= (["jl4"] :: [Text])
                ]
            ]
        , "implementation" .= [[("jl4" :: Text), jl4program]]
        ]
      url = baseUrl <> "/functions/" <> Text.unpack uuidText

    -- Create and send POST request
    initialRequest <- HTTP.parseRequest url
    let request = initialRequest
          { HTTP.method = "POST"
          , HTTP.requestBody = HTTP.RequestBodyLBS $ Aeson.encode functionImpl
          , HTTP.requestHeaders = [("Content-Type", "application/json")]
          }

    -- Fire and forget - don't block on response, but log errors
    void $ HTTP.httpLbs request env.httpManager
    putStrLn $ "Pushed function " <> Text.unpack uuidText <> " to decision service"
  `catch` \(e :: HTTP.HttpException) -> do
    putStrLn $ "Warning: Failed to push to decision service: " <> show e

-- | to avoid orphan instance
withToFieldUUID :: ((SQLite.ToField UUID) => r) -> r
withToFieldUUID = withDict $ SQLite.SQLBlob . BS.toStrict . UUID.toByteString

_deleteStmt :: SQLite.Query
_deleteStmt = [sql| DELETE FROM session WHERE sessionid = ? |]

createDB :: FilePath -> IO ()
createDB dbPath = do
  fe <- doesFileExist dbPath
  unless fe do
    SQLite.withConnection dbPath (`SQLite.execute_` createDBStmt)

createDBStmt :: SQLite.Query
createDBStmt = [sql|CREATE TABLE sessions (sessionid BLOB PRIMARY KEY, jl4program TEXT NOT NULL) |]
