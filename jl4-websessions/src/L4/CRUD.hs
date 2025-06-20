{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}
module L4.CRUD (mkApp, withEnv, createDB, Api (..), HandlerEnv (..), JL4Program (..)) where

import GHC.Exts (withDict)
import GHC.Generics
import System.Environment
import Text.Read

import Control.Monad (unless)
import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.ByteString as BS
import Data.Text (Text)
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import qualified Database.SQLite.Simple as SQLite
import Database.SQLite.Simple.QQ (sql)
import qualified Database.SQLite.Simple.ToField as SQLite
import qualified Database.SQLite.Simple.ToRow as SQLite
import System.Directory
import Network.Wai.Logger (ApacheLogger, withStdoutLogger)
import Data.Map (Map)

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
  [readMaybe -> Just port, dbPath] <- getArgs
  createDB dbPath
  SQLite.withConnection dbPath \dbConn ->
    withStdoutLogger \logger -> do
      let env = MkHandlerEnv{dbConn}
      k port logger env

data Api mode
  = MkApi
  { createSession :: mode :- ReqBody '[JSON] Text                       :> Post '[JSON] UUID
  , readSession   :: mode :- QueryParam' '[Required, Strict] "id" UUID  :> Get '[JSON] Text
  , readFnParams  :: mode :- QueryParam' '[Required, Strict] "id" UUID  :> Get '[JSON] (Map Text Text)
  -- NOTE: all sessions are persistant once they're created
  -- and not supposed to be updated
  -- , updateSession :: mode :- ReqBody '[JSON] JL4Program                 :> PutNoContent
  }
  deriving stock (Generic)

newtype HandlerEnv = MkHandlerEnv {dbConn :: SQLite.Connection}

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
  conn <- asks (.dbConn)
  sessionid <- liftIO UUID.nextRandom
  liftIO $ SQLite.execute conn createStmt MkJL4Program{sessionid, jl4program}
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
