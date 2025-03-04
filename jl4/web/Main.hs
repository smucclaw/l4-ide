{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import GHC.Generics
import GHC.Exts (withDict)
import System.Environment
import Text.Read

import Data.Aeson
import Data.Text
import Data.UUID (UUID)
import Control.Monad.Except
import Control.Monad.Reader
import qualified Data.ByteString as BS
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import Database.SQLite.Simple.QQ (sql)
import qualified Database.SQLite.Simple as SQLite
import qualified Database.SQLite.Simple.ToField as SQLite
import qualified Database.SQLite.Simple.ToRow as SQLite

import Servant
import Servant.Server.Generic

import qualified Network.Wai.Handler.Warp as Warp

newtype HandlerEnv = MkHandlerEnv
  { dbConn :: SQLite.Connection }

main :: IO ()
main = do
  [readMaybe -> Just port, dbPath] <- getArgs
  SQLite.withConnection dbPath \dbConn -> do
    let env = MkHandlerEnv {dbConn}
    Warp.run port $ genericServeT (runHandlerM env) server

data JL4Program
  = MkJL4Program
  { sessionid :: UUID
  , jl4program :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON)

instance SQLite.ToRow JL4Program where
  toRow = withToFieldUUID $ SQLite.gtoRow . from

data Api mode
  = MkApi
  { createSession :: mode :- ReqBody '[JSON] Text       :> Post '[JSON] UUID
  , readSession   :: mode :- ReqBody '[JSON] UUID       :> Get '[JSON] Text
  , updateSession :: mode :- ReqBody '[JSON] JL4Program :> PostNoContent
  }
  deriving stock Generic

newtype HandlerM a
  = MkHandlerM {unHandlerM :: ReaderT HandlerEnv Handler a}
  deriving newtype
    ( Functor, Applicative, Monad, MonadIO
    , MonadReader HandlerEnv, MonadError ServerError
    )

runHandlerM :: HandlerEnv -> HandlerM a -> Handler a
runHandlerM env = flip runReaderT env . (.unHandlerM)

server :: ServerT (NamedRoutes Api) HandlerM
server = MkApi {createSession, readSession, updateSession}

createSession :: Text -> HandlerM UUID
createSession jl4program = do
  conn <- asks (.dbConn)
  sessionid <- liftIO UUID.nextRandom
  liftIO $ SQLite.execute conn createStmt MkJL4Program {sessionid, jl4program}
  pure sessionid

createStmt :: SQLite.Query
createStmt = [sql| INSERT INTO sessions (sessionid, jl4program) VALUES (?, ?) |]

updateSession :: JL4Program -> HandlerM NoContent
updateSession pl = do
  conn <- asks (.dbConn)
  liftIO
    $ withToFieldUUID
    $ SQLite.execute conn updateStmt (pl.sessionid, pl.jl4program)
  pure NoContent

updateStmt :: SQLite.Query
updateStmt = [sql| UPDATE sessions SET jl4program = ? WHERE sessionid = ? |]

readSession :: UUID -> HandlerM Text
readSession sessionid = do
  conn <- asks (.dbConn)
  res <- liftIO
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
withToFieldUUID :: (SQLite.ToField UUID => r) -> r
withToFieldUUID = withDict $ SQLite.SQLBlob . BS.toStrict . UUID.toByteString

deleteStmt :: SQLite.Query
deleteStmt = [sql| DELETE FROM session WHERE sessionid = ? |]
