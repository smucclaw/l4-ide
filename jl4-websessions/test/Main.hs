module Main where

import Test.Hspec

import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as Aeson
import Data.Text (Text)
import qualified Data.Text as Text
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import Database.SQLite.Simple as SQLite (withConnection)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.HTTP.Types (hContentType, methodGet, methodPost, status200)
import Network.HTTP.Types.URI (renderQuery)
import Network.Wai (Request (..))
import Network.Wai.Test
  ( SRequest (..)
  , SResponse
  , Session
  , defaultRequest
  , runSession
  , simpleBody
  , simpleStatus
  , srequest
  )
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)

import L4.CRUD


main :: IO ()
main = hspec do
  it "performs all crud operations as expected" crudSmokeTest

crudSmokeTest :: Expectation
crudSmokeTest =
  withSystemTempDirectory "crud-smoke-test" \fp -> do
    let dbPath = fp </> "test.db"
        program = Text.pack "add x y MEANS x + y"
    createDB dbPath
    httpMgr <- newManager defaultManagerSettings
    SQLite.withConnection dbPath \dbConn -> do
      let env = MkHandlerEnv
            { dbConn = dbConn
            , httpManager = httpMgr
            , decisionServiceUrl = Nothing
            }
      runSession (crudSession program) (mkApp env)

crudSession :: Text -> Session ()
crudSession program = do
  sessionId <- createSessionRequest program
  retrieved <- readSessionRequest sessionId
  liftIO $ retrieved `shouldBe` program

createSessionRequest :: Text -> Session UUID
createSessionRequest program = do
  let req =
        defaultRequest
          { requestMethod = methodPost
          , requestHeaders = [(hContentType, "application/json")]
          , pathInfo = []
          , rawPathInfo = "/"
          }
  response <- srequest $ SRequest req (Aeson.encode program)
  assertStatus response
  decodeJson response

readSessionRequest :: UUID -> Session Text
readSessionRequest sessionId = do
  let req =
        defaultRequest
          { requestMethod = methodGet
          , queryString = query
          , rawQueryString = renderQuery True query
          , pathInfo = []
          , rawPathInfo = "/"
          }
  response <- srequest $ SRequest req mempty
  assertStatus response
  decodeJson response
  where
    query = [("id", Just $ UUID.toASCIIBytes sessionId)]

assertStatus :: SResponse -> Session ()
assertStatus response =
  if simpleStatus response == status200
    then pure ()
    else
      liftIO
        $ expectationFailure
        $ "Expected 200 OK but got "
        <> show (simpleStatus response)
        <> " with body: "
        <> show (simpleBody response)

decodeJson :: Aeson.FromJSON a => SResponse -> Session a
decodeJson response = case Aeson.eitherDecode (simpleBody response) of
  Left err -> do
    liftIO $ expectationFailure $ "Failed to decode JSON response: " <> err
    pure (error "unreachable")
  Right value -> pure value
