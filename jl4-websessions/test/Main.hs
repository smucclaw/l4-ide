module Main where

import Test.Hspec

import Control.Monad.IO.Class (liftIO)
import Database.SQLite.Simple as SQLite (withConnection)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.Wai.Handler.Warp (testWithApplication)
import Servant.Client
import Servant.Client.Generic (genericClient)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)

import L4.CRUD


main :: IO ()
main = hspec do
  it "performs all crud operations as expected" crudSmokeTest

crudSmokeTest :: Expectation
crudSmokeTest = do
  let apiClient = genericClient @Api @ClientM
      runC port c =  do
        mgr <- newManager defaultManagerSettings
        let baseUrl = BaseUrl Http "localhost" port ""
        runClientM c $ mkClientEnv mgr baseUrl
  result <- withSystemTempDirectory "crud-smoke-test" \fp -> do
    let dbPath = fp </> "test.db"
    createDB dbPath
    httpMgr <- newManager defaultManagerSettings
    SQLite.withConnection dbPath \dbConn -> do
      let env = MkHandlerEnv
            { dbConn = dbConn
            , httpManager = httpMgr
            , decisionServiceUrl = Nothing  -- No decision service for tests
            }
      testWithApplication (pure $ mkApp env) \port -> do
        runC port do
          let prog = "add x y MEANS x + y"
          uuid <- apiClient.createSession prog
          prog' <- apiClient.readSession uuid
          liftIO $ prog `shouldBe` prog'

          -- NOTE: this test was for putting an updated file
          -- but it has become redundant b/c all sessions are
          -- supposed to stay persistant
          -- let prog2 = "sub x y MEANS x - y"
          -- NoContent <- apiClient.updateSession MkJL4Program {jl4program = prog2, sessionid = uuid}
          -- prog2' <- apiClient.readSession uuid
          -- liftIO $ prog2 `shouldBe` prog2'

  result `shouldBe` Right ()
