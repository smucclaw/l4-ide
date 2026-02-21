{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module IntegrationSpec (spec) where

import Test.Hspec

import Application (app)
import Backend.Api
import BundleStore (initStore)
import Compiler (compileBundle)
import ControlPlane (DeploymentStatusResponse (..))
import Types

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (newTVarIO)
import Control.Exception (try)
import qualified Codec.Archive.Zip as Zip
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding
import Network.HTTP.Client (defaultManagerSettings, newManager, httpLbs, parseRequest, requestBody, requestHeaders, method, Request, RequestBody (..), Response, responseStatus, responseBody, Manager)
import Network.HTTP.Types.Status (statusCode)
import Network.Wai.Handler.Warp (testWithApplication)
import System.Directory (removeDirectoryRecursive, doesDirectoryExist)
import System.IO.Error (isPermissionError)

import TestData (qualifiesJL4)

spec :: SpecWith ()
spec = describe "integration" do
  describe "data plane (direct compilation)" do
    it "evaluates a deployed function with all args true" do
      withServiceFromSources "eval-true" [("qualifies.l4", qualifiesJL4)] \baseUrl mgr -> do
        resp <- evalFunction baseUrl mgr "eval-true" "compute_qualifies"
          (Aeson.object
            [ "fnArguments" Aeson..= Aeson.object
                [ "walks" Aeson..= True
                , "eats" Aeson..= True
                , "drinks" Aeson..= True
                ]
            ])
        assertSuccess resp \r ->
          Map.lookup "value" r.fnResult `shouldBe` Just (FnLitBool True)

    it "evaluates a deployed function with all args false" do
      withServiceFromSources "eval-false" [("qualifies.l4", qualifiesJL4)] \baseUrl mgr -> do
        resp <- evalFunction baseUrl mgr "eval-false" "compute_qualifies"
          (Aeson.object
            [ "fnArguments" Aeson..= Aeson.object
                [ "walks" Aeson..= False
                , "eats" Aeson..= False
                , "drinks" Aeson..= False
                ]
            ])
        assertSuccess resp \r ->
          Map.lookup "value" r.fnResult `shouldBe` Just (FnLitBool False)

    it "lists functions for a deployment" do
      withServiceFromSources "list-fns" [("qualifies.l4", qualifiesJL4)] \baseUrl mgr -> do
        req <- parseRequest (baseUrl <> "/deployments/list-fns/functions")
        resp <- httpLbs req mgr
        statusCode' resp `shouldBe` 200

    it "returns 404 for unknown deployment" do
      withServiceFromSources "exists" [("qualifies.l4", qualifiesJL4)] \baseUrl mgr -> do
        req <- parseRequest (baseUrl <> "/deployments/nonexistent/functions")
        resp <- httpLbs req mgr
        statusCode' resp `shouldBe` 404

    it "returns 404 for unknown function" do
      withServiceFromSources "exists2" [("qualifies.l4", qualifiesJL4)] \baseUrl mgr -> do
        req <- parseRequest (baseUrl <> "/deployments/exists2/functions/no_such_fn")
        resp <- httpLbs req mgr
        statusCode' resp `shouldBe` 404

  describe "batch evaluation" do
    it "evaluates multiple cases in parallel" do
      withServiceFromSources "batch" [("qualifies.l4", qualifiesJL4)] \baseUrl mgr -> do
        let batchBody = Aeson.object
              [ "outcomes" Aeson..= ([] :: [Text])
              , "cases" Aeson..= map mkBatchCase [1..10 :: Int]
              ]
        req <- buildJsonPost (baseUrl <> "/deployments/batch/functions/compute_qualifies/evaluation/batch") batchBody
        resp <- httpLbs req mgr
        statusCode' resp `shouldBe` 200
        let mBatch = Aeson.decode (responseBody resp) :: Maybe BatchResponse
        case mBatch of
          Nothing -> expectationFailure ("Failed to decode batch response: " <> show (responseBody resp))
          Just batch -> do
            length batch.cases `shouldBe` 10
            batch.summary.casesRead `shouldBe` 10
            batch.summary.casesProcessed `shouldBe` 10
            batch.summary.casesIgnored `shouldBe` 0

  describe "control plane (HTTP multipart)" do
    it "deploys a bundle and reaches ready state" do
      withEmptyService \baseUrl mgr -> do
        let zipBytes = createZipBundle [("qualifies.l4", qualifiesJL4)]
        postReq <- buildMultipartRequest (baseUrl <> "/deployments") "http-deploy" zipBytes
        postResp <- httpLbs postReq mgr
        statusCode' postResp `shouldBe` 202
        let mStatus = Aeson.decode (responseBody postResp) :: Maybe DeploymentStatusResponse
        case mStatus of
          Nothing -> expectationFailure "Failed to decode deployment response"
          Just status -> do
            status.dsId `shouldBe` "http-deploy"
            status.dsStatus `shouldBe` "compiling"

        pollUntilReady baseUrl mgr "http-deploy" 60

    it "lists deployments" do
      withEmptyService \baseUrl mgr -> do
        let zipBytes = createZipBundle [("qualifies.l4", qualifiesJL4)]
        req1 <- buildMultipartRequest (baseUrl <> "/deployments") "list-a" zipBytes
        _ <- httpLbs req1 mgr
        req2 <- buildMultipartRequest (baseUrl <> "/deployments") "list-b" zipBytes
        _ <- httpLbs req2 mgr
        pollUntilReady baseUrl mgr "list-a" 60
        pollUntilReady baseUrl mgr "list-b" 60

        listReq <- parseRequest (baseUrl <> "/deployments")
        listResp <- httpLbs listReq mgr
        statusCode' listResp `shouldBe` 200
        let mList = Aeson.decode (responseBody listResp) :: Maybe [DeploymentStatusResponse]
        case mList of
          Nothing -> expectationFailure "Failed to decode deployment list"
          Just deploys -> do
            length deploys `shouldSatisfy` (>= 2)
            map (.dsId) deploys `shouldContain` ["list-a"]
            map (.dsId) deploys `shouldContain` ["list-b"]

    it "deletes a deployment" do
      withEmptyService \baseUrl mgr -> do
        let zipBytes = createZipBundle [("qualifies.l4", qualifiesJL4)]
        postReq <- buildMultipartRequest (baseUrl <> "/deployments") "to-delete" zipBytes
        _ <- httpLbs postReq mgr
        pollUntilReady baseUrl mgr "to-delete" 60

        deleteReq <- parseRequest (baseUrl <> "/deployments/to-delete")
        let deleteReq' = deleteReq { method = "DELETE" }
        deleteResp <- httpLbs deleteReq' mgr
        statusCode' deleteResp `shouldBe` 204

        getReq <- parseRequest (baseUrl <> "/deployments/to-delete")
        getResp <- httpLbs getReq mgr
        statusCode' getResp `shouldBe` 404

  describe "compiler" do
    it "compiles valid L4 sources" do
      let sources = Map.singleton "qualifies.l4" qualifiesJL4
      result <- compileBundle sources
      case result of
        Left err -> expectationFailure ("Compilation failed: " <> Text.unpack err)
        Right (fns, meta, _bundles) -> do
          Map.size fns `shouldSatisfy` (> 0)
          length meta.metaFunctions `shouldSatisfy` (> 0)
          Text.length meta.metaVersion `shouldBe` 64  -- SHA-256 hex

    it "rejects empty bundles" do
      let sources = Map.empty :: Map FilePath Text
      result <- compileBundle sources
      case result of
        Left err -> err `shouldBe` "No .l4 files found in bundle"
        Right _ -> expectationFailure "Expected compilation to fail for empty bundle"

-- ----------------------------------------------------------------------------
-- Helpers
-- ----------------------------------------------------------------------------

statusCode' :: Response a -> Int
statusCode' = statusCode . responseStatus

mkBatchCase :: Int -> Aeson.Value
mkBatchCase n = Aeson.object
  [ "@id" Aeson..= n
  , "walks" Aeson..= True
  , "eats" Aeson..= True
  , "drinks" Aeson..= True
  ]

-- | Compile sources and register them directly in the TVar,
-- then run a test against the WAI app.
withServiceFromSources :: Text -> [(FilePath, Text)] -> (String -> Manager -> IO a) -> IO a
withServiceFromSources deployId sources act = do
  resOrExc <- try (withServiceFromSources' deployId sources act)
  case resOrExc of
    Left ioe ->
      if isPermissionError ioe
        then pendingWith ("Skipping integration test (cannot bind sockets): " <> show ioe) >> pure undefined
        else do
          expectationFailure (show ioe)
          pure undefined
    Right result -> pure result

withServiceFromSources' :: Text -> [(FilePath, Text)] -> (String -> Manager -> IO a) -> IO a
withServiceFromSources' deployId sources act = do
  let tmpPath = "/tmp/jl4-service-test-" <> Text.unpack deployId
  cleanDir tmpPath
  store <- initStore tmpPath

  -- Compile the bundle directly
  let sourceMap = Map.fromList sources
  result <- compileBundle sourceMap
  (fns, meta) <- case result of
    Left err -> fail ("Test setup: compilation failed: " <> Text.unpack err)
    Right (f, m, _bundles) -> pure (f, m)

  -- Register directly in the TVar
  registry <- newTVarIO $ Map.singleton (DeploymentId deployId) (DeploymentReady fns meta)
  let env = MkAppEnv registry store Nothing

  mgr <- newManager defaultManagerSettings
  testWithApplication (pure $ app env) \port -> do
    let baseUrl = "http://localhost:" <> show port
    result' <- act baseUrl mgr
    cleanDir tmpPath
    pure result'

-- | Start a service with an empty deployment registry.
withEmptyService :: (String -> Manager -> IO a) -> IO a
withEmptyService act = do
  resOrExc <- try (withEmptyService' act)
  case resOrExc of
    Left ioe ->
      if isPermissionError ioe
        then pendingWith ("Skipping integration test (cannot bind sockets): " <> show ioe) >> pure undefined
        else do
          expectationFailure (show ioe)
          pure undefined
    Right result -> pure result

withEmptyService' :: (String -> Manager -> IO a) -> IO a
withEmptyService' act = do
  let tmpPath = "/tmp/jl4-service-test-empty"
  cleanDir tmpPath
  store <- initStore tmpPath
  registry <- newTVarIO Map.empty
  let env = MkAppEnv registry store Nothing

  mgr <- newManager defaultManagerSettings
  testWithApplication (pure $ app env) \port -> do
    let baseUrl = "http://localhost:" <> show port
    result <- act baseUrl mgr
    cleanDir tmpPath
    pure result

-- | Evaluate a function via the API.
evalFunction :: String -> Manager -> Text -> Text -> Aeson.Value -> IO (Response LBS.ByteString)
evalFunction baseUrl mgr deployId fnName body = do
  req <- buildJsonPost (baseUrl <> "/deployments/" <> Text.unpack deployId <> "/functions/" <> Text.unpack fnName <> "/evaluation") body
  httpLbs req mgr

-- | Assert a successful evaluation response.
assertSuccess :: Response LBS.ByteString -> (ResponseWithReason -> IO ()) -> IO ()
assertSuccess resp check = do
  statusCode' resp `shouldBe` 200
  case Aeson.decode (responseBody resp) :: Maybe SimpleResponse of
    Nothing -> expectationFailure ("Failed to decode eval response: " <> show (responseBody resp))
    Just (SimpleResponse r) -> check r
    Just (SimpleError e) -> expectationFailure ("Evaluation error: " <> show e)

-- | Poll a deployment until its status is "ready", with a timeout in seconds.
pollUntilReady :: String -> Manager -> String -> Int -> IO ()
pollUntilReady baseUrl mgr deployId timeoutSec = go timeoutSec
 where
  go 0 = expectationFailure $ "Deployment " <> deployId <> " did not become ready within timeout"
  go remaining = do
    req <- parseRequest (baseUrl <> "/deployments/" <> deployId)
    resp <- httpLbs req mgr
    let mStatus = Aeson.decode (responseBody resp) :: Maybe DeploymentStatusResponse
    case mStatus of
      Just s | s.dsStatus == "ready" -> pure ()
      Just s | s.dsStatus == "failed" ->
        expectationFailure $ "Deployment failed: " <> show s.dsError
      _ -> do
        threadDelay 500_000  -- 0.5 seconds
        go (remaining - 1)

-- | Create a zip archive from a list of (filename, content) pairs.
createZipBundle :: [(FilePath, Text)] -> LBS.ByteString
createZipBundle entries =
  Zip.fromArchive $ foldr addEntry Zip.emptyArchive entries
 where
  addEntry (path, content) archive =
    Zip.addEntryToArchive
      (Zip.toEntry path 0 (LBS.fromStrict $ Text.Encoding.encodeUtf8 content))
      archive

-- | Build a multipart POST request with an "id" field and a "sources" zip file.
buildMultipartRequest :: String -> Text -> LBS.ByteString -> IO Request
buildMultipartRequest url deployId zipBytes = do
  req <- parseRequest url
  let boundary = "----TestBoundary12345" :: BS.ByteString
      crlf = "\r\n" :: BS.ByteString
      dashdash = "--" :: BS.ByteString
      bodyParts =
        [ dashdash <> boundary <> crlf
        , "Content-Disposition: form-data; name=\"id\"" <> crlf <> crlf
        , Text.Encoding.encodeUtf8 deployId <> crlf
        , dashdash <> boundary <> crlf
        , "Content-Disposition: form-data; name=\"sources\"; filename=\"bundle.zip\"" <> crlf
        , "Content-Type: application/zip" <> crlf <> crlf
        ]
      body = LBS.fromChunks bodyParts <> zipBytes <> LBS.fromChunks [crlf, dashdash <> boundary <> dashdash <> crlf]
  pure req
    { method = "POST"
    , requestBody = RequestBodyLBS body
    , requestHeaders = [("Content-Type", "multipart/form-data; boundary=" <> boundary)]
    }

-- | Build a JSON POST request.
buildJsonPost :: String -> Aeson.Value -> IO Request
buildJsonPost url body = do
  req <- parseRequest url
  pure req
    { method = "POST"
    , requestBody = RequestBodyLBS (Aeson.encode body)
    , requestHeaders = [("Content-Type", "application/json")]
    }

-- | Clean/remove a directory if it exists.
cleanDir :: FilePath -> IO ()
cleanDir path = do
  exists <- doesDirectoryExist path
  if exists then removeDirectoryRecursive path else pure ()
