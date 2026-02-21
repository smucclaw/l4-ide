{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module SerialisationSpec (spec) where

import Test.Hspec

import Application (app)
import Backend.Api (FnLiteral (..), ResponseWithReason (..))
import BundleStore (BundleStore (..), initStore, saveBundle, loadBundle, deleteBundle, saveBundleCbor, loadBundleCbor, SerializedBundle (..), StoredMetadata (..))
import Compiler (compileBundle, buildFromCborBundle, computeVersion)
import ControlPlane (DeploymentStatusResponse (..))
import Types

import Codec.Serialise (serialise, deserialiseOrFail)
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (newTVarIO)
import Data.Maybe (isJust, isNothing)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding
import qualified Codec.Archive.Zip as Zip
import Network.HTTP.Client (defaultManagerSettings, newManager, httpLbs, parseRequest, requestBody, requestHeaders, method, Request, RequestBody (..), Response, responseStatus, responseBody, Manager)
import Network.HTTP.Types.Status (statusCode)
import Network.Wai.Handler.Warp (testWithApplication)
import System.Directory (removeDirectoryRecursive, doesDirectoryExist)
import System.FilePath ((</>))

import TestData (qualifiesJL4)

spec :: SpecWith ()
spec = describe "CBOR serialisation" do
  describe "SerializedBundle round-trip" do
    it "compileBundle produces non-empty SerializedBundle list" do
      let sources = Map.singleton "qualifies.l4" qualifiesJL4
      result <- compileBundle sources
      case result of
        Left err -> expectationFailure ("Compilation failed: " <> Text.unpack err)
        Right (_fns, _meta, bundles) -> do
          length bundles `shouldSatisfy` (> 0)

    it "SerializedBundle survives CBOR encode/decode round-trip" do
      let sources = Map.singleton "qualifies.l4" qualifiesJL4
      result <- compileBundle sources
      case result of
        Left err -> expectationFailure ("Compilation failed: " <> Text.unpack err)
        Right (_fns, _meta, bundles) -> do
          -- Encode and decode each bundle
          mapM_ (\bundle -> do
            let encoded = serialise bundle
            case deserialiseOrFail encoded of
              Left decodeErr -> expectationFailure ("CBOR decode failed: " <> show decodeErr)
              Right (decoded :: SerializedBundle) -> do
                -- Verify the module still has the same exports
                -- (we can't compare directly since Anno_ is stripped,
                -- but we can rebuild functions from it)
                rebuildResult <- buildFromCborBundle decoded sources
                  (StoredMetadata [] (computeVersion sources) "2025-01-01T00:00:00Z")
                case rebuildResult of
                  Left rebuildErr -> expectationFailure ("Rebuild from decoded CBOR failed: " <> Text.unpack rebuildErr)
                  Right (fns, _meta') -> do
                    Map.size fns `shouldSatisfy` (> 0)
            ) bundles

  describe "BundleStore CBOR persistence" do
    it "saveBundleCbor and loadBundleCbor round-trip" do
      withTempStore $ \store -> do
        let sources = Map.singleton "qualifies.l4" qualifiesJL4
            meta = StoredMetadata [] "v1" "2025-01-01T00:00:00Z"
        saveBundle store "cbor-test" sources meta

        result <- compileBundle sources
        case result of
          Left err -> expectationFailure ("Compilation failed: " <> Text.unpack err)
          Right (_fns, _meta, bundles) -> do
            mapM_ (saveBundleCbor store "cbor-test") bundles

            loaded <- loadBundleCbor store "cbor-test"
            isJust loaded `shouldBe` True

    it "loadBundleCbor returns Nothing for non-existent deployment" do
      withTempStore $ \store -> do
        loaded <- loadBundleCbor store "nonexistent"
        isNothing loaded `shouldBe` True

    it "loadBundleCbor returns Nothing for corrupt CBOR data" do
      withTempStore $ \store -> do
        let sources = Map.singleton "qualifies.l4" qualifiesJL4
            meta = StoredMetadata [] "v1" "2025-01-01T00:00:00Z"
        saveBundle store "corrupt-test" sources meta

        -- Write garbage to bundle.cbor
        let BundleStore root = store
            cborPath = root </> "corrupt-test" </> "bundle.cbor"
        LBS.writeFile cborPath "this is not valid CBOR"

        loaded <- loadBundleCbor store "corrupt-test"
        isNothing loaded `shouldBe` True

    it "saveBundleCbor overwrites existing CBOR atomically" do
      withTempStore $ \store -> do
        let sources = Map.singleton "qualifies.l4" qualifiesJL4
            meta = StoredMetadata [] "v1" "2025-01-01T00:00:00Z"
        saveBundle store "overwrite-test" sources meta

        result <- compileBundle sources
        case result of
          Left err -> expectationFailure ("Compilation failed: " <> Text.unpack err)
          Right (_fns, _meta, bundles) -> do
            -- Save twice — second should overwrite first cleanly
            mapM_ (saveBundleCbor store "overwrite-test") bundles
            mapM_ (saveBundleCbor store "overwrite-test") bundles

            loaded <- loadBundleCbor store "overwrite-test"
            isJust loaded `shouldBe` True

  describe "buildFromCborBundle" do
    it "produces the same function names as compileBundle" do
      let sources = Map.singleton "qualifies.l4" qualifiesJL4
      result <- compileBundle sources
      case result of
        Left err -> expectationFailure ("Compilation failed: " <> Text.unpack err)
        Right (compiledFns, _meta, bundles) -> do
          -- Rebuild from CBOR
          let storedMeta = StoredMetadata [] (computeVersion sources) "2025-01-01T00:00:00Z"
          rebuiltResults <- mapM (\b -> buildFromCborBundle b sources storedMeta) bundles
          let rebuiltFns = mconcat [fns | Right (fns, _) <- rebuiltResults]

          -- Same function names
          Map.keys rebuiltFns `shouldMatchList` Map.keys compiledFns

          -- Each function has the same name/description in its impl
          mapM_ (\fnName -> do
            case (Map.lookup fnName compiledFns, Map.lookup fnName rebuiltFns) of
              (Just c, Just r) -> do
                c.fnImpl.name `shouldBe` r.fnImpl.name
                c.fnImpl.description `shouldBe` r.fnImpl.description
              _ -> expectationFailure $ "Missing function: " <> Text.unpack fnName
            ) (Map.keys compiledFns)

    it "produces functions that evaluate correctly (all true)" do
      let sources = Map.singleton "qualifies.l4" qualifiesJL4
      withCborRebuiltService "cbor-eval-true" sources $ \baseUrl mgr -> do
        resp <- evalFunction baseUrl mgr "cbor-eval-true" "compute_qualifies"
          (Aeson.object
            [ "fnArguments" Aeson..= Aeson.object
                [ "walks" Aeson..= True
                , "eats" Aeson..= True
                , "drinks" Aeson..= True
                ]
            ])
        assertSuccess resp $ \r ->
          Map.lookup "value" r.fnResult `shouldBe` Just (FnLitBool True)

    it "produces functions that evaluate correctly (all false)" do
      let sources = Map.singleton "qualifies.l4" qualifiesJL4
      withCborRebuiltService "cbor-eval-false" sources $ \baseUrl mgr -> do
        resp <- evalFunction baseUrl mgr "cbor-eval-false" "compute_qualifies"
          (Aeson.object
            [ "fnArguments" Aeson..= Aeson.object
                [ "walks" Aeson..= False
                , "eats" Aeson..= False
                , "drinks" Aeson..= False
                ]
            ])
        assertSuccess resp $ \r ->
          Map.lookup "value" r.fnResult `shouldBe` Just (FnLitBool False)

    it "produces functions that evaluate correctly (mixed args)" do
      let sources = Map.singleton "qualifies.l4" qualifiesJL4
      withCborRebuiltService "cbor-eval-mixed" sources $ \baseUrl mgr -> do
        resp <- evalFunction baseUrl mgr "cbor-eval-mixed" "compute_qualifies"
          (Aeson.object
            [ "fnArguments" Aeson..= Aeson.object
                [ "walks" Aeson..= True
                , "eats" Aeson..= False
                , "drinks" Aeson..= True
                ]
            ])
        assertSuccess resp $ \r ->
          Map.lookup "value" r.fnResult `shouldBe` Just (FnLitBool False)

  describe "full restart simulation" do
    it "compile → save CBOR → load CBOR → rebuild → evaluate" do
      let sources = Map.singleton "qualifies.l4" qualifiesJL4
          deployId' = "restart-sim"
      withTempStore $ \store -> do
        let meta = StoredMetadata [] (computeVersion sources) "2025-01-01T00:00:00Z"
        saveBundle store deployId' sources meta

        -- Step 1: Compile from source (initial deployment)
        result <- compileBundle sources
        case result of
          Left err -> expectationFailure ("Compilation failed: " <> Text.unpack err)
          Right (_fns, _meta, bundles) -> do
            -- Step 2: Save CBOR cache (as ControlPlane.hs does)
            mapM_ (saveBundleCbor store deployId') bundles

            -- Step 3: Simulate restart — load CBOR from disk
            mCbor <- loadBundleCbor store deployId'
            case mCbor of
              Nothing -> expectationFailure "Expected bundle.cbor to exist after save"
              Just bundle -> do
                -- Step 4: Rebuild from CBOR (as Application.hs does)
                (loadedSources, storedMeta) <- loadBundle store deployId'
                rebuildResult <- buildFromCborBundle bundle loadedSources storedMeta
                case rebuildResult of
                  Left err -> expectationFailure ("Rebuild from CBOR failed: " <> Text.unpack err)
                  Right (fns, rebuildMeta) -> do
                    -- Step 5: Verify we got working functions
                    Map.size fns `shouldSatisfy` (> 0)
                    length rebuildMeta.metaFunctions `shouldSatisfy` (> 0)

                    -- Step 6: Actually serve requests using rebuilt functions
                    registry <- newTVarIO $ Map.singleton (DeploymentId deployId') (DeploymentReady fns rebuildMeta)
                    let env = MkAppEnv registry store Nothing
                    mgrLocal <- newManager defaultManagerSettings
                    testWithApplication (pure $ app env) $ \port -> do
                      let baseUrl = "http://localhost:" <> show port
                      resp <- evalFunction baseUrl mgrLocal deployId' "compute_qualifies"
                        (Aeson.object
                          [ "fnArguments" Aeson..= Aeson.object
                              [ "walks" Aeson..= True
                              , "eats" Aeson..= True
                              , "drinks" Aeson..= True
                              ]
                          ])
                      assertSuccess resp $ \r ->
                        Map.lookup "value" r.fnResult `shouldBe` Just (FnLitBool True)

    it "deleteBundle also removes CBOR cache" do
      let sources = Map.singleton "qualifies.l4" qualifiesJL4
          deployId' = "delete-cbor"
      withTempStore $ \store -> do
        let meta = StoredMetadata [] (computeVersion sources) "2025-01-01T00:00:00Z"
        saveBundle store deployId' sources meta

        result <- compileBundle sources
        case result of
          Left err -> expectationFailure ("Compilation failed: " <> Text.unpack err)
          Right (_fns, _meta, bundles) -> do
            mapM_ (saveBundleCbor store deployId') bundles

            -- Verify CBOR exists
            mCbor <- loadBundleCbor store deployId'
            isJust mCbor `shouldBe` True

            -- Delete entire deployment
            deleteBundle store deployId'

            -- CBOR should be gone too (directory deleted)
            mCbor2 <- loadBundleCbor store deployId'
            isNothing mCbor2 `shouldBe` True

  describe "HTTP control plane with CBOR" do
    it "deploy via POST, then verify CBOR was saved" do
      withEmptyService $ \baseUrl mgr store -> do
        -- Deploy via multipart POST
        let zipBytes = createZipBundle [("qualifies.l4", qualifiesJL4)]
        postReq <- buildMultipartRequest (baseUrl <> "/deployments") "cbor-http" zipBytes
        postResp <- httpLbs postReq mgr
        statusCode' postResp `shouldBe` 202

        -- Wait for compilation to finish
        pollUntilReady baseUrl mgr "cbor-http" 60

        -- Verify CBOR was saved by the POST handler
        mCbor <- loadBundleCbor store "cbor-http"
        isJust mCbor `shouldBe` True

        -- Evaluate via the normally-compiled service
        resp <- evalFunction baseUrl mgr "cbor-http" "compute_qualifies"
          (Aeson.object
            [ "fnArguments" Aeson..= Aeson.object
                [ "walks" Aeson..= True
                , "eats" Aeson..= True
                , "drinks" Aeson..= True
                ]
            ])
        assertSuccess resp $ \r ->
          Map.lookup "value" r.fnResult `shouldBe` Just (FnLitBool True)

-- ----------------------------------------------------------------------------
-- Helpers
-- ----------------------------------------------------------------------------

statusCode' :: Response a -> Int
statusCode' = statusCode . responseStatus

-- | Create a temp BundleStore, run the test, then clean up.
withTempStore :: (BundleStore -> IO a) -> IO a
withTempStore action = do
  let tmpPath = "/tmp/jl4-service-test-serialisation"
  cleanDir tmpPath
  store <- initStore tmpPath
  result <- action store
  cleanDir tmpPath
  pure result

-- | Compile sources, rebuild from CBOR, register, and serve via WAI.
-- This simulates a restart: compile → serialize → deserialize → serve.
withCborRebuiltService :: Text -> Map FilePath Text -> (String -> Manager -> IO a) -> IO a
withCborRebuiltService deployId sources act = do
  let tmpPath = "/tmp/jl4-service-test-" <> Text.unpack deployId
  cleanDir tmpPath
  store <- initStore tmpPath

  -- Compile from source
  result <- compileBundle sources
  (_fns, meta, bundles) <- case result of
    Left err -> fail ("Test setup: compilation failed: " <> Text.unpack err)
    Right r -> pure r

  -- Serialize and deserialize (simulating restart)
  let storedMeta = StoredMetadata [] (computeVersion sources) "2025-01-01T00:00:00Z"
  rebuiltFns <- fmap mconcat $ mapM (\bundle -> do
    -- Encode to CBOR bytes, then decode back
    let encoded = serialise bundle
    case deserialiseOrFail encoded of
      Left err -> fail ("CBOR decode failed: " <> show err)
      Right decoded -> do
        rebuildResult <- buildFromCborBundle decoded sources storedMeta
        case rebuildResult of
          Left err -> fail ("Rebuild failed: " <> Text.unpack err)
          Right (rebuiltFns', _) -> pure rebuiltFns'
    ) bundles

  -- Register rebuilt functions and serve
  registry <- newTVarIO $ Map.singleton (DeploymentId deployId) (DeploymentReady rebuiltFns meta)
  let env = MkAppEnv registry store Nothing

  mgrLocal <- newManager defaultManagerSettings
  testWithApplication (pure $ app env) $ \port -> do
    let baseUrl = "http://localhost:" <> show port
    result' <- act baseUrl mgrLocal
    cleanDir tmpPath
    pure result'

-- | Start a service with an empty deployment registry, exposing the store.
withEmptyService :: (String -> Manager -> BundleStore -> IO a) -> IO a
withEmptyService act = do
  let tmpPath = "/tmp/jl4-service-test-cbor-empty"
  cleanDir tmpPath
  store <- initStore tmpPath
  registry <- newTVarIO Map.empty
  let env = MkAppEnv registry store Nothing

  mgrLocal <- newManager defaultManagerSettings
  testWithApplication (pure $ app env) $ \port -> do
    let baseUrl = "http://localhost:" <> show port
    result <- act baseUrl mgrLocal store
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
pollUntilReady baseUrl mgr deployId' timeoutSec = go timeoutSec
 where
  go 0 = expectationFailure $ "Deployment " <> deployId' <> " did not become ready within timeout"
  go remaining = do
    req <- parseRequest (baseUrl <> "/deployments/" <> deployId')
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
  let boundary = "----TestBoundary12345"
      crlf = "\r\n"
      dashdash = "--"
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
