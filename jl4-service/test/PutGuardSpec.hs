{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | End-to-end tests for the PUT deployment backwards-compatibility
-- guard, exercised through the real WAI application.
--
-- Also pins down that the operator description is *optional*: an older
-- VSCode extension that does not send a @description@ multipart field
-- can still deploy (it just gets no description), and a source-only PUT
-- that omits it preserves whatever was set before.
module PutGuardSpec (spec) where

import Test.Hspec

import Application (app)
import Backend.Api (FnLiteral (..), ResponseWithReason (..))
import BundleStore (initStore)
import ControlPlane (DeploymentStatusResponse (..))
import Logging (newLogger)
import Options (Options (..))
import Types

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (newTVarIO)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding
import qualified Codec.Archive.Zip as Zip
import Network.HTTP.Client
  ( Manager, Request, RequestBody (..), Response
  , defaultManagerSettings, httpLbs, method, newManager, parseRequest
  , requestBody, requestHeaders, responseBody, responseStatus )
import Network.HTTP.Types.Method (Method)
import Network.HTTP.Types.Status (statusCode)
import Network.Wai.Handler.Warp (testWithApplication)
import System.Directory (doesDirectoryExist, removeDirectoryRecursive)
import Data.String.Interpolate (i)

------------------------------------------------------------------------
-- L4 fixtures: same function name, three interface/behaviour variants.
------------------------------------------------------------------------

-- | Base: walks/eats/drinks BOOLEAN → BOOLEAN, logic = AND.
baseSrc :: Text
baseSrc = [i|
@export default person qualifies
GIVEN walks IS A BOOLEAN
      eats  IS A BOOLEAN
      drinks IS A BOOLEAN
GIVETH A BOOLEAN
DECIDE compute_qualifies IF walks AND eats AND drinks
|]

-- | Compatible change: identical interface, logic changed to OR.
compatSrc :: Text
compatSrc = [i|
@export default person qualifies
GIVEN walks IS A BOOLEAN
      eats  IS A BOOLEAN
      drinks IS A BOOLEAN
GIVETH A BOOLEAN
DECIDE compute_qualifies IF walks OR eats OR drinks
|]

-- | Breaking change: parameter `walks` retyped BOOLEAN → NUMBER.
breakingSrc :: Text
breakingSrc = [i|
@export default person qualifies
GIVEN walks IS A NUMBER
      eats  IS A BOOLEAN
      drinks IS A BOOLEAN
GIVETH A BOOLEAN
DECIDE compute_qualifies IF walks >= 1 AND eats AND drinks
|]

------------------------------------------------------------------------
-- Spec
------------------------------------------------------------------------

spec :: Spec
spec = describe "PUT deployment compatibility guard (end-to-end)" do

  it "POST without a description (older client) still deploys" do
    withService $ \baseUrl mgr -> do
      r <- deployPost baseUrl mgr "guard-nodesc" Nothing baseSrc
      statusCode' r `shouldBe` 202
      pollUntilReady baseUrl mgr "guard-nodesc"
      desc <- deploymentDescription baseUrl mgr "guard-nodesc"
      desc `shouldBe` Nothing

  it "accepts and applies a compatible PUT (older client omits description)" do
    withService $ \baseUrl mgr -> do
      _ <- deployPost baseUrl mgr "guard-compat" Nothing baseSrc
      pollUntilReady baseUrl mgr "guard-compat"

      -- Base logic is AND: one false ⇒ False.
      evalQualifies baseUrl mgr "guard-compat" False True True
        `shouldReturnResult` FnLitBool False

      r <- deployPut baseUrl mgr "guard-compat" Nothing compatSrc
      statusCode' r `shouldBe` 202
      -- PUT is async: it returns "compiling" and the gate runs in the
      -- background; the old version keeps serving until it lands.
      dsStatusOf r `shouldBe` Just "compiling"
      pollUntilReady baseUrl mgr "guard-compat"

      -- Compatible logic is OR: same inputs now ⇒ True.
      evalQualifies baseUrl mgr "guard-compat" False True True
        `shouldReturnResult` FnLitBool True

  it "rejects a breaking PUT asynchronously and leaves the deployment intact" do
    withService $ \baseUrl mgr -> do
      _ <- deployPost baseUrl mgr "guard-break" Nothing baseSrc
      pollUntilReady baseUrl mgr "guard-break"

      r <- deployPut baseUrl mgr "guard-break" Nothing breakingSrc
      -- Accepted for processing; rejection is surfaced via the status
      -- endpoint, not the PUT response.
      statusCode' r `shouldBe` 202
      (st, mErr) <- pollOutcome baseUrl mgr "guard-break"
      st `shouldBe` "failed"
      let err = fromMaybe "" mErr
      err `shouldSatisfy` Text.isInfixOf "break"
      err `shouldSatisfy` Text.isInfixOf "type changed"

      -- TTL'd, not read-and-clear: a second poll still sees the
      -- rejection (it is not consumed by the first observer).
      (st2, _) <- statusOnce baseUrl mgr "guard-break"
      st2 `shouldBe` "failed"

      -- The original (boolean) interface must still work unchanged.
      evalQualifies baseUrl mgr "guard-break" True True True
        `shouldReturnResult` FnLitBool True

  it "PUT to a non-existent deployment is 404" do
    withService $ \baseUrl mgr -> do
      r <- deployPut baseUrl mgr "does-not-exist" Nothing baseSrc
      statusCode' r `shouldBe` 404

  it "preserves a description across a source-only PUT (older client omits it)" do
    withService $ \baseUrl mgr -> do
      _ <- deployPost baseUrl mgr "guard-desc" (Just "Intended use: demo") baseSrc
      pollUntilReady baseUrl mgr "guard-desc"
      deploymentDescription baseUrl mgr "guard-desc"
        `shouldReturn` Just "Intended use: demo"

      r <- deployPut baseUrl mgr "guard-desc" Nothing compatSrc
      statusCode' r `shouldBe` 202
      pollUntilReady baseUrl mgr "guard-desc"
      deploymentDescription baseUrl mgr "guard-desc"
        `shouldReturn` Just "Intended use: demo"

------------------------------------------------------------------------
-- Harness
------------------------------------------------------------------------

testOpts :: Options
testOpts = Options
  { port = 0
  , storePath = "/tmp/jl4-service-test-putguard"
  , serverName = Nothing
  , lazyLoad = False
  , debug = True
  , maxZipSize = 2097152
  , maxFileCount = 5096
  , maxDeployments = 1024
  , maxConcurrentRequests = 20
  , maxEvalMemoryMb = 256
  , maxCompileMemoryMb = 512
  , evalTimeout = 60
  , compileTimeout = 60
  , instanceToken = Nothing
  }

withService :: (String -> Manager -> IO a) -> IO a
withService act = do
  logger <- newLogger False
  let tmpPath = "/tmp/jl4-service-test-putguard"
  cleanDir tmpPath
  store <- initStore tmpPath
  registry <- newTVarIO Map.empty
  pendingUpd <- newTVarIO Map.empty
  let env = MkAppEnv registry pendingUpd store Nothing logger testOpts
  mgr <- newManager defaultManagerSettings
  testWithApplication (pure $ app env) $ \prt -> do
    let baseUrl = "http://localhost:" <> show prt
    res <- act baseUrl mgr
    cleanDir tmpPath
    pure res

statusCode' :: Response a -> Int
statusCode' = statusCode . responseStatus

dsStatusOf :: Response LBS.ByteString -> Maybe Text
dsStatusOf r =
  (\s -> s.dsStatus) <$> (Aeson.decode (responseBody r) :: Maybe DeploymentStatusResponse)

deployPost :: String -> Manager -> Text -> Maybe Text -> Text -> IO (Response LBS.ByteString)
deployPost baseUrl mgr deployId mDesc src = do
  let fields = [("id", deployId)] <> maybe [] (\d -> [("description", d)]) mDesc
  req <- multipartReq "POST" (baseUrl <> "/deployments") fields
           (createZipBundle [("qualifies.l4", src)])
  httpLbs req mgr

deployPut :: String -> Manager -> Text -> Maybe Text -> Text -> IO (Response LBS.ByteString)
deployPut baseUrl mgr deployId mDesc src = do
  let fields = maybe [] (\d -> [("description", d)]) mDesc
  req <- multipartReq "PUT"
           (baseUrl <> "/deployments/" <> Text.unpack deployId) fields
           (createZipBundle [("qualifies.l4", src)])
  httpLbs req mgr

-- | GET /deployments/{id} → metadata.description (Nothing when unset).
deploymentDescription :: String -> Manager -> Text -> IO (Maybe Text)
deploymentDescription baseUrl mgr deployId = do
  req <- parseRequest (baseUrl <> "/deployments/" <> Text.unpack deployId)
  resp <- httpLbs req mgr
  case Aeson.decode (responseBody resp) :: Maybe DeploymentStatusResponse of
    Just s -> pure (s.dsMetadata >>= \m -> m.metaDescription)
    Nothing -> pure Nothing

evalQualifies
  :: String -> Manager -> Text -> Bool -> Bool -> Bool
  -> IO (Response LBS.ByteString)
evalQualifies baseUrl mgr deployId w e d = do
  let url = baseUrl <> "/deployments/" <> Text.unpack deployId
              <> "/functions/compute_qualifies/evaluation"
  req0 <- parseRequest url
  let body = Aeson.object
        [ "arguments" Aeson..= Aeson.object
            [ "walks" Aeson..= w, "eats" Aeson..= e, "drinks" Aeson..= d ]
        ]
  httpLbs req0
    { method = "POST"
    , requestBody = RequestBodyLBS (Aeson.encode body)
    , requestHeaders = [("Content-Type", "application/json")]
    } mgr

-- | Assert a 200 evaluation whose "value" equals the expected literal.
shouldReturnResult :: IO (Response LBS.ByteString) -> FnLiteral -> Expectation
shouldReturnResult act expected = do
  resp <- act
  statusCode' resp `shouldBe` 200
  case Aeson.decode (responseBody resp) :: Maybe SimpleResponse of
    Just (SimpleResponse r) ->
      Map.lookup "value" r.fnResult `shouldBe` Just expected
    Just (SimpleError err) ->
      expectationFailure ("Evaluation error: " <> show err)
    Nothing ->
      expectationFailure ("Undecodable eval response: " <> show (responseBody resp))

pollUntilReady :: String -> Manager -> Text -> IO ()
pollUntilReady baseUrl mgr deployId = go (120 :: Int)
 where
  go 0 = expectationFailure ("Deployment " <> Text.unpack deployId <> " never became ready")
  go n = do
    req <- parseRequest (baseUrl <> "/deployments/" <> Text.unpack deployId)
    resp <- httpLbs req mgr
    case Aeson.decode (responseBody resp) :: Maybe DeploymentStatusResponse of
      Just s | s.dsStatus == "ready"  -> pure ()
      Just s | s.dsStatus == "failed" ->
        expectationFailure ("Deployment failed: " <> show s.dsError)
      _ -> threadDelay 500_000 >> go (n - 1)

-- | Poll until the deployment reaches a terminal status, returning it
-- with any error message (does not fail the test on "failed").
pollOutcome :: String -> Manager -> Text -> IO (Text, Maybe Text)
pollOutcome baseUrl mgr deployId = go (120 :: Int)
 where
  go 0 = pure ("timeout", Nothing)
  go n = do
    req <- parseRequest (baseUrl <> "/deployments/" <> Text.unpack deployId)
    resp <- httpLbs req mgr
    case Aeson.decode (responseBody resp) :: Maybe DeploymentStatusResponse of
      Just s | s.dsStatus == "ready"  -> pure ("ready", s.dsError)
      Just s | s.dsStatus == "failed" -> pure ("failed", s.dsError)
      _ -> threadDelay 500_000 >> go (n - 1)

-- | Single GET /deployments/{id} → (status, error).
statusOnce :: String -> Manager -> Text -> IO (Text, Maybe Text)
statusOnce baseUrl mgr deployId = do
  req <- parseRequest (baseUrl <> "/deployments/" <> Text.unpack deployId)
  resp <- httpLbs req mgr
  case Aeson.decode (responseBody resp) :: Maybe DeploymentStatusResponse of
    Just s  -> pure (s.dsStatus, s.dsError)
    Nothing -> pure ("undecodable", Nothing)

createZipBundle :: [(FilePath, Text)] -> LBS.ByteString
createZipBundle entries =
  Zip.fromArchive $ foldr addEntry Zip.emptyArchive entries
 where
  addEntry (path, content) archive =
    Zip.addEntryToArchive
      (Zip.toEntry path 0 (LBS.fromStrict $ Text.Encoding.encodeUtf8 content))
      archive

-- | Build a multipart request: arbitrary text fields + a "sources" zip.
multipartReq :: Method -> String -> [(Text, Text)] -> LBS.ByteString -> IO Request
multipartReq httpMethod url textFields zipBytes = do
  req <- parseRequest url
  let boundary = "----PutGuardBoundary"
      crlf = "\r\n"
      dd = "--"
      field (k, v) =
        [ dd <> boundary <> crlf
        , "Content-Disposition: form-data; name=\"" <> Text.Encoding.encodeUtf8 k <> "\"" <> crlf <> crlf
        , Text.Encoding.encodeUtf8 v <> crlf
        ]
      filePreamble =
        [ dd <> boundary <> crlf
        , "Content-Disposition: form-data; name=\"sources\"; filename=\"bundle.zip\"" <> crlf
        , "Content-Type: application/zip" <> crlf <> crlf
        ]
      body = LBS.fromChunks (concatMap field textFields)
               <> LBS.fromChunks filePreamble
               <> zipBytes
               <> LBS.fromChunks [crlf, dd <> boundary <> dd <> crlf]
  pure req
    { method = httpMethod
    , requestBody = RequestBodyLBS body
    , requestHeaders = [("Content-Type", "multipart/form-data; boundary=" <> boundary)]
    }

cleanDir :: FilePath -> IO ()
cleanDir path = do
  exists <- doesDirectoryExist path
  if exists then removeDirectoryRecursive path else pure ()
