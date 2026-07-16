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
import ControlPlane (DeploymentStatusResponse (..), UpdateStatusResponse (..))
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

-- | Does not parse — guarantees a compile failure.
brokenSrc :: Text
brokenSrc = [i|
@export default person qualifies
GIVEN walks IS A BOOLEAN
GIVETH A BOOLEAN
DECIDE compute_qualifies IF walks AND AND
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
      uid <- mustUpdateId r
      (st, _) <- pollJob baseUrl mgr "guard-nodesc" uid
      st `shouldBe` "applied"
      deploymentDescription baseUrl mgr "guard-nodesc" `shouldReturn` Nothing

  it "accepts and applies a compatible PUT (older client omits description)" do
    withService $ \baseUrl mgr -> do
      deployApplied baseUrl mgr "guard-compat" Nothing baseSrc

      -- Base logic is AND: one false ⇒ False.
      evalQualifies baseUrl mgr "guard-compat" False True True
        `shouldReturnResult` FnLitBool False

      r <- deployPut baseUrl mgr "guard-compat" Nothing compatSrc
      statusCode' r `shouldBe` 202
      -- PUT is async: 202 + a job id; the old version keeps serving
      -- until the job applies. The deployment's own status is unchanged.
      dsStatusOf r `shouldBe` Just "compiling"
      uid <- mustUpdateId r
      (st, _) <- pollJob baseUrl mgr "guard-compat" uid
      st `shouldBe` "applied"

      -- Compatible logic is OR: same inputs now ⇒ True.
      evalQualifies baseUrl mgr "guard-compat" False True True
        `shouldReturnResult` FnLitBool True

  it "rejects a breaking PUT via the job, leaving the deployment intact" do
    withService $ \baseUrl mgr -> do
      deployApplied baseUrl mgr "guard-break" Nothing baseSrc

      r <- deployPut baseUrl mgr "guard-break" Nothing breakingSrc
      -- Accepted for processing; rejection is on the job, not the
      -- deployment, and never via the PUT response.
      statusCode' r `shouldBe` 202
      uid <- mustUpdateId r
      (st, mErr) <- pollJob baseUrl mgr "guard-break" uid
      st `shouldBe` "rejected"
      let err = fromMaybe "" mErr
      err `shouldSatisfy` Text.isInfixOf "break"
      err `shouldSatisfy` Text.isInfixOf "type changed"

      -- Retained, not read-and-clear: a second poll still sees it.
      (st2, _) <- jobOnce baseUrl mgr "guard-break" uid
      st2 `shouldBe` "rejected"

      -- The deployment's own status stays truthful (never overridden):
      -- still "ready", still serving the original interface.
      deploymentStatusText baseUrl mgr "guard-break" `shouldReturn` "ready"
      evalQualifies baseUrl mgr "guard-break" True True True
        `shouldReturnResult` FnLitBool True

  it "POST overwrite that fails to compile leaves the old version intact" do
    withService $ \baseUrl mgr -> do
      deployApplied baseUrl mgr "guard-postfail" Nothing baseSrc

      r <- deployPost baseUrl mgr "guard-postfail" Nothing brokenSrc
      statusCode' r `shouldBe` 202
      uid <- mustUpdateId r
      (st, _) <- pollJob baseUrl mgr "guard-postfail" uid
      st `shouldBe` "rejected"

      deploymentStatusText baseUrl mgr "guard-postfail" `shouldReturn` "ready"
      evalQualifies baseUrl mgr "guard-postfail" True True True
        `shouldReturnResult` FnLitBool True

  it "PUT to a non-existent deployment is 404" do
    withService $ \baseUrl mgr -> do
      r <- deployPut baseUrl mgr "does-not-exist" Nothing baseSrc
      statusCode' r `shouldBe` 404

  it "preserves a description across a source-only PUT (older client omits it)" do
    withService $ \baseUrl mgr -> do
      deployApplied baseUrl mgr "guard-desc" (Just "Intended use: demo") baseSrc
      deploymentDescription baseUrl mgr "guard-desc"
        `shouldReturn` Just "Intended use: demo"

      r <- deployPut baseUrl mgr "guard-desc" Nothing compatSrc
      statusCode' r `shouldBe` 202
      uid <- mustUpdateId r
      (st, _) <- pollJob baseUrl mgr "guard-desc" uid
      st `shouldBe` "applied"
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
  tasksReg <- newTVarIO Map.empty
  let env = MkAppEnv registry pendingUpd store Nothing logger testOpts tasksReg
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

-- | Extract the async job id from a 202 deploy response, failing the
-- test if absent (a job-bearing deploy must carry one).
mustUpdateId :: Response LBS.ByteString -> IO Text
mustUpdateId r =
  case Aeson.decode (responseBody r) :: Maybe DeploymentStatusResponse of
    Just s | Just uid <- s.dsUpdateId -> pure uid
    _ -> expectationFailure "expected an updateId in the deploy response"
           >> pure ""

-- | Single GET /deployments/{id}/updates/{updateId} → (status, error).
jobOnce :: String -> Manager -> Text -> Text -> IO (Text, Maybe Text)
jobOnce baseUrl mgr deployId uid = do
  req <- parseRequest
    (baseUrl <> "/deployments/" <> Text.unpack deployId
              <> "/updates/" <> Text.unpack uid)
  resp <- httpLbs req mgr
  case Aeson.decode (responseBody resp) :: Maybe UpdateStatusResponse of
    Just u  -> pure (u.usStatus, u.usError)
    Nothing -> pure ("undecodable", Nothing)

-- | Poll a job until it reaches a terminal status (applied/rejected).
pollJob :: String -> Manager -> Text -> Text -> IO (Text, Maybe Text)
pollJob baseUrl mgr deployId uid = go (120 :: Int)
 where
  go 0 = pure ("timeout", Nothing)
  go n = do
    (st, mErr) <- jobOnce baseUrl mgr deployId uid
    if st == "applied" || st == "rejected"
      then pure (st, mErr)
      else threadDelay 500_000 >> go (n - 1)

-- | Single GET /deployments/{id} → status string (truthful live state).
deploymentStatusText :: String -> Manager -> Text -> IO Text
deploymentStatusText baseUrl mgr deployId = do
  req <- parseRequest (baseUrl <> "/deployments/" <> Text.unpack deployId)
  resp <- httpLbs req mgr
  case Aeson.decode (responseBody resp) :: Maybe DeploymentStatusResponse of
    Just s  -> pure s.dsStatus
    Nothing -> pure "undecodable"

-- | POST a bundle and block until its job has applied.
deployApplied :: String -> Manager -> Text -> Maybe Text -> Text -> IO ()
deployApplied baseUrl mgr deployId mDesc src = do
  r <- deployPost baseUrl mgr deployId mDesc src
  statusCode' r `shouldBe` 202
  uid <- mustUpdateId r
  (st, mErr) <- pollJob baseUrl mgr deployId uid
  st `shouldBe` "applied"
  -- (mErr is Nothing on success; surfaced here only if it isn't)
  maybe (pure ()) (\e -> expectationFailure ("unexpected: " <> Text.unpack e)) mErr

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
