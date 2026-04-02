{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module IntegrationSpec (spec) where

import Test.Hspec

import Application (app)
import Backend.Api
import qualified BundleStore
import BundleStore (initStore)
import Compiler (compileBundle, computeVersion)
import ControlPlane (DeploymentStatusResponse (..))
import Logging (newLogger)
import Options (Options (..))
import Types

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (newTVarIO)
import Control.Exception (try)
import Data.Foldable (toList)
import qualified Codec.Archive.Zip as Zip
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Aeson.Key
import qualified Data.Aeson.KeyMap as Aeson.KeyMap
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Maybe as Maybe
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

import TestData (qualifiesJL4, recordJL4, maybeParamJL4, saleContractJL4, deonticExportJL4, deonticRecordPartyJL4, spacedFieldsJL4)

spec :: SpecWith ()
spec = describe "integration" do
  describe "data plane (direct compilation)" do
    it "evaluates a deployed function with all args true" do
      withServiceFromSources "eval-true" [("qualifies.l4", qualifiesJL4)] \baseUrl mgr -> do
        resp <- evalFunction baseUrl mgr "eval-true" "compute_qualifies"
          (Aeson.object
            [ "arguments" Aeson..= Aeson.object
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
            [ "arguments" Aeson..= Aeson.object
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

  describe "record output with named fields" do
    it "returns record fields as named object keys" do
      withServiceFromSources "record-named" [("record.l4", recordJL4)] \baseUrl mgr -> do
        resp <- evalFunction baseUrl mgr "record-named" "make_person"
          (Aeson.object
            [ "arguments" Aeson..= Aeson.object
                [ "n" Aeson..= ("Alice" :: Text)
                , "a" Aeson..= (30 :: Int)
                ]
            ])
        assertSuccess resp \r -> do
          let mValue = Map.lookup "value" r.fnResult
          case mValue of
            Just (FnObject [(conName, FnObject fields)]) -> do
              conName `shouldBe` "Person"
              let fieldMap = Map.fromList fields
              Map.lookup "name" fieldMap `shouldBe` Just (FnLitString "Alice")
              Map.lookup "age" fieldMap `shouldBe` Just (FnLitInt 30)
            other ->
              expectationFailure ("Expected FnObject with named fields, got: " <> show other)

  describe "MAYBE parameter handling" do
    it "handles NOTHING (null) for a MAYBE parameter" do
      withServiceFromSources "maybe-null" [("maybe.l4", maybeParamJL4)] \baseUrl mgr -> do
        resp <- evalFunction baseUrl mgr "maybe-null" "with_maybe"
          (Aeson.object
            [ "arguments" Aeson..= Aeson.object
                [ "label" Aeson..= ("test" :: Text)
                , "extra" Aeson..= Aeson.Null
                ]
            ])
        assertSuccess resp \r -> do
          let mValue = Map.lookup "value" r.fnResult
          case mValue of
            Just (FnObject [(conName, FnObject fields)]) -> do
              conName `shouldBe` "Result"
              let fieldMap = Map.fromList fields
              Map.lookup "label" fieldMap `shouldBe` Just (FnLitString "test")
              Map.lookup "extra_provided" fieldMap `shouldBe` Just (FnLitBool False)
            other ->
              expectationFailure ("Expected FnObject with named fields, got: " <> show other)

    it "handles JUST (non-null) for a MAYBE parameter" do
      withServiceFromSources "maybe-just" [("maybe.l4", maybeParamJL4)] \baseUrl mgr -> do
        resp <- evalFunction baseUrl mgr "maybe-just" "with_maybe"
          (Aeson.object
            [ "arguments" Aeson..= Aeson.object
                [ "label" Aeson..= ("test" :: Text)
                , "extra" Aeson..= ("hello" :: Text)
                ]
            ])
        assertSuccess resp \r -> do
          let mValue = Map.lookup "value" r.fnResult
          case mValue of
            Just (FnObject [(conName, FnObject fields)]) -> do
              conName `shouldBe` "Result"
              let fieldMap = Map.fromList fields
              Map.lookup "label" fieldMap `shouldBe` Just (FnLitString "test")
              Map.lookup "extra_provided" fieldMap `shouldBe` Just (FnLitBool True)
            other ->
              expectationFailure ("Expected FnObject with named fields, got: " <> show other)

  describe "field name sanitization (hyphen remapping)" do
    it "accepts hyphenated field names and hyphenated function name in URL" do
      withServiceFromSources "hyphen-eval" [("spaced.l4", spacedFieldsJL4)] \baseUrl mgr -> do
        -- Use hyphenated function name and field keys (as advertised in schemas)
        resp <- evalFunction baseUrl mgr "hyphen-eval" "check-person"
          (Aeson.object
            [ "arguments" Aeson..= Aeson.object
                [ "first-name" Aeson..= ("Alice" :: Text)
                , "is-a-citizen" Aeson..= True
                ]
            ])
        assertSuccess resp \r ->
          Map.lookup "value" r.fnResult `shouldBe` Just (FnLitBool True)

    it "accepts URL-encoded spaced function name (percent encoding)" do
      withServiceFromSources "urlenc-eval" [("spaced.l4", spacedFieldsJL4)] \baseUrl mgr -> do
        resp <- evalFunction baseUrl mgr "urlenc-eval" "check%20person"
          (Aeson.object
            [ "arguments" Aeson..= Aeson.object
                [ "first-name" Aeson..= ("Alice" :: Text)
                , "is-a-citizen" Aeson..= True
                ]
            ])
        assertSuccess resp \r ->
          Map.lookup "value" r.fnResult `shouldBe` Just (FnLitBool True)

    it "accepts plus-encoded spaced function name" do
      withServiceFromSources "plus-eval" [("spaced.l4", spacedFieldsJL4)] \baseUrl mgr -> do
        resp <- evalFunction baseUrl mgr "plus-eval" "check+person"
          (Aeson.object
            [ "arguments" Aeson..= Aeson.object
                [ "first-name" Aeson..= ("Alice" :: Text)
                , "is-a-citizen" Aeson..= True
                ]
            ])
        assertSuccess resp \r ->
          Map.lookup "value" r.fnResult `shouldBe` Just (FnLitBool True)

    it "accepts original spaced field names in REST API" do
      withServiceFromSources "space-eval" [("spaced.l4", spacedFieldsJL4)] \baseUrl mgr -> do
        -- Use original spaced keys (backwards compatibility)
        resp <- evalFunction baseUrl mgr "space-eval" "check%20person"
          (Aeson.object
            [ "arguments" Aeson..= Aeson.object
                [ "first name" Aeson..= ("Bob" :: Text)
                , "is a citizen" Aeson..= False
                ]
            ])
        assertSuccess resp \r ->
          Map.lookup "value" r.fnResult `shouldBe` Just (FnLitBool False)

    it "returns original spaced field names in OpenAPI schema" do
      withServiceFromSources "openapi-san" [("spaced.l4", spacedFieldsJL4)] \baseUrl mgr -> do
        -- Use /deployments?functions=full to check parameter schemas
        req <- parseRequest (baseUrl <> "/deployments?functions=full")
        resp <- httpLbs req mgr
        statusCode' resp `shouldBe` 200
        let body = responseBody resp
        case Aeson.decode body of
          Just (Aeson.Array deps) -> do
            length deps `shouldSatisfy` (> 0)
            let dep = case toList deps of { (x:_) -> x; [] -> error "empty deployments" }
                mFns = case dep of
                  Aeson.Object o -> case Aeson.KeyMap.lookup "metadata" o of
                    Just (Aeson.Object m) -> Aeson.KeyMap.lookup "functions" m
                    _ -> Nothing
                  _ -> Nothing
            case mFns of
              Just (Aeson.Array fns) -> do
                length fns `shouldSatisfy` (> 0)
                let fn = case toList fns of { (x:_) -> x; [] -> error "empty functions" }
                    mParams = case fn of
                      Aeson.Object o -> Aeson.KeyMap.lookup "parameters" o
                      _ -> Nothing
                    mProps = case mParams of
                      Just (Aeson.Object p) -> Aeson.KeyMap.lookup "properties" p
                      _ -> Nothing
                case mProps of
                  Just (Aeson.Object props) -> do
                    -- Deployments endpoint preserves original L4 names (spaces, not hyphens)
                    Aeson.KeyMap.member "first name" props `shouldBe` True
                    Aeson.KeyMap.member "is a citizen" props `shouldBe` True
                  other ->
                    expectationFailure ("Expected properties object, got: " <> show other)
              other ->
                expectationFailure ("Expected functions array, got: " <> show other)
          other ->
            expectationFailure ("Expected deployments array, got: " <> show other)

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

  describe "query-plan" do
    it "returns a query plan with no bindings" do
      withServiceFromSources "qp-empty" [("qualifies.l4", qualifiesJL4)] \baseUrl mgr -> do
        resp <- queryPlan' baseUrl mgr "qp-empty" "compute_qualifies"
          (Aeson.object ["arguments" Aeson..= Aeson.object []])
        statusCode' resp `shouldBe` 200
        let body = decodeObject (responseBody resp)
        lookupKey "determined" body `shouldBe` Just Aeson.Null
        lookupArrayLength "stillNeeded" body `shouldSatisfy` maybe False (> 0)
        lookupArrayLength "asks" body `shouldSatisfy` maybe False (> 0)

    it "determines True when all args are true" do
      withServiceFromSources "qp-all-true" [("qualifies.l4", qualifiesJL4)] \baseUrl mgr -> do
        resp <- queryPlan' baseUrl mgr "qp-all-true" "compute_qualifies"
          (Aeson.object
            [ "arguments" Aeson..= Aeson.object
                [ "walks" Aeson..= True
                , "eats" Aeson..= True
                , "drinks" Aeson..= True
                ]
            ])
        statusCode' resp `shouldBe` 200
        let body = decodeObject (responseBody resp)
        lookupKey "determined" body `shouldBe` Just (Aeson.Bool True)
        lookupArrayLength "stillNeeded" body `shouldBe` Just 0

    it "determines False when one arg is false" do
      withServiceFromSources "qp-one-false" [("qualifies.l4", qualifiesJL4)] \baseUrl mgr -> do
        resp <- queryPlan' baseUrl mgr "qp-one-false" "compute_qualifies"
          (Aeson.object
            [ "arguments" Aeson..= Aeson.object
                [ "walks" Aeson..= False
                ]
            ])
        statusCode' resp `shouldBe` 200
        let body = decodeObject (responseBody resp)
        lookupKey "determined" body `shouldBe` Just (Aeson.Bool False)

    it "is undetermined with partial true bindings" do
      withServiceFromSources "qp-partial" [("qualifies.l4", qualifiesJL4)] \baseUrl mgr -> do
        resp <- queryPlan' baseUrl mgr "qp-partial" "compute_qualifies"
          (Aeson.object
            [ "arguments" Aeson..= Aeson.object
                [ "walks" Aeson..= True
                , "eats" Aeson..= True
                ]
            ])
        statusCode' resp `shouldBe` 200
        let body = decodeObject (responseBody resp)
        lookupKey "determined" body `shouldBe` Just Aeson.Null
        lookupArrayLength "stillNeeded" body `shouldSatisfy` maybe False (> 0)

    it "caches the query plan across requests" do
      withServiceFromSources "qp-cache" [("qualifies.l4", qualifiesJL4)] \baseUrl mgr -> do
        -- First request builds the cache
        resp1 <- queryPlan' baseUrl mgr "qp-cache" "compute_qualifies"
          (Aeson.object ["arguments" Aeson..= Aeson.object []])
        statusCode' resp1 `shouldBe` 200

        -- Second request should use the cached version
        resp2 <- queryPlan' baseUrl mgr "qp-cache" "compute_qualifies"
          (Aeson.object
            [ "arguments" Aeson..= Aeson.object
                [ "walks" Aeson..= True
                , "eats" Aeson..= True
                , "drinks" Aeson..= True
                ]
            ])
        statusCode' resp2 `shouldBe` 200
        let body = decodeObject (responseBody resp2)
        lookupKey "determined" body `shouldBe` Just (Aeson.Bool True)

    it "returns 404 for unknown function" do
      withServiceFromSources "qp-404" [("qualifies.l4", qualifiesJL4)] \baseUrl mgr -> do
        resp <- queryPlan' baseUrl mgr "qp-404" "no_such_fn"
          (Aeson.object ["arguments" Aeson..= Aeson.object []])
        statusCode' resp `shouldBe` 404

  describe "evaluation with trace" do
    it "includes reasoning when trace=full" do
      withServiceFromSources "trace-full" [("qualifies.l4", qualifiesJL4)] \baseUrl mgr -> do
        req <- buildJsonPost
          (baseUrl <> "/deployments/trace-full/functions/compute_qualifies/evaluation?trace=full")
          (Aeson.object
            [ "arguments" Aeson..= Aeson.object
                [ "walks" Aeson..= True
                , "eats" Aeson..= True
                , "drinks" Aeson..= True
                ]
            ])
        resp <- httpLbs req mgr
        assertSuccess resp \r -> do
          -- trace=full should produce a non-trivial reasoning tree
          let ResponseWithReason{reasoning = tree} = r
          isEmptyReasoning tree `shouldBe` False

    it "includes graphviz DOT when trace=full and graphviz=true" do
      withServiceFromSources "trace-gv" [("qualifies.l4", qualifiesJL4)] \baseUrl mgr -> do
        req <- buildJsonPost
          (baseUrl <> "/deployments/trace-gv/functions/compute_qualifies/evaluation?trace=full&graphviz=true")
          (Aeson.object
            [ "arguments" Aeson..= Aeson.object
                [ "walks" Aeson..= True
                , "eats" Aeson..= True
                , "drinks" Aeson..= True
                ]
            ])
        resp <- httpLbs req mgr
        assertSuccess resp \r ->
          case r.graphviz of
            Just gv ->
              gv.dot `shouldSatisfy` Text.isInfixOf "digraph"
            Nothing ->
              expectationFailure "Expected graphviz to be present with trace=full&graphviz=true"

    it "omits graphviz when trace=none" do
      withServiceFromSources "trace-none" [("qualifies.l4", qualifiesJL4)] \baseUrl mgr -> do
        req <- buildJsonPost
          (baseUrl <> "/deployments/trace-none/functions/compute_qualifies/evaluation?trace=none")
          (Aeson.object
            [ "arguments" Aeson..= Aeson.object
                [ "walks" Aeson..= True
                , "eats" Aeson..= True
                , "drinks" Aeson..= True
                ]
            ])
        resp <- httpLbs req mgr
        assertSuccess resp \r ->
          r.graphviz `shouldBe` Nothing

  describe "function details and metadata" do
    it "returns function schema via GET" do
      withServiceFromSources "fn-details" [("qualifies.l4", qualifiesJL4)] \baseUrl mgr -> do
        req <- parseRequest (baseUrl <> "/deployments/fn-details/functions/compute_qualifies")
        resp <- httpLbs req mgr
        statusCode' resp `shouldBe` 200
        let body = decodeObject (responseBody resp)
        -- FunctionSummary has name, parameters, returnType
        lookupKey "name" body `shouldBe` Just (Aeson.String "compute_qualifies")
        lookupKey "returnType" body `shouldSatisfy` Maybe.isJust

    it "returns OpenAPI 3.0 spec via per-deployment openapi.json" do
      withServiceFromSources "openapi" [("qualifies.l4", qualifiesJL4)] \baseUrl mgr -> do
        req <- parseRequest (baseUrl <> "/deployments/openapi/openapi.json")
        resp <- httpLbs req mgr
        statusCode' resp `shouldBe` 200
        let body = decodeObject (responseBody resp)
        lookupKey "openapi" body `shouldBe` Just (Aeson.String "3.0.0")
        lookupKey "paths" body `shouldSatisfy` Maybe.isJust

    it "returns OpenAPI 3.0 spec via org-wide /openapi.json" do
      withServiceFromSources "org-openapi" [("qualifies.l4", qualifiesJL4)] \baseUrl mgr -> do
        req <- parseRequest (baseUrl <> "/openapi.json")
        resp <- httpLbs req mgr
        statusCode' resp `shouldBe` 200
        let body = decodeObject (responseBody resp)
        lookupKey "openapi" body `shouldBe` Just (Aeson.String "3.0.0")
        lookupKey "paths" body `shouldSatisfy` Maybe.isJust

    it "filters deployments by scope" do
      withServiceFromSources "scope-test" [("qualifies.l4", qualifiesJL4)] \baseUrl mgr -> do
        -- With matching scope
        req1 <- parseRequest (baseUrl <> "/deployments?functions=full&scope=scope-test/*")
        resp1 <- httpLbs req1 mgr
        statusCode' resp1 `shouldBe` 200
        case Aeson.decode (responseBody resp1) of
          Just (Aeson.Array deps) -> length deps `shouldSatisfy` (> 0)
          _ -> expectationFailure "Expected non-empty deployments array"
        -- With non-matching scope
        req2 <- parseRequest (baseUrl <> "/deployments?functions=full&scope=nonexistent/*")
        resp2 <- httpLbs req2 mgr
        statusCode' resp2 `shouldBe` 200
        case Aeson.decode (responseBody resp2) of
          Just (Aeson.Array deps) -> length deps `shouldBe` 0
          _ -> expectationFailure "Expected empty deployments array"

  describe "deontic evaluation" do
    it "evaluates a deontic function to FULFILLED with all events" do
      withServiceFromSources "deontic-ok" [("contract.l4", deonticExportJL4)] \baseUrl mgr -> do
        resp <- evalFunction baseUrl mgr "deontic-ok" "the sale contract"
          (Aeson.object
            [ "arguments" Aeson..= Aeson.object []
            , "startTime" Aeson..= (0 :: Int)
            , "events" Aeson..=
                [ Aeson.object ["party" Aeson..= ("the seller" :: Text), "action" Aeson..= ("deliver the goods" :: Text), "at" Aeson..= (5 :: Int)]
                , Aeson.object ["party" Aeson..= ("the buyer" :: Text), "action" Aeson..= ("pay the invoice" :: Text), "at" Aeson..= (20 :: Int)]
                ]
            ])
        assertSuccess resp \r ->
          Map.lookup "value" r.fnResult `shouldBe` Just (FnLitString "FULFILLED")

    it "returns initial OBLIGATION with empty events" do
      withServiceFromSources "deontic-init" [("contract.l4", deonticExportJL4)] \baseUrl mgr -> do
        resp <- evalFunction baseUrl mgr "deontic-init" "the sale contract"
          (Aeson.object
            [ "arguments" Aeson..= Aeson.object []
            , "startTime" Aeson..= (0 :: Int)
            , "events" Aeson..= ([] :: [Aeson.Value])
            ])
        assertSuccess resp \r -> do
          let mValue = Map.lookup "value" r.fnResult
          case mValue of
            Just (FnObject [("OBLIGATION", FnObject fields)]) -> do
              let fieldMap = Map.fromList fields
              Map.lookup "modal" fieldMap `shouldBe` Just (FnLitString "MUST")
            other ->
              expectationFailure ("Expected OBLIGATION, got: " <> show other)

    it "returns residual OBLIGATION after partial events" do
      withServiceFromSources "deontic-partial" [("contract.l4", deonticExportJL4)] \baseUrl mgr -> do
        resp <- evalFunction baseUrl mgr "deontic-partial" "the sale contract"
          (Aeson.object
            [ "arguments" Aeson..= Aeson.object []
            , "startTime" Aeson..= (0 :: Int)
            , "events" Aeson..=
                [ Aeson.object ["party" Aeson..= ("the seller" :: Text), "action" Aeson..= ("deliver the goods" :: Text), "at" Aeson..= (5 :: Int)]
                ]
            ])
        assertSuccess resp \r -> do
          let mValue = Map.lookup "value" r.fnResult
          case mValue of
            Just (FnObject [("OBLIGATION", FnObject fields)]) -> do
              let fieldMap = Map.fromList fields
              Map.lookup "modal" fieldMap `shouldBe` Just (FnLitString "MUST")
            other ->
              expectationFailure ("Expected OBLIGATION for buyer to pay, got: " <> show other)

    it "returns 400 when events provided for non-deontic function" do
      withServiceFromSources "deontic-err" [("qualifies.l4", qualifiesJL4)] \baseUrl mgr -> do
        req <- buildJsonPost (baseUrl <> "/deployments/deontic-err/functions/compute_qualifies/evaluation")
          (Aeson.object
            [ "arguments" Aeson..= Aeson.object ["walks" Aeson..= True, "eats" Aeson..= True, "drinks" Aeson..= True]
            , "startTime" Aeson..= (0 :: Int)
            , "events" Aeson..= ([] :: [Aeson.Value])
            ])
        resp <- httpLbs req mgr
        statusCode' resp `shouldBe` 400
        let body = decodeObject (responseBody resp)
        lookupKey "error" body `shouldSatisfy` Maybe.isJust

    it "returns 400 when startTime missing for deontic function" do
      withServiceFromSources "deontic-no-st" [("contract.l4", deonticExportJL4)] \baseUrl mgr -> do
        req <- buildJsonPost (baseUrl <> "/deployments/deontic-no-st/functions/the%20sale%20contract/evaluation")
          (Aeson.object
            [ "arguments" Aeson..= Aeson.object []
            , "events" Aeson..= ([] :: [Aeson.Value])
            ])
        resp <- httpLbs req mgr
        statusCode' resp `shouldBe` 400
        let body = decodeObject (responseBody resp)
        lookupKey "error" body `shouldSatisfy` Maybe.isJust

    it "returns 400 when events missing for deontic function" do
      withServiceFromSources "deontic-no-ev" [("contract.l4", deonticExportJL4)] \baseUrl mgr -> do
        req <- buildJsonPost (baseUrl <> "/deployments/deontic-no-ev/functions/the%20sale%20contract/evaluation")
          (Aeson.object
            [ "arguments" Aeson..= Aeson.object []
            , "startTime" Aeson..= (0 :: Int)
            ])
        resp <- httpLbs req mgr
        statusCode' resp `shouldBe` 400
        let body = decodeObject (responseBody resp)
        lookupKey "error" body `shouldSatisfy` Maybe.isJust

    it "includes startTime and events in function schema for deontic function" do
      withServiceFromSources "deontic-schema" [("contract.l4", deonticExportJL4)] \baseUrl mgr -> do
        req <- parseRequest (baseUrl <> "/deployments/deontic-schema/functions/the%20sale%20contract")
        resp <- httpLbs req mgr
        statusCode' resp `shouldBe` 200
        let body = decodeObject (responseBody resp)
        -- Check that parameters include startTime and events in required
        case lookupKey "parameters" body of
          Just (Aeson.Object params) ->
            case Aeson.KeyMap.lookup "required" params of
              Just reqArr -> do
                let reqList = Aeson.decode (Aeson.encode reqArr) :: Maybe [Text]
                reqList `shouldSatisfy` maybe False (elem "startTime")
                reqList `shouldSatisfy` maybe False (elem "events")
              _ -> expectationFailure "Missing required array in parameters"
          _ -> expectationFailure "Missing parameters in response"

  describe "deontic evaluation with record-typed parties" do
    it "evaluates to FULFILLED when record-typed party wears seatbelt then drives" do
      withServiceFromSources "deontic-rec-ok" [("seatbelt.l4", deonticRecordPartyJL4)] \baseUrl mgr -> do
        resp <- evalFunction baseUrl mgr "deontic-rec-ok" "Seatbelt Requirement"
          (Aeson.object
            [ "arguments" Aeson..= Aeson.object
                [ "car" Aeson..= Aeson.object ["number of wheels" Aeson..= (4 :: Int)]
                , "driver" Aeson..= Aeson.object ["name" Aeson..= ("Alice" :: Text)]
                ]
            , "startTime" Aeson..= (0 :: Int)
            , "events" Aeson..=
                [ Aeson.object ["party" Aeson..= Aeson.object ["name" Aeson..= ("Alice" :: Text)], "action" Aeson..= ("wear seatbelt" :: Text), "at" Aeson..= (1 :: Int)]
                , Aeson.object ["party" Aeson..= Aeson.object ["name" Aeson..= ("Alice" :: Text)], "action" Aeson..= ("drive" :: Text), "at" Aeson..= (2 :: Int)]
                ]
            ])
        assertSuccess resp \r ->
          Map.lookup "value" r.fnResult `shouldBe` Just (FnLitString "FULFILLED")

    it "returns OBLIGATION when record-typed party drives without seatbelt" do
      withServiceFromSources "deontic-rec-obl" [("seatbelt.l4", deonticRecordPartyJL4)] \baseUrl mgr -> do
        resp <- evalFunction baseUrl mgr "deontic-rec-obl" "Seatbelt Requirement"
          (Aeson.object
            [ "arguments" Aeson..= Aeson.object
                [ "car" Aeson..= Aeson.object ["number of wheels" Aeson..= (4 :: Int)]
                , "driver" Aeson..= Aeson.object ["name" Aeson..= ("Alice" :: Text)]
                ]
            , "startTime" Aeson..= (0 :: Int)
            , "events" Aeson..=
                [ Aeson.object ["party" Aeson..= Aeson.object ["name" Aeson..= ("Alice" :: Text)], "action" Aeson..= ("drive" :: Text), "at" Aeson..= (1 :: Int)]
                ]
            ])
        assertSuccess resp \r -> do
          let mValue = Map.lookup "value" r.fnResult
          case mValue of
            Just (FnObject [("OBLIGATION", FnObject fields)]) -> do
              let fieldMap = Map.fromList fields
              Map.lookup "modal" fieldMap `shouldBe` Just (FnLitString "MUST")
            other ->
              expectationFailure ("Expected OBLIGATION, got: " <> show other)

    it "evaluates 3-wheeled car to FULFILLED without seatbelt" do
      withServiceFromSources "deontic-rec-3w" [("seatbelt.l4", deonticRecordPartyJL4)] \baseUrl mgr -> do
        resp <- evalFunction baseUrl mgr "deontic-rec-3w" "Seatbelt Requirement"
          (Aeson.object
            [ "arguments" Aeson..= Aeson.object
                [ "car" Aeson..= Aeson.object ["number of wheels" Aeson..= (3 :: Int)]
                , "driver" Aeson..= Aeson.object ["name" Aeson..= ("Bob" :: Text)]
                ]
            , "startTime" Aeson..= (0 :: Int)
            , "events" Aeson..=
                [ Aeson.object ["party" Aeson..= Aeson.object ["name" Aeson..= ("Bob" :: Text)], "action" Aeson..= ("drive" :: Text), "at" Aeson..= (1 :: Int)]
                ]
            ])
        assertSuccess resp \r ->
          Map.lookup "value" r.fnResult `shouldBe` Just (FnLitString "FULFILLED")

    it "includes record-typed party schema in events parameter" do
      withServiceFromSources "deontic-rec-sch" [("seatbelt.l4", deonticRecordPartyJL4)] \baseUrl mgr -> do
        req <- parseRequest (baseUrl <> "/deployments/deontic-rec-sch/functions/Seatbelt%20Requirement")
        resp <- httpLbs req mgr
        statusCode' resp `shouldBe` 200
        let body = decodeObject (responseBody resp)
        -- Check events parameter has object-typed party with "name" field
        case lookupKey "parameters" body of
          Just (Aeson.Object params) ->
            case Aeson.KeyMap.lookup "properties" params of
              Just (Aeson.Object props) ->
                case Aeson.KeyMap.lookup "events" props of
                  Just (Aeson.Object eventsParam) ->
                    case Aeson.KeyMap.lookup "items" eventsParam of
                      Just (Aeson.Object items) ->
                        case Aeson.KeyMap.lookup "properties" items of
                          Just (Aeson.Object eventProps) -> do
                            -- party should be an object type (record), not a string (enum)
                            case Aeson.KeyMap.lookup "party" eventProps of
                              Just (Aeson.Object partyParam) ->
                                Aeson.KeyMap.lookup "type" partyParam `shouldBe` Just (Aeson.String "object")
                              _ -> expectationFailure "Missing party in event properties"
                            -- action should be a string type (enum)
                            case Aeson.KeyMap.lookup "action" eventProps of
                              Just (Aeson.Object actionParam) ->
                                Aeson.KeyMap.lookup "type" actionParam `shouldBe` Just (Aeson.String "string")
                              _ -> expectationFailure "Missing action in event properties"
                          _ -> expectationFailure "Missing properties in events items"
                      _ -> expectationFailure "Missing items in events parameter"
                  _ -> expectationFailure "Missing events in properties"
              _ -> expectationFailure "Missing properties in parameters"
          _ -> expectationFailure "Missing parameters in response"

  describe "state graphs" do
    it "returns empty list for a boolean-only module" do
      withServiceFromSources "sg-empty" [("qualifies.l4", qualifiesJL4)] \baseUrl mgr -> do
        req <- parseRequest (baseUrl <> "/deployments/sg-empty/functions/compute_qualifies/state-graphs")
        resp <- httpLbs req mgr
        statusCode' resp `shouldBe` 200
        let body = decodeObject (responseBody resp)
        lookupArrayLength "graphs" body `shouldBe` Just 0

    it "returns state graphs for a module with regulative rules" do
      withServiceFromSources "sg-contract" [("contract.l4", saleContractJL4)] \baseUrl mgr -> do
        req <- parseRequest (baseUrl <> "/deployments/sg-contract/functions/test_fn/state-graphs")
        resp <- httpLbs req mgr
        statusCode' resp `shouldBe` 200
        let body = decodeObject (responseBody resp)
        lookupArrayLength "graphs" body `shouldSatisfy` maybe False (> 0)

    it "returns DOT text for a specific state graph" do
      withServiceFromSources "sg-dot" [("contract.l4", saleContractJL4)] \baseUrl mgr -> do
        -- First get the list to find a graph name
        listReq <- parseRequest (baseUrl <> "/deployments/sg-dot/functions/test_fn/state-graphs")
        listResp <- httpLbs listReq mgr
        statusCode' listResp `shouldBe` 200
        let listBody = decodeObject (responseBody listResp)
        case lookupKey "graphs" listBody of
          Just (Aeson.Array graphs) | not (null graphs) -> do
            -- Extract the first graph name
            case toList graphs of
              (Aeson.Object g : _) ->
                case Aeson.KeyMap.lookup "graphName" g of
                  Just (Aeson.String graphName) -> do
                    -- Fetch the DOT output
                    dotReq <- parseRequest (baseUrl <> "/deployments/sg-dot/functions/test_fn/state-graphs/" <> Text.unpack graphName)
                    dotResp <- httpLbs dotReq mgr
                    statusCode' dotResp `shouldBe` 200
                    let dotText = Text.Encoding.decodeUtf8 (LBS.toStrict (responseBody dotResp))
                    dotText `shouldSatisfy` Text.isInfixOf "digraph"
                  _ -> expectationFailure "Graph object missing graphName"
              _ -> expectationFailure "Expected graph object in array"
          _ -> expectationFailure "Expected non-empty graphs array"

    it "returns 404 for non-existent state graph" do
      withServiceFromSources "sg-404" [("contract.l4", saleContractJL4)] \baseUrl mgr -> do
        req <- parseRequest (baseUrl <> "/deployments/sg-404/functions/test_fn/state-graphs/nonexistent")
        resp <- httpLbs req mgr
        statusCode' resp `shouldBe` 404

  describe "visibility headers" do
    it "X-Include-Functions: false hides functions from deployment response" do
      withServiceFromSources "vis-fn" [("qualifies.l4", qualifiesJL4)] \baseUrl mgr -> do
        req <- parseRequest (baseUrl <> "/deployments")
        let req' = req { requestHeaders = [("X-Include-Functions", "false")] }
        resp <- httpLbs req' mgr
        statusCode' resp `shouldBe` 200
        case Aeson.decode (responseBody resp) of
          Just (Aeson.Array deps) -> do
            length deps `shouldSatisfy` (> 0)
            let dep = safeHead (toList deps)
                mFns = case dep of
                  Aeson.Object o -> case Aeson.KeyMap.lookup "metadata" o of
                    Just (Aeson.Object m) -> Aeson.KeyMap.lookup "functions" m
                    _ -> Nothing
                  _ -> Nothing
            -- functions should be absent (empty = omitted)
            mFns `shouldBe` Nothing
          _ -> expectationFailure "Expected deployments array"

    it "X-Include-Files: false hides files from deployment response" do
      withServiceFromSources "vis-fi" [("qualifies.l4", qualifiesJL4)] \baseUrl mgr -> do
        req <- parseRequest (baseUrl <> "/deployments")
        let req' = req { requestHeaders = [("X-Include-Files", "false")] }
        resp <- httpLbs req' mgr
        statusCode' resp `shouldBe` 200
        case Aeson.decode (responseBody resp) of
          Just (Aeson.Array deps) -> do
            length deps `shouldSatisfy` (> 0)
            let dep = safeHead (toList deps)
                mFiles = case dep of
                  Aeson.Object o -> case Aeson.KeyMap.lookup "metadata" o of
                    Just (Aeson.Object m) -> Aeson.KeyMap.lookup "files" m
                    _ -> Nothing
                  _ -> Nothing
            -- files should be absent
            mFiles `shouldBe` Nothing
          _ -> expectationFailure "Expected deployments array"

    it "X-Include-Evaluate: false hides evaluation paths from OpenAPI" do
      withServiceFromSources "vis-ev" [("qualifies.l4", qualifiesJL4)] \baseUrl mgr -> do
        req <- parseRequest (baseUrl <> "/openapi.json")
        let req' = req { requestHeaders = [("X-Include-Evaluate", "false")] }
        resp <- httpLbs req' mgr
        statusCode' resp `shouldBe` 200
        let body = decodeObject (responseBody resp)
        case lookupKey "paths" body of
          Just (Aeson.Object paths) -> do
            -- Should have function GET paths but no evaluation POST paths
            let pathKeys = map Aeson.Key.toText (Aeson.KeyMap.keys paths)
                hasEval = any (Text.isInfixOf "/evaluation") pathKeys
            hasEval `shouldBe` False
          _ -> expectationFailure "Expected paths object"

    it "X-Include-Evaluate: false hides evaluation tools from MCP" do
      withServiceFromSources "vis-mcp" [("qualifies.l4", qualifiesJL4)] \baseUrl mgr -> do
        let mcpReq = Aeson.object
              [ "jsonrpc" Aeson..= ("2.0" :: Text)
              , "id" Aeson..= (1 :: Int)
              , "method" Aeson..= ("tools/list" :: Text)
              ]
        req <- buildJsonPost (baseUrl <> "/.mcp") mcpReq
        let req' = req { requestHeaders = ("X-Include-Evaluate", "false") : requestHeaders req }
        resp <- httpLbs req' mgr
        statusCode' resp `shouldBe` 200
        let body = decodeObject (responseBody resp)
        -- result.tools should exist but have no function evaluation tools
        case lookupKey "result" body of
          Just (Aeson.Object result) ->
            case Aeson.KeyMap.lookup "tools" result of
              Just (Aeson.Array tools) ->
                -- Should have file tools but no function tools
                let toolNames = [ n | Aeson.Object t <- toList tools
                                    , Just (Aeson.String n) <- [Aeson.KeyMap.lookup "name" t] ]
                    hasEvalTool = any (\n -> n /= "list_files" && n /= "read_file"
                                          && n /= "search_identifier" && n /= "search_text") toolNames
                in hasEvalTool `shouldBe` False
              _ -> expectationFailure "Expected tools array"
          _ -> expectationFailure "Expected result object"

    it "X-Include-Functions: false hides all function tools from MCP" do
      withServiceFromSources "vis-mcp-fn" [("qualifies.l4", qualifiesJL4)] \baseUrl mgr -> do
        let mcpReq = Aeson.object
              [ "jsonrpc" Aeson..= ("2.0" :: Text)
              , "id" Aeson..= (1 :: Int)
              , "method" Aeson..= ("tools/list" :: Text)
              ]
        req <- buildJsonPost (baseUrl <> "/.mcp") mcpReq
        let req' = req { requestHeaders = ("X-Include-Functions", "false") : requestHeaders req }
        resp <- httpLbs req' mgr
        statusCode' resp `shouldBe` 200
        let body = decodeObject (responseBody resp)
        case lookupKey "result" body of
          Just (Aeson.Object result) ->
            case Aeson.KeyMap.lookup "tools" result of
              Just (Aeson.Array tools) ->
                -- Should only have file tools, no function evaluation tools
                let toolNames = [ n | Aeson.Object t <- toList tools
                                    , Just (Aeson.String n) <- [Aeson.KeyMap.lookup "name" t] ]
                    fileToolNames = ["list_files", "read_file", "search_identifier", "search_text"] :: [Text]
                    nonFileTools = filter (`notElem` fileToolNames) toolNames
                in nonFileTools `shouldBe` []
              _ -> expectationFailure "Expected tools array"
          _ -> expectationFailure "Expected result object"

    it "X-Include-Files: false hides file tools from MCP" do
      withServiceFromSources "vis-mcp-fi" [("qualifies.l4", qualifiesJL4)] \baseUrl mgr -> do
        let mcpReq = Aeson.object
              [ "jsonrpc" Aeson..= ("2.0" :: Text)
              , "id" Aeson..= (1 :: Int)
              , "method" Aeson..= ("tools/list" :: Text)
              ]
        req <- buildJsonPost (baseUrl <> "/.mcp") mcpReq
        let req' = req { requestHeaders = ("X-Include-Files", "false") : requestHeaders req }
        resp <- httpLbs req' mgr
        statusCode' resp `shouldBe` 200
        let body = decodeObject (responseBody resp)
        case lookupKey "result" body of
          Just (Aeson.Object result) ->
            case Aeson.KeyMap.lookup "tools" result of
              Just (Aeson.Array tools) ->
                -- Should have function tools but no file tools
                let toolNames = [ n | Aeson.Object t <- toList tools
                                    , Just (Aeson.String n) <- [Aeson.KeyMap.lookup "name" t] ]
                    fileToolNames = ["list_files", "read_file", "search_identifier", "search_text"] :: [Text]
                    hasFileTool = any (`elem` fileToolNames) toolNames
                in hasFileTool `shouldBe` False
              _ -> expectationFailure "Expected tools array"
          _ -> expectationFailure "Expected result object"

  describe "deployments query params" do
    it "?functions=full includes parameters in listing" do
      withServiceFromSources "qp-full" [("qualifies.l4", qualifiesJL4)] \baseUrl mgr -> do
        req <- parseRequest (baseUrl <> "/deployments?functions=full")
        resp <- httpLbs req mgr
        statusCode' resp `shouldBe` 200
        case Aeson.decode (responseBody resp) of
          Just (Aeson.Array deps) -> do
            length deps `shouldSatisfy` (> 0)
            let dep = safeHead (toList deps)
                hasFnParams = case dep of
                  Aeson.Object o -> case Aeson.KeyMap.lookup "metadata" o of
                    Just (Aeson.Object m) -> case Aeson.KeyMap.lookup "functions" m of
                      Just (Aeson.Array fns) | not (null fns) ->
                        case safeHead (toList fns) of
                          Aeson.Object fn -> Aeson.KeyMap.member "parameters" fn
                          _ -> False
                      _ -> False
                    _ -> False
                  _ -> False
            hasFnParams `shouldBe` True
          _ -> expectationFailure "Expected deployments array"

    it "default listing includes functions with name but not parameters" do
      withServiceFromSources "qp-default" [("qualifies.l4", qualifiesJL4)] \baseUrl mgr -> do
        req <- parseRequest (baseUrl <> "/deployments")
        resp <- httpLbs req mgr
        statusCode' resp `shouldBe` 200
        case Aeson.decode (responseBody resp) of
          Just (Aeson.Array deps) -> do
            length deps `shouldSatisfy` (> 0)
            let dep = safeHead (toList deps)
                check = case dep of
                  Aeson.Object o -> case Aeson.KeyMap.lookup "metadata" o of
                    Just (Aeson.Object m) -> case Aeson.KeyMap.lookup "functions" m of
                      Just (Aeson.Array fns) | not (null fns) ->
                        case safeHead (toList fns) of
                          Aeson.Object fn ->
                            -- Has name but parameters should be empty (simple mode)
                            Aeson.KeyMap.member "name" fn
                          _ -> False
                      _ -> False
                    _ -> False
                  _ -> False
            check `shouldBe` True
          _ -> expectationFailure "Expected deployments array"

    it "?scope= filters deployments" do
      withServiceFromSources "qp-scope" [("qualifies.l4", qualifiesJL4)] \baseUrl mgr -> do
        -- Matching scope
        req1 <- parseRequest (baseUrl <> "/deployments?scope=qp-scope")
        resp1 <- httpLbs req1 mgr
        statusCode' resp1 `shouldBe` 200
        case Aeson.decode (responseBody resp1) of
          Just (Aeson.Array deps) -> length deps `shouldSatisfy` (> 0)
          _ -> expectationFailure "Expected non-empty deployments array"
        -- Non-matching scope
        req2 <- parseRequest (baseUrl <> "/deployments?scope=nonexistent")
        resp2 <- httpLbs req2 mgr
        statusCode' resp2 `shouldBe` 200
        case Aeson.decode (responseBody resp2) of
          Just (Aeson.Array deps) -> length deps `shouldBe` 0
          _ -> expectationFailure "Expected empty deployments array"

  describe "lazy-load (compile on first request)" do
    it "GET /deployments/{id} compiles pending deployment and returns ready" do
      withPendingService "lazy-get" [("qualifies.l4", qualifiesJL4)] \baseUrl mgr -> do
        req <- parseRequest (baseUrl <> "/deployments/lazy-get")
        resp <- httpLbs req mgr
        statusCode' resp `shouldBe` 200
        let mStatus = Aeson.decode (responseBody resp) :: Maybe DeploymentStatusResponse
        case mStatus of
          Just s -> s.dsStatus `shouldBe` "ready"
          Nothing -> expectationFailure "Failed to decode deployment status response"

    it "evaluation on pending deployment compiles and succeeds in one request" do
      withPendingService "lazy-eval" [("qualifies.l4", qualifiesJL4)] \baseUrl mgr -> do
        resp <- evalFunction baseUrl mgr "lazy-eval" "compute_qualifies"
          (Aeson.object
            [ "arguments" Aeson..= Aeson.object
                [ "walks" Aeson..= True
                , "eats" Aeson..= True
                , "drinks" Aeson..= True
                ]
            ])
        assertSuccess resp \r ->
          Map.lookup "value" r.fnResult `shouldBe` Just (FnLitBool True)

    it "listing functions on pending deployment compiles and returns functions" do
      withPendingService "lazy-fns" [("qualifies.l4", qualifiesJL4)] \baseUrl mgr -> do
        req <- parseRequest (baseUrl <> "/deployments/lazy-fns/functions")
        resp <- httpLbs req mgr
        statusCode' resp `shouldBe` 200

    it "GET /deployments lists pending deployments without triggering compilation" do
      withPendingService "lazy-list" [("qualifies.l4", qualifiesJL4)] \baseUrl mgr -> do
        req <- parseRequest (baseUrl <> "/deployments")
        resp <- httpLbs req mgr
        statusCode' resp `shouldBe` 200
        let mList = Aeson.decode (responseBody resp) :: Maybe [DeploymentStatusResponse]
        case mList of
          Just ds -> do
            length ds `shouldBe` 1
            case ds of
              (d:_) -> d.dsStatus `shouldBe` "pending"
              [] -> expectationFailure "Expected at least one deployment"
          Nothing -> expectationFailure "Failed to decode deployment list"

    it "health endpoint shows pending count before compilation" do
      withPendingService "lazy-health" [("qualifies.l4", qualifiesJL4)] \baseUrl mgr -> do
        req <- parseRequest (baseUrl <> "/health")
        resp <- httpLbs req mgr
        statusCode' resp `shouldBe` 200
        let body = decodeObject (responseBody resp)
        lookupKey "deployments" body `shouldSatisfy` \case
          Just (Aeson.Object dObj) ->
            case Aeson.KeyMap.lookup "pending" dObj of
              Just (Aeson.Number n) -> n >= 1
              _ -> False
          _ -> False

  describe "compiler" do
    it "compiles valid L4 sources" do
      logger <- newLogger False
      let sources = Map.singleton "qualifies.l4" qualifiesJL4
      result <- compileBundle logger "test" sources
      case result of
        Left err -> expectationFailure ("Compilation failed: " <> Text.unpack err)
        Right (fns, meta, _bundles) -> do
          Map.size fns `shouldSatisfy` (> 0)
          length meta.metaFunctions `shouldSatisfy` (> 0)
          Text.length meta.metaVersion `shouldBe` 64  -- SHA-256 hex

    it "rejects empty bundles" do
      logger <- newLogger False
      let sources = Map.empty :: Map FilePath Text
      result <- compileBundle logger "test" sources
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

-- | Save sources to the BundleStore and register as DeploymentPending,
-- simulating a lazy-load restart. The sources exist on disk but are not compiled.
withPendingService :: Text -> [(FilePath, Text)] -> (String -> Manager -> IO a) -> IO a
withPendingService deployId sources act = do
  resOrExc <- try (withPendingService' deployId sources act)
  case resOrExc of
    Left ioe ->
      if isPermissionError ioe
        then pendingWith ("Skipping integration test (cannot bind sockets): " <> show ioe) >> pure undefined
        else do
          expectationFailure (show ioe)
          pure undefined
    Right result -> pure result

withPendingService' :: Text -> [(FilePath, Text)] -> (String -> Manager -> IO a) -> IO a
withPendingService' deployId sources act = do
  let tmpPath = "/tmp/jl4-service-test-" <> Text.unpack deployId
  cleanDir tmpPath
  store <- initStore tmpPath
  logger <- newLogger False

  -- Save sources to the BundleStore (so loadAndRegister can find them)
  let sourceMap = Map.fromList sources
      version = computeVersion sourceMap
      storedMeta = BundleStore.StoredMetadata
        { BundleStore.smVersion = version
        , BundleStore.smCreatedAt = "2026-01-01T00:00:00Z"
        }
  BundleStore.saveBundle store deployId sourceMap storedMeta

  -- Register as Pending (not compiled)
  registry <- newTVarIO $ Map.singleton (DeploymentId deployId) (DeploymentPending Nothing)
  let env = MkAppEnv registry store Nothing logger testOptions

  mgr <- newManager defaultManagerSettings
  testWithApplication (pure $ app env) \port -> do
    let baseUrl = "http://localhost:" <> show port
    result <- act baseUrl mgr
    cleanDir tmpPath
    pure result

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
  logger <- newLogger False

  -- Compile the bundle directly
  let sourceMap = Map.fromList sources
  result <- compileBundle logger "test" sourceMap
  (fns, meta) <- case result of
    Left err -> fail ("Test setup: compilation failed: " <> Text.unpack err)
    Right (f, m, _bundles) -> pure (f, m)

  -- Register directly in the TVar
  registry <- newTVarIO $ Map.singleton (DeploymentId deployId) (DeploymentReady fns meta)
  let env = MkAppEnv registry store Nothing logger testOptions

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
  logger <- newLogger False
  registry <- newTVarIO Map.empty
  let env = MkAppEnv registry store Nothing logger testOptions

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

-- | Query a function's query-plan via the API.
queryPlan' :: String -> Manager -> Text -> Text -> Aeson.Value -> IO (Response LBS.ByteString)
queryPlan' baseUrl mgr deployId fnName body = do
  req <- buildJsonPost (baseUrl <> "/deployments/" <> Text.unpack deployId <> "/functions/" <> Text.unpack fnName <> "/query-plan") body
  httpLbs req mgr

-- | Decode a JSON response body as an Aeson Object.
decodeObject :: LBS.ByteString -> Maybe Aeson.Object
decodeObject = Aeson.decode

-- | Look up a key in a Maybe Object.
lookupKey :: Text -> Maybe Aeson.Object -> Maybe Aeson.Value
lookupKey k = (>>= Aeson.KeyMap.lookup (Aeson.Key.fromText k))

-- | Look up a JSON array in a Maybe Object, returning its length.
lookupArrayLength :: Text -> Maybe Aeson.Object -> Maybe Int
lookupArrayLength k obj = do
  v <- lookupKey k obj
  arr <- Aeson.decode (Aeson.encode v) :: Maybe [Aeson.Value]
  Just (length arr)

-- | Safe head for lists — avoids partial function warning.
safeHead :: [a] -> a
safeHead (x:_) = x
safeHead [] = error "safeHead: empty list"

-- | Default options for tests.
testOptions :: Options
testOptions = Options
  { port = 0
  , storePath = "/tmp/jl4-service-test"
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
