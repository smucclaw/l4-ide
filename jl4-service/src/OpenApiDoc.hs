module OpenApiDoc (
  buildOpenApiDoc,
) where

import Shared (sanitizePropertyName)
import Types (DeploymentMetadata (..), FunctionSummary (..), Visibility (..))

import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Aeson.Key
import qualified Data.Aeson.KeyMap as Aeson.KeyMap
import Data.Text (Text)
import qualified Data.Text as Text

-- | Build a valid OpenAPI 3.0 document from deployment metadata.
--
-- The document describes the canonical API paths for each deployment's
-- functions and files, filtered by visibility headers:
--   showFunctions → GET /functions, GET /functions/{fn}
--   showEvaluate  → POST /evaluation, POST /evaluation/batch, POST /query-plan, GET /state-graphs
--   showFiles     → GET /files, GET /files/{path}
buildOpenApiDoc
  :: Maybe Text                    -- ^ Server name/URL (for OpenAPI servers list)
  -> Visibility                    -- ^ Which paths to include
  -> [(Text, DeploymentMetadata)]  -- ^ (deploymentId, metadata) pairs (already scope-filtered)
  -> Aeson.Value
buildOpenApiDoc mServerName vis deployments =
  Aeson.object $
    [ "openapi" .= ("3.0.0" :: Text)
    , "info" .= Aeson.object
        [ "title" .= ("L4 Deployments" :: Text)
        , "version" .= ("1.0.0" :: Text)
        ]
    , "paths" .= buildPaths vis deployments
    ]
    <> maybe [] (\s -> ["servers" .= [Aeson.object ["url" .= s]]]) mServerName
    <> ["components" .= buildComponents]

-- | Build the paths object from deployment metadata.
buildPaths :: Visibility -> [(Text, DeploymentMetadata)] -> Aeson.Value
buildPaths vis deployments =
  Aeson.Object $ Aeson.KeyMap.fromList $ concatMap (uncurry (buildDeploymentPaths vis)) deployments

-- | Build paths for a single deployment.
buildDeploymentPaths :: Visibility -> Text -> DeploymentMetadata -> [(Aeson.Key, Aeson.Value)]
buildDeploymentPaths vis deployId meta =
  functionListPath ++ functionPaths ++ filePaths
 where
  prefix = "/deployments/" <> deployId

  -- GET /deployments/{id}/functions — list all functions
  functionListPath
    | vis.showFunctions, not (null meta.metaFunctions) =
        [ ( Aeson.Key.fromText (prefix <> "/functions")
          , Aeson.object
              [ "get" .= Aeson.object
                  [ "summary" .= ("List functions in deployment " <> deployId :: Text)
                  , "operationId" .= ("listFunctions_" <> sanitizeOperationId deployId :: Text)
                  , "responses" .= standardResponses
                  ]
              ]
          )
        ]
    | otherwise = []

  -- Per-function paths
  functionPaths
    | vis.showFunctions = concatMap (buildFunctionPaths vis prefix deployId) meta.metaFunctions
    | otherwise = []

  -- File paths
  filePaths
    | vis.showFiles, not (null meta.metaFiles) =
        [ ( Aeson.Key.fromText (prefix <> "/files")
          , Aeson.object
              [ "get" .= Aeson.object
                  [ "summary" .= ("Browse L4 source files in deployment " <> deployId :: Text)
                  , "operationId" .= ("listFiles_" <> sanitizeOperationId deployId :: Text)
                  , "parameters" .= fileQueryParams
                  , "responses" .= standardResponses
                  ]
              ]
          )
        , ( Aeson.Key.fromText (prefix <> "/files/{path}")
          , Aeson.object
              [ "get" .= Aeson.object
                  [ "summary" .= ("Read L4 source file from deployment " <> deployId :: Text)
                  , "operationId" .= ("readFile_" <> sanitizeOperationId deployId :: Text)
                  , "parameters" .=
                      [ Aeson.object
                          [ "name" .= ("path" :: Text)
                          , "in" .= ("path" :: Text)
                          , "required" .= True
                          , "schema" .= Aeson.object ["type" .= ("string" :: Text)]
                          , "description" .= ("Relative file path (e.g. rules/main.l4)" :: Text)
                          ]
                      , Aeson.object
                          [ "name" .= ("lines" :: Text)
                          , "in" .= ("query" :: Text)
                          , "schema" .= Aeson.object ["type" .= ("string" :: Text)]
                          , "description" .= ("Line range, e.g. 10:20" :: Text)
                          ]
                      ]
                  , "responses" .= Aeson.object
                      [ "200" .= Aeson.object
                          [ "description" .= ("L4 source file content" :: Text)
                          , "content" .= Aeson.object
                              [ "text/plain" .= Aeson.object
                                  [ "schema" .= Aeson.object ["type" .= ("string" :: Text)]
                                  ]
                              ]
                          ]
                      ]
                  ]
              ]
          )
        ]
    | otherwise = []

-- | Build paths for a single function within a deployment.
buildFunctionPaths :: Visibility -> Text -> Text -> FunctionSummary -> [(Aeson.Key, Aeson.Value)]
buildFunctionPaths vis prefix deployId fn =
  schemaPath ++ evaluatePaths
 where
  sanitizedName = sanitizePropertyName fn.fsName
  fnPath = prefix <> "/functions/" <> sanitizedName
  opIdBase = sanitizeOperationId deployId <> "_" <> sanitizeOperationId fn.fsName

  -- GET /deployments/{id}/functions/{fn} — function schema
  schemaPath =
    [ ( Aeson.Key.fromText fnPath
      , Aeson.object
          [ "get" .= Aeson.object
              [ "summary" .= fn.fsDescription
              , "operationId" .= ("getFunction_" <> opIdBase :: Text)
              , "responses" .= standardResponses
              ]
          ]
      )
    ]

  -- POST paths (evaluation, batch, query-plan) + GET state-graphs — only when showEvaluate
  evaluatePaths
    | vis.showEvaluate =
        [ ( Aeson.Key.fromText (fnPath <> "/evaluation")
          , Aeson.object
              [ "post" .= Aeson.object
                  [ "summary" .= fn.fsDescription
                  , "operationId" .= ("evaluate_" <> opIdBase :: Text)
                  , "x-return-type" .= fn.fsReturnType
                  , "requestBody" .= Aeson.object
                      [ "required" .= True
                      , "content" .= Aeson.object
                          [ "application/json" .= Aeson.object
                              [ "schema" .= fn.fsParameters
                              ]
                          ]
                      ]
                  , "responses" .= Aeson.object
                      [ "200" .= Aeson.object
                          [ "description" .= ("Evaluation result" :: Text)
                          , "content" .= Aeson.object
                              [ "application/json" .= Aeson.object
                                  [ "schema" .= Aeson.object
                                      [ "$ref" .= ("#/components/schemas/EvaluationResponse" :: Text)
                                      ]
                                  ]
                              ]
                          ]
                      ]
                  ]
              ]
          )
        , ( Aeson.Key.fromText (fnPath <> "/evaluation/batch")
          , Aeson.object
              [ "post" .= Aeson.object
                  [ "summary" .= ("Batch evaluate " <> fn.fsName :: Text)
                  , "operationId" .= ("batchEvaluate_" <> opIdBase :: Text)
                  , "requestBody" .= Aeson.object
                      [ "required" .= True
                      , "content" .= Aeson.object
                          [ "application/json" .= Aeson.object
                              [ "schema" .= Aeson.object
                                  [ "$ref" .= ("#/components/schemas/BatchRequest" :: Text)
                                  ]
                              ]
                          ]
                      ]
                  , "responses" .= standardResponses
                  ]
              ]
          )
        , ( Aeson.Key.fromText (fnPath <> "/query-plan")
          , Aeson.object
              [ "post" .= Aeson.object
                  [ "summary" .= ("Query plan for " <> fn.fsName :: Text)
                  , "operationId" .= ("queryPlan_" <> opIdBase :: Text)
                  , "responses" .= standardResponses
                  ]
              ]
          )
        , ( Aeson.Key.fromText (fnPath <> "/state-graphs")
          , Aeson.object
              [ "get" .= Aeson.object
                  [ "summary" .= ("State graphs for " <> fn.fsName :: Text)
                  , "operationId" .= ("stateGraphs_" <> opIdBase :: Text)
                  , "responses" .= standardResponses
                  ]
              ]
          )
        ]
    | otherwise = []

-- | File query parameters for the /files endpoint.
fileQueryParams :: [Aeson.Value]
fileQueryParams =
  [ Aeson.object
      [ "name" .= ("file" :: Text), "in" .= ("query" :: Text)
      , "schema" .= Aeson.object ["type" .= ("string" :: Text)]
      , "description" .= ("Scope to a specific file" :: Text)
      ]
  , Aeson.object
      [ "name" .= ("identifier" :: Text), "in" .= ("query" :: Text)
      , "schema" .= Aeson.object ["type" .= ("string" :: Text)]
      , "description" .= ("Search for L4 identifier definitions and references" :: Text)
      ]
  , Aeson.object
      [ "name" .= ("search" :: Text), "in" .= ("query" :: Text)
      , "schema" .= Aeson.object ["type" .= ("string" :: Text)]
      , "description" .= ("Grep source files (case-insensitive)" :: Text)
      ]
  ]

-- | Standard 200 JSON response.
standardResponses :: Aeson.Value
standardResponses = Aeson.object
  [ "200" .= Aeson.object
      [ "description" .= ("Success" :: Text)
      , "content" .= Aeson.object
          [ "application/json" .= Aeson.object
              [ "schema" .= Aeson.object ["type" .= ("object" :: Text)]
              ]
          ]
      ]
  ]

-- | Shared component schemas.
buildComponents :: Aeson.Value
buildComponents = Aeson.object
  [ "schemas" .= Aeson.object
      [ "EvaluationResponse" .= Aeson.object
          [ "type" .= ("object" :: Text)
          , "properties" .= Aeson.object
              [ "tag" .= Aeson.object ["type" .= ("string" :: Text)]
              , "contents" .= Aeson.object ["type" .= ("object" :: Text)]
              ]
          ]
      , "BatchRequest" .= Aeson.object
          [ "type" .= ("object" :: Text)
          , "properties" .= Aeson.object
              [ "outcomes" .= Aeson.object ["type" .= ("array" :: Text)]
              , "cases" .= Aeson.object ["type" .= ("array" :: Text)]
              ]
          , "required" .= (["outcomes", "cases"] :: [Text])
          ]
      ]
  ]

-- | Sanitize a text for use as an OpenAPI operationId.
sanitizeOperationId :: Text -> Text
sanitizeOperationId = Text.map (\c -> if isIdChar c then c else '_')
 where
  isIdChar c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c == '_'
