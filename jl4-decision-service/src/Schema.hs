{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wall -Wno-orphans #-}

module Schema (
  serverOpenApi,
  ServerName,
) where

--

import Backend.Api
import Control.Lens hiding ((.=))
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import Data.Map (Map)
import qualified Data.Maybe as Maybe
import Data.OpenApi
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.TypeLits
import Servant
import Servant.OpenApi
import Server
import qualified LSP.L4.Viz.VizExpr as VizExpr

type ServerName = Text

serverOpenApi :: Maybe ServerName -> OpenApi
serverOpenApi serverName =
  annotateGraphVizParams $
    toOpenApi (Proxy :: Proxy Api)
      & info . title .~ "JL4 Function API"
      & info . version .~ "1.0"
      & info . description ?~ "API for invoking JL4 functions"
      & servers .~ Maybe.maybeToList ((\sName -> Server sName mempty mempty) <$> serverName)
 where
  annotateGraphVizParams =
    allOperations . parameters %~ fmap annotateParam

  annotateParam (Inline param)
    | param ^. name == "graphviz" =
        Inline $
          param
            & description
              ?~ "Set to true to include GraphViz DOT traces in the JSON response. Requires `trace=full`; defaults to false."
            & schema
              ?~ Inline
                ( mempty
                    & type_ ?~ OpenApiBoolean
                    & default_ ?~ Aeson.Bool False
                )
    | otherwise = Inline param
  annotateParam ref = ref

instance (KnownSymbol desc, HasOpenApi api) => HasOpenApi (OperationId desc :> api) where
  toOpenApi _ =
    toOpenApi (Proxy :: Proxy api)
      & allOperations . operationId %~ (Just (Text.pack (symbolVal (Proxy :: Proxy desc))) <>)

-- ----------------------------------------------------------------------------
-- Document and describe the Json schema using the OpenAPI standard
-- ----------------------------------------------------------------------------

instance ToParamSchema TraceLevel where
  toParamSchema _ = mempty
    & type_ ?~ OpenApiString
    & enum_ ?~ ["none", "full"]
    & description ?~ "Control evaluation trace detail level. 'none' (default) returns only results, 'full' returns results with trace."

instance ToSchema SimpleFunction where
  declareNamedSchema _ = do
    textRef <- declareSchemaRef (Proxy @Text)
    pure $
      NamedSchema (Just "Function") $
        mempty
          & title ?~ "Function"
          & type_ ?~ OpenApiObject
          & properties
            .~ [ ("type", textRef)
               ,
                 ( "function"
                 , Inline $
                    mempty
                      & properties
                        .~ [ ("name", textRef)
                           , ("description", textRef)
                           ]
                 )
               ]

-- This is correct, since we don't overwrite the
-- 'ToJSON' instance yet.
instance ToSchema SimpleResponse

-- This is correct, since we don't overwrite the
-- 'ToJSON' instance yet.
instance ToSchema EvaluatorError

-- This is correct, since we don't overwrite the
-- 'ToJSON' instance yet.
instance ToSchema ParameterMismatch

-- This is correct, since we don't overwrite the
-- 'ToJSON' instance yet.
instance ToSchema ResponseWithReason

-- This is correct, since we don't overwrite the
-- 'ToJSON' instance yet.
instance ToSchema Reasoning

-- This is correct, since we don't overwrite the
-- 'ToJSON' instance yet.
instance ToSchema ReasoningTree

-- This is correct, since we don't overwrite the
-- 'ToJSON' instance yet.
instance ToSchema ReasonNode

-- This is correct, since we don't overwrite the
-- 'ToJSON' instance yet.
instance ToSchema FnArguments

instance ToSchema QueryAtom
instance ToSchema QueryOutcome
instance ToSchema QueryImpact
instance ToSchema QueryInput
instance ToSchema QueryAsk
instance ToSchema QueryPlanResponse

instance ToSchema VizExpr.RenderAsLadderInfo where
  declareNamedSchema _ =
    pure $
      NamedSchema (Just "RenderAsLadderInfo") $
        mempty
          & type_ ?~ OpenApiObject
          & additionalProperties ?~ AdditionalPropertiesAllowed True

instance ToSchema PngImage where
  declareNamedSchema _ =
    pure $
      NamedSchema (Just "PngImage") $
        mempty
          & type_ ?~ OpenApiString
          & format ?~ "binary"
          & description ?~ "PNG image representing an evaluation trace"

instance ToSchema Function where
  declareNamedSchema _ = do
    textRef <- declareSchemaRef (Proxy @Text)
    parametersRef <- declareSchemaRef (Proxy @Parameters)
    evalBackendsRef <- declareSchemaRef (Proxy @[EvalBackend])
    pure $
      NamedSchema (Just "Function") $
        mempty
          & title ?~ "Function"
          & type_ ?~ OpenApiObject
          & required .~ ["type", "function"]
          & properties
            .~ [ ("type", textRef)
               ,
                 ( "function"
                 , Inline $
                    mempty
                      & required .~ ["name", "description", "supportedBackends", "parameters"] -- the keys to the Map in parametersRef
                      & properties
                        .~ [ ("name", textRef)
                           , ("description", textRef)
                           , ("supportedBackends", evalBackendsRef)
                           , ("parameters", parametersRef)
                           ]
                 )
               ]

instance ToSchema FunctionImplementation where
  declareNamedSchema _ = do
    implRef <- declareSchemaRef (Proxy @(Map EvalBackend Text))
    functionDeclRef <- declareSchemaRef (Proxy @Function)
    pure $
      NamedSchema (Just "Implementation") $
        mempty
          & title ?~ "Implementation"
          & type_ ?~ OpenApiObject
          & properties
            .~ [ ("declaration", functionDeclRef)
               , ("implementation", implRef)
               ]
          & required .~ ["declaration", "implementation"]

instance ToSchema Parameters where
  declareNamedSchema _ = do
    textSchema <- declareSchemaRef (Proxy @Text)
    textListSchema <- declareSchemaRef (Proxy @[Text])
    mapSchema <- declareSchemaRef (Proxy @(Map Text Parameter))
    pure $
      NamedSchema (Just "FunctionParameters") $
        mempty
          & type_ ?~ OpenApiObject
          & title ?~ "FunctionParameters"
          & properties
            .~ [ ("type", textSchema)
               , ("properties", mapSchema)
               , ("required", textListSchema)
               ]
          & required .~ ["type", "properties", "required"]

instance ToSchema Parameter where
  declareNamedSchema _ = do
    textSchema <- declareSchemaRef (Proxy @Text)
    mTextSchema <- declareSchemaRef (Proxy @(Maybe Text))
    textListSchema <- declareSchemaRef (Proxy @[Text])
    mTextListSchema <- declareSchemaRef (Proxy @(Maybe [Text]))
    nestedPropsSchema <- declareSchemaRef (Proxy @(Maybe (Map Text Parameter)))
    itemsSchema <- declareSchemaRef (Proxy @(Maybe Parameter))
    pure $
      NamedSchema (Just "FunctionParameter") $
        mempty
          & type_ ?~ OpenApiObject
          & title ?~ "Parameter"
          & properties
            .~ [ ("enum", textListSchema)
               , ("description", textSchema)
               , ("alias", mTextSchema)
               , ("type", textSchema)
               , ("properties", nestedPropsSchema)
               , ("propertyOrder", mTextListSchema)
               , ("items", itemsSchema)
               ]
          & required .~ ["type"]
          & example
            ?~ Aeson.object
              [ "enum" .= (["true", "false", "uncertain"] :: Aeson.Array)
              , "description" .= Aeson.String "Can a person walk?"
              , "alias" .= Aeson.String "w"
              , "type" .= Aeson.String "string"
              ]

instance ToParamSchema FnLiteral where
  toParamSchema _ =
    mempty
      & title ?~ "Argument"
      -- Even though this is strictly speaking not *only* a string, custom GPT seem
      -- to need this, otherwise they will fail to send any requests to any endpoint with
      -- this query parameter.
      & type_ ?~ OpenApiString
      & example ?~ Aeson.String "true"
      & description ?~ "A Function argument which can be either 'true' or 'false', or a floating point number. Additionally accepts 'yes' and 'no' as synonyms for 'true' and 'false' respectively."

instance ToSchema FnLiteral where
  declareNamedSchema p = do
    intSchema <- declareSchemaRef (Proxy @Int)
    mTextSchema <- declareSchemaRef (Proxy @Text)
    fracSchema <- declareSchemaRef (Proxy @Double)
    boolSchema <- declareSchemaRef (Proxy @Bool)
    fnLiteralSchema <- declareSchemaRef (Proxy @FnLiteral)
    pure $
      NamedSchema (Just "Literal") $
        toParamSchema p
          & oneOf
            ?~ [ intSchema
               , mTextSchema
               , fracSchema
               , boolSchema
               , Inline $
                  mempty
                    & type_ ?~ OpenApiObject
                    & properties .~ []
                    & additionalProperties ?~ AdditionalPropertiesAllowed True
               , Inline $
                  mempty & type_ ?~ OpenApiNull
               , Inline $
                  mempty
                    & type_ ?~ OpenApiArray
                    & items ?~ OpenApiItemsObject fnLiteralSchema
               ]

instance ToSchema EvalBackend where
  declareNamedSchema p = do
    pure $
      NamedSchema (Just "EvalBackend") $
        toParamSchema p

instance ToParamSchema EvalBackend where
  toParamSchema _ =
    mempty
      & type_ ?~ OpenApiString
      & title ?~ "Evaluation Backends"
      & example ?~ Aeson.String "jl4"
      & default_ ?~ Aeson.String "hl4"
      & enum_ ?~ [Aeson.String "jl4"]
      & description ?~ "Backend for evaluation of a function. Backends can greatly affect the explanation quality. Additionally, backends may or may not support parts of natural4."

instance ToParamSchema (Map Text FnLiteral) where
  toParamSchema _ =
    mempty
      & type_ ?~ OpenApiObject
      & title ?~ "Function Arguments"
      & example
        ?~ Aeson.Object
          [ "drinks" .= Aeson.String "true"
          , "eats" .= Aeson.String "true"
          , "walks" .= Aeson.String "false"
          , "amount" .= Aeson.Number 2.0
          ]
      & description ?~ "Provide arguments to the function to be invoked."

instance ToSchema BatchRequest where
  declareNamedSchema _ = do
    textRef <- declareSchemaRef (Proxy @Text)
    let
      intRef =
        Inline $
          toParamSchema (Proxy @Int)
            & default_ ?~ Aeson.Number 0
            & example ?~ Aeson.Number 0
    pure $
      NamedSchema (Just "BatchRequest") $
        mempty
          & type_ ?~ OpenApiObject
          & title ?~ "Batch Request"
          & properties
            .~ [
                 ( "outcomes"
                 , Inline $
                    mempty
                      & type_ ?~ OpenApiArray
                      & items
                        ?~ OpenApiItemsObject
                          ( Inline $
                              mempty
                                & oneOf
                                  ?~ [ textRef
                                     , Inline $
                                        mempty
                                          & type_ ?~ OpenApiObject
                                          & additionalProperties ?~ AdditionalPropertiesAllowed True
                                          & properties
                                            .~ [ ("@id", textRef)
                                               ]
                                     ]
                          )
                 )
               ,
                 ( "cases"
                 , Inline $
                    mempty
                      & type_ ?~ OpenApiArray
                      & items
                        ?~ OpenApiItemsObject
                          ( Inline $
                              mempty
                                & additionalProperties ?~ AdditionalPropertiesAllowed True
                                & type_ ?~ OpenApiObject
                                & properties
                                  .~ [ ("@id", intRef)
                                     ]
                          )
                 )
               ]

instance ToSchema BatchResponse where
  declareNamedSchema _ = do
    let
      intRef =
        Inline $
          toParamSchema (Proxy @Int)
            & default_ ?~ Aeson.Number 0
            & example ?~ Aeson.Number 0
    doubleRef <- declareSchemaRef (Proxy @Double)
    graphvizRef <- declareSchemaRef (Proxy @GraphVizResponse)
    pure $
      NamedSchema (Just "BatchResponse") $
        mempty
          & type_ ?~ OpenApiObject
          & title ?~ "Batch Response"
          & properties
            .~ [
                 ( "cases"
                 , Inline $
                    mempty
                      & type_ ?~ OpenApiArray
                      & items
                        ?~ OpenApiItemsObject
                              ( Inline $
                                  mempty
                                    & type_ ?~ OpenApiObject
                                    & additionalProperties ?~ AdditionalPropertiesAllowed True
                                    & properties
                                      .~ [ ("@id", intRef)
                                         , ("@graphviz", graphvizRef)
                                         ]
                          )
                 )
               ,
                 ( "summary"
                 , Inline $
                    mempty
                      & type_ ?~ OpenApiArray
                      & type_ ?~ OpenApiObject
                      & properties
                        .~ [ ("casesRead", intRef)
                           , ("casesProcessed", intRef)
                           , ("casesIgnored", intRef)
                           , ("processorDurationSec", doubleRef)
                           , ("processorCasesPerSec", doubleRef)
                           , ("processorQueuedSec", doubleRef)
                           ]
                 )
               ]
