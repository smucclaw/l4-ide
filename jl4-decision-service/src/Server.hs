{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Server (
  -- * AppM
  AppM,
  AppEnv (..),
  ValidatedFunction (..),

  -- * Servant
  OperationId,

  -- * REST API
  Api,
  FunctionApi,
  FunctionApi' (..),
  SingleFunctionApi,
  SingleFunctionApi' (..),
  FunctionCrud,
  FunctionCrud' (..),
  handler,

  -- * API json types
  Parameters (..),
  Parameter (..),
  Function (..),
  SimpleFunction (..),
  SimpleResponse (..),
  Reasoning (..),
  ReasoningTree (..),
  ResponseWithReason (..),
  EvaluatorError (..),
  FnLiteral (..),
  EvalBackend (..),
  FunctionImplementation (..),
  FnArguments (..),
  Outcomes (..),
  OutcomeObject (..),
  OutcomeStyle (..),
  BatchRequest (..),
  InputCase (..),
  OutputCase (..),
  BatchResponse (..),
  OutputSummary (..),
  QueryAtom (..),
  QueryOutcome (..),
  QueryImpact (..),
  QueryInput (..),
  QueryAsk (..),
  QueryPlanResponse (..),
  StateGraphListResponse (..),
  StateGraphInfo (..),
  StateGraphFormat (..),

  -- * utilities
  toDecl,
  parametersOfDecide,
) where

import Base
import Backend.Api as Api
import Backend.FunctionSchema (Parameter (..), Parameters (..))
import qualified Backend.Jl4 as Jl4
import Backend.GraphVizRender (isGraphVizAvailable, renderPNG, renderSVG)
import Backend.DecisionQueryPlan (CachedDecisionQuery, QueryAsk (..), QueryAtom (..), QueryImpact (..), QueryInput (..), QueryOutcome (..), QueryPlanResponse (..))
import qualified Backend.DecisionQueryPlan as DecisionQueryPlan

import qualified Chronos
import Control.Applicative
import Control.Concurrent.Async (forConcurrently)
import Control.Concurrent.STM
import Control.Exception (displayException, evaluate)
import Data.Aeson (FromJSON, ToJSON, (.:), (.=))
import qualified Data.Aeson as Aeson
import Data.Aeson.Combinators.Decode (Decoder)
import qualified Data.Aeson.Combinators.Decode as ACD
import qualified Data.Aeson.Key as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Fixed
import Data.Int
import qualified Data.Map.Strict as Map
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import Data.Scientific (Scientific)
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Tuple.Extra as Tuple
import qualified GHC.Clock as Clock
import GHC.TypeLits
import qualified Network.HTTP.Types.URI as URI
import Servant
import System.Timeout (timeout)
import Servant.Client.Core.HasClient

import Data.UUID (UUID)
import qualified Data.UUID as UUID
import Servant.Client (BaseUrl, ClientError, ClientM, runClientM, mkClientEnv)
import qualified L4.CRUD as CRUD
import Servant.Client.Generic (genericClient)
import Network.HTTP.Client (Manager)
import qualified Base.Text as T
import L4.Export (ExportedFunction (..), ExportedParam (..), getExportedFunctions)
import L4.Syntax
import L4.Print (prettyLayout)
import qualified L4.StateGraph as StateGraph
import Data.Function
import qualified Optics
import L4.Lexer
import qualified LSP.L4.Rules as Rules


-- ----------------------------------------------------------------------------
-- Servant API
-- ----------------------------------------------------------------------------

data AppEnv = MkAppEnv
  { functionDatabase :: TVar (Map Text ValidatedFunction)
  , baseUrl :: BaseUrl
  , manager :: Manager
  }
  deriving stock (Generic)

data ValidatedFunction = ValidatedFunction
  { fnImpl :: !Function
  , fnEvaluator :: !(Map EvalBackend RunFunction)
  , fnCompiled :: !(Maybe Jl4.CompiledModule)
  , fnSources :: !(Map EvalBackend Text)
  , fnDecisionQueryCache :: !(Maybe CachedDecisionQuery)
  }

type AppM = ReaderT AppEnv Handler
type Api = NamedRoutes FunctionApi'
type FunctionApi = NamedRoutes FunctionApi'

-- | API that can be invoked by a custom gpt.
--
-- See https://openai.com/index/introducing-gpts/
data FunctionApi' mode = FunctionApi
  { functionRoutes :: mode :- "functions" :> FunctionCrud
  }
  deriving stock (Generic)

type FunctionCrud = NamedRoutes FunctionCrud'

-- | API for interacting with the 'function' resource.
data FunctionCrud' mode = FunctionCrud
  { batchEntities ::
      mode
        :- Summary "Shortened descriptions of all available functions and their parameters"
          :> OperationId "getAllFunctions"
          :> Get '[JSON] [SimpleFunction]
  , singleEntity ::
      mode
        :- Capture "name" String
          :> SingleFunctionApi
  }
  deriving stock (Generic)

type SingleFunctionApi = NamedRoutes SingleFunctionApi'
data SingleFunctionApi' mode = SingleFunctionApi
  { getFunction ::
      mode
        :- Summary "Get a detailed description of the function and its parameters"
          :> OperationId "getFunction"
          :> Get '[JSON] Function
  , postFunction ::
      mode
        :- Summary "Add a function resource that can be evaluated."
          :> ReqBody '[JSON] FunctionImplementation
          :> OperationId "createFunction"
          :> Post '[JSON] ()
  , putFunction ::
      mode
        :- Summary "Update a function resource"
          :> ReqBody '[JSON] FunctionImplementation
          :> OperationId "updateFunction"
          :> Put '[JSON] ()
  , deleteFunction ::
      mode
        :- Summary "Delete the function"
          :> OperationId "deleteFunction"
          :> Delete '[JSON] ()
  , evalFunction ::
      mode
        :- "evaluation"
          :> Summary "Evaluate a function with arguments"
          :> Header "X-L4-Trace" Text
          :> QueryParam "trace" Api.TraceLevel
          :> QueryParam "graphviz" Bool
          :> ReqBody '[JSON] FnArguments
          :> OperationId "evalFunction"
          :> Post '[JSON] SimpleResponse
  , evalFunctionTracePNG ::
      mode
        :- "evaluation"
          :> "trace.png"
          :> Summary "Render evaluation trace as PNG"
          :> Header "X-L4-Trace" Text
          :> QueryParam "trace" Api.TraceLevel
          :> ReqBody '[JSON] FnArguments
          :> OperationId "evalFunctionTracePng"
          :> Post '[OctetStream] Api.PngImage
  , evalFunctionTraceSVG ::
      mode
        :- "evaluation"
          :> "trace.svg"
          :> Summary "Render evaluation trace as SVG"
          :> Header "X-L4-Trace" Text
          :> QueryParam "trace" Api.TraceLevel
          :> ReqBody '[JSON] FnArguments
          :> OperationId "evalFunctionTraceSvg"
          :> Post '[PlainText] Text
  , batchFunction ::
      mode
        :- "batch"
          :> Summary "Run a function using a batch of arguments"
          :> Description "Evaluate a function with a batch of arguments, conforming to Oracle Intelligent Advisor Batch API"
          :> Header "X-L4-Trace" Text
          :> QueryParam "trace" Api.TraceLevel
          :> QueryParam "graphviz" Bool
          :> ReqBody '[JSON] BatchRequest
          :> Post '[JSON] BatchResponse
  , queryPlan ::
      mode
        :- "query-plan"
          :> Summary "Suggest which boolean inputs to elicit next"
          :> Description "Returns remaining relevant boolean atoms (not necessarily original inputs), per-atom impact analysis, and a heuristic ranking of function parameters inferred from atom dependencies."
          :> ReqBody '[JSON] FnArguments
          :> OperationId "queryPlan"
          :> Post '[JSON] QueryPlanResponse
  -- ^ Run a function with a "batch" of parameters.
  -- This API aims to be consistent with
  -- https://docs.oracle.com/en/cloud/saas/b2c-service/opawx/using-batch-assess-rest-api.html
  , listStateGraphs ::
      mode
        :- "state-graphs"
          :> Summary "List all state transition graphs in this module"
          :> Description "Extracts state graphs from regulative rules (MUST/MAY/SHANT) in the L4 module"
          :> OperationId "listStateGraphs"
          :> Get '[JSON] StateGraphListResponse
  , getStateGraphDot ::
      mode
        :- "state-graphs"
          :> Capture "graphName" Text
          :> Summary "Get state transition graph as GraphViz DOT"
          :> OperationId "getStateGraphDot"
          :> Get '[PlainText] Text
  , getStateGraphSvg ::
      mode
        :- "state-graphs"
          :> Capture "graphName" Text
          :> "svg"
          :> Summary "Render state transition graph as SVG"
          :> OperationId "getStateGraphSvg"
          :> Get '[PlainText] Text
  , getStateGraphPng ::
      mode
        :- "state-graphs"
          :> Capture "graphName" Text
          :> "png"
          :> Summary "Render state transition graph as PNG"
          :> OperationId "getStateGraphPng"
          :> Get '[OctetStream] Api.PngImage
  }
  deriving stock (Generic)

-- Query-plan API types are defined in Backend.DecisionQueryPlan and re-exported here.

data SimpleFunction = SimpleFunction
  { simpleName :: Text
  , simpleDescription :: Text
  }
  deriving stock (Show, Read, Ord, Eq, Generic)

data Function = Function
  { name :: !Text
  , description :: !Text
  , parameters :: !Parameters
  , supportedEvalBackend :: [EvalBackend]
  }
  deriving stock (Show, Read, Ord, Eq, Generic)

data FunctionImplementation = FunctionImplementation
  { declaration :: !Function
  , implementation :: !(Map EvalBackend Text)
  }
  deriving stock (Show, Read, Ord, Eq, Generic)
data SimpleResponse
  = SimpleResponse !ResponseWithReason
  | SimpleError !EvaluatorError
  deriving stock (Show, Read, Ord, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- ----------------------------------------------------------------------------
-- Servant Combinators
-- ----------------------------------------------------------------------------

data OperationId (symbol :: Symbol)

instance (HasLink sub) => HasLink (OperationId s :> sub) where
  type MkLink (OperationId s :> sub) a = MkLink sub a
  toLink = simpleToLink (Proxy :: Proxy sub)

simpleToLink ::
  forall sub a combinator.
  (HasLink sub, MkLink sub a ~ MkLink (combinator :> sub) a) =>
  Proxy sub ->
  (Link -> a) ->
  Proxy (combinator :> sub) ->
  Link ->
  MkLink (combinator :> sub) a
simpleToLink _ toA _ = toLink toA (Proxy :: Proxy sub)

-- | Ignore @'OperationId'@ in server handlers.
instance (HasServer api ctx) => HasServer (OperationId desc :> api) ctx where
  type ServerT (OperationId desc :> api) m = ServerT api m

  route _ = route (Proxy :: Proxy api)
  hoistServerWithContext _ pc nt s = hoistServerWithContext (Proxy :: Proxy api) pc nt s

instance HasClient m api => HasClient m (OperationId desc :> api) where
  type Client m (OperationId desc :> api) = Client m api

  clientWithRoute pm _ = clientWithRoute pm (Proxy :: Proxy api)

  hoistClientMonad pm _ f cl = hoistClientMonad pm (Proxy :: Proxy api) f cl

-- ----------------------------------------------------------------------------
-- Web Service Handlers
-- ----------------------------------------------------------------------------

-- | Parse trace level from header value
parseTraceHeader :: Maybe BS.ByteString -> Api.TraceLevel
parseTraceHeader Nothing = Api.TraceNone  -- Default to no trace
parseTraceHeader (Just bs) = case Text.toLower (Text.decodeUtf8 bs) of
  "none" -> Api.TraceNone
  "full" -> Api.TraceFull
  _ -> Api.TraceNone  -- Default to no trace on invalid value

-- | Determine trace level from header and query param
-- Header takes precedence over query param
determineTraceLevel :: Maybe Text -> Maybe Api.TraceLevel -> Api.TraceLevel
determineTraceLevel (Just headerVal) _ = parseTraceHeader (Just $ Text.encodeUtf8 headerVal)
determineTraceLevel Nothing (Just paramVal) = paramVal
determineTraceLevel Nothing Nothing = Api.TraceNone  -- Default to no trace

handler :: ServerT Api AppM
handler =
  FunctionApi
    { functionRoutes =
        FunctionCrud
          { batchEntities = getAllFunctions
          , singleEntity = \name ->
              SingleFunctionApi
                { getFunction =
                    getFunctionHandler name
                , putFunction =
                    putFunctionHandler name
                , postFunction =
                    postFunctionHandler name
                , deleteFunction =
                    deleteFunctionHandler name
                , evalFunction = \mTraceHeader mTraceParam mGraphViz ->
                    evalFunctionHandler name mTraceHeader mTraceParam mGraphViz
                , evalFunctionTracePNG = \mTraceHeader mTraceParam ->
                    evalFunctionTracePngHandler name mTraceHeader mTraceParam
                , evalFunctionTraceSVG = \mTraceHeader mTraceParam ->
                    evalFunctionTraceSvgHandler name mTraceHeader mTraceParam
                , batchFunction = \mTraceHeader mTraceParam mGraphViz ->
                    batchFunctionHandler name mTraceHeader mTraceParam mGraphViz
                , queryPlan =
                    queryPlanHandler name
                , listStateGraphs =
                    listStateGraphsHandler name
                , getStateGraphDot =
                    getStateGraphDotHandler name
                , getStateGraphSvg =
                    getStateGraphSvgHandler name
                , getStateGraphPng =
                    getStateGraphPngHandler name
                }
          }
    }

queryPlanHandler :: String -> FnArguments -> AppM QueryPlanResponse
queryPlanHandler name' args = do
  let name = Text.pack name'
  functionsTVar <- asks (.functionDatabase)
  cached <-
    DecisionQueryPlan.getOrBuildDecisionQueryCache
      functionsTVar
      name
      (.fnDecisionQueryCache)
      (\fn -> DecisionQueryPlan.decisionQueryCacheKey name fn.fnSources)
      (\c fn -> fn {fnDecisionQueryCache = Just c})
      (\funName fn -> DecisionQueryPlan.buildDecisionQueryCache funName fn.fnSources)

  pure (DecisionQueryPlan.queryPlan name cached args)

-- ----------------------------------------------------------------------------
-- State Graph Handlers
-- ----------------------------------------------------------------------------

-- | List all state graphs available in the module
listStateGraphsHandler :: String -> AppM StateGraphListResponse
listStateGraphsHandler name' = do
  let name = Text.pack name'
  vf <- lookupValidatedFunction name
  case vf.fnCompiled of
    Nothing -> throwError err404 { errBody = "No compiled module found for function" }
    Just compiled -> do
      let graphs = StateGraph.extractStateGraphs compiled.compiledModule
      pure $ StateGraphListResponse
        { graphs = map (\sg -> StateGraphInfo sg.sgName Nothing) graphs
        }

-- | Get state graph as DOT text
getStateGraphDotHandler :: String -> Text -> AppM Text
getStateGraphDotHandler name' graphName = do
  let name = Text.pack name'
  vf <- lookupValidatedFunction name
  case vf.fnCompiled of
    Nothing -> throwError err404 { errBody = "No compiled module found for function" }
    Just compiled -> do
      let graphs = StateGraph.extractStateGraphs compiled.compiledModule
      case find (\sg -> sg.sgName == graphName) graphs of
        Nothing -> throwError err404 { errBody = "State graph not found: " <> encodeTextLBS graphName }
        Just graph -> do
          let opts = StateGraph.defaultStateGraphOptions
          pure $ StateGraph.stateGraphToDot opts graph

-- | Render state graph as SVG
getStateGraphSvgHandler :: String -> Text -> AppM Text
getStateGraphSvgHandler name' graphName = do
  dot <- getStateGraphDotHandler name' graphName
  ensureGraphVizAvailable
  result <- liftIO $ renderSVG dot
  case result of
    Left err -> throwError err500 { errBody = encodeTextLBS err }
    Right svgText -> pure svgText

-- | Render state graph as PNG
getStateGraphPngHandler :: String -> Text -> AppM Api.PngImage
getStateGraphPngHandler name' graphName = do
  dot <- getStateGraphDotHandler name' graphName
  ensureGraphVizAvailable
  result <- liftIO $ renderPNG dot
  case result of
    Left err -> throwError err500 { errBody = encodeTextLBS err }
    Right bytes -> pure (Api.PngImage bytes)

-- | Helper to look up a validated function by name
lookupValidatedFunction :: Text -> AppM ValidatedFunction
lookupValidatedFunction name = do
  functionsTVar <- asks (.functionDatabase)
  functions <- liftIO $ readTVarIO functionsTVar
  case Map.lookup name functions of
    Nothing -> throwError err404 { errBody = "Function not found: " <> encodeTextLBS name }
    Just vf -> pure vf

evalFunctionHandler :: String -> Maybe Text -> Maybe Api.TraceLevel -> Maybe Bool -> FnArguments -> AppM SimpleResponse
evalFunctionHandler name' mTraceHeader mTraceParam mGraphViz args = do
  let traceLevel = determineTraceLevel mTraceHeader mTraceParam
      includeGraphViz = traceLevel == Api.TraceFull && Maybe.fromMaybe False mGraphViz
  let fnArgs = Map.assocs args.fnArguments
      name = Text.pack name'
  runFunctionEval name args.fnEvalBackend fnArgs traceLevel includeGraphViz

evalFunctionTracePngHandler :: String -> Maybe Text -> Maybe Api.TraceLevel -> FnArguments -> AppM Api.PngImage
evalFunctionTracePngHandler name' mTraceHeader mTraceParam args = do
  dot <- prepareTraceForImage name' mTraceHeader mTraceParam args
  ensureGraphVizAvailable
  result <- liftIO $ renderPNG dot
  case result of
    Left err -> throwError err500 {errBody = encodeTextLBS err}
    Right bytes -> pure (Api.PngImage bytes)

evalFunctionTraceSvgHandler :: String -> Maybe Text -> Maybe Api.TraceLevel -> FnArguments -> AppM Text
evalFunctionTraceSvgHandler name' mTraceHeader mTraceParam args = do
  dot <- prepareTraceForImage name' mTraceHeader mTraceParam args
  ensureGraphVizAvailable
  result <- liftIO $ renderSVG dot
  case result of
    Left err -> throwError err500 {errBody = encodeTextLBS err}
    Right svgText -> pure svgText

batchFunctionHandler :: String -> Maybe Text -> Maybe Api.TraceLevel -> Maybe Bool -> BatchRequest -> AppM BatchResponse
batchFunctionHandler name' mTraceHeader mTraceParam mGraphViz batchArgs = do
  let traceLevel = determineTraceLevel mTraceHeader mTraceParam
      includeGraphViz = traceLevel == Api.TraceFull && Maybe.fromMaybe False mGraphViz
  functionsTVar <- asks (.functionDatabase)
  functions <- liftIO $ readTVarIO functionsTVar
  case Map.lookup name functions of
    Nothing -> throwError err404
    Just fnImpl -> do
      -- Capture the environment before going concurrent
      env <- ask
      -- Use parallel evaluation with forConcurrently for better performance
      (execTime, evalResults) <- stopwatchM $ liftIO $ forConcurrently batchArgs.cases $ \inputCase -> do
        let
          args = Map.assocs $ fmap Just inputCase.attributes

        -- Note: runEvaluatorFor is now run concurrently across all cases
        r <- runAppM env (runEvaluatorFor Nothing fnImpl args outputFilter traceLevel includeGraphViz)
        pure (inputCase.id, r)

      -- Check for fatal ServerError exceptions (timeout, missing backend) and propagate them
      -- These should fail the entire batch, not be treated as per-case errors
      case [err | (_, Left err) <- evalResults] of
        (err:_) -> throwError err  -- Fail batch with first fatal error
        [] -> pure ()

      -- Only process successful responses and per-case evaluation errors
      let
        attachLinks = tagGraphVizPaths name
        responses = [(rid, attachLinks simpleResp) | (rid, Right simpleResp) <- evalResults]
        nCases = length responses

        successfulRuns =
          Maybe.mapMaybe
            ( \(rid, simpleRes) -> case simpleRes of
                SimpleResponse r -> Just (rid, r)
                SimpleError _ -> Nothing
            )
            responses

        nSuccessful = length successfulRuns
        nIgnored = nCases - nSuccessful

      pure $
        BatchResponse
          { cases =
              [ OutputCase
                { id = rid
                , attributes = Map.fromList response.values
                , graphviz = response.graphviz
                }
              | (rid, response) <- successfulRuns
              ]
          , summary =
              OutputSummary
                { casesRead = nCases
                , casesProcessed = nSuccessful
                , casesIgnored = nIgnored
                , processorDurationSec = nsToS execTime
                , casesPerSec = nsToS execTime / realToFrac nCases
                , processorQueuedSec = 0
                }
          }
 where
  outputFilter = if null outputFilter' then Nothing else Just outputFilter'
  outputFilter' =
    Set.fromList $
      Maybe.mapMaybe
        ( \ case
            OutcomeAttribute t -> Just t
            OutcomePropertyObject _ -> Nothing
        )
        batchArgs.outcomes
  name = Text.pack name'

  nsToS :: Chronos.Timespan -> Centi
  nsToS n = (realToFrac @Int @Centi $ fromIntegral @Int64 @Int (Chronos.getTimespan n)) / 10e9

runEvaluatorFor :: Maybe EvalBackend -> ValidatedFunction -> [(Text, Maybe FnLiteral)] -> Maybe (Set Text) -> Api.TraceLevel -> Bool -> AppM SimpleResponse
runEvaluatorFor engine validatedFunc args outputFilter traceLevel includeGraphViz = do
  eval <- evaluationEngine evalBackend validatedFunc
  evaluationResult <-
    timeoutAction $
      runExceptT
        ( eval.runFunction
            args
            outputFilter
            traceLevel
            includeGraphViz
        )

  case evaluationResult of
    Left err -> pure $ SimpleError err
    Right r -> pure $ SimpleResponse r
 where
  evalBackend = Maybe.fromMaybe JL4 engine

runFunctionEval :: Text -> Maybe EvalBackend -> [(Text, Maybe FnLiteral)] -> Api.TraceLevel -> Bool -> AppM SimpleResponse
runFunctionEval name engine fnArgs traceLevel includeGraphViz = do
  functionsTVar <- asks (.functionDatabase)
  functions <- liftIO $ readTVarIO functionsTVar
  let evalFn fnImpl = runEvaluatorFor engine fnImpl fnArgs Nothing traceLevel includeGraphViz
  case Map.lookup name functions of
    Nothing ->
      withUUIDFunction
        name
        (\fnImpl -> do
            resp <- evalFn fnImpl
            pure (tagGraphVizPaths name resp)
        )
        (\k -> throwError (k err404))
    Just fnImpl -> do
      resp <- evalFn fnImpl
      pure (tagGraphVizPaths name resp)

-- | Stamp relative PNG/SVG URLs onto a GraphViz payload so the client knows
-- where to fetch ready-made images. We intentionally keep these as URLs
-- instead of embedding image bytes so the main evaluation endpoint stays
-- side-effect-free; the PNG/SVG routes simply rerun the same evaluation via
-- their own handlers whenever the client follows the link.
tagGraphVizPaths :: Text -> SimpleResponse -> SimpleResponse
tagGraphVizPaths name = \ case
  SimpleResponse ResponseWithReason{values = vals, reasoning = rsn, graphviz = mGraphViz} ->
    SimpleResponse
      ResponseWithReason
        { values = vals
        , reasoning = rsn
        , graphviz = graphVizLinksFor name <$> mGraphViz
        }
  err -> err

graphVizLinksFor :: Text -> Api.GraphVizResponse -> Api.GraphVizResponse
graphVizLinksFor name resp =
  resp
    { png = Just (graphVizRelativeLink name "trace.png")
    , svg = Just (graphVizRelativeLink name "trace.svg")
    }

graphVizRelativeLink :: Text -> Text -> Text
graphVizRelativeLink name suffix =
  "/functions/" <> encodePathSegment name <> "/evaluation/" <> suffix

encodePathSegment :: Text -> Text
encodePathSegment =
  Text.decodeUtf8 . URI.urlEncode False . Text.encodeUtf8

prepareTraceForImage :: String -> Maybe Text -> Maybe Api.TraceLevel -> FnArguments -> AppM Text
prepareTraceForImage name' mTraceHeader mTraceParam args = do
  let requestedTrace = determineTraceLevel mTraceHeader mTraceParam
  requireFullTrace requestedTrace
  let name = Text.pack name'
      fnArgs = Map.assocs args.fnArguments
  simpleResp <- runFunctionEval name args.fnEvalBackend fnArgs Api.TraceFull True
  case simpleResp of
    SimpleError evalErr -> throwError err500 {errBody = Aeson.encode evalErr}
    SimpleResponse resp ->
      case resp.graphviz of
        Nothing -> throwError err500 {errBody = encodeTextLBS "Trace data unavailable"}
        Just gv -> pure gv.dot

requireFullTrace :: Api.TraceLevel -> AppM ()
requireFullTrace lvl =
  when (lvl /= Api.TraceFull) $
    throwError err400 {errBody = encodeTextLBS "GraphViz rendering endpoints require trace=full"}

ensureGraphVizAvailable :: AppM ()
ensureGraphVizAvailable = do
  available <- liftIO isGraphVizAvailable
  unless available $
    throwError err503 {errBody = encodeTextLBS "GraphViz 'dot' command not found in PATH. Install graphviz to enable image rendering."}

encodeTextLBS :: Text -> LBS.ByteString
encodeTextLBS = LBS.fromStrict . Text.encodeUtf8

deleteFunctionHandler :: String -> AppM ()
deleteFunctionHandler name' = do
  functionsTVar <- asks (.functionDatabase)
  exists <- liftIO $ atomically $ stateTVar functionsTVar $ \functions ->
    case Map.member name functions of
      True ->
        (True, Map.delete name functions)
      False ->
        (False, functions)

  when (not exists) $
    throwError
      err404
        { errBody =
            "Resource "
              <> BS.fromStrict (Text.encodeUtf8 name)
              <> " does not exist"
        }
 where
  name = Text.pack name'

putFunctionHandler :: String -> FunctionImplementation -> AppM ()
putFunctionHandler name' updatedFunctionImpl = do
  validatedFunction <- validateFunction updatedFunctionImpl
  functionsTVar <- asks (.functionDatabase)
  exists <- liftIO $ atomically $ stateTVar functionsTVar $ \functions ->
    case Map.member name functions of
      True ->
        (True, Map.insert name validatedFunction functions)
      False ->
        (False, functions)

  when (not exists) $
    -- Error code has been chosen in accordance with
    -- https://stackoverflow.com/a/70371989
    throwError
      err404
        { errBody =
            "Resource "
              <> BS.fromStrict (Text.encodeUtf8 name)
              <> " does not exist"
        }
 where
  name = Text.pack name'

postFunctionHandler :: String -> FunctionImplementation -> AppM ()
postFunctionHandler name' newFunctionImpl = do
  validatedFunction <- validateFunction newFunctionImpl
  functionsTVar <- asks (.functionDatabase)
  exists <- liftIO $ atomically $ stateTVar functionsTVar $ \functions ->
    case Map.member name functions of
      True ->
        (True, functions)
      False ->
        (False, Map.insert name validatedFunction functions)

  when exists $
    -- Error code has been chosen in accordance with
    -- https://stackoverflow.com/a/70371989
    throwError
      err409
        { errBody =
            "Resource "
              <> BS.fromStrict (Text.encodeUtf8 name)
              <> " already exists"
        }
 where
  name = Text.pack name'

validateFunction :: FunctionImplementation -> AppM ValidatedFunction
validateFunction fn = do
  filledDeclaration <- fillMetadataFromAnnotations fn
  let fnWithDeclaration = fn{declaration = filledDeclaration}
  (evaluators, mCompiled) <- Map.traverseWithKey (validateImplementation fnWithDeclaration) fnWithDeclaration.implementation
    >>= \evalMap -> do
      -- Extract the compiled module from any JL4 backend
      -- Since we only have JL4 backend currently, we can just look it up
      let mCompiled = case Map.lookup JL4 evalMap of
            Just (_, compiled) -> compiled
            Nothing -> Nothing
      pure (Map.map fst evalMap, mCompiled)

  pure
    ValidatedFunction
      { fnImpl = fnWithDeclaration.declaration
      , fnEvaluator = evaluators
      , fnCompiled = mCompiled
      , fnSources = fnWithDeclaration.implementation
      , fnDecisionQueryCache = Nothing
      }
 where
  validateImplementation :: FunctionImplementation -> EvalBackend -> Text -> AppM (RunFunction, Maybe Jl4.CompiledModule)
  validateImplementation fnImpl JL4 program =
    liftIO $ Jl4.createFunction (Text.unpack fnImpl.declaration.name <> ".l4") (toDecl fnImpl.declaration) program Map.empty

fillMetadataFromAnnotations :: FunctionImplementation -> AppM Function
fillMetadataFromAnnotations fn =
  case Map.lookup JL4 fn.implementation of
    Nothing -> pure fn.declaration
    Just source
      | hasUserMetadata fn.declaration -> pure fn.declaration
      | otherwise -> do
          derived <- deriveFunctionFromSource fn.declaration source
          pure (Maybe.fromMaybe fn.declaration derived)

hasUserMetadata :: Function -> Bool
hasUserMetadata decl =
  let descPresent = not (Text.null (Text.strip decl.description))
      paramsPresent = not (Map.null decl.parameters.parameterMap)
  in descPresent && paramsPresent

deriveFunctionFromSource :: Function -> Text -> AppM (Maybe Function)
deriveFunctionFromSource existing source = do
  let fileName =
        if Text.null (Text.strip existing.name)
          then "uploaded.l4"
          else Text.unpack existing.name <> ".l4"
  (errs, mTcRes) <- liftIO $ Jl4.typecheckModule fileName source Map.empty
  case mTcRes of
    Nothing -> do
      unless (null errs) $
        liftIO $ putStrLn $ "Failed to derive metadata from annotations: " <> Text.unpack (Text.intercalate "; " errs)
      pure Nothing
    Just Rules.TypeCheckResult{module' = resolvedModule} -> do
      let exports = getExportedFunctions resolvedModule
      case selectExport existing exports of
        Nothing -> pure Nothing
        Just exported -> do
          let derived = exportToFunction resolvedModule exported
          pure $ Just Function
            { name = chooseField existing.name derived.name
            , description = chooseField existing.description derived.description
            , parameters =
                if Map.null existing.parameters.parameterMap
                  then derived.parameters
                  else existing.parameters
            , supportedEvalBackend = existing.supportedEvalBackend
            }
 where
  chooseField orig new =
    if Text.null (Text.strip orig) then new else orig

selectExport :: Function -> [ExportedFunction] -> Maybe ExportedFunction
selectExport existing exports =
  let byName = if Text.null (Text.strip existing.name)
                  then Nothing
                  else List.find (\e -> e.exportName == existing.name) exports
      byDefault = List.find (.exportIsDefault) exports
  in byName <|> byDefault <|> Maybe.listToMaybe exports

exportToFunction :: Module Resolved -> ExportedFunction -> Function
exportToFunction resolvedModule export =
  Function
    { name = export.exportName
    , description = T.strip export.exportDescription
    , parameters = parametersFromExport resolvedModule export.exportParams
    , supportedEvalBackend = [JL4]
    }

parametersFromExport :: Module Resolved -> [ExportedParam] -> Parameters
parametersFromExport resolvedModule params =
  let declares = declaresFromModule resolvedModule
   in
  MkParameters
    { parameterMap = Map.fromList [(param.paramName, paramToParameter declares param) | param <- params]
    , required = [param.paramName | param <- params, param.paramRequired]
    }

paramToParameter :: Map Text (Declare Resolved) -> ExportedParam -> Parameter
paramToParameter declares param =
  let p0 =
        Maybe.fromMaybe
          (Parameter "object" Nothing [] "" Nothing Nothing Nothing)
          (typeToParameter declares Set.empty <$> param.paramType)
   in
    p0
      { parameterAlias = Nothing
      , parameterDescription = T.strip $ Maybe.fromMaybe "" param.paramDescription
      }

declaresFromModule :: Module Resolved -> Map Text (Declare Resolved)
declaresFromModule (MkModule _ _ section) =
  Map.fromList (collectSection section)
 where
  collectSection (MkSection _ _ _ decls) =
    decls >>= collectDecl

  collectDecl = \case
    Declare _ decl@(MkDeclare _ _ (MkAppForm _ name _ _) _) ->
      [(resolvedNameText name, decl)]
    Section _ sub ->
      collectSection sub
    _ ->
      []

typeToParameter ::
  Map Text (Declare Resolved) ->
  Set Text ->
  Type' Resolved ->
  Parameter
typeToParameter declares visited ty =
  case ty of
    Type _ -> emptyParam "object"
    TyApp _ name [] ->
      typeNameToParameter name
    TyApp _ name [inner] ->
      let lowered = Text.toLower (resolvedNameText name)
       in
        if lowered `elem` ["list", "listof"]
          then
            (emptyParam "array")
              { parameterItems = Just (typeToParameter declares visited inner)
              }
          else
            if lowered `elem` ["maybe", "optional"]
              then typeToParameter declares visited inner
              else typeNameToParameter name
    TyApp _ name _ ->
      typeNameToParameter name
    Fun{} -> emptyParam "object"
    Forall _ _ inner -> typeToParameter declares visited inner
    InfVar{} -> emptyParam "object"
 where
  emptyParam :: Text -> Parameter
  emptyParam t =
    Parameter
      { parameterType = t
      , parameterAlias = Nothing
      , parameterEnum = []
      , parameterDescription = ""
      , parameterProperties = Nothing
      , parameterPropertyOrder = Nothing
      , parameterItems = Nothing
      }

  typeNameToParameter :: Resolved -> Parameter
  typeNameToParameter name =
    case primitiveJsonType name of
      Just t -> emptyParam t
      Nothing ->
        case Map.lookup (resolvedNameText name) declares of
          Nothing -> emptyParam "object"
          Just decl -> declareToParameter (resolvedNameText name) decl

  primitiveJsonType :: Resolved -> Maybe Text
  primitiveJsonType name =
    case Text.toLower (resolvedNameText name) of
      "number" -> Just "number"
      "int" -> Just "number"
      "integer" -> Just "number"
      "float" -> Just "number"
      "double" -> Just "number"
      "boolean" -> Just "boolean"
      "bool" -> Just "boolean"
      "string" -> Just "string"
      "text" -> Just "string"
      "date" -> Just "string"
      "datetime" -> Just "string"
      _ -> Nothing

  declareToParameter :: Text -> Declare Resolved -> Parameter
  declareToParameter typeName (MkDeclare declAnn _ _ typeDecl)
    | typeName `Set.member` visited =
        emptyParam "object"
    | otherwise =
        case typeDecl of
          RecordDecl _ _ fields ->
            let
              visited' = Set.insert typeName visited
              fieldOrder = [resolvedNameText fieldName | MkTypedName _ fieldName _ <- fields]
              props =
                Map.fromList
                  [ (resolvedNameText fieldName, addDesc fieldDesc (typeToParameter declares visited' fieldTy))
                  | MkTypedName fieldAnn fieldName fieldTy <- fields
                  , let fieldDesc = fmap getDesc (fieldAnn Optics.^. annDesc)
                  ]
             in
              (emptyParam "object")
                { parameterDescription = Maybe.fromMaybe "" (fmap getDesc (declAnn Optics.^. annDesc))
                , parameterProperties = Just props
                , parameterPropertyOrder = Just fieldOrder
                }
          EnumDecl _ constructors ->
            (emptyParam "string")
              { parameterDescription = Maybe.fromMaybe "" (fmap getDesc (declAnn Optics.^. annDesc))
              , parameterEnum = [resolvedNameText c | MkConDecl _ c _ <- constructors]
              }
          SynonymDecl _ inner ->
            typeToParameter declares (Set.insert typeName visited) inner
   where
    addDesc :: Maybe Text -> Parameter -> Parameter
    addDesc Nothing p = p
    addDesc (Just d) p = p {parameterDescription = d}

resolvedNameText :: Resolved -> Text
resolvedNameText =
  rawNameToText . rawName . getActual

getAllFunctions :: AppM [SimpleFunction]
getAllFunctions = do
  functions <- liftIO . readTVarIO =<< asks (.functionDatabase)
  pure $ fmap (toSimpleFunction . (.fnImpl)) $ filter (not . looksLikeUUID . (.fnImpl.name)) $ Map.elems functions
 where
  toSimpleFunction s =
    SimpleFunction
      { simpleName = s.name
      , simpleDescription = s.description
      }
  
  -- | Check if a function name looks like a UUID (with optional suffix after colon)
  -- Matches patterns like:
  --   b52992ed-39fd-4226-bad2-2deee2473881
  --   b52992ed-39fd-4226-bad2-2deee2473881:functionName
  looksLikeUUID :: Text -> Bool
  looksLikeUUID name =
    let (beforeColon, _) = Text.breakOn ":" name
        nameToCheck = if Text.null beforeColon then name else beforeColon
    in Maybe.isJust (UUID.fromText nameToCheck)

getFunctionHandler :: String -> AppM Function
getFunctionHandler name = do
  let tname = Text.pack name
  functions <- liftIO . readTVarIO =<< asks (.functionDatabase)
  case Map.lookup tname functions of
    Nothing ->
      withUUIDFunction
        tname
        (pure . (.fnImpl))
        (\k -> throwError (k err404))
    Just function -> pure function.fnImpl

withUUIDFunction :: Text -> (ValidatedFunction -> AppM a) -> ((ServerError -> ServerError) -> AppM a) -> AppM a
withUUIDFunction uuidAndFun k err = case UUID.fromText muuid of
  Nothing -> err id
  Just uuid -> do
    MkAppEnv {baseUrl, manager} <- ask
    eprog <- liftIO $ runExceptT $ readCrudUUID uuid baseUrl manager
    case eprog of
      Left err' -> do
        liftIO do
          hPutStrLn stderr "failed to retrieve function from CRUD backend"
          hPutStrLn stderr $ displayException err'
        err (\e -> e {errBody = "uuid not present on remote backend: " <> UUID.toLazyASCIIBytes uuid})
      Right prog -> do
        let fileName = Text.unpack muuid <> ".l4"
        (typecheckErrs, mTcRes) <- liftIO $ Jl4.typecheckModule fileName prog Map.empty
        let mResolvedModule = (\Rules.TypeCheckResult{module' = m} -> m) <$> mTcRes

        -- Derive actual function name from exports if not specified in UUID
        actualFunName <- if Text.null (Text.strip funNameFromUUID)
          then do
            case mTcRes of
              Nothing -> do
                unless (null typecheckErrs) $
                  liftIO $ putStrLn $ "Failed to derive function name from exports: " <> Text.unpack (Text.intercalate "; " typecheckErrs)
                throwError err500 {errBody = "could not determine function name from exports"}
              Just Rules.TypeCheckResult{module' = resolvedModule} -> do
                let exports = getExportedFunctions resolvedModule
                    dummyFn = Function "" "" MkParameters{parameterMap = Map.empty, required = []} []
                case selectExport dummyFn exports of
                  Nothing -> throwError err500 {errBody = "no exported functions found in session"}
                  Just exported -> pure exported.exportName
          else pure funNameFromUUID

        -- Directive filtering happens at the AST level in Jl4.evaluateWrapperInContext
        let fnImpl = mkSessionFunction actualFunName MkParameters {parameterMap = Map.empty, required = []} prog
            fnDecl = toDecl fnImpl

        decide <- liftIO (runExceptT (Jl4.buildFunDecide prog fnDecl))
          >>= either (\e -> do
            liftIO $ hPutStrLn stderr "the evaluator failed with error:"
            liftIO $ hPrint stderr e
            throwError err500 {errBody = "evaluator failed"}
            ) pure

        (runFn, mCompiled) <- liftIO $ Jl4.createFunction (Text.unpack actualFunName <> ".l4") fnDecl prog Map.empty

        k ValidatedFunction
          { fnImpl = fnImpl { parameters = parametersOfDecideWithModule mResolvedModule decide }
          , fnEvaluator = Map.singleton JL4 runFn
          , fnCompiled = mCompiled
          , fnSources = Map.singleton JL4 prog
          , fnDecisionQueryCache = Nothing
          }
  where
   (muuid, funNameFromUUID) = T.drop 1 <$> T.breakOn ":" uuidAndFun

parametersOfDecide :: Decide Resolved -> Parameters
parametersOfDecide = parametersOfDecideWithModule Nothing

parametersOfDecideWithModule :: Maybe (Module Resolved) -> Decide Resolved -> Parameters
parametersOfDecideWithModule mMod (MkDecide _ (MkTypeSig _ (MkGivenSig _ typedNames) _) (MkAppForm _ _ args _) _)  =
  -- TODO:
  -- need to change the description of the parameters as soon as we have it in the Decide
  let declares = maybe Map.empty declaresFromModule mMod
   in
    MkParameters
      { parameterMap =
          Map.fromList $
            map
              ( \x ->
                  let
                    argInfo = lookup x bestEffortArgInfo
                    baseParam =
                      case argInfo >>= \(_tyText, mTy, _desc) -> mTy of
                        Nothing -> Parameter "object" Nothing [] "" Nothing Nothing Nothing
                        Just ty -> typeToParameter declares Set.empty ty
                    descTxt = maybe "" (\(_tyText, _mTy, d) -> d) argInfo
                   in
                    (x, baseParam {parameterDescription = descTxt, parameterAlias = Nothing})
              )
              argList
      , required = argList
      }
 where
  bestEffortArgInfo = foldr fn [] args
  fn r acc = case find (\(MkOptionallyTypedName _ r' _) -> r `sameResolved` r') typedNames of
    Just tn@(MkOptionallyTypedName _ r' mt)
      | Just t <- mt
      , let descTokens = Optics.toListOf (Optics.gplate @TAnnotations Optics.% #_TDesc) tn
            exportTokens = Optics.toListOf (Optics.gplate @TAnnotations Optics.% #_TExport) tn
            descriptions = mconcat $ nubOrd (descTokens <> exportTokens)
      -> (prettyLayout r', (prettyLayout t, Just t, descriptions)) : acc
    _ -> acc
  sameResolved = (==) `on` getUnique
  argList = map prettyLayout args

-- | Run an AppM action in IO with a given environment, returning Either
-- Used for concurrent batch evaluation
runAppM :: AppEnv -> AppM a -> IO (Either ServerError a)
runAppM env action = runHandler $ runReaderT action env

timeoutAction :: IO b -> AppM b
timeoutAction act =
  liftIO (timeout (seconds 60) act) >>= \ case
    Nothing -> throwError err500
    Just r -> pure r
 where
  seconds n = 1_000_000 * n

stopwatchM :: AppM a -> AppM (Chronos.Timespan, a)
stopwatchM action = do
  start <- liftIO Clock.getMonotonicTimeNSec
  a' <- action
  a <- liftIO $ evaluate a'
  end <- liftIO Clock.getMonotonicTimeNSec
  pure ((Chronos.Timespan (fromIntegral (end - start))), a)

-- ----------------------------------------------------------------------------
-- "Database" layer
-- ----------------------------------------------------------------------------

evaluationEngine :: EvalBackend -> ValidatedFunction -> AppM RunFunction
evaluationEngine b valFn = do
  case Map.lookup b valFn.fnEvaluator of
    Nothing -> throwError err404
    Just eval -> pure eval

-- ----------------------------------------------------------------------------
-- Json encoders and decoders that are not derived.
-- We often need custom instances, as we want to be more lenient in what we accept
-- than what aeson does by default. Further, we try to provide a specific json schema.
--
-- ----------------------------------------------------------------------------

instance ToJSON SimpleFunction where
  toJSON (SimpleFunction n desc) =
    Aeson.object
      [ "type" .= Aeson.String "function"
      , "function"
          .= Aeson.object
            [ "name" .= Aeson.String n
            , "description" .= Aeson.String desc
            ]
      ]

instance FromJSON SimpleFunction where
  parseJSON = Aeson.withObject "Function" $ \o -> do
    "function" :: Text <- o .: "type"
    props <- o .: "function"
    simpleFn <-
      Aeson.withObject
        "function body"
        ( \p -> do
            SimpleFunction
              <$> p .: "name"
              <*> p .: "description"
        )
        props
    pure simpleFn

instance ToJSON Function where
  toJSON (Function n desc params backends) =
    Aeson.object
      [ "type" .= Aeson.String "function"
      , "function"
          .= Aeson.object
            [ "name" .= Aeson.String n
            , "description" .= Aeson.String desc
            , "parameters" .= params
            , "supportedBackends" .= backends
            ]
      ]

instance FromJSON Function where
  parseJSON = Aeson.withObject "Function" $ \o -> do
    fnType <- o .: "type"
    case fnType :: Text of
      "function" -> pure ()
      e -> fail $ "Expected \"function\" but got" <> Text.unpack e
    props <- o .: "function"
    Aeson.withObject
      "function body"
      ( \p -> do
          Function
            <$> p .: "name"
            <*> p .: "description"
            <*> p .: "parameters"
            <*> p .: "supportedBackends"
      )
      props

instance ToJSON FunctionImplementation where
  toJSON fnImpl =
    Aeson.object
      [ "declaration" .= fnImpl.declaration
      , "implementation" .= fnImpl.implementation
      ]

instance FromJSON FunctionImplementation where
  parseJSON = Aeson.withObject "Function Implementation" $ \o -> do
    FunctionImplementation
      <$> o .: "declaration"
      <*> o .: "implementation"

toDecl :: Function -> Api.FunctionDeclaration
toDecl fn =
  Api.FunctionDeclaration
    { Api.name = fn.name
    , Api.description = fn.description
    , Api.longNames = Map.keysSet $ fn.parameters.parameterMap
    , Api.nameMapping = shortToLongNameMapping
    }
 where
  shortToLongNameMapping :: Map Text Text
  shortToLongNameMapping =
    Map.fromList $
      Maybe.mapMaybe (fmap Tuple.swap . Tuple.secondM (.parameterAlias)) $
        Map.assocs fn.parameters.parameterMap

-- ----------------------------------------------------------------------------
-- Oracle DB
-- ----------------------------------------------------------------------------

type Id = Int

data Outcomes
  = OutcomeAttribute Text
  | OutcomePropertyObject OutcomeObject
  deriving stock (Show, Eq, Ord)

data OutcomeObject = OutcomeObject
  { id :: Text
  , showSilent :: Maybe Bool
  , showInvisible :: Maybe Bool
  , resolveIndecisionRelationships :: Maybe Bool
  , knownOutcomeStyle :: Maybe OutcomeStyle
  , unknownOutcomeStyle :: Maybe OutcomeStyle
  }
  deriving stock (Show, Eq, Ord)

data OutcomeStyle
  = ValueOnly
  | DecisionReport
  | BaseAttributes
  deriving stock (Show, Ord, Eq, Enum, Bounded)

data BatchRequest = BatchRequest
  { outcomes :: [Outcomes]
  , cases :: [InputCase]
  }
  deriving stock (Show, Eq, Ord)

data InputCase = InputCase
  { id :: Id
  , attributes :: Map Text FnLiteral
  }
  deriving stock (Show, Eq, Ord)

data OutputCase = OutputCase
  { id :: Id
  , attributes :: Map Text FnLiteral
  , graphviz :: Maybe Api.GraphVizResponse
  }
  deriving stock (Show, Eq, Ord)

data BatchResponse = BatchResponse
  { cases :: [OutputCase]
  , summary :: OutputSummary
  }
  deriving stock (Show, Eq, Ord)

data OutputSummary = OutputSummary
  { casesRead :: Int
  , casesProcessed :: Int
  , casesIgnored :: Int
  , processorDurationSec :: Fixed E2
  , casesPerSec :: Fixed E2
  , processorQueuedSec :: Fixed E2
  }
  deriving stock (Show, Eq, Ord)

-- ----------------------------------------------------------------------------
-- Batch request json
-- ----------------------------------------------------------------------------

batchRequestDecoder :: Decoder BatchRequest
batchRequestDecoder =
  BatchRequest
    <$> ACD.key "outcomes" (ACD.list outcomesDecoder)
    <*> ACD.key "cases" (ACD.list inputCaseDecoder)

outcomeStyleDecoder :: Decoder OutcomeStyle
outcomeStyleDecoder =
  ACD.text >>= \ case
    "value-only" -> pure ValueOnly
    "decision-report" -> pure DecisionReport
    "base-attributes" -> pure BaseAttributes
    val -> fail $ "Unknown value " <> show val

outcomeObjectDecoder :: Decoder OutcomeObject
outcomeObjectDecoder =
  OutcomeObject
    <$> ACD.key "id" ACD.text
    <*> ACD.maybeKey "showSilent" ACD.bool
    <*> ACD.maybeKey "showInvisible" ACD.bool
    <*> ACD.maybeKey "resolveIndecisionRelationships" ACD.bool
    <*> ACD.maybeKey "knownOutcomeStyle" outcomeStyleDecoder
    <*> ACD.maybeKey "unknownOutcomeStyle" outcomeStyleDecoder

outcomesDecoder :: Decoder Outcomes
outcomesDecoder =
  (OutcomeAttribute <$> ACD.text)
    <|> (OutcomePropertyObject <$> outcomeObjectDecoder)

inputCaseDecoder :: Decoder InputCase
inputCaseDecoder = do
  caseId <- ACD.key "@id" ACD.int
  attributes <- ACD.mapStrict fnLiteralDecoder
  let
    attributes' = Map.delete "@id" attributes
  pure $ InputCase caseId attributes'

fnLiteralDecoder :: Decoder FnLiteral
fnLiteralDecoder =
  (FnLitInt <$> ACD.integer)
    <|> (FnLitDouble <$> ACD.double)
    <|> (FnLitBool <$> ACD.bool)
    <|> (parseTextAsFnLiteral <$> ACD.text)
    <|> (FnArray <$> ACD.list fnLiteralDecoder)
    <|> (FnObject <$> ACD.list (liftA2 (,) ACD.text fnLiteralDecoder))

batchRequestEncoder :: BatchRequest -> Aeson.Value
batchRequestEncoder br =
  Aeson.object
    [ "outcomes" .= fmap outcomesEncoder br.outcomes
    , "cases" .= fmap inputCaseEncoder br.cases
    ]

inputCaseEncoder :: InputCase -> Aeson.Value
inputCaseEncoder ic =
  Aeson.object $
    [ "@id" .= ic.id
    ] ++ [(Aeson.fromText k, Aeson.toJSON v) | (k, v) <- Map.toList ic.attributes]

outcomesEncoder :: Outcomes -> Aeson.Value
outcomesEncoder (OutcomeAttribute t) = Aeson.String t
outcomesEncoder (OutcomePropertyObject o) =
  Aeson.object
    [ "@id" .= o.id
    , "showSilent" .= o.showSilent
    , "showInvisible" .= o.showInvisible
    , "resolveIndecisionRelationships" .= o.resolveIndecisionRelationships
    , "knownOutcomeStyle" .= fmap outcomeStyleEncoder o.knownOutcomeStyle
    , "unknownOutcomeStyle" .= fmap outcomeStyleEncoder o.unknownOutcomeStyle
    ]

outcomeStyleEncoder :: OutcomeStyle -> Aeson.Value
outcomeStyleEncoder = \ case
  ValueOnly -> Aeson.String "value-only"
  DecisionReport -> Aeson.String "decision-report"
  BaseAttributes -> Aeson.String "base-attributes"

-- ----------------------------------------------------------------------------
-- Batch response json
-- ----------------------------------------------------------------------------

batchResponseDecoder :: Decoder BatchResponse
batchResponseDecoder =
  BatchResponse
    <$> ACD.key "cases" (ACD.list casesDecoder)
    <*> ACD.key "summary" summaryDecoder

casesDecoder :: Decoder OutputCase
casesDecoder = do
  caseId <- ACD.key "@id" ACD.int
  attributes <- ACD.mapStrict fnLiteralDecoder
  graphvizTrace <- parseGraphviz attributes
  let
    attributes' =
      Map.delete "@graphviz" $
        Map.delete "@id" attributes
  pure $ OutputCase caseId attributes' graphvizTrace
 where
  parseGraphviz attrs = case Map.lookup "@graphviz" attrs of
    Nothing -> pure Nothing
    Just literal -> parseGraphvizLiteral literal

  parseGraphvizLiteral (FnLitString dot) =
    pure $ Just Api.GraphVizResponse {dot, png = Nothing, svg = Nothing}
  parseGraphvizLiteral (FnObject fields) = do
    dotText <- requireTextField "dot" fields
    let pngText = optionalTextField "png" fields
        svgText = optionalTextField "svg" fields
    pure $ Just Api.GraphVizResponse {dot = dotText, png = pngText, svg = svgText}
  parseGraphvizLiteral other =
    fail $ "Expected @graphviz to be an object, but got " <> show other

  requireTextField label fields =
    case lookup label fields of
      Just (FnLitString t) -> pure t
      Just v -> fail $ "Expected @graphviz." <> Text.unpack label <> " to be a string, but got " <> show v
      Nothing -> fail $ "Missing @graphviz." <> Text.unpack label

  optionalTextField label fields =
    case lookup label fields of
      Just (FnLitString t) -> Just t
      _ -> Nothing

summaryDecoder :: Decoder OutputSummary
summaryDecoder =
  OutputSummary
    <$> ACD.key "casesRead" ACD.int
    <*> ACD.key "casesProcessed" ACD.int
    <*> ACD.key "casesIgnored" ACD.int
    <*> ACD.key "processorDurationSec" (fmap toFixed ACD.scientific)
    <*> ACD.key "processorCasesPerSec" (fmap toFixed ACD.scientific)
    <*> ACD.key "processorQueuedSec" (fmap toFixed ACD.scientific)
 where
  toFixed = realToFrac @Scientific @Centi

batchResponseEncoder :: BatchResponse -> Aeson.Value
batchResponseEncoder br =
  Aeson.object $
    [ "cases" .= fmap outputCaseEncoder br.cases
    , "summary" .= outputSummaryEncoder br.summary
    ]

outputSummaryEncoder :: OutputSummary -> Aeson.Value
outputSummaryEncoder os =
  Aeson.object
    [ "casesRead" .= os.casesRead
    , "casesProcessed" .= os.casesProcessed
    , "casesIgnored" .= os.casesIgnored
    , "processorDurationSec" .= toSci os.processorDurationSec
    , "processorCasesPerSec" .= toSci os.casesPerSec
    , "processorQueuedSec" .= toSci os.processorQueuedSec
    ]
 where
  toSci = realToFrac @Centi @Scientific

outputCaseEncoder :: OutputCase -> Aeson.Value
outputCaseEncoder oc =
  Aeson.object $
    [ "@id" .= oc.id
    ]
      <> maybe [] (\dot -> ["@graphviz" .= dot]) oc.graphviz
      <> [ (Aeson.fromText k .= v)
         | (k, v) <- Map.assocs oc.attributes
         ]

-- ----------------------------------------------------------------------------
-- Final FromJSON/ToJSON instances
-- ----------------------------------------------------------------------------

instance FromJSON BatchRequest where
  parseJSON = ACD.fromDecoder batchRequestDecoder

instance ToJSON BatchRequest where
  toJSON = batchRequestEncoder

instance ToJSON BatchResponse where
  toJSON = batchResponseEncoder

instance FromJSON BatchResponse where
  parseJSON = ACD.fromDecoder batchResponseDecoder

crudAPIClient :: CRUD.Api (AsClientT ClientM)
crudAPIClient = genericClient

runCrudClient :: ClientM a -> BaseUrl -> Manager -> ExceptT ClientError IO a
runCrudClient m crudBaseUrl mgr = ExceptT $ runClientM m $ mkClientEnv mgr crudBaseUrl

readCrudUUID :: UUID -> BaseUrl -> Manager -> ExceptT ClientError IO Text
readCrudUUID = runCrudClient . crudAPIClient.readSession

mkSessionFunction :: Text -> Parameters -> Text -> Function
mkSessionFunction name parameters description =
  Function
    { name
    , description
    , parameters
    , supportedEvalBackend = [JL4]
    }
