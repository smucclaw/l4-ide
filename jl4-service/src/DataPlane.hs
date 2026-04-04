{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module DataPlane (
  DataPlaneApi,
  dataPlaneHandler,
  ShortRoutes,
  shortRoutesHandler,
) where

import Backend.Api
import qualified BundleStore
import ControlPlane (DeploymentStatusResponse, getDeploymentHandler, putDeploymentHandler, deleteDeploymentHandler)
import OpenApiDoc (buildOpenApiDoc)
import FileBrowser (ShortFileBrowseApi, shortFileBrowseHandler)
import DeploymentLoader (tryCompileWithTimeout, CompilationResult (..))
import Servant.Multipart
import Backend.DecisionQueryPlan (CachedDecisionQuery, buildDecisionQueryCacheFromCompiled, queryPlan, QueryPlanResponse)
import L4.FunctionSchema (Parameter, Parameters(..))
import Shared (buildPropertyReverseMap, remapArguments, sanitizePropertyName)
import Backend.Jl4 (CompiledModule (..), evaluateWithCompiledDeontic)
import qualified L4.StateGraph as StateGraph
import Compiler (toDecl)
import Logging (logInfo)
import Options (Options (..))
import Shared (jsonError)
import Types

import qualified Data.Aeson as Aeson
import Data.Aeson ((.=))
import Data.Int (Int64)
import Control.Concurrent.Async (forConcurrently)
import Control.Concurrent.STM (atomically, modifyTVar', readTVarIO)
import Control.Exception (catch)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.Trans.Reader (runReaderT, asks, ask)
import Data.List (find)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import Data.Scientific (Scientific)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Conc (setAllocationCounter, getAllocationCounter, enableAllocationLimit)
import GHC.IO.Exception (AllocationLimitExceeded (..))
import Servant
import System.FilePath ((<.>))
import System.Timeout (timeout)

-- | The data plane API — per-deployment function evaluation.
type DataPlaneApi =
  "deployments" :> Capture "deploymentId" Text :> DeploymentRoutes

-- | Short routes: /{id} and /{id}/{fn}/... as aliases for the full paths.
-- These MUST appear after HealthApi, ControlPlaneApi, and DataPlaneApi in ServiceApi
-- so that literal prefixes like "health" and "deployments" match first.
type ShortRoutes =
  Capture "deploymentId" Text :> ShortDeploymentRoutes

type ShortDeploymentRoutes =
       -- GET /{id} → deployment status
       Get '[JSON] DeploymentStatusResponse
       -- PUT /{id} → replace deployment
  :<|> MultipartForm Mem (MultipartData Mem) :> Verb 'PUT 202 '[JSON] DeploymentStatusResponse
       -- DELETE /{id} → remove deployment
  :<|> DeleteNoContent
       -- GET /{id}/functions → list functions
  :<|> "functions" :> Get '[JSON] [SimpleFunction]
       -- GET /{id}/openapi.json → OpenAPI spec
  :<|> "openapi.json" :> Get '[JSON] Aeson.Value
       -- GET /{id}/files → file browsing
  :<|> ShortFileBrowseApi
       -- /{id}/{fn}/... → function routes
  :<|> Capture "name" Text :> FunctionRoutes

type DeploymentRoutes =
       "functions" :> Get '[JSON] [SimpleFunction]
  :<|> "functions" :> Capture "name" Text :> FunctionRoutes
  :<|> "openapi.json" :> Get '[JSON] Aeson.Value

-- | Evaluation responses include an X-Eval-Alloc-Bytes header reporting
-- the GHC allocation bytes consumed by the evaluation.
type EvalResponse a = Headers '[Header "X-Eval-Alloc-Bytes" Int64] a

type FunctionRoutes =
       Get '[JSON] FunctionSummary
  :<|> "evaluation" :> Header "X-L4-Trace" Text :> QueryParam "trace" TraceLevel :> QueryParam "graphviz" Bool :> ReqBody '[JSON] FnArguments :> Post '[JSON] (EvalResponse SimpleResponse)
  :<|> "evaluation" :> "batch" :> Header "X-L4-Trace" Text :> QueryParam "trace" TraceLevel :> QueryParam "graphviz" Bool :> ReqBody '[JSON] BatchRequest :> Post '[JSON] (EvalResponse BatchResponse)
  :<|> "query-plan" :> ReqBody '[JSON] FnArguments :> Post '[JSON] QueryPlanResponse
  :<|> "state-graphs" :> Get '[JSON] StateGraphListResponse
  :<|> "state-graphs" :> Capture "graphName" Text :> Get '[PlainText] Text

-- | Combined handler.
dataPlaneHandler :: Visibility -> ServerT DataPlaneApi AppM
dataPlaneHandler vis deployIdText =
       listFunctionsHandler deployId
  :<|> functionRoutesHandler deployId
  :<|> openApiHandler vis deployId
 where
  deployId = DeploymentId deployIdText

-- | Handler for short routes: /{id}/...
shortRoutesHandler :: Visibility -> ServerT ShortRoutes AppM
shortRoutesHandler vis deployIdText =
       getDeploymentHandler vis deployIdText
  :<|> putDeploymentHandler deployIdText
  :<|> deleteDeploymentHandler deployIdText
  :<|> listFunctionsHandler deployId
  :<|> openApiHandler vis deployId
  :<|> shortFileBrowseHandler deployId
  :<|> functionRoutesHandler deployId
 where
  deployId = DeploymentId deployIdText

functionRoutesHandler :: DeploymentId -> Text -> ServerT FunctionRoutes AppM
functionRoutesHandler deployId fnName =
       getFunctionHandler deployId fnName
  :<|> evalFunctionHandler deployId fnName
  :<|> batchFunctionHandler deployId fnName
  :<|> queryPlanHandler' deployId fnName
  :<|> listStateGraphsHandler deployId fnName
  :<|> getStateGraphDotHandler deployId fnName

-- ----------------------------------------------------------------------------
-- Deployment lookup helpers
-- ----------------------------------------------------------------------------

-- | Require a deployment to be in Ready state.
-- Uses optimistic compilation: if the deployment is Pending or Compiling,
-- waits up to 2 seconds for compilation to finish. If it finishes in time,
-- returns the result. Otherwise returns HTTP 202 with a "compiling" response.
requireDeploymentReady :: DeploymentId -> AppM (Map Text ValidatedFunction, DeploymentMetadata)
requireDeploymentReady deployId = do
  debugMode <- asks (.options.debug)
  -- First check if the deployment exists at all
  registry <- asks (.deploymentRegistry) >>= liftIO . readTVarIO
  case Map.lookup deployId registry of
    Nothing -> throwError err404
    _ -> pure ()

  result <- tryCompileWithTimeout deployId 2_000_000
  case result of
    CompilationReady fns meta -> pure (fns, meta)
    CompilationInProgress ->
      throwError ServerError
        { errHTTPCode = 202
        , errReasonPhrase = "Accepted"
        , errBody = Aeson.encode $ Aeson.object
            [ "status" .= ("compiling" :: Text)
            , "retryAfterMs" .= (2000 :: Int)
            ]
        , errHeaders = [("Retry-After", "2")]
        }
    CompilationError err ->
      if debugMode
        then throwError err500 { errBody = jsonError ("Deployment compilation failed: " <> err) }
        else throwError err500 { errBody = jsonError "Deployment compilation failed" }

-- | Look up a function by name within a deployment.
-- Tries the exact name first, then falls back to matching by sanitized name
-- (so that "check-person" in a URL matches the function "check person").
requireFunction :: Map Text ValidatedFunction -> Text -> AppM ValidatedFunction
requireFunction fns fnName =
  case Map.lookup fnName fns of
    Just vf -> pure vf
    Nothing ->
      -- Fall back: find a function whose name sanitizes to the same form
      let sanitized = sanitizePropertyName fnName
          match = find (\(k, _) -> sanitizePropertyName k == sanitized) (Map.toList fns)
      in case match of
        Just (_, vf) -> pure vf
        Nothing -> throwError err404 { errBody = jsonError "Function not found" }

-- ----------------------------------------------------------------------------
-- Handlers
-- ----------------------------------------------------------------------------

-- | GET /deployments/{id}/functions
listFunctionsHandler :: DeploymentId -> AppM [SimpleFunction]
listFunctionsHandler deployId = do
  logger <- asks (.logger)
  liftIO $ logInfo logger "Functions listed"
    [("deploymentId", Aeson.toJSON deployId.unDeploymentId)]
  (fns, _meta) <- requireDeploymentReady deployId
  pure [SimpleFunction fn.fnImpl.name fn.fnImpl.description | fn <- Map.elems fns]

-- | GET /deployments/{id}/functions/{fn}
getFunctionHandler :: DeploymentId -> Text -> AppM FunctionSummary
getFunctionHandler deployId fnName = do
  logger <- asks (.logger)
  liftIO $ logInfo logger "Function retrieved"
    [ ("deploymentId", Aeson.toJSON deployId.unDeploymentId)
    , ("functionName", Aeson.toJSON fnName)
    ]
  (fns, meta) <- requireDeploymentReady deployId
  vf <- requireFunction fns fnName
  let fn = vf.fnImpl
  -- Find the source file from metadata
  let sourceFile = case [fs.fsSourceFile | fs <- meta.metaFunctions, fs.fsName == fn.name] of
        (sf:_) -> sf
        [] -> Nothing
  pure FunctionSummary
    { fsName = fn.name
    , fsDescription = fn.description
    , fsParameters = fn.parameters
    , fsReturnType = fn.returnType
    , fsSection = Nothing
    , fsIsDeontic = fn.isDeontic
    , fsSourceFile = sourceFile
    }

-- | POST /deployments/{id}/functions/{fn}/evaluation
evalFunctionHandler
  :: DeploymentId
  -> Text
  -> Maybe Text       -- X-L4-Trace header
  -> Maybe TraceLevel -- ?trace= query param
  -> Maybe Bool       -- ?graphviz= query param
  -> FnArguments
  -> AppM (EvalResponse SimpleResponse)
evalFunctionHandler deployId fnName mTraceHeader mTraceParam mGraphViz fnArgs = do
  logger <- asks (.logger)
  liftIO $ logInfo logger "Function evaluated"
    [ ("deploymentId", Aeson.toJSON deployId.unDeploymentId)
    , ("functionName", Aeson.toJSON fnName)
    ]
  (fns, _meta) <- requireDeploymentReady deployId
  vf <- requireFunction fns fnName

  -- Check if this function has deontic parameters (indicating DEONTIC return type)
  let paramMap = vf.fnImpl.parameters.parameterMap :: Map Text Parameter
      isDeontic = Map.member "startTime" paramMap
                  && Map.member "events" paramMap

  -- Build reverse mapping so REST API accepts both hyphenated and spaced field names
  let reverseMap = buildPropertyReverseMap vf.fnImpl.parameters
      rawArgs = Map.toList fnArgs.fnArguments
      remappedArgs = remapArguments reverseMap rawArgs

  (result, allocBytes) <- case (isDeontic, fnArgs.startTime, fnArgs.events) of
    -- Non-deontic function: reject deontic params
    (False, Just _, _) ->
      throwError err400 { errBody = jsonError "startTime and events are only valid for functions returning DEONTIC" }
    (False, _, Just _) ->
      throwError err400 { errBody = jsonError "startTime and events are only valid for functions returning DEONTIC" }
    -- Non-deontic function: existing path
    (False, Nothing, Nothing) ->
      runEvaluatorFor vf fnArgs.fnEvalBackend remappedArgs Nothing mTraceHeader mTraceParam mGraphViz
    -- Deontic function: require both startTime and events
    (True, Nothing, _) ->
      throwError err400 { errBody = jsonError "startTime is required for functions returning DEONTIC" }
    (True, _, Nothing) ->
      throwError err400 { errBody = jsonError "events is required for functions returning DEONTIC" }
    -- Deontic function with both params: use deontic evaluator
    (True, Just st, Just evts) ->
      runDeonticEvaluatorFor vf fnArgs.fnEvalBackend remappedArgs st evts
        vf.fnImpl.deonticPartyType vf.fnImpl.deonticActionType
        mTraceHeader mTraceParam mGraphViz
  pure $ addHeader allocBytes result

-- | POST /deployments/{id}/functions/{fn}/evaluation/batch
batchFunctionHandler
  :: DeploymentId
  -> Text
  -> Maybe Text       -- X-L4-Trace header
  -> Maybe TraceLevel -- ?trace= query param
  -> Maybe Bool       -- ?graphviz= query param
  -> BatchRequest
  -> AppM (EvalResponse BatchResponse)
batchFunctionHandler deployId fnName mTraceHeader mTraceParam mGraphViz batchArgs = do
  logger <- asks (.logger)
  liftIO $ logInfo logger "Batch evaluated"
    [ ("deploymentId", Aeson.toJSON deployId.unDeploymentId)
    , ("functionName", Aeson.toJSON fnName)
    ]
  let traceLevel = determineTraceLevel mTraceHeader mTraceParam
      includeGraphViz = traceLevel == TraceFull && Maybe.fromMaybe False mGraphViz
  (fns, _meta) <- requireDeploymentReady deployId
  vf <- requireFunction fns fnName

  -- Capture the environment before going concurrent
  env <- ask

  -- Build reverse mapping so REST API accepts both hyphenated and spaced field names
  let reverseMap = buildPropertyReverseMap vf.fnImpl.parameters

  -- Evaluate all cases in parallel, collecting alloc bytes per case
  evalResults <- liftIO $ forConcurrently batchArgs.cases $ \inputCase -> do
    let args = remapArguments reverseMap $ Map.assocs $ fmap Just inputCase.attributes
    r <- runAppM env (runEvaluatorForDirect vf Nothing args outputFilter traceLevel includeGraphViz)
    pure (inputCase.id, r)

  -- Check for fatal errors and propagate
  case [err | (_, Left err) <- evalResults] of
    (err:_) -> throwError err
    [] -> pure ()

  let
    responses = [(rid, simpleResp, alloc) | (rid, Right (simpleResp, alloc)) <- evalResults]
    nCases = length responses
    totalAllocBytes = sum [alloc | (_, _, alloc) <- responses]

    successfulRuns =
      Maybe.mapMaybe
        ( \(rid, simpleRes, _) -> case simpleRes of
            SimpleResponse r -> Just (rid, r)
            SimpleError _ -> Nothing
        )
        responses

    nSuccessful = length successfulRuns
    nIgnored = nCases - nSuccessful

  pure $ addHeader totalAllocBytes $ BatchResponse
    { cases =
        [ OutputCase
          { id = rid
          , attributes = response.fnResult
          , graphviz = response.graphviz
          }
        | (rid, response) <- successfulRuns
        ]
    , summary = OutputSummary
        { casesRead = nCases
        , casesProcessed = nSuccessful
        , casesIgnored = nIgnored
        , processorDurationSec = 0
        , casesPerSec = 0
        , processorQueuedSec = 0
        }
    }
 where
  outputFilter = if Set.null outputFilter' then Nothing else Just outputFilter'
  outputFilter' =
    Set.fromList $
      Maybe.mapMaybe
        ( \case
            OutcomeAttribute t -> Just t
            OutcomePropertyObject _ -> Nothing
        )
        batchArgs.outcomes

-- | POST /deployments/{id}/functions/{fn}/query-plan
queryPlanHandler' :: DeploymentId -> Text -> FnArguments -> AppM QueryPlanResponse
queryPlanHandler' deployId fnName fnArgs = do
  logger <- asks (.logger)
  liftIO $ logInfo logger "Query plan generated"
    [ ("deploymentId", Aeson.toJSON deployId.unDeploymentId)
    , ("functionName", Aeson.toJSON fnName)
    ]
  (fns, _meta) <- requireDeploymentReady deployId
  vf <- requireFunction fns fnName

  cached <- case vf.fnDecisionQueryCache of
    Just c -> pure c
    Nothing -> do
      compiled <- case vf.fnCompiled of
        Nothing -> throwError err500 { errBody = jsonError "No compiled module available for query-plan" }
        Just c -> pure c
      c <- buildDecisionQueryCacheFromCompiled fnName compiled vf.fnSources
      storeDecisionQueryCache deployId fnName c
      pure c

  pure $ queryPlan fnName cached fnArgs

-- | Persist a freshly-built decision query cache back into the deployment registry
-- so subsequent requests can reuse it without rebuilding.
storeDecisionQueryCache :: DeploymentId -> Text -> CachedDecisionQuery -> AppM ()
storeDecisionQueryCache deployId fnName cache = do
  registryTVar <- asks (.deploymentRegistry)
  liftIO $ atomically $ modifyTVar' registryTVar $ \registry ->
    case Map.lookup deployId registry of
      Just (DeploymentReady fns meta) ->
        case Map.lookup fnName fns of
          Just vf ->
            let vf' = vf { fnDecisionQueryCache = Just cache }
            in Map.insert deployId (DeploymentReady (Map.insert fnName vf' fns) meta) registry
          Nothing -> registry
      _ -> registry

-- ----------------------------------------------------------------------------
-- State Graph Handlers
-- ----------------------------------------------------------------------------

-- | GET /deployments/{id}/functions/{fn}/state-graphs
listStateGraphsHandler :: DeploymentId -> Text -> AppM StateGraphListResponse
listStateGraphsHandler deployId fnName = do
  logger <- asks (.logger)
  liftIO $ logInfo logger "State graphs listed"
    [ ("deploymentId", Aeson.toJSON deployId.unDeploymentId)
    , ("functionName", Aeson.toJSON fnName)
    ]
  (fns, _meta) <- requireDeploymentReady deployId
  vf <- requireFunction fns fnName
  case vf.fnCompiled of
    Nothing -> throwError err404 { errBody = jsonError "No compiled module found for function" }
    Just compiled -> do
      let graphs = StateGraph.extractStateGraphs compiled.compiledModule
      pure $ StateGraphListResponse
        { graphs = map (\sg -> StateGraphInfo sg.sgName Nothing) graphs
        }

-- | GET /deployments/{id}/functions/{fn}/state-graphs/{graphName}
getStateGraphDotHandler :: DeploymentId -> Text -> Text -> AppM Text
getStateGraphDotHandler deployId fnName graphName = do
  logger <- asks (.logger)
  liftIO $ logInfo logger "State graph retrieved"
    [ ("deploymentId", Aeson.toJSON deployId.unDeploymentId)
    , ("functionName", Aeson.toJSON fnName)
    , ("graphName", Aeson.toJSON graphName)
    ]
  (fns, _meta) <- requireDeploymentReady deployId
  vf <- requireFunction fns fnName
  case vf.fnCompiled of
    Nothing -> throwError err404 { errBody = jsonError "No compiled module found for function" }
    Just compiled -> do
      let graphs = StateGraph.extractStateGraphs compiled.compiledModule
      case find (\sg -> sg.sgName == graphName) graphs of
        Nothing -> throwError err404 { errBody = jsonError "State graph not found" }
        Just graph -> do
          let opts = StateGraph.defaultStateGraphOptions
          pure $ StateGraph.stateGraphToDot opts graph

-- | GET /deployments/{id}/openapi.json
-- Serves from in-memory registry if ready, or from disk cache if pending/compiling.
-- Never triggers compilation — only evaluation endpoints should do that.
openApiHandler :: Visibility -> DeploymentId -> AppM Aeson.Value
openApiHandler vis deployId = do
  logger <- asks (.logger)
  serverName <- asks (.serverName)
  liftIO $ logInfo logger "OpenAPI spec retrieved"
    [("deploymentId", Aeson.toJSON deployId.unDeploymentId)]
  registry <- asks (.deploymentRegistry) >>= liftIO . readTVarIO
  meta <- case Map.lookup deployId registry of
    Just (DeploymentReady _fns meta) -> pure meta
    Just (DeploymentPending _) -> serveCachedOrFail deployId
    Just DeploymentCompiling -> serveCachedOrFail deployId
    Just (DeploymentFailed _) -> serveCachedOrFail deployId
    Nothing -> throwError err404
  pure $ buildOpenApiDoc serverName vis [(deployId.unDeploymentId, meta)]
 where
  serveCachedOrFail did = do
    cached <- tryLoadCachedMeta did
    case cached of
      Just meta -> pure meta
      Nothing -> throwError err503 { errBody = jsonError "Deployment is pending and no cached metadata available" }
  tryLoadCachedMeta did = do
    store <- asks (.bundleStore)
    mBytes <- liftIO $ BundleStore.loadMetadataCache store did.unDeploymentId
    case mBytes of
      Nothing -> pure Nothing
      Just bytes -> case Aeson.eitherDecode bytes of
        Left _ -> pure Nothing
        Right meta -> pure (Just meta)

-- ----------------------------------------------------------------------------
-- Evaluation helpers
-- ----------------------------------------------------------------------------

-- | Run a function evaluator, returning SimpleResponse and alloc bytes.
runEvaluatorFor
  :: ValidatedFunction
  -> Maybe EvalBackend
  -> [(Text, Maybe FnLiteral)]
  -> Maybe (Set.Set Text)
  -> Maybe Text       -- X-L4-Trace header
  -> Maybe TraceLevel -- ?trace= query param
  -> Maybe Bool       -- ?graphviz= query param
  -> AppM (SimpleResponse, Int64)
runEvaluatorFor vf engine args outputFilter mTraceHeader mTraceParam mGraphViz = do
  let traceLevel = determineTraceLevel mTraceHeader mTraceParam
      includeGraphViz = traceLevel == TraceFull && Maybe.fromMaybe False mGraphViz
  runEvaluatorForDirect vf engine args outputFilter traceLevel includeGraphViz

-- | Core evaluator logic. Returns the response and GHC allocation bytes consumed.
runEvaluatorForDirect
  :: ValidatedFunction
  -> Maybe EvalBackend
  -> [(Text, Maybe FnLiteral)]
  -> Maybe (Set.Set Text)
  -> TraceLevel
  -> Bool
  -> AppM (SimpleResponse, Int64)
runEvaluatorForDirect vf engine args outputFilter traceLevel includeGraphViz = do
  let evalBackend = Maybe.fromMaybe JL4 engine
  case Map.lookup evalBackend vf.fnEvaluator of
    Nothing -> throwError err500 { errBody = jsonError "No evaluator available for backend" }
    Just runFn -> do
      (evaluationResult, allocBytes) <-
        timeoutAction $
          runExceptT
            ( runFn.runFunction
                args
                outputFilter
                traceLevel
                includeGraphViz
            )
      case evaluationResult of
        Left err -> pure (SimpleError err, allocBytes)
        Right r -> pure (SimpleResponse r, allocBytes)

-- | Run deontic evaluation with EVALTRACE.
runDeonticEvaluatorFor
  :: ValidatedFunction
  -> Maybe EvalBackend
  -> [(Text, Maybe FnLiteral)]
  -> Scientific       -- ^ Start time
  -> [TraceEvent]     -- ^ Events
  -> Maybe Text       -- ^ Party type name
  -> Maybe Text       -- ^ Action type name
  -> Maybe Text       -- X-L4-Trace header
  -> Maybe TraceLevel -- ?trace= query param
  -> Maybe Bool       -- ?graphviz= query param
  -> AppM (SimpleResponse, Int64)
runDeonticEvaluatorFor vf _engine args startTime events mPartyType mActionType mTraceHeader mTraceParam mGraphViz = do
  let traceLevel = determineTraceLevel mTraceHeader mTraceParam
      includeGraphViz = traceLevel == TraceFull && Maybe.fromMaybe False mGraphViz

  compiled <- case vf.fnCompiled of
    Nothing -> throwError err500 { errBody = jsonError "No compiled module available for deontic evaluation" }
    Just c -> pure c

  let fnDecl = toDecl vf.fnImpl
      filepath = Text.unpack vf.fnImpl.name <.> "l4"

  (evaluationResult, allocBytes) <-
    timeoutAction $
      runExceptT
        ( evaluateWithCompiledDeontic
            filepath
            fnDecl
            compiled
            args
            startTime
            events
            mPartyType
            mActionType
            traceLevel
            includeGraphViz
        )

  case evaluationResult of
    Left err -> pure (SimpleError err, allocBytes)
    Right r -> pure (SimpleResponse r, allocBytes)

-- | Run an AppM action in IO with a given environment.
runAppM :: AppEnv -> AppM a -> IO (Either ServerError a)
runAppM env action = runHandler $ runReaderT action env

-- | Timeout and memory-limited evaluation action.
-- Uses configurable eval timeout and per-evaluation allocation limits.
-- Returns the result and the number of GHC allocation bytes consumed.
timeoutAction :: IO b -> AppM (b, Int64)
timeoutAction act = do
  cfg <- asks (.options)
  let timeoutMicros = cfg.evalTimeout * 1_000_000
      memLimitBytes = fromIntegral cfg.maxEvalMemoryMb * 1024 * 1024 :: Int64
  result <- liftIO $
    (timeout timeoutMicros $ do
      setAllocationCounter memLimitBytes
      enableAllocationLimit
      r <- act
      remaining <- getAllocationCounter
      pure (r, memLimitBytes - remaining)
    ) `catch` \AllocationLimitExceeded ->
      pure Nothing
  case result of
    Nothing -> throwError err500 { errBody = jsonError "Evaluation resource limit exceeded" }
    Just x -> pure x

-- ----------------------------------------------------------------------------
-- Helpers
-- ----------------------------------------------------------------------------

-- | Determine trace level from header and query parameter.
determineTraceLevel :: Maybe Text -> Maybe TraceLevel -> TraceLevel
determineTraceLevel mHeader mParam =
  case mHeader of
    Just "full" -> TraceFull
    Just "none" -> TraceNone
    _ -> maybe TraceNone id mParam

