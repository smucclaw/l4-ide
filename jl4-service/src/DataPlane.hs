{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module DataPlane (
  DataPlaneApi,
  dataPlaneHandler,
) where

import Backend.Api
import Backend.DecisionQueryPlan (CachedDecisionQuery, buildDecisionQueryCacheFromCompiled, queryPlan, QueryPlanResponse)
import Backend.Jl4 (CompiledModule (..))
import qualified L4.StateGraph as StateGraph
import Options (Options (..))
import Types

import Data.Int (Int64)
import Control.Concurrent.Async (forConcurrently)
import Control.Concurrent.STM (atomically, modifyTVar', readTVarIO)
import Control.Exception (catch)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.Trans.Reader (runReaderT, asks, ask)
import qualified Data.ByteString.Lazy as LBS
import Data.List (find)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text.Encoding as Text.Encoding
import GHC.Conc (setAllocationCounter, enableAllocationLimit)
import GHC.IO.Exception (AllocationLimitExceeded (..))
import Servant
import System.Timeout (timeout)

-- | The data plane API — per-deployment function evaluation.
type DataPlaneApi =
  "deployments" :> Capture "deploymentId" Text :> DeploymentRoutes

type DeploymentRoutes =
       "functions" :> Get '[JSON] [SimpleFunction]
  :<|> "functions" :> Capture "name" Text :> FunctionRoutes
  :<|> "openapi.json" :> Get '[JSON] DeploymentMetadata  -- Simplified; returns metadata for now

type FunctionRoutes =
       Get '[JSON] Function
  :<|> "evaluation" :> Header "X-L4-Trace" Text :> QueryParam "trace" TraceLevel :> QueryParam "graphviz" Bool :> ReqBody '[JSON] FnArguments :> Post '[JSON] SimpleResponse
  :<|> "evaluation" :> "batch" :> Header "X-L4-Trace" Text :> QueryParam "trace" TraceLevel :> QueryParam "graphviz" Bool :> ReqBody '[JSON] BatchRequest :> Post '[JSON] BatchResponse
  :<|> "query-plan" :> ReqBody '[JSON] FnArguments :> Post '[JSON] QueryPlanResponse
  :<|> "state-graphs" :> Get '[JSON] StateGraphListResponse
  :<|> "state-graphs" :> Capture "graphName" Text :> Get '[PlainText] Text

-- | Combined handler.
dataPlaneHandler :: ServerT DataPlaneApi AppM
dataPlaneHandler deployIdText =
       listFunctionsHandler deployId
  :<|> functionRoutesHandler deployId
  :<|> openApiHandler deployId
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
-- In non-debug mode, error details are hidden.
requireDeploymentReady :: DeploymentId -> AppM (Map Text ValidatedFunction, DeploymentMetadata)
requireDeploymentReady deployId = do
  debugMode <- asks (.options.debug)
  registry <- asks (.deploymentRegistry) >>= liftIO . readTVarIO
  case Map.lookup deployId registry of
    Nothing -> throwError err404
    Just DeploymentPending ->
      throwError err503 { errBody = "Deployment is still compiling" }
    Just (DeploymentFailed msg) ->
      if debugMode
        then throwError err500 { errBody = textToLBS ("Deployment compilation failed: " <> msg) }
        else throwError err500 { errBody = "Deployment compilation failed" }
    Just (DeploymentReady fns meta) ->
      pure (fns, meta)

-- | Look up a function by name within a deployment.
requireFunction :: Map Text ValidatedFunction -> Text -> AppM ValidatedFunction
requireFunction fns fnName =
  case Map.lookup fnName fns of
    Nothing -> throwError err404 { errBody = "Function not found" }
    Just vf -> pure vf

-- ----------------------------------------------------------------------------
-- Handlers
-- ----------------------------------------------------------------------------

-- | GET /deployments/{id}/functions
listFunctionsHandler :: DeploymentId -> AppM [SimpleFunction]
listFunctionsHandler deployId = do
  (fns, _meta) <- requireDeploymentReady deployId
  pure [SimpleFunction fn.fnImpl.name fn.fnImpl.description | fn <- Map.elems fns]

-- | GET /deployments/{id}/functions/{fn}
getFunctionHandler :: DeploymentId -> Text -> AppM Function
getFunctionHandler deployId fnName = do
  (fns, _meta) <- requireDeploymentReady deployId
  vf <- requireFunction fns fnName
  pure vf.fnImpl

-- | POST /deployments/{id}/functions/{fn}/evaluation
evalFunctionHandler
  :: DeploymentId
  -> Text
  -> Maybe Text       -- X-L4-Trace header
  -> Maybe TraceLevel -- ?trace= query param
  -> Maybe Bool       -- ?graphviz= query param
  -> FnArguments
  -> AppM SimpleResponse
evalFunctionHandler deployId fnName mTraceHeader mTraceParam mGraphViz fnArgs = do
  (fns, _meta) <- requireDeploymentReady deployId
  vf <- requireFunction fns fnName
  runEvaluatorFor vf fnArgs.fnEvalBackend (Map.toList fnArgs.fnArguments) Nothing mTraceHeader mTraceParam mGraphViz

-- | POST /deployments/{id}/functions/{fn}/evaluation/batch
batchFunctionHandler
  :: DeploymentId
  -> Text
  -> Maybe Text       -- X-L4-Trace header
  -> Maybe TraceLevel -- ?trace= query param
  -> Maybe Bool       -- ?graphviz= query param
  -> BatchRequest
  -> AppM BatchResponse
batchFunctionHandler deployId fnName mTraceHeader mTraceParam mGraphViz batchArgs = do
  let traceLevel = determineTraceLevel mTraceHeader mTraceParam
      includeGraphViz = traceLevel == TraceFull && Maybe.fromMaybe False mGraphViz
  (fns, _meta) <- requireDeploymentReady deployId
  vf <- requireFunction fns fnName

  -- Capture the environment before going concurrent
  env <- ask

  -- Evaluate all cases in parallel
  evalResults <- liftIO $ forConcurrently batchArgs.cases $ \inputCase -> do
    let args = Map.assocs $ fmap Just inputCase.attributes
    r <- runAppM env (runEvaluatorForDirect vf Nothing args outputFilter traceLevel includeGraphViz)
    pure (inputCase.id, r)

  -- Check for fatal errors and propagate
  case [err | (_, Left err) <- evalResults] of
    (err:_) -> throwError err
    [] -> pure ()

  let
    responses = [(rid, simpleResp) | (rid, Right simpleResp) <- evalResults]
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

  pure $ BatchResponse
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
  (fns, _meta) <- requireDeploymentReady deployId
  vf <- requireFunction fns fnName

  cached <- case vf.fnDecisionQueryCache of
    Just c -> pure c
    Nothing -> do
      compiled <- case vf.fnCompiled of
        Nothing -> throwError err500 { errBody = "No compiled module available for query-plan" }
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
  (fns, _meta) <- requireDeploymentReady deployId
  vf <- requireFunction fns fnName
  case vf.fnCompiled of
    Nothing -> throwError err404 { errBody = "No compiled module found for function" }
    Just compiled -> do
      let graphs = StateGraph.extractStateGraphs compiled.compiledModule
      pure $ StateGraphListResponse
        { graphs = map (\sg -> StateGraphInfo sg.sgName Nothing) graphs
        }

-- | GET /deployments/{id}/functions/{fn}/state-graphs/{graphName}
getStateGraphDotHandler :: DeploymentId -> Text -> Text -> AppM Text
getStateGraphDotHandler deployId fnName graphName = do
  (fns, _meta) <- requireDeploymentReady deployId
  vf <- requireFunction fns fnName
  case vf.fnCompiled of
    Nothing -> throwError err404 { errBody = "No compiled module found for function" }
    Just compiled -> do
      let graphs = StateGraph.extractStateGraphs compiled.compiledModule
      case find (\sg -> sg.sgName == graphName) graphs of
        Nothing -> throwError err404 { errBody = "State graph not found" }
        Just graph -> do
          let opts = StateGraph.defaultStateGraphOptions
          pure $ StateGraph.stateGraphToDot opts graph

-- | GET /deployments/{id}/openapi.json
openApiHandler :: DeploymentId -> AppM DeploymentMetadata
openApiHandler deployId = do
  (_fns, meta) <- requireDeploymentReady deployId
  pure meta

-- ----------------------------------------------------------------------------
-- Evaluation helpers
-- ----------------------------------------------------------------------------

-- | Run a function evaluator, returning SimpleResponse.
runEvaluatorFor
  :: ValidatedFunction
  -> Maybe EvalBackend
  -> [(Text, Maybe FnLiteral)]
  -> Maybe (Set.Set Text)
  -> Maybe Text       -- X-L4-Trace header
  -> Maybe TraceLevel -- ?trace= query param
  -> Maybe Bool       -- ?graphviz= query param
  -> AppM SimpleResponse
runEvaluatorFor vf engine args outputFilter mTraceHeader mTraceParam mGraphViz = do
  let traceLevel = determineTraceLevel mTraceHeader mTraceParam
      includeGraphViz = traceLevel == TraceFull && Maybe.fromMaybe False mGraphViz
  runEvaluatorForDirect vf engine args outputFilter traceLevel includeGraphViz

-- | Core evaluator logic.
runEvaluatorForDirect
  :: ValidatedFunction
  -> Maybe EvalBackend
  -> [(Text, Maybe FnLiteral)]
  -> Maybe (Set.Set Text)
  -> TraceLevel
  -> Bool
  -> AppM SimpleResponse
runEvaluatorForDirect vf engine args outputFilter traceLevel includeGraphViz = do
  let evalBackend = Maybe.fromMaybe JL4 engine
  case Map.lookup evalBackend vf.fnEvaluator of
    Nothing -> throwError err500 { errBody = "No evaluator available for backend" }
    Just runFn -> do
      evaluationResult <-
        timeoutAction $
          runExceptT
            ( runFn.runFunction
                args
                outputFilter
                traceLevel
                includeGraphViz
            )
      case evaluationResult of
        Left err -> pure $ SimpleError err
        Right r -> pure $ SimpleResponse r

-- | Run an AppM action in IO with a given environment.
runAppM :: AppEnv -> AppM a -> IO (Either ServerError a)
runAppM env action = runHandler $ runReaderT action env

-- | Timeout and memory-limited evaluation action.
-- Uses configurable eval timeout and per-evaluation allocation limits.
timeoutAction :: IO b -> AppM b
timeoutAction act = do
  cfg <- asks (.options)
  let timeoutMicros = cfg.evalTimeout * 1_000_000
      memLimitBytes = fromIntegral cfg.maxEvalMemoryMb * 1024 * 1024 :: Int64
  result <- liftIO $
    (timeout timeoutMicros $ do
      setAllocationCounter memLimitBytes
      enableAllocationLimit
      act
    ) `catch` \AllocationLimitExceeded ->
      pure Nothing
  case result of
    Nothing -> throwError err500 { errBody = "Evaluation resource limit exceeded" }
    Just r -> pure r

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

-- | Convert Text to lazy ByteString for error bodies.
textToLBS :: Text -> LBS.ByteString
textToLBS = LBS.fromStrict . Text.Encoding.encodeUtf8
