{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module McpServer (
  mcpHandler,
  -- * Tasks extension lifecycle
  reapExpiredTasks,
  -- * Constants (exported for discovery handlers / tests)
  supportedProtocolVersions,
  defaultProtocolVersion,
  tasksExtensionKey,
) where

import qualified BundleStore
import DeploymentLoader (tryCompileWithTimeout, CompilationResult (..))
import FileBrowser (searchIdentifier, searchText, SearchMatch (..))
import Logging (logInfo, logWarn)
import Shared (collectMetadataEntries, sanitizeParameters, buildPropertyReverseMap, remapFnLiteralKeys, sanitizeFieldNamesInText)
import Types

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (asks, ask)
import Control.Monad.Trans.Except (runExceptT)
import Control.Concurrent.Async (async, cancel)
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (atomically, modifyTVar', readTVar, readTVarIO, writeTVar, TVar)
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Aeson.Key
import qualified Data.Aeson.KeyMap as Aeson.KeyMap
import Data.Char (isAlphaNum)
import Data.Int (Int64)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Maybe as Maybe
import Data.Time (UTCTime, getCurrentTime, diffUTCTime)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import GHC.Conc (setAllocationCounter, getAllocationCounter, enableAllocationLimit)
import Control.Exception (SomeException, catch)
import GHC.IO.Exception (AllocationLimitExceeded (..))
import System.Timeout (timeout)

import Backend.Api (EvalBackend (..), FnLiteral (..), RunFunction (..), TraceLevel (..), prettyEvaluatorError)
import Options (Options (..))

-- ----------------------------------------------------------------------------
-- Protocol versions and extension identifiers
-- ----------------------------------------------------------------------------

-- | Protocol versions we accept on the wire, newest first. We negotiate
-- against this list when a client sends 'protocolVersion' in 'initialize'.
-- Keep oldest entries until we are ready to formally deprecate them per the
-- 2026-07-28 deprecation policy (12 months).
supportedProtocolVersions :: [Text]
supportedProtocolVersions =
  [ "2026-07-28"      -- final shipped name announced for the 2026 RC
  , "DRAFT-2026-v1"   -- in-flight draft constant from the spec repo
  , "2025-11-25"      -- previous GA
  , "2025-06-18"
  , "2025-03-26"      -- what jl4-service originally advertised
  ]

-- | What we advertise on the discovery endpoints when no client version
-- has been observed. Picking the latest is correct: clients that don't
-- know it MUST tolerate a higher number per spec.
defaultProtocolVersion :: Text
defaultProtocolVersion = "2026-07-28"

-- | Reverse-DNS identifier for the Tasks extension. Per the 2026-07-28
-- Extensions framework, all extensions are namespaced.
tasksExtensionKey :: Text
tasksExtensionKey = "io.modelcontextprotocol/tasks"

-- ----------------------------------------------------------------------------
-- Top-level handler
-- ----------------------------------------------------------------------------

-- | Handle an MCP JSON-RPC 2.0 request and return a JSON-RPC 2.0 response.
-- The first argument is an optional deployment scope (Nothing = org-wide).
-- Visibility controls which tools are advertised and callable.
--
-- This handler is fully stateless across requests: any two interleaved
-- calls observe identical state. Any per-conversation context (client info,
-- extension opt-ins) travels in '_meta' on every request.
mcpHandler :: Visibility -> Maybe Text -> Aeson.Value -> AppM Aeson.Value
mcpHandler vis mScope reqVal = do
  case parseJsonRpcRequest reqVal of
    Left errMsg ->
      pure $ jsonRpcError Nothing (-32700) errMsg
    Right (reqId, method, params) -> do
      logClientInfo params
      handleMethod vis mScope reqId method params

-- | Parse a JSON-RPC 2.0 request into (id, method, params).
parseJsonRpcRequest :: Aeson.Value -> Either Text (Maybe Aeson.Value, Text, Aeson.Value)
parseJsonRpcRequest val = case val of
  Aeson.Object obj -> do
    let mId = Aeson.KeyMap.lookup "id" obj
        mMethod = Aeson.KeyMap.lookup "method" obj
        params = Maybe.fromMaybe (Aeson.object []) (Aeson.KeyMap.lookup "params" obj)
    case mMethod of
      Just (Aeson.String method) -> Right (mId, method, params)
      _ -> Left "Missing or invalid 'method' field"
  _ -> Left "Request must be a JSON object"

-- ----------------------------------------------------------------------------
-- Method dispatch
-- ----------------------------------------------------------------------------

handleMethod :: Visibility -> Maybe Text -> Maybe Aeson.Value -> Text -> Aeson.Value -> AppM Aeson.Value
handleMethod _vis mScope reqId "initialize" params = do
  -- Initialize is informational under the 2026-07-28 stateless model: we
  -- echo what we support but do not record session-state. Old clients
  -- (2025-03-26 / 2025-11-25) still use this; new clients may skip it.
  let negotiated = negotiateProtocolVersion params
  mInstr <- case mScope of
    Just scope -> do
      env <- ask
      registry <- liftIO $ readTVarIO env.deploymentRegistry
      pure $ case Map.lookup (DeploymentId scope) registry of
        Just (DeploymentReady _ meta)        -> meta.metaDescription
        Just (DeploymentPending (Just meta)) -> meta.metaDescription
        _                                    -> Nothing
    Nothing -> pure Nothing
  pure $ jsonRpcResult reqId $ Aeson.object $
    [ "protocolVersion" .= negotiated
    , "serverInfo" .= Aeson.object
        [ "name" .= ("L4 Tools" :: Text)
        , "version" .= ("1.0.0" :: Text)
        ]
    , "capabilities" .= serverCapabilities
    ]
    <> maybe [] (\d -> ["instructions" .= d]) mInstr

handleMethod _vis _mScope reqId "notifications/initialized" _params =
  -- The notification is meaningless in the stateless model; we acknowledge
  -- it so old clients (2025-03-26) are happy.
  pure $ jsonRpcResult reqId (Aeson.object [])

handleMethod vis mScope reqId "tools/list" _params = do
  tools <- buildToolList vis mScope
  pure $ jsonRpcResult reqId $ Aeson.object
    [ "tools" .= tools ]

handleMethod vis mScope reqId "tools/call" params = do
  case params of
    Aeson.Object obj -> do
      let mToolName = case Aeson.KeyMap.lookup "name" obj of
            Just (Aeson.String n) -> Just n
            _ -> Nothing
          mArguments = case Aeson.KeyMap.lookup "arguments" obj of
            Just v -> Just v
            _ -> Nothing
          tasksRequested = clientWantsTasks params
      case mToolName of
        Nothing ->
          pure $ jsonRpcError reqId (-32602) "Missing 'name' in params"
        Just toolName -> do
          callTool vis mScope reqId toolName tasksRequested
                   (Maybe.fromMaybe (Aeson.object []) mArguments)
    _ ->
      pure $ jsonRpcError reqId (-32602) "Invalid params for tools/call"

-- 2026-07-28 Tasks extension methods. We accept these regardless of the
-- protocol-version the client claimed: a future client that advertises a
-- lower version but still wants tasks shouldn't get a -32601.
handleMethod _vis _mScope reqId "tasks/get" params =
  handleTasksGet reqId params

handleMethod _vis _mScope reqId "tasks/update" params =
  handleTasksUpdate reqId params

handleMethod _vis _mScope reqId "tasks/cancel" params =
  handleTasksCancel reqId params

handleMethod _vis _mScope reqId method _params = do
  pure $ jsonRpcError reqId (-32601) ("Method not found: " <> method)

-- ----------------------------------------------------------------------------
-- Capabilities and version negotiation
-- ----------------------------------------------------------------------------

-- | Our advertised capabilities. Both 'extensions' (the 2026-07-28
-- canonical location for extension negotiation) and 'experimental' (used
-- by the original SEP-2663 Tasks PR) are populated so clients on either
-- shape recognise that we speak Tasks.
serverCapabilities :: Aeson.Value
serverCapabilities = Aeson.object
  [ "tools" .= Aeson.object []
  , "extensions" .= Aeson.object
      [ Aeson.Key.fromText tasksExtensionKey .= Aeson.object [] ]
  , "experimental" .= Aeson.object
      [ "ext-tasks" .= Aeson.object [] ]
  ]

-- | Pick the highest mutually supported protocol version, or
-- 'defaultProtocolVersion' when the client did not send one (or sent
-- one we do not recognise — per spec the client must tolerate this).
negotiateProtocolVersion :: Aeson.Value -> Text
negotiateProtocolVersion params = case params of
  Aeson.Object obj -> case Aeson.KeyMap.lookup "protocolVersion" obj of
    Just (Aeson.String clientVer)
      | clientVer `elem` supportedProtocolVersions -> clientVer
    _ -> defaultProtocolVersion
  _ -> defaultProtocolVersion

-- | Detect whether the client has opted in to Tasks for THIS request.
-- Looks in two places:
--   1. '_meta.io.modelcontextprotocol/tasks' (2026-07-28 reverse-DNS form)
--   2. '_meta.tasks' (short legacy form some early clients use)
-- Without an opt-in we return the synchronous fallback (the existing
-- text-encoded "{status:'compiling'}" payload), keeping 2025-era clients
-- working unchanged.
clientWantsTasks :: Aeson.Value -> Bool
clientWantsTasks params = case params of
  Aeson.Object obj -> case Aeson.KeyMap.lookup "_meta" obj of
    Just (Aeson.Object meta) ->
      hasTrueKey (Aeson.Key.fromText tasksExtensionKey) meta
        || hasTrueKey "tasks" meta
        || hasTrueKey "ext-tasks" meta
    _ -> False
  _ -> False
 where
  hasTrueKey k m = case Aeson.KeyMap.lookup k m of
    Just (Aeson.Bool True) -> True
    Just (Aeson.Object _) -> True  -- presence of a config object = opt-in
    _ -> False

-- | Log client identifiers carried in '_meta' when present. New clients
-- (2026-07-28) put this on every request instead of in 'initialize'.
logClientInfo :: Aeson.Value -> AppM ()
logClientInfo params = case params of
  Aeson.Object obj -> case Aeson.KeyMap.lookup "_meta" obj of
    Just (Aeson.Object meta) ->
      case Aeson.KeyMap.lookup "io.modelcontextprotocol/clientInfo" meta of
        Just info -> do
          env <- ask
          liftIO $ logInfo env.logger "MCP client info"
            [("clientInfo", info)]
        Nothing -> pure ()
    _ -> pure ()
  _ -> pure ()

-- ----------------------------------------------------------------------------
-- Tool list
-- ----------------------------------------------------------------------------

-- | Build the list of tools from deployment metadata.
-- Visibility controls which tools are included:
--   showFunctions && showEvaluate → include per-function evaluation tools
--   showFiles                     → include file browsing tools
buildToolList :: Visibility -> Maybe Text -> AppM [Aeson.Value]
buildToolList vis mScope = do
  fnTools <- if vis.showFunctions && vis.showEvaluate
    then do
      entries <- collectMetadataEntries mScope
      let toolNames = buildToolNames entries
      pure [ Aeson.object
        [ "name" .= tn
        , "description" .= ("L4 Rule: " <> fn.fsDescription <> " [" <> deployId <> "/" <> fn.fsName <> "]")
        , "inputSchema" .= mcpToolSchema fn
        ]
        | (tn, deployId, fn) <- toolNames
        ]
    else pure []

  let fileTools = if vis.showFiles then fileToolDefinitions else []

  pure (fnTools ++ fileTools)

mcpToolSchema :: FunctionSummary -> Aeson.Value
mcpToolSchema fn
  | "DEONTIC" `Text.isPrefixOf` fn.fsReturnType =
      Aeson.object
        [ "type" .= ("object" :: Text)
        , "properties" .= Aeson.object
            [ "arguments" .= sanitizeParameters fn.fsParameters
            , "startTime" .= Aeson.object
                [ "type" .= ("number" :: Text)
                , "description" .= ("Start time for contract simulation" :: Text)
                ]
            , "events" .= Aeson.object
                [ "type" .= ("array" :: Text)
                , "description" .= ("Events for contract simulation (each: {party, action, at})" :: Text)
                , "items" .= Aeson.object ["type" .= ("object" :: Text)]
                ]
            ]
        , "required" .= (["arguments", "startTime", "events"] :: [Text])
        ]
  | otherwise = sanitizeParameters fn.fsParameters

fileToolDefinitions :: [Aeson.Value]
fileToolDefinitions =
  [ Aeson.object
    [ "name" .= ("list_files" :: Text)
    , "description" .= ("Tooling: List .l4 files and their exports in a deployment" :: Text)
    , "inputSchema" .= Aeson.object
        [ "type" .= ("object" :: Text)
        , "properties" .= Aeson.object
            [ "deployment" .= Aeson.object
                [ "type" .= ("string" :: Text)
                , "description" .= ("Omit for all deployments" :: Text)
                ]
            ]
        ]
    ]
  , Aeson.object
    [ "name" .= ("read_file" :: Text)
    , "description" .= ("Tooling: Read .l4 source file content, optionally a line range" :: Text)
    , "inputSchema" .= Aeson.object
        [ "type" .= ("object" :: Text)
        , "properties" .= Aeson.object
            [ "deployment" .= Aeson.object
                [ "type" .= ("string" :: Text)
                ]
            , "path" .= Aeson.object
                [ "type" .= ("string" :: Text)
                , "description" .= ("e.g. rules/main.l4" :: Text)
                ]
            , "lines" .= Aeson.object
                [ "type" .= ("string" :: Text)
                , "description" .= ("start:end (e.g. 85:89)" :: Text)
                ]
            ]
        , "required" .= (["deployment", "path"] :: [Text])
        ]
    ]
  , Aeson.object
    [ "name" .= ("search_identifier" :: Text)
    , "description" .= ("Tooling: Find definitions and references of an L4 identifier. Text-based, may match in comments." :: Text)
    , "inputSchema" .= Aeson.object
        [ "type" .= ("object" :: Text)
        , "properties" .= Aeson.object
            [ "identifier" .= Aeson.object
                [ "type" .= ("string" :: Text)
                ]
            , "deployment" .= Aeson.object
                [ "type" .= ("string" :: Text)
                , "description" .= ("Omit for all deployments" :: Text)
                ]
            , "file" .= Aeson.object
                [ "type" .= ("string" :: Text)
                , "description" .= ("Scope to one file" :: Text)
                ]
            ]
        , "required" .= (["identifier"] :: [Text])
        ]
    ]
  , Aeson.object
    [ "name" .= ("search_text" :: Text)
    , "description" .= ("Tooling: Search text in .l4 source files (case-insensitive)" :: Text)
    , "inputSchema" .= Aeson.object
        [ "type" .= ("object" :: Text)
        , "properties" .= Aeson.object
            [ "query" .= Aeson.object
                [ "type" .= ("string" :: Text)
                ]
            , "deployment" .= Aeson.object
                [ "type" .= ("string" :: Text)
                , "description" .= ("Omit for all deployments" :: Text)
                ]
            , "file" .= Aeson.object
                [ "type" .= ("string" :: Text)
                , "description" .= ("Scope to one file" :: Text)
                ]
            ]
        , "required" .= (["query"] :: [Text])
        ]
    ]
  ]

-- | Sanitize a name for use as a tool name. Allowed chars match
-- Anthropic's tool-name regex `^[a-zA-Z0-9_-]{1,64}$` exactly — no
-- dots, even though MCP spec is more permissive. Dots in the source
-- name are remapped to hyphens.
sanitizeName :: Int -> Text -> Text
sanitizeName maxLen name =
  let s = Text.map (\c -> if isAlphaNum c || c == '_' || c == '-' then c else '-') name
      s' = collapseHyphens $ Text.dropWhile (== '-') $ Text.dropWhileEnd (== '-') s
      s'' = if Text.null s' || not (isAlphaStart (Text.head s'))
            then "_" <> s' else s'
  in Text.take maxLen s''
 where
  isAlphaStart c = isAlphaNum c || c == '_'
  collapseHyphens t =
    let collapsed = Text.replace "--" "-" t
    in if collapsed == t then t else collapseHyphens collapsed

buildToolNames :: [(Text, FunctionSummary)] -> [(Text, Text, FunctionSummary)]
buildToolNames entries =
  let maxLen = 60
      groups :: Map Text [(Text, FunctionSummary)]
      groups = Map.fromListWith (++)
        [ (sanitizeName maxLen fn.fsName, [(deployId, fn)])
        | (deployId, fn) <- entries
        ]
  in concatMap (\(key, grp) ->
    case grp of
      [(deployId, fn)] -> [(key, deployId, fn)]
      _ ->
        let deps = [(sanitizeName maxLen deployId, deployId, fn) | (deployId, fn) <- grp]
        in [ let suffix = "-" <> Text.take prefixLen sanitizedDep
                 baseName = sanitizeName (maxLen - Text.length suffix) fn.fsName
                 prefixLen = findUniquePrefix sanitizedDep
                   [sd | (sd, _, _) <- deps, sd /= sanitizedDep]
             in (baseName <> suffix, deployId, fn)
           | (sanitizedDep, deployId, fn) <- deps
           ]
    ) (Map.toList groups)

findUniquePrefix :: Text -> [Text] -> Int
findUniquePrefix dep others = go 1
 where
  go n
    | n >= Text.length dep = Text.length dep
    | all (\o -> Text.take n o /= Text.take n dep) others = n
    | otherwise = go (n + 1)

-- ----------------------------------------------------------------------------
-- tools/call dispatch
-- ----------------------------------------------------------------------------

callTool :: Visibility -> Maybe Text -> Maybe Aeson.Value -> Text -> Bool -> Aeson.Value -> AppM Aeson.Value
callTool vis mScope reqId toolName tasksRequested arguments = do
  case toolName of
    "list_files" | vis.showFiles -> callListFiles reqId arguments
    "read_file" | vis.showFiles -> callReadFile reqId arguments
    "search_identifier" | vis.showFiles -> callSearchIdentifier reqId arguments
    "search_text" | vis.showFiles -> callSearchText reqId arguments
    _ -> callFunctionTool vis mScope reqId toolName tasksRequested arguments

-- | Execute a function evaluation tool call.
-- When the deployment is still compiling AND the client opted in to the
-- Tasks extension via _meta, we return a task handle and continue work
-- in the background. Otherwise we fall back to the legacy synchronous
-- "{status:'compiling'}" text payload that 2025-era clients understand.
callFunctionTool :: Visibility -> Maybe Text -> Maybe Aeson.Value -> Text -> Bool -> Aeson.Value -> AppM Aeson.Value
callFunctionTool _vis mScope reqId toolName tasksRequested arguments = do
  env <- ask
  entries <- collectMetadataEntries mScope
  let toolMap = Map.fromList
        [ (tn, (deployId, fn.fsName, fn.fsParameters, "DEONTIC" `Text.isPrefixOf` fn.fsReturnType))
        | (tn, deployId, fn) <- buildToolNames entries
        ]

  case Map.lookup toolName toolMap of
    Nothing -> do
      liftIO $ logWarn env.logger "MCP tool not found"
        [("toolName", Aeson.toJSON toolName)]
      pure $ jsonRpcError reqId (-32602) ("Unknown tool: " <> toolName)
    Just (deployId, fnName, params, isDeontic) -> do
      liftIO $ logInfo env.logger "MCP tool called"
        [ ("toolName", Aeson.toJSON toolName)
        , ("deploymentId", Aeson.toJSON deployId)
        , ("functionName", Aeson.toJSON fnName)
        , ("tasksRequested", Aeson.toJSON tasksRequested)
        ]
      let reverseMap = buildPropertyReverseMap params
      let argPairs = buildArgPairs reverseMap isDeontic arguments

      let did = DeploymentId deployId
      compResult <- tryCompileWithTimeout did 2_000_000
      case compResult of
        CompilationReady fns _meta ->
          case Map.lookup fnName fns of
            Just vf -> do
              evalResult <- runMcpEvaluation vf argPairs
              pure $ evalResultToToolResponse reqId reverseMap evalResult
            Nothing ->
              pure $ jsonRpcError reqId (-32602) ("Function not found: " <> fnName)
        CompilationInProgress
          | tasksRequested ->
              spawnEvaluationTask reqId did fnName isDeontic reverseMap argPairs
          | otherwise ->
              pure $ mcpTextResult reqId
                "{\"status\":\"compiling\",\"retryAfterMs\":2000}" False
        CompilationError err ->
          -- 2026-07-28 reassigns -32002 to MissingResource (JSON-RPC standard
          -- aliasing). Use -32000 (server-defined) for "deployment failed
          -- to compile" — this code is in the JSON-RPC server-error band
          -- and has no protocol meaning attached to it.
          pure $ jsonRpcError reqId (-32000) ("Deployment failed: " <> err)

-- | Build the (name, value) argument list from a tools/call invocation.
-- Splits deontic vs non-deontic arg layout and applies the reverse-mapping
-- from sanitized JSON keys to original L4 identifiers.
buildArgPairs :: Map Text Text -> Bool -> Aeson.Value -> [(Text, Maybe FnLiteral)]
buildArgPairs reverseMap isDeontic arguments = case arguments of
  Aeson.Object obj
    | isDeontic
    , Just (Aeson.Object argsObj) <- Aeson.KeyMap.lookup "arguments" obj ->
        [ (Map.findWithDefault k k reverseMap, remapFnLiteralKeys reverseMap <$> parseFnLiteral v)
        | (k, v) <- Map.toList (Aeson.KeyMap.toMapText argsObj)
        ]
    | otherwise ->
        [ (Map.findWithDefault k k reverseMap, remapFnLiteralKeys reverseMap <$> parseFnLiteral v)
        | (k, v) <- Map.toList (Aeson.KeyMap.toMapText obj)
        , Aeson.Key.fromText k `notElem` ["startTime", "events", "arguments"]
        ]
  _ -> []

-- | Convert the synchronous evaluation result (Left err | Right resultText)
-- into the MCP @tools/call@ JSON-RPC response shape.
evalResultToToolResponse :: Maybe Aeson.Value -> Map Text Text -> Either Text Text -> Aeson.Value
evalResultToToolResponse reqId reverseMap = \case
  Left errMsg ->
    let sanitizedErr = sanitizeFieldNamesInText reverseMap errMsg
    in jsonRpcResult reqId $ Aeson.object
        [ "content" .= [ Aeson.object
            [ "type" .= ("text" :: Text)
            , "text" .= sanitizedErr
            ]
          ]
        , "isError" .= True
        ]
  Right resultVal ->
    jsonRpcResult reqId $ Aeson.object
      [ "content" .= [ Aeson.object
          [ "type" .= ("text" :: Text)
          , "text" .= resultVal
          ]
        ]
      ]

-- | Like 'evalResultToToolResponse' but produces ONLY the result body
-- (omitting JSON-RPC envelope). Used to store completed-task payloads.
evalResultToToolPayload :: Map Text Text -> Either Text Text -> Aeson.Value
evalResultToToolPayload reverseMap = \case
  Left errMsg ->
    let sanitizedErr = sanitizeFieldNamesInText reverseMap errMsg
    in Aeson.object
        [ "content" .= [ Aeson.object
            [ "type" .= ("text" :: Text)
            , "text" .= sanitizedErr
            ]
          ]
        , "isError" .= True
        ]
  Right resultVal ->
    Aeson.object
      [ "content" .= [ Aeson.object
          [ "type" .= ("text" :: Text)
          , "text" .= resultVal
          ]
        ]
      ]

-- | Run evaluation for an MCP tool call.
runMcpEvaluation :: ValidatedFunction -> [(Text, Maybe FnLiteral)] -> AppM (Either Text Text)
runMcpEvaluation vf args = do
  cfg <- asks (.options)
  liftIO $ runMcpEvaluationIO cfg vf args

runMcpEvaluationIO :: Options -> ValidatedFunction -> [(Text, Maybe FnLiteral)] -> IO (Either Text Text)
runMcpEvaluationIO cfg vf args = do
  let timeoutMicros = cfg.evalTimeout * 1_000_000
      memLimitBytes = fromIntegral cfg.maxEvalMemoryMb * 1024 * 1024 :: Int64
  case Map.lookup JL4 vf.fnEvaluator of
    Nothing -> pure $ Left "No evaluator available"
    Just runFn -> do
      mResult <- (timeout timeoutMicros $ do
          setAllocationCounter memLimitBytes
          enableAllocationLimit
          r <- runExceptT (runFn.runFunction args Nothing TraceNone False)
          remaining <- getAllocationCounter
          let _ = memLimitBytes - remaining
          pure r
        ) `catch` \AllocationLimitExceeded ->
          pure Nothing
      case mResult of
        Nothing -> pure $ Left "Evaluation resource limit exceeded"
        Just (Left err) ->
          pure $ Left (prettyEvaluatorError err)
        Just (Right rwr) ->
          let encoded = Aeson.encode (SimpleResponse rwr)
          in pure $ Right (Text.Encoding.decodeUtf8 (LBS.toStrict encoded))

-- ----------------------------------------------------------------------------
-- Tasks extension (2026-07-28)
-- ----------------------------------------------------------------------------

-- | Default TTL for in-memory tasks before the sweeper reaps them.
defaultTaskTtlSeconds :: Int
defaultTaskTtlSeconds = 3600

-- | How often clients should poll @tasks/get@ for our compile-then-eval
-- workload. Compilation usually completes within a few seconds.
defaultTaskPollIntervalMs :: Int
defaultTaskPollIntervalMs = 1000

-- | Spawn a background task that waits for compilation to finish, runs
-- the evaluation, and stores the result. Returns the JSON-RPC response
-- carrying the task handle so the client can begin polling immediately.
spawnEvaluationTask
  :: Maybe Aeson.Value
  -> DeploymentId
  -> Text
  -> Bool
  -> Map Text Text
  -> [(Text, Maybe FnLiteral)]
  -> AppM Aeson.Value
spawnEvaluationTask reqId did fnName _isDeontic reverseMap argPairs = do
  env <- ask
  taskIdRaw <- liftIO UUID.nextRandom
  now <- liftIO getCurrentTime
  let taskId = TaskId (UUID.toText taskIdRaw)
  let cfg = env.options
  -- Spawn the worker first so we can store its Async handle in the task.
  workerAsync <- liftIO $ async $ runEvalWorker env cfg taskId did fnName reverseMap argPairs
  let initial = TaskState
        { tsStatus = TaskPending
        , tsResult = Nothing
        , tsError = Nothing
        , tsCreatedAt = now
        , tsTtlSeconds = defaultTaskTtlSeconds
        , tsAsync = Just workerAsync
        }
  liftIO $ atomically $ modifyTVar' env.mcpTasks (Map.insert taskId initial)
  liftIO $ logInfo env.logger "MCP task created"
    [ ("taskId", Aeson.toJSON taskId.unTaskId)
    , ("deploymentId", Aeson.toJSON did.unDeploymentId)
    , ("functionName", Aeson.toJSON fnName)
    ]
  pure $ jsonRpcResult reqId (taskHandleJson taskId TaskPending Nothing Nothing)

-- | Background worker: poll the deployment registry until compilation
-- finishes (or fails), then run the evaluation and store the outcome on
-- the task. Cancelled mid-flight via @tasks/cancel@.
runEvalWorker
  :: AppEnv
  -> Options
  -> TaskId
  -> DeploymentId
  -> Text
  -> Map Text Text
  -> [(Text, Maybe FnLiteral)]
  -> IO ()
runEvalWorker env cfg taskId did fnName reverseMap argPairs = do
  outcome <- catchSomeException $ waitForCompileAndEval
  finaliseTask env taskId outcome
 where
  waitForCompileAndEval = do
    -- Poll registry up to compileTimeout seconds.
    let deadlineMicros = cfg.compileTimeout * 1_000_000
    mFns <- pollForReady env did deadlineMicros
    case mFns of
      Left err -> pure (Left err)
      Right fns -> do
        atomically $ modifyTVar' env.mcpTasks $ Map.adjust
          (\t -> t { tsStatus = TaskWorking }) taskId
        case Map.lookup fnName fns of
          Nothing -> pure $ Left ("Function not found: " <> fnName)
          Just vf -> runMcpEvaluationIO cfg vf argPairs

  catchSomeException io = io `catch` \(e :: SomeException) ->
    pure $ Left ("Task crashed: " <> Text.pack (show e))

  finaliseTask appEnv tid result = do
    let payload = evalResultToToolPayload reverseMap result
    atomically $ modifyTVar' appEnv.mcpTasks $ Map.adjust
      (\t -> case t.tsStatus of
        TaskCancelled -> t  -- cancellation wins
        _ -> t { tsStatus = TaskCompleted
               , tsResult = Just payload
               , tsAsync = Nothing
               }
      ) tid
    logInfo appEnv.logger "MCP task completed"
      [("taskId", Aeson.toJSON tid.unTaskId)]

-- | Poll the deployment registry until a deployment becomes Ready, fails,
-- or the deadline is hit. Uses a fixed 250ms tick for simplicity; the
-- compile completes in the order of seconds so the overhead is negligible.
pollForReady
  :: AppEnv
  -> DeploymentId
  -> Int
  -> IO (Either Text (Map Text ValidatedFunction))
pollForReady env did deadlineMicros = go 0
 where
  tickMicros = 250_000 :: Int
  go elapsed
    | elapsed >= deadlineMicros =
        pure $ Left "Compilation timed out"
    | otherwise = do
        registry <- readTVarIO env.deploymentRegistry
        case Map.lookup did registry of
          Just (DeploymentReady fns _meta) -> pure $ Right fns
          Just (DeploymentFailed err) -> pure $ Left ("Deployment failed: " <> err)
          _ -> do
            threadDelay tickMicros
            go (elapsed + tickMicros)

-- | The standard JSON shape for a task handle returned from @tools/call@
-- and @tasks/get@.
taskHandleJson :: TaskId -> TaskStatus -> Maybe Aeson.Value -> Maybe Text -> Aeson.Value
taskHandleJson tid status mResult mErr = Aeson.object $
  [ "resultType" .= ("task" :: Text)
  , "taskId" .= tid.unTaskId
  , "status" .= status
  , "pollIntervalMilliseconds" .= defaultTaskPollIntervalMs
  , "ttlSeconds" .= defaultTaskTtlSeconds
  ]
  <> maybe [] (\r -> ["result" .= r]) mResult
  <> maybe [] (\e -> ["error" .= Aeson.object ["message" .= e]]) mErr

-- | Look up a task by id, performing TTL-based eviction if expired.
lookupTask :: TaskId -> AppM (Maybe TaskState)
lookupTask tid = do
  env <- ask
  now <- liftIO getCurrentTime
  liftIO $ atomically $ do
    m <- readTVar env.mcpTasks
    case Map.lookup tid m of
      Nothing -> pure Nothing
      Just t
        | taskExpired now t -> do
            writeTVar env.mcpTasks (Map.delete tid m)
            pure Nothing
        | otherwise -> pure (Just t)

taskExpired :: UTCTime -> TaskState -> Bool
taskExpired now t =
  let elapsed = realToFrac (diffUTCTime now t.tsCreatedAt) :: Double
  in elapsed > fromIntegral t.tsTtlSeconds

-- | Background-callable: walk the registry and drop expired entries.
-- Exposed so the Application boot-up can fork a sweeper.
reapExpiredTasks :: TVar (Map TaskId TaskState) -> IO ()
reapExpiredTasks tv = do
  now <- getCurrentTime
  atomically $ modifyTVar' tv $ Map.filter (not . taskExpired now)

-- | Parse a @taskId@ field out of a JSON-RPC params object.
parseTaskId :: Aeson.Value -> Either Text TaskId
parseTaskId = \case
  Aeson.Object obj -> case Aeson.KeyMap.lookup "taskId" obj of
    Just (Aeson.String s) -> Right (TaskId s)
    _ -> Left "Missing or invalid 'taskId'"
  _ -> Left "Invalid params"

handleTasksGet :: Maybe Aeson.Value -> Aeson.Value -> AppM Aeson.Value
handleTasksGet reqId params = case parseTaskId params of
  Left err -> pure $ jsonRpcError reqId (-32602) err
  Right tid -> do
    mTask <- lookupTask tid
    case mTask of
      Nothing -> pure $ jsonRpcError reqId (-32000) "Task not found"
      Just t  -> pure $ jsonRpcResult reqId $
        taskHandleJson tid t.tsStatus t.tsResult t.tsError

-- | We do not currently surface any input-required tasks, so update is
-- accepted but no-op. Clients can still call it without erroring.
handleTasksUpdate :: Maybe Aeson.Value -> Aeson.Value -> AppM Aeson.Value
handleTasksUpdate reqId params = case parseTaskId params of
  Left err -> pure $ jsonRpcError reqId (-32602) err
  Right tid -> do
    mTask <- lookupTask tid
    case mTask of
      Nothing -> pure $ jsonRpcError reqId (-32000) "Task not found"
      Just t  -> pure $ jsonRpcResult reqId $
        taskHandleJson tid t.tsStatus t.tsResult t.tsError

handleTasksCancel :: Maybe Aeson.Value -> Aeson.Value -> AppM Aeson.Value
handleTasksCancel reqId params = case parseTaskId params of
  Left err -> pure $ jsonRpcError reqId (-32602) err
  Right tid -> do
    env <- ask
    mTask <- lookupTask tid
    case mTask of
      Nothing -> pure $ jsonRpcError reqId (-32000) "Task not found"
      Just t -> do
        case t.tsAsync of
          Just a -> liftIO $ cancel a
          Nothing -> pure ()
        liftIO $ atomically $ modifyTVar' env.mcpTasks $ Map.adjust
          (\s -> s { tsStatus = TaskCancelled, tsAsync = Nothing }) tid
        let cancelled = t { tsStatus = TaskCancelled }
        pure $ jsonRpcResult reqId $
          taskHandleJson tid cancelled.tsStatus cancelled.tsResult cancelled.tsError

-- ----------------------------------------------------------------------------
-- File browsing tool handlers
-- ----------------------------------------------------------------------------

callListFiles :: Maybe Aeson.Value -> Aeson.Value -> AppM Aeson.Value
callListFiles reqId arguments = do
  env <- ask
  let mDeployment = getStringArg "deployment" arguments
  registry <- liftIO $ readTVarIO env.deploymentRegistry

  let deployIds = case mDeployment of
        Nothing -> [did.unDeploymentId | (did, _) <- Map.toList registry]
        Just d  -> [d]

  results <- liftIO $ mapM (\did -> do
    let deployId = DeploymentId did
    case Map.lookup deployId registry of
      Just (DeploymentReady _ meta) ->
        pure [(did, fe) | fe <- meta.metaFiles]
      _ -> do
        files <- BundleStore.listSourceFiles env.bundleStore did
        pure [(did, FileEntry (Text.pack f) []) | f <- files]
    ) deployIds

  let result = [ Aeson.object
                   [ "deployment" .= deployId
                   , "path" .= fe.fePath
                   , "exports" .= fe.feExports
                   ]
               | (deployId, fe) <- concat results
               ]

  pure $ jsonRpcResult reqId $ Aeson.object
    [ "content" .= [ Aeson.object
        [ "type" .= ("text" :: Text)
        , "text" .= Text.Encoding.decodeUtf8 (LBS.toStrict (Aeson.encode result))
        ]
      ]
    ]

callReadFile :: Maybe Aeson.Value -> Aeson.Value -> AppM Aeson.Value
callReadFile reqId arguments = do
  env <- ask
  let mDeployment = getStringArg "deployment" arguments
      mPath = getStringArg "path" arguments
      mLines = getStringArg "lines" arguments

  case (mDeployment, mPath) of
    (Just deployId, Just rawPath) -> do
      let path = normalizeReadFilePath deployId rawPath
      mContent <- liftIO $ BundleStore.loadSingleFile env.bundleStore deployId (Text.unpack path)
      case mContent of
        Nothing ->
          pure $ mcpTextResult reqId "File not found" True
        Just content -> do
          let result = case mLines of
                Nothing -> content
                Just lineSpec -> case parseLineRangeMcp lineSpec of
                  Nothing -> content
                  Just (s, e) ->
                    let allLines = Text.lines content
                        total = length allLines
                        start = max 1 (min s total)
                        end' = max start (min e total)
                    in Text.unlines $ take (end' - start + 1) (drop (start - 1) allLines)
          pure $ mcpTextResult reqId result False
    _ ->
      pure $ jsonRpcError reqId (-32602) "Missing required arguments: deployment, path"

normalizeReadFilePath :: Text -> Text -> Text
normalizeReadFilePath deployId path
  | Just rest <- Text.stripPrefix ("/deployments/" <> deployId <> "/files/") path = rest
  | Just rest <- Text.stripPrefix "/deployments/" path
  , (_, rest') <- Text.breakOn "/files/" rest
  , Just rest'' <- Text.stripPrefix "/files/" rest' = rest''
  | Just rest <- Text.stripPrefix "/" path = rest
  | otherwise = path

callSearchIdentifier :: Maybe Aeson.Value -> Aeson.Value -> AppM Aeson.Value
callSearchIdentifier reqId arguments = do
  env <- ask
  let mIdentifier = getStringArg "identifier" arguments
      mDeployment = getStringArg "deployment" arguments
      mFile = getStringArg "file" arguments

  case mIdentifier of
    Nothing ->
      pure $ jsonRpcError reqId (-32602) "Missing required argument: identifier"
    Just identifier -> do
      sources <- loadSourcesForSearch env mDeployment mFile
      let matches = searchIdentifier identifier sources
          result = Aeson.encode (map matchToJson matches)
      pure $ mcpTextResult reqId (Text.Encoding.decodeUtf8 (LBS.toStrict result)) False

callSearchText :: Maybe Aeson.Value -> Aeson.Value -> AppM Aeson.Value
callSearchText reqId arguments = do
  env <- ask
  let mQuery = getStringArg "query" arguments
      mDeployment = getStringArg "deployment" arguments
      mFile = getStringArg "file" arguments

  case mQuery of
    Nothing ->
      pure $ jsonRpcError reqId (-32602) "Missing required argument: query"
    Just query -> do
      sources <- loadSourcesForSearch env mDeployment mFile
      let matches = searchText query sources
          result = Aeson.encode (map matchToJson matches)
      pure $ mcpTextResult reqId (Text.Encoding.decodeUtf8 (LBS.toStrict result)) False

loadSourcesForSearch :: AppEnv -> Maybe Text -> Maybe Text -> AppM (Map FilePath Text)
loadSourcesForSearch env mDeployment mFile = do
  registry <- liftIO $ readTVarIO env.deploymentRegistry
  let deployIds = case mDeployment of
        Nothing -> [did.unDeploymentId | (did, _) <- Map.toList registry]
        Just d  -> [d]

  allSources <- liftIO $ fmap Map.unions $ mapM (\did -> do
    result <- (do
      (sourceMap, _) <- BundleStore.loadBundle env.bundleStore did
      let l4Only = Map.filterWithKey (\k _ -> takeExtensionText k == ".l4") sourceMap
      pure $ case mDeployment of
        Just _ -> l4Only
        Nothing -> Map.mapKeys (\k -> Text.unpack did ++ "/" ++ k) l4Only
      ) `catch` \(_ :: SomeException) -> pure Map.empty
    pure result
    ) deployIds

  case mFile of
    Nothing -> pure allSources
    Just f  -> pure $ Map.filterWithKey (\k _ -> Text.pack k == f) allSources
 where
  takeExtensionText fp = let parts = break (== '.') (reverse fp)
                         in case parts of
                              (ext, '.':_) -> '.' : reverse ext
                              _ -> ""

matchToJson :: SearchMatch -> Aeson.Value
matchToJson m = Aeson.object
  [ "file" .= m.smFile
  , "lineStart" .= m.smLineStart
  , "lineEnd" .= m.smLineEnd
  , "content" .= m.smContent
  , "isDefinition" .= m.smIsDefinition
  ]

mcpTextResult :: Maybe Aeson.Value -> Text -> Bool -> Aeson.Value
mcpTextResult reqId text isError = jsonRpcResult reqId $ Aeson.object $
  [ "content" .= [ Aeson.object
      [ "type" .= ("text" :: Text)
      , "text" .= text
      ]
    ]
  ] <> if isError then ["isError" .= True] else []

getStringArg :: Text -> Aeson.Value -> Maybe Text
getStringArg key (Aeson.Object obj) = case Aeson.KeyMap.lookup (Aeson.Key.fromText key) obj of
  Just (Aeson.String s) -> Just s
  _ -> Nothing
getStringArg _ _ = Nothing

parseLineRangeMcp :: Text -> Maybe (Int, Int)
parseLineRangeMcp spec = case Text.splitOn ":" spec of
  [sText, eText] -> case (readMaybeInt (Text.unpack sText), readMaybeInt (Text.unpack eText)) of
    (Just s, Just e) | s > 0 && e >= s -> Just (s, e)
    _ -> Nothing
  _ -> Nothing
 where
  readMaybeInt s = case reads s of
    [(n, "")] -> Just (n :: Int)
    _ -> Nothing

-- ----------------------------------------------------------------------------
-- JSON-RPC helpers
-- ----------------------------------------------------------------------------

parseFnLiteral :: Aeson.Value -> Maybe FnLiteral
parseFnLiteral v = case Aeson.fromJSON v of
  Aeson.Success lit -> Just lit
  Aeson.Error _ -> Nothing

jsonRpcResult :: Maybe Aeson.Value -> Aeson.Value -> Aeson.Value
jsonRpcResult reqId result = Aeson.object $
  [ "jsonrpc" .= ("2.0" :: Text)
  , "result" .= result
  ] <> maybe [] (\i -> ["id" .= i]) reqId

jsonRpcError :: Maybe Aeson.Value -> Int -> Text -> Aeson.Value
jsonRpcError reqId code msg = Aeson.object $
  [ "jsonrpc" .= ("2.0" :: Text)
  , "error" .= Aeson.object
      [ "code" .= code
      , "message" .= msg
      ]
  ] <> maybe [] (\i -> ["id" .= i]) reqId
