{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module McpServer (
  mcpHandler,
) where

import qualified BundleStore
import Logging (logInfo, logWarn)
import Types

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (asks, ask)
import Control.Monad.Trans.Except (runExceptT)
import Control.Concurrent.STM (readTVarIO)
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as Aeson.KeyMap
import Data.Char (isAlphaNum)
import Data.Int (Int64)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Maybe as Maybe
import GHC.Conc (setAllocationCounter, getAllocationCounter, enableAllocationLimit)
import GHC.IO.Exception (AllocationLimitExceeded (..))
import Control.Exception (catch)
import System.Timeout (timeout)

import Backend.Api (EvalBackend (..), FnLiteral, RunFunction (..), TraceLevel (..))
import Options (Options (..))

-- | Handle an MCP JSON-RPC 2.0 request and return a JSON-RPC 2.0 response.
-- The first argument is an optional deployment scope (Nothing = org-wide).
mcpHandler :: Maybe Text -> Aeson.Value -> AppM Aeson.Value
mcpHandler mScope reqVal = do
  case parseJsonRpcRequest reqVal of
    Left errMsg ->
      pure $ jsonRpcError Nothing (-32700) errMsg
    Right (reqId, method, params) -> do
      handleMethod mScope reqId method params

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

-- | Route to the appropriate handler based on the JSON-RPC method.
handleMethod :: Maybe Text -> Maybe Aeson.Value -> Text -> Aeson.Value -> AppM Aeson.Value
handleMethod _mScope reqId "initialize" _params =
  pure $ jsonRpcResult reqId $ Aeson.object
    [ "protocolVersion" .= ("2025-03-26" :: Text)
    , "serverInfo" .= Aeson.object
        [ "name" .= ("L4 Legal Rules" :: Text)
        , "version" .= ("1.0.0" :: Text)
        ]
    , "capabilities" .= Aeson.object
        [ "tools" .= Aeson.object [] ]
    ]

handleMethod _mScope reqId "notifications/initialized" _params =
  pure $ jsonRpcResult reqId (Aeson.object [])

handleMethod mScope reqId "tools/list" _params = do
  tools <- buildToolList mScope
  pure $ jsonRpcResult reqId $ Aeson.object
    [ "tools" .= tools ]

handleMethod mScope reqId "tools/call" params = do
  case params of
    Aeson.Object obj -> do
      let mToolName = case Aeson.KeyMap.lookup "name" obj of
            Just (Aeson.String n) -> Just n
            _ -> Nothing
          mArguments = case Aeson.KeyMap.lookup "arguments" obj of
            Just v -> Just v
            _ -> Nothing
      case mToolName of
        Nothing ->
          pure $ jsonRpcError reqId (-32602) "Missing 'name' in params"
        Just toolName -> do
          callTool mScope reqId toolName (Maybe.fromMaybe (Aeson.object []) mArguments)
    _ ->
      pure $ jsonRpcError reqId (-32602) "Invalid params for tools/call"

handleMethod _mScope reqId method _params = do
  pure $ jsonRpcError reqId (-32601) ("Method not found: " <> method)

-- | Build the list of tools from deployment metadata.
-- Reuses the same metadata collection logic as orgOpenApiHandler.
buildToolList :: Maybe Text -> AppM [Aeson.Value]
buildToolList mScope = do
  entries <- collectMetadataEntries mScope
  let toolNames = buildToolNames entries
  pure [ Aeson.object
    [ "name" .= tn
    , "description" .= (fn.fsDescription <> " [" <> deployId <> "/" <> fn.fsName <> "]")
    , "inputSchema" .= fn.fsParameters
    ]
    | (tn, deployId, fn) <- toolNames
    ]

-- | Collect metadata entries from all deployments, optionally filtered by scope.
-- Returns a list of (deploymentId, FunctionSummary) pairs.
collectMetadataEntries :: Maybe Text -> AppM [(Text, FunctionSummary)]
collectMetadataEntries mScope = do
  env <- ask
  registry <- liftIO . readTVarIO $ env.deploymentRegistry
  let store = env.bundleStore

  liftIO $ fmap concat $ mapM (\(did, state) -> do
    mMeta <- case state of
      DeploymentReady _ meta -> pure (Just meta)
      _ -> do
        mBytes <- BundleStore.loadMetadataCache store did.unDeploymentId
        case mBytes of
          Just bytes -> case Aeson.eitherDecode bytes of
            Right meta -> pure (Just meta)
            Left _ -> pure Nothing
          Nothing -> pure Nothing
    pure $ case mMeta of
      Nothing -> []
      Just meta ->
        [ (did.unDeploymentId, fn)
        | fn <- meta.metaFunctions
        , matchesScope mScope did.unDeploymentId fn.fsName
        ]
    ) (Map.toList registry)

-- | Check if a deployment/function matches the scope filter.
matchesScope :: Maybe Text -> Text -> Text -> Bool
matchesScope Nothing _ _ = True
matchesScope (Just scope) deployId fnName =
  any matchPattern (Text.splitOn "," scope)
 where
  matchPattern pat =
    let trimmed = Text.strip pat
        (depPat, rest) = Text.breakOn "/" trimmed
        fnPat = if Text.null rest then "*" else Text.drop 1 rest
    in (depPat == "*" || depPat == deployId)
       && (fnPat == "*" || fnPat == fnName)

-- | Sanitize a name for use as a tool name.
sanitizeName :: Int -> Text -> Text
sanitizeName maxLen name =
  let s = Text.map (\c -> if isAlphaNum c || c == '_' || c == '.' || c == '-' then c else '-') name
      s' = collapseHyphens $ Text.dropWhile (== '-') $ Text.dropWhileEnd (== '-') s
      s'' = if Text.null s' || not (isAlphaStart (Text.head s'))
            then "_" <> s' else s'
  in Text.take maxLen s''
 where
  isAlphaStart c = isAlphaNum c || c == '_'
  collapseHyphens t =
    let collapsed = Text.replace "--" "-" t
    in if collapsed == t then t else collapseHyphens collapsed

-- | Build unique tool names from metadata entries.
-- Uses the same 60-char logic as WebMCPPage.hs buildToolNames:
-- function-name only, with .deployment-prefix on collision.
buildToolNames :: [(Text, FunctionSummary)] -> [(Text, Text, FunctionSummary)]
buildToolNames entries =
  let maxLen = 60
      -- Group by sanitized function name
      groups :: Map Text [(Text, FunctionSummary)]
      groups = Map.fromListWith (++)
        [ (sanitizeName maxLen fn.fsName, [(deployId, fn)])
        | (deployId, fn) <- entries
        ]
  in concatMap (\(key, grp) ->
    case grp of
      [(deployId, fn)] -> [(key, deployId, fn)]
      _ ->
        -- Collision: add deployment prefix
        let deps = [(sanitizeName maxLen deployId, deployId, fn) | (deployId, fn) <- grp]
        in [ let suffix = "." <> Text.take prefixLen sanitizedDep
                 baseName = sanitizeName (maxLen - Text.length suffix) fn.fsName
                 prefixLen = findUniquePrefix sanitizedDep
                   [sd | (sd, _, _) <- deps, sd /= sanitizedDep]
             in (baseName <> suffix, deployId, fn)
           | (sanitizedDep, deployId, fn) <- deps
           ]
    ) (Map.toList groups)

-- | Find the shortest prefix length that distinguishes a string from all others.
findUniquePrefix :: Text -> [Text] -> Int
findUniquePrefix dep others = go 1
 where
  go n
    | n >= Text.length dep = Text.length dep
    | all (\o -> Text.take n o /= Text.take n dep) others = n
    | otherwise = go (n + 1)

-- | Execute a tool call by looking up the deployment and function, then evaluating.
callTool :: Maybe Text -> Maybe Aeson.Value -> Text -> Aeson.Value -> AppM Aeson.Value
callTool mScope reqId toolName arguments = do
  env <- ask
  -- Build the tool lookup map from current metadata
  entries <- collectMetadataEntries mScope
  let toolMap = Map.fromList
        [ (tn, (deployId, fn.fsName))
        | (tn, deployId, fn) <- buildToolNames entries
        ]

  case Map.lookup toolName toolMap of
    Nothing -> do
      liftIO $ logWarn env.logger "MCP tool not found"
        [("toolName", Aeson.toJSON toolName)]
      pure $ jsonRpcError reqId (-32602) ("Unknown tool: " <> toolName)
    Just (deployId, fnName) -> do
      liftIO $ logInfo env.logger "MCP tool called"
        [ ("toolName", Aeson.toJSON toolName)
        , ("deploymentId", Aeson.toJSON deployId)
        , ("functionName", Aeson.toJSON fnName)
        ]
      -- Parse arguments into FnLiteral values expected by the evaluator
      let argPairs = case arguments of
            Aeson.Object obj ->
              [ (k, parseFnLiteral v)
              | (k, v) <- Map.toList (Aeson.KeyMap.toMapText obj)
              ]
            _ -> []

      -- Look up the deployment and function
      let did = DeploymentId deployId
      registry <- liftIO $ readTVarIO env.deploymentRegistry
      case Map.lookup did registry of
        Just (DeploymentReady fns _meta) ->
          case Map.lookup fnName fns of
            Just vf -> do
              -- Run evaluation
              evalResult <- runMcpEvaluation vf argPairs
              case evalResult of
                Left errMsg ->
                  pure $ jsonRpcResult reqId $ Aeson.object
                    [ "content" .= [ Aeson.object
                        [ "type" .= ("text" :: Text)
                        , "text" .= errMsg
                        ]
                      ]
                    , "isError" .= True
                    ]
                Right resultVal ->
                  pure $ jsonRpcResult reqId $ Aeson.object
                    [ "content" .= [ Aeson.object
                        [ "type" .= ("text" :: Text)
                        , "text" .= resultVal
                        ]
                      ]
                    ]
            Nothing ->
              pure $ jsonRpcError reqId (-32602) ("Function not found: " <> fnName)
        _ ->
          pure $ jsonRpcError reqId (-32602) ("Deployment not ready: " <> deployId)

-- | Run evaluation for an MCP tool call.
-- Returns Left errorText or Right resultJsonText.
runMcpEvaluation :: ValidatedFunction -> [(Text, Maybe FnLiteral)] -> AppM (Either Text Text)
runMcpEvaluation vf args = do
  cfg <- asks (.options)
  let timeoutMicros = cfg.evalTimeout * 1_000_000
      memLimitBytes = fromIntegral cfg.maxEvalMemoryMb * 1024 * 1024 :: Int64
  case Map.lookup JL4 vf.fnEvaluator of
    Nothing -> pure $ Left "No evaluator available"
    Just runFn -> do
      mResult <- liftIO $
        (timeout timeoutMicros $ do
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
          pure $ Left (Text.pack (show err))
        Just (Right rwr) ->
          pure $ Right (Text.pack (show (Aeson.toJSON (SimpleResponse rwr))))

-- ----------------------------------------------------------------------------
-- JSON-RPC helpers
-- ----------------------------------------------------------------------------

-- | Parse a JSON value into a FnLiteral, returning Nothing if parsing fails.
parseFnLiteral :: Aeson.Value -> Maybe FnLiteral
parseFnLiteral v = case Aeson.fromJSON v of
  Aeson.Success lit -> Just lit
  Aeson.Error _ -> Nothing

-- | Build a JSON-RPC 2.0 success response.
jsonRpcResult :: Maybe Aeson.Value -> Aeson.Value -> Aeson.Value
jsonRpcResult reqId result = Aeson.object $
  [ "jsonrpc" .= ("2.0" :: Text)
  , "result" .= result
  ] <> maybe [] (\i -> ["id" .= i]) reqId

-- | Build a JSON-RPC 2.0 error response.
jsonRpcError :: Maybe Aeson.Value -> Int -> Text -> Aeson.Value
jsonRpcError reqId code msg = Aeson.object $
  [ "jsonrpc" .= ("2.0" :: Text)
  , "error" .= Aeson.object
      [ "code" .= code
      , "message" .= msg
      ]
  ] <> maybe [] (\i -> ["id" .= i]) reqId
