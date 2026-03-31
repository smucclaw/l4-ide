{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module McpServer (
  mcpHandler,
) where

import DeploymentLoader (triggerCompilationIfPending)
import Logging (logInfo, logWarn)
import Shared (collectMetadataEntries)
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
import qualified Data.Aeson.Key as Aeson.Key
import L4.FunctionSchema (Parameters (..), Parameter (..))
import qualified Data.Text.Encoding as Text.Encoding
import qualified Data.ByteString.Lazy as LBS
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
    , "inputSchema" .= sanitizeParameters fn.fsParameters
    ]
    | (tn, deployId, fn) <- toolNames
    ]

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
        [ (tn, (deployId, fn.fsName, fn.fsParameters))
        | (tn, deployId, fn) <- buildToolNames entries
        ]

  case Map.lookup toolName toolMap of
    Nothing -> do
      liftIO $ logWarn env.logger "MCP tool not found"
        [("toolName", Aeson.toJSON toolName)]
      pure $ jsonRpcError reqId (-32602) ("Unknown tool: " <> toolName)
    Just (deployId, fnName, params) -> do
      liftIO $ logInfo env.logger "MCP tool called"
        [ ("toolName", Aeson.toJSON toolName)
        , ("deploymentId", Aeson.toJSON deployId)
        , ("functionName", Aeson.toJSON fnName)
        ]
      -- Build reverse mapping: sanitized property key -> original L4 name
      let reverseMap = buildPropertyReverseMap params
      -- Parse arguments and remap sanitized keys back to original L4 names
      let argPairs = case arguments of
            Aeson.Object obj ->
              [ (Map.findWithDefault k k reverseMap, parseFnLiteral v)
              | (k, v) <- Map.toList (Aeson.KeyMap.toMapText obj)
              ]
            _ -> []

      -- Look up the deployment and function, triggering lazy compilation if needed
      let did = DeploymentId deployId
      triggerCompilationIfPending did
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
        Just DeploymentCompiling ->
          pure $ jsonRpcError reqId (-32002) ("Deployment is still compiling: " <> deployId)
        Just (DeploymentFailed err) ->
          pure $ jsonRpcError reqId (-32002) ("Deployment failed to compile: " <> err)
        _ ->
          pure $ jsonRpcError reqId (-32602) ("Deployment not found: " <> deployId)

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
          let encoded = Aeson.encode (SimpleResponse rwr)
          in pure $ Right (Text.Encoding.decodeUtf8 (LBS.toStrict encoded))

-- ----------------------------------------------------------------------------
-- Parameter name sanitization
-- ----------------------------------------------------------------------------

-- | Sanitize a property name for use as a JSON property key in MCP tool schemas.
-- Replaces special characters (spaces, backticks, etc.) with hyphens and
-- collapses consecutive hyphens. Preserves alphanumeric, underscore, dot, and hyphen.
-- Uses the same replacement character as 'sanitizeName' for consistency.
sanitizePropertyName :: Text -> Text
sanitizePropertyName name =
  let s = Text.map (\c -> if isAlphaNum c || c == '_' || c == '.' || c == '-' then c else '-') name
      s' = collapseHyphens $ Text.dropWhile (== '-') $ Text.dropWhileEnd (== '-') s
  in if Text.null s' then "_unnamed" else s'
 where
  collapseHyphens t =
    let collapsed = Text.replace "--" "-" t
    in if collapsed == t then t else collapseHyphens collapsed

-- | Sanitize all property names in a Parameters schema for MCP compatibility.
-- MCP tool schemas require property names to be simple identifiers.
-- This recursively sanitizes property names in nested object types.
sanitizeParameters :: Parameters -> Aeson.Value
sanitizeParameters (MkParameters props reqProps) =
  Aeson.object
    [ "type" .= ("object" :: Text)
    , "properties" .= Aeson.object
        [ (Aeson.Key.fromText (sanitizePropertyName k), sanitizeParameterValue v)
        | (k, v) <- Map.toList props
        ]
    , "required" .= map sanitizePropertyName reqProps
    ]

-- | Sanitize a Parameter value, recursively sanitizing nested properties.
sanitizeParameterValue :: Parameter -> Aeson.Value
sanitizeParameterValue p =
  Aeson.object $
    [ "type" .= p.parameterType
    , "alias" .= p.parameterAlias
    , "enum" .= p.parameterEnum
    , "description" .= p.parameterDescription
    ]
    ++ case p.parameterFormat of
        Nothing -> []
        Just fmt -> ["format" .= fmt]
    ++ case p.parameterProperties of
        Nothing -> []
        Just nested -> ["properties" .= Aeson.object
          [ (Aeson.Key.fromText (sanitizePropertyName nk), sanitizeParameterValue nv)
          | (nk, nv) <- Map.toList nested
          ]]
    ++ case p.parameterPropertyOrder of
        Nothing -> []
        Just ord -> ["propertyOrder" .= map sanitizePropertyName ord]
    ++ case p.parameterItems of
        Nothing -> []
        Just items -> ["items" .= sanitizeParameterValue items]

-- | Build a reverse mapping from sanitized property names back to original L4 names.
-- Only includes entries where the sanitized name differs from the original.
buildPropertyReverseMap :: Parameters -> Map Text Text
buildPropertyReverseMap (MkParameters props _) =
  Map.fromList
    [ (sanitizePropertyName k, k)
    | k <- Map.keys props
    , sanitizePropertyName k /= k
    ]

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
