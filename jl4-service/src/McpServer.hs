{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module McpServer (
  mcpHandler,
) where

import qualified BundleStore
import DeploymentLoader (triggerCompilationIfPending)
import FileBrowser (searchIdentifier, searchText, SearchMatch (..))
import Logging (logInfo, logWarn)
import Shared (collectMetadataEntries, sanitizeParameters, buildPropertyReverseMap, remapFnLiteralKeys)
import Types

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (asks, ask)
import Control.Monad.Trans.Except (runExceptT)
import Control.Concurrent.STM (readTVarIO)
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
import GHC.Conc (setAllocationCounter, getAllocationCounter, enableAllocationLimit)
import GHC.IO.Exception (AllocationLimitExceeded (..))
import Control.Exception (catch)
import System.Timeout (timeout)

import Backend.Api (EvalBackend (..), FnLiteral (..), RunFunction (..), TraceLevel (..))
import Options (Options (..))

-- | Handle an MCP JSON-RPC 2.0 request and return a JSON-RPC 2.0 response.
-- The first argument is an optional deployment scope (Nothing = org-wide).
-- Visibility controls which tools are advertised and callable.
mcpHandler :: Visibility -> Maybe Text -> Aeson.Value -> AppM Aeson.Value
mcpHandler vis mScope reqVal = do
  case parseJsonRpcRequest reqVal of
    Left errMsg ->
      pure $ jsonRpcError Nothing (-32700) errMsg
    Right (reqId, method, params) -> do
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

-- | Route to the appropriate handler based on the JSON-RPC method.
handleMethod :: Visibility -> Maybe Text -> Maybe Aeson.Value -> Text -> Aeson.Value -> AppM Aeson.Value
handleMethod _vis _mScope reqId "initialize" _params =
  pure $ jsonRpcResult reqId $ Aeson.object
    [ "protocolVersion" .= ("2025-03-26" :: Text)
    , "serverInfo" .= Aeson.object
        [ "name" .= ("L4 Tools" :: Text)
        , "version" .= ("1.0.0" :: Text)
        ]
    , "capabilities" .= Aeson.object
        [ "tools" .= Aeson.object [] ]
    ]

handleMethod _vis _mScope reqId "notifications/initialized" _params =
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
      case mToolName of
        Nothing ->
          pure $ jsonRpcError reqId (-32602) "Missing 'name' in params"
        Just toolName -> do
          callTool vis mScope reqId toolName (Maybe.fromMaybe (Aeson.object []) mArguments)
    _ ->
      pure $ jsonRpcError reqId (-32602) "Invalid params for tools/call"

handleMethod _vis _mScope reqId method _params = do
  pure $ jsonRpcError reqId (-32601) ("Method not found: " <> method)

-- | Build the list of tools from deployment metadata.
-- Visibility controls which tools are included:
--   showFunctions → include per-function evaluation tools
--   showFiles     → include file browsing tools
-- The proxy gates tools/call by permission (l4:evaluate or l4:read).
buildToolList :: Visibility -> Maybe Text -> AppM [Aeson.Value]
buildToolList vis mScope = do
  -- Function evaluation tools (caller has l4:rules to see them; proxy gates tools/call on l4:evaluate)
  fnTools <- if vis.showFunctions
    then do
      entries <- collectMetadataEntries mScope
      let toolNames = buildToolNames entries
      pure [ Aeson.object
        [ "name" .= tn
        , "description" .= (fn.fsDescription <> " [" <> deployId <> "/" <> fn.fsName <> "]")
        , "inputSchema" .= sanitizeParameters fn.fsParameters
        ]
        | (tn, deployId, fn) <- toolNames
        ]
    else pure []

  -- File browsing tools (when showFiles is True)
  let fileTools = if vis.showFiles then fileToolDefinitions else []

  pure (fnTools ++ fileTools)

-- | Static tool definitions for file browsing.
fileToolDefinitions :: [Aeson.Value]
fileToolDefinitions =
  [ Aeson.object
    [ "name" .= ("list_files" :: Text)
    , "description" .= ("List .l4 files and their exports in a deployment" :: Text)
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
    , "description" .= ("Read .l4 source file content, optionally a line range" :: Text)
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
    , "description" .= ("Find definitions (DECLARE/DECIDE/MEANS/ASSUME blocks with decorators) and references of an L4 identifier. Text-based, not AST — may match in comments." :: Text)
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
    , "description" .= ("Grep .l4 files (case-insensitive substring)" :: Text)
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
callTool :: Visibility -> Maybe Text -> Maybe Aeson.Value -> Text -> Aeson.Value -> AppM Aeson.Value
callTool vis mScope reqId toolName arguments = do
  -- Check file browsing tools first
  case toolName of
    "list_files" | vis.showFiles -> callListFiles reqId arguments
    "read_file" | vis.showFiles -> callReadFile reqId arguments
    "search_identifier" | vis.showFiles -> callSearchIdentifier reqId arguments
    "search_text" | vis.showFiles -> callSearchText reqId arguments
    _ -> callFunctionTool vis mScope reqId toolName arguments

-- | Execute a function evaluation tool call.
callFunctionTool :: Visibility -> Maybe Text -> Maybe Aeson.Value -> Text -> Aeson.Value -> AppM Aeson.Value
callFunctionTool _vis mScope reqId toolName arguments = do
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
      -- This map includes all nested property names recursively.
      let reverseMap = buildPropertyReverseMap params
      -- Parse arguments and remap sanitized keys back to original L4 names.
      -- Remapping is applied recursively to nested FnObject keys.
      let argPairs = case arguments of
            Aeson.Object obj ->
              [ (Map.findWithDefault k k reverseMap, remapFnLiteralKeys reverseMap <$> parseFnLiteral v)
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
-- File browsing tool handlers
-- ----------------------------------------------------------------------------

-- | list_files: list all .l4 source files across deployments.
callListFiles :: Maybe Aeson.Value -> Aeson.Value -> AppM Aeson.Value
callListFiles reqId arguments = do
  env <- ask
  let mDeployment = getStringArg "deployment" arguments
  registry <- liftIO $ readTVarIO env.deploymentRegistry

  let entries = [ (did.unDeploymentId, meta.metaFiles)
                | (did, DeploymentReady _ meta) <- Map.toList registry
                , case mDeployment of
                    Nothing -> True
                    Just d  -> did.unDeploymentId == d
                ]
      result = [ Aeson.object
                   [ "deployment" .= deployId
                   , "path" .= fe.fePath
                   , "exports" .= fe.feExports
                   ]
               | (deployId, files) <- entries
               , fe <- files
               ]

  pure $ jsonRpcResult reqId $ Aeson.object
    [ "content" .= [ Aeson.object
        [ "type" .= ("text" :: Text)
        , "text" .= Text.Encoding.decodeUtf8 (LBS.toStrict (Aeson.encode result))
        ]
      ]
    ]

-- | read_file: read an L4 source file (whole or line range).
callReadFile :: Maybe Aeson.Value -> Aeson.Value -> AppM Aeson.Value
callReadFile reqId arguments = do
  env <- ask
  let mDeployment = getStringArg "deployment" arguments
      mPath = getStringArg "path" arguments
      mLines = getStringArg "lines" arguments

  case (mDeployment, mPath) of
    (Just deployId, Just path) -> do
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

-- | search_identifier: search for L4 identifier definitions and references.
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

-- | search_text: grep-like text search across files.
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

-- | Load source files for search, filtered by deployment and file path.
loadSourcesForSearch :: AppEnv -> Maybe Text -> Maybe Text -> AppM (Map FilePath Text)
loadSourcesForSearch env mDeployment mFile = do
  registry <- liftIO $ readTVarIO env.deploymentRegistry
  let deployIds = case mDeployment of
        Nothing -> [did.unDeploymentId | (did, DeploymentReady _ _) <- Map.toList registry]
        Just d  -> [d]

  allSources <- liftIO $ fmap Map.unions $ mapM (\did -> do
    (sourceMap, _) <- BundleStore.loadBundle env.bundleStore did
    let l4Only = Map.filterWithKey (\k _ -> takeExtensionText k == ".l4") sourceMap
    -- Prefix file paths with deployment ID for cross-deployment uniqueness
    pure $ case mDeployment of
      Just _ -> l4Only  -- Single deployment: keep relative paths
      Nothing -> Map.mapKeys (\k -> Text.unpack did ++ "/" ++ k) l4Only
    ) deployIds

  case mFile of
    Nothing -> pure allSources
    Just f  -> pure $ Map.filterWithKey (\k _ -> Text.pack k == f) allSources
 where
  takeExtensionText fp = let parts = break (== '.') (reverse fp)
                         in case parts of
                              (ext, '.':_) -> '.' : reverse ext
                              _ -> ""

-- | Convert a SearchMatch to JSON value.
matchToJson :: SearchMatch -> Aeson.Value
matchToJson m = Aeson.object
  [ "file" .= m.smFile
  , "lineStart" .= m.smLineStart
  , "lineEnd" .= m.smLineEnd
  , "content" .= m.smContent
  , "isDefinition" .= m.smIsDefinition
  ]

-- | Build a text content result for MCP.
mcpTextResult :: Maybe Aeson.Value -> Text -> Bool -> Aeson.Value
mcpTextResult reqId text isError = jsonRpcResult reqId $ Aeson.object $
  [ "content" .= [ Aeson.object
      [ "type" .= ("text" :: Text)
      , "text" .= text
      ]
    ]
  ] <> if isError then ["isError" .= True] else []

-- | Extract a string argument from a JSON object.
getStringArg :: Text -> Aeson.Value -> Maybe Text
getStringArg key (Aeson.Object obj) = case Aeson.KeyMap.lookup (Aeson.Key.fromText key) obj of
  Just (Aeson.String s) -> Just s
  _ -> Nothing
getStringArg _ _ = Nothing

-- | Parse a line range like "85:89" into (start, end).
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
