-- | Shared utilities used across multiple service modules.
--
-- Consolidates duplicated logic from Application, McpServer, ControlPlane,
-- and DataPlane into a single location.
module Shared (
  -- * Scope filtering
  matchesScope,
  -- * Metadata collection
  collectMetadataEntries,
  collectDeploymentMetadata,
  -- * JSON error encoding
  jsonError,
  -- * Property name sanitization and remapping
  sanitizePropertyName,
  sanitizeParameters,
  buildPropertyReverseMap,
  remapFnLiteralKeys,
  remapArguments,
  -- * Collision detection
  validateNoSanitizationCollisions,
) where

import Backend.Api (FnLiteral (..))
import qualified BundleStore
import Types

import Control.Concurrent.STM (readTVarIO)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ask)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Aeson.Key
import Data.Aeson ((.=), object)
import qualified Data.ByteString.Lazy as LBS
import Data.Char (isAlphaNum)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Text (Text)
import qualified Data.Text as Text
import L4.FunctionSchema (Parameters (..), Parameter (..))

-- | Check if a deployment/function matches the scope filter.
--
-- The scope is a comma-separated list of patterns, each of the form
-- @deploymentId/functionName@. Either part may be @*@ to match all.
-- If scope is 'Nothing', everything matches.
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

-- | Collect metadata entries from all deployments, optionally filtered by scope.
-- Returns a list of (deploymentId, FunctionSummary) pairs.
--
-- For ready deployments, metadata comes from the in-memory registry.
-- For pending/compiling/failed deployments, it falls back to the disk cache.
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

-- | Collect full deployment metadata, optionally filtered by scope.
-- Returns (deploymentId, DeploymentMetadata) pairs with functions filtered by scope.
collectDeploymentMetadata :: Maybe Text -> AppM [(Text, DeploymentMetadata)]
collectDeploymentMetadata mScope = do
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
        let filteredFns = [ fn | fn <- meta.metaFunctions
                               , matchesScope mScope did.unDeploymentId fn.fsName ]
            -- Include deployment if it has matching functions OR if scope matches deployment wildcard
            depMatches = matchesScope mScope did.unDeploymentId "*"
        in if null filteredFns && not depMatches
           then []
           else [(did.unDeploymentId, meta { metaFunctions = filteredFns })]
    ) (Map.toList registry)

-- | Encode an error message as a JSON object: @{\"error\": \"...\"}@
jsonError :: Text -> LBS.ByteString
jsonError msg = Aeson.encode $ object ["error" .= msg]

-- ----------------------------------------------------------------------------
-- Property name sanitization and remapping
-- ----------------------------------------------------------------------------

-- | Sanitize a property name for use as a JSON property key.
-- Replaces special characters (spaces, backticks, etc.) with hyphens and
-- collapses consecutive hyphens. Preserves alphanumeric, underscore, dot, and hyphen.
sanitizePropertyName :: Text -> Text
sanitizePropertyName name =
  let s = Text.map (\c -> if isAlphaNum c || c == '_' || c == '.' || c == '-' then c else '-') name
      s' = collapseHyphens $ Text.dropWhile (== '-') $ Text.dropWhileEnd (== '-') s
  in if Text.null s' then "_unnamed" else s'
 where
  collapseHyphens t =
    let collapsed = Text.replace "--" "-" t
    in if collapsed == t then t else collapseHyphens collapsed

-- | Sanitize all property names in a Parameters schema.
-- Recursively sanitizes property names in nested object types.
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
-- Produces a JSON Schema draft 2020-12 compliant object by omitting
-- non-standard properties (alias, propertyOrder) and empty enum arrays.
sanitizeParameterValue :: Parameter -> Aeson.Value
sanitizeParameterValue p =
  Aeson.object $
    [ "type" .= p.parameterType
    , "description" .= p.parameterDescription
    ]
    ++ case p.parameterEnum of
        [] -> []
        enums -> ["enum" .= enums]
    ++ case p.parameterFormat of
        Nothing -> []
        Just fmt -> ["format" .= fmt]
    ++ case p.parameterProperties of
        Nothing
          | p.parameterType == "object" -> ["properties" .= Aeson.object []]
          | otherwise -> []
        Just nested -> ["properties" .= Aeson.object
          [ (Aeson.Key.fromText (sanitizePropertyName nk), sanitizeParameterValue nv)
          | (nk, nv) <- Map.toList nested
          ]]
    ++ case p.parameterItems of
        Nothing -> []
        Just items -> ["items" .= sanitizeParameterValue items]

-- | Build a reverse mapping from sanitized property names back to original L4 names.
-- Recursively includes entries from nested object properties and array items.
buildPropertyReverseMap :: Parameters -> Map Text Text
buildPropertyReverseMap (MkParameters props _) =
  buildNestedReverseMap props

-- | Recursively collect sanitized -> original name mappings from a property map.
buildNestedReverseMap :: Map Text Parameter -> Map Text Text
buildNestedReverseMap props =
  Map.fromList
    [ (sanitizePropertyName k, k)
    | k <- Map.keys props
    , sanitizePropertyName k /= k
    ]
  <> foldMap collectFromParameter (Map.elems props)
 where
  collectFromParameter :: Parameter -> Map Text Text
  collectFromParameter p =
    maybe mempty buildNestedReverseMap p.parameterProperties
    <> maybe mempty (maybe mempty buildNestedReverseMap . (.parameterProperties)) p.parameterItems

-- | Recursively remap keys in FnObject (and nested FnArray/FnObject) values
-- using the sanitized -> original reverse map.
remapFnLiteralKeys :: Map Text Text -> FnLiteral -> FnLiteral
remapFnLiteralKeys reverseMap = go
 where
  go (FnObject ps) = FnObject [(Map.findWithDefault k k reverseMap, go v) | (k, v) <- ps]
  go (FnArray vs) = FnArray (map go vs)
  go other = other

-- | Remap both top-level argument keys and nested FnLiteral object keys.
remapArguments :: Map Text Text -> [(Text, Maybe FnLiteral)] -> [(Text, Maybe FnLiteral)]
remapArguments reverseMap args =
  [ (Map.findWithDefault k k reverseMap, fmap (remapFnLiteralKeys reverseMap) v)
  | (k, v) <- args
  ]

-- ----------------------------------------------------------------------------
-- Collision detection
-- ----------------------------------------------------------------------------

-- | Validate that no two property names in a Parameters schema collide after
-- sanitization. Returns a list of collision descriptions (empty = no collisions).
-- Checks recursively into nested object properties and array items.
validateNoSanitizationCollisions :: Text -> Parameters -> [Text]
validateNoSanitizationCollisions context (MkParameters props _) =
  checkPropertyMap context props

-- | Check a property map for sanitization collisions, recursing into nested types.
checkPropertyMap :: Text -> Map Text Parameter -> [Text]
checkPropertyMap context props =
  let keys = Map.keys props
      sanitizedPairs = [(k, sanitizePropertyName k) | k <- keys]
      -- Group by sanitized name
      groups = Map.fromListWith (++) [(san, [orig]) | (orig, san) <- sanitizedPairs]
      -- Find groups with more than one original name
      collisions =
        [ "In " <> context <> ": field names " <> showNames origs
          <> " all sanitize to '" <> san <> "'"
        | (san, origs) <- Map.toList groups
        , length origs > 1
        ]
      -- Recurse into nested properties
      nestedCollisions = concatMap (checkParameter context) (Map.toList props)
  in collisions ++ nestedCollisions
 where
  showNames names = Text.intercalate ", " ["'" <> n <> "'" | n <- names]

-- | Check a single parameter's nested properties for collisions.
checkParameter :: Text -> (Text, Parameter) -> [Text]
checkParameter parentContext (fieldName, p) =
  let nestedContext = parentContext <> "." <> fieldName
  in maybe [] (checkPropertyMap nestedContext) p.parameterProperties
     ++ maybe [] (\items ->
          maybe [] (checkPropertyMap (nestedContext <> "[]")) items.parameterProperties
        ) p.parameterItems
