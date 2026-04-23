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
  sanitizePropertyNameRaw,
  sanitizePropertyName,
  sanitizePropertyNames,
  sanitizeParameters,
  buildPropertyReverseMap,
  remapFnLiteralKeys,
  remapArguments,
  -- * Error text sanitization
  sanitizeFieldNamesInText,
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
import qualified Data.Aeson.KeyMap as Aeson.KeyMap
import Data.Aeson ((.=), object)
import qualified Data.ByteString.Lazy as LBS
import Data.Char (isAlphaNum)
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Text (Text)
import qualified Data.Text as Text
import L4.FunctionSchema (Parameters (..), Parameter (..))
import Numeric (showHex)

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

-- | Raw property name sanitization: replaces special characters with hyphens
-- and collapses consecutive hyphens. Does NOT truncate.
-- Preserves alphanumeric, underscore, dot, and hyphen.
-- Used for structural collision detection (so "foo bar" vs "foo-bar" still collide).
sanitizePropertyNameRaw :: Text -> Text
sanitizePropertyNameRaw name =
  let s = Text.map (\c -> if isAlphaNum c || c == '_' || c == '.' || c == '-' then c else '-') name
      s' = collapseHyphens $ Text.dropWhile (== '-') $ Text.dropWhileEnd (== '-') s
  in if Text.null s' then "_unnamed" else s'
 where
  collapseHyphens t =
    let collapsed = Text.replace "--" "-" t
    in if collapsed == t then t else collapseHyphens collapsed

-- | Sanitize a property name and truncate to 60 chars (leaves room for a
-- 4-char dedup suffix to bring the total within Anthropic's 64-char limit).
-- Drops any trailing hyphen left by truncation.
sanitizePropertyName :: Text -> Text
sanitizePropertyName =
  Text.dropWhileEnd (== '-') . Text.take 60 . sanitizePropertyNameRaw

-- | Deterministic 3 hex-char hash suffix of a Text.
-- Used to disambiguate property names whose 60-char truncations collide.
-- Stable across runs so deployments are reproducible.
shortHashSuffix :: Text -> Text
shortHashSuffix t =
  let h = Text.foldl' (\acc c -> (acc * 131 + fromEnum c) `mod` 16777259) 1 t :: Int
      s = showHex (h `mod` 4096) ""
  in Text.pack (replicate (3 - length s) '0' ++ s)

-- | Map original property names → final unique sanitized names.
-- When two originals truncate to the same 60-char prefix, disambiguates each
-- with a hash suffix of the original (total ≤ 64 chars).
-- Structural collisions (e.g. "foo bar" and "foo-bar") still collide and
-- should be caught separately via 'validateNoSanitizationCollisions'.
sanitizePropertyNames :: [Text] -> Map Text Text
sanitizePropertyNames origs =
  let naive = [(o, sanitizePropertyName o) | o <- origs]
      groups :: Map Text [Text]
      groups = Map.fromListWith (flip (++)) [(s, [o]) | (o, s) <- naive]
  in Map.fromList $ concat
       [ case os of
           [o] -> [(o, s)]
           _   -> [(o, disambiguate s o) | o <- os]
       | (s, os) <- Map.toList groups
       ]
 where
  disambiguate s o =
    Text.dropWhileEnd (== '-') (Text.take 60 s) <> "-" <> shortHashSuffix o

-- | Sanitize all property names in a Parameters schema for MCP.
-- Applies dedup at each object level (hash-suffix disambiguation) and
-- recurses into nested objects and array items.
sanitizeParameters :: Parameters -> Aeson.Value
sanitizeParameters (MkParameters props reqProps) =
  let nameMap = sanitizePropertyNames (Map.keys props)
      finalOf o = Map.findWithDefault (sanitizePropertyName o) o nameMap
  in Aeson.object
    [ "type" .= ("object" :: Text)
    , "properties" .= Aeson.object
        [ (Aeson.Key.fromText (finalOf k), sanitizeParameterValue v)
        | (k, v) <- Map.toList props
        ]
    , "required" .= map finalOf reqProps
    ]

-- | Sanitize a Parameter value for MCP tool schemas.
-- Recursively rebuilds nested object properties with per-level dedup and
-- strips non-standard fields (alias, propertyOrder).
sanitizeParameterValue :: Parameter -> Aeson.Value
sanitizeParameterValue p =
  let base = Aeson.KeyMap.delete "alias"
           $ Aeson.KeyMap.delete "propertyOrder"
           $ Aeson.KeyMap.delete "properties"
           $ Aeson.KeyMap.delete "items"
           $ Aeson.KeyMap.delete "required"
           $ case Aeson.toJSON p of
               Aeson.Object o -> o
               _ -> Aeson.KeyMap.empty
      withProps = case p.parameterProperties of
        Just nested ->
          let nameMap = sanitizePropertyNames (Map.keys nested)
              finalOf o = Map.findWithDefault (sanitizePropertyName o) o nameMap
              propsVal = Aeson.object
                [ (Aeson.Key.fromText (finalOf k), sanitizeParameterValue v)
                | (k, v) <- Map.toList nested
                ]
              reqField = case p.parameterRequired of
                Just req -> Aeson.KeyMap.insert "required"
                              (Aeson.toJSON (map finalOf req))
                Nothing -> id
          in reqField $ Aeson.KeyMap.insert "properties" propsVal base
        Nothing | p.parameterType == "object" ->
          Aeson.KeyMap.insert "properties" (Aeson.object []) base
        Nothing -> base
      withItems = case p.parameterItems of
        Just items -> Aeson.KeyMap.insert "items" (sanitizeParameterValue items) withProps
        Nothing -> withProps
  in Aeson.Object withItems

-- | Build a reverse mapping from sanitized property names back to original L4 names.
-- Recursively includes entries from nested object properties and array items,
-- using the same dedup logic as 'sanitizeParameters'.
buildPropertyReverseMap :: Parameters -> Map Text Text
buildPropertyReverseMap (MkParameters props _) =
  buildNestedReverseMap props

-- | Recursively collect sanitized -> original name mappings from a property map.
buildNestedReverseMap :: Map Text Parameter -> Map Text Text
buildNestedReverseMap props =
  let nameMap = sanitizePropertyNames (Map.keys props)
      thisLevel = Map.fromList
        [ (san, orig)
        | (orig, san) <- Map.toList nameMap
        , san /= orig
        ]
  in thisLevel <> foldMap collectFromParameter (Map.elems props)
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
-- Error text sanitization
-- ----------------------------------------------------------------------------

-- | Replace original L4 field names with their sanitized equivalents in error text.
-- The reverseMap is sanitized->original; we invert it to find originals and replace
-- them with their sanitized form. Replaces longer names first to avoid partial matches.
sanitizeFieldNamesInText :: Map Text Text -> Text -> Text
sanitizeFieldNamesInText reverseMap text =
  let -- Invert: original -> sanitized (only where they differ)
      forwardPairs = [(original, sanitized)
                     | (sanitized, original) <- Map.toList reverseMap
                     , sanitized /= original]
      -- Sort by descending length to replace longer names first
      sorted = List.sortOn (negate . Text.length . fst) forwardPairs
  in foldl' (\t (original, sanitized) -> Text.replace original sanitized t) text sorted

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
      sanitizedPairs = [(k, sanitizePropertyNameRaw k) | k <- keys]
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
