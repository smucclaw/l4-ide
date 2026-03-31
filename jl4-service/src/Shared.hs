-- | Shared utilities used across multiple service modules.
--
-- Consolidates duplicated logic from Application, McpServer, ControlPlane,
-- and DataPlane into a single location.
module Shared (
  -- * Scope filtering
  matchesScope,
  -- * Metadata collection
  collectMetadataEntries,
  -- * JSON error encoding
  jsonError,
) where

import qualified BundleStore
import Types

import Control.Concurrent.STM (readTVarIO)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ask)
import qualified Data.Aeson as Aeson
import Data.Aeson ((.=), object)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text

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

-- | Encode an error message as a JSON object: @{\"error\": \"...\"}@
jsonError :: Text -> LBS.ByteString
jsonError msg = Aeson.encode $ object ["error" .= msg]
