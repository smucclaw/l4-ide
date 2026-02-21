module BundleStore (
  BundleStore (..),
  StoredMetadata (..),
  StoredFunctionSummary (..),
  SerializedBundle (..),
  initStore,
  saveBundle,
  loadBundle,
  listDeployments,
  deleteBundle,
  saveBundleCbor,
  loadBundleCbor,
) where

import Codec.Serialise (Serialise, deserialiseOrFail, serialise)
import Data.Aeson (FromJSON, ToJSON, eitherDecodeFileStrict', encodeFile)
import qualified Data.ByteString.Lazy as LBS
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import GHC.Generics (Generic)
import L4.Syntax (Module, Resolved)
import L4.TypeCheck.Types (Environment, EntityInfo)
import System.Directory
  ( createDirectoryIfMissing
  , doesDirectoryExist
  , doesFileExist
  , listDirectory
  , removeDirectoryRecursive
  , removeFile
  , renameDirectory
  , renameFile
  )
import System.FilePath ((</>), takeExtension, makeRelative)

-- | Handle to the on-disk bundle store.
newtype BundleStore = BundleStore { storePath :: FilePath }
  deriving stock (Show)

-- | Metadata that is persisted alongside the sources.
-- This is the on-disk representation; it matches 'Types.DeploymentMetadata'
-- but is kept here to avoid circular imports.
data StoredMetadata = StoredMetadata
  { smFunctions :: ![StoredFunctionSummary]
  , smVersion   :: !Text
  , smCreatedAt :: !Text  -- UTCTime serialized as ISO-8601 string
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data StoredFunctionSummary = StoredFunctionSummary
  { sfName        :: !Text
  , sfDescription :: !Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Cached typechecked state for a deployment.
-- Stores the expensive-to-recompute outputs of typechecking so restarts
-- can skip parsing and typechecking entirely.
data SerializedBundle = SerializedBundle
  { sbModule      :: !(Module Resolved)
  , sbEnvironment :: !Environment     -- ^ Map RawName [Unique]
  , sbEntityInfo  :: !EntityInfo      -- ^ Map Unique (Name, CheckEntity)
  }
  deriving stock (Generic)
  deriving anyclass (Serialise)

-- | Initialize the bundle store, creating the root directory if absent.
initStore :: FilePath -> IO BundleStore
initStore path = do
  createDirectoryIfMissing True path
  pure (BundleStore path)

-- | Atomically save bundle sources and metadata.
-- Uses a temp directory + rename for crash safety.
saveBundle
  :: BundleStore
  -> Text           -- ^ deployment id
  -> Map FilePath Text  -- ^ .l4 source files (relative paths as keys)
  -> StoredMetadata
  -> IO ()
saveBundle (BundleStore root) deployId sources meta = do
  let did = Text.unpack deployId
      targetDir = root </> did
      tmpDir = root </> (did <> ".tmp")

  -- Write to a temp directory first
  createDirectoryIfMissing True (tmpDir </> "sources")

  -- Write each source file
  mapM_ (\(relPath, content) -> do
    let fullPath = tmpDir </> "sources" </> relPath
    createDirectoryIfMissing True (takeDirectory fullPath)
    Text.IO.writeFile fullPath content
    ) (Map.toList sources)

  -- Write metadata
  encodeFile (tmpDir </> "metadata.json") meta

  -- Atomic swap: remove old directory if it exists, then rename
  exists <- doesDirectoryExist targetDir
  if exists
    then do
      removeDirectoryRecursive targetDir
      renameDirectory tmpDir targetDir
    else
      renameDirectory tmpDir targetDir

-- | Load sources and metadata from a stored deployment.
loadBundle
  :: BundleStore
  -> Text          -- ^ deployment id
  -> IO (Map FilePath Text, StoredMetadata)
loadBundle (BundleStore root) deployId = do
  let did = Text.unpack deployId
      deployDir = root </> did
      sourcesDir = deployDir </> "sources"
      metaFile = deployDir </> "metadata.json"

  -- Load all .l4 files recursively
  sources <- loadSourcesRecursive sourcesDir sourcesDir

  -- Load metadata
  metaResult <- eitherDecodeFileStrict' metaFile
  meta <- case metaResult of
    Left err -> fail $ "Failed to decode metadata for " <> did <> ": " <> err
    Right m -> pure m

  pure (sources, meta)

-- | Save the CBOR-serialized compiled state for a deployment.
-- Uses a temp file + rename for crash safety.
saveBundleCbor :: BundleStore -> Text -> SerializedBundle -> IO ()
saveBundleCbor (BundleStore root) deployId bundle = do
  let did = Text.unpack deployId
      cborFile = root </> did </> "bundle.cbor"
      tmpFile = cborFile <> ".tmp"
  LBS.writeFile tmpFile (serialise bundle)
  -- Atomic swap: remove old file if it exists, then rename
  exists <- doesFileExist cborFile
  if exists
    then do
      removeFile cborFile
      renameFile tmpFile cborFile
    else
      renameFile tmpFile cborFile

-- | Try to load the CBOR-serialized compiled state for a deployment.
-- Returns 'Nothing' if the file is absent or fails to parse
-- (safe fallback to source recompilation).
loadBundleCbor :: BundleStore -> Text -> IO (Maybe SerializedBundle)
loadBundleCbor (BundleStore root) deployId = do
  let cborFile = root </> Text.unpack deployId </> "bundle.cbor"
  exists <- doesFileExist cborFile
  if not exists
    then pure Nothing
    else do
      bytes <- LBS.readFile cborFile
      case deserialiseOrFail bytes of
        Left _err -> do
          putStrLn $ "  Warning: corrupt bundle.cbor for " <> Text.unpack deployId <> ", will recompile"
          pure Nothing
        Right bundle -> pure (Just bundle)

-- | Recursively load all .l4 files from a directory.
loadSourcesRecursive :: FilePath -> FilePath -> IO (Map FilePath Text)
loadSourcesRecursive baseDir dir = do
  exists <- doesDirectoryExist dir
  if not exists
    then pure Map.empty
    else do
      entries <- listDirectory dir
      maps <- mapM (\entry -> do
        let fullPath = dir </> entry
        isDir <- doesDirectoryExist fullPath
        if isDir
          then loadSourcesRecursive baseDir fullPath
          else if takeExtension entry == ".l4"
            then do
              content <- Text.IO.readFile fullPath
              let relPath = makeRelative baseDir fullPath
              pure (Map.singleton relPath content)
            else pure Map.empty
        ) entries
      pure (Map.unions maps)

-- | List all deployment IDs in the store.
listDeployments :: BundleStore -> IO [Text]
listDeployments (BundleStore root) = do
  exists <- doesDirectoryExist root
  if not exists
    then pure []
    else do
      entries <- listDirectory root
      -- Filter to directories that contain metadata.json
      validEntries <- filterM (\entry -> do
        let metaFile = root </> entry </> "metadata.json"
        doesFileExist metaFile
        ) entries
      pure (map Text.pack validEntries)

-- | Delete a deployment's directory from the store.
deleteBundle :: BundleStore -> Text -> IO ()
deleteBundle (BundleStore root) deployId = do
  let deployDir = root </> Text.unpack deployId
  exists <- doesDirectoryExist deployDir
  if exists
    then removeDirectoryRecursive deployDir
    else pure ()

-- | Get the directory component of a file path.
takeDirectory :: FilePath -> FilePath
takeDirectory path =
  let parts = break (== '/') (reverse path)
  in case parts of
    (_, '/':rest) -> reverse rest
    _ -> "."

-- | Filter a list using a monadic predicate.
filterM :: Monad m => (a -> m Bool) -> [a] -> m [a]
filterM _ [] = pure []
filterM p (x:xs) = do
  b <- p x
  rest <- filterM p xs
  pure (if b then x : rest else rest)
