module BundleStore (
  BundleStore (..),
  StoredMetadata (..),
  SerializedBundle (..),
  initStore,
  cleanupStore,
  saveBundle,
  loadBundle,
  listDeployments,
  deleteBundle,
  saveBundleCbor,
  loadBundleCbor,
  deleteBundleCbor,
  saveMetadataCache,
  loadMetadataCache,
  loadSingleFile,
  listSourceFiles,
) where

import Codec.Serialise (Serialise, deserialiseOrFail, serialise)
import Control.Exception (IOException, catch)
import Data.Aeson (FromJSON, ToJSON, eitherDecodeFileStrict', encodeFile, toJSON)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import GHC.Generics (Generic)
import L4.Export (ExportedFunction)
import L4.Syntax (Module, Resolved, Declare)
import L4.TypeCheck.Types (Environment, EntityInfo)
import Data.List (isSuffixOf)
import Logging (Logger, logInfo, logWarn)
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
import System.FilePath ((</>), takeExtension, makeRelative, splitDirectories)

-- | Handle to the on-disk bundle store.
newtype BundleStore = BundleStore { storePath :: FilePath }
  deriving stock (Show)

-- | Metadata that is persisted alongside the sources.
-- This is the on-disk representation; it matches 'Types.DeploymentMetadata'
-- but is kept here to avoid circular imports.
data StoredMetadata = StoredMetadata
  { smVersion   :: !Text
  , smCreatedAt :: !Text  -- UTCTime serialized as ISO-8601 string
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
  , sbExports     :: ![ExportedFunction]  -- ^ Exported functions (annotations are stripped in CBOR, so these are stored explicitly)
  , sbDeclares    :: !(Map Text (Declare Resolved))  -- ^ All type declares (transitive imports included) for schema resolution
  , sbFilePath    :: !FilePath        -- ^ Source file this bundle was compiled from
  }
  deriving stock (Generic)
  deriving anyclass (Serialise)

-- | Initialize the bundle store, creating the root directory if absent.
initStore :: FilePath -> IO BundleStore
initStore path = do
  createDirectoryIfMissing True path
  pure (BundleStore path)

-- | Clean up stale .tmp directories left behind by interrupted deploys.
-- NFS .nfs* files are not cleaned here — they disappear automatically
-- once the process holding the file handle restarts.
cleanupStore :: Logger -> BundleStore -> IO ()
cleanupStore logger (BundleStore root) = do
  exists <- doesDirectoryExist root
  if not exists then pure ()
  else do
    entries <- listDirectory root
    let tmpDirs = filter (".tmp" `isSuffixOf`) entries
    mapM_ (\d -> do
      let path = root </> d
      removeDirectoryRecursive path
        `catch` \(e :: IOException) ->
          logWarn logger "Failed to clean up tmp directory"
            [("path", toJSON d), ("error", toJSON (show e))]
      ) tmpDirs
    if not (null tmpDirs)
      then logInfo logger "Cleaned up stale tmp directories"
             [("count", toJSON (length tmpDirs))]
      else pure ()

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

-- | Save all CBOR-serialized compiled bundles for a deployment.
-- Writes the full list atomically using a temp file + rename for crash safety.
saveBundleCbor :: BundleStore -> Text -> [SerializedBundle] -> IO ()
saveBundleCbor (BundleStore root) deployId bundles = do
  let did = Text.unpack deployId
      cborFile = root </> did </> "bundle.cbor"
      tmpFile = cborFile <> ".tmp"
  LBS.writeFile tmpFile (serialise bundles)
  -- Atomic swap: remove old file if it exists, then rename
  exists <- doesFileExist cborFile
  if exists
    then do
      removeFile cborFile
      renameFile tmpFile cborFile
    else
      renameFile tmpFile cborFile

-- | Try to load CBOR-serialized compiled bundles for a deployment.
-- Returns 'Nothing' if the file is absent or fails to parse
-- (safe fallback to source recompilation).
loadBundleCbor :: Logger -> BundleStore -> Text -> IO (Maybe [SerializedBundle])
loadBundleCbor logger (BundleStore root) deployId = do
  let cborFile = root </> Text.unpack deployId </> "bundle.cbor"
  exists <- doesFileExist cborFile
  if not exists
    then pure Nothing
    else do
      -- Read strictly to ensure the file handle is closed immediately.
      -- Lazy readFile leaks the fd when the deserialized data is kept alive.
      bytes <- LBS.fromStrict <$> BS.readFile cborFile
      case deserialiseOrFail bytes of
        Left _err -> do
          logWarn logger "Corrupt bundle.cbor, will recompile"
            [("deploymentId", toJSON deployId)]
          pure Nothing
        Right bundles -> pure (Just bundles)

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
-- Returns False if removal failed (e.g. EFS/NFS race with stale files).
deleteBundle :: BundleStore -> Text -> IO Bool
deleteBundle (BundleStore root) deployId = do
  let deployDir = root </> Text.unpack deployId
  exists <- doesDirectoryExist deployDir
  if exists
    then do
      removeDirectoryRecursive deployDir
      pure True
    `catch` \(_ :: IOException) -> pure False
  else pure True

-- | Delete only the CBOR cache for a deployment, keeping sources intact.
deleteBundleCbor :: BundleStore -> Text -> IO ()
deleteBundleCbor (BundleStore root) deployId = do
  let cborFile = root </> Text.unpack deployId </> "bundle.cbor"
  exists <- doesFileExist cborFile
  if exists
    then removeFile cborFile
    else pure ()

-- | Save the full DeploymentMetadata as a JSON cache after successful compilation.
-- This allows serving metadata for pending/lazy-loaded deployments without recompilation.
saveMetadataCache :: BundleStore -> Text -> LBS.ByteString -> IO ()
saveMetadataCache (BundleStore root) deployId metaJson = do
  let cacheFile = root </> Text.unpack deployId </> "openapi-cache.json"
      tmpFile = cacheFile <> ".tmp"
  LBS.writeFile tmpFile metaJson
  exists <- doesFileExist cacheFile
  if exists
    then do
      removeFile cacheFile
      renameFile tmpFile cacheFile
    else
      renameFile tmpFile cacheFile

-- | Try to load the cached DeploymentMetadata for a deployment.
-- Returns the raw JSON ByteString (caller decodes as DeploymentMetadata).
loadMetadataCache :: BundleStore -> Text -> IO (Maybe LBS.ByteString)
loadMetadataCache (BundleStore root) deployId = do
  let cacheFile = root </> Text.unpack deployId </> "openapi-cache.json"
  exists <- doesFileExist cacheFile
  if not exists
    then pure Nothing
    else (Just . LBS.fromStrict <$> BS.readFile cacheFile)
      `catch` \(_ :: IOException) -> pure Nothing

-- | Load a single source file from a deployment.
-- Returns 'Nothing' if the file doesn't exist or the path is unsafe.
loadSingleFile :: BundleStore -> Text -> FilePath -> IO (Maybe Text)
loadSingleFile (BundleStore root) deployId relPath
  | not (isPathSafe relPath) = pure Nothing
  | otherwise = do
      let fullPath = root </> Text.unpack deployId </> "sources" </> relPath
      exists <- doesFileExist fullPath
      if exists
        then (Just <$> Text.IO.readFile fullPath)
          `catch` \(_ :: IOException) -> pure Nothing
        else pure Nothing

-- | Check if a path is safe (no path traversal).
isPathSafe :: FilePath -> Bool
isPathSafe path = ".." `notElem` splitDirectories path

-- | List all .l4 source files in a deployment.
listSourceFiles :: BundleStore -> Text -> IO [FilePath]
listSourceFiles (BundleStore root) deployId = do
  let sourcesDir = root </> Text.unpack deployId </> "sources"
  exists <- doesDirectoryExist sourcesDir
  if not exists
    then pure []
    else listL4FilesRecursive sourcesDir sourcesDir

-- | Recursively list .l4 file paths relative to a base directory.
listL4FilesRecursive :: FilePath -> FilePath -> IO [FilePath]
listL4FilesRecursive baseDir dir = do
  entries <- listDirectory dir
  paths <- mapM (\entry -> do
    let fullPath = dir </> entry
    isDir <- doesDirectoryExist fullPath
    if isDir
      then listL4FilesRecursive baseDir fullPath
      else if takeExtension entry == ".l4"
        then pure [makeRelative baseDir fullPath]
        else pure []
    ) entries
  pure (concat paths)

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
