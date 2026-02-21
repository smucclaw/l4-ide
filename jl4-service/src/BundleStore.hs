module BundleStore (
  BundleStore (..),
  StoredMetadata (..),
  StoredFunctionSummary (..),
  initStore,
  saveBundle,
  loadBundle,
  listDeployments,
  deleteBundle,
) where

import Data.Aeson (FromJSON, ToJSON, eitherDecodeFileStrict', encodeFile)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import GHC.Generics (Generic)
import System.Directory
  ( createDirectoryIfMissing
  , doesDirectoryExist
  , doesFileExist
  , listDirectory
  , removeDirectoryRecursive
  , renameDirectory
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
