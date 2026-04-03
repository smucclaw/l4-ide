{-# LANGUAGE StrictData #-}

-- | Binary cache for loaded ontologies.
--
-- Loading RDF files is slow (parsing XML, building data structures).
-- This module provides a binary cache that can load the ontology graph
-- much faster on subsequent runs.
module L4.ACTUS.Ontology.Cache (
  -- * Cache Operations
  loadFromCache,
  saveToCache,
  invalidateCache,

  -- * Cache Paths
  defaultCachePath,
  getCachePath,

  -- * Cache Metadata
  CacheMetadata (..),
  isCacheValid,
) where

import Data.Binary (Binary (..), Get, decodeFileOrFail, encodeFile)
import Data.Int (Int64)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import GHC.Generics (Generic)
import L4.ACTUS.Ontology.Types (OntologyGraph)
import System.Directory (
  createDirectoryIfMissing,
  doesFileExist,
  getModificationTime,
  removeFile,
 )
import System.FilePath (takeDirectory, (</>))

-- | Default cache directory.
defaultCachePath :: FilePath
defaultCachePath = ".cache/jl4-actus"

-- | Get the full cache file path.
getCachePath :: Maybe FilePath -> FilePath
getCachePath mBase =
  let base = maybe defaultCachePath id mBase
   in base </> "ontology.bin"

-- | Wrapper for UTCTime with Binary instance
newtype SerializableTime = SerializableTime UTCTime
  deriving stock (Eq, Show)

instance Binary SerializableTime where
  put (SerializableTime t) = put (realToFrac (utcTimeToPOSIXSeconds t) :: Double)
  get = do
    d <- get :: Get Double
    pure $ SerializableTime $ posixSecondsToUTCTime (realToFrac d)

-- | Metadata about the cached ontology.
data CacheMetadata = CacheMetadata
  { cacheVersion :: Int
  -- ^ Cache format version (increment when changing Binary instances)
  , cacheCreatedAt :: SerializableTime
  -- ^ When the cache was created
  , cacheSourceFiles :: [FilePath]
  -- ^ Source RDF files that were loaded
  , cacheSourceModTimes :: [(FilePath, SerializableTime)]
  -- ^ Modification times of source files at cache time
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Binary)

-- | Current cache version. Increment when changing Binary instances.
currentCacheVersion :: Int
currentCacheVersion = 1

-- | A cached ontology with metadata.
data CachedOntology = CachedOntology
  { metadata :: CacheMetadata
  , graph :: OntologyGraph
  }
  deriving stock (Generic)
  deriving anyclass (Binary)

-- | Load an ontology from the cache if valid.
loadFromCache :: Maybe FilePath -> IO (Maybe OntologyGraph)
loadFromCache mCachePath = do
  let path = getCachePath mCachePath
  exists <- doesFileExist path
  if not exists
    then pure Nothing
    else do
      result <- decodeFileOrFail path :: IO (Either (Int64, String) CachedOntology)
      case result of
        Left _ -> pure Nothing
        Right cached -> do
          let CachedOntology meta graph' = cached
          valid <- isCacheValid meta
          pure $ if valid then Just graph' else Nothing

-- | Save an ontology to the cache.
saveToCache :: Maybe FilePath -> [(FilePath, UTCTime)] -> OntologyGraph -> IO ()
saveToCache mCachePath sourceModTimes graph' = do
  let path = getCachePath mCachePath
  createDirectoryIfMissing True (takeDirectory path)
  now <- getCurrentTime
  let metadata' =
        CacheMetadata
          { cacheVersion = currentCacheVersion
          , cacheCreatedAt = SerializableTime now
          , cacheSourceFiles = map fst sourceModTimes
          , cacheSourceModTimes = [(f, SerializableTime t) | (f, t) <- sourceModTimes]
          }
  let cached = CachedOntology{metadata = metadata', graph = graph'}
  encodeFile path cached

-- | Invalidate (delete) the cache.
invalidateCache :: Maybe FilePath -> IO ()
invalidateCache mCachePath = do
  let path = getCachePath mCachePath
  exists <- doesFileExist path
  if exists then removeFile path else pure ()

-- | Check if cached data is still valid.
--
-- The cache is invalid if:
-- - The cache version doesn't match
-- - Any source file has been modified since caching
-- - Any source file no longer exists
isCacheValid :: CacheMetadata -> IO Bool
isCacheValid meta = do
  -- Check version
  if meta.cacheVersion /= currentCacheVersion
    then pure False
    else do
      -- Check each source file
      validities <- mapM checkSourceFile meta.cacheSourceModTimes
      pure $ and validities
 where
  checkSourceFile (path, SerializableTime cachedTime) = do
    exists <- doesFileExist path
    if not exists
      then pure False
      else do
        currentTime <- getModificationTime path
        pure $ currentTime <= cachedTime
