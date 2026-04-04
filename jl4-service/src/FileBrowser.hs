{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module FileBrowser (
  FileBrowseApi,
  ShortFileBrowseApi,
  fileBrowseHandler,
  shortFileBrowseHandler,
  -- * Response types
  FilesResponse (..),
  FileDetail (..),
  SearchMatch (..),
  -- * Search utilities (re-exported for MCP)
  searchIdentifier,
  searchText,
) where

import qualified BundleStore
import Types

import Control.Concurrent.STM (readTVarIO)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (asks)
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import Data.List (sortBy)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Ord (comparing)
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)
import Servant
import System.FilePath (takeExtension)

-- ----------------------------------------------------------------------------
-- API types
-- ----------------------------------------------------------------------------

-- | File browsing API under /deployments/{id}/files
-- Raw .l4 file access (/{id}/file.l4) is handled by l4FileRewriteMiddleware in Application.hs.
type FileBrowseApi =
       "deployments" :> Capture "deploymentId" Text :> "files"
       :> QueryParam "file" Text
       :> QueryParam "identifier" Text
       :> QueryParam "search" Text
       :> Get '[JSON] FilesResponse

-- | Short file browsing route: /{id}/files[?file=&identifier=&search=]
-- Raw .l4 file access on short routes (/{id}/path/to/file.l4) is handled
-- by l4FileRewriteMiddleware which rewrites to the canonical form.
type ShortFileBrowseApi =
       "files"
       :> QueryParam "file" Text
       :> QueryParam "identifier" Text
       :> QueryParam "search" Text
       :> Get '[JSON] FilesResponse

-- ----------------------------------------------------------------------------
-- Response types
-- ----------------------------------------------------------------------------

data FilesResponse = FilesResponse
  { frFiles   :: ![FileDetail]
  , frMatches :: !(Maybe [SearchMatch])
  }
  deriving stock (Show, Generic)

instance Aeson.ToJSON FilesResponse where
  toJSON fr = Aeson.object $
    [ "files" .= fr.frFiles
    ] <> maybe [] (\m -> ["matches" .= m]) fr.frMatches

instance Aeson.FromJSON FilesResponse where
  parseJSON = Aeson.withObject "FilesResponse" $ \o ->
    FilesResponse
      <$> o Aeson..: "files"
      <*> o Aeson..:? "matches"

data FileDetail = FileDetail
  { fdPath       :: !Text
  , fdTotalLines :: !Int
  , fdExports    :: ![Text]
  , fdContent    :: !Text
  }
  deriving stock (Show, Generic)

instance Aeson.ToJSON FileDetail where
  toJSON fd = Aeson.object
    [ "path"       .= fd.fdPath
    , "totalLines" .= fd.fdTotalLines
    , "exports"    .= fd.fdExports
    , "content"    .= fd.fdContent
    ]

instance Aeson.FromJSON FileDetail where
  parseJSON = Aeson.withObject "FileDetail" $ \o ->
    FileDetail
      <$> o Aeson..: "path"
      <*> o Aeson..: "totalLines"
      <*> o Aeson..:? "exports" Aeson..!= []
      <*> o Aeson..: "content"

data SearchMatch = SearchMatch
  { smFile         :: !Text
  , smLineStart    :: !Int
  , smLineEnd      :: !Int
  , smContent      :: !Text
  , smIsDefinition :: !Bool
  }
  deriving stock (Show, Generic)

instance Aeson.ToJSON SearchMatch where
  toJSON sm = Aeson.object
    [ "file"         .= sm.smFile
    , "lineStart"    .= sm.smLineStart
    , "lineEnd"      .= sm.smLineEnd
    , "content"      .= sm.smContent
    , "isDefinition" .= sm.smIsDefinition
    ]

instance Aeson.FromJSON SearchMatch where
  parseJSON = Aeson.withObject "SearchMatch" $ \o ->
    SearchMatch
      <$> o Aeson..: "file"
      <*> o Aeson..: "lineStart"
      <*> o Aeson..: "lineEnd"
      <*> o Aeson..: "content"
      <*> o Aeson..:? "isDefinition" Aeson..!= False

-- ----------------------------------------------------------------------------
-- Handlers
-- ----------------------------------------------------------------------------

fileBrowseHandler :: ServerT FileBrowseApi AppM
fileBrowseHandler deployIdText = filesQueryHandler (DeploymentId deployIdText)

shortFileBrowseHandler :: DeploymentId -> ServerT ShortFileBrowseApi AppM
shortFileBrowseHandler deployId = filesQueryHandler deployId

-- | GET /files — JSON query endpoint.
-- Works without compilation by reading source files from disk.
-- Uses metadata for export info when available (deployment is Ready).
filesQueryHandler
  :: DeploymentId
  -> Maybe Text    -- ^ ?file= filter
  -> Maybe Text    -- ^ ?identifier= search
  -> Maybe Text    -- ^ ?search= text search
  -> AppM FilesResponse
filesQueryHandler deployId mFileFilter mIdentifier mSearch = do
  env <- asks id
  let store = env.bundleStore

  -- Try to get metadata from registry (if Ready) for export info
  registry <- liftIO $ readTVarIO env.deploymentRegistry
  let mMeta = case Map.lookup deployId registry of
        Just (DeploymentReady _ meta) -> Just meta
        _ -> Nothing

  -- Load source files from disk (always available post-upload)
  (sourceMap, _storedMeta) <- liftIO $ BundleStore.loadBundle store deployId.unDeploymentId
  let l4Sources = Map.filterWithKey (\k _ -> takeExtension k == ".l4") sourceMap

  -- Apply file filter
  let filteredSources = case mFileFilter of
        Nothing -> l4Sources
        Just f  -> Map.filterWithKey (\k _ -> Text.pack k == f) l4Sources

  -- Build export lookup from metadata (empty if not compiled yet)
  let exportsByFile = case mMeta of
        Just meta -> Map.fromListWith (++)
          [ (sf, [fs.fsName])
          | fs <- meta.metaFunctions
          , Just sf <- [fs.fsSourceFile]
          ]
        Nothing -> Map.empty

  case (mIdentifier, mSearch) of
    (Just identifier, _) -> do
      -- Identifier search
      let matches = searchIdentifier identifier filteredSources
          matchFiles = map (.smFile) matches
          fileDetails = [ mkFileDetail exportsByFile fp content
                        | (fp, content) <- sortBy (comparing fst) (Map.toList filteredSources)
                        , Text.pack fp `elem` matchFiles
                        ]
      pure FilesResponse
        { frFiles = fileDetails
        , frMatches = Just matches
        }
    (_, Just query) -> do
      -- Text search
      let matches = searchText query filteredSources
          matchFiles = map (.smFile) matches
          fileDetails = [ mkFileDetail exportsByFile fp content
                        | (fp, content) <- sortBy (comparing fst) (Map.toList filteredSources)
                        , Text.pack fp `elem` matchFiles
                        ]
      pure FilesResponse
        { frFiles = fileDetails
        , frMatches = Just matches
        }
    (Nothing, Nothing) -> do
      -- No search: return all files with full content
      let fileDetails = [ mkFileDetail exportsByFile fp content
                        | (fp, content) <- sortBy (comparing fst) (Map.toList filteredSources)
                        ]
      pure FilesResponse
        { frFiles = fileDetails
        , frMatches = Nothing
        }

-- ----------------------------------------------------------------------------
-- Search logic
--
-- NOTE: Search and definition block detection use text-based line scanning
-- (not the L4 parser/AST). This is intentional:
--   - Works on any .l4 file regardless of compilation state (pending/failed)
--   - No coupling to the compilation pipeline
--   - Simpler and faster for a browsing feature
--
-- Known limitations:
--   - May match identifiers inside string literals or comments
--   - Block boundary detection is heuristic (indentation + keyword-based)
--   - Unusual formatting or multi-line backtick identifiers may confuse it
--
-- A future improvement could use the AST (SerializedBundle.sbModule has source
-- spans) for compiled deployments, falling back to text scanning for
-- pending/failed ones.
-- ----------------------------------------------------------------------------

-- | Search for an L4 identifier across source files.
-- Returns matches with context. For definitions (DECLARE/DECIDE/MEANS/ASSUME),
-- returns the entire definition block including decorators and GIVEN/GIVETH.
searchIdentifier :: Text -> Map FilePath Text -> [SearchMatch]
searchIdentifier identifier sources =
  concatMap (\(fp, content) -> searchIdentifierInFile identifier (Text.pack fp) content)
    (Map.toList sources)

searchIdentifierInFile :: Text -> Text -> Text -> [SearchMatch]
searchIdentifierInFile identifier filepath content =
  let allLines = zip [1..] (Text.lines content)
      totalLines = length allLines
      matchingLineNums = [ lineNum
                         | (lineNum, line) <- allLines
                         , identifierMatches identifier line
                         ]
  in deduplicateMatches $ map (buildMatch filepath allLines totalLines) matchingLineNums

-- | Check if a line contains the identifier (whole-word match).
-- Handles both backtick-quoted identifiers and bare identifiers.
identifierMatches :: Text -> Text -> Bool
identifierMatches identifier line =
  let backtickForm = "`" <> identifier <> "`"
  in backtickForm `Text.isInfixOf` line
     || wordBoundaryMatch identifier line

-- | Simple word boundary matching for non-backtick identifiers.
wordBoundaryMatch :: Text -> Text -> Bool
wordBoundaryMatch word line =
  let positions = Text.breakOnAll word line
  in any (\(before, after) ->
    let charBefore = if Text.null before then ' ' else Text.last before
        rest = Text.drop (Text.length word) after
        charAfter = if Text.null rest then ' ' else Text.head rest
    in not (isIdentChar charBefore) && not (isIdentChar charAfter)
    ) positions
 where
  isIdentChar c = c == '_' || c == '\'' || c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z' || c >= '0' && c <= '9'

-- | Build a search match for a given line number.
-- Detects definition blocks and expands them appropriately.
buildMatch :: Text -> [(Int, Text)] -> Int -> Int -> SearchMatch
buildMatch filepath allLines totalLines lineNum =
  let lineText = maybe "" id $ lookup' lineNum allLines
  in if isDefinitionKeyword lineText
    then buildDefinitionMatch filepath allLines totalLines lineNum
    else buildContextMatch filepath allLines totalLines lineNum

-- | Check if a line starts with a definition keyword.
isDefinitionKeyword :: Text -> Bool
isDefinitionKeyword line =
  let stripped = Text.stripStart line
  in any (`Text.isPrefixOf` stripped)
       ["DECLARE ", "DECIDE ", "MEANS ", "ASSUME "]

-- | Build a match for a definition block.
-- Scans up for decorators/GIVEN/GIVETH, down for block end.
buildDefinitionMatch :: Text -> [(Int, Text)] -> Int -> Int -> SearchMatch
buildDefinitionMatch filepath allLines totalLines lineNum =
  let -- Scan upward for decorators, GIVEN, GIVETH
      blockStart = scanUpForBlockStart allLines lineNum
      -- Scan downward for block end
      blockEnd = scanDownForBlockEnd allLines totalLines lineNum
      -- Add 3 lines padding
      paddedStart = max 1 (blockStart - 3)
      paddedEnd = min totalLines (blockEnd + 3)
      -- Extract content
      slicedLines = [ snd l | l <- allLines, fst l >= paddedStart, fst l <= paddedEnd ]
  in SearchMatch
    { smFile = filepath
    , smLineStart = paddedStart
    , smLineEnd = paddedEnd
    , smContent = Text.unlines slicedLines
    , smIsDefinition = True
    }

-- | Scan upward from a definition keyword to find the block start.
-- Includes preceding @-decorators, GIVEN, GIVETH lines.
scanUpForBlockStart :: [(Int, Text)] -> Int -> Int
scanUpForBlockStart allLines lineNum = go (lineNum - 1)
 where
  go n
    | n < 1 = lineNum
    | otherwise = case lookup' n allLines of
        Nothing -> lineNum
        Just line ->
          let stripped = Text.stripStart line
          in if Text.isPrefixOf "@" stripped
                || Text.isPrefixOf "GIVEN " stripped || stripped == "GIVEN"
                || Text.isPrefixOf "GIVETH " stripped || stripped == "GIVETH"
                || Text.null stripped  -- blank line between decorator and keyword
             then go (n - 1)
             else n + 1

-- | Scan downward from a definition keyword to find the block end.
-- A block ends at the next top-level keyword or a blank line followed by
-- a non-indented line.
scanDownForBlockEnd :: [(Int, Text)] -> Int -> Int -> Int
scanDownForBlockEnd allLines totalLines lineNum = go (lineNum + 1) lineNum
 where
  go n lastContentLine
    | n > totalLines = lastContentLine
    | otherwise = case lookup' n allLines of
        Nothing -> lastContentLine
        Just line
          | Text.null (Text.strip line) ->
              -- Blank line: check if next non-blank line is a new top-level block
              let nextNonBlank = findNextNonBlank allLines totalLines (n + 1)
              in case nextNonBlank of
                   Nothing -> n  -- End of file
                   Just (_, nextLine) ->
                     if isTopLevelStart nextLine
                       then lastContentLine
                       else go (n + 1) n
          | isTopLevelStart line && n > lineNum -> lastContentLine
          | otherwise -> go (n + 1) n

-- | Check if a line starts a new top-level block.
isTopLevelStart :: Text -> Bool
isTopLevelStart line =
  not (Text.null line)
  && Text.head line /= ' '
  && Text.head line /= '\t'
  && any (`Text.isPrefixOf` line)
       [ "DECLARE ", "DECIDE ", "MEANS ", "ASSUME "
       , "GIVEN ", "GIVETH ", "@"
       , "§"
       ]

-- | Find the next non-blank line.
findNextNonBlank :: [(Int, Text)] -> Int -> Int -> Maybe (Int, Text)
findNextNonBlank allLines totalLines n
  | n > totalLines = Nothing
  | otherwise = case lookup' n allLines of
      Nothing -> Nothing
      Just line
        | Text.null (Text.strip line) -> findNextNonBlank allLines totalLines (n + 1)
        | otherwise -> Just (n, line)

-- | Build a context match (non-definition, +/-3 lines).
buildContextMatch :: Text -> [(Int, Text)] -> Int -> Int -> SearchMatch
buildContextMatch filepath allLines totalLines lineNum =
  let paddedStart = max 1 (lineNum - 3)
      paddedEnd = min totalLines (lineNum + 3)
      slicedLines = [ snd l | l <- allLines, fst l >= paddedStart, fst l <= paddedEnd ]
  in SearchMatch
    { smFile = filepath
    , smLineStart = paddedStart
    , smLineEnd = paddedEnd
    , smContent = Text.unlines slicedLines
    , smIsDefinition = False
    }

-- | Grep-like text search across source files (case-insensitive).
searchText :: Text -> Map FilePath Text -> [SearchMatch]
searchText query sources =
  concatMap (\(fp, content) -> searchTextInFile query (Text.pack fp) content)
    (Map.toList sources)

searchTextInFile :: Text -> Text -> Text -> [SearchMatch]
searchTextInFile query filepath content =
  let allLines = zip [1..] (Text.lines content)
      totalLines = length allLines
      queryLower = Text.toLower query
      matchingLineNums = [ lineNum
                         | (lineNum, line) <- allLines
                         , queryLower `Text.isInfixOf` Text.toLower line
                         ]
  in deduplicateMatches $ map (buildContextMatch filepath allLines totalLines) matchingLineNums

-- | Remove overlapping matches by merging adjacent ones.
deduplicateMatches :: [SearchMatch] -> [SearchMatch]
deduplicateMatches [] = []
deduplicateMatches [m] = [m]
deduplicateMatches (m1:m2:rest)
  | m1.smFile == m2.smFile && m1.smLineEnd >= m2.smLineStart - 1 =
      -- Overlapping or adjacent: merge
      let merged = m1
            { smLineEnd = max m1.smLineEnd m2.smLineEnd
            , smContent = mergeContent m1 m2
            , smIsDefinition = m1.smIsDefinition || m2.smIsDefinition
            }
      in deduplicateMatches (merged : rest)
  | otherwise = m1 : deduplicateMatches (m2 : rest)
 where
  mergeContent a b
    | a.smLineEnd >= b.smLineEnd = a.smContent
    | otherwise =
        let aLines = Text.lines a.smContent
            bLines = Text.lines b.smContent
            overlap = a.smLineEnd - b.smLineStart + 1
            newLines = drop (max 0 overlap) bLines
        in Text.unlines (aLines ++ newLines)

-- ----------------------------------------------------------------------------
-- Helpers
-- ----------------------------------------------------------------------------


-- | Build a FileDetail from a source file entry.
mkFileDetail :: Map Text [Text] -> FilePath -> Text -> FileDetail
mkFileDetail exportsByFile fp content =
  let path = Text.pack fp
      allLines = Text.lines content
  in FileDetail
    { fdPath = path
    , fdTotalLines = length allLines
    , fdExports = maybe [] id (Map.lookup path exportsByFile)
    , fdContent = content
    }

-- | Lookup a value in an association list by key.
lookup' :: Eq a => a -> [(a, b)] -> Maybe b
lookup' _ [] = Nothing
lookup' k ((k', v):rest)
  | k == k'   = Just v
  | otherwise = lookup' k rest
