module LSP.Core.Types.Location where

import Language.LSP.Protocol.Types
import Data.Maybe
import Data.Hashable (hash)

normalizeFilePath :: FilePath -> NormalizedFilePath
-- We want to keep empty paths instead of normalising them to "."
normalizeFilePath "" = emptyFilePath
normalizeFilePath fp = toNormalizedFilePath fp

emptyFilePath :: NormalizedFilePath
emptyFilePath = emptyNormalizedFilePath

-- | We use an empty string as a filepath when we don’t have a file.
-- However, haskell-lsp doesn’t support that in uriToFilePath and given
-- that it is not a valid filepath it does not make sense to upstream a fix.
-- So we have our own wrapper here that supports empty filepaths.
uriToFilePath' :: Uri -> Maybe FilePath
uriToFilePath' uri
    | uri == fromNormalizedUri emptyPathUri = Just ""
    | otherwise = uriToFilePath uri

emptyPathUri :: NormalizedUri
emptyPathUri =
    let s = "file://"
    in NormalizedUri (hash s) s

fromUri :: NormalizedUri -> NormalizedFilePath
fromUri = fromMaybe (normalizeFilePath noFilePath) . uriToNormalizedFilePath

noFilePath :: FilePath
noFilePath = "<unknown>"

-- A dummy range to use when range is unknown
noRange :: Range
noRange =  Range (Position 0 0) (Position 1 0)
