module L4.Citations
( validRelPath
, readContents
, normalizeRef
, mkReferences
, partitionEithersOnL
, srcRangeToInterval
, intervalToSrcRange
) where

import Base
import qualified Base.Text as Text
import Control.Exception.Safe (IOException, displayException, tryJust)
import Data.ByteString.Lazy (LazyByteString)
import qualified System.OsPath as Path
import qualified System.File.OsPath as Path
import qualified HaskellWorks.Data.IntervalMap.FingerTree as IVMap
import qualified Data.Map.Lazy as Map
import qualified Data.Csv as Csv

import qualified L4.Lexer as Lexer

-- | obtain a valid relative file path from the ref-src annos
validRelPath :: Path.OsPath -> Lexer.PosToken -> [(Lexer.SrcRange, Path.OsPath)]
validRelPath ownPath = \case
  Lexer.MkPosToken {payload = Lexer.TRefSrc src, range}
    | Just osp <- Path.encodeUtf $ Text.unpack $ Text.strip src
    , Path.isRelative osp
    -> [(range, Path.normalise $ Path.takeDirectory ownPath Path.</> osp)]
  _ -> mempty

-- | read the contents of the filepaths specified
readContents :: Path.OsPath -> IO (Either String LazyByteString)
readContents fp =
  tryJust (\(e :: IOException) -> Just (displayException e))
  $ Path.readFile fp

normalizeRef :: Text -> Text
normalizeRef = Text.toLower . Text.strip

mkReferences
  :: [Lexer.PosToken]
  -- ^ The tokens containing the reference annotations
  -> Either String LazyByteString
  -- ^ the csv that contains the references
  -> Either String (IVMap.IntervalMap Lexer.SrcPos (Int, Maybe Text))
  -- ^ an intervalmap that can be used to check whether a
  --   given source position points to a reference
mkReferences tokens csv = do
  decoded <- Csv.decode Csv.NoHeader =<< csv
  let refLookup = foldMap (uncurry Map.singleton) decoded

      records = foldMap getReferences tokens

      getReferences = \case
        Lexer.MkPosToken {payload = Lexer.TRef reference _, range} ->
           let mk v = IVMap.singleton (srcRangeToInterval range) (range.length, v)
            in mk $ Map.lookup (normalizeRef reference) refLookup
        _ ->  IVMap.empty

  pure records

partitionEithersOnL :: (ann -> err -> err') -> [(ann, Either err a)] -> ([err'], [a])
partitionEithersOnL k = foldMap \case
  (ann, Left l) -> ([k ann l], [])
  (_, Right r) -> ([], [r])

srcRangeToInterval :: Lexer.SrcRange -> IVMap.Interval Lexer.SrcPos
srcRangeToInterval range = IVMap.Interval range.start range.end

intervalToSrcRange :: Int -> IVMap.Interval Lexer.SrcPos -> Lexer.SrcRange
intervalToSrcRange len iv = Lexer.MkSrcRange iv.low iv.high len
