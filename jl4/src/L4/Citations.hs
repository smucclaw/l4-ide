module L4.Citations
( validRelPath
, readContents
, normalizeRef
, mkReferences
, partitionEithersOnL
, srcRangeToInterval
, intervalToSrcRange
) where

import Base (Text)
import Text.Regex.Pcre2
import Data.Monoid (Alt (..))
import qualified Base.Text as Text
import Control.Exception.Safe (IOException, displayException, tryJust)
import Data.ByteString.Lazy (LazyByteString)
import qualified System.OsPath as Path
import qualified System.File.OsPath as Path
import qualified HaskellWorks.Data.IntervalMap.FingerTree as IVMap
import qualified Data.Csv as Csv

import L4.Parser.SrcSpan  as Lexer
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
  let
      records = foldMap getReferences tokens

      getReferences = \case
        Lexer.MkPosToken {payload = Lexer.TRef reference _, range} ->
           let mk v = IVMap.singleton (srcRangeToInterval range) (range.length, v)
               ref = normalizeRef reference

               replaceVerbatim p r =
                 Alt if ref == p
                   then Just r
                   else Nothing

               replaceRegex p r =
                 Alt if p `matches` ref
                   then Just (sub p r ref)
                   else Nothing

               -- NOTE: we replace verbatim in any case, except if the pattern starts with
               -- "regex:"
               doMatching p r
                 = case Text.stripPrefix "regex:" p of
                     Nothing -> replaceVerbatim p r
                     Just p' -> replaceRegex p' r

            -- NOTE: This does linearly many matches (and thus makes the entire algorithm quadratic),
            -- which could be more efficient by just unioning the regular expression, however,
            -- in that case picking a replacement becomes extra work, so we just match each one
            -- of them and then run the replacement
            in mk $ getAlt $ foldMap (uncurry doMatching) decoded
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
