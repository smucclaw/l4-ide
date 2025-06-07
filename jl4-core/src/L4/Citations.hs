module L4.Citations
( withRefSrc
, withRefMap
, readContents
, normalizeRef
, mkReferences
) where

import Base (Text, NormalizedUri)
import Data.Monoid (Alt (..))
import Data.Char (isSpace)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified Base.Text as Text
import Control.Monad.Except (ExceptT (..), liftEither, withExceptT)
import Control.Exception.Safe (IOException, displayException, tryJust)
import Data.ByteString.Lazy (LazyByteString)
import qualified Text.Regex.Pcre2 as Pcre2
import qualified System.OsPath as Path
import qualified System.File.OsPath as Path
import qualified Data.Csv as Csv

import L4.Parser.SrcSpan  as Lexer
import qualified L4.Utils.IntervalMap as IVMap
import qualified L4.Lexer as Lexer

-- | obtain a valid relative file path from the ref-src annos
withRefSrc :: Path.OsPath -> Lexer.PosToken -> ExceptT (Lexer.SrcRange, String) IO (Vector (Text, Text))
withRefSrc ownPath = \ case
  Lexer.MkPosToken {payload = Lexer.TRefSrc src, range}
    | Just osp <- Path.encodeUtf $ Text.unpack $ Text.strip src
    , Path.isRelative osp
    -> withExceptT (range,) do
      ecs <- ExceptT
        $ readContents
        $ Path.normalise
        $ Path.takeDirectory ownPath Path.</> osp
      liftEither $ Csv.decode Csv.NoHeader ecs
  _ -> pure mempty

-- | parse a ref-map token into a pair of reference name and reference url
withRefMap :: Lexer.PosToken -> Vector (Text, Text)
withRefMap = \ case
  Lexer.MkPosToken {payload = Lexer.TRefMap refmp}
    -- NOTE: annotations like @ref-src foo bar baz https://example.com should work as well, so we break on
    -- end because we don't expect spaces in the url.
    | (ref, url) <- Text.breakOnEnd " " $ Text.strip refmp
    , not $ Text.all isSpace ref
    , not $ Text.all isSpace url
    -> Vector.singleton (normalizeRef ref, normalizeRef url)
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
  -- ^ The tokens containing the @ref annotations
  -> Vector (Text, Text)
  -- ^ the references in that document
  -> IVMap.IntervalMap Lexer.SrcPos (NormalizedUri, Int, Maybe Text)
  -- ^ an intervalmap that can be used to check whether a
  --   given source position points to a reference
mkReferences tokens decoded = do
  foldMap getReferences tokens
  where
    getReferences = \ case
      Lexer.MkPosToken {payload = Lexer.TRef reference _, range} ->
        let mk v = IVMap.singleton (IVMap.srcRangeToInterval range) (range.moduleUri, range.length, v)
            ref = normalizeRef reference

            replaceVerbatim p r =
              Alt if ref == p
                then Just r
                else Nothing

            replaceRegex p r =
              Alt if p `Pcre2.matches` ref
                then Just (Pcre2.sub p r ref)
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

