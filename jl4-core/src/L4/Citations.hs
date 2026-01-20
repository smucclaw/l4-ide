-- | Reference annotations for L4 documents.
--
-- This module provides functionality for handling @ref and @ref-map annotations
-- that create clickable links in L4 source files.
--
-- Supported annotations:
--   * @ref <text> - Creates a clickable reference link
--   * @ref-map <name> <url> - Defines a mapping from reference name to URL
--
-- Note: The @ref-src annotation (loading from CSV files) and regex pattern
-- matching have been removed to enable WASM compilation.
module L4.Citations
( withRefMap
, normalizeRef
, mkReferences
) where

import Base (Text, NormalizedUri)
import Data.Monoid (Alt (..))
import Data.Char (isSpace)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified Base.Text as Text

import L4.Parser.SrcSpan  as Lexer
import qualified L4.Utils.IntervalMap as IVMap
import qualified L4.Lexer as Lexer

-- | Parse a ref-map token into a pair of reference name and reference url.
-- Handles @ref-map annotations like: @ref-map "case 123" https://example.com/case/123
withRefMap :: Lexer.PosToken -> Vector (Text, Text)
withRefMap = \ case
  Lexer.MkPosToken {payload = Lexer.TAnnotations (Lexer.TRefMap refmp)}
    -- NOTE: annotations like @ref-map foo bar baz https://example.com should work as well, so we break on
    -- end because we don't expect spaces in the url.
    | (ref, url) <- Text.breakOnEnd " " $ Text.strip refmp
    , not $ Text.all isSpace ref
    , not $ Text.all isSpace url
    -> Vector.singleton (normalizeRef ref, normalizeRef url)
  _ -> mempty

-- | Normalize a reference string for case-insensitive matching.
-- Strips whitespace and converts to lowercase.
normalizeRef :: Text -> Text
normalizeRef = Text.toLower . Text.strip

-- | Build an interval map of references from tokens and decoded mappings.
--
-- This creates a lookup structure that maps source positions to reference
-- information (module URI, length, and optional URL).
mkReferences
  :: [Lexer.PosToken]
  -- ^ The tokens containing the @ref annotations
  -> Vector (Text, Text)
  -- ^ The reference mappings from @ref-map annotations (name -> url)
  -> IVMap.IntervalMap Lexer.SrcPos (NormalizedUri, Int, Maybe Text)
  -- ^ An interval map for checking if a source position points to a reference
mkReferences tokens decoded = do
  foldMap getReferences tokens
  where
    getReferences = \ case
      Lexer.MkPosToken {payload = Lexer.TAnnotations (Lexer.TRef reference _), range} ->
        let mk v = IVMap.singleton (IVMap.srcRangeToInterval range) (range.moduleUri, range.length, v)
            ref = normalizeRef reference

            -- Simple verbatim matching (case-insensitive)
            matchVerbatim p r =
              Alt if ref == p
                then Just r
                else Nothing

         in mk $ getAlt $ foldMap (uncurry matchVerbatim) decoded
      _ ->  IVMap.empty

