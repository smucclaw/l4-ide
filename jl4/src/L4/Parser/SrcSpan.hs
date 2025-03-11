module L4.Parser.SrcSpan where

import Base

import qualified Generics.SOP as SOP
import qualified Base.Text as Text

-- ----------------------------------------------------------------------------
-- SrcSpan
-- ----------------------------------------------------------------------------

data SrcSpan = MkSrcSpan
  { start :: !SrcPos
  , end :: !SrcPos
  }
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (SOP.Generic)

instance Semigroup SrcSpan where
  -- Always expand the 'SrcSpan' s.t. it now covers both 'SrcSpan's.
  s1 <> s2 = MkSrcSpan (min s1.start s2.start) (max s1.end s2.end)

fromSrcRange :: SrcRange -> SrcSpan
fromSrcRange rng = MkSrcSpan rng.start rng.end

posInRange :: SrcPos -> SrcSpan -> Bool
posInRange p (MkSrcSpan start end) =
  start <= p && p <= end

spansOverlap :: SrcSpan -> SrcSpan -> Bool
spansOverlap s1 s2 =
  s1.start <= s2.end && s2.start <= s1.end

subRangeOf :: SrcSpan -> SrcSpan -> Bool
subRangeOf s1 s2 =
  s2.start <= s1.start && s1.end <= s2.end

rangeBefore :: SrcSpan -> SrcSpan -> Bool
rangeBefore s1 s2 =
  s1.end < s2.start

rangeAfter :: SrcSpan -> SrcSpan -> Bool
rangeAfter s1 s2 =
  s1.start < s2.end

prettySrcSpan :: SrcSpan -> Text
prettySrcSpan (MkSrcSpan p1 p2) = prettySrcPos p1 <> prettyPartialSrcPos p1 p2

-- ----------------------------------------------------------------------------
-- SrcRange
-- ----------------------------------------------------------------------------

-- | A range of source positions. We store the length of a range as well.
data SrcRange =
  MkSrcRange
    { start   :: !SrcPos -- inclusive
    , end     :: !SrcPos -- inclusive
    , length  :: !Int
    }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToExpr, NFData)


-- TODO: eventually, we may want to take sufficient info so that we can print the file path
-- for "external" ranges, but not ranges in the current file.
prettySrcRange :: Maybe FilePath -> Maybe SrcRange -> Text
prettySrcRange fp Nothing = prettyFilePath fp <> "<unknown range>"
prettySrcRange fp (Just (MkSrcRange p1 p2 _)) = prettyFilePath fp <> prettySrcPos p1 <> prettyPartialSrcPos p1 p2

prettyFilePath :: Maybe FilePath -> Text
prettyFilePath Nothing   = ""
prettyFilePath (Just fp) = Text.pack fp <> ":"

-- ----------------------------------------------------------------------------
-- SrcPos
-- ----------------------------------------------------------------------------


-- | A single source position. Line and column numbers are 1-based.
data SrcPos =
  MkSrcPos
    {
    -- filename :: !FilePath
      line     :: !Int
    , column   :: !Int
    }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToExpr, NFData)

-- | We ignore the file name, because we assume this has already been checked.
inRange :: SrcPos -> SrcRange -> Bool
inRange (MkSrcPos l c) (MkSrcRange (MkSrcPos l1 c1) (MkSrcPos l2 c2) _) =
     (l, c) >= (l1, c1)
  && (l, c) <= (l2, c2)


prettySrcPos :: SrcPos -> Text
prettySrcPos (MkSrcPos l c) = Text.show l <> ":" <> Text.show c

prettyPartialSrcPos :: SrcPos -> SrcPos -> Text
prettyPartialSrcPos (MkSrcPos rl rc) p@(MkSrcPos l c)
  | rl == l && rc == c = ""
  | rl == l            = "-" <> Text.show c
  | otherwise          = "-" <> prettySrcPos p

