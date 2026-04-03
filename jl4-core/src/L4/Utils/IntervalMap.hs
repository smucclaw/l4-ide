-- | A simple interval map implementation.
-- This is a pure Haskell replacement for hw-fingertree's IntervalMap,
-- which depends on mmap and is not WASM-compatible.
--
-- The implementation uses a list of intervals, which is O(n) for search
-- but sufficient for our use case (source position mapping in small files).
-- A more efficient implementation could use a proper interval tree.
module L4.Utils.IntervalMap (
  IntervalMap,
  Interval (..),
  search,
  dominators,
  insert,
  union,
  intervalHigh,
  intervalLow,
  singleton,
  empty,
  smallestContaining,
  intervalToRange,
  intervalToRange0,
  srcRangeToInterval,
  intervalToSrcRange,
) where

import Base hiding (singleton, insert, union)
import L4.Parser.SrcSpan

-- | An interval with low and high bounds
data Interval v = Interval
  { low :: !v
  , high :: !v
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

-- | A simple interval map storing (interval, value) pairs.
-- Implemented as a list for simplicity. For larger maps, consider
-- using a proper interval tree data structure.
newtype IntervalMap v a = IntervalMap [(Interval v, a)]
  deriving stock (Eq, Show, Generic)
  deriving newtype (Semigroup, Monoid)
  deriving anyclass (NFData)

-- | Create an empty interval map
empty :: IntervalMap v a
empty = IntervalMap []

-- | Create a singleton interval map
singleton :: Interval v -> a -> IntervalMap v a
singleton iv val = IntervalMap [(iv, val)]

-- | Insert an interval into the map
insert :: Interval v -> a -> IntervalMap v a -> IntervalMap v a
insert iv val (IntervalMap xs) = IntervalMap ((iv, val) : xs)

-- | Union of two interval maps
union :: IntervalMap v a -> IntervalMap v a -> IntervalMap v a
union (IntervalMap xs) (IntervalMap ys) = IntervalMap (xs ++ ys)

-- | Search for all intervals containing a point
search :: Ord v => v -> IntervalMap v a -> [(Interval v, a)]
search p (IntervalMap xs) =
  [(iv, val) | (iv, val) <- xs, iv.low <= p && p <= iv.high]

-- | Find all intervals that dominate (fully contain) the given point.
-- This is the same as search for point queries.
dominators :: Ord v => v -> IntervalMap v a -> [(Interval v, a)]
dominators = search

-- | Get the high bound of an interval
intervalHigh :: Interval v -> v
intervalHigh iv = iv.high

-- | Get the low bound of an interval
intervalLow :: Interval v -> v
intervalLow iv = iv.low

-- | Find the smallest interval containing a position
smallestContaining ::
  NormalizedUri ->
  SrcPos ->
  IntervalMap SrcPos b ->
  Maybe (SrcRange, b)
smallestContaining nuri sp info =
  first (intervalToRange0 nuri) <$> listToMaybe (sortOn (\(i, _) -> i.high `minus` i.low) $ search sp info)
  where
    minus x y = MkSrcPos
      { line = x.line - y.line
      , column = x.column - y.column
      }

intervalToRange :: NormalizedUri -> Int -> Interval SrcPos -> SrcRange
intervalToRange uri len iv = intervalToSrcRange uri len iv

intervalToRange0 :: NormalizedUri -> Interval SrcPos -> SrcRange
intervalToRange0 uri iv = intervalToRange uri 0 iv

srcRangeToInterval :: SrcRange -> Interval SrcPos
srcRangeToInterval range = Interval range.start range.end

intervalToSrcRange :: NormalizedUri -> Int -> Interval SrcPos -> SrcRange
intervalToSrcRange uri len iv = MkSrcRange iv.low iv.high len uri
