module L4.Utils.IntervalMap (
  IV.IntervalMap,
  IV.search,
  IV.dominators,
  IV.insert,
  IV.union,
  IV.high,
  IV.low,
  IV.singleton,
  IV.empty,
  smallestContaining,
  intervalToRange,
  intervalToRange0,
  srcRangeToInterval,
  intervalToSrcRange,
) where

import Base

import L4.Parser.SrcSpan

import Control.Arrow (first)
import HaskellWorks.Data.IntervalMap.FingerTree (Interval, IntervalMap)
import qualified HaskellWorks.Data.IntervalMap.FingerTree as IV

smallestContaining ::
  NormalizedUri ->
  SrcPos ->
  IntervalMap SrcPos b ->
  Maybe (SrcRange, b)
smallestContaining nuri sp info =
  let
    r = IV.search sp info
  in
    case r of
      [] -> Nothing
      _ -> Just (first (intervalToRange0 nuri) $ maximumBy cmp r)
 where
  cmp (a, _) (b, _) =
    (b.high `compare` a.high)
      <> (a.low `compare` b.low)

intervalToRange :: NormalizedUri -> Int -> Interval SrcPos -> SrcRange
intervalToRange uri len iv = intervalToSrcRange uri len iv

intervalToRange0 :: NormalizedUri -> Interval SrcPos -> SrcRange
intervalToRange0 uri iv = intervalToRange uri 0 iv

srcRangeToInterval :: SrcRange -> Interval SrcPos
srcRangeToInterval range = IV.Interval range.start range.end

intervalToSrcRange :: NormalizedUri -> Int -> Interval SrcPos -> SrcRange
intervalToSrcRange uri len iv = MkSrcRange iv.low iv.high len uri
