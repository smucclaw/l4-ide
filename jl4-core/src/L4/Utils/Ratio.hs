module L4.Utils.Ratio (Rational, denominator, numerator, isInteger, prettyRatio) where

import Data.Ratio
import Base
import qualified Base.Text as Text
import qualified Data.Scientific as Sci

isInteger :: Rational -> Maybe Integer
isInteger r =
  if denominator r == 1
    then Just $ numerator r
    else Nothing

prettyRatio :: Rational -> Text
prettyRatio r = case isInteger r of
  Nothing -> Text.pack $ Sci.formatScientific Sci.Fixed Nothing $ Sci.fromFloatDigits (fromRational @Double r)
  Just i -> Text.show i
