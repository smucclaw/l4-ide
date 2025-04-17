-- | Computing info for "go to definition"
module L4.FindDefinition where

import Base
import L4.Annotation
import L4.Parser.SrcSpan (SrcPos(..), SrcRange(..), inRange)
import L4.Syntax
import L4.TypeCheck

findDefinition :: ToResolved a => SrcPos -> a -> Maybe SrcRange
findDefinition pos a = do
  resolved <- find matches (toResolved a)
  let original = getOriginal resolved
  rangeOf original
  where
    matches :: Resolved -> Bool
    matches r =
      case rangeOf (getName r) of
        Just range -> inRange pos range
        Nothing -> False
