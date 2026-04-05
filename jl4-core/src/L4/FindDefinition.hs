-- | Computing info for "go to definition"
module L4.FindDefinition where

import Base
import Control.Applicative ((<|>))
import L4.Annotation
import L4.Parser.SrcSpan (SrcPos(..), SrcRange(..), inRange)
import L4.Syntax
import L4.TypeCheck

findDefinition :: ToResolved a => SrcPos -> a -> Maybe SrcRange
findDefinition pos a = do
  resolved <- find matches allResolved
  let original = getOriginal resolved
  -- The original defining name may have had its source annotation cleared
  -- (e.g. synthetic record constructors created by the type checker for
  -- DECLARE HAS records). In that case, fall back to finding any Def
  -- with the same raw name that does have a source range.
  rangeOf original <|> findDefByName (rawName original)
  where
    allResolved = toResolved a

    matches :: Resolved -> Bool
    matches r =
      case rangeOf (getName r) of
        Just range -> inRange pos range
        Nothing -> False

    findDefByName :: RawName -> Maybe SrcRange
    findDefByName rn = do
      Def _ defName <- find (isDefWithName rn) allResolved
      rangeOf defName

    isDefWithName :: RawName -> Resolved -> Bool
    isDefWithName rn (Def _ n) = rawName n == rn && isJust (rangeOf n)
    isDefWithName _ _          = False
