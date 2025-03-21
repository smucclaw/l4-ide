-- | Computing info for "go to definition"
module L4.FindDefinition where

import Base
import L4.Annotation
import L4.Parser.SrcSpan (SrcPos(..), SrcRange(..), inRange)
import L4.Syntax
import L4.TypeCheck

import qualified Generics.SOP as SOP
import Language.LSP.Protocol.Types (NormalizedUri)

-- | It would be better to have a tree structure so that we can exclude
-- large parts of the tree easily. However, right now we don't have range
-- information cheaply in the tree, so I thought I could as well build a
-- list.
--
-- Note that we are building a tree for type information, and possibly we
-- could do something similar here. It's just probably not worth it.
--
class ToResolved a where
  toResolved :: a -> [Resolved]
  default toResolved :: (SOP.Generic a, SOP.All (AnnoFirst a ToResolved) (SOP.Code a)) => a -> [Resolved]
  toResolved = genericToResolved

deriving anyclass instance ToResolved (Module  Resolved)
deriving anyclass instance ToResolved (Section Resolved)
deriving anyclass instance ToResolved (TopDecl Resolved)
deriving anyclass instance ToResolved (LocalDecl Resolved)
deriving anyclass instance ToResolved (Assume Resolved)
deriving anyclass instance ToResolved (Declare Resolved)
deriving anyclass instance ToResolved (TypeDecl Resolved)
deriving anyclass instance ToResolved (ConDecl Resolved)
deriving anyclass instance ToResolved (Type' Resolved)
deriving anyclass instance ToResolved (TypedName Resolved)
deriving anyclass instance ToResolved (OptionallyTypedName Resolved)
deriving anyclass instance ToResolved (OptionallyNamedType Resolved)
deriving anyclass instance ToResolved (Decide Resolved)
deriving anyclass instance ToResolved (AppForm Resolved)
deriving anyclass instance ToResolved (Aka Resolved)
deriving anyclass instance ToResolved (Expr Resolved)
deriving anyclass instance ToResolved (NamedExpr Resolved)
deriving anyclass instance ToResolved (Branch Resolved)
deriving anyclass instance ToResolved (Pattern Resolved)
deriving anyclass instance ToResolved (TypeSig Resolved)
deriving anyclass instance ToResolved (GivethSig Resolved)
deriving anyclass instance ToResolved (GivenSig Resolved)
deriving anyclass instance ToResolved (Directive Resolved)
deriving anyclass instance ToResolved (Import Resolved)

instance ToResolved NormalizedUri where
  toResolved = const []

instance ToResolved Lit where
  toResolved = const []

instance ToResolved Int where
  toResolved = const []

instance ToResolved RawName where
  toResolved = const []

instance ToResolved Resolved where
  toResolved = pure

instance ToResolved a => ToResolved (Maybe a) where
  toResolved = concatMap toResolved

instance ToResolved a => ToResolved [a] where
  toResolved = concatMap toResolved

genericToResolved :: forall a. (SOP.Generic a, SOP.All (AnnoFirst a ToResolved) (SOP.Code a)) => a -> [Resolved]
genericToResolved =
  genericToNodes
    (Proxy @ToResolved)
    toResolved
    (const concat :: Anno' a -> [[Resolved]] -> [Resolved])

findDefinition :: ToResolved a => SrcPos -> a -> Maybe SrcRange
findDefinition pos a = do
  r <- find matches (toResolved a)
  rangeOf (getOriginal r)
  where
    matches :: Resolved -> Bool
    matches r =
      case rangeOf (getName r) of
        Just range -> inRange pos range
        Nothing -> False
