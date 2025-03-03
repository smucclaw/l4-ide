-- | Computing info for hover (currently types and kinds of expressions)
module L4.HoverInfo where

import Base
import L4.Annotation
import L4.Lexer (PosToken(..), SrcPos(..), SrcRange(..), inRange)
import L4.Syntax
import L4.TypeCheck

import Control.Applicative
import qualified Generics.SOP as SOP

data InfoTree =
  InfoNode
    { range    :: Maybe SrcRange
    , info     :: Maybe Info
    , children :: [InfoTree]
    }

class ToInfoTree a where
  toInfoTree :: a -> [InfoTree]
  default toInfoTree :: (SOP.Generic a, AnnoToken a ~ PosToken, AnnoExtra a ~ Extension, SOP.All (AnnoFirst a ToInfoTree) (SOP.Code a)) => a -> [InfoTree]
  toInfoTree = genericToInfoTree

instance HasSrcRange InfoTree where
  rangeOf = (.range)

genericToInfoTree :: forall a. (SOP.Generic a, AnnoToken a ~ PosToken, AnnoExtra a ~ Extension, SOP.All (AnnoFirst a ToInfoTree) (SOP.Code a)) => a -> [InfoTree]
genericToInfoTree =
  genericToNodes
    (Proxy @ToInfoTree)
    toInfoTree
    (mergeInfoTrees (Proxy @a))

mergeInfoTrees :: (AnnoToken a ~ PosToken, AnnoExtra a ~ Extension) => Proxy a -> Anno' a -> [[InfoTree]] -> [InfoTree]
mergeInfoTrees _ anno children =
  [InfoNode (rangeOf anno) (getInfo anno) (concat children)]

findInfo :: ToInfoTree a => SrcPos -> a -> Maybe (SrcRange, Info)
findInfo pos a =
  asum (go <$> toInfoTree a)
  where
    go :: InfoTree -> Maybe (SrcRange, Info)
    go (InfoNode Nothing _info children)     =
        asum (go <$> children) -- <|> if t == Nothing then Just (range, Type mempty) else (range,) <$> t
    go (InfoNode (Just range) info children)
      | pos `inRange` range                =
        asum (go <$> children) <|> {- if t == Nothing then Just (range, Type mempty) else -} (range,) <$> info
      | otherwise                          = Nothing

deriving anyclass instance ToInfoTree (Program Resolved)
deriving anyclass instance ToInfoTree (Section Resolved)
deriving anyclass instance ToInfoTree (TopDecl Resolved)
deriving anyclass instance ToInfoTree (LocalDecl Resolved)
deriving anyclass instance ToInfoTree (Assume Resolved)
deriving anyclass instance ToInfoTree (Declare Resolved)
deriving anyclass instance ToInfoTree (TypeDecl Resolved)
deriving anyclass instance ToInfoTree (ConDecl Resolved)
deriving anyclass instance ToInfoTree (Type' Resolved)
deriving anyclass instance ToInfoTree (TypedName Resolved)
deriving anyclass instance ToInfoTree (OptionallyTypedName Resolved)
deriving anyclass instance ToInfoTree (OptionallyNamedType Resolved)
deriving anyclass instance ToInfoTree (Decide Resolved)
deriving anyclass instance ToInfoTree (AppForm Resolved)
deriving anyclass instance ToInfoTree (Aka Resolved)
deriving anyclass instance ToInfoTree (Expr Resolved)
deriving anyclass instance ToInfoTree (NamedExpr Resolved)
deriving anyclass instance ToInfoTree (Branch Resolved)
deriving anyclass instance ToInfoTree (Pattern Resolved)
deriving anyclass instance ToInfoTree (TypeSig Resolved)
deriving anyclass instance ToInfoTree (GivethSig Resolved)
deriving anyclass instance ToInfoTree (GivenSig Resolved)
deriving anyclass instance ToInfoTree (Directive Resolved)

-- | Extract suitable hover info from a tree node.
getInfo :: (HasAnno a, AnnoToken a ~ PosToken, AnnoExtra a ~ Extension) => a -> Maybe Info
getInfo x =
  view annInfo (getAnno x)

mkInfoLeaf :: (HasAnno a, AnnoToken a ~ PosToken, AnnoExtra a ~ Extension) => a -> InfoTree
mkInfoLeaf x =
  InfoNode (rangeOf (getAnno x)) (getInfo x) []

instance ToInfoTree Lit where
  toInfoTree l =
    [mkInfoLeaf l]

instance ToInfoTree Int where
  toInfoTree _ =
    [InfoNode Nothing Nothing []]

instance ToInfoTree Name where
  toInfoTree n =
    [mkInfoLeaf n]

instance ToInfoTree RawName where
  toInfoTree _ =
    [InfoNode Nothing Nothing []]

instance ToInfoTree Resolved where
  toInfoTree n =
    toInfoTree (getName n)

instance ToInfoTree a => ToInfoTree (Maybe a) where
  toInfoTree =
    concatMap toInfoTree

instance ToInfoTree a => ToInfoTree [a] where
  toInfoTree =
    concatMap toInfoTree
