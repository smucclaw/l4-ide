{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
module L4.Annotation where

import Data.Foldable qualified as Foldable
import Data.List qualified as List
import Data.TreeDiff.Class (ToExpr)
import qualified GHC.Generics as GHC
import Optics

data NodeVisibility
  = -- | A token cluster that is hidden because it was inserted by some tool.
    -- The user did not write it.
    Hidden
  | -- | A token written by the user.
    Visible
  deriving stock (Show, Ord, Eq, Enum, Bounded, GHC.Generic)
  deriving anyclass ToExpr

data ConcreteSyntaxNode t = ConcreteSyntaxNode
  { tokens :: [t]
  -- , range :: Maybe SrcRange
  , visibility :: NodeVisibility
  }
  deriving stock (Show, Ord, Eq, GHC.Generic)
  deriving anyclass ToExpr

data CsnCluster_ t = CsnCluster
  { payload :: ConcreteSyntaxNode t
  , trailing :: ConcreteSyntaxNode t
  }
  deriving stock (Show, Ord, Eq, GHC.Generic)
  deriving anyclass ToExpr

data AnnoElement_ t
  = AnnoHole
  | AnnoCsn (CsnCluster_ t)
  deriving stock (Show, Ord, Eq, GHC.Generic)
  deriving anyclass ToExpr

mkHoleWithType :: a -> AnnoElement_ t
mkHoleWithType _ = AnnoHole

mkHole :: AnnoElement_ t
mkHole = AnnoHole

mkCsn :: CsnCluster_ t -> AnnoElement_ t
mkCsn = AnnoCsn

data Anno_ t = Anno
  { payload :: [AnnoElement_ t]
  }
  deriving stock (Show, Ord, Eq, GHC.Generic)
  deriving anyclass ToExpr

csnTokens :: CsnCluster_ t -> [t]
csnTokens cluster = cluster.payload.tokens <> cluster.trailing.tokens

mkAnno :: [AnnoElement_ t] -> Anno_ t
mkAnno = Anno

emptyAnno :: Anno_ t
emptyAnno = mkAnno []

isEmptyAnno :: Anno_ t -> Bool
isEmptyAnno m = List.null m.payload

mkConcreteSyntaxNode :: [t] -> ConcreteSyntaxNode t
mkConcreteSyntaxNode posTokens =
  ConcreteSyntaxNode
    { tokens = posTokens
    -- , range = Nothing -- TODO fendor: fix this
    , visibility =
        if Foldable.null posTokens
          then Hidden
          else Visible
    }

mkHiddenCsnCluster :: CsnCluster_ t
mkHiddenCsnCluster =
  CsnCluster
    { payload = mkConcreteSyntaxNode []
    , trailing = mkConcreteSyntaxNode []
    }

class HasAnno t where
  type AnnoToken t
  getAnno :: t -> Anno_ (AnnoToken t)
  setAnno :: Anno_ (AnnoToken t) -> t -> t

  default setAnno :: (GPosition 1 t t a (Anno_ (AnnoToken t))) => Anno_ (AnnoToken t) -> t -> t
  setAnno ann e = set (gposition @1) ann e

  default getAnno :: (GPosition 1 t t (Anno_ (AnnoToken t)) (Anno_ (AnnoToken t))) => t -> Anno_ (AnnoToken t)
  getAnno e = e ^. gposition @1

-- ----------------------------------------------------------------------------
-- Annotation Instances
-- ----------------------------------------------------------------------------

instance Semigroup (Anno_ t) where
  (Anno m1) <> (Anno m2) = Anno (m1 <> m2)

instance Monoid (Anno_ t) where
  mempty = emptyAnno

