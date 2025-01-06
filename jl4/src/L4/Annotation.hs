{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
module L4.Annotation where

import qualified Data.Foldable as Foldable
import Data.Kind
import qualified Data.List as List
import Data.TreeDiff.Class (ToExpr)
import qualified GHC.Generics as GHC
import Optics

import L4.Lexer
import Generics.SOP.Constraint

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
  type AnnoToken t :: Type
  type AnnoToken t = PosToken
  getAnno :: t -> Anno_ (AnnoToken t)
  setAnno :: Anno_ (AnnoToken t) -> t -> t

  default setAnno :: (GPosition 1 t t a (Anno_ (AnnoToken t))) => Anno_ (AnnoToken t) -> t -> t
  setAnno ann e = set (gposition @1) ann e

  default getAnno :: (GPosition 1 t t (Anno_ (AnnoToken t)) (Anno_ (AnnoToken t))) => t -> Anno_ (AnnoToken t)
  getAnno e = e ^. gposition @1

-- This constraint enforces that Anno is the first field (of each constructor).
--
-- It would be better to unify this with HasAnno somehow.
class (Head xs ~ Anno_ t, All c (Tail xs), xs ~ (Head xs : Tail xs)) => AnnoFirst t c (xs :: [Type])
instance (Head xs ~ Anno_ t, All c (Tail xs), xs ~ (Head xs : Tail xs)) => AnnoFirst t c (xs :: [Type])

-- ----------------------------------------------------------------------------
-- Annotation Instances
-- ----------------------------------------------------------------------------

instance Semigroup (Anno_ t) where
  (Anno m1) <> (Anno m2) = Anno (m1 <> m2)

instance Monoid (Anno_ t) where
  mempty = emptyAnno

