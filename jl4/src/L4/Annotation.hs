{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
module L4.Annotation where

import L4.Lexer ( SrcRange (..) )

import Data.Kind
import qualified Data.List as List
import Data.TreeDiff.Class (ToExpr)
import qualified GHC.Generics as GHC
import Generics.SOP.Constraint
import Optics

data NodeVisibility
  = -- | A token cluster that is hidden because it was inserted by some tool.
    -- The user did not write it.
    Hidden
  | -- | A token written by the user.
    Visible
  deriving stock (Show, Ord, Eq, Enum, Bounded, GHC.Generic)
  deriving anyclass ToExpr

data ConcreteSyntaxNode_ t = ConcreteSyntaxNode
  { tokens :: [t]
  , range :: Maybe SrcRange
  , visibility :: NodeVisibility
  }
  deriving stock (Show, Ord, Eq, GHC.Generic)
  deriving anyclass ToExpr

data CsnCluster_ t = CsnCluster
  { payload :: ConcreteSyntaxNode_ t
  , trailing :: ConcreteSyntaxNode_ t
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
mkAnno es = Anno es

emptyAnno :: Anno_ t
emptyAnno = mkAnno []

isEmptyAnno :: Anno_ t -> Bool
isEmptyAnno m = List.null m.payload

class HasAnno t where
  type AnnoToken t :: Type
  getAnno :: t -> Anno_ (AnnoToken t)
  setAnno :: Anno_ (AnnoToken t) -> t -> t

  default setAnno :: (GPosition 1 t t a (Anno_ (AnnoToken t))) => Anno_ (AnnoToken t) -> t -> t
  setAnno ann e = set (gposition @1) ann e

  default getAnno :: (GPosition 1 t t (Anno_ (AnnoToken t)) (Anno_ (AnnoToken t))) => t -> Anno_ (AnnoToken t)
  getAnno e = e ^. gposition @1

instance HasAnno (Anno_ t) where
  type AnnoToken (Anno_ t) = t
  getAnno = id
  setAnno = const

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
