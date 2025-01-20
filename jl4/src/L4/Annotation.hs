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

data Anno_ t e = Anno
  { extra   :: Maybe e
  , payload :: [AnnoElement_ t]
  }
  deriving stock (Show, Ord, Eq, GHC.Generic)
  deriving anyclass ToExpr

csnTokens :: CsnCluster_ t -> [t]
csnTokens cluster = cluster.payload.tokens <> cluster.trailing.tokens

mkAnno :: [AnnoElement_ t] -> Anno_ t e
mkAnno es = Anno Nothing es

emptyAnno :: Anno_ t e
emptyAnno = mkAnno []

isEmptyAnno :: Anno_ t e -> Bool
isEmptyAnno m = List.null m.payload

type Anno' t = Anno_ (AnnoToken t) (AnnoExtra t)

class HasAnno t where
  type AnnoToken t :: Type
  type AnnoExtra t :: Type
  getAnno :: t -> Anno' t
  setAnno :: Anno' t -> t -> t

  default setAnno :: (GPosition 1 t t (Anno' t) (Anno' t)) => Anno' t -> t -> t
  setAnno = genericSetAnno

  default getAnno :: (GPosition 1 t t (Anno' t) (Anno' t)) => t -> Anno' t
  getAnno = genericGetAnno

genericSetAnno :: GPosition 1 s t a b => b -> s -> t
genericSetAnno ann e = set (gposition @1) ann e

genericGetAnno :: GPosition 1 s s a a => s -> a
genericGetAnno e = e ^. gposition @1

instance HasAnno (Anno_ t e) where
  type AnnoToken (Anno_ t e) = t
  type AnnoExtra (Anno_ t e) = e
  getAnno = id
  setAnno = const

-- This constraint enforces that Anno is the first field (of each constructor).
--
-- It would be better to unify this with HasAnno somehow.
class (Head xs ~ Anno' a, All c (Tail xs), xs ~ (Head xs : Tail xs)) => AnnoFirst a c (xs :: [Type])
instance (Head xs ~ Anno' a, All c (Tail xs), xs ~ (Head xs : Tail xs)) => AnnoFirst a c (xs :: [Type])

-- ----------------------------------------------------------------------------
-- Annotation Instances
-- ----------------------------------------------------------------------------

instance Semigroup (Anno_ t e) where
  (Anno _e1 m1) <> (Anno _e2 m2) = Anno Nothing (m1 <> m2)

instance Monoid (Anno_ t e) where
  mempty = emptyAnno
