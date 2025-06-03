module L4.Names where

import L4.Syntax

class HasName a where
  getName :: a -> Name

instance HasName Name where
  getName n = n

instance HasName Resolved where
  getName = getActual

instance HasName a => HasName (AppForm a) where
  getName (MkAppForm _ n _ _) = getName n

instance HasName a => HasName (ConDecl a) where
  getName (MkConDecl _ n _) = getName n

instance HasName a => HasName (TypedName a) where
  getName (MkTypedName _ann n _t) = getName n

instance HasName a => HasName (OptionallyTypedName a) where
  getName (MkOptionallyTypedName _ann n _mt) = getName n
