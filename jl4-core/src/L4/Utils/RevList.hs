module L4.Utils.RevList where

import Base

newtype RevList a = MkRevList [a]
  deriving stock (Show, Generic)
  deriving anyclass NFData

unRevList :: RevList a -> [a]
unRevList (MkRevList xs) = reverse xs

emptyRevList :: RevList a
emptyRevList = MkRevList []

pushRevList :: forall a. a -> RevList a -> RevList a
pushRevList = coerce ((:) @a)
