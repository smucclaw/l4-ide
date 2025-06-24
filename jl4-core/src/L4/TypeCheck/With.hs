{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
-- | The With type and instances.
--
-- This is in principle a general-purpose datatype, and a variant
-- of the delay or partiality monad.
--
-- We use it during type checking to record errors.
--
module L4.TypeCheck.With where

import Base

import Control.Monad.Logic

data With e a =
    With e (With e a)
  | Plain a
  deriving (Show, Generic)

instance Functor (With e) where
  fmap = liftM

instance Applicative (With e) where
  pure = Plain
  (<*>) = ap

instance Monad (With e) where
  Plain a  >>= f = f a
  With e m >>= f = With e (m >>= f)

class MonadWith e m | m -> e where
  with :: e -> m ()

instance MonadWith e (With e) where
  with e = With e (Plain ())

instance (Monad m, MonadWith e m) => MonadWith e (LogicT m) where
  with e = lift (with e)

instance (Monad m, MonadWith e m) => MonadWith e (StateT s m) where
  with e = lift (with e)

distributeWith :: With e [(With e a, b)] -> [(With e a, b)]
distributeWith (Plain xs) = xs
distributeWith (With e w) = fmap (first (With e)) (distributeWith w)

runWith :: With e a -> ([e], a)
runWith (Plain a)  = ([], a)
runWith (With e w) = first (e :) (runWith w)
