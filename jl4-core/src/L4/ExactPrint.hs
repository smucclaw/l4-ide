{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module L4.ExactPrint where

import Base
import qualified Base.Text as Text
import L4.Annotation
import L4.Lexer

import qualified Control.Monad.Extra as Extra

-- ----------------------------------------------------------------------------
-- ExactPrinting Interface
-- ----------------------------------------------------------------------------

type ExactPrintM = Except TraverseAnnoError

type HoleFit_ t = ExactPrintM [t]

concreteNodesToTokens :: ToConcreteNodes t a => a -> ExceptT TraverseAnnoError Identity [t]
concreteNodesToTokens a = concatMap allClusterTokens <$> toNodes a

class ToTokens t a where
  toTokens :: a -> HoleFit_ t

instance ToTokens t a => ToTokens t [a] where
  toTokens =
    Extra.concatMapM toTokens

instance ToTokens t a => ToTokens t (Maybe a) where
  toTokens =
    maybe (pure []) toTokens

-- ----------------------------------------------------------------------------
-- JL4 specific implementation
-- ----------------------------------------------------------------------------

exactprint :: ToConcreteNodes PosToken p => p -> Either TraverseAnnoError Text
exactprint =
  runExcept . fmap (Text.concat . fmap displayPosToken) . concreteNodesToTokens
