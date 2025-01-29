{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module L4.ExactPrint where

import Base
import L4.Annotation
import L4.Lexer
import qualified L4.Parser as Parser
import L4.Syntax

import qualified Control.Monad.Extra as Extra
import qualified Data.Text as Text
import GHC.Stack (HasCallStack)

-- ----------------------------------------------------------------------------
-- ExactPrinting Interface
-- ----------------------------------------------------------------------------

type ExactPrintM = Except TraverseAnnoError

type HoleFit_ t = ExactPrintM [t]

concreteNodesToTokens :: ToConcreteNodes t a => a -> ExceptT TraverseAnnoError Identity [t]
concreteNodesToTokens a = fmap (concatMap allClusterTokens) $ toNodes a

class ToTokens t a where
  toTokens :: a -> HoleFit_ t

data Tree =
  Fork SrcRange [Tree] | Leaf Name

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

exactprint' :: (HasCallStack, ToConcreteNodes PosToken p) => p -> Text
exactprint' p =
  case exactprint p of
    Left err -> error $ Text.unpack $ prettyTraverseAnnoError err
    Right t -> t

-- | Parse a source file and exact-print the result.
exactprintFile :: String -> Text -> Text
exactprintFile file input =
  case Parser.execParser Parser.program file input of
    Left errs -> Text.unlines $ fmap (.message) $ toList errs
    Right prog ->
      case exactprint prog of
        Left epError -> prettyTraverseAnnoError epError
        Right ep -> ep
