{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module L4.ExactPrint where

import L4.Annotation
import L4.Lexer
import L4.Syntax

import qualified Control.Monad.Extra as Extra
import Control.Monad.Trans.Except
import Data.Kind
import Data.Text
import qualified Data.Text as Text
import Generics.SOP as SOP
import Generics.SOP.Constraint
import Generics.SOP.NP
import Generics.SOP.NS
import GHC.Stack

type EPM = Except EPError

data EPError
  = InsufficientHoleFit CallStack
  deriving (Show)

prettyEPError :: EPError -> Text
prettyEPError (InsufficientHoleFit cs) = "HoleFit requested but not enough given at: " <> Text.pack (prettyCallStack cs)

type HoleFit = EPM [PosToken]

exactprint :: Program Name -> Either EPError Text
exactprint =
  runExcept . fmap (Text.concat . fmap displayPosToken) . toTokens

class ToTokens a where
  toTokens :: a -> HoleFit

  default toTokens ::
       (SOP.Generic a, All (AnnoFirst ToTokens) (Code a))
    => a -> HoleFit
  toTokens =
    genericToTokens (Proxy @ToTokens) toTokens applyTokensWithHoles

genericToTokens :: forall c a r. (Generic a, All (AnnoFirst c) (Code a)) => Proxy c -> (forall x. c x => x -> r) -> (Anno -> [r] -> r) -> a -> r
genericToTokens _ rec f x =
    collapse_NS
  $ cmap_NS
      (Proxy @(AnnoFirst c))
      (\ (I anno :* xs) ->
        K (f anno (collapse_NP (cmap_NP (Proxy @c) (mapIK rec) xs))))
  $ unSOP
  $ from x

-- This constraint enforces that Anno is the first field (of each constructor).
--
-- It would be better to unify this with HasAnno somehow.
class (Head xs ~ Anno, All c (Tail xs), xs ~ (Head xs : Tail xs)) => AnnoFirst c (xs :: [Type])
instance (Head xs ~ Anno, All c (Tail xs), xs ~ (Head xs : Tail xs)) => AnnoFirst c (xs :: [Type])

instance ToTokens a => ToTokens [a] where
  toTokens =
    Extra.concatMapM toTokens

instance ToTokens a => ToTokens (Maybe a) where
  toTokens =
    maybe (pure []) toTokens

deriving anyclass instance ToTokens (Program Name)

-- Generic instance does not apply because we exclude the level.
instance ToTokens (Section Name) where
  toTokens (MkSection ann _lvl name decls) =
    applyTokensWithHoles ann [toTokens name, toTokens decls]

deriving anyclass instance ToTokens (TopDecl Name)
deriving anyclass instance ToTokens (Assume Name)
deriving anyclass instance ToTokens (Declare Name)
deriving anyclass instance ToTokens (TypeDecl Name)
deriving anyclass instance ToTokens (ConDecl Name)
deriving anyclass instance ToTokens (Type' Name)
deriving anyclass instance ToTokens (TypedName Name)
deriving anyclass instance ToTokens (OptionallyTypedName Name)
deriving anyclass instance ToTokens (Decide Name)
deriving anyclass instance ToTokens (AppForm Name)
deriving anyclass instance ToTokens (Expr Name)
deriving anyclass instance ToTokens (Branch Name)
deriving anyclass instance ToTokens (Pattern Name)
deriving anyclass instance ToTokens (TypeSig Name)
deriving anyclass instance ToTokens (GivethSig Name)
deriving anyclass instance ToTokens (GivenSig Name)
deriving anyclass instance ToTokens (Directive Name)

instance ToTokens Name where
  toTokens (Name ann _) =
    applyTokensWithHoles ann []

applyTokensWithHoles :: (HasCallStack) => Anno -> [HoleFit] -> EPM [PosToken]
applyTokensWithHoles (Anno []) _ = pure []
applyTokensWithHoles (Anno (AnnoHole : cs)) holeFits = case holeFits of
  [] -> do
    throwE $ InsufficientHoleFit callStack
  (x : xs) -> do
    r <- x
    rs <- applyTokensWithHoles (Anno cs) xs
    pure (r <> rs)
applyTokensWithHoles (Anno (AnnoCsn m : cs)) xs = do
  let
    r = csnTokens m
  rs <- applyTokensWithHoles (Anno cs) xs
  pure (r <> rs)
