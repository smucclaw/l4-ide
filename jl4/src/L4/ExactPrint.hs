{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuantifiedConstraints #-}

module L4.ExactPrint where

import Base
import L4.Annotation
import L4.Lexer
import qualified L4.Parser as Parser
import L4.Syntax

import qualified Control.Monad.Extra as Extra
import qualified Data.Text as Text
import Generics.SOP as SOP
import Generics.SOP.NP
import Generics.SOP.NS
import GHC.Stack

-- ----------------------------------------------------------------------------
-- ExactPrinting Interface
-- ----------------------------------------------------------------------------

type ExactPrintM = Except EPError

data EPError
  = InsufficientHoleFit CallStack
  deriving (Show)

prettyEPError :: EPError -> Text
prettyEPError (InsufficientHoleFit cs) = "HoleFit requested but not enough given at: " <> Text.pack (prettyCallStack cs)

type AnyHoleFit_ m t = m t

type HoleFit_ t = AnyHoleFit_ ExactPrintM [t]

class ToTokens t a where
  toTokens :: a -> HoleFit_ t

  default toTokens ::
       (SOP.Generic a, All (AnnoFirst t (ToTokens t)) (Code a))
    => a -> HoleFit_ t
  toTokens =
    genericToTokens (Proxy @(ToTokens t)) toTokens applyTokensWithHoles

genericToTokens :: forall c a t r. (SOP.Generic a, All (AnnoFirst t c) (Code a)) => Proxy c -> (forall x. c x => x -> r) -> (Anno_ t -> [r] -> r) -> a -> r
genericToTokens _ rec f x =
    collapse_NS
  $ cmap_NS
      (Proxy @(AnnoFirst t c))
      (\ (I anno :* xs) ->
        K (f anno (collapse_NP (cmap_NP (Proxy @c) (mapIK rec) xs))))
  $ unSOP
  $ from x

instance ToTokens t a => ToTokens t [a] where
  toTokens =
    Extra.concatMapM toTokens

instance ToTokens t a => ToTokens t (Maybe a) where
  toTokens =
    maybe (pure []) toTokens

applyTokensWithHoles :: (HasCallStack, MonadError EPError m) => Anno_ t -> [AnyHoleFit_ m [t]] -> m [t]
applyTokensWithHoles (Anno []) _ = pure []
applyTokensWithHoles (Anno (AnnoHole : cs)) holeFits = case holeFits of
  [] -> do
    throwError $ InsufficientHoleFit callStack
  (x : xs) -> do
    r <- x
    rs <- applyTokensWithHoles (Anno cs) xs
    pure (r <> rs)
applyTokensWithHoles (Anno (AnnoCsn m : cs)) xs = do
  let
    r = csnTokens m
  rs <- applyTokensWithHoles (Anno cs) xs
  pure (r <> rs)

-- ----------------------------------------------------------------------------
-- JL4 specific implementation
-- ----------------------------------------------------------------------------

exactprint :: Program Name -> Either EPError Text
exactprint =
  runExcept . fmap (Text.concat . fmap displayPosToken) . toTokens

-- | Parse a source file and exact-print the result.
exactprintFile :: String -> Text -> Text
exactprintFile file input =
  case Parser.execParser Parser.program file input of
    Left errs -> Text.unlines $ fmap (.message) $ toList errs
    Right prog ->
      case exactprint prog of
        Left epError -> prettyEPError epError
        Right ep -> ep

deriving anyclass instance ToTokens PosToken (Program Name)

-- Generic instance does not apply because we exclude the level.
instance ToTokens PosToken (Section Name) where
  toTokens (MkSection ann _lvl name decls) =
    applyTokensWithHoles ann [toTokens name, toTokens decls]

deriving anyclass instance ToTokens PosToken (TopDecl Name)
deriving anyclass instance ToTokens PosToken (Assume Name)
deriving anyclass instance ToTokens PosToken (Declare Name)
deriving anyclass instance ToTokens PosToken (TypeDecl Name)
deriving anyclass instance ToTokens PosToken (ConDecl Name)
deriving anyclass instance ToTokens PosToken (Type' Name)
deriving anyclass instance ToTokens PosToken (TypedName Name)
deriving anyclass instance ToTokens PosToken (OptionallyTypedName Name)
deriving anyclass instance ToTokens PosToken (Decide Name)
deriving anyclass instance ToTokens PosToken (AppForm Name)
deriving anyclass instance ToTokens PosToken (Expr Name)
deriving anyclass instance ToTokens PosToken (Branch Name)
deriving anyclass instance ToTokens PosToken (Pattern Name)
deriving anyclass instance ToTokens PosToken (TypeSig Name)
deriving anyclass instance ToTokens PosToken (GivethSig Name)
deriving anyclass instance ToTokens PosToken (GivenSig Name)
deriving anyclass instance ToTokens PosToken (Directive Name)

instance ToTokens PosToken Name where
  toTokens (Name ann _) =
    applyTokensWithHoles ann []
  toTokens (PreDef ann _) =
    applyTokensWithHoles ann []
