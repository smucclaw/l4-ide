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
import Generics.SOP as SOP
import Generics.SOP.NP
import Generics.SOP.NS
import GHC.Stack
import qualified Data.List.NonEmpty as NonEmpty

-- ----------------------------------------------------------------------------
-- ExactPrinting Interface
-- ----------------------------------------------------------------------------

type ExactPrintM = Except EPError

data EPError
  = InsufficientHoleFit CallStack
  deriving (Show)

prettyEPError :: EPError -> Text
prettyEPError (InsufficientHoleFit cs) = "HoleFit requested but not enough given at: " <> Text.pack (prettyCallStack cs)

type HoleFit_ t = ExactPrintM [t]

concreteNodesToTokens :: ToConcreteNodes t a => a -> ExceptT EPError Identity [t]
concreteNodesToTokens a = fmap (concatMap csnTokens) $ toNodes a

class ToTokens t a where
  toTokens :: a -> HoleFit_ t

class ToConcreteNodes t a | a -> t where
  toNodes :: a -> Except EPError [CsnCluster_ t]

  default toNodes ::
       (SOP.Generic a, All (AnnoFirst a (ToConcreteNodes t)) (Code a), AnnoToken a ~ t)
    => a -> Except EPError [CsnCluster_ t]
  toNodes =
    genericToNodes (Proxy @(ToConcreteNodes t)) toNodes flattenConcreteNodes

genericToNodes :: forall c a r. (SOP.Generic a, All (AnnoFirst a c) (Code a)) => Proxy c -> (forall x. c x => x -> r) -> (Anno' a -> [r] -> r) -> a -> r
genericToNodes _ rec f x =
    collapse_NS
  $ cmap_NS
      (Proxy @(AnnoFirst a c))
      (\ (I anno :* xs) ->
        K (f anno (collapse_NP (cmap_NP (Proxy @c) (mapIK rec) xs))))
  $ unSOP
  $ from x

data Tree =
  Fork SrcRange [Tree] | Leaf Name

instance ToTokens t a => ToTokens t [a] where
  toTokens =
    Extra.concatMapM toTokens

instance ToTokens t a => ToTokens t (Maybe a) where
  toTokens =
    maybe (pure []) toTokens

instance ToConcreteNodes t a => ToConcreteNodes t [a] where
  toNodes =
    Extra.concatMapM toNodes

instance ToConcreteNodes t a => ToConcreteNodes t (Maybe a) where
  toNodes =
    maybe (pure []) toNodes

flattenConcreteNodes :: (HasCallStack, MonadError EPError m) => Anno_ t e -> [m [CsnCluster_ t]] -> m [CsnCluster_ t]
flattenConcreteNodes (Anno _ csns) = go csns
  where
    go []               _        = pure []
    go (AnnoHole : cs)  holeFits =
      case holeFits of
        [] -> throwError $ InsufficientHoleFit callStack
        (x : xs) -> (<>) <$> x <*> go cs xs
    go (AnnoCsn m : cs) holeFits =
      (m :) <$> go cs holeFits

-- ----------------------------------------------------------------------------
-- JL4 specific implementation
-- ----------------------------------------------------------------------------

exactprint :: ToConcreteNodes PosToken p => p -> Either EPError Text
exactprint =
  runExcept . fmap (Text.concat . fmap displayPosToken) . concreteNodesToTokens

-- | Parse a source file and exact-print the result.
exactprintFile :: String -> Text -> Text
exactprintFile file input =
  case Parser.execParser Parser.program file input of
    Left errs -> Text.unlines $ fmap (.message) $ toList errs
    Right prog ->
      case exactprint prog of
        Left epError -> prettyEPError epError
        Right ep -> ep

deriving anyclass instance ToConcreteNodes PosToken (Program Name)

-- Generic instance does not apply because we exclude the level.
instance ToConcreteNodes PosToken (Section Name) where
  toNodes (MkSection ann _lvl name decls) =
    flattenConcreteNodes ann [toNodes name, toNodes decls]

deriving anyclass instance ToConcreteNodes PosToken (TopDecl Name)
deriving anyclass instance ToConcreteNodes PosToken (Assume Name)
deriving anyclass instance ToConcreteNodes PosToken (Declare Name)
deriving anyclass instance ToConcreteNodes PosToken (TypeDecl Name)
deriving anyclass instance ToConcreteNodes PosToken (ConDecl Name)
deriving anyclass instance ToConcreteNodes PosToken (Type' Name)
deriving anyclass instance ToConcreteNodes PosToken (TypedName Name)
deriving anyclass instance ToConcreteNodes PosToken (OptionallyTypedName Name)
deriving anyclass instance ToConcreteNodes PosToken (OptionallyNamedType Name)
deriving anyclass instance ToConcreteNodes PosToken (Decide Name)
deriving anyclass instance ToConcreteNodes PosToken (AppForm Name)
deriving anyclass instance ToConcreteNodes PosToken (Expr Name)
deriving anyclass instance ToConcreteNodes PosToken (NamedExpr Name)
deriving anyclass instance ToConcreteNodes PosToken (Branch Name)
deriving anyclass instance ToConcreteNodes PosToken (Pattern Name)
deriving anyclass instance ToConcreteNodes PosToken (TypeSig Name)
deriving anyclass instance ToConcreteNodes PosToken (GivethSig Name)
deriving anyclass instance ToConcreteNodes PosToken (GivenSig Name)
deriving anyclass instance ToConcreteNodes PosToken (Directive Name)

deriving anyclass instance ToConcreteNodes PosToken (TopDecl Resolved)
deriving anyclass instance ToConcreteNodes PosToken (Assume Resolved)
deriving anyclass instance ToConcreteNodes PosToken (Declare Resolved)
deriving anyclass instance ToConcreteNodes PosToken (TypeDecl Resolved)
deriving anyclass instance ToConcreteNodes PosToken (ConDecl Resolved)
deriving anyclass instance ToConcreteNodes PosToken (Type' Resolved)
deriving anyclass instance ToConcreteNodes PosToken (TypedName Resolved)
deriving anyclass instance ToConcreteNodes PosToken (OptionallyTypedName Resolved)
deriving anyclass instance ToConcreteNodes PosToken (OptionallyNamedType Resolved)
deriving anyclass instance ToConcreteNodes PosToken (Decide Resolved)
deriving anyclass instance ToConcreteNodes PosToken (AppForm Resolved)
deriving anyclass instance ToConcreteNodes PosToken (Expr Resolved)
deriving anyclass instance ToConcreteNodes PosToken (NamedExpr Resolved)
deriving anyclass instance ToConcreteNodes PosToken (Branch Resolved)
deriving anyclass instance ToConcreteNodes PosToken (Pattern Resolved)
deriving anyclass instance ToConcreteNodes PosToken (TypeSig Resolved)
deriving anyclass instance ToConcreteNodes PosToken (GivethSig Resolved)
deriving anyclass instance ToConcreteNodes PosToken (GivenSig Resolved)
deriving anyclass instance ToConcreteNodes PosToken (Directive Resolved)

instance ToConcreteNodes PosToken Int where
  toNodes _txt = pure []
  -- TODO: This is lossy and should be improved (but we should not need to
  -- exact-print inference variables).

instance ToConcreteNodes PosToken Name where
  toNodes (MkName ann _) =
    flattenConcreteNodes ann []

instance ToConcreteNodes PosToken Resolved where
  toNodes r =
    toNodes (getActual r)

instance ToConcreteNodes PosToken RawName where
  toNodes _rn = pure []
  -- TODO: This is lossy and should be improved (but we should not need to
  -- exact-print inference variables).

instance ToConcreteNodes PosToken Lit where
  toNodes (NumericLit ann _) =
    flattenConcreteNodes ann []
  toNodes (StringLit ann _) =
    flattenConcreteNodes ann []

-- -- ----------------------------------------------------------------------------
-- -- Source Range manipulation
-- -- ----------------------------------------------------------------------------

class HasSrcRange a where
  rangeOf :: a -> Maybe SrcRange

instance HasSrcRange a => HasSrcRange [a] where
  rangeOf as = do
    ne <- NonEmpty.nonEmpty as
    l <- rangeOf $ NonEmpty.head ne
    h <- rangeOf $ NonEmpty.last ne
    lens <- traverse rangeOf ne
    pure MkSrcRange
      { start = l.start
      , end = h.end
      , length = sum $ fmap (.length) lens
      }

instance HasSrcRange (CsnCluster_ a) where
  rangeOf cluster = rangeOf cluster.payload

instance HasSrcRange (ConcreteSyntaxNode_ a) where
  rangeOf csn = csn.range

rangeOfNode :: ToConcreteNodes t a => a -> Maybe SrcRange
rangeOfNode a = case runExcept $ toNodes a of
  Left _ -> Nothing
  Right e -> rangeOf e

