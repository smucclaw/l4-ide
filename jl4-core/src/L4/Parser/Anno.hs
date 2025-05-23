{-# LANGUAGE DataKinds #-}
module L4.Parser.Anno (
  -- * Parser with intermediate annotations
  AnnoParser_ (..),
  wrapAnnoParser,
  unwrapAnnoParser,
  -- * Source Annotations
  WithAnno_(..),
  attachAnno,
  attachEpa,
  annoEpa,
  annoHole,
  annoLexeme,
  annoLexeme_,

  -- * Utilities for source annotations
  inlineAnnoHole,
  mkHoleAnnoFor,

  -- * Megaparsec
  tryParser,
  -- * Exact print annotations
  Epa_(..),
  mkSimpleEpaAnno,
  tokenToEpa,
  -- * Exact printing annotations to Anno
  epaToHiddenCluster,
  epaToCluster,
  -- * Lexemes
  Lexeme_(..),
  mkLexeme,
  lexToEpa',
  lexToEpa,
  lexesToEpa,
  lexesToEpa',
) where

import Control.Applicative
import Control.Monad (MonadPlus)
import Data.Coerce (coerce)
import qualified Data.Foldable as Foldable
import Data.Functor (void)
import Data.Functor.Compose
import qualified Data.List.NonEmpty as NonEmpty
import GHC.Records
import Optics
import L4.Annotation
import L4.Parser.SrcSpan
import Text.Megaparsec (MonadParsec(..))


newtype AnnoParser_ f t a = AnnoParser {runAnnoParser :: Compose f (WithAnno_ t) a}
  deriving newtype (Functor, Applicative, Alternative)

instance (MonadPlus p) => MonadPlus (AnnoParser_ p t)

instance (Monad f) => Monad (AnnoParser_ f t) where
  ma >>= mab = wrapAnnoParser $ do
    wa <- unwrapAnnoParser ma
    wb <- unwrapAnnoParser $ mab wa.payload
    pure $ wb{anno = wa.anno <> wb.anno}

wrapAnnoParser :: f (WithAnno_ t a) -> AnnoParser_ f t a
wrapAnnoParser = coerce -- AnnoParser . Compose

unwrapAnnoParser :: AnnoParser_ f t a -> f (WithAnno_ t a)
unwrapAnnoParser = coerce -- getCompose . (.runAnnoParser)

-- ----------------------------------------------------------------------------
-- MegaParsec specific
-- ----------------------------------------------------------------------------

tryParser ::
  (MonadParsec err s p) =>
  AnnoParser_ p t a ->
  AnnoParser_ p t a
tryParser = wrapAnnoParser . try . unwrapAnnoParser


-- ----------------------------------------------------------------------------
-- Construction Helpers
-- ----------------------------------------------------------------------------

mkConcreteSyntaxNode :: (HasField "range" a SrcRange) => [a] -> ConcreteSyntaxNode_ a
mkConcreteSyntaxNode posTokens =
  ConcreteSyntaxNode
    { tokens = posTokens
    , range = do
        ne <- NonEmpty.nonEmpty posTokens
        let l = NonEmpty.head ne
            h = NonEmpty.last ne

        pure MkSrcRange
          { start = l.range.start
          , end = h.range.end
          , length = sum $ fmap (.range.length) ne
          , moduleUri = l.range.moduleUri
          }
    , visibility =
        if Foldable.null posTokens
          then Hidden
          else Visible
    }

mkHiddenConcreteSyntaxNode :: (HasField "range" a SrcRange) => [a] -> ConcreteSyntaxNode_ a
mkHiddenConcreteSyntaxNode ps = mkConcreteSyntaxNode ps
  & #visibility .~ Hidden

-- ----------------------------------------------------------------------------
-- Annotation Combinators
-- ----------------------------------------------------------------------------

data WithAnno_ t a = WithAnno
  { anno :: [AnnoElement_ t]
  , payload :: a
  }
  deriving stock (Functor, Show)

withHoleAnno :: (HasSrcRange a) => a -> WithAnno_ t a
withHoleAnno a = WithAnno [mkHoleWithSrcRange a] a

withEpaAnno :: (HasField "range" t SrcRange) => Epa_ t a -> WithAnno_ t a
withEpaAnno p = WithAnno (fmap mkCluster $ epaToCluster p : p.hiddenClusters) p.payload

epaToCluster :: (HasField "range" t SrcRange) => Epa_ t a -> CsnCluster_ t
epaToCluster p = CsnCluster
  { payload = mkConcreteSyntaxNode p.original
  , trailing = mkHiddenConcreteSyntaxNode p.trailingTokens
  }

epaToHiddenCluster :: (HasField "range" t SrcRange) => Epa_ t a -> CsnCluster_ t
epaToHiddenCluster p = CsnCluster
  { payload =  mkHiddenConcreteSyntaxNode p.original
  , trailing = mkHiddenConcreteSyntaxNode p.trailingTokens
  }

annoHole :: (Functor f, HasSrcRange a) => f a -> AnnoParser_ f t a
annoHole p = wrapAnnoParser $ fmap withHoleAnno p

annoEpa :: (HasField "range" t SrcRange, Functor f) => f (Epa_ t a) -> AnnoParser_ f t a
annoEpa p = wrapAnnoParser $ fmap withEpaAnno p

annoLexeme :: (HasField "range" t SrcRange, Functor f) => f (Lexeme_ t t) -> AnnoParser_ f t t
annoLexeme = annoEpa . fmap lexToEpa

annoLexeme_ :: (HasField "range" t SrcRange, Functor f) => f (Lexeme_ t a) -> AnnoParser_ f t ()
annoLexeme_ = void . annoLexemes . fmap (fmap (const []))

annoLexemes :: (HasField "range" t SrcRange, Functor f) => f (Lexeme_ t [t]) -> AnnoParser_ f t [t]
annoLexemes = annoEpa . fmap lexesToEpa

instance Applicative (WithAnno_ t) where
  pure a = WithAnno [] a
  WithAnno ps f <*> WithAnno ps2 x = WithAnno (ps <> ps2) (f x)

attachAnno :: (HasAnno a, AnnoToken a ~ t, AnnoExtra a ~ e, Functor f) => AnnoParser_ f t a -> f a
attachAnno p = fmap (\(WithAnno ann e) -> setAnno (mkAnno ann) e) $ unwrapAnnoParser p

attachEpa :: (HasAnno e, AnnoToken e ~ t, HasField "range" t SrcRange, Functor f) => f (Epa_ t e) -> f e
attachEpa =
  attachAnno . annoEpa

-- | Replace the first occurrence of 'AnnoHole' with the exactprint annotations.
-- Removes an indirection in the Annotation tree.
inlineAnnoHole :: (HasAnno a, AnnoToken a ~ t, AnnoExtra a ~ e, Functor f) => AnnoParser_ f t a -> f a
inlineAnnoHole p = (\ (WithAnno ann e) -> setAnno (mkAnno (inlineFirstAnnoHole ann (getAnno e).payload)) e) <$> unwrapAnnoParser p
  where
    inlineFirstAnnoHole []                 _   = []
    inlineFirstAnnoHole (AnnoHole _ : as1) as2 = as2 ++ as1
    inlineFirstAnnoHole (a : as1)          as2 = a : inlineFirstAnnoHole as1 as2

-- | Create an annotation hole with a source range hint.
-- This source range hint is used to compute the final source range
-- of the produced 'Anno_'.
mkHoleAnnoFor :: (HasSrcRange a, Monoid e) => a -> Anno_ t e
mkHoleAnnoFor a =
  mkAnno [mkHoleWithSrcRange a]

mkSimpleEpaAnno :: (HasField "range" t SrcRange, Monoid e) => Epa_ t a -> Anno_ t e
mkSimpleEpaAnno =
  mkAnno . (.anno) . withEpaAnno

data Lexeme_ t a = Lexeme
  { trailingTokens :: [t]
  , payload :: a
  , hiddenClusters :: [CsnCluster_ t]
  -- ^ A hidden cluster is something that is structured but not part
  -- of the abstract syntax tree. Think comments, which often need to be processed later
  -- or highlighted in a specific way.
  -- Having comment tokens unstructured as part of 'trailingTokens' can be quite
  -- tricky later to manage.
  }
  deriving stock Show
  deriving (Functor)

mkLexeme :: [t] -> a -> Lexeme_ t a
mkLexeme trail a = Lexeme
  { trailingTokens = trail
  , payload = a
  , hiddenClusters = []
  }

-- | 'Epa_' stands for _E_xact_p_rint _a_nnotation
data Epa_ t a = Epa
  { original :: [t]
  , trailingTokens :: [t]
  , payload :: a
  , hiddenClusters :: [CsnCluster_ t]
  -- ^ A hidden cluster is something that is structured but not part
  -- of the abstract syntax tree. Think comments, which often need to be processed later
  -- or highlighted in a specific way.
  -- Having comment tokens unstructured as part of 'trailingTokens' can be quite
  -- tricky later to manage.
  }
  deriving stock Show
  deriving (Functor)

tokenToEpa :: t -> Epa_ t t
tokenToEpa t = Epa
  { original = [t]
  , payload = t
  , trailingTokens = []
  , hiddenClusters = []
  }

lexesToEpa :: Lexeme_ t [t] -> Epa_ t [t]
lexesToEpa l = Epa
  { original = l.payload
  , trailingTokens = l.trailingTokens
  , payload = l.payload
  , hiddenClusters = l.hiddenClusters
  }

lexesToEpa' :: Lexeme_ t ([t], a) -> Epa_ t a
lexesToEpa' l = Epa
  { original = fst l.payload
  , trailingTokens = l.trailingTokens
  , payload = snd l.payload
  , hiddenClusters = l.hiddenClusters
  }

lexToEpa :: Lexeme_ t t -> Epa_ t t
lexToEpa l = Epa
  { original = [l.payload]
  , trailingTokens = l.trailingTokens
  , payload = l.payload
  , hiddenClusters = l.hiddenClusters
  }

lexToEpa' :: Lexeme_ t (t, a) -> Epa_ t a
lexToEpa' l = Epa
  { original = [fst l.payload]
  , trailingTokens = l.trailingTokens
  , payload = snd l.payload
  , hiddenClusters = l.hiddenClusters
  }
