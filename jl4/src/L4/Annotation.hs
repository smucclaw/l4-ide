{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module L4.Annotation where

import Base
import L4.Lexer ( SrcRange (..) )

import Control.DeepSeq (NFData)
import qualified Control.Monad.Extra as Extra
import qualified Data.Functor.Compose as Compose
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import Data.TreeDiff.Class (ToExpr)
import qualified GHC.Generics as GHC
import GHC.Stack
import Generics.SOP as SOP
import Generics.SOP.Constraint
import Generics.SOP.NP
import Generics.SOP.NS
import Optics
import GHC.Records (HasField)

data NodeVisibility
  = -- | A token cluster that is hidden because it was inserted by some tool.
    -- The user did not write it.
    Hidden
  | -- | A token written by the user.
    Visible
  deriving stock (Show, Ord, Eq, Enum, Bounded, GHC.Generic)
  deriving anyclass (ToExpr, NFData)

-- | A 'ConcreteSyntaxNode_' is a collection of tokens that semantically
-- belong together to form a single node in the concrete syntax of your language.
-- It contains meta-information such as 'NodeVisibility' and a 'SrcRange'.
-- The 'SrcRange' may be 'Nothing', if the 'tokens' are empty ('[]').
-- This can be sometimes convenient to express.
data ConcreteSyntaxNode_ t = ConcreteSyntaxNode
  { tokens :: [t]
  , range :: Maybe SrcRange
  , visibility :: NodeVisibility
  }
  deriving stock (Show, Ord, Eq, GHC.Generic)
  deriving anyclass (ToExpr, NFData)

-- | A Concrete Syntax Node (CSN) cluster is a 'ConcreteSyntaxNode' for tokens
-- in the language paired with any trailing information that is not part of
-- the language grammar itself.
data CsnCluster_ t = CsnCluster
  { payload :: ConcreteSyntaxNode_ t
  , trailing :: ConcreteSyntaxNode_ t
  }
  deriving stock (Show, Ord, Eq, GHC.Generic)
  deriving anyclass (ToExpr, NFData)

data AnnoElement_ t
  = AnnoHole (Maybe SrcRange)
  | AnnoCsn  (Maybe SrcRange) (CsnCluster_ t)
  deriving stock (Show, Ord, Eq, GHC.Generic)
  deriving anyclass (ToExpr, NFData)

rangeOfAnnoElement :: AnnoElement_ t -> Maybe SrcRange
rangeOfAnnoElement = \case
  AnnoHole srcRange -> srcRange
  AnnoCsn srcRange _ -> srcRange

mkHoleWithSrcRangeHint :: Maybe SrcRange -> AnnoElement_ t
mkHoleWithSrcRangeHint = AnnoHole

mkHoleWithSrcRange :: HasSrcRange a => a -> AnnoElement_ t
mkHoleWithSrcRange a = mkHoleWithSrcRangeHint (rangeOf a)

mkCluster :: CsnCluster_ t -> AnnoElement_ t
mkCluster csn = AnnoCsn (rangeOf csn) csn

data Anno_ t e = Anno
  { extra   :: Maybe e
  , range   :: Maybe SrcRange
  , payload :: [AnnoElement_ t]
  }
  deriving stock (Show, Ord, Eq, GHC.Generic)
  deriving anyclass (ToExpr, NFData)

allClusterTokens :: CsnCluster_ t -> [t]
allClusterTokens cluster = cluster.payload.tokens <> cluster.trailing.tokens

mkAnno :: [AnnoElement_ t] -> Anno_ t e
mkAnno es = fixAnnoSrcRange $ Anno Nothing Nothing es

emptyAnno :: Anno_ t e
emptyAnno = mkAnno []

isEmptyAnno :: Anno_ t e -> Bool
isEmptyAnno m = List.null m.payload

-- | Calculate the actual 'Maybe SrcRange' of this source annotation.
--
-- We assume that the child 'AnnoElement_'s contain hints for their respective
-- 'SrcRange's. Usually, that should be the case, if the 'AnnoElement_'s have
-- been constructed with the smart constructors 'mkHoleWithSrcRangeHint',
-- 'mkHoleWithSrcRange' and 'mkCluster'.
--
-- The result may be 'Nothing', if the 'Anno_' is empty (i.e. 'emptyAnno'),
-- or none of the child elements 'AnnoElement_' contain any cached 'SrcRange's.
--
computeAnnoSrcRange :: Anno_ t e -> Maybe SrcRange
computeAnnoSrcRange ann = rangeOf ann.payload

-- | Calls 'computeAnnoSrcRange' and sets the @Maybe 'SrcRange'@ for this 'Anno_'
fixAnnoSrcRange :: Anno_ t e -> Anno_ t e
fixAnnoSrcRange ann = set #range (computeAnnoSrcRange ann) ann

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
-- Source Range manipulation
-- ----------------------------------------------------------------------------

data TraverseAnnoError
  = InsufficientHoleFit CallStack
  deriving (Show)

prettyTraverseAnnoError :: TraverseAnnoError -> Text
prettyTraverseAnnoError (InsufficientHoleFit cs) = "HoleFit requested but not enough given at: " <> Text.pack (prettyCallStack cs)

class ToConcreteNodes t a | a -> t where
  toNodes :: a -> Except TraverseAnnoError [CsnCluster_ t]

  default toNodes ::
       (SOP.Generic a, All (AnnoFirst a (ToConcreteNodes t)) (Code a), AnnoToken a ~ t, AnnoExtra a ~ e)
    => a -> Except TraverseAnnoError [CsnCluster_ t]
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

instance ToConcreteNodes t a => ToConcreteNodes t [a] where
  toNodes =
    Extra.concatMapM toNodes

instance ToConcreteNodes t a => ToConcreteNodes t (Maybe a) where
  toNodes =
    maybe (pure []) toNodes

flattenConcreteNodes :: (HasCallStack, MonadError TraverseAnnoError m) => Anno_ t e -> [m [CsnCluster_ t]] -> m [CsnCluster_ t]
flattenConcreteNodes (Anno _ _ csns) =go csns
  where
    go []                 _        = pure []
    go (AnnoHole _ : cs)  holeFits =
      case holeFits of
        [] -> throwError $ InsufficientHoleFit callStack
        (x : xs) -> (<>) <$> x <*> go cs xs
    go (AnnoCsn _ m : cs) holeFits =
      (m :) <$> go cs holeFits

-- ----------------------------------------------------------------------------
-- Source Range manipulation
-- ----------------------------------------------------------------------------

class HasSrcRange a where
  rangeOf :: a -> Maybe SrcRange

  default rangeOf :: HasAnno a => a -> Maybe SrcRange
  rangeOf a = rangeOf $ getAnno a

instance HasSrcRange a => HasSrcRange [a] where
  rangeOf as = do
    let
      rs = Maybe.mapMaybe rangeOf as

    rs' <- NonEmpty.nonEmpty rs
    let
      h = NonEmpty.head rs'
      l = NonEmpty.last rs'

    pure $ MkSrcRange
      { start = h.start
      , end = l.end
      , length = sum $ fmap (.length) rs'
      }

instance HasSrcRange (CsnCluster_ a) where
  rangeOf cluster = rangeOf [cluster.payload, cluster.trailing]

instance HasSrcRange (ConcreteSyntaxNode_ a) where
  rangeOf csn = csn.range

instance HasSrcRange (AnnoElement_ a) where
  rangeOf = rangeOfAnnoElement

instance HasSrcRange a => HasSrcRange (Maybe a) where
  rangeOf a = a >>= rangeOf

instance HasSrcRange (Anno_ e t) where
  rangeOf a = rangeOf a.payload

rangeOfNode :: ToConcreteNodes t a => a -> Maybe SrcRange
rangeOfNode a = case runExcept $ toNodes a of
  Left _ -> Nothing
  Right e -> rangeOf e

-- ----------------------------------------------------------------------------
-- Annotation Instances
-- ----------------------------------------------------------------------------

instance Semigroup (Anno_ t e) where
  (Anno _e1 _r1 m1) <> (Anno _e2 _r2 m2) = Anno Nothing Nothing (m1 <> m2)

-- ----------------------------------------------------------------------------
-- Annotation Combinators
-- ----------------------------------------------------------------------------

data WithAnno_ t e a = WithAnno (Anno_ t e) a
  deriving stock Show
  deriving (Functor)

withHoleAnno :: HasSrcRange a => a -> WithAnno_ t e a
withHoleAnno a = WithAnno (mkHoleAnnoFor a) a

withEpaAnno :: (HasField "range" t SrcRange) => Epa_ t a -> WithAnno_ t e a
withEpaAnno (Epa this trailing e) = WithAnno (mkAnno [mkCluster cluster]) e
 where
  cluster =
    CsnCluster
      { payload = mkConcreteSyntaxNode this
      , trailing = mkConcreteSyntaxNode trailing
      }

unWithAnno :: WithAnno_ t e a -> a
unWithAnno (WithAnno _ a) = a

toAnno :: WithAnno_ t e a -> Anno_ t e
toAnno (WithAnno ann _) = ann

annoHole :: (Functor f, HasSrcRange a) => f a -> Compose.Compose f (WithAnno_ t e) a
annoHole p = Compose.Compose $ fmap withHoleAnno p

annoEpa :: (Functor f, HasField "range" t SrcRange) => f (Epa_ t a) -> Compose.Compose f (WithAnno_ t e) a
annoEpa p = Compose.Compose $ fmap withEpaAnno p

annoLexeme :: (Functor f, HasField "range" t SrcRange) => f (Lexeme_ t t) -> Compose.Compose f (WithAnno_ t e) t
annoLexeme = annoEpa . fmap lexToEpa

annoLexemes :: (Functor f, HasField "range" t SrcRange) => f (Lexeme_ t [t]) -> Compose.Compose f (WithAnno_ t e) [t]
annoLexemes = annoEpa . fmap lexesToEpa

instance Applicative (WithAnno_ t e) where
  pure a = WithAnno emptyAnno a
  WithAnno ps f <*> WithAnno ps2 x = WithAnno (ps <> ps2) (f x)

attachAnno :: (Functor f, HasAnno a, AnnoToken a ~ t, AnnoExtra a ~ e) => Compose.Compose f (WithAnno_ t e) a -> f a
attachAnno p = fmap (\(WithAnno ann e) -> setAnno (fixAnnoSrcRange ann) e) $ Compose.getCompose p

attachEpa :: (Functor f, HasAnno e, AnnoToken e ~ t, HasField "range" t SrcRange) =>
  f (Epa_ t e) ->
  f e
attachEpa =
  attachAnno . annoEpa

-- | Replace the first occurrence of 'AnnoHole' with the exactprint annotations.
-- Removes an indirection in the Annotation tree.
inlineAnnoHole :: (Functor f, HasAnno a, AnnoToken a ~ t, AnnoExtra a ~ e) => Compose.Compose f (WithAnno_ t e) a -> f a
inlineAnnoHole p = (\ (WithAnno ann e) -> setAnno (mkAnno (inlineFirstAnnoHole ann.payload (getAnno e).payload)) e) <$> Compose.getCompose p
  where
    inlineFirstAnnoHole []                 _   = []
    inlineFirstAnnoHole (AnnoHole _ : as1) as2 = as2 ++ as1
    inlineFirstAnnoHole (a : as1)          as2 = a : inlineFirstAnnoHole as1 as2

-- | Create an annotation hole with a source range hint.
-- This source range hint is used to compute the final source range
-- of the produced 'Anno_'.
mkHoleAnnoFor :: HasSrcRange a => a -> Anno_ t e
mkHoleAnnoFor a =
  mkAnno [mkHoleWithSrcRange a]

mkSimpleEpaAnno :: (HasField "range" t SrcRange) => Epa_ t a -> Anno_ t e
mkSimpleEpaAnno =
  toAnno . withEpaAnno

data Lexeme_ t a = Lexeme [t] a
  deriving stock Show
  deriving (Functor)

unLexeme :: Lexeme_ t a -> a
unLexeme (Lexeme _ a) = a

mkLexeme :: [t] -> Lexeme_ t [t]
mkLexeme ts = Lexeme ts ts

data Epa_ t a = Epa [t] [t] a
  deriving stock Show
  deriving (Functor)

unEpa :: Epa_ t a -> a
unEpa (Epa _ _ a) = a

lexesToEpa :: Lexeme_ t [t] -> Epa_ t [t]
lexesToEpa (Lexeme trailing this) = Epa this trailing this

lexesToEpa' :: Lexeme_ t ([t], a) -> Epa_ t a
lexesToEpa' (Lexeme trailing (this, a)) = Epa this trailing a

lexToEpa :: Lexeme_ t t -> Epa_ t t
lexToEpa (Lexeme trailing this) = Epa [this] trailing this

lexToEpa' :: Lexeme_ t (t, a) -> Epa_ t a
lexToEpa' (Lexeme trailing (this, a)) = Epa [this] trailing a

-- ----------------------------------------------------------------------------
-- Concrete Syntax Node helpers
-- ----------------------------------------------------------------------------

mkConcreteSyntaxNode :: (HasField "range" a SrcRange) => [a] -> ConcreteSyntaxNode_ a
mkConcreteSyntaxNode posTokens =
  ConcreteSyntaxNode
    { tokens = posTokens
    , range = do
        ne <- NonEmpty.nonEmpty posTokens
        let l = NonEmpty.last ne
            h = NonEmpty.last ne

        pure MkSrcRange
          { start = l.range.start
          , end = h.range.end
          , length = sum $ fmap (.range.length) ne
          }
    , visibility =
        if null posTokens
          then Hidden
          else Visible
    }

mkHiddenCsnCluster :: (HasField "range" a SrcRange) => CsnCluster_ a
mkHiddenCsnCluster =
  CsnCluster
    { payload = mkConcreteSyntaxNode []
    , trailing = mkConcreteSyntaxNode []
    }
