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

data NodeVisibility
  = -- | A token cluster that is hidden because it was inserted by some tool.
    -- The user did not write it.
    Hidden
  | -- | A token written by the user.
    Visible
  deriving stock (Show, Ord, Eq, Enum, Bounded, GHC.Generic)
  deriving anyclass (ToExpr, NFData)

data ConcreteSyntaxNode_ t = ConcreteSyntaxNode
  { tokens :: [t]
  , range :: Maybe SrcRange
  , visibility :: NodeVisibility
  }
  deriving stock (Show, Ord, Eq, GHC.Generic)
  deriving anyclass (ToExpr, NFData)

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

mkHoleWithType :: a -> AnnoElement_ t
mkHoleWithType _ = AnnoHole Nothing

mkHole :: AnnoElement_ t
mkHole = AnnoHole Nothing

mkHole' :: Maybe SrcRange -> AnnoElement_ t
mkHole' = AnnoHole

mkCsn :: CsnCluster_ t -> AnnoElement_ t
mkCsn csn = AnnoCsn (rangeOf csn) csn

data Anno_ t e = Anno
  { extra   :: Maybe e
  , range   :: Maybe SrcRange
  , payload :: [AnnoElement_ t]
  }
  deriving stock (Show, Ord, Eq, GHC.Generic)
  deriving anyclass (ToExpr, NFData)

csnTokens :: CsnCluster_ t -> [t]
csnTokens cluster = cluster.payload.tokens <> cluster.trailing.tokens

mkAnno :: [AnnoElement_ t] -> Anno_ t e
mkAnno es = Anno Nothing (rangeOf es) es

emptyAnno :: Anno_ t e
emptyAnno = mkAnno []

isEmptyAnno :: Anno_ t e -> Bool
isEmptyAnno m = List.null m.payload

-- | Calculate the actual src range of this source annotation.
--
computeAnnoSrcRange :: Anno_ t e -> Maybe SrcRange
computeAnnoSrcRange ann = do
  let
    csnRanges = Maybe.mapMaybe (\csn -> rangeOf csn) ann.payload

  ne <- NonEmpty.nonEmpty csnRanges
  let
    h = NonEmpty.head ne
    l = NonEmpty.last ne

  pure $ MkSrcRange
    { start = h.start
    , end = l.end
    , length = sum $ fmap (.length) ne
    }

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

data EPError
  = InsufficientHoleFit CallStack
  deriving (Show)

prettyEPError :: EPError -> Text
prettyEPError (InsufficientHoleFit cs) = "HoleFit requested but not enough given at: " <> Text.pack (prettyCallStack cs)

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

instance ToConcreteNodes t a => ToConcreteNodes t [a] where
  toNodes =
    Extra.concatMapM toNodes

instance ToConcreteNodes t a => ToConcreteNodes t (Maybe a) where
  toNodes =
    maybe (pure []) toNodes

flattenConcreteNodes :: (HasCallStack, MonadError EPError m) => Anno_ t e -> [m [CsnCluster_ t]] -> m [CsnCluster_ t]
flattenConcreteNodes (Anno _ _ csns) = go csns
  where
    go []               _        = pure []
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
      rngs = Maybe.mapMaybe (\csn -> rangeOf csn) as

    ne <- NonEmpty.nonEmpty rngs
    let
      h = NonEmpty.head ne
      l = NonEmpty.last ne

    pure $ MkSrcRange
      { start = h.start
      , end = l.end
      , length = sum $ fmap (.length) ne
      }

instance HasSrcRange (CsnCluster_ a) where
  rangeOf cluster = rangeOf cluster.payload

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
    -- where
    --   newRange = do
    --     ne <- NonEmpty.nonEmpty (m1 <> m2)
    --     l <- rangeOfAnnoElement $ NonEmpty.head ne
    --     h <- rangeOfAnnoElement $ NonEmpty.last ne
    --     ls <- traverse rangeOfAnnoElement ne
    --     pure MkSrcRange
    --       { start = l.start
    --       , end = h.end
    --       , length = sum $ fmap (.length) ls
    --       }
