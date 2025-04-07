{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module L4.Annotation where

import Base
import qualified Base.Text as Text
import L4.Parser.SrcSpan

import qualified Control.Monad.Extra as Extra
import Data.Default
import qualified Data.List.NonEmpty as NonEmpty
import qualified GHC.Generics as GHC
import GHC.Stack
import Generics.SOP as SOP
import Generics.SOP.Constraint
import Generics.SOP.NP
import Generics.SOP.NS
import Optics.Generic
import Optics.Operators

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
  deriving anyclass (SOP.Generic, ToExpr, NFData)

-- | A Concrete Syntax Node (CSN) cluster is a 'ConcreteSyntaxNode_' for tokens
-- in the language paired with any trailing information that is not part of
-- the language grammar itself.
data CsnCluster_ t = CsnCluster
  { payload :: ConcreteSyntaxNode_ t
  , trailing :: ConcreteSyntaxNode_ t
  }
  deriving stock (Show, Ord, Eq, GHC.Generic)
  deriving anyclass (SOP.Generic, ToExpr, NFData)

data AnnoElement_ t
  = AnnoHole (Maybe SrcRange)
  | AnnoCsn  (Maybe SrcRange) (CsnCluster_ t)
  deriving stock (Show, Ord, Eq, GHC.Generic)
  deriving anyclass (SOP.Generic, ToExpr, NFData)

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
  { extra   :: e
  , range   :: Maybe SrcRange
  , payload :: [AnnoElement_ t]
  }
  deriving stock (Show, Ord, Eq, GHC.Generic)
  deriving anyclass (SOP.Generic, ToExpr, NFData)

annoExtra :: Lens' (Anno_ t e) e
annoExtra = #extra

allClusterTokens :: CsnCluster_ t -> [t]
allClusterTokens cluster = cluster.payload.tokens <> cluster.trailing.tokens

mkAnno :: Default e => [AnnoElement_ t] -> Anno_ t e
mkAnno es = fixAnnoSrcRange $ Anno def Nothing es

emptyAnno :: Default e => Anno_ t e
emptyAnno = mkAnno []

instance Default e => Default (Anno_ t e) where
  def = emptyAnno

isEmptyAnno :: Anno_ t e -> Bool
isEmptyAnno m = null m.payload

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

class (Default (AnnoExtra t)) => HasAnno t where
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

instance Default e => HasAnno (Anno_ t e) where
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

toNodesEither :: ToConcreteNodes t a => a -> Either TraverseAnnoError [CsnCluster_ t]
toNodesEither = runExcept . toNodes

class ToConcreteNodes t a | a -> t where
  toNodes :: a -> Except TraverseAnnoError [CsnCluster_ t]

  default toNodes ::
       (SOP.Generic a, All (AnnoFirst a (ToConcreteNodes t)) (Code a), AnnoToken a ~ t)
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
flattenConcreteNodes (Anno _ _ csns) = go csns
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

class HasTrailingSrcRange a where
  rangeOfTrailing :: a -> Maybe SrcRange

  -- default rangeOfTrailing :: HasAnno a => a -> Maybe SrcRange
  -- rangeOfTrailing a = rangeOfTrailing $ getAnno a

instance HasSrcRange a => HasSrcRange [a] where
  rangeOf as = do
    let
      rs = mapMaybe rangeOf as

    rs' <- NonEmpty.nonEmpty rs
    let
      h = NonEmpty.head rs'
      l = NonEmpty.last rs'

    pure $ MkSrcRange
      { start = h.start
      , end = l.end
      , length = sum $ fmap (.length) rs'
      , moduleUri = l.moduleUri
      }

instance HasTrailingSrcRange a => HasTrailingSrcRange [a] where
  rangeOfTrailing as = do
    let
      rs = mapMaybe rangeOfTrailing as

    rs' <- NonEmpty.nonEmpty rs
    let
      l = NonEmpty.last rs'

    pure l

instance HasTrailingSrcRange a => HasTrailingSrcRange (Maybe a) where
  rangeOfTrailing = (>>= rangeOfTrailing)

instance HasSrcRange (CsnCluster_ a) where
  rangeOf cluster = rangeOf cluster.payload

instance HasTrailingSrcRange (CsnCluster_ a) where
  rangeOfTrailing cluster = rangeOf cluster.trailing

instance HasSrcRange (ConcreteSyntaxNode_ a) where
  rangeOf csn = csn.range

instance HasSrcRange (AnnoElement_ a) where
  rangeOf = rangeOfAnnoElement

instance HasSrcRange a => HasSrcRange (Maybe a) where
  rangeOf a = a >>= rangeOf

instance HasSrcRange (Anno_ t e) where
  rangeOf a =
    let
      -- Only take the 'SrcRange' of elements into account that are visible
      applicable =
        filter
          (\e ->
              maybe
                True
                (isCsnClusterVisible . snd)
                (preview #_AnnoCsn e)
          )
          a.payload
    in
      rangeOf applicable

isCsnClusterVisible :: CsnCluster_ t -> Bool
isCsnClusterVisible csn = csn.payload.visibility == Visible

debugShow :: AnnoElement_ t -> String
debugShow = \case
  AnnoHole r -> "AnnoHole [" <> show r <> "]"
  AnnoCsn r p  -> "AnnoCsn [" <> show r <> "]: " <> show (p.payload.visibility, p.trailing.visibility, p.payload.range)

debugShowL :: [AnnoElement_ t] -> String
debugShowL = intercalate "\n" . fmap debugShow

-- | Calculate the 'SrcRange' of the AST node including hidden concrete syntax nodes.
--
-- This is notably different to @'rangeOf' (ann :: 'Anno_' t)@, which
-- filters out any hidden concrete syntax nodes.
rangeOfNode :: ToConcreteNodes t a => a -> Maybe SrcRange
rangeOfNode a = case runExcept $ toNodes a of
  Left _ -> Nothing
  Right e -> rangeOf e

-- ----------------------------------------------------------------------------
-- Annotation Instances
-- ----------------------------------------------------------------------------

instance Default e => Semigroup (Anno_ t e) where
  (Anno _e1 _r1 m1) <> (Anno _e2 _r2 m2) = Anno def Nothing (m1 <> m2)
