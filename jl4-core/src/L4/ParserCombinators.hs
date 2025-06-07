module L4.ParserCombinators
  (between,
  sepBy,
  sepBy1,
  sepEndBy,
  sepEndBy1,
  -- * How to process sepBy* family
  SepBy,
  SepBy1,
  toListSepBy,
  sepByElems,
  sepBySeps,
  zipSepBy,
  toListSepBy1,
  sepBy1Elems,
  sepBy1Seps,
  zipSepBy1,
  ) where

import Control.Applicative ((<|>), many, Alternative)
import Control.Monad (MonadPlus)

-- ----------------------------------------------------------------------------
-- Parser Utilities
-- ----------------------------------------------------------------------------

-- | @'between' open close p@ parses @open@, followed by @p@ and @close@.
-- Returns the parsed values of @open@, @p@ and @close@.
between :: (Applicative f) => f open -> f close -> f b -> f (open, b, close)
between open close p =
  (,,)
    <$> open
    <*> p
    <*> close

-- | @'sepByP' p sep@ parses /zero/ or more occurrences of @p@, separated by
-- @sep@. Returns a list of values returned by @p@ and a list of separators
-- parsed by @sep@.
--
-- > commaSep p = p `sepBy` comma
sepBy :: (Alternative m) => m a -> m sep -> m (SepBy a sep)
sepBy p sep = fmap (SepBy . Just) (sepBy1 p sep) <|> pure (SepBy Nothing)
{-# INLINE sepBy #-}

-- | @'sepBy1' p sep@ parses /one/ or more occurrences of @p@, separated by
-- @sep@. Returns a list of values returned by @p@ and a list of separators
-- parsed by @sep@.
sepBy1 :: (Alternative m) => m a -> m sep -> m (SepBy1 a sep)
sepBy1 p sep = go <$> p <*> many ((,) <$> sep <*> p)
 where
  go a sepsAndA =
    SepBy1 a sepsAndA

-- | @'sepEndBy' p sep@ parses /zero/ or more occurrences of @p@, separated
-- and optionally ended by @sep@. Returns a list of values returned by @p@ and a
-- list of separators parsed by @sep@.
sepEndBy :: (MonadPlus m) => m a -> m sep -> m ([a], [sep])
sepEndBy p sep = sepEndBy1 p sep <|> pure ([], [])
{-# INLINE sepEndBy #-}

-- | @'sepEndBy1' p sep@ parses /one/ or more occurrences of @p@, separated
-- and optionally ended by @sep@. Returns a list of values returned by @p@ and a
-- list of separators parsed by @sep@.
sepEndBy1 :: (MonadPlus m) => m a -> m sep -> m ([a], [sep])
sepEndBy1 p sep = do
  a <- p
  (as, seps) <-
    do
      s <- sep
      (as, seps) <- sepEndBy p sep
      pure (as, s : seps)
      <|> pure ([], [])
  pure (a : as, seps)
{-# INLINEABLE sepEndBy1 #-}

newtype SepBy a b = SepBy
  { getSepBy :: Maybe (SepBy1 a b)
  }
  deriving (Show, Eq, Ord)

toListSepBy :: (a -> c) -> (b -> c) -> SepBy a b -> [c]
toListSepBy fac fbc s = maybe [] (toListSepBy1 fac fbc) s.getSepBy

sepByElems :: SepBy a b -> [a]
sepByElems s = maybe [] sepBy1Elems s.getSepBy

sepBySeps :: SepBy a b -> [b]
sepBySeps s = maybe [] sepBy1Seps s.getSepBy

zipSepBy :: (a -> c) -> (a -> b -> c) -> SepBy a b -> [c]
zipSepBy l fabc s = case s.getSepBy of
  Nothing -> []
  Just s' -> zipSepBy1 l fabc s'

data SepBy1 a b = SepBy1
  { sepBy1Head :: a
  , sepBy1Tail :: [(b, a)]
  }
  deriving (Show, Eq, Ord)

toListSepBy1 :: (a -> c) -> (b -> c) -> SepBy1 a b -> [c]
toListSepBy1 fac fbc interleaved1 =
  fac interleaved1.sepBy1Head :
    concatMap
      ((\(b, a) -> [fbc b, fac a]))
      interleaved1.sepBy1Tail

sepBy1Elems :: SepBy1 a b -> [a]
sepBy1Elems s = s.sepBy1Head : fmap snd s.sepBy1Tail

sepBy1Seps :: SepBy1 a b -> [b]
sepBy1Seps s = fmap fst s.sepBy1Tail

zipSepBy1 :: (a -> c) -> (a -> b -> c) -> SepBy1 a b -> [c]
zipSepBy1 l fabc s = go s.sepBy1Head s.sepBy1Tail
  where
    go a [] = [l a]
    go a ((b, aNext):r) = fabc a b : go aNext r
