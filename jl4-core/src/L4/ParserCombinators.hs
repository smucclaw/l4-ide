module L4.ParserCombinators where

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
sepBy :: (Alternative m) => m a -> m sep -> m ([a], [sep])
sepBy p sep = sepBy1 p sep <|> pure ([], [])
{-# INLINE sepBy #-}

-- | @'sepBy1' p sep@ parses /one/ or more occurrences of @p@, separated by
-- @sep@. Returns a list of values returned by @p@ and a list of separators
-- parsed by @sep@.
sepBy1 :: (Alternative m) => m a -> m sep -> m ([a], [sep])
sepBy1 p sep = go <$> p <*> many ((,) <$> sep <*> p)
 where
  go a sepsAndA =
    let
      (seps, as) = unzip sepsAndA
    in
      (a : as, seps)

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

