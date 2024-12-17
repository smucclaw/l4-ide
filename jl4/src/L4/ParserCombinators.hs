module L4.ParserCombinators where
import Text.Megaparsec (Parsec)
import Control.Applicative ((<|>), many)
import Text.Megaparsec.Stream

-- ----------------------------------------------------------------------------
-- Parser Utilities
-- ----------------------------------------------------------------------------

between :: Applicative f => f open -> f close -> f b -> f (open, b, close)
between open close p =
  (,,)
    <$> open
    <*> p
    <*> close

-- | @'sepByP' p sep@ parses /zero/ or more occurrences of @p@, separated by
-- @sep@. Returns a list of values returned by @p@.
--
-- > commaSep p = p `sepBy` comma
sepBy :: (Stream s, Ord e) => Parsec e s a -> Parsec e s sep -> Parsec e s ([a], [sep])
sepBy p sep = sepBy1 p sep <|> pure ([], [])
{-# INLINE sepBy #-}

-- | @'sepBy1' p sep@ parses /one/ or more occurrences of @p@, separated by
-- @sep@. Returns a list of values returned by @p@.
sepBy1 :: (Stream s, Ord e) => Parsec e s a -> Parsec e s sep -> Parsec e s ([a], [sep])
sepBy1 p sep = do
  a <- p
  sepsAndA <- many ((,) <$> sep <*> p)
  let (seps, as) = unzip sepsAndA
  pure (a : as, seps)

-- | @'sepEndBy' p sep@ parses /zero/ or more occurrences of @p@, separated
-- and optionally ended by @sep@. Returns a list of values returned by @p@.
sepEndBy :: (Stream s, Ord e) => Parsec e s a -> Parsec e s sep -> Parsec e s ([a], [sep])
sepEndBy p sep = sepEndBy1 p sep <|> pure ([], [])
{-# INLINE sepEndBy #-}

-- | @'sepEndBy1' p sep@ parses /one/ or more occurrences of @p@, separated
-- and optionally ended by @sep@. Returns a list of values returned by @p@.
sepEndBy1 :: (Stream s, Ord e) => Parsec e s a -> Parsec e s sep -> Parsec e s ([a], [sep])
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

