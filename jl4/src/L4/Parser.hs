{-# LANGUAGE ScopedTypeVariables #-}
module L4.Parser (
  -- * Public API
  parseFile,
  execParser,
  program,
  PError(..),
  -- * Testing API
  parseTest,
) where

import Base

import Data.Functor.Compose
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Optics
import Text.Megaparsec hiding (parseTest)
import qualified Text.Megaparsec.Char.Lexer as Lexer
import Text.Pretty.Simple

import L4.Annotation
import L4.Lexer as L
import qualified L4.ParserCombinators as P
import L4.Syntax

type Parser = Parsec Void TokenStream

spaces :: Parser [PosToken]
spaces =
  takeWhileP (Just "space token") isSpaceToken

isSpaceToken :: PosToken -> Bool
isSpaceToken t =
  case t.payload of
    TSpace _        -> True
    TLineComment _  -> True
    TBlockComment _ -> True
    _               -> False

lexeme :: Parser a -> Parser (Lexeme a)
lexeme p = do
  a <- p
  trailingWs <- spaces
  pure $ Lexeme trailingWs a

plainToken :: TokenType -> Parser PosToken
plainToken tt =
  token
    (\ t -> if t.payload == tt then Just t else Nothing)
    (Set.singleton (Tokens (trivialToken tt :| [])))

trivialToken :: TokenType -> PosToken
trivialToken tt =
  MkPosToken trivialRange tt
  where
    trivialRange :: SrcRange
    trivialRange = MkSrcRange trivialPos trivialPos 0

    trivialPos :: SrcPos
    trivialPos = MkSrcPos "" 0 0

spacedToken_ :: TokenType -> Parser (Lexeme PosToken)
spacedToken_ tt =
  lexeme (plainToken tt)

spacedToken :: (TokenType -> Maybe a) -> String -> Parser (Epa a)
spacedToken cond lbl =
  lexToEpa' <$>
    lexeme
      (token
        (\ t -> (t,) <$> cond t.payload)
        Set.empty
      )
    <?> lbl

(<<$>>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
fab <<$>> fga = (fmap . fmap) fab fga

-- | A quoted identifier between backticks.
quotedName :: Parser (Epa Name)
quotedName =
  Name mempty <<$>> spacedToken (preview #_TQuoted) "quoted identifier"

simpleName :: Parser (Epa Name)
simpleName =
  Name mempty <<$>> spacedToken (preview #_TIdentifier) "identifier"

name :: Parser Name
name = attachEpa (quotedName <|> simpleName) <?> "identifier"

program :: Parser (Program Name)
program = do
  initSpace <- spaces
  let wsSpace = Lexeme [] initSpace
  MkProgram (mkSimpleEpaAnno (lexesToEpa wsSpace) <> mkHoleAnno) <$> many section

manyLines :: Parser a -> Parser [a]
manyLines p = do
  current <- Lexer.indentLevel
  many (withIndent EQ current (const p))

-- | Run the parser only when the indentation is correct and fail otherwise.
withIndent :: (TraversableStream s, MonadParsec e s m) => Ordering -> Pos -> (Pos -> m b) -> m b
withIndent ordering current p = do
  actual <- Lexer.indentLevel
  if compare actual current == ordering
    then do
      p actual
    else do
      fancyFailure . Set.singleton $
        ErrorIndentation ordering current actual

section :: Parser (Section Name)
section =
  attachAnno $
    MkSection emptyAnno
      <$> annoEpa sectionSymbols
      <*> annoHole name
      <*> annoHole (manyLines decl)

sectionSymbols :: Parser (Epa Int)
sectionSymbols = do
  paragraphSymbols <- lexeme (some (plainToken TParagraph))
  pure $ length <$> lexesToEpa paragraphSymbols

decl :: Parser (Decl Name)
decl =
      Declare mkHoleAnno <$> declare
  <|> Decide  mkHoleAnno <$> decide

declare :: Parser (Declare Name)
declare =
  attachAnno $
    MkDeclare emptyAnno
      <$  annoLexeme (spacedToken_ TKDeclare)
      <*> annoHole name
      <*> annoHole ofType

decide :: Parser (Decide Name)
decide = do
  sig <- typeSig
  current <- Lexer.indentLevel
  decideKW sig current <|> meansKW sig
  where
    decideKW sig current = do
      tkDecide <- spacedToken_ TKDecide
      clauses <- manyLines (clause current) -- I see room for ambiguity here
      attachAnno $
        MkDecide emptyAnno
          <$> annoHole (pure sig)
          <*  annoLexeme (pure tkDecide)
          <*> annoHole (pure clauses)

    -- We attach the concrete source node of 'TKMeans' to 'GuardedClause'.
    -- This is necessary as the current exact printing infrastructure
    -- doesn't allow interleaving of concrete syntax nodes and abstract syntax nodes.
    -- The problem is that 'Expr' is part of 'GuardedClause', but 'TKMeans'
    -- likely *should* be part of 'Decide'. Modelling it like this would require us to first
    -- print the first element of 'GuardedClause' then the concrete syntax node 'TKMeans' from
    -- 'Decide', followed by the rest of the concrete syntax nodes of 'GuardedClause'.
    meansKW sig = do
      e <- baseExpr
      tkMeans <- spacedToken_ TKMeans
      clauseGuard <- guard_
      attachAnno $
        MkDecide emptyAnno
          <$> annoHole (pure sig)
          <*> annoHole (
                List.singleton <$>
                  (attachAnno $
                    GuardedClause emptyAnno
                      <$> annoHole   (pure e)
                      <*  annoLexeme (pure tkMeans)
                      <*> annoHole   (pure clauseGuard)
                  )
          )

typeSig :: Parser (TypeSig Name)
typeSig =
  attachAnno $
    MkTypeSig emptyAnno
      <$> annoHole (option (MkGivenSig emptyAnno []) given)
      <*> annoHole (optional giveth)

given :: Parser (GivenSig Name)
given =
  attachAnno $
    MkGivenSig emptyAnno
      <$  annoLexeme (spacedToken_ TKGiven)
      <*> annoHole (manyLines typedName)

giveth :: Parser (GivethSig Name)
giveth =
  attachAnno $
    MkGivethSig emptyAnno
      <$  annoLexeme (spacedToken_ TKGiveth)
      <*> annoHole (typedName)

clause :: Pos -> Parser (Clause Name)
clause p =
  attachAnno $
    GuardedClause emptyAnno
      <$> annoHole (indentedExpr p)
      <*> annoHole guard_

-- primarily for testing
expr :: Parser (Expr Name)
expr =
  indentedExpr (mkPos 1)

guard_ :: Parser (Guard Name)
guard_ =
      otherwiseGuard
  <|> plainGuard

otherwiseGuard :: Parser (Guard n)
otherwiseGuard =
  attachAnno $
    Otherwise emptyAnno
      <$ annoLexeme (spacedToken_ TKOtherwise)

plainGuard :: Parser (Guard Name)
plainGuard = do
  current <- Lexer.indentLevel
  attachAnno $
    PlainGuard emptyAnno
      <$ annoLexeme (spacedToken_ TKIf)
      <*> annoHole (indentedExpr current)

ofType :: Parser (Type' Name)
ofType =
      isType
  <|> record

isType :: Parser (Type' Name)
isType =
  attachAnno $
    annoLexeme (spacedToken_ TKIs)
      *> (   namedType
         <|> enumType
         )

namedType :: Compose Parser WithAnno (Type' Name)
namedType =
  NamedType emptyAnno
    <$  annoLexeme (spacedToken_ TKA <|> spacedToken_ TKAn)
    <*> annoHole name

enumType :: Compose Parser WithAnno (Type' Name)
enumType =
  Enum emptyAnno
    <$  annoLexeme (spacedToken_ TKOne)
    <*  annoLexeme (spacedToken_ TKOf)
    <*> annoHole
            ( concat
                <$> manyLines
                  ( do
                      (names, commas) <- P.sepBy1 name (spacedToken_ TComma)
                      pure $ zipWithLeftovers names commas
                  )
            )
 where
  zipWithLeftovers :: [Name] -> [Lexeme PosToken] -> [Name]
  zipWithLeftovers ns [] = ns
  zipWithLeftovers [] _ = []
  zipWithLeftovers (n : ns) (c : cs) = setAnno (getAnno n <> mkSimpleEpaAnno (lexToEpa c)) n : zipWithLeftovers ns cs

record :: Parser (Type' Name)
record =
  attachAnno $
    Record emptyAnno
      <$ annoLexeme (spacedToken_ TKHas)
      <*> annoHole (manyLines typedName)

typedName :: Parser (TypedName Name)
typedName =
  attachAnno $
    MkTypedName emptyAnno
      <$> annoHole name
      <*> annoHole ofType

-- |
-- An expression is a base expression followed by
-- a sequence of expression continuations, which are
-- themselves a binary operator followed by an expression.
--
-- For each expression continuation, we store its indentation,
-- and then in a post-processing step, we determine the correct
-- order of precedence.
--
-- An indented expression must in principle occur all to
-- the right of the given column threshold.
--
indentedExpr :: Pos -> Parser (Expr Name)
indentedExpr p = do
  withIndent GT p $ \_ -> do
    e <- baseExpr
    efs <- many (expressionCont p)
    pure (combineExpr End e efs)

data Stack =
    Frame Stack (Expr Name) (Expr Name -> Expr Name -> Expr Name) Prio Pos
  | End

combineExpr :: Stack -> Expr Name -> [ExprCont] -> Expr Name
combineExpr End e [] = e
combineExpr (Frame s e1 op _ _) e2 [] =
  combineExpr s (e1 `op` e2) []
combineExpr End e1 (MkExprCont op1 prio1 p1 e2 : efs) =
  combineExpr (Frame End e1 op1 prio1 p1) e2 efs
combineExpr s1@(Frame s e1 op1 prio1 p1) e2 (MkExprCont op2 prio2 p2 e3 : efs)
  | p2 > p1 || p2 == p1 && prio2 >= prio1 = -- (>=) is for *right*-associative operators
  combineExpr (Frame s1 e2 op2 prio2 p2) e3 efs -- push
  | otherwise =
  combineExpr s (e1 `op1` e2) (MkExprCont op2 prio2 p2 e3 : efs) -- pop

-- The real problem is:
--
--          e1
--   OR     e2
--      AND e3
--
-- Why? Because "OR e" occurs sequentially, but we have to reorder it.
-- So we cannot use a representation for "continuations" which already
-- associates the operators and base expressions.
--
-- But can we really do anything until the very end?
--
-- In the situation above, the following could happen:
--
-- Nothing further is in the input.
-- Final output: e1 OR (e2 AND e3)
--
--          e1
--   OR     e2
--      AND e3
--
-- Hmmm, I guess what we really know is that "e1 OR" belong together!
-- We also know that "e2 AND" belong together. So really *this* should
-- be our stack.
--
-- [ (e1, OR@3), (e2, AND@6), e3 ]
--
-- Now let's say what follows is `OP@i e4`. What do we do?
--
-- If i > 6, say 8, then
--
-- [ (e1, OR@3), (e2, AND@6), (e3, OP@8), e4 ]
--
-- If i < 6, but > 3, say 4, then we know e2 and e3 belong together; we get
--
-- [ (e1, OR@3), (e2 AND e3, OP@4), e4 ]
--
-- If i == 6, then
--
-- [ (e1, OR@3), (e2 AND e3, OP@6), e4 ]   (same as above; only in principle we have to resolve the associativity in the `e2 AND e3` expr)
--
-- If i < 3, say 2, then
--
-- [ (e1 OR (e2 AND e3), OP@2, e4 ]

data ExprCont =
  MkExprCont
    { op   :: Expr Name -> Expr Name -> Expr Name
    , prio :: Prio
    , pos  :: Pos
    , arg  :: Expr Name
    }

expressionCont :: Pos -> Parser ExprCont
expressionCont p = do
  withIndent GT p $ \pop -> do
    (prio, op) <- operator
    -- parg <- Lexer.indentGuard spaces GT p
    arg <- baseExpr
    pure (MkExprCont op prio pop arg)

type Prio = Int

operator :: Parser (Prio, Expr Name -> Expr Name -> Expr Name)
operator =
      (\op -> (4, infix2 And op)) <$> spacedToken_ TKAnd
  <|> (\op -> (3, infix2 Or  op)) <$> spacedToken_ TKOr
  <|> (\op -> (5, infix2 Is  op)) <$> spacedToken_ TKIs

infix2 :: (Anno -> Expr n -> Expr n -> Expr n) -> Lexeme PosToken -> Expr n -> Expr n -> Expr n
infix2 f op l r =
  f (mkHoleAnno <> mkSimpleEpaAnno (lexToEpa op) <> mkHoleAnno) l r

baseExpr :: Parser (Expr Name)
baseExpr =
  projection
    <|> do
      current <- Lexer.indentLevel
      attachAnno $
        Not emptyAnno
          <$ annoLexeme (spacedToken_ TKNot)
          <*> annoHole (indentedExpr current)

-- Some manual left-factoring here to prevent left-recursion;
-- projections can be plain variables
projection :: Parser (Expr Name)
projection =
      -- TODO: should 'TGenitive' be part of 'Name' or 'Proj'?
      -- May affect the source span of the name.
      -- E.g. Goto definition of `name's` would be affected, as clicking on `'s` would not be part
      -- of the overall name source span. It is possible to implement this, but slightly annoying.
      (\ n ns -> foldl' (\e (gen, n') -> Proj (mkHoleAnno <> mkSimpleEpaAnno (lexToEpa gen) <> mkHoleAnno) e n') (Var mkHoleAnno n) ns)
  <$> name
  <*> many ((,) <$> spacedToken_ TGenitive <*> name)

example1 :: Text
example1 =
  Text.unlines
    [ "     foo"
    , " AND bar"
    ]

example1b :: Text
example1b =
  Text.unlines
    [ "     foo"
    , " AND NOT bar"
    ]

example2 :: Text
example2 =
  Text.unlines
    [ "        foo"
    , "     OR bar"
    , " AND baz"
    ]

example3 :: Text
example3 =
  Text.unlines
    [ "        foo"
    , "     OR bar"
    , " AND    foo"
    , "     OR baz"
    ]

example3b :: Text
example3b =
  Text.unlines
    [ "     NOT    foo"
    , "         OR bar"
    , " AND        foo"
    , "     OR NOT baz"
    ]

example4 :: Text
example4 =
  Text.unlines
    [ "        foo"
    , "    AND bar"
    , " OR     baz"
    ]

example5 :: Text
example5 =
  Text.unlines
    [ "        foo"
    , "    AND bar"
    , "    AND foobar"
    , " OR     baz"
    ]

example6 :: Text
example6 =
  Text.unlines
    [ "            foo"
    , "        AND bar"
    , "     OR     baz"
    , " AND        foobar"
    ]

example7 :: Text
example7 =
  Text.unlines
    [ "            foo IS x"
    , "        AND bar IS y"
    , "     OR     baz IS z"
    ]

example7b :: Text
example7b =
  Text.unlines
    [ "               foo"
    , "            IS x"
    , "        AND    bar"
    , "            IS y"
    , "     OR        baz"
    , "            IS z"
    ]

example8 :: Text
example8 =
  Text.unlines
    [ "          b's stage     IS Seed"
    , "    AND   b's sector    IS `Information Technology`"
    , "    AND   b's stage_com IS Pre_Revenue"
    , " AND      b's stage     IS `Series A`"
    , "    OR    b's sector    IS `Information Technology`"
    , "    AND   b's stage_com IS Pre_Profit"
    , " OR     inv's wants_ESG"
    , "    AND   b's has_ESG"
    ]

example9 :: Text
example9 =
  Text.unlines
    [ "     foo"
    , " AND bar"
    , " OR  foo"
    , " AND baz"
    ]

example9b :: Text
example9b =
  Text.unlines
    [ "     foo"
    , " OR  bar"
    , " AND foo"
    , " OR  baz"
    ]

example10 :: Text
example10 =
  Text.unlines
    [ "     foo"
    , " AND bar"
    , " AND baz"
    ]

-- This looks wrong, should be foo (AND bar) OR baz
example11a :: Text
example11a =
      " foo AND bar OR baz"

-- This is unclear
example11b :: Text
example11b =
  Text.unlines
    [ " foo "
    , "     AND bar"
    , "             OR baz"
    ]

example11c :: Text
example11c =
  Text.unlines
    [ "                foo"
    , "     AND        bar"
    , "             OR baz"
    ]

-- This is unclear, probably (foo AND bar) OR baz
example11d :: Text
example11d =
  Text.unlines
    [ " foo AND bar"
    , "     OR  baz"
    ]

-- Hannes says: (foo OR bar) AND baz
example11e :: Text
example11e =
  Text.unlines
    [ " foo OR  bar"
    , "     AND baz"
    ]

--
-- data Expr =
--     And  Expr Expr
--   | Or   Expr Expr
--   | Is   Expr Expr
--   | Proj Name Expr
--   | Var  Name
--   deriving stock Show

execParser :: Parser a -> String -> Text -> Either (NonEmpty PError) a
execParser p file input =
  case execLexer file input of
    Left errs -> Left $ fmap (mkPError "lexer") errs
    Right ts ->
      case parse (p <* eof) file (MkTokenStream (Text.unpack input) ts) of
        Left err -> Left (fmap (mkPError "parser") $ errorBundleToErrorMessages err)
        Right x  -> Right x

parseFile :: Show a => Parser a -> String -> Text -> IO ()
parseFile p file input =
  case execParser p file input of
    Left errs -> Text.putStr $ Text.unlines $ fmap (.message) (toList errs)
    Right x -> pPrint x

parseTest :: Show a => Parser a -> Text -> IO ()
parseTest p = parseFile p ""

-- ----------------------------------------------------------------------------
-- Parser error messages
-- ----------------------------------------------------------------------------

data PError
  = PError
    { message :: Text
    , start :: SrcPos
    , origin :: Text
    }
  deriving (Show, Eq, Ord)

mkPError :: Text -> (Text, SourcePos) -> PError
mkPError orig (m, s) =
  PError
    { message = m
    , start = MkSrcPos
        { filename = sourceName s
        , line = unPos $ sourceLine s
        , column = unPos $ sourceColumn s
        }
    , origin = orig
    }

-- ----------------------------------------------------------------------------
-- jl4 specific annotation helpers
-- ----------------------------------------------------------------------------

type WithAnno = WithAnno_ PosToken

type Epa = Epa_ PosToken

type Lexeme = Lexeme_ PosToken

-- ----------------------------------------------------------------------------
-- Annotation Combinators
-- ----------------------------------------------------------------------------

data WithAnno_ t a = WithAnno (Anno_ t) a
  deriving stock Show
  deriving (Functor)

unAnno :: WithAnno_ t a -> a
unAnno (WithAnno _ a) = a

toAnno :: WithAnno_ t a -> Anno_ t
toAnno (WithAnno ann _) = ann

annoHole :: Parser e -> Compose Parser (WithAnno_ t) e
annoHole p = Compose $ fmap (WithAnno (mkAnno [mkHole])) p

annoEpa :: Parser (Epa_ t e) -> Compose Parser (WithAnno_ t) e
annoEpa p = Compose $ fmap epaToAnno p

annoLexeme :: Parser (Lexeme_ t t) -> Compose Parser (WithAnno_ t) t
annoLexeme = annoEpa . fmap lexToEpa

instance Applicative (WithAnno_ t) where
  pure a = WithAnno emptyAnno a
  WithAnno ps f <*> WithAnno ps2 x = WithAnno (ps <> ps2) (f x)

attachAnno :: (HasAnno e, AnnoToken e ~ t) => Compose Parser (WithAnno_ t) e -> Parser e
attachAnno p = fmap (\(WithAnno ann e) -> setAnno ann e) $ getCompose p

attachEpa :: (HasAnno e, AnnoToken e ~ t) => Parser (Epa_ t e) -> Parser e
attachEpa =
  attachAnno . annoEpa

mkHoleAnno :: Anno_ t
mkHoleAnno =
  mkAnno [mkHole]

mkSimpleEpaAnno :: Epa_ t a -> Anno_ t
mkSimpleEpaAnno =
  toAnno . epaToAnno

epaToAnno :: Epa_ t a -> WithAnno_ t a
epaToAnno (Epa this trailing e) = WithAnno (mkAnno [mkCsn cluster]) e
 where
  cluster =
    CsnCluster
      { payload = mkConcreteSyntaxNode this
      , trailing = mkConcreteSyntaxNode trailing
      }

data Lexeme_ t a = Lexeme [t] a
  deriving stock Show
  deriving (Functor)

unLexeme :: Lexeme_ t a -> a
unLexeme (Lexeme _ a) = a

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
