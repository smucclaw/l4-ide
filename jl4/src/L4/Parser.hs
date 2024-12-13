{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
module L4.Parser where

import Base

import Control.Monad
import Data.Char
import Data.List
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Void
import Optics
import Text.Megaparsec hiding (parseTest)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Lexer

import L4.Lexer
import L4.Syntax

import Text.Pretty.Simple
import qualified Data.Set as E

type Parser = Parsec Void TokenStream

spaces :: Parser ()
spaces =
  void (takeWhileP (Just "space token") isSpaceToken)

isSpaceToken :: PosToken -> Bool
isSpaceToken t =
  case t.payload of
    TSpace _        -> True
    TLineComment _  -> True
    TBlockComment _ -> True
    _               -> False

lexeme :: Parser a -> Parser a
lexeme =
  Lexer.lexeme spaces

plainToken_ :: TokenType -> Parser ()
plainToken_ tt =
  token
    (\ t -> if t.payload == tt then Just () else Nothing)
    (Set.singleton (Tokens (trivialToken tt :| [])))

trivialToken :: TokenType -> PosToken
trivialToken tt =
  MkPosToken trivialRange tt
  where
    trivialRange :: SrcRange
    trivialRange = MkSrcRange trivialPos trivialPos 0

    trivialPos :: SrcPos
    trivialPos = MkSrcPos "" 0 0

spacedToken_ :: TokenType -> Parser ()
spacedToken_ tt =
  lexeme (plainToken_ tt)

spacedToken :: (TokenType -> Maybe a) -> String -> Parser a
spacedToken cond lbl =
  lexeme
    (token
      (\ t -> cond t.payload)
      Set.empty
    )
  <?> lbl

-- | A quoted identifier between backticks.
quotedName :: Parser Name
quotedName =
  spacedToken (preview #_TQuoted) "quoted identifier"

simpleName :: Parser Name
simpleName =
  spacedToken (preview #_TIdentifier) "identifier"

name :: Parser Name
name =
  (quotedName <|> simpleName) <?> "identifier"

program :: Parser (Program Name)
program =
  MkProgram <$> many section

manyLines :: Parser () -> Parser a -> Parser [a]
manyLines sc p = do
  current <- Lexer.indentLevel
  many (withIndent p sc EQ current)

-- | Run the parser only when the indentation is correct and fail otherwise.
withIndent :: (TraversableStream s, MonadParsec e s m) => m b -> m a -> Ordering -> Pos -> m b
withIndent p sc ordering current = do
  actual <- Lexer.indentLevel
  if compare current actual == ordering
    then do
      a <- p
      _ <- sc
      pure a
    else
      fancyFailure . E.singleton $
        ErrorIndentation ordering current actual

section :: Parser (Section Name)
section =
  MkSection
  <$> sectionSymbols
  <*> name
  <*> manyLines spaces decl

sectionSymbols :: Parser Int
sectionSymbols =
  length <$> lexeme (some (plainToken_ TParagraph))

decl :: Parser (Decl Name)
decl = Declare <$> declare <|> Decide <$> decide

declare :: Parser (Declare Name)
declare =
  MkDeclare
  <$  spacedToken_ TKDeclare
  <*> name
  <*> ofType

decide :: Parser (Decide Name)
decide = do
  sig <- typeSig
  current <- Lexer.indentLevel
  _ <- spacedToken_ TKDecide
  clauses <- manyLines spaces (clause current) -- I see room for ambiguity here
  pure (MkDecide sig clauses)

typeSig :: Parser (TypeSig Name)
typeSig =
  MkTypeSig
  <$> option [] given
  <*> optional giveth

given :: Parser [TypedName Name]
given =
     spacedToken_ TKGiven
  *> manyLines spaces typedName

giveth :: Parser (TypedName Name)
giveth =
     spacedToken_ TKGiveth
  *> typedName

clause :: Pos -> Parser (Clause Name)
clause p =
  GuardedClause
  <$> indentedExpr p
  <*> guard_

-- primarily for testing
expr :: Parser (Expr Name)
expr =
  indentedExpr (mkPos 1)

guard_ :: Parser (Guard Name)
guard_ =
      Otherwise <$ spacedToken_ TKOtherwise
  <|> plainGuard

plainGuard :: Parser (Guard Name)
plainGuard = do
  current <- Lexer.indentLevel
  _ <- spacedToken_ TKIf
  PlainGuard <$> indentedExpr current

ofType :: Parser (Type' Name)
ofType =
      spacedToken_ TKIs  *> isType
  <|> spacedToken_ TKHas *> record

isType :: Parser (Type' Name)
isType =
      NamedType <$ (spacedToken_ TKA <|> spacedToken_ TKAn) <*> name
  <|> Enum <$ spacedToken_ TKOne <* spacedToken_ TKOf <*> (concat <$> manyLines spaces (sepBy name (spacedToken_ TComma)))

record :: Parser (Type' Name)
record =
  Record <$> manyLines spaces typedName

typedName :: Parser (TypedName Name)
typedName =
  MkTypedName <$> name <*> ofType

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
  _ <- Lexer.indentGuard spaces GT p
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
  pop <- Lexer.indentGuard spaces GT p
  (prio, op) <- operator
  -- parg <- Lexer.indentGuard spaces GT p
  arg <- baseExpr
  pure (MkExprCont op prio pop arg)

type Prio = Int

operator :: Parser (Prio, Expr Name -> Expr Name -> Expr Name)
operator =
      (4, And) <$ spacedToken_ TKAnd
  <|> (3, Or ) <$ spacedToken_ TKOr
  <|> (5, Is ) <$ spacedToken_ TKIs

baseExpr :: Parser (Expr Name)
baseExpr =
      projection
  <|> do
        current <- Lexer.indentLevel
        _ <- spacedToken_ TKNot
        Not <$> indentedExpr current

-- Some manual left-factoring here to prevent left-recursion;
-- projections can be plain variables
projection :: Parser (Expr Name)
projection =
      (\ n ns -> foldl' Proj (Var n) ns)
  <$> name
  <*> many (spacedToken_ TGenitive *> name)

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

execParser :: Parser a -> String -> Text -> Either String a
execParser p file input =
  case execLexer file input of
    Left errs -> Left errs
    Right ts ->
      case parse (spaces *> p <* eof) file (MkTokenStream (Text.unpack input) ts) of
        Left err -> Left (errorBundlePretty err)
        Right x  -> Right x

parseFile :: Show a => Parser a -> String -> Text -> IO ()
parseFile p file input =
  case execParser p file input of
    Left errs -> putStr errs
    Right x -> pPrint x

parseTest :: Show a => Parser a -> Text -> IO ()
parseTest p = parseFile p ""
