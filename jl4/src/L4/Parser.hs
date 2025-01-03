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

spacesOrAnnotations :: Parser [PosToken]
spacesOrAnnotations =  concat <$> many (spaces1 <|> blockNlgAnnotation <|> blockRefAnnotation)

spaces :: Parser [PosToken]
spaces =
  takeWhileP (Just "space token") (\t -> isSpaceToken t || isAnnotationToken t)

spaces1 :: Parser [PosToken]
spaces1 =
  takeWhile1P (Just "space token") (\t -> isSpaceToken t || isAnnotationToken t)

blockNlgAnnotation :: Parser [PosToken]
blockNlgAnnotation =
  (\open (mid, close) -> [open] <> mid <> [close] ) <$> plainToken TSOpen <*> manyTill_ (anySingle <?> "NLG Block Annotation") (plainToken TSClose)

blockRefAnnotation :: Parser [PosToken]
blockRefAnnotation =
  (\open (mid, close) -> [open] <> mid <> [close] ) <$> plainToken TAOpen <*> manyTill_ (anySingle <?> "Ref Block Annotation") (plainToken TAClose)

isSpaceToken :: PosToken -> Bool
isSpaceToken t =
  case t.payload of
    TSpace _        -> True
    TLineComment _  -> True
    TBlockComment _ -> True
    _               -> False

isAnnotationToken :: PosToken -> Bool
isAnnotationToken t =
  case t.payload of
    TNlg _ -> True
    TRef _ -> True
    _      -> False

lexeme :: Parser a -> Parser (Lexeme a)
lexeme p = do
  a <- p
  trailingTokens <- spacesOrAnnotations
  pure $ Lexeme trailingTokens a

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

indentedName :: Pos -> Parser Name
indentedName p =
  withIndent GT p $ \_ -> name

program :: Parser (Program Name)
program = do
  leadingTokens <- spacesOrAnnotations
  let wsSpace = Lexeme [] leadingTokens
  (\ s ss -> MkProgram (mkSimpleEpaAnno (lexesToEpa wsSpace) <> mkHoleAnno) (s : ss)) <$> anonymousSection <*> many section

manyLines :: Parser a -> Parser [a]
manyLines p = do
  current <- Lexer.indentLevel
  many (withIndent EQ current (const p))

someLines :: Parser a -> Parser [a]
someLines p = do
  current <- Lexer.indentLevel
  some (withIndent EQ current (const p))

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

anonymousSection :: Parser (Section Name)
anonymousSection =
  attachAnno $
    MkSection emptyAnno 0 Nothing
      <$> annoHole (manyLines topdecl)

section :: Parser (Section Name)
section =
  attachAnno $
    MkSection emptyAnno
      <$> annoEpa sectionSymbols
      <*> annoHole (optional name)
      <*> annoHole (manyLines topdecl)

sectionSymbols :: Parser (Epa Int)
sectionSymbols = do
  paragraphSymbols <- lexeme (some (plainToken TParagraph))
  pure $ length <$> lexesToEpa paragraphSymbols

topdecl :: Parser (TopDecl Name)
topdecl =
      Declare mkHoleAnno <$> declare
  <|> Decide  mkHoleAnno <$> decide
  <|> Assume  mkHoleAnno <$> assume

assume :: Parser (Assume Name)
assume = do
  current <- Lexer.indentLevel
  attachAnno $
    MkAssume emptyAnno
      <$  annoLexeme (spacedToken_ TKAssume)
      <*> annoHole appForm
      <*  annoLexeme (spacedToken_ TKIs)
      <*  optional article
      <*> annoHole (indentedType current)

declare :: Parser (Declare Name)
declare =
  attachAnno $
    MkDeclare emptyAnno
      <$  annoLexeme (spacedToken_ TKDeclare)
      <*> annoHole appForm
      <*> annoHole typeDecl

typeDecl :: Parser (TypeDecl Name)
typeDecl =
  recordDecl <|> enumDecl

recordDecl :: Parser (TypeDecl Name)
recordDecl =
  attachAnno $
    RecordDecl emptyAnno
      <$  annoLexeme (spacedToken_ TKHas)
      <*> annoHole (lsepBy reqParam (spacedToken_ TComma))

enumDecl :: Parser (TypeDecl Name)
enumDecl =
  attachAnno $
    EnumDecl emptyAnno
      <$  annoLexeme (spacedToken_ TKIs)
      <*  annoLexeme (spacedToken_ TKOne)
      <*  annoLexeme (spacedToken_ TKOf)
      <*> annoHole (lsepBy conDecl (spacedToken_ TComma))

conDecl :: Parser (ConDecl Name)
conDecl =
  attachAnno $
    MkConDecl emptyAnno
      <$> annoHole name
      <*> option [] (annoLexeme (spacedToken_ TKHas) *> annoHole (lsepBy reqParam (spacedToken_ TComma)))

decide :: Parser (Decide Name)
decide = do
  sig <- typeSig
  current <- Lexer.indentLevel
  decideKW sig current <|> meansKW sig current
  where
    decideKW sig current =
      attachAnno $
        MkDecide emptyAnno
          <$> annoHole (pure sig)
          <*  annoLexeme (spacedToken_ TKDecide)
          <*> annoHole appForm
          <*  annoLexeme (spacedToken_ TKIs <|> spacedToken_ TKIf)
          <*> annoHole (indentedExpr current)

    meansKW sig current =
      attachAnno $
        MkDecide emptyAnno
          <$> annoHole (pure sig)
          <*> annoHole appForm
          <*  annoLexeme (spacedToken_ TKMeans)
          <*> annoHole (indentedExpr current)

appForm :: Parser (AppForm Name)
appForm = do
  current <- Lexer.indentLevel
  attachAnno $
    MkAppForm emptyAnno
      <$> annoHole name
      <*> (   annoLexeme (spacedToken_ TKOf) *> annoHole (lsepBy1 name (spacedToken_ TKAnd))
          <|> annoHole (lmany (indentedName current))
          )

typeSig :: Parser (TypeSig Name)
typeSig =
  attachAnno $
    MkTypeSig emptyAnno
      <$> annoHole (option (MkGivenSig emptyAnno []) givens)
      <*> annoHole (optional giveth)

givens :: Parser (GivenSig Name)
givens =
  attachAnno $
    MkGivenSig emptyAnno
      <$  annoLexeme (spacedToken_ TKGiven)
      <*> annoHole (lsepBy param (spacedToken_ TComma))

giveth :: Parser (GivethSig Name)
giveth = do
  current <- Lexer.indentLevel
  attachAnno $
    MkGivethSig emptyAnno
      <$  annoLexeme (spacedToken_ TKGiveth)
      <*  optional article
      <*> annoHole (indentedType current)

-- primarily for testing
expr :: Parser (Expr Name)
expr =
  indentedExpr (mkPos 1)

indentedType :: Pos -> Parser (Type' Name)
indentedType p =
  withIndent GT p $ \_ -> type'

type' :: Parser (Type' Name)
type' =
      (attachAnno $
         Type emptyAnno <$ annoLexeme (spacedToken_ TKType))
  <|> tyApp
  <|> fun
  <|> forall'

tyApp :: Parser (Type' Name)
tyApp = do
  current <- Lexer.indentLevel
  attachAnno $
    TyApp emptyAnno
    <$> annoHole name
    <*> (   annoLexeme (spacedToken_ TKOf) *> annoHole (lsepBy1 (indentedType current) (spacedToken_ TKAnd))
        <|> annoHole (lmany (indentedType current))
        )

fun :: Parser (Type' Name)
fun = do
  current <- Lexer.indentLevel
  attachAnno $
    Fun emptyAnno
    <$  annoLexeme (spacedToken_ TKFunction)
    <*  annoLexeme (spacedToken_ TKFrom)
    <*> annoHole (lsepBy1 (indentedType current) (spacedToken_ TKAnd))
    <*  annoLexeme (spacedToken_ TKTo)
    <*> annoHole (indentedType current)

forall' :: Parser (Type' Name)
forall' = do
  current <- Lexer.indentLevel
  attachAnno $
    Forall emptyAnno
    <$  annoLexeme (spacedToken_ TKFor)
    <*  annoLexeme (spacedToken_ TKAll)
    <*> annoHole (lsepBy1 name (spacedToken_ TKAnd))
    <*  optional article
    <*> annoHole (indentedType current)

article :: Compose Parser (WithAnno_ PosToken) PosToken
article =
  annoLexeme (spacedToken_ TKA <|> spacedToken_ TKAn <|> spacedToken_ TKThe)

{-
enumType :: Compose Parser WithAnno (Type' Name)
enumType =
  Enum emptyAnno
    <$  annoLexeme (spacedToken_ TKOne)
    <*  annoLexeme (spacedToken_ TKOf)
    <*> annoHole (lsepBy1 name (spacedToken_ TComma))
-}

lmany :: Parser a -> Parser [a]
lmany pp =
  fmap concat $ manyLines $ some pp

lsepBy :: forall a. HasAnno a => Parser a -> Parser (Lexeme_ (AnnoToken a) (AnnoToken a)) -> Parser [a]
lsepBy pp sep =
  fmap concat $ manyLines $ do
    (ps, seps) <- P.sepBy1 pp sep
    pure $ zipWithLeftovers ps seps
  where
    zipWithLeftovers :: [a] -> [Lexeme_ (AnnoToken a) (AnnoToken a)] -> [a]
    zipWithLeftovers ps [] = ps
    zipWithLeftovers [] _  = []
    zipWithLeftovers (p : ps) (s : ss) = setAnno (getAnno p <> mkSimpleEpaAnno (lexToEpa s)) p : zipWithLeftovers ps ss

lsepBy1 :: forall a. HasAnno a => Parser a -> Parser (Lexeme_ (AnnoToken a) (AnnoToken a)) -> Parser [a]
lsepBy1 pp sep =
  fmap concat $ someLines $ do
    (ps, seps) <- P.sepBy1 pp sep
    pure $ zipWithLeftovers ps seps
  where
    zipWithLeftovers :: [a] -> [Lexeme_ (AnnoToken a) (AnnoToken a)] -> [a]
    zipWithLeftovers ps [] = ps
    zipWithLeftovers [] _  = []
    zipWithLeftovers (p : ps) (s : ss) = setAnno (getAnno p <> mkSimpleEpaAnno (lexToEpa s)) p : zipWithLeftovers ps ss

reqParam :: Parser (TypedName Name)
reqParam =
  attachAnno $
    MkTypedName emptyAnno
      <$> annoHole name
      <*  annoLexeme (spacedToken_ TKIs)
      <*  optional article
      <*> annoHole type'

param :: Parser (OptionallyTypedName Name)
param =
  attachAnno $
    MkOptionallyTypedName emptyAnno
      <$> annoHole name
      <*> optional (annoLexeme (spacedToken_ TKIs) *> optional article *> annoHole type')

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
    pure (combine End e efs)

data Stack a =
    Frame (Stack a) (a Name) (a Name -> a Name -> a Name) Prio Pos
  | End

combine :: Stack a -> a Name -> [Cont a] -> a Name
combine End e [] = e
combine (Frame s e1 op _ _) e2 [] =
  combine s (e1 `op` e2) []
combine End e1 (MkCont op1 prio1 p1 e2 : efs) =
  combine (Frame End e1 op1 prio1 p1) e2 efs
combine s1@(Frame s e1 op1 prio1 p1) e2 (MkCont op2 prio2 p2 e3 : efs)
  | p2 > p1 || p2 == p1 && prio2 >= prio1 = -- (>=) is for *right*-associative operators
  combine (Frame s1 e2 op2 prio2 p2) e3 efs -- push
  | otherwise =
  combine s (e1 `op1` e2) (MkCont op2 prio2 p2 e3 : efs) -- pop

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

data Cont a =
  MkCont
    { _op   :: a Name -> a Name -> a Name
    , _prio :: Prio
    , _pos  :: Pos
    , _arg  :: a Name
    }

cont :: Parser (Prio, a Name -> a Name -> a Name) -> Parser (a Name) -> Pos -> Parser (Cont a)
cont pop pbase p =
  withIndent GT p $ \ pos -> do
    (prio, op) <- pop
    -- parg <- Lexer.indentGuard spaces GT p
    arg <- pbase
    pure (MkCont op prio pos arg)

expressionCont :: Pos -> Parser (Cont Expr)
expressionCont = cont operator baseExpr

type Prio = Int

-- TODO: My ad-hoc fix for multi-token operators can probably be done more elegantly.
operator :: Parser (Prio, Expr Name -> Expr Name -> Expr Name)
operator =
      (\ op -> (3, infix2  Or        op)) <$> (spacedToken_ TKOr     <|> spacedToken_ TOr    )
  <|> (\ op -> (4, infix2  And       op)) <$> (spacedToken_ TKAnd    <|> spacedToken_ TAnd   )
  <|> (\ op -> (5, infix2  Equals    op)) <$> (spacedToken_ TKEquals <|> spacedToken_ TEquals)
  <|> (\ op -> (5, infix2' Cons      op)) <$> ((\ l1 l2 -> mkSimpleEpaAnno (lexToEpa l1) <> mkSimpleEpaAnno (lexToEpa l2)) <$> spacedToken_ TKFollowed <*> spacedToken_ TKBy)
  <|> (\ op -> (6, infix2  Plus      op)) <$> (spacedToken_ TKPlus   <|> spacedToken_ TPlus  )
  <|> (\ op -> (6, infix2  Minus     op)) <$> (spacedToken_ TKMinus  <|> spacedToken_ TMinus )
  <|> (\ op -> (7, infix2  Times     op)) <$> (spacedToken_ TKTimes  <|> spacedToken_ TTimes )
  <|> (\ op -> (7, infix2' DividedBy op)) <$> (((\ l1 l2 -> mkSimpleEpaAnno (lexToEpa l1) <> mkSimpleEpaAnno (lexToEpa l2)) <$> spacedToken_ TKDivided <*> spacedToken_ TKBy) <|> (mkSimpleEpaAnno . lexToEpa) <$> spacedToken_ TDividedBy)

infix2 :: (Anno -> a n -> a n -> a n) -> Lexeme PosToken -> a n -> a n -> a n
infix2 f op l r =
  f (mkHoleAnno <> mkSimpleEpaAnno (lexToEpa op) <> mkHoleAnno) l r

infix2' :: (Anno -> a n-> a n -> a n) -> Anno -> a n -> a n -> a n
infix2' f op l r =
  f (mkHoleAnno <> op <> mkHoleAnno) l r

baseExpr :: Parser (Expr Name)
baseExpr =
      try projection
  <|> negation
  <|> ifthenelse
  <|> lam
  <|> consider
  <|> app
  <|> parenExpr

parenExpr :: Parser (Expr Name)
parenExpr =
  attachAnno $
    ParenExpr emptyAnno
    <$  annoLexeme (spacedToken_ TPOpen)
    <*> annoHole expr
    <*  annoLexeme (spacedToken_ TPClose)

app :: Parser (Expr Name)
app = do
  current <- Lexer.indentLevel
  attachAnno $
    App emptyAnno
    <$> annoHole name
    <*> (   annoLexeme (spacedToken_ TKOf) *> annoHole (lsepBy1 (indentedExpr current) (spacedToken_ TKAnd))
        <|> annoHole (lmany (indentedExpr current))
        )

negation :: Parser (Expr Name)
negation = do
  current <- Lexer.indentLevel
  attachAnno $
    Not emptyAnno
      <$  annoLexeme (spacedToken_ TKNot)
      <*> annoHole (indentedExpr current)

lam :: Parser (Expr Name)
lam = do
  current <- Lexer.indentLevel
  attachAnno $
    Lam emptyAnno <$> annoHole givens <* annoLexeme (spacedToken_ TKYield) <*> annoHole (indentedExpr current)

ifthenelse :: Parser (Expr Name)
ifthenelse = do
  current <- Lexer.indentLevel
  attachAnno $
    IfThenElse emptyAnno
      <$  annoLexeme (spacedToken_ TKIf)
      <*> annoHole (indentedExpr current)
      <*  annoLexeme (spacedToken_ TKThen)
      <*> annoHole (indentedExpr current)
      <*  annoLexeme (spacedToken_ TKElse)
      <*> annoHole (indentedExpr current)

consider :: Parser (Expr Name)
consider = do
  current <- Lexer.indentLevel
  attachAnno $
    Consider emptyAnno
      <$  annoLexeme (spacedToken_ TKConsider)
      <*> annoHole (indentedExpr current)
      <*> annoHole (lsepBy branch (spacedToken_ TComma))

branch :: Parser (Branch Name)
branch =
  when' <|> otherwise'

when' :: Parser (Branch Name)
when' = do
  current <- Lexer.indentLevel
  attachAnno $
    When emptyAnno
      <$  annoLexeme (spacedToken_ TKWhen)
      <*> annoHole (indentedPattern current)
      <*  annoLexeme (spacedToken_ TKThen)
      <*> annoHole (indentedExpr current)

otherwise' :: Parser (Branch Name)
otherwise' = do
  current <- Lexer.indentLevel
  attachAnno $
    Otherwise emptyAnno
      <$  annoLexeme (spacedToken_ TKOtherwise)
      <*> annoHole (indentedExpr current)

indentedPattern :: Pos -> Parser (Pattern Name)
indentedPattern p =
  withIndent GT p $ \ _ -> do
    pat <- basePattern
    pfs <- many (patternCont p)
    pure (combine End pat pfs)

basePattern :: Parser (Pattern Name)
basePattern =
  patApp

patternCont :: Pos -> Parser (Cont Pattern)
patternCont = cont patOperator basePattern

patOperator :: Parser (Prio, Pattern Name -> Pattern Name -> Pattern Name)
patOperator =
  (\ op -> (5, infix2' PatCons      op)) <$> ((\ l1 l2 -> mkSimpleEpaAnno (lexToEpa l1) <> mkSimpleEpaAnno (lexToEpa l2)) <$> spacedToken_ TKFollowed <*> spacedToken_ TKBy)

patApp :: Parser (Pattern Name)
patApp = do
  current <- Lexer.indentLevel
  attachAnno $
    PatApp emptyAnno
    <$> annoHole name
    <*> (   annoLexeme (spacedToken_ TKOf) *> annoHole (lsepBy (indentedPattern current) (spacedToken_ TKAnd))
        <|> annoHole (lmany (indentedPattern current))
        )

-- Some manual left-factoring here to prevent left-recursion
-- TODO: the interaction between projection and application has to be properly sorted out
projection :: Parser (Expr Name)
projection =
      -- TODO: should 'TGenitive' be part of 'Name' or 'Proj'?
      -- May affect the source span of the name.
      -- E.g. Goto definition of `name's` would be affected, as clicking on `'s` would not be part
      -- of the overall name source span. It is possible to implement this, but slightly annoying.
      (\ n ns -> foldl' (\e (gen, n') -> Proj (mkHoleAnno <> mkSimpleEpaAnno (lexToEpa gen) <> mkHoleAnno) e n') (Var mkHoleAnno n) ns)
  <$> name
  <*> some ((,) <$> spacedToken_ TGenitive <*> name)

_example1 :: Text
_example1 =
  Text.unlines
    [ "     foo"
    , " AND bar"
    ]

_example1b :: Text
_example1b =
  Text.unlines
    [ "     foo"
    , " AND NOT bar"
    ]

_example2 :: Text
_example2 =
  Text.unlines
    [ "        foo"
    , "     OR bar"
    , " AND baz"
    ]

_example3 :: Text
_example3 =
  Text.unlines
    [ "        foo"
    , "     OR bar"
    , " AND    foo"
    , "     OR baz"
    ]

_example3b :: Text
_example3b =
  Text.unlines
    [ "     NOT    foo"
    , "         OR bar"
    , " AND        foo"
    , "     OR NOT baz"
    ]

_example4 :: Text
_example4 =
  Text.unlines
    [ "        foo"
    , "    AND bar"
    , " OR     baz"
    ]

_example5 :: Text
_example5 =
  Text.unlines
    [ "        foo"
    , "    AND bar"
    , "    AND foobar"
    , " OR     baz"
    ]

_example6 :: Text
_example6 =
  Text.unlines
    [ "            foo"
    , "        AND bar"
    , "     OR     baz"
    , " AND        foobar"
    ]

_example7 :: Text
_example7 =
  Text.unlines
    [ "            foo IS x"
    , "        AND bar IS y"
    , "     OR     baz IS z"
    ]

_example7b :: Text
_example7b =
  Text.unlines
    [ "               foo"
    , "            IS x"
    , "        AND    bar"
    , "            IS y"
    , "     OR        baz"
    , "            IS z"
    ]

_example8 :: Text
_example8 =
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

_example9 :: Text
_example9 =
  Text.unlines
    [ "     foo"
    , " AND bar"
    , " OR  foo"
    , " AND baz"
    ]

_example9b :: Text
_example9b =
  Text.unlines
    [ "     foo"
    , " OR  bar"
    , " AND foo"
    , " OR  baz"
    ]

_example10 :: Text
_example10 =
  Text.unlines
    [ "     foo"
    , " AND bar"
    , " AND baz"
    ]

-- This looks wrong, should be foo (AND bar) OR baz
_example11a :: Text
_example11a =
      " foo AND bar OR baz"

-- This is unclear
_example11b :: Text
_example11b =
  Text.unlines
    [ " foo "
    , "     AND bar"
    , "             OR baz"
    ]

_example11c :: Text
_example11c =
  Text.unlines
    [ "                foo"
    , "     AND        bar"
    , "             OR baz"
    ]

-- This is unclear, probably (foo AND bar) OR baz
_example11d :: Text
_example11d =
  Text.unlines
    [ " foo AND bar"
    , "     OR  baz"
    ]

-- Hannes says: (foo OR bar) AND baz
_example11e :: Text
_example11e =
  Text.unlines
    [ " foo OR  bar"
    , "     AND baz"
    ]

execParser :: Parser a -> String -> Text -> Either (NonEmpty PError) a
execParser p file input =
  case execLexer file input of
    Left errs -> Left $ fmap (mkPError "lexer") errs
    Right ts ->
      case parse (p <* eof) file (MkTokenStream (Text.unpack input) ts) of
        Left err -> Left (fmap (mkPError "parser") $ errorBundleToErrorMessages err)
        Right x  -> Right x

-- | Parse a source file and pretty-print the resulting syntax tree.
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
