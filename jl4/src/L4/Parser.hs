{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
module L4.Parser (
  -- * Public API
  parseFile,
  execParser,
  execParserForTokens,
  program,
  PError(..),
  mkPError,
  -- * Testing API
  parseTest,
) where

import Base

import qualified Control.Applicative as Applicative
import Data.Functor.Compose
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
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Foldable as Foldable
import GHC.Records

type Parser = Parsec Void TokenStream

spacesOrAnnotations :: Parser [PosToken]
spacesOrAnnotations =  concat <$> many (spaces1 <|> blockNlgAnnotation <|> blockRefAnnotation)

spaces1 :: Parser [PosToken]
spaces1 =
  takeWhile1P (Just "space token") isWhiteSpaceToken

isWhiteSpaceToken :: PosToken -> Bool
isWhiteSpaceToken t = isSpaceToken t || isAnnotationToken t

blockNlgAnnotation :: Parser [PosToken]
blockNlgAnnotation =
  (\open (mid, close) -> [open] <> mid <> [close]) <$> plainToken TSOpen <*> manyTill_ (anySingle <?> "NLG Block Annotation") (plainToken TSClose)

blockRefAnnotation :: Parser [PosToken]
blockRefAnnotation =
  (\open (mid, close) -> [open] <> mid <> [close]) <$> plainToken TAOpen <*> manyTill_ (anySingle <?> "Ref Block Annotation") (plainToken TAClose)

lexeme :: Parser a -> Parser (Lexeme a)
lexeme p = do
  a <- p
  trailingTokens <- spacesOrAnnotations
  pure $ Lexeme trailingTokens a

plainToken :: TokenType -> Parser PosToken
plainToken tt =
  token
    (\ t -> if computedPayload t == tt then Just t else Nothing)
    (Set.singleton (Tokens (L.trivialToken tt :| [])))

spacedToken_ :: TokenType -> Parser (Lexeme PosToken)
spacedToken_ tt =
  lexeme (plainToken tt)

spacedToken :: (TokenType -> Maybe a) -> String -> Parser (Epa a)
spacedToken cond lbl =
  lexToEpa' <$>
    lexeme
      (token
        (\ t -> (t,) <$> cond (computedPayload t))
        Set.empty
      )
    <?> lbl

(<<$>>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
fab <<$>> fga = (fmap . fmap) fab fga

-- | A quoted identifier between backticks.
quotedName :: Parser (Epa Name)
quotedName =
  (MkName emptyAnno . NormalName) <<$>> spacedToken (preview #_TQuoted) "quoted identifier"

simpleName :: Parser (Epa Name)
simpleName =
  (MkName emptyAnno . NormalName) <<$>> spacedToken (preview #_TIdentifier) "identifier"

name :: Parser Name
name = attachEpa (quotedName <|> simpleName) <?> "identifier"

tokenAsName :: TokenType -> Parser Name
tokenAsName tt =
  attachEpa (lexToEpa' . fmap convert <$> spacedToken_ tt)
  where
    convert :: PosToken -> (PosToken, Name)
    convert p@(MkPosToken range _) = (MkPosToken range (TIdentifier t), MkName emptyAnno (NormalName t))
      where
        t = displayPosToken p

indented :: (TraversableStream s, MonadParsec e s m) => m b -> Pos -> m b
indented parser pos =
  withIndent GT pos $ \ _ -> parser

program :: Parser (Program Name)
program = do
  attachAnno $
    MkProgram emptyAnno
      <$  annoLexemes ((\ts -> Lexeme ts []) <$> spacesOrAnnotations)
      <*> annoHole
          ((\s ss -> s:ss)
            <$> anonymousSection
            <*> many section
          )


manyLines :: Parser a -> Parser [a]
manyLines p = do
  current <- Lexer.indentLevel
  manyLinesPos current p

manyLinesPos :: Pos -> Parser a -> Parser [a]
manyLinesPos current p = do
  many (withIndent EQ current (const p))

someLines :: Parser a -> Parser [a]
someLines p = do
  current <- Lexer.indentLevel
  someLinesPos current p

someLinesPos :: Pos -> Parser a -> Parser [a]
someLinesPos current p = do
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
    MkSection emptyAnno
      <$> pure 0
      <*> annoHole (pure Nothing)
      <*> annoHole (manyLines topdeclWithRecovery) -- TODO: semicolon

section :: Parser (Section Name)
section =
  attachAnno $
    MkSection emptyAnno
      <$> sectionSymbols
      <*> annoHole (optional name)
      <*> annoHole (manyLines topdeclWithRecovery) -- TODO: semicolon

sectionSymbols :: Compose Parser WithAnno Int
sectionSymbols =
  length <$> Applicative.some (annoLexeme (spacedToken_ TParagraph))

topdeclWithRecovery :: Parser (TopDecl Name)
topdeclWithRecovery = do
  start <- lookAhead anySingle
  withRecovery
    (\e -> do
      -- Ignoring tokens here is fine, as we will fail parsing any way.
      _ <- takeWhileP Nothing (\t -> not (isWhiteSpaceToken t) && t.range.start.column > 1)
      current <- lookAhead anySingle
      registerParseError e
      -- If we didn't make any progress whatsoever,
      -- end parsing to avoid endless loops.
      if start == current
        then
          parseError e
        else do
          topdeclWithRecovery)
    topdecl

topdecl :: Parser (TopDecl Name)
topdecl =
  withTypeSig (\ sig -> attachAnno $
        Declare   emptyAnno <$> annoHole (declare sig)
    <|> Decide    emptyAnno <$> annoHole (decide sig)
    <|> Assume    emptyAnno <$> annoHole (assume sig)
  ) <|> attachAnno
        (Directive emptyAnno <$> annoHole directive)

localdecl :: Parser (LocalDecl Name)
localdecl =
  withTypeSig (\ sig -> attachAnno $
        LocalDecide    emptyAnno <$> annoHole (decide sig)
    <|> LocalAssume    emptyAnno <$> annoHole (assume sig)
  )

withTypeSig :: (TypeSig Name -> Parser (d Name)) -> Parser (d Name)
withTypeSig p = do
  sig <- typeSig
  p sig

directive :: Parser (Directive Name)
directive = do
  attachAnno $
    choice
      [ Eval emptyAnno
          <$ annoLexeme (spacedToken_ (TDirective "EVAL"))
      , Check emptyAnno
          <$ annoLexeme (spacedToken_ (TDirective "CHECK"))
      ]
      <*> annoHole expr

assume :: TypeSig Name -> Parser (Assume Name)
assume sig = do
  current <- Lexer.indentLevel
  attachAnno $
    MkAssume emptyAnno
      <$> annoHole (pure sig)
      <*  annoLexeme (spacedToken_ TKAssume)
      <*> annoHole appForm
      <*> optional (annoLexeme (spacedToken_ TKIs) *> {- optional article *> -} annoHole (indented type' current))

declare :: TypeSig Name -> Parser (Declare Name)
declare sig =
  attachAnno $
    MkDeclare emptyAnno
      <$> annoHole (pure sig)
      <*  annoLexeme (spacedToken_ TKDeclare)
      <*> annoHole appForm
      <*> annoHole typeDecl

typeDecl :: Parser (TypeDecl Name)
typeDecl =
  recordDecl <|> enumDecl

recordDecl :: Parser (TypeDecl Name)
recordDecl =
  attachAnno $
    RecordDecl emptyAnno <$> recordDecl'

recordDecl' :: Compose Parser WithAnno [TypedName Name]
recordDecl' =
     annoLexeme (spacedToken_ TKHas)
  *> annoHole (lsepBy reqParam (spacedToken_ TComma))

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
      <*> option [] recordDecl'

decide :: TypeSig Name -> Parser (Decide Name)
decide sig = do
  current <- Lexer.indentLevel
  decideKW current <|> meansKW current
  where
    decideKW current =
      attachAnno $
        MkDecide emptyAnno
          <$> annoHole (pure sig)
          <*  annoLexeme (spacedToken_ TKDecide)
          <*> annoHole appForm
          <*  annoLexeme (spacedToken_ TKIs <|> spacedToken_ TKIf)
          <*> annoHole (indentedExpr current)

    meansKW current =
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
      <*> (   annoLexeme (spacedToken_ TKOf) *> annoHole (lsepBy1 name (spacedToken_ TComma))
          <|> annoHole (lmany (indented name current))
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
--      <*  optional article
      <*> annoHole (indented type' current)

-- primarily for testing
expr :: Parser (Expr Name)
expr =
  indentedExpr (mkPos 1)

type' :: Parser (Type' Name)
type' =
      withOptionalArticle
      (   typeKind
      <|> tyApp
      <|> fun
      )
  <|> forall'
  <|> parenType

typeKind :: Parser (Type' Name)
typeKind =
  attachAnno $
  Type emptyAnno <$ annoLexeme (spacedToken_ TKType)

atomicType :: Parser (Type' Name)
atomicType =
      withOptionalArticle
      (   typeKind
      <|> nameAsApp TyApp
      )
  <|> parenType

parenType :: Parser (Type' Name)
parenType =
  inlineAnnoHole $
    id
    <$  annoLexeme (spacedToken_ TPOpen)
    <*> annoHole type'
    <*  annoLexeme (spacedToken_ TPClose)

-- We don't actually currently allow parsing an optional name
optionallyNamedType :: Parser (OptionallyNamedType Name)
optionallyNamedType = do
  attachAnno $
    MkOptionallyNamedType emptyAnno
    <$> annoHole (pure Nothing)
    <*> annoHole type'

tyApp :: Parser (Type' Name)
tyApp = do
  current <- Lexer.indentLevel
  attachAnno $
    TyApp emptyAnno
    <$> annoHole (tokenAsName TKList <|> name)
    <*> (   annoLexeme (spacedToken_ TKOf) *> annoHole (lsepBy1 (indented type' current) (spacedToken_ TComma))
        <|> annoHole (lmany (indented atomicType current))
        )

fun :: Parser (Type' Name)
fun = do
  current <- Lexer.indentLevel
  attachAnno $
    Fun emptyAnno
    <$  annoLexeme (spacedToken_ TKFunction)
    <*  annoLexeme (spacedToken_ TKFrom)
    <*> annoHole (lsepBy1 (indented optionallyNamedType current) (spacedToken_ TKAnd))
    <*  annoLexeme (spacedToken_ TKTo)
    <*> annoHole (indented type' current)

forall' :: Parser (Type' Name)
forall' = do
  -- current <- Lexer.indentLevel
  attachAnno $
    Forall emptyAnno
    <$  annoLexeme (spacedToken_ TKFor)
    <*  annoLexeme (spacedToken_ TKAll)
    <*> annoHole (lsepBy1 name (spacedToken_ TKAnd))
--    <*  optional article
    <*> annoHole type' -- (indented type' current)

article :: Compose Parser WithAnno PosToken
article =
  annoLexeme (spacedToken_ TKA <|> spacedToken_ TKAn <|> spacedToken_ TKThe)

withOptionalArticle :: (HasSrcRange a, HasAnno a, AnnoToken a ~ PosToken, AnnoExtra a ~ Type' Resolved) => Parser a -> Parser a
withOptionalArticle p =
  inlineAnnoHole $
    id
    <$  optional article
    <*> annoHole p

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

lsepBy :: forall a. (HasAnno a, HasField "range" (AnnoToken a) SrcRange) => Parser a -> Parser (Lexeme_ (AnnoToken a) (AnnoToken a)) -> Parser [a]
lsepBy pp sep =
  fmap concat $ manyLines $ do
    (ps, seps) <- P.sepBy1 pp sep
    pure $ zipWithLeftovers ps seps
  where
    zipWithLeftovers :: [a] -> [Lexeme_ (AnnoToken a) (AnnoToken a)] -> [a]
    zipWithLeftovers ps [] = ps
    zipWithLeftovers [] _  = []
    zipWithLeftovers (p : ps) (s : ss) = setAnno (fixAnnoSrcRange $ getAnno p <> mkSimpleEpaAnno (lexToEpa s)) p : zipWithLeftovers ps ss

lsepBy1 :: forall a. (HasAnno a, HasField "range" (AnnoToken a) SrcRange) => Parser a -> Parser (Lexeme_ (AnnoToken a) (AnnoToken a)) -> Parser [a]
lsepBy1 pp sep =
  fmap concat $ someLines $ do
    (ps, seps) <- P.sepBy1 pp sep
    pure $ zipWithLeftovers ps seps
  where
    zipWithLeftovers :: [a] -> [Lexeme_ (AnnoToken a) (AnnoToken a)] -> [a]
    zipWithLeftovers ps [] = ps
    zipWithLeftovers [] _  = []
    zipWithLeftovers (p : ps) (s : ss) = setAnno (fixAnnoSrcRange $ getAnno p <> mkSimpleEpaAnno (lexToEpa s)) p : zipWithLeftovers ps ss

reqParam :: Parser (TypedName Name)
reqParam =
  attachAnno $
    MkTypedName emptyAnno
      <$> annoHole name
      <*  annoLexeme (spacedToken_ TKIs)
--      <*  optional article
      <*> annoHole type'

param :: Parser (OptionallyTypedName Name)
param =
  attachAnno $
    MkOptionallyTypedName emptyAnno
      <$> annoHole name
      <*> optional (annoLexeme (spacedToken_ TKIs) *> {- optional article *> -} annoHole type')

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
indentedExpr p =
  withIndent GT p $ \ _ -> do
    l <- currentLine
    e <- baseExpr
    efs <- many (expressionCont p)
    mw <- optional (whereExpr p)
    pure ((maybe id id mw) (combine End l e efs))

whereExpr :: Pos -> Parser (Expr Name -> Expr Name)
whereExpr p =
  withIndent GT p $ \ _ -> do
    ann <- opToken TKWhere
    ds <- many (indented localdecl p)
    pure (\ e -> Where (mkHoleAnnoFor e <> ann <> mkHoleAnnoFor ds) e ds)

data Stack a =
    Frame (Stack a) (a Name) (a Name -> a Name -> a Name) !Prio !Assoc !Pos !Pos
  | End

-- | This function decides how stack frames and continuations are combined
-- into a single expression.
--
-- For this, we take line and column information of various entities into
-- account. The general rule of thumb is: If items occur on a single line,
-- then normal precedence prevails. Similarly, if items occur on different
-- lines, but all start on the same column, then normal precedence should
-- prevail.
--
-- In other scenarios, we use the indentation for grouping.
--
-- Initially, the stack is empty, the given entity is the starting point,
-- and we have a number of continuation frames. We need the line of the
-- starting point.
--
-- The general process of moving through an expression is best demonstrated
-- by example:
--
-- a + b * c + d = e * f
--
-- We mark the focused position by [_], we write the stack to the left,
-- elements separated by commas, and the continuations to the right,
-- elements separated by commas.
--
-- Note that the stack and the continuations have exactly the same
-- structure.
--
-- [a] + b, * c,   + d,  = e, * f   -- empty stack, always push
-- a +  [b] * c,   + d,  = e, * f   -- * is stronger than + => push
-- a +, b *  [c]   + d,  = e, * f   -- + is weaker than * => pop
-- a +, [(b * c)]  + d,  = e, * f   -- + is like +, depends on associativity, left-associative => pop
-- [(a + (b * c))] + d,  = e, * f   -- empty stack, always push
-- (a + (b * c)) +  [d]  = e, * f   -- = is weaker than + => pop
-- [((a + (b * c)) + d)] = e, * f   -- empty stack, always push
-- ((a + (b * c)) + d) =  [e] * f   -- * is stronger than => push
-- ((a + (b * c)) + d) =, e *  [f]  -- empty continuation, always pop
-- ((a + (b * c)) + d) =, [(e * f)] -- empty continuation, always pop
-- [((a + (b * c)) + d) = (e * f)]  -- final result
--
-- The only difference between the process depicted above and the
-- real process is that the rules for which operator is stronger factor
-- in layout information. So in general, operators that are indented
-- more are seen to be stronger.
--
-- The exceptions are if either the whole expression is on the same line,
-- or if all operators are on the same column.
--
combine :: Stack a -> Pos -> a Name -> [Cont a] -> a Name

-- If we have a single expression and nothing on the stack, then we
-- can just return it.
combine End _l e [] = e

-- If there is no continuation, we always pop. The line passed to
-- combine is always the starting point of the currently focused
-- expression, so we take the line stored on the stack.
combine (Frame s e1 op _ _ l1 _) _l2 e2 [] =
  combine s l1 (e1 `op` e2) []

-- If there is nothing on the stack, we always push.
combine End l1 e1 (MkCont op1 prio1 assoc1 l2 p2 e2 : efs) =
  combine (Frame End e1 op1 prio1 assoc1 l1 p2) l2 e2 efs

-- This is the standard case. We have something on the stack and a
-- continuation. We have to compare the two operators. If the operator
-- on the continuation side is stronger, we push. Otherwise, we pop.
--
-- TODO: we currently do not track associativity.
--
combine s1@(Frame s e1 op1 prio1 assoc1 l1 p1) _l e2 (MkCont op2 prio2 assoc2 l2 p2 e3 : efs)
  | (l2, p2, prio2, assoc2) `stronger` (l1, p1, prio1, assoc1) =
  combine (Frame s1 e2 op2 prio2 assoc2 l2 p2) l2 e3 efs -- push
  | otherwise =
  combine s l1 (e1 `op1` e2) (MkCont op2 prio2 assoc2 l2 p2 e3 : efs) -- pop
  where
    stronger (ly, py, prioy, assocy) (lx, px, priox, assocx)
      | lx == ly  = priostronger
      | otherwise = py > px || py == px && priostronger
      where
        priostronger =
          case compare prioy priox of
            GT -> True
            EQ -> assocstronger
            LT -> False
        assocstronger =
          case (assocx, assocy) of
            (AssocRight, AssocRight) -> True
            (AssocLeft , AssocLeft ) -> False
            _                        -> True -- this should be a parse error, but we tolerate it and treat it as right-associative

-- Older thoughts on the operator layout parsing problem:
--
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
    { _op    :: a Name -> a Name -> a Name
    , _prio  :: !Prio
    , _assoc :: !Assoc
    , _line  :: !Pos
    , _pos   :: !Pos
    , _arg   :: a Name
    }

cont :: Parser (Prio, Assoc, a Name -> a Name -> a Name) -> Parser (a Name) -> Pos -> Parser (Cont a)
cont pop pbase p =
  withIndent GT p $ \ pos -> do
    l <- currentLine
    (prio, assoc, op) <- pop
    -- parg <- Lexer.indentGuard spaces GT p
    arg <- pbase
    pure (MkCont op prio assoc l pos arg)

-- TODO: We should think whether we can obtain this more cheaply.
currentLine :: Parser Pos
currentLine = sourceLine <$> getSourcePos

expressionCont :: Pos -> Parser (Cont Expr)
expressionCont = cont operator baseExpr

type Prio = Int
data Assoc = AssocLeft | AssocRight

-- TODO: My ad-hoc fix for multi-token operators can probably be done more elegantly.
operator :: Parser (Prio, Assoc, Expr Name -> Expr Name -> Expr Name)
operator =
      (\ op -> (2, AssocRight, infix2  Or        op)) <$> (spacedToken_ TKOr     <|> spacedToken_ TOr    )
  <|> (\ op -> (3, AssocRight, infix2  And       op)) <$> (spacedToken_ TKAnd    <|> spacedToken_ TAnd   )
  <|> (\ op -> (4, AssocRight, infix2  Equals    op)) <$> (spacedToken_ TKEquals <|> spacedToken_ TEquals)
  <|> (\ op -> (4, AssocRight, infix2' Leq       op)) <$> (try ((<>) <$> opToken TKAt <*> opToken TKMost) <|> opToken TLessEquals)
  <|> (\ op -> (4, AssocRight, infix2' Geq       op)) <$> (try ((<>) <$> opToken TKAt <*> opToken TKLeast) <|> opToken TGreaterEquals)
  <|> (\ op -> (4, AssocRight, infix2' Lt        op)) <$> ((<>) <$> opToken TKLess <*> opToken TKThan <|> opToken TKBelow <|> opToken TLessThan)
  <|> (\ op -> (4, AssocRight, infix2' Gt        op)) <$> ((<>) <$> opToken TKGreater <*> opToken TKThan <|> opToken TKAbove <|> opToken TGreaterThan)
  <|> (\ op -> (5, AssocRight, infix2' Cons      op)) <$> ((<>) <$> opToken TKFollowed <*> opToken TKBy)
  <|> (\ op -> (6, AssocLeft,  infix2  Plus      op)) <$> (spacedToken_ TKPlus   <|> spacedToken_ TPlus  )
  <|> (\ op -> (6, AssocLeft,  infix2  Minus     op)) <$> (spacedToken_ TKMinus  <|> spacedToken_ TMinus )
  <|> (\ op -> (7, AssocLeft,  infix2  Times     op)) <$> (spacedToken_ TKTimes  <|> spacedToken_ TTimes )
  <|> (\ op -> (7, AssocLeft,  infix2' DividedBy op)) <$> (((<>) <$> opToken TKDivided <*> opToken TKBy) <|> opToken TDividedBy)
  <|> (\ op -> (7, AssocLeft,  infix2  Modulo    op)) <$> spacedToken_ TKModulo

opToken :: TokenType -> Parser Anno
opToken t =
  (mkSimpleEpaAnno . lexToEpa) <$> spacedToken_ t

infix2 :: HasSrcRange (a n) => (Anno -> a n -> a n -> a n) -> Lexeme PosToken -> a n -> a n -> a n
infix2 f op l r =
  f (fixAnnoSrcRange $ mkHoleAnnoFor l <> mkSimpleEpaAnno (lexToEpa op) <> mkHoleAnnoFor r) l r

infix2' :: HasSrcRange (a n) => (Anno -> a n -> a n -> a n) -> Anno -> a n -> a n -> a n
infix2' f op l r =
  f (fixAnnoSrcRange $ mkHoleAnnoFor l <> op <> mkHoleAnnoFor r) l r

baseExpr :: Parser (Expr Name)
baseExpr =
      try projection
  <|> negation
  <|> ifthenelse
  <|> lam
  <|> consider
  <|> try namedApp -- This is not nice
  <|> app
  <|> lit
  <|> list
  <|> parenExpr

atomicExpr :: Parser (Expr Name)
atomicExpr =
      lit
  <|> nameAsApp App
  <|> parenExpr

nameAsApp :: (HasField "range" (AnnoToken b) SrcRange, HasAnno b, HasSrcRange a) => (Anno -> Name -> [a] -> b) -> Parser b
nameAsApp f =
  attachAnno $
    f emptyAnno
    <$> annoHole name
    <*> annoHole (pure [])

lit :: Parser (Expr Name)
lit = attachAnno $
  Lit emptyAnno <$> annoHole (numericLit <|> stringLit)

list :: Parser (Expr Name)
list = do
  current <- Lexer.indentLevel
  attachAnno $
    List emptyAnno
      <$  annoLexeme (spacedToken_ TKList)
      <*> annoHole (lsepBy (indentedExpr current) (spacedToken_ TComma))

numericLit :: Parser Lit
numericLit =
  attachAnno $
    NumericLit emptyAnno
      <$> annoEpa (spacedToken (fmap snd <$> preview #_TIntLit) "Numeric Literal")

stringLit :: Parser Lit
stringLit =
  attachAnno $
    StringLit emptyAnno
      <$> annoEpa (spacedToken (preview #_TStringLit) "String Literal")

parenExpr :: Parser (Expr Name)
parenExpr =
  inlineAnnoHole $
    id
    <$  annoLexeme (spacedToken_ TPOpen)
    <*> annoHole expr
    <*  annoLexeme (spacedToken_ TPClose)

-- | Parser for function application.
--
app :: Parser (Expr Name)
app = do
  current <- Lexer.indentLevel
  attachAnno $
    App emptyAnno
    <$> annoHole name
    <*> (   annoLexeme (spacedToken_ TKOf) *> annoHole (lsepBy1 (indentedExpr current) (spacedToken_ TComma)) -- (withIndent EQ current $ \ _ -> spacedToken_ TKAnd))
        <|> annoHole (lmany (indented atomicExpr current))
        )

namedApp :: Parser (Expr Name)
namedApp = do
  current <- Lexer.indentLevel
  attachAnno $
    AppNamed emptyAnno
    <$> annoHole name
    <*> (   annoLexeme (spacedToken_ TKWith) *> annoHole (lsepBy1 (namedExpr current) (spacedToken_ TComma))
        )
    <*> pure Nothing

namedExpr :: Pos -> Parser (NamedExpr Name)
namedExpr current  =
  attachAnno $
    MkNamedExpr emptyAnno
      <$> annoHole   name
      <*  annoLexeme (spacedToken_ TKIs)
      <*  optional article
      <*> annoHole   (indentedExpr current)

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
    Lam emptyAnno
      <$> annoHole givens
      <* annoLexeme (spacedToken_ TKYield)
      <*> annoHole (indentedExpr current)

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
    l <- currentLine
    pat <- basePattern
    pfs <- many (patternCont p)
    pure (combine End l pat pfs)

basePattern :: Parser (Pattern Name)
basePattern =
  patApp

atomicPattern :: Parser (Pattern Name)
atomicPattern =
  --    lit
  nameAsPatApp

nameAsPatApp :: Parser (Pattern Name)
nameAsPatApp =
  attachAnno $
    PatApp emptyAnno
    <$> annoHole name
    <*> annoHole (pure [])

patternCont :: Pos -> Parser (Cont Pattern)
patternCont = cont patOperator basePattern

patOperator :: Parser (Prio, Assoc, Pattern Name -> Pattern Name -> Pattern Name)
patOperator =
  (\ op -> (5, AssocRight, infix2' PatCons      op)) <$> ((\ l1 l2 -> mkSimpleEpaAnno (lexToEpa l1) <> mkSimpleEpaAnno (lexToEpa l2)) <$> spacedToken_ TKFollowed <*> spacedToken_ TKBy)

patApp :: Parser (Pattern Name)
patApp = do
  current <- Lexer.indentLevel
  attachAnno $
    PatApp emptyAnno
    <$> annoHole name
    <*> (      annoLexeme (spacedToken_ TKOf)
            *> annoHole (lsepBy (indented basePattern current) (spacedToken_ TComma))
        <|> annoHole (lmany (indented atomicPattern current))
        )

-- Some manual left-factoring here to prevent left-recursion
-- TODO: the interaction between projection and application has to be properly sorted out
projection :: Parser (Expr Name)
projection =
      -- TODO: should 'TGenitive' be part of 'Name' or 'Proj'?
      -- May affect the source span of the name.
      -- E.g. Goto definition of `name's` would be affected, as clicking on `'s` would not be part
      -- of the overall name source span. It is possible to implement this, but slightly annoying.
      (\ ae ns ->
        foldl'
          (\e (gen, n') ->
            Proj (fixAnnoSrcRange $ mkHoleAnnoFor e <> mkSimpleEpaAnno (lexToEpa gen) <> mkHoleAnnoFor n')
              e
              n'
          )
          ae -- (Var (fixAnnoSrcRange $ mkHoleAnnoFor n) n)
          ns
      )
  <$> atomicExpr
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

execParserForTokens :: Parser a -> String -> Text -> [PosToken] -> Either (NonEmpty PError) a
execParserForTokens p file input ts =
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
        { line = unPos $ sourceLine s
        , column = unPos $ sourceColumn s
        }
    , origin = orig
    }

-- ----------------------------------------------------------------------------
-- jl4 specific annotation helpers
-- ----------------------------------------------------------------------------

type WithAnno = WithAnno_ PosToken (Type' Resolved)

type Epa = Epa_ PosToken

type Lexeme = Lexeme_ PosToken

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
        if Foldable.null posTokens
          then Hidden
          else Visible
    }

mkHiddenCsnCluster :: (HasField "range" a SrcRange) => CsnCluster_ a
mkHiddenCsnCluster =
  CsnCluster
    { payload = mkConcreteSyntaxNode []
    , trailing = mkConcreteSyntaxNode []
    }

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

annoHole :: (HasSrcRange a) => Parser a -> Compose Parser (WithAnno_ t e) a
annoHole p = Compose $ fmap withHoleAnno p

annoEpa :: (HasField "range" t SrcRange) => Parser (Epa_ t a) -> Compose Parser (WithAnno_ t e) a
annoEpa p = Compose $ fmap withEpaAnno p

annoLexeme :: (HasField "range" t SrcRange) => Parser (Lexeme_ t t) -> Compose Parser (WithAnno_ t e) t
annoLexeme = annoEpa . fmap lexToEpa

annoLexemes :: (HasField "range" t SrcRange) => Parser (Lexeme_ t [t]) -> Compose Parser (WithAnno_ t e) [t]
annoLexemes = annoEpa . fmap lexesToEpa

instance Applicative (WithAnno_ t e) where
  pure a = WithAnno emptyAnno a
  WithAnno ps f <*> WithAnno ps2 x = WithAnno (ps <> ps2) (f x)

attachAnno :: (HasAnno a, AnnoToken a ~ t, AnnoExtra a ~ e) => Compose Parser (WithAnno_ t e) a -> Parser a
attachAnno p = fmap (\(WithAnno ann e) -> setAnno (fixAnnoSrcRange ann) e) $ getCompose p

attachEpa :: (HasAnno e, AnnoToken e ~ t, HasField "range" t SrcRange) => Parser (Epa_ t e) -> Parser e
attachEpa =
  attachAnno . annoEpa

-- | Replace the first occurrence of 'AnnoHole' with the exactprint annotations.
-- Removes an indirection in the Annotation tree.
inlineAnnoHole :: (HasAnno a, AnnoToken a ~ t, AnnoExtra a ~ e) => Compose Parser (WithAnno_ t e) a -> Parser a
inlineAnnoHole p = (\ (WithAnno ann e) -> setAnno (mkAnno (inlineFirstAnnoHole ann.payload (getAnno e).payload)) e) <$> getCompose p
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
