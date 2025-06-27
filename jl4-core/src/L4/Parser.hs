{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
module L4.Parser (
  -- * Public API
  parseFile,
  execParser,
  execParserForTokens,
  module',
  PError (..),
  mkPError,
  PState (..),

  -- * Debug combinators
  expr,
  spaceOrAnnotations,

  -- * High-level JL4 parser
  execProgramParser,
  execProgramParserForTokens,
) where

import Base

import qualified Control.Applicative as Applicative
import Generics.SOP.BasicFunctors
import Generics.SOP.NS
import qualified Data.List.Extra as List
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import GHC.Generics
import GHC.Records
import Optics
import Text.Megaparsec hiding (parseTest)
import qualified Text.Megaparsec.Char.Lexer as Lexer
import Text.Pretty.Simple

import L4.Annotation
import L4.Lexer as L
import qualified L4.Parser.ResolveAnnotation as Resolve
import qualified L4.ParserCombinators as P
import L4.Syntax hiding (app, forall', fun)
import L4.Parser.SrcSpan
import qualified Generics.SOP as SOP
import L4.Parser.Anno

type Parser = ReaderT Env (StateT PState (Parsec Void TokenStream))

newtype Env = Env
  { moduleUri :: NormalizedUri
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (SOP.Generic)

data PState = PState
  { comments :: [Comment]
  , nlgs :: [Nlg]
  , refs :: [Ref]
  }
  deriving stock (Show, Eq, Generic)
  deriving (Semigroup, Monoid) via Generically PState

addNlg :: Nlg -> PState -> PState
addNlg n s = over #nlgs (n:) s

addRef :: Ref -> PState -> PState
addRef ref s = over #refs (ref:) s

spaces :: Parser [PosToken]
spaces =
  takeWhileP (Just "space token") isSpaceToken

spaceOrAnnotations :: Parser (Lexeme ())
spaceOrAnnotations = do
  ws <- spaces
  nlgs :: [NS Epa [Ref, Nlg, ()]] <- many (fmap (S . S . Z) refAdditionalP <|> fmap (S . Z) nlgAnnotationP <|> fmap Z refP)
  traverse_ addNlgOrRef nlgs
  let
    epaNlgs = fmap (collapse_NS . map_NS (K . epaToHiddenCluster)) nlgs
  pure $ Lexeme
    { trailingTokens = ws
    , payload = ()
    , hiddenClusters = epaNlgs
    }

refP :: Parser (Epa Ref)
refP = do
  refExpr <- refAnnotationP
  pure $ fmap (MkRef (mkSimpleEpaAnno refExpr)) refExpr

nlgAnnotationP :: Parser (Epa Nlg)
nlgAnnotationP = do
  currentPosition <- getSourcePos
  moduleUri <- asks (.moduleUri)
  rawText <- hidden $ spacedTokenWs (\ case
    TAnnotations (TNlg t ty) -> Just $ toNlgAnno t ty
    _ -> Nothing)
    "Natural Language Annotation"

  let
    nlgParser :: Parser Nlg
    nlgParser =
      blockNlg <|> lineNlg

    blockNlg :: Parser Nlg
    blockNlg = do
      (open, a, close) <- P.between (plainToken $ TSymbols TNlgOpen) (plainToken $ TSymbols TNlgClose) (nlgFragment True)
      attachAnno $
        MkParsedNlg emptyAnno
          <$  annoEpa  (pure (tokenToEpa open))
          <*> annoHole (pure a)
          <*  annoEpa  (pure (tokenToEpa close))

    lineNlg :: Parser Nlg
    lineNlg = do
      attachAnno $
        MkParsedNlg emptyAnno
          <$  annoLexeme (spacedTokenWs_ $ TAnnotations TNlgPrefix)
          <*> annoHole   (nlgFragment False)

    nlgFragment :: Bool -> Parser [NlgFragment Name]
    nlgFragment isBlock = do
      many $
            try nameRefP
        <|> textFragment isBlock

    runNlgParserForInputAt initPos input = case execNlgLexer initPos moduleUri input of
      Left err -> do
        -- TODO: We should delay the parse error since parser failures in the
        -- annotation are not immediately fatal, we could report much more errors this way.
        -- Just like we when parsing fails
        fancyFailure $ Set.singleton $ ErrorFail $ errorBundlePretty err
      Right toks ->
        case execNlgParserForTokens (nlgParser <* eof) moduleUri rawText.payload toks of
          Left err -> do
            traverse_ registerParseError (bundleErrors err)
            attachAnno $
              MkInvalidNlg emptyAnno
                <$ annoEpa (pure $ TNlg <$> rawText)

          Right nlg ->
            pure nlg

  nlg <- runNlgParserForInputAt currentPosition rawText.payload
  case toNodesEither nlg of
    Left err -> do
      fancyFailure $ Set.singleton $
        ErrorFail $ "Internal error while parsing NLG annotation: " <> Text.unpack (prettyTraverseAnnoError err)
    Right csns -> do
      pure $ Epa
        { payload = nlg
        , original = concatMap allClusterTokens csns
        , trailingTokens = rawText.trailingTokens
        , hiddenClusters = []
        }

nameRefP :: Parser (NlgFragment Name)
nameRefP = do
  (open, n, close) <- P.between
    (hidden $ spacedSymbol_ $ TPercent)
    -- We don't want to consume trailing whitespace, because we would need to "reproduce"
    -- the whitespace during natural language generation. Otherwise, the text looks scuffed.
    -- Thus, only parse the 'TPercent' here, and let the 'textFragment' parser
    -- take care of any leading whitespace.
    (plainToken_ $ TSymbols TPercent)
    name
  attachAnno $
    MkNlgRef emptyAnno
      <$  annoLexeme (pure open)
      <*> annoHole   (pure n)
      <*  annoLexeme (pure close)

textFragment :: Bool -> Parser (NlgFragment Name)
textFragment isBlock = do
  when isBlock $ do
    notFollowedBy (plainToken (TSymbols TNlgClose) *> eof)
  attachAnno $
    MkNlgText emptyAnno
      <$> annoEpa (wrapInEpa <$> anySingle)
  where
    -- We replace any 'PosToken' we are parsing with 'TNlgString' to make sure
    -- it is highlighted correctly
    wrapInEpa :: PosToken -> Epa Text
    wrapInEpa posToken =
      displayTokenType . (.payload) <$> tokenToEpa (posToken & #payload %~ replaceTokenType)

    replaceTokenType :: TokenType -> TokenType
    replaceTokenType tt@(TSpaces (TSpace _)) = tt
    replaceTokenType tt = TAnnotations $ TNlgString $ displayTokenType tt

refAnnotationP :: Parser (Epa Text)
refAnnotationP = hidden $ spacedTokenWs (\ case
  TAnnotations (TRef t ty) -> Just $ toRefAnno t ty
  _ -> Nothing)
  "Reference Annotation"


-- TODO:
-- (1) should ref-src /ref-map be allowed anywhere else than at the toplevel
-- (2) should we add it to the AST at all? Currently we don't need it
refAdditionalP :: Parser (Epa ())
refAdditionalP = hidden $ spacedTokenWs (\ case
  TAnnotations (TRefSrc _t) -> Just ()
  TAnnotations (TRefMap _t) -> Just ()
  _ -> Nothing
  )
  "Reference source or map annotation"

lexeme :: Parser a -> Parser (Lexeme a)
lexeme p = do
  a <- p
  wsOrAnnotation <- spaceOrAnnotations
  pure $ Lexeme
    { payload = a
    , trailingTokens = wsOrAnnotation.trailingTokens
    , hiddenClusters = wsOrAnnotation.hiddenClusters
    }

addNlgOrRef :: NS Epa (Ref : Nlg : xs) -> Parser ()
addNlgOrRef = \ case
  S (Z nlg) -> modify' (addNlg nlg.payload)
  Z ref -> modify' (addRef ref.payload)
  _ -> pure ()

lexemeWs :: Parser a -> Parser (Lexeme a)
lexemeWs p = do
  a <- p
  trailingTokens <- spaces
  pure $ Lexeme
    { trailingTokens = trailingTokens
    , payload = a
    , hiddenClusters = []
    }

spacedTokenWs :: (TokenType -> Maybe a) -> String -> Parser (Epa a)
spacedTokenWs cond lbl =
  lexToEpa' <$>
    lexemeWs
      (token
        (\ t -> (t,) <$> cond t.payload)
        Set.empty
      )
    <?> lbl

spacedTokenWs_ :: TokenType -> Parser (Lexeme PosToken)
spacedTokenWs_ tt =
  lexemeWs (plainToken tt)

plainToken :: TokenType -> Parser PosToken
plainToken tt = do
  uri <- asks (.moduleUri)
  token
    (\ t -> if computedPayload t == tt then Just t else Nothing)
    (Set.singleton (Tokens (L.trivialToken uri tt :| [])))

plainToken_ :: TokenType -> Parser (Lexeme PosToken)
plainToken_ tt =
  mkLexeme [] <$> plainToken tt

spacedKeyword_ :: TKeywords -> Parser (Lexeme PosToken)
spacedKeyword_ tt = spacedToken_ (TKeywords tt)

spacedSymbol_ :: TSymbols -> Parser (Lexeme PosToken)
spacedSymbol_ tt = spacedToken_ (TSymbols tt)

spacedToken_ :: TokenType -> Parser (Lexeme PosToken)
spacedToken_ tt =
  lexeme (plainToken tt)

spacedToken :: Is k An_AffineFold => Optic' k is TokenType a -> String -> Parser (Epa a)
spacedToken cond lbl =
  lexToEpa' <$>
    lexeme
      (token
        (\ t -> (t,) <$> preview cond (computedPayload t))
        Set.empty
      )
    <?> lbl

(<<$>>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
fab <<$>> fga = (fmap . fmap) fab fga

-- | A quoted identifier between backticks.
quotedName :: Parser (Epa Name)
quotedName =
  (MkName emptyAnno . NormalName) <<$>> spacedToken (#_TIdentifiers % #_TQuoted) "quoted identifier"

simpleName :: Parser (Epa Name)
simpleName =
  (MkName emptyAnno . NormalName) <<$>> spacedToken (#_TIdentifiers % #_TIdentifier) "identifier"

qualifiedName :: Parser (Epa Name)
qualifiedName = do
  -- TODO: in future we may also want to allow `tokOf #_TQuoted`
  let nameAndQualifiers = List.unsnoc . mapMaybe (either (const Nothing) (Just . snd))
  res@(nameAndQualifiers -> Just (q : qs, n)) <- do
    x <- tokOf $ #_TIdentifiers % #_TIdentifier
    dotOrIdentifier <- some do
      d <- tokOf $ #_TSymbols % #_TDot
      i <- tokOf $ #_TIdentifiers % #_TIdentifier
      pure [Left $ fst d, Right i]
    pure $ (Right x :) $ mconcat dotOrIdentifier
  wsOrAnnotation <- spaceOrAnnotations
  let e = Epa
        { original = map (either id fst) res
        , trailingTokens = wsOrAnnotation.trailingTokens
        , payload = QualifiedName (q :| qs) n
        , hiddenClusters = wsOrAnnotation.hiddenClusters
        }
  pure $ MkName emptyAnno <$> e

 where
 tokOf p = token (\t -> (t,) <$> preview p (computedPayload t)) Set.empty

name :: Parser Name
name = attachEpa (quotedName <|> try qualifiedName <|> simpleName) <?> "identifier"

tokenAsName :: TokenType -> Parser Name
tokenAsName tt =
  attachEpa (lexToEpa' . fmap convert <$> spacedToken_ tt)
  where
    convert :: PosToken -> (PosToken, Name)
    convert p@(MkPosToken range _) = (MkPosToken range (TIdentifiers $ TIdentifier t), MkName emptyAnno (NormalName t))
      where
        t = displayPosToken p

indented :: Parser b -> Pos -> Parser b
indented parser pos =
  withIndent GT pos $ \ _ -> parser

indented' :: AnnoParser b -> Pos -> AnnoParser b
indented' parser pos = wrapAnnoParser $ indented (unwrapAnnoParser parser) pos

module' :: NormalizedUri -> Parser (Module Name)
module' uri = do
  attachAnno $
    MkModule emptyAnno uri
      <$  annoLexeme_ spaceOrAnnotations
      <*> annoHole anonymousSection

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
      <$> annoHole (pure Nothing)
      <*> annoHole (pure Nothing)
      <*> annoHole (lsepBy (topdeclWithRecovery 0) (spacedSymbol_ TSemicolon))

section :: Int -> Parser (Section Name)
section n = attachAnno $
  MkSection emptyAnno
    <$> (wrapAnnoParser (try do
           wa@WithAnno {payload = syms} <- unwrapAnnoParser sectionSymbols
           guard (syms >= n)
           pure wa) *> annoHole (optional name)
        )
    <*> annoHole (optional aka)
    <*> annoHole (lsepBy (topdeclWithRecovery n) (spacedSymbol_ TSemicolon))

sectionSymbols :: AnnoParser Int
sectionSymbols =
  length <$> Applicative.some (annoLexeme (spacedSymbol_ TParagraph))

topdeclWithRecovery :: Int -> Parser (TopDecl Name)
topdeclWithRecovery n = do
  start <- lookAhead anySingle
  withRecovery
    (\e -> do
      -- Ignoring tokens here is fine, as we will fail parsing any way.
      _ <- takeWhileP Nothing (\t -> not (isSpaceToken t) && t.range.start.column > 1)
      current <- lookAhead anySingle
      registerParseError e
      -- If we didn't make any progress whatsoever,
      -- end parsing to avoid endless loops.
      if start == current
        then parseError e
        else topdeclWithRecovery n

    )
    topdecl
      <|> attachAnno (Section   emptyAnno <$> annoHole (section (n + 1)))

topdecl :: Parser (TopDecl Name)
topdecl =
  withTypeSig (\ sig -> attachAnno $
        Declare   emptyAnno <$> annoHole (declare sig)
    <|> Decide    emptyAnno <$> annoHole (decide sig)
    <|> Assume    emptyAnno <$> annoHole (assume sig)
  ) <|> attachAnno
        (Directive emptyAnno <$> annoHole directive)
    <|> attachAnno
        (Import    emptyAnno <$> annoHole import')

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
directive =
  attachAnno $
    choice
      [ LazyEval emptyAnno
          <$ annoLexeme (spacedToken_ (TDirectives TLazyEvalDirective))
          <*> annoHole expr
      , LazyEvalTrace emptyAnno
          <$ annoLexeme (spacedToken_ (TDirectives TLazyEvalTraceDirective))
          <*> annoHole expr
      , Check emptyAnno
          <$ annoLexeme (spacedToken_ (TDirectives TCheckDirective))
          <*> annoHole expr
      , Contract emptyAnno
          <$ annoLexeme (spacedToken_ (TDirectives TContractDirective))
          <*> annoHole expr
          <* optional (annoLexeme (spacedKeyword_ TKStarting))
          <* annoLexeme (spacedKeyword_ TKAt)
          <*> annoHole expr
          <* annoLexeme (spacedKeyword_ TKWith)
          <*> contractEvents
      , Assert emptyAnno
          <$ annoLexeme (spacedToken_ (TDirectives TAssertDirective))
          <*> annoHole expr
      ]

contractEvents :: AnnoParser [Expr Name]
contractEvents = annoHole $ lmany expr

import' :: Parser (Import Name)
import' =
  attachAnno $
    MkImport emptyAnno
      <$  annoLexeme (spacedKeyword_ TKImport)
      <*> annoHole name
      <*> pure Nothing

assume :: TypeSig Name -> Parser (Assume Name)
assume sig = do
  current <- Lexer.indentLevel
  attachAnno $
    MkAssume emptyAnno
      <$> annoHole (pure sig)
      <*  annoLexeme (spacedKeyword_ TKAssume)
      <*> annoHole appForm
      <*> optional (annoLexeme (spacedKeyword_ TKIs) *> {- optional article *> -} annoHole (indented type' current))

declare :: TypeSig Name -> Parser (Declare Name)
declare sig =
  attachAnno $
    MkDeclare emptyAnno
      <$> annoHole (pure sig)
      <*  annoLexeme (spacedKeyword_ TKDeclare)
      <*> annoHole appForm
      <*> annoHole typeDecl

typeDecl :: Parser (TypeDecl Name)
typeDecl =
  recordDecl <|> enumOrSynonymDecl

recordDecl :: Parser (TypeDecl Name)
recordDecl =
  attachAnno $
    RecordDecl emptyAnno
      <$> annoHole (pure Nothing)
      <*> recordDecl'

recordDecl' :: AnnoParser [TypedName Name]
recordDecl' =
     annoLexeme (spacedKeyword_ TKHas)
  *> annoHole (lsepBy reqParam (spacedSymbol_ TComma))

enumOrSynonymDecl :: Parser (TypeDecl Name)
enumOrSynonymDecl =
  attachAnno $
       annoLexeme (spacedKeyword_ TKIs)
    *> (enumDecl <|> synonymDecl)

separator :: Parser (Lexeme PosToken)
separator = spacedKeyword_ TKIs <|> hidden (spacedSymbol_ TColon)

enumDecl :: AnnoParser (TypeDecl Name)
enumDecl =
  EnumDecl emptyAnno
    <$  annoLexeme (spacedKeyword_ TKOne)
    <*  annoLexeme (spacedKeyword_ TKOf)
    <*> annoHole (lsepBy conDecl (spacedSymbol_ TComma))

synonymDecl :: AnnoParser (TypeDecl Name)
synonymDecl =
  SynonymDecl emptyAnno
    <$> annoHole type'

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
          <*  annoLexeme (spacedKeyword_ TKDecide)
          <*> annoHole appForm
          <*  annoLexeme (spacedKeyword_ TKIs <|> spacedKeyword_ TKIf)
          <*> annoHole (indentedExpr current)

    meansKW current =
      attachAnno $
        MkDecide emptyAnno
          <$> annoHole (pure sig)
          <*> annoHole appForm
          <*  annoLexeme (spacedKeyword_ TKMeans)
          <*> annoHole (indentedExpr current)

appForm :: Parser (AppForm Name)
appForm = do
  current <- Lexer.indentLevel
  attachAnno $
    MkAppForm emptyAnno
      <$> annoHole name
      <*> (   annoLexeme (spacedKeyword_ TKOf) *> annoHole (lsepBy1 name (spacedSymbol_ TComma))
          <|> annoHole (lmany (indented name current))
          )
      <*> annoHole (optional aka)

aka :: Parser (Aka Name)
aka =
  attachAnno $
    MkAka emptyAnno
      <$  annoLexeme (spacedKeyword_ TKAka)
      <*> annoHole (lsepBy name (spacedSymbol_ TComma))

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
      <$  annoLexeme (spacedKeyword_ TKGiven)
      <*> annoHole (lsepBy param (spacedSymbol_ TComma))

giveth :: Parser (GivethSig Name)
giveth = do
  current <- Lexer.indentLevel
  attachAnno $
    MkGivethSig emptyAnno
      <$  annoLexeme (spacedKeyword_ TKGiveth)
--      <*  optional article
      <*> annoHole (indented type' current)

-- This isn't ideal, because it says an expr must be indented
-- (and `mkPos` does not allow 0).
--
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
  <|> paren type'

typeKind :: Parser (Type' Name)
typeKind =
  attachAnno $
  Type emptyAnno <$ annoLexeme (spacedKeyword_ TKType)

atomicType :: Parser (Type' Name)
atomicType =
      withOptionalArticle
      (   typeKind
      <|> nameAsApp TyApp
      )
  <|> paren type'

paren :: (AnnoToken a ~ PosToken, HasAnno a, HasSrcRange a) => Parser a -> Parser a
paren p =
  inlineAnnoHole $
    id
    <$  annoLexeme (spacedSymbol_ TPOpen)
    <*> annoHole p
    <*  annoLexeme (spacedSymbol_ TPClose)

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
    <$> annoHole (tokenAsName (TKeywords TKList) <|> name)
    <*> (   annoLexeme (spacedKeyword_ TKOf) *> annoHole (lsepBy1 (indented type' current) (spacedSymbol_ TComma))
        <|> annoHole (lmany (indented atomicType current))
        )

fun :: Parser (Type' Name)
fun = do
  current <- Lexer.indentLevel
  attachAnno $
    Fun emptyAnno
    <$  annoLexeme (spacedKeyword_ TKFunction)
    <*  annoLexeme (spacedKeyword_ TKFrom)
    <*> annoHole (lsepBy1 (indented optionallyNamedType current) (spacedKeyword_ TKAnd))
    <*  annoLexeme (spacedKeyword_ TKTo)
    <*> annoHole (indented type' current)

forall' :: Parser (Type' Name)
forall' = do
  -- current <- Lexer.indentLevel
  attachAnno $
    Forall emptyAnno
    <$  annoLexeme (spacedKeyword_ TKFor)
    <*  annoLexeme (spacedKeyword_ TKAll)
    <*> annoHole (lsepBy1 name (spacedKeyword_ TKAnd))
--    <*  optional article
    <*> annoHole type' -- (indented type' current)

article :: AnnoParser PosToken
article =
  annoLexeme (spacedKeyword_ TKA <|> spacedKeyword_ TKAn <|> spacedKeyword_ TKThe)

withOptionalArticle :: (HasSrcRange a, HasAnno a, AnnoToken a ~ PosToken, AnnoExtra a ~ Extension) => Parser a -> Parser a
withOptionalArticle p =
  inlineAnnoHole $
    id
    <$  optional article
    <*> annoHole p

{-
enumType :: AnnoParser (Type' Name)
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
    ps <- P.sepBy1 pp sep
    pure $ P.zipSepBy1 id zipAnno ps
  where
    zipAnno :: a -> Lexeme_ (AnnoToken a) (AnnoToken a) -> a
    zipAnno p s = setAnno (fixAnnoSrcRange $ getAnno p <> mkSimpleEpaAnno (lexToEpa s)) p

lsepBy1 :: forall a. (HasAnno a, HasField "range" (AnnoToken a) SrcRange) => Parser a -> Parser (Lexeme_ (AnnoToken a) (AnnoToken a)) -> Parser [a]
lsepBy1 pp sep =
  fmap concat $ someLines $ do
    ps <- P.sepBy1 pp sep
    pure $ P.zipSepBy1 id zipAnno ps
  where
    zipAnno :: a -> Lexeme_ (AnnoToken a) (AnnoToken a) -> a
    zipAnno p s = setAnno (fixAnnoSrcRange $ getAnno p <> mkSimpleEpaAnno (lexToEpa s)) p

reqParam :: Parser (TypedName Name)
reqParam =
  attachAnno $
    MkTypedName emptyAnno
      <$> annoHole name
      <*  annoLexeme separator
--      <*  optional article
      <*> annoHole type'

param :: Parser (OptionallyTypedName Name)
param =
  attachAnno $
    MkOptionallyTypedName emptyAnno
      <$> annoHole name
      <*> optional (annoLexeme separator *> {- optional article *> -} annoHole type')

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
    ann <- opToken $ TKeywords TKWhere
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
-- NOTE: The line which is the second argument of combine is the line
-- of the operand (not operator) currently under consideration.
--
-- The line stored on top of the stack is the line where the previous
-- expression starts, the line on top of the continuations is the line
-- where the subsequent expression continues.
--
-- Operators are considered to be "on the same line" if both operands
-- are on the same line, meaning that both the top stack frame and the
-- top continuation frame have to have the same line number to make
-- a real same-line choice.
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
combine s1@(Frame s e1 op1 prio1 assoc1 l1 p1) l e2 (MkCont op2 prio2 assoc2 l2 p2 e3 : efs)
  | (l2, p2, prio2, assoc2) `stronger` (l1, p1, prio1, assoc1) =
  combine (Frame s1 e2 op2 prio2 assoc2 l p2) l2 e3 efs -- push
  -- NOTE: the topmost frame now starts with e2, thus at line l
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
    (prio, assoc, op) <- pop
    -- parg <- Lexer.indentGuard spaces GT p
    l <- currentLine
    arg <- pbase
    pure (MkCont op prio assoc l pos arg) -- the line we store is the line of the argument, not the operator

-- TODO: We should think whether we can obtain this more cheaply.
currentLine :: Parser Pos
currentLine = sourceLine <$> getSourcePos

expressionCont :: Pos -> Parser (Cont Expr)
expressionCont = cont operator baseExpr

postfixP :: Parser (a -> a) -> Parser a -> Parser a
postfixP ops p = do
  a <- p
  mf <- optional ops
  case mf of
    Nothing -> pure a
    Just f -> pure $ f a

type Prio = Int
data Assoc = AssocLeft | AssocRight

-- TODO: My ad-hoc fix for multi-token operators can probably be done more elegantly.
operator :: Parser (Prio, Assoc, Expr Name -> Expr Name -> Expr Name)
operator =
      (\ op -> (1, AssocRight, infix2  Implies   op)) <$> (spacedKeyword_ TKImplies <|> spacedTokenOp_ TImplies )
  <|> (\ op -> (2, AssocRight, infix2  Or        op)) <$> (spacedKeyword_ TKOr      <|> spacedTokenOp_ TOr      )
  <|> (\ op -> (3, AssocRight, infix2  And       op)) <$> (spacedKeyword_ TKAnd     <|> spacedTokenOp_ TAnd     )
  <|> (\ op -> (2, AssocRight, infix2  ROr       op)) <$> spacedKeyword_ TKROr
  <|> (\ op -> (3, AssocRight, infix2  RAnd      op)) <$> spacedKeyword_ TKRAnd
  <|> (\ op -> (4, AssocRight, infix2  Equals    op)) <$> (spacedKeyword_ TKEquals <|> spacedTokenOp_ TEquals)
  <|> (\ op -> (4, AssocRight, infix2' Leq       op)) <$> (try ((<>) <$> opToken (TKeywords TKAt) <*> opToken (TKeywords TKMost)) <|> opToken (TOperators TLessEquals))
  <|> (\ op -> (4, AssocRight, infix2' Geq       op)) <$> (try ((<>) <$> opToken (TKeywords TKAt) <*> opToken (TKeywords TKLeast)) <|> opToken (TOperators TGreaterEquals))
  <|> (\ op -> (4, AssocRight, infix2' Lt        op)) <$> ((<>) <$> opToken (TKeywords TKLess) <*> opToken (TKeywords TKThan) <|> opToken (TKeywords TKBelow) <|> opToken (TOperators TLessThan))
  <|> (\ op -> (4, AssocRight, infix2' Gt        op)) <$> ((<>) <$> opToken (TKeywords TKGreater) <*> opToken (TKeywords TKThan) <|> opToken (TKeywords TKAbove) <|> opToken (TOperators TGreaterThan))
  <|> (\ op -> (5, AssocRight, infix2' Cons      op)) <$> ((<>) <$> opToken (TKeywords TKFollowed) <*> opToken (TKeywords TKBy))
  <|> (\ op -> (6, AssocLeft,  infix2  Plus      op)) <$> (spacedKeyword_ TKPlus   <|> spacedTokenOp_ TPlus  )
  <|> (\ op -> (6, AssocLeft,  infix2  Minus     op)) <$> (spacedKeyword_ TKMinus  <|> spacedTokenOp_ TMinus )
  <|> (\ op -> (7, AssocLeft,  infix2  Times     op)) <$> (spacedKeyword_ TKTimes  <|> spacedTokenOp_ TTimes )
  <|> (\ op -> (7, AssocLeft,  infix2' DividedBy op)) <$> (((<>) <$> opToken (TKeywords TKDivided) <*> opToken (TKeywords TKBy)) <|> opToken (TOperators TDividedBy))
  <|> (\ op -> (7, AssocLeft,  infix2  Modulo    op)) <$> spacedKeyword_ TKModulo
  where spacedTokenOp_ = spacedToken_ . TOperators

postfixOperator :: Parser (Expr Name -> Expr Name)
postfixOperator =
      (\ op -> (postfix Percent   op)) <$> (spacedSymbol_ TPercent)

opToken :: TokenType -> Parser Anno
opToken t =
  (mkSimpleEpaAnno . lexToEpa) <$> spacedToken_ t

infix2 :: HasSrcRange (a n) => (Anno -> a n -> a n -> a n) -> Lexeme PosToken -> a n -> a n -> a n
infix2 f op l r =
  f (fixAnnoSrcRange $ mkHoleAnnoFor l <> mkSimpleEpaAnno (lexToEpa op) <> mkHoleAnnoFor r) l r

infix2' :: HasSrcRange (a n) => (Anno -> a n -> a n -> a n) -> Anno -> a n -> a n -> a n
infix2' f op l r =
  f (fixAnnoSrcRange $ mkHoleAnnoFor l <> op <> mkHoleAnnoFor r) l r

postfix :: HasSrcRange (a n) => (Anno -> a n -> a n) -> Lexeme PosToken -> a n -> a n
postfix f op l =
  f (fixAnnoSrcRange $ mkHoleAnnoFor l <> mkSimpleEpaAnno (lexToEpa op)) l

baseExpr :: Parser (Expr Name)
baseExpr = postfixP postfixOperator baseExpr'

baseExpr' :: Parser (Expr Name)
baseExpr' =
      try projection
  <|> negation
  <|> ifthenelse
  <|> multiWayIf
  <|> try event
  <|> regulative
  <|> lam
  <|> consider
  <|> try namedApp -- This is not nice
  <|> app
  <|> lit
  <|> list
  <|> paren expr

event :: Parser (Expr Name)
event = attachAnno $ Event emptyAnno <$> annoHole parseEvent

parseEvent :: Parser (Event Name)
parseEvent =
  attachAnno (MkEvent emptyAnno <$> parseParty <*> parseDoes <*> parseAt <*> pure False)
  <|> attachAnno do
    -- NOTE: allow to specify AT first, without breaking backwards
    -- compatibility
    timestamp <- parseAt
    party <- parseParty
    action <- parseDoes
    pure MkEvent {anno = emptyAnno, atFirst = True, ..}
  where
    parseParty =
      annoLexeme (spacedKeyword_ TKParty)
      *> annoHole expr
    parseDoes =
      annoLexeme (spacedKeyword_ TKDoes)
      *> annoHole expr
    parseAt =
      annoLexeme (spacedKeyword_ TKAt)
      *> annoHole expr

atomicExpr :: Parser (Expr Name)
atomicExpr = postfixP postfixOperator atomicExpr'

atomicExpr' :: Parser (Expr Name)
atomicExpr' =
      lit
  <|> nameAsApp App
  <|> paren expr

nameAsApp :: (HasField "range" (AnnoToken b) SrcRange, HasAnno b, HasSrcRange a) => (Anno -> Name -> [a] -> b) -> Parser b
nameAsApp f =
  attachAnno $
    f emptyAnno
    <$> annoHole name
    <*> annoHole (pure [])

lit :: Parser (Expr Name)
lit = attachAnno $
  Lit emptyAnno <$> annoHole rawLit

rawLit :: Parser Lit
rawLit = try decimalLit <|> intLit <|> stringLit

list :: Parser (Expr Name)
list = do
  current <- Lexer.indentLevel
  attachAnno $
    List emptyAnno
      <$  annoLexeme (spacedKeyword_ TKList)
      <*> annoHole (lsepBy (indentedExpr current) (spacedSymbol_ TComma))

intLit :: Parser Lit
intLit =
  attachAnno $
    NumericLit emptyAnno
      <$> annoEpa (spacedToken (#_TLiterals % #_TIntLit % _2 % Optics.to fromIntegral) "Numeric Literal")

decimalLit :: Parser Lit
decimalLit =
  attachAnno $
    NumericLit emptyAnno
      <$> annoEpa (spacedToken (#_TLiterals % #_TRationalLit % _2) "Float Literal")

stringLit :: Parser Lit
stringLit =
  attachAnno $
    StringLit emptyAnno
      <$> annoEpa (spacedToken (#_TLiterals % #_TStringLit) "String Literal")

-- | Parser for function application.
--
app :: Parser (Expr Name)
app = do
  current <- Lexer.indentLevel
  attachAnno $
    App emptyAnno
    <$> annoHole name
    <*> (   annoLexeme (spacedKeyword_ TKOf) *> annoHole (lsepBy1 (indentedExpr current) (spacedSymbol_ TComma)) -- (withIndent EQ current $ \ _ -> spacedToken_ TKAnd))
        <|> annoHole (lmany (indented atomicExpr current))
        )

namedApp :: Parser (Expr Name)
namedApp = do
  current <- Lexer.indentLevel
  attachAnno $
    AppNamed emptyAnno
    <$> annoHole name
    <*> (   annoLexeme (spacedKeyword_ TKWith) *> annoHole (lsepBy1 (namedExpr current) (spacedSymbol_ TComma))
        )
    <*> pure Nothing

namedExpr :: Pos -> Parser (NamedExpr Name)
namedExpr current  =
  attachAnno $
    MkNamedExpr emptyAnno
      <$> annoHole   name
      <*  annoLexeme separator
      <*  optional article
      <*> annoHole   (indentedExpr current)

negation :: Parser (Expr Name)
negation = do
  current <- Lexer.indentLevel
  attachAnno $
    Not emptyAnno
      <$  annoLexeme (spacedKeyword_ TKNot)
      <*  optional (annoLexeme (spacedKeyword_ TKOf))
      <*> annoHole (indentedExpr current)

lam :: Parser (Expr Name)
lam = do
  current <- Lexer.indentLevel
  attachAnno $
    Lam emptyAnno
      <$> annoHole givens
      <* annoLexeme (spacedKeyword_ TKYield)
      <*> annoHole (indentedExpr current)

ifthenelse :: Parser (Expr Name)
ifthenelse = do
  current <- Lexer.indentLevel
  attachAnno $
    IfThenElse emptyAnno
      <$  annoLexeme (spacedKeyword_ TKIf)
      <*> annoHole (indentedExpr current)
      <*  annoLexeme (spacedKeyword_ TKThen)
      <*> annoHole (indentedExpr current)
      <*  annoLexeme (spacedKeyword_ TKElse)
      <*> annoHole (indentedExpr current)

-- NOTE: this is a bit subtle: each of the
-- indents is scoped over only one token,
-- so we need to be careful to apply it to
-- each of them
multiWayIf :: Parser (Expr Name)
multiWayIf = do
  current <- Lexer.indentLevel
  attachAnno do
    _ <- annoLexeme (spacedKeyword_ TKBranch)
    let ind = flip indented' current
    MultiWayIf emptyAnno
      <$> annoHole (many (parseGuardedExpr current))
      <*> ind do
        annoLexeme (spacedKeyword_ TKOtherwise)
          *> annoHole (indentedExpr current)

parseGuardedExpr :: Pos -> Parser (GuardedExpr Name)
parseGuardedExpr pos = attachAnno $
  MkGuardedExpr emptyAnno
    <$> ind do
       annoLexeme (spacedKeyword_ TKIf)
        *> annoHole (indentedExpr pos)
    <*> ind do
       annoLexeme (spacedKeyword_ TKThen)
        *> annoHole (indentedExpr pos)
  where
  ind = flip indented' pos

regulative :: Parser (Expr Name)
regulative = attachAnno $
  Regulative emptyAnno <$> annoHole obligation

optionalWithHole :: HasSrcRange a => AnnoParser a -> AnnoParser (Maybe a)
optionalWithHole p = Just <$> p <|> annoHole (pure Nothing)

obligation :: Parser (Obligation Name)
obligation = do
  current <- Lexer.indentLevel
  attachAnno $
    MkObligation emptyAnno
      <$  annoLexeme (spacedKeyword_ TKParty)
      <*> annoHole (indentedExpr current)
      <*> annoHole (must current)
      <*> optionalWithHole (deadline current)
      <*> optionalWithHole (hence current)
      <*> optionalWithHole (lest current)

must :: Pos -> Parser (RAction Name)
must current = attachAnno $
   MkAction emptyAnno
     <$> asum
      [ annoLexeme (spacedKeyword_ TKMust)
        *> optional (annoLexeme (spacedKeyword_ TKDo))
        *> annoHole (indentedPattern current)
      , annoLexeme (spacedKeyword_ TKMay)
        *> optional (annoLexeme (spacedKeyword_ TKDo))
        *> annoHole (indentedPattern current)
      , annoLexeme (spacedKeyword_ TKDo)
        *> annoHole (indentedPattern current)
      ]
     <*> optionalWithHole do
      annoLexeme (spacedKeyword_ TKProvided)
        *> annoHole (indentedExpr current)

deadline :: Pos -> AnnoParser (Expr Name)
deadline current =
  annoLexeme (spacedKeyword_ TKWithin) *> annoHole (indentedExpr current)

hence :: Pos -> AnnoParser (Expr Name)
hence current =
  annoLexeme (spacedKeyword_ TKHence) *> annoHole (indentedExpr current)

lest :: Pos -> AnnoParser (Expr Name)
lest current =
  annoLexeme (spacedKeyword_ TKLest) *> annoHole (indentedExpr current)

consider :: Parser (Expr Name)
consider = do
  current <- Lexer.indentLevel
  attachAnno $
    Consider emptyAnno
      <$  annoLexeme (spacedKeyword_ TKConsider)
      <*> annoHole (indentedExpr current)
      <*> annoHole (lsepBy branch (spacedSymbol_ TComma))

branch :: Parser (Branch Name)
branch =
  when' <|> otherwise'

when' :: Parser (Branch Name)
when' = do
  current <- Lexer.indentLevel
  attachAnno $
    When emptyAnno
      <$  annoLexeme (spacedKeyword_ TKWhen)
      <*> annoHole (indentedPattern current)
      <*  annoLexeme (spacedKeyword_ TKThen)
      <*> annoHole (indentedExpr current)

otherwise' :: Parser (Branch Name)
otherwise' = do
  current <- Lexer.indentLevel
  attachAnno $
    Otherwise emptyAnno
      <$  annoLexeme (spacedKeyword_ TKOtherwise)
      <*> annoHole (indentedExpr current)

indentedPattern :: Pos -> Parser (Pattern Name)
indentedPattern p =
  withIndent GT p $ \ _ -> do
    l <- currentLine
    pat <- basePattern
    pfs <- many (patternCont p)
    pure (combine End l pat pfs)

-- This isn't ideal, because it says a pattern must be indented
-- (and 'mkPos' does not allow 0).
--
-- See also 'expr'.
--
pattern' :: Parser (Pattern Name)
pattern' =
  indentedPattern (mkPos 1)


basePattern :: Parser (Pattern Name)
basePattern =
  patLit
  <|> patExpr
  <|> patApp
  <|> paren pattern'

atomicPattern :: Parser (Pattern Name)
atomicPattern =
  patLit
  <|> patExpr
  <|> nameAsPatApp
  <|> paren pattern'

patLit :: Parser (Pattern Name)
patLit = attachAnno $ PatLit emptyAnno <$> annoHole rawLit

patExpr :: Parser (Pattern Name)
patExpr = attachAnno $
  PatExpr emptyAnno
    <$> do
      annoLexeme (spacedKeyword_ TKExact)
        *> annoHole expr

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
  (\ op -> (5, AssocRight, infix2' PatCons      op)) <$> ((\ l1 l2 -> mkSimpleEpaAnno (lexToEpa l1) <> mkSimpleEpaAnno (lexToEpa l2)) <$> spacedKeyword_ TKFollowed <*> spacedKeyword_ TKBy)

patApp :: Parser (Pattern Name)
patApp = do
  current <- Lexer.indentLevel
  attachAnno $
    PatApp emptyAnno
    <$> annoHole name
    <*> (      annoLexeme (spacedKeyword_ TKOf)
            *> annoHole (lsepBy (indented basePattern current) (spacedSymbol_ TComma))
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
  <*> some ((,) <$> spacedToken_ (TIdentifiers TGenitive) <*> name)

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

-- ----------------------------------------------------------------------------
-- Nlg Annotation parsers.
-- Parse NLG annotations such that we can process them later.
-- ----------------------------------------------------------------------------

execNlgParserForTokens :: Parser a -> NormalizedUri -> Text -> [PosToken] -> Either (ParseErrorBundle TokenStream Void) a
execNlgParserForTokens p uri input ts =
  case runJl4Parser env st p (showNormalizedUri uri) stream of
    Left err -> Left err
    Right (a, _pstate) -> Right a
  where
    env = Env
      { moduleUri = uri
      }
    st = PState
      { nlgs = []
      , comments = []
      , refs = []
      }
    stream = MkTokenStream (Text.unpack input) ts

-- ----------------------------------------------------------------------------
-- JL4 parsers
-- ----------------------------------------------------------------------------

execParser :: Resolve.HasNlg a => Parser a -> NormalizedUri -> Text -> Either (NonEmpty PError) (a, [Resolve.Warning], PState)
execParser p uri input =
  case execLexer uri input of
    Left errs -> Left errs
    -- TODO: we should probably push in the uri even further.
    Right ts -> execParserForTokens p uri input ts

execParserForTokens :: Resolve.HasNlg a => Parser a -> NormalizedUri -> Text -> [PosToken] -> Either (NonEmpty PError) (a, [Resolve.Warning], PState)
execParserForTokens p file input ts =
  case runJl4Parser env st p (showNormalizedUri file) stream  of
    Left err -> Left (fmap (mkPError "parser") $ errorBundleToErrorMessages err)
    Right (a, pstate)  ->
      let
        (annotatedA, nlgS) = Resolve.addNlgCommentsToAst pstate.nlgs a
      in
        Right (annotatedA, nlgS.warnings, pstate)
  where
    env = Env
      { moduleUri = file
      }
    st = PState
      { nlgs = []
      , comments = []
      , refs = []
      }
    stream = MkTokenStream (Text.unpack input) ts

runJl4Parser :: Env -> PState -> Parser a -> FilePath -> TokenStream -> Either (ParseErrorBundle TokenStream Void) (a, PState)
runJl4Parser env initState p input stream =
  parse (runStateT (runReaderT (p <* eof) env) initState) input stream

-- ----------------------------------------------------------------------------
-- JL4 Program parser
-- ----------------------------------------------------------------------------

execProgramParser :: NormalizedUri -> Text -> Either (NonEmpty PError) (Module Name, [Resolve.Warning])
execProgramParser uri input =
  forgetPState $ execParser (module' uri) uri input
  where
    forgetPState = fmap (\(p, warns, _) -> (p, warns))

execProgramParserForTokens :: NormalizedUri -> Text -> [PosToken] -> Either (NonEmpty PError) (Module Name, [Resolve.Warning])
execProgramParserForTokens uri input ts =
  forgetPState $  execParserForTokens (module' uri) uri input ts
  where
    forgetPState = fmap (\(p, warns, _) -> (p, warns))

-- ----------------------------------------------------------------------------
-- Debug helpers
-- ----------------------------------------------------------------------------

-- | Parse a source file and pretty-print the resulting syntax tree.
parseFile :: (Show a, Resolve.HasNlg a) => Parser a -> NormalizedUri -> Text -> IO ()
parseFile p uri input =
  case execParser p uri input of
    Left errs -> Text.putStr $ Text.unlines $ fmap (.message) (toList errs)
    Right (x, _, _pState) -> pPrint x

-- ----------------------------------------------------------------------------
-- jl4 specific annotation helpers
-- ----------------------------------------------------------------------------

type AnnoParser = AnnoParser_ Parser PosToken

type Epa = Epa_ PosToken

type Lexeme = Lexeme_ PosToken
