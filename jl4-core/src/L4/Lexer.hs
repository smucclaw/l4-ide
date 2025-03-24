{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module L4.Lexer where

import Base
import qualified Base.Map as Map
import qualified Base.Set as Set
import qualified Base.Text as Text

import Data.Monoid (Alt (..))
import Data.Char hiding (Space)
import GHC.Show (showLitString)
import Text.Megaparsec as Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.State
import qualified Text.Megaparsec.Char.Lexer as Lexer
import L4.Parser.SrcSpan

type Lexer = Parsec Void Text

-- | Megaparsec stores offsets as integers.
type Offset = Int

-- | A raw token is a token with offset information attached by Megaparsec.
data RawToken =
  MkRawToken
    { start   :: !Offset
    , payload :: !TokenType
    , end     :: !Offset
    }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToExpr, NFData)

-- | A pos token is a token with position information attached.
data PosToken =
  MkPosToken
    { range   :: !SrcRange
    , payload :: !TokenType
    }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToExpr, NFData)

instance ToExpr NormalizedUri

data AnnoType
  = InlineAnno
  | LineAnno
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToExpr, NFData)

data DirectiveType
  = TEvalDirective
  | TCheckDirective
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (ToExpr, NFData)

-- | The type of token, plus information needed to reconstruct its contents.
data TokenType =
    TIdentifier   !Text
  | TQuoted       !Text
  | TIntLit       !Text !Int
  | TStringLit    !Text
  | TDirective    !DirectiveType
    -- copy token / ditto mark, currently '^'
  | TCopy         (Maybe TokenType)
    -- parentheses
  | TPOpen
  | TPClose
  | TCOpen
  | TCClose
    -- punctuation
  | TParagraph
  | TComma
  | TSemicolon
  | TDot
    -- genitive
  | TGenitive
    -- symbolic operators
  | TTimes
  | TPlus
  | TMinus
  | TGreaterEquals
  | TLessEquals
  | TGreaterThan
  | TLessThan
  | TEquals
  | TEqualsEquals
  | TNotEquals
  | TAnd
  | TOr
  | TImplies
  | TDividedBy
  | TOtherSymbolic !Text
    -- keywords
  | TKGiven
  | TKGiveth
  | TKDecide
  | TKMeans
  | TKDeclare
  | TKIf
  | TKThen
  | TKElse
  | TKOtherwise
  -- | TKFalse
  -- | TKTrue
  | TKAnd
  | TKOr
  | TKNot
  | TKIs
  | TKHas
  | TKOne
  | TKOf
  | TKWith
  | TKA
  | TKAn
  | TKThe
  | TKYield
  | TKConsider
  | TKWhere
  | TKList
  | TKAssume
  | TKWhen
  | TKType
  | TKFunction
  | TKFrom
  | TKTo
  | TKEquals
  | TKImplies
  | TKPlus
  | TKMinus
  | TKTimes
  | TKDivided
  | TKModulo
  | TKBy
  | TKGreater
  | TKLess
  | TKThan
  | TKAbove
  | TKBelow
  | TKAt
  | TKLeast
  | TKMost
  | TKFollowed
  | TKFor
  | TKAll
  | TKAka
  | TKImport
    -- annotations
  | TNlg          !Text !AnnoType
  | TRefSrc       !Text
  | TRefMap       !Text
  | TRef          !Text !AnnoType
    -- space
  | TSpace        !Text
  | TLineComment  !Text
  | TBlockComment !Text
  | EOF
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (ToExpr, NFData)

annotations :: [Text]
annotations = ["nlg", "ref", "ref-map", "ref-src"]

nlgAnnotation :: Lexer (Text, AnnoType)
nlgAnnotation =
  lineAnno "@nlg"
    <|> inlineAnno "[" "]"
  <?> "Natural Language Generation Annotation"

refAnnotation :: Lexer (Text, AnnoType)
refAnnotation =
  lineAnno "@ref"
    <|> inlineAnno "<<" ">>"
  <?> "Reference Annotation"

refSrcAnnotation :: Lexer Text
refSrcAnnotation = fst <$> lineAnno "@ref-src"

refMapAnnotation :: Lexer Text
refMapAnnotation = fst <$> lineAnno "@ref-map"

inlineAnno :: Text -> Text -> Lexer (Text, AnnoType)
inlineAnno openingHerald closingHerald = do
  _o <- string openingHerald
  (anno, _c) <- manyTill_ anySingle (string closingHerald)
  pure (Text.pack anno, InlineAnno)

lineAnno :: Text -> Lexer (Text, AnnoType)
lineAnno herald = do
  _ <- string herald
  (, LineAnno) <$> takeWhileP (Just "character") (/= '\n')

whitespace :: Lexer Text
whitespace =
  takeWhile1P (Just "whitespace") isSpace

lineComment :: Lexer Text
lineComment =
  (<>) <$> string "--" <*> takeWhileP (Just "character") (/= '\n')

blockComment :: Lexer Text
blockComment =
  (\ b (c, e) -> Text.concat (b : c ++ [e])) <$> string "{-" <*> manyTill_ inner (string "-}")
  where
    inner = blockComment <|> Text.singleton <$> anySingle

stringLiteral :: Lexer Text
stringLiteral =
  char '"' *> (Text.pack <$> manyTill Lexer.charLiteral (char '"'))

-- | A quoted identifier between backticks.
quoted :: Lexer Text
quoted =
  char '`' *> takeWhile1P (Just "printable char except backticks") (\ x -> isPrint x && not (x `elem` ("`" :: String))) <* char '`'

directiveLiteral :: Lexer DirectiveType
directiveLiteral = do
  _herald <- "#"
  getAlt $ foldMap (\(d, t) -> Alt $ d <$ chunk t) directives

directives :: [(DirectiveType, Text)]
directives = [(TEvalDirective, "EVAL"), (TCheckDirective, "CHECK")]

integerLiteral :: Lexer (Text, Int)
integerLiteral =
      (\ x (xs, i) -> (x <> xs, negate i)) <$> string "-" <*> decimal
  <|> decimal

decimal :: Lexer (Text, Int)
decimal = decimal_ <?> "integer"

decimal_ :: Lexer (Text, Int)
decimal_ = (\ s -> (s, mkNum s)) <$> takeWhile1P (Just "digit") isDigit
  where
    mkNum :: Text -> Int
    mkNum = foldl' step 0 . chunkToTokens (Proxy :: Proxy Text)
    step a c = a * 10 + fromIntegral (digitToInt c)

tokenPayload :: Lexer TokenType
tokenPayload =
      uncurry TIntLit <$> try integerLiteral
  <|> TStringLit      <$> stringLiteral
  <|> TGenitive       <$  string "'s"
  <|> TQuoted         <$> quoted
  <|> TDirective      <$> directiveLiteral
  <|> TRefSrc         <$> refSrcAnnotation
  <|> TRefMap         <$> refMapAnnotation
  <|> uncurry TNlg    <$> nlgAnnotation
  <|> uncurry TRef    <$> refAnnotation
  <|> TSpace          <$> whitespace
  <|> TLineComment    <$> lineComment
  <|> TBlockComment   <$> blockComment
  <|> TPOpen          <$  char '('
  <|> TPClose         <$  char ')'
  <|> TCOpen          <$  char '{'
  <|> TCClose         <$  char '}'
  <|> TParagraph      <$  char '§'
  <|> TComma          <$  char ','
  <|> TSemicolon      <$  char ';'
  <|> TDot            <$  char '.'
  <|> TCopy Nothing   <$  char '^'
  <|> symbolic
  <|> identifierOrKeyword

symbolic :: Lexer TokenType
symbolic =
  do
    s <- symbolString
    case Map.lookup s symbols of
      Nothing -> pure (TOtherSymbolic s)
      Just tt -> pure tt

symbolString :: Lexer Text
symbolString =
  takeWhile1P (Just "symbol char") (\ x -> x `elem` ("=<>+-*/:~&|%§" :: [Char]))

identifierOrKeyword :: Lexer TokenType
identifierOrKeyword =
  do
    i <- identifier
    case Map.lookup i keywords of
      Nothing -> pure (TIdentifier i)
      Just tt -> pure tt

identifier :: Lexer Text
identifier =
  Text.cons
  <$> satisfy isAlpha
  <*> takeWhileP (Just "identifier char") (\ x -> isAlphaNum x || x == '_')

symbols :: Map Text TokenType
symbols =
  Map.fromList
    [ ("*" , TTimes        )
    , ("+" , TPlus         )
    , ("-" , TMinus        )
    , (">=", TGreaterEquals)
    , ("<=", TLessEquals   )
    , (">" , TGreaterThan  )
    , ("<" , TLessThan     )
    , ("=" , TEquals       )
    , ("==", TEqualsEquals )
    , ("&&", TAnd          )
    , ("||", TOr           )
    , ("=>", TImplies      )
    , ("/" , TDividedBy    )
    ]

keywords :: Map Text TokenType
keywords =
  Map.fromList
    [ ("GIVEN"      , TKGiven      )
    , ("GIVETH"     , TKGiveth     )
    , ("DECIDE"     , TKDecide     )
    , ("MEANS"      , TKMeans      )
    , ("DECLARE"    , TKDeclare    )
    , ("IF"         , TKIf         )
    , ("THEN"       , TKThen       )
    , ("ELSE"       , TKElse       )
    , ("OTHERWISE"  , TKOtherwise  )
    -- , ("FALSE"      , TKFalse      )
    -- , ("TRUE"       , TKTrue       )
    , ("AND"        , TKAnd        )
    , ("OR"         , TKOr         )
    , ("NOT"        , TKNot        )
    , ("IS"         , TKIs         )
    , ("ONE"        , TKOne        )
    , ("OF"         , TKOf         )
    , ("WITH"       , TKWith       )
    , ("A"          , TKA          )
    , ("AN"         , TKAn         )
    , ("HAS"        , TKHas        )
    , ("THE"        , TKThe        )
    , ("YIELD"      , TKYield      )
    , ("CONSIDER"   , TKConsider   )
    , ("WHERE"      , TKWhere      )
    , ("LIST"       , TKList       )
    , ("ASSUME"     , TKAssume     )
    , ("WHEN"       , TKWhen       )
    , ("TYPE"       , TKType       )
    , ("FUNCTION"   , TKFunction   )
    , ("FROM"       , TKFrom       )
    , ("TO"         , TKTo         )
    , ("EQUALS"     , TKEquals     )
    , ("IMPLIES"    , TKImplies    )
    , ("PLUS"       , TKPlus       )
    , ("MINUS"      , TKMinus      )
    , ("TIMES"      , TKTimes      )
    , ("DIVIDED"    , TKDivided    )
    , ("MODULO"     , TKModulo     )
    , ("BY"         , TKBy         )
    , ("GREATER"    , TKGreater    )
    , ("LESS"       , TKLess       )
    , ("THAN"       , TKThan       )
    , ("ABOVE"      , TKAbove      )
    , ("BELOW"      , TKBelow      )
    , ("AT"         , TKAt         )
    , ("LEAST"      , TKLeast      )
    , ("MOST"       , TKMost       )
    , ("FOLLOWED"   , TKFollowed   )
    , ("FOR"        , TKFor        )
    , ("ALL"        , TKAll        )
    , ("AKA"        , TKAka        )
    , ("IMPORT"     , TKImport     )
    ]

trivialToken :: TokenType -> PosToken
trivialToken tt =
  MkPosToken trivialRange tt
  where
    trivialRange :: SrcRange
    trivialRange = MkSrcRange trivialPos trivialPos 0

    trivialPos :: SrcPos
    trivialPos = MkSrcPos 0 0

rawTokens :: Lexer [RawToken]
rawTokens = many (MkRawToken <$> getOffset <*> tokenPayload <*> getOffset)

execLexer :: NormalizedUri -> Text -> Either (NonEmpty PError) [PosToken]
execLexer uri input =
  let
    r = parse (rawTokens <* eof) (showNormalizedUri uri) input
  in
    case r of
      Right rtoks -> Right (mkPosTokens uri input rtoks)
      Left errs   -> Left (fmap (mkPError "lexer") $ errorBundleToErrorMessages errs)

data TokenState =
  MkTokenState
    { posState        :: !(PosState Text)
    , currentLine     :: Int
    , currentLineToks :: [PosToken]
    , prevLineToks    :: [PosToken] -- immediately preceding non-whitespace line
    }
  deriving Generic

initialTokenState :: NormalizedUri -> Text -> TokenState
initialTokenState uri txt =
  MkTokenState
    (initialPosState (showNormalizedUri uri) txt)
    0
    []
    []

-- | This traversal postprocesses raw tokens and turns them into pos tokens
-- by converting their positions.
--
-- At the same time, we interpret copy tokens.
--
mkPosTokens :: NormalizedUri -> Text -> [RawToken] -> [PosToken]
mkPosTokens uri txt rtoks =
    evalState (traverse go rtoks) (initialTokenState uri txt)
  where
    go :: RawToken -> Base.State TokenState PosToken
    go rtok = do
      ts <- get
      let
        pst      = ts.posState
        pstStart = reachOffsetNoLine rtok.start pst
        pstEnd   = reachOffsetNoLine rtok.end pstStart
        posStart = convertPos (pstateSourcePos pstStart)
        posEnd   = convertPos (pstateSourcePos pstEnd  )
      when (posStart.line /= ts.currentLine) $ do
        assign #currentLine posStart.line
        assign #currentLineToks []
        -- We ignore purely "whitespace" lines, and count comments as whitespace, but currently not annotations.
        unless (all isSpaceToken ts.currentLineToks) (assign #prevLineToks ts.currentLineToks)
      assign #posState pstEnd
      prevLineToks' <- use #prevLineToks
      let
        payload  =
          case rtok.payload of
            TCopy Nothing -> TCopy (findMatchingToken posStart.column prevLineToks')
            other         -> other
        pt       =
          MkPosToken
            (MkSrcRange
              posStart
              posEnd
              (rtok.end - rtok.start)
            )
            payload
      modifying #currentLineToks (pt :)
      pure pt

findMatchingToken :: Int -> [PosToken] -> Maybe TokenType
findMatchingToken c pts = do
  -- trace ("trying to find match: " <> show pts) (pure ())
  pt <- find (\ pt -> pt.range.start.column == c) pts
  -- trace ("found match: " <> show (computedPayload pt)) $
  pure (computedPayload pt)

computedPayload :: PosToken -> TokenType
computedPayload pt =
  case pt.payload of
    TCopy (Just original) -> original
    other                 -> other

isSpaceToken :: PosToken -> Bool
isSpaceToken t =
  case computedPayload t of
    TSpace _        -> True
    TLineComment _  -> True
    TBlockComment _ -> True
    _               -> False

isAnnotationToken :: PosToken -> Bool
isAnnotationToken t =
  case computedPayload t of
    TNlg {} -> True
    TRef {} -> True
    _      -> False

-- | Convert from a Megaparsec source position to one of ours.
convertPos :: SourcePos -> SrcPos
convertPos (SourcePos _fn l c) =
  MkSrcPos (unPos l) (unPos c)

unconvertPos :: FilePath -> SrcPos -> SourcePos
unconvertPos fn (MkSrcPos l c) =
  SourcePos fn (mkPos l) (mkPos c)

data TokenStream =
  MkTokenStream
    { input  :: String
    , tokens :: [PosToken]
    }
  deriving stock Show

proxyTokenStream :: Proxy TokenStream
proxyTokenStream = Proxy

instance Stream TokenStream where
  type Token TokenStream = PosToken
  type Tokens TokenStream = [PosToken]

  tokenToChunk :: Proxy TokenStream -> Token TokenStream -> Tokens TokenStream
  tokenToChunk Proxy x = [x]

  tokensToChunk :: Proxy TokenStream -> [Token TokenStream] -> Tokens TokenStream
  tokensToChunk Proxy xs = xs

  chunkToTokens :: Proxy TokenStream -> Tokens TokenStream -> [Token TokenStream]
  chunkToTokens Proxy xs = xs

  chunkLength :: Proxy TokenStream -> Tokens TokenStream -> Int
  chunkLength Proxy xs = length xs

  chunkEmpty :: Proxy TokenStream -> Tokens TokenStream -> Bool
  chunkEmpty Proxy = null

  take1_ :: TokenStream -> Maybe (Token TokenStream, TokenStream)
  take1_ (MkTokenStream _ [])         = Nothing
  take1_ (MkTokenStream txt (t : ts)) =
    Just (t, MkTokenStream (drop (tokensLength proxyTokenStream (t :| [])) txt) ts)

  takeN_ :: Int -> TokenStream -> Maybe (Tokens TokenStream, TokenStream)
  takeN_ n (MkTokenStream txt ts)
    | n <= 0 = Just ([], MkTokenStream txt ts)
    | null ts = Nothing
    | otherwise =
      let
        (ts1, ts2) = splitAt n ts
      in
        case nonEmpty ts1 of
          Nothing -> Just (ts1, MkTokenStream txt ts2)
          Just nex -> Just (ts1, MkTokenStream (drop (tokensLength proxyTokenStream nex) txt) ts2)


  takeWhile_ :: (Token TokenStream -> Bool) -> TokenStream -> (Tokens TokenStream, TokenStream)
  takeWhile_ p (MkTokenStream txt ts) =
    let
      (ts1, ts2) = span p ts
    in
      case nonEmpty ts1 of
        Nothing  -> (ts1, MkTokenStream txt ts2)
        Just nex -> (ts1, MkTokenStream (drop (tokensLength proxyTokenStream nex) txt) ts2)

instance VisualStream TokenStream where
  showTokens :: Proxy TokenStream -> NonEmpty (Token TokenStream) -> String
  showTokens Proxy xs =
    concat (toList (Text.unpack . displayPosToken <$> xs))

  tokensLength :: Proxy TokenStream -> NonEmpty (Token TokenStream) -> Int
  tokensLength Proxy xs = sum ((.range.length) <$> xs)

instance TraversableStream TokenStream where
  reachOffset :: Int -> PosState TokenStream -> (Maybe String, PosState TokenStream)
  reachOffset o pst =
    ( Just (prefix ++ restOfLine)
    , PosState
        { pstateInput      = MkTokenStream postTxt post
        , pstateOffset     = max pst.pstateOffset o
        , pstateSourcePos  = newSourcePos
        , pstateTabWidth   = pst.pstateTabWidth
        , pstateLinePrefix = prefix
        }
    )
    where
      fn = pst.pstateSourcePos.sourceName

      prefix
        | sameLine  = pst.pstateLinePrefix ++ preLine
        | otherwise = preLine

      sameLine :: Bool
      sameLine = sourceLine newSourcePos == sourceLine pst.pstateSourcePos

      newSourcePos :: SourcePos
      newSourcePos =
        case post of
          [] -> case pst.pstateInput.tokens of
            [] -> pst.pstateSourcePos
            xs -> unconvertPos fn (last xs).range.end
          (x : _) -> unconvertPos fn x.range.start

      pre, post :: [PosToken]
      (pre, post) = splitAt (o - pst.pstateOffset) pst.pstateInput.tokens

      preTxt, postTxt :: String
      (preTxt, postTxt) = splitAt tokensConsumed pst.pstateInput.input

      preLine :: String
      preLine = reverse . takeWhile (/= '\n') . reverse $ preTxt

      tokensConsumed :: Int
      tokensConsumed =
        case nonEmpty pre of
          Nothing    -> 0
          Just nePre -> tokensLength proxyTokenStream nePre

      restOfLine :: String
      restOfLine = takeWhile (/= '\n') postTxt


-- ----------------------------------------------------------------------------
-- Parser error messages
-- ----------------------------------------------------------------------------

data PError
  = PError
    { message :: Text
    , range :: SrcSpan
    , origin :: Text
    }
  deriving (Show, Eq, Ord)

mkPError :: Text -> (Text, SourcePos, SourcePos) -> PError
mkPError orig (m, s, e) =
  PError
    { message = m
    , range = MkSrcSpan
      { start =
          MkSrcPos
            { line = unPos $ sourceLine s
            , column = unPos $ sourceColumn s
            }
      , end =
          MkSrcPos
            { line = unPos $ sourceLine e
            , column = unPos $ sourceColumn e
            }
      }
    , origin = orig
    }

errorBundleToErrorMessages ::
  forall s e.
  ( VisualStream s
  , TraversableStream s
  , ShowErrorComponent e
  ) =>
  -- | Parse error bundle to display
  ParseErrorBundle s e ->
  -- | Textual rendition of the bundle
  NonEmpty (Text, SourcePos, SourcePos)
errorBundleToErrorMessages ParseErrorBundle{..} =
  let
    (results, _) = runState (traverse format bundleErrors) bundlePosState
  in
    results
 where
  format :: ParseError s e -> Base.State (PosState s) (Text, SourcePos, SourcePos)
  format e = do
    pst <- get
    let
      (msline, pst') = calculateOffset pst
      epos = pstateSourcePos pst'
      eposNext = pstateSourcePos $ reachOffsetNoLine 1 pst'
      errMsg = parseErrorTextPretty e
      parseErrCtx = offendingLine msline epos
    put pst'
    pure $ (Text.pack $ parseErrCtx <> errMsg, epos, eposNext)
   where
    calculateOffset pst = reachOffset (errorOffset e) pst
    offendingLine msline epos =
      case msline of
        Nothing -> ""
        Just sline ->
          let
            rpadding =
              if pointerLen > 0
                then replicate rpshift ' '
                else ""
            pointerLen =
              if rpshift + elen > slineLen
                then slineLen - rpshift + 1
                else elen
            pointer = replicate pointerLen '^'
            lineNumber = (show . unPos . sourceLine) epos
            padding = replicate (length lineNumber + 1) ' '
            rpshift = unPos (sourceColumn epos) - 1
            slineLen = length sline
          in
            padding
              <> "|\n"
              <> lineNumber
              <> " | "
              <> sline
              <> "\n"
              <> padding
              <> "| "
              <> rpadding
              <> pointer
              <> "\n"
    pxy = Proxy :: Proxy s
    elen =
      case e of
        TrivialError _ Nothing _ -> 1
        TrivialError _ (Just x) _ -> errorItemLength pxy x
        FancyError _ xs ->
          Set.foldl' (\a b -> max a (errorFancyLength b)) 1 xs

-- | Get length of the “pointer” to display under a given 'ErrorItem'.
errorItemLength :: (VisualStream s) => Proxy s -> ErrorItem (Token s) -> Int
errorItemLength pxy = \case
  Tokens ts -> tokensLength pxy ts
  _ -> 1

-- | Get length of the “pointer” to display under a given 'ErrorFancy'.
errorFancyLength :: (ShowErrorComponent e) => ErrorFancy e -> Int
errorFancyLength = \case
  ErrorCustom a -> errorComponentLen a
  _ -> 1

-- | This isn't truly precise, but it should be mostly fine, because we use
-- the Haskell escape sequences both in the parser and in printing.
--
showStringLit :: Text -> Text
showStringLit t =
  Text.pack ((showChar '"' . showLitString (Text.unpack t) . showChar '"') "")

toAnno
  :: Text
  -- ^ line herald
  -> Text
  -- ^ inline herald opening
  -> Text
  -- ^ inline herald closing
  -> Text
  -- ^ anno contents
  -> AnnoType
  -- ^ inline or line anno
  -> Text
toAnno lh oh ch t = \case
  InlineAnno -> oh <> t <> ch
  LineAnno -> lh <> t


toRefAnno, toNlgAnno :: Text -> AnnoType -> Text
toRefAnno = toAnno "@ref" "<<" ">>"
toNlgAnno = toAnno "@nlg" "[" "]"

displayPosToken :: PosToken -> Text
displayPosToken (MkPosToken _r tt) =
  case tt of
    TIdentifier t    -> t
    TQuoted t        -> "`" <> t <> "`"
    TIntLit t _i     -> t
    TStringLit s     -> showStringLit s
    TDirective d     -> showDirective d
    TCopy _          -> "^"
    TPOpen           -> "("
    TPClose          -> ")"
    TCOpen           -> "{"
    TCClose          -> "}"
    TParagraph       -> "§"
    TComma           -> ","
    TSemicolon       -> ";"
    TDot             -> "."
    TGenitive        -> "'s"
    TTimes           -> "*"
    TPlus            -> "+"
    TMinus           -> "-"
    TGreaterEquals   -> ">="
    TLessEquals      -> "<="
    TGreaterThan     -> ">"
    TLessThan        -> "<"
    TEquals          -> "="
    TEqualsEquals    -> "=="
    TNotEquals       -> "/="
    TAnd             -> "&&"
    TOr              -> "||"
    TImplies         -> "=>"
    TDividedBy       -> "/"
    TOtherSymbolic t -> t
    TKGiven          -> "GIVEN"
    TKGiveth         -> "GIVETH"
    TKDecide         -> "DECIDE"
    TKMeans          -> "MEANS"
    TKDeclare        -> "DECLARE"
    TKIf             -> "IF"
    TKThen           -> "THEN"
    TKElse           -> "ELSE"
    TKOtherwise      -> "OTHERWISE"
    -- TKFalse          -> "FALSE"
    -- TKTrue           -> "TRUE"
    TKAnd            -> "AND"
    TKOr             -> "OR"
    TKNot            -> "NOT"
    TKIs             -> "IS"
    TKHas            -> "HAS"
    TKOne            -> "ONE"
    TKOf             -> "OF"
    TKWith           -> "WITH"
    TKA              -> "A"
    TKAn             -> "AN"
    TKThe            -> "THE"
    TKYield          -> "YIELD"
    TKConsider       -> "CONSIDER"
    TKWhere          -> "WHERE"
    TKList           -> "LIST"
    TKAssume         -> "ASSUME"
    TKWhen           -> "WHEN"
    TKType           -> "TYPE"
    TKFunction       -> "FUNCTION"
    TKFrom           -> "FROM"
    TKTo             -> "TO"
    TKEquals         -> "EQUALS"
    TKImplies        -> "IMPLIES"
    TKPlus           -> "PLUS"
    TKMinus          -> "MINUS"
    TKTimes          -> "TIMES"
    TKDivided        -> "DIVIDED"
    TKModulo         -> "MODULO"
    TKBy             -> "BY"
    TKGreater        -> "GREATER"
    TKLess           -> "LESS"
    TKThan           -> "THAN"
    TKAbove          -> "ABOVE"
    TKBelow          -> "BELOW"
    TKAt             -> "AT"
    TKLeast          -> "LEAST"
    TKMost           -> "MOST"
    TKFollowed       -> "FOLLOWED"
    TKFor            -> "FOR"
    TKAll            -> "ALL"
    TKAka            -> "AKA"
    TNlg t ty        -> toNlgAnno t ty
    TRef t ty        -> toRefAnno t ty
    TRefSrc t        -> "@ref-src" <> t
    TRefMap t        -> "@ref-map" <> t
    TKImport         -> "IMPORT"
    TSpace t         -> t
    TLineComment t   -> t
    TBlockComment t  -> t
    EOF              -> ""

showDirective :: DirectiveType -> Text
showDirective = \case
  TEvalDirective -> "#EVAL"
  TCheckDirective -> "#CHECK"

data TokenCategory
  = CIdentifier
  | CStringLit
  | CNumberLit
  | CSymbol
  | COperator
  | CKeyword
  | CComment
  | CWhitespace
  | CDirective
  | CAnnotation
  | CEOF
  deriving stock Eq

posTokenCategory :: TokenType -> TokenCategory
posTokenCategory =
  \case
    TIdentifier _ -> CIdentifier
    TQuoted _ -> CIdentifier
    TIntLit _ _ -> CNumberLit
    TStringLit _ -> CStringLit
    TDirective _ -> CDirective
    TCopy Nothing -> CSymbol
    TCopy (Just t) -> posTokenCategory t
    TPOpen -> CSymbol
    TPClose -> CSymbol
    TCOpen -> CSymbol
    TCClose -> CSymbol
    TParagraph -> CSymbol
    TComma -> CSymbol
    TSemicolon -> CSymbol
    TDot -> CSymbol
    TGenitive -> CIdentifier
    TTimes -> COperator
    TPlus -> COperator
    TMinus -> COperator
    TGreaterEquals -> COperator
    TLessEquals -> COperator
    TGreaterThan -> COperator
    TLessThan -> COperator
    TEquals -> COperator
    TEqualsEquals -> COperator
    TNotEquals -> COperator
    TAnd -> COperator
    TOr -> COperator
    TImplies -> COperator
    TDividedBy -> COperator
    TOtherSymbolic _ -> CSymbol
    TKGiven -> CKeyword
    TKGiveth -> CKeyword
    TKDecide -> CKeyword
    TKMeans -> CKeyword
    TKDeclare -> CKeyword
    TKIf -> CKeyword
    TKThen -> CKeyword
    TKElse -> CKeyword
    TKOtherwise -> CIdentifier
    -- TKFalse -> CKeyword
    -- TKTrue -> CKeyword
    TKAnd -> CKeyword
    TKOr -> CKeyword
    TKNot -> CKeyword
    TKIs -> CKeyword
    TKHas -> CKeyword
    TKOne -> CKeyword
    TKOf -> CKeyword
    TKWith -> CKeyword
    TKA -> CKeyword
    TKAn -> CKeyword
    TKThe -> CKeyword
    TKYield -> CKeyword
    TKConsider -> CKeyword
    TKWhere -> CKeyword
    TKList -> CKeyword
    TKAssume -> CKeyword
    TKWhen -> CKeyword
    TKType -> CKeyword
    TKFunction -> CKeyword
    TKFrom -> CKeyword
    TKTo -> CKeyword
    TKEquals -> CKeyword
    TKImplies -> CKeyword
    TKPlus -> CKeyword
    TKMinus -> CKeyword
    TKTimes -> CKeyword
    TKDivided -> CKeyword
    TKModulo -> CKeyword
    TKBy -> CKeyword
    TKGreater -> CKeyword
    TKLess -> CKeyword
    TKThan -> CKeyword
    TKAbove -> CKeyword
    TKBelow -> CKeyword
    TKAt -> CKeyword
    TKLeast -> CKeyword
    TKMost -> CKeyword
    TKFollowed -> CKeyword
    TKFor -> CKeyword
    TKAll -> CKeyword
    TKAka -> CKeyword
    TNlg {} -> CAnnotation
    TRef {} -> CAnnotation
    TRefSrc _ -> CAnnotation
    TRefMap _ -> CAnnotation
    TKImport -> CKeyword
    TSpace _ -> CWhitespace
    TLineComment _ -> CComment
    TBlockComment _ -> CComment
    EOF -> CEOF

showNormalizedUri :: NormalizedUri -> String
showNormalizedUri =  Text.unpack . (.getUri) . fromNormalizedUri
