{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module L4.Lexer where

import Base
import qualified Base.Map as Map
import qualified Base.Set as Set
import qualified Base.Text as Text

import Data.Monoid (Alt (..))
import qualified Data.Scientific as Sci
import Data.Char hiding (Space)
import GHC.Show (showLitString)
import Text.Megaparsec as Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.State ( initialPosState )
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

data AnnoType
  = InlineAnno
  | LineAnno
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToExpr, NFData)

data TDirectives
  = TLazyEvalDirective
  | TLazyEvalTraceDirective
  | TCheckDirective
  | TContractDirective
  | TAssertDirective
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (ToExpr, NFData)

directives :: Map Text TDirectives
directives = Map.fromList
  [ ("EVALTRACE", TLazyEvalTraceDirective)
  , ("EVAL"     , TLazyEvalDirective)
  , ("CHECK"    , TCheckDirective)
  , ("TRACE"    , TContractDirective)
  , ("ASSERT"   , TAssertDirective)
  ]

data TAnnotations
  = TNlg          !Text !AnnoType
  | TRefSrc       !Text
  | TRefMap       !Text
  | TRef          !Text !AnnoType
  | TNlgString    !Text
  | TNlgPrefix    -- ^ "@nlg"
  | TDesc         !Text -- ^ "@desc"
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (ToExpr, NFData)

data TIdentifiers
  = TIdentifier !Text
  | TQuoted     !Text
  | TGenitive
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (ToExpr, NFData)

data TSymbols
  = TRefOpen
  | TRefClose
  | TNlgOpen
  | TNlgClose
  | TParagraph
  | TPOpen
  | TPClose
  | TCOpen
  | TCClose
  | TComma
  | TSemicolon
  | TDot
  | TColon
  | TPercent
  | TUnderscore
  | TCopy (Maybe TokenType)
  | TOtherSymbolic !Text
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (ToExpr, NFData)

symbols :: Map Text TSymbols
symbols = Map.fromList
  [ ("^" , TCopy Nothing)
  , ("(" , TPOpen)
  , (")" , TPClose)
  , ("{" , TCOpen)
  , ("}" , TCClose)
  , ("<<", TRefOpen)
  , (">>", TRefClose)
  , ("[" , TNlgOpen)
  , ("]" , TNlgClose)
  , ("§" , TParagraph)
  , ("," , TComma)
  , (";" , TSemicolon)
  , ("." , TDot)
  , ("%" , TPercent)
  , ("_" , TUnderscore)
  , (":" , TColon)
  ]

data TKeywords
  = TKGiven
  | TKGiveth
  | TKDecide
  | TKExact
  | TKMeans
  | TKDeclare
  | TKIf
  | TKBranch
  | TKThen
  | TKElse
  | TKOtherwise
  | TKAnd
  | TKOr
  | TKRAnd
  | TKROr
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
  | TKParty
  | TKDo
  | TKDoes
  | TKMust
  | TKMay
  | TKProvided
  | TKWithin
  | TKHence
  | TKLest
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
  | TKStarting
  | TKLeast
  | TKMost
  | TKFollowed
  | TKFor
  | TKAll
  | TKAka
  | TKImport
  | TKFetch
  | TKPost
  | TKEnv
  | TKConcat
  | TKAs
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (ToExpr, NFData)

keywords :: Map Text TKeywords
keywords = Map.fromList
  [ ("GIVEN"      , TKGiven      )
  , ("GIVETH"     , TKGiveth     )
  , ("DECIDE"     , TKDecide     )
  , ("EXACTLY"    , TKExact      )
  , ("MEANS"      , TKMeans      )
  , ("DECLARE"    , TKDeclare    )
  , ("IF"         , TKIf         )
  , ("BRANCH"     , TKBranch     )
  , ("THEN"       , TKThen       )
  , ("ELSE"       , TKElse       )
  , ("OTHERWISE"  , TKOtherwise  )
  , ("AND"        , TKAnd        )
  , ("OR"         , TKOr         )
  , ("RAND"       , TKRAnd       )
  , ("ROR"        , TKROr        )
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
  , ("PARTY"      , TKParty      )
  , ("DO"         , TKDo         )
  , ("DOES"       , TKDoes       )
  , ("MUST"       , TKMust       )
  , ("MAY"        , TKMay       )
  , ("PROVIDED"   , TKProvided   )
  , ("WITHIN"     , TKWithin     )
  , ("HENCE"      , TKHence      )
  , ("LEST"       , TKLest       )
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
  , ("STARTING"   , TKStarting   )
  , ("LEAST"      , TKLeast      )
  , ("MOST"       , TKMost       )
  , ("FOLLOWED"   , TKFollowed   )
  , ("FOR"        , TKFor        )
  , ("ALL"        , TKAll        )
  , ("AKA"        , TKAka        )
  , ("IMPORT"     , TKImport     )
  , ("FETCH"      , TKFetch      )
  , ("POST"       , TKPost       )
  , ("ENV"        , TKEnv        )
  , ("CONCAT"     , TKConcat     )
  , ("AS"         , TKAs         )
  ]

data TOperators
  = TTimes
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
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (ToExpr, NFData)

operators :: Map Text TOperators
operators = Map.fromList
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


data TSpaces
  = TSpace        !Text
  | TLineComment  !Text
  | TBlockComment !Text
  | EOF
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (ToExpr, NFData)

data TLiterals
  = TIntLit       !Text !Integer
  | TRationalLit  !Text !Rational
  | TStringLit    !Text
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (ToExpr, NFData)

-- | The type of token, plus information needed to reconstruct its contents.
data TokenType
  = TDirectives  !TDirectives
  | TLiterals    !TLiterals
  | TSpaces      !TSpaces
  | TKeywords    !TKeywords
  | TAnnotations !TAnnotations
  | TSymbols     !TSymbols
  | TIdentifiers !TIdentifiers
  | TOperators   !TOperators
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (ToExpr, NFData)

annotations :: [Text]
annotations = ["nlg", "ref", "ref-map", "ref-src"]

nlgAnnotation :: Lexer (Text, AnnoType)
nlgAnnotation =
  lineAnno "@nlg"
    <|> inlineAnno (Text.singleton nlgInlineAnnotationOpenChar) (Text.singleton nlgInlineAnnotationCloseChar)
  <?> "Natural Language Generation Annotation"

nlgInlineAnnotationCloseChar :: Char
nlgInlineAnnotationCloseChar = ']'

nlgInlineAnnotationOpenChar :: Char
nlgInlineAnnotationOpenChar = '['

refAnnotation :: Lexer (Text, AnnoType)
refAnnotation =
  lineAnno "@ref"
    <|> inlineAnno "<<" ">>"
  <?> "Reference Annotation"

refSrcAnnotation :: Lexer Text
refSrcAnnotation = fst <$> lineAnno "@ref-src"

refMapAnnotation :: Lexer Text
refMapAnnotation = fst <$> lineAnno "@ref-map"

descAnnotation :: Lexer Text
descAnnotation = fst <$> lineAnno "@desc"

nlgString :: Lexer Text
nlgString =
  takeWhile1P (Just "character") (\c -> c `notElem` nlgSpecialChars && not (isSpace c))
  where
    nlgSpecialChars =
      [ nlgInlineAnnotationCloseChar
      , nlgExprDelimiterSymbol
      ]

nlgExprDelimiter :: Lexer Char
nlgExprDelimiter =
  satisfy (== nlgExprDelimiterSymbol)

nlgExprDelimiterSymbol :: Char
nlgExprDelimiterSymbol = '%'

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
  (<>) <$> (string "--" <|> string "//") <*> takeWhileP (Just "character") (/= '\n')

blockComment :: Lexer Text
blockComment =
      (\ b (c, e) -> Text.concat (b : c ++ [e])) <$> string "{-" <*> manyTill_ inner (string "-}")
  <|> (\ b (c, e) -> Text.concat (b : c ++ [e])) <$> string "/*" <*> manyTill_ inner (string "*/")
  where
    inner = blockComment <|> Text.singleton <$> anySingle

stringLiteral :: Lexer Text
stringLiteral =
  char '"' *> (Text.pack <$> manyTill Lexer.charLiteral (char '"'))

-- | A quoted identifier between backticks.
quoted :: Lexer Text
quoted =
  char '`' *> takeWhile1P (Just "printable char except backticks") (\ x -> isPrint x && not (x `elem` ("`" :: String))) <* char '`'

directiveLiteral :: Lexer TDirectives
directiveLiteral = do
  _herald <- "#"
  getAlt $ foldMap (\(t, d) -> Alt $ d <$ chunk t) $ Map.toDescList directives
    -- we use toDescList to make sure that longer tokens with common prefixes
    -- appear before shorter ones, in particular EVALTRACE before EVAL

integerLiteral :: Lexer (Text, Integer)
integerLiteral =
      (\ x (xs, i) -> (x <> xs, negate i)) <$> string "-" <*> decimal
  <|> decimal

decimal :: Lexer (Text, Integer)
decimal = decimal_ <?> "integer"

decimal_ :: Lexer (Text, Integer)
decimal_ = (\ s -> (s, mkNum s)) <$> takeWhile1P (Just "digit") isDigit
  where
    mkNum :: Text -> Integer
    mkNum = foldl' step 0 . chunkToTokens (Proxy :: Proxy Text)
    step a c = a * 10 + fromIntegral (digitToInt c)

rationalLiteral :: Lexer (Text, Rational)
rationalLiteral =
      (\ x (xs, i) -> (x <> xs, negate i)) <$> string "-" <*> floatP
  <|> floatP
{-# INLINE rationalLiteral #-}

floatP :: Lexer (Text, Rational)
floatP = float_ <?> "float"
{-# INLINE floatP #-}

float_ :: Lexer (Text, Rational)
float_ = do
  (t, c') <- decimal_
  (c, e, t2) <- dotDecimal_ c'
  pure (t <> "." <> t2, realToFrac $ Sci.scientific c e)
{-# INLINE float_ #-}

dotDecimal_ :: Integer -> Lexer (Integer, Int, Text)
dotDecimal_ c' = do
  void (satisfy (== '.'))
  let mkNum = foldl' step (c', 0) . chunkToTokens (Proxy :: Proxy Text)
      step (a, e') c =
          ( (a * 10 + fromIntegral (digitToInt c))
          , (e' - 1)
          )
  (\ s -> let (c, e) = mkNum s in (c, e, s)) <$> takeWhile1P (Just "digit") isDigit
{-# INLINE dotDecimal_ #-}

literalPayload :: Lexer TLiterals
literalPayload = asum
  [ uncurry TRationalLit <$> try rationalLiteral
  , uncurry TIntLit      <$> try integerLiteral
  , TStringLit           <$> stringLiteral
  ]

identifiersPayload :: Lexer TIdentifiers
identifiersPayload = asum
  [ TGenitive <$  string "'s"
  , TQuoted   <$> quoted
  ]

annotationsPayload :: Lexer TAnnotations
annotationsPayload = asum
  [ TRefSrc       <$> refSrcAnnotation
  , TRefMap       <$> refMapAnnotation
  , TDesc         <$> descAnnotation
  , uncurry TNlg  <$> nlgAnnotation
  , uncurry TRef  <$> refAnnotation
  ]

spacesPayload :: Lexer TSpaces
spacesPayload = asum
  [ TSpace        <$> whitespace
  , TLineComment  <$> lineComment
  , TBlockComment <$> blockComment
  ]

symbolsPayload :: Lexer TSymbols
symbolsPayload
  = asum
  $ map (\(t, sym) -> sym <$ string t)
  $ Map.toList symbols

tokenPayload :: Lexer TokenType
tokenPayload = asum
  [ TLiterals    <$> literalPayload
  , TIdentifiers <$> identifiersPayload
  , TDirectives  <$> directiveLiteral
  , TAnnotations <$> annotationsPayload
  , TSpaces      <$> spacesPayload
  , TSymbols     <$> symbolsPayload
  , TOperators   <$> operatorsPayload
  , TSymbols . TOtherSymbolic
                 <$> operatorString
  , either TIdentifiers TKeywords
                 <$> identifierOrKeyword
  ]

nlgTokenPayload :: Lexer TokenType
nlgTokenPayload = asum
  [ TSymbols TPercent          <$  nlgExprDelimiter
  , TSpaces . TSpace           <$> whitespace
  , TIdentifiers . TQuoted     <$> quoted
  , TIdentifiers . TIdentifier <$> identifier
  , TSymbols TNlgOpen          <$  char nlgInlineAnnotationOpenChar
  , TSymbols TNlgClose         <$  char nlgInlineAnnotationCloseChar
  , TAnnotations TNlgPrefix    <$  "@nlg"
  , TAnnotations . TNlgString  <$> nlgString
  ]

operatorsPayload :: Lexer TOperators
operatorsPayload = do
  s <- operatorString
  maybe mzero pure (Map.lookup s operators)

operatorString :: Lexer Text
operatorString =
  takeWhile1P (Just "operator char") (\ x -> x `elem` ("=<>+-*/:~&|%§" :: [Char]))

identifierOrKeyword :: Lexer (Either TIdentifiers TKeywords)
identifierOrKeyword = do
  i <- identifier
  pure case Map.lookup i keywords of
    Nothing -> Left $ TIdentifier i
    Just tt -> Right tt

identifier :: Lexer Text
identifier =
  Text.cons
  <$> satisfy isAlpha
  <*> takeWhileP (Just "identifier char") (\ x -> isAlphaNum x || x == '_')

trivialToken :: NormalizedUri -> TokenType -> PosToken
trivialToken uri tt =
  MkPosToken trivialRange tt
  where
    trivialRange :: SrcRange
    trivialRange = MkSrcRange trivialPos trivialPos 0 uri

    trivialPos :: SrcPos
    trivialPos = MkSrcPos 0 0

rawTokens :: Lexer [RawToken]
rawTokens = many (MkRawToken <$> getOffset <*> tokenPayload <*> getOffset)

rawNlgTokens :: Lexer [RawToken]
rawNlgTokens = many (MkRawToken <$> getOffset <*> nlgTokenPayload <*> getOffset)

execLexer :: NormalizedUri -> Text -> Either (NonEmpty PError) [PosToken]
execLexer uri input =
  case parse (rawTokens <* eof) (showNormalizedUri uri) input of
    Right rtoks -> Right (mkPosTokens Nothing uri input rtoks)
    Left errs   -> Left (fmap (mkPError "lexer") $ errorBundleToErrorMessages errs)

execNlgLexer :: SourcePos -> NormalizedUri -> Text -> Either (ParseErrorBundle Text Void) [PosToken]
execNlgLexer offset uri input = do
  rtoks <- parse (rawNlgTokens <* eof) (showNormalizedUri uri) input
  pure (mkPosTokens (Just offset) uri input rtoks)

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
mkPosTokens :: Maybe SourcePos -> NormalizedUri -> Text -> [RawToken] -> [PosToken]
mkPosTokens sourcePosOffset uri txt rtoks =
    evalState (traverse go rtoks) tokenSt
  where
    startTokenState = initialTokenState uri txt
    tokenSt = case sourcePosOffset of
      Nothing -> startTokenState
      Just offset -> startTokenState
        { posState = startTokenState.posState
          { pstateSourcePos = offset
          }
        }
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
            TSymbols (TCopy Nothing) -> TSymbols $ TCopy (findMatchingToken posStart.column prevLineToks')
            other         -> other
        pt       =
          MkPosToken
            (MkSrcRange
              posStart
              posEnd
              (rtok.end - rtok.start)
              uri
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
    RealTCopy original -> original
    other                 -> other

isSpaceToken :: PosToken -> Bool
isSpaceToken t =
  case computedPayload t of
    TSpaces {} -> True
    _ -> False

isAnnotationToken :: PosToken -> Bool
isAnnotationToken t =
  case computedPayload t of
    TAnnotations {} -> True
    _ -> False

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
errorItemLength pxy = \ case
  Tokens ts -> tokensLength pxy ts
  _ -> 1

-- | Get length of the “pointer” to display under a given 'ErrorFancy'.
errorFancyLength :: (ShowErrorComponent e) => ErrorFancy e -> Int
errorFancyLength = \ case
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
toAnno lh oh ch t = \ case
  InlineAnno -> oh <> t <> ch
  LineAnno -> lh <> t


toRefAnno, toNlgAnno :: Text -> AnnoType -> Text
toRefAnno = toAnno "@ref" "<<" ">>"
toNlgAnno = toAnno "@nlg" "[" "]"

displayPosToken :: PosToken -> Text
displayPosToken (MkPosToken _r tt) =
  displayTokenType tt

-- | "turns around" a map that is expected to
-- - be surjective
-- - be injective
-- - be a function otherwise (i.e. all elements in its
--   codomain have exactly one image
--
-- NOTE: This is really slow for obvious reasons, so it might
-- be a good target for potential future optimizations
inverseCompleteLookup :: (Eq a, Show a) => a -> Map c a -> c
inverseCompleteLookup k
  = fromMaybe (error $ "inverse complete lookup for " <> show k <> "was actually not cmoplete")
  . lookup k
  . map (\(a, b) -> (b, a))
  . Map.toList

displayTokenType :: TokenType -> Text
displayTokenType = \case
  TSpaces spcs -> case spcs of
    TSpace t          -> t
    TLineComment t    -> t
    TBlockComment t   -> t
    EOF               -> ""
  TLiterals lit -> case lit of
    TIntLit t _i      -> t
    TRationalLit t _i -> t
    TStringLit s      -> showStringLit s
  TAnnotations ann -> case ann of
    TNlg t ty         -> toNlgAnno t ty
    TRef t ty         -> toRefAnno t ty
    TRefSrc t         -> "@ref-src" <> t
    TRefMap t         -> "@ref-map" <> t
    TNlgString t      -> t
    TNlgPrefix        -> "@nlg"
    TDesc t           -> "@desc" <> t
  TIdentifiers i -> case i of
    TGenitive         -> "'s"
    TIdentifier t     -> t
    TQuoted t         -> "`" <> t <> "`"
  TDirectives dir -> ("#" <>) $ inverseCompleteLookup dir directives
  TKeywords kws -> inverseCompleteLookup kws keywords
  -- NOTE: we cannot look up TCopy (Just _) in the map - instead we just act as if it was
  -- TCopy Nothing, which is fine
  RealTCopy _ -> displayTokenType (TSymbols (TCopy Nothing))
  TSymbols sym -> inverseCompleteLookup sym symbols
  TOperators op -> inverseCompleteLookup op operators

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
posTokenCategory = \ case
  TSpaces EOF -> CEOF
  TSpaces TLineComment {} -> CComment
  TSpaces TBlockComment {} -> CComment
  TSpaces TSpace {} -> CWhitespace
  TLiterals TStringLit {} -> CStringLit
  TLiterals TIntLit {} -> CNumberLit
  TLiterals TRationalLit {} -> CNumberLit
  TOperators {} -> COperator
  TDirectives {} -> CDirective
  TKeywords {} -> CKeyword
  TAnnotations {} -> CAnnotation
  RealTCopy tt -> posTokenCategory tt
  TSymbols {} -> CSymbol
  TIdentifiers {} -> CIdentifier

showNormalizedUri :: NormalizedUri -> String
showNormalizedUri =  Text.unpack . (.getUri) . fromNormalizedUri

-- | A TCopy that carries a copied token
pattern RealTCopy :: TokenType -> TokenType
pattern RealTCopy tt = TSymbols (TCopy (Just tt))
