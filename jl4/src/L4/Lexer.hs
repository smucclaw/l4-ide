{-# LANGUAGE TypeFamilies #-}
module L4.Lexer where

import Base
import qualified Base.Map as Map
import qualified Base.Text as Text

import Data.Char hiding (Space)
import Data.List.NonEmpty (NonEmpty((:|)), nonEmpty)
import Data.Proxy
import Text.Megaparsec as Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.State
import qualified Text.Megaparsec.Char.Lexer as Lexer

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

-- | A pos token is a token with position information attached.
data PosToken =
  MkPosToken
    { range   :: !SrcRange
    , payload :: !TokenType
    }
  deriving stock (Eq, Ord, Show)

-- | A range of source positions. We store the length of a range as well.
data SrcRange =
  MkSrcRange
    { start   :: !SrcPos -- inclusive
    , end     :: !SrcPos -- inclusive
    , length  :: !Int
    }
  deriving stock (Eq, Ord, Show)

-- | A single source position. Line and column numbers are 1-based.
data SrcPos =
  MkSrcPos
    { filename :: !FilePath
    , line     :: !Int
    , column   :: !Int
    }
  deriving stock (Eq, Ord, Show)

-- | The type of token, plus information needed to reconstruct its contents.
data TokenType =
    TIdentifier   !Text
  | TQuoted       !Text
  | TIntLit       !Text !Int
  | TStringLit    !Text
    -- parentheses
  | TPOpen
  | TPClose
  | TCOpen
  | TCClose
  | TSOpen
  | TSClose
    -- punctuation
  | TComma
  | TSemicolon
  | TDot
    -- genitive
  | TGenitive
    -- symbolic operators
  | TParagraph
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
  | TOtherSymbolic !Text
    -- keywords
  | TKGiven
  | TKGiveth
  | TKDecide
  | TKDeclare
  | TKIf
  | TKThen
  | TKElse
  | TKOtherwise
  | TKFalse
  | TKTrue
  | TKAnd
  | TKOr
  | TKNot
  | TKIs
  | TKHas
  | TKOne
  | TKOf
  | TKA
  | TKAn
    -- space
  | TSpace        !Text
  | TLineComment  !Text
  | TBlockComment !Text
  | EOF
  deriving stock (Eq, Generic, Ord, Show)

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

directive :: Lexer Text
directive =
  char '#' *> identifier

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
  <|> TGenitive       <$ string "'s"
  <|> TQuoted         <$> quoted
  <|> TSpace          <$> whitespace
  <|> TLineComment    <$> lineComment
  <|> TBlockComment   <$> blockComment
  <|> TPOpen          <$ char '('
  <|> TPClose         <$ char ')'
  <|> TCOpen          <$ char '{'
  <|> TCClose         <$ char '}'
  <|> TSOpen          <$ char '['
  <|> TSClose         <$ char ']'
  <|> TComma          <$ char ','
  <|> TSemicolon      <$ char ';'
  <|> TDot            <$ char '.'
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
    [ ("§" , TParagraph    )
    , ("*" , TTimes        )
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
    ]

keywords :: Map Text TokenType
keywords =
  Map.fromList
    [ ("GIVEN"      , TKGiven      )
    , ("GIVETH"     , TKGiveth     )
    , ("DECIDE"     , TKDecide     )
    , ("DECLARE"    , TKDeclare    )
    , ("IF"         , TKIf         )
    , ("THEN"       , TKThen       )
    , ("ELSE"       , TKElse       )
    , ("OTHERWISE"  , TKOtherwise  )
    , ("FALSE"      , TKFalse      )
    , ("TRUE"       , TKTrue       )
    , ("AND"        , TKAnd        )
    , ("OR"         , TKOr         )
    , ("NOT"        , TKNot        )
    , ("IS"         , TKIs         )
    , ("ONE"        , TKOne        )
    , ("OF"         , TKOf         )
    , ("A"          , TKA          )
    , ("AN"         , TKAn         )
    , ("HAS"        , TKHas        )
    ]

rawTokens :: Lexer [RawToken]
rawTokens = many (MkRawToken <$> getOffset <*> tokenPayload <*> getOffset)

execLexer :: FilePath -> Text -> Either String [PosToken]
execLexer file input =
  let
    r = parse (rawTokens <* eof) file input
  in
    case r of
      Right rtoks -> Right (mkPosTokens file input rtoks)
      Left errs   -> Left (errorBundlePretty errs)

mkPosTokens :: FilePath -> Text -> [RawToken] -> [PosToken]
mkPosTokens filepath txt rtoks =
    evalState (traverse go rtoks) (initialPosState filepath txt)
  where
    go :: RawToken -> StateT (PosState Text) Identity PosToken
    go rtok = do
      pst <- get
      let
        pstStart = reachOffsetNoLine rtok.start pst
        pstEnd   = reachOffsetNoLine rtok.end pstStart
      put pstEnd
      pure
        (MkPosToken
          (MkSrcRange
            (convertPos (pstateSourcePos pstStart))
            (convertPos (pstateSourcePos pstEnd  ))
            (rtok.end - rtok.start)
          )
          rtok.payload
        )

-- | Convert from a Megaparsec source position to one of ours.
convertPos :: SourcePos -> SrcPos
convertPos (SourcePos fn l c) =
  MkSrcPos fn (unPos l) (unPos c)

unconvertPos :: SrcPos -> SourcePos
unconvertPos (MkSrcPos fn l c) =
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
            xs -> unconvertPos (last xs).range.end
          (x : _) -> unconvertPos x.range.start

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


displayPosToken :: PosToken -> Text
displayPosToken (MkPosToken _r tt) =
  case tt of
    TIdentifier t    -> t
    TQuoted t        -> "`" <> t <> "`"
    TIntLit t _i     -> t
    TStringLit s     -> Text.pack (show s) -- ideally, this should be fine, because we use the Haskell escape sequences
    TPOpen           -> "("
    TPClose          -> ")"
    TCOpen           -> "{"
    TCClose          -> "}"
    TSOpen           -> "["
    TSClose          -> "]"
    TComma           -> ","
    TSemicolon       -> ";"
    TDot             -> "."
    TGenitive        -> "'s"
    TParagraph       -> "§"
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
    TOtherSymbolic t -> t
    TKGiven          -> "GIVEN"
    TKGiveth         -> "GIVETH"
    TKDecide         -> "DECIDE"
    TKDeclare        -> "DECLARE"
    TKIf             -> "IF"
    TKThen           -> "THEN"
    TKElse           -> "ELSE"
    TKOtherwise      -> "OTHERWISE"
    TKFalse          -> "FALSE"
    TKTrue           -> "TRUE"
    TKAnd            -> "AND"
    TKOr             -> "OR"
    TKNot            -> "NOT"
    TKIs             -> "IS"
    TKHas            -> "HAS"
    TKOne            -> "ONE"
    TKOf             -> "OF"
    TKA              -> "A"
    TKAn             -> "AN"
    TSpace t         -> t
    TLineComment t   -> t
    TBlockComment t  -> t
    EOF              -> ""

