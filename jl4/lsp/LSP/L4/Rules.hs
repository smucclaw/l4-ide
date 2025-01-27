{-# LANGUAGE TypeFamilies #-}

module LSP.L4.Rules where

import Control.DeepSeq
import Control.Lens ((^.))
import Data.Foldable (Foldable (..))
import Data.Hashable (Hashable)
import qualified Data.Maybe as Maybe
import Data.Text (Text, pack)
import qualified Data.Text.Mixed.Rope as Rope
import Data.Typeable
import Development.IDE.Graph
import GHC.Generics (Generic)
import L4.Evaluate
import L4.Annotation (HasSrcRange (..))
import L4.Lexer (PosToken, SrcPos (..), SrcRange)
import qualified L4.Lexer as Lexer
import qualified L4.Parser as Parser
import L4.Syntax
import L4.TypeCheck (CheckErrorWithContext (..), Substitution)
import qualified L4.TypeCheck as TypeCheck
import LSP.Core.PositionMapping
import LSP.Core.RuleTypes
import LSP.Core.Shake hiding (Log)
import qualified LSP.Core.Shake as Shake
import LSP.Core.Types.Diagnostics
import LSP.L4.SemanticTokens
import LSP.Logger
import LSP.SemanticTokens
import qualified Language.LSP.Protocol.Lens as J
import Language.LSP.Protocol.Types
import qualified Language.LSP.Protocol.Types as LSP


type instance RuleResult GetLexTokens = ([PosToken], Text)
data GetLexTokens = GetLexTokens
  deriving stock (Generic, Show, Eq)
  deriving anyclass (NFData, Typeable, Hashable)

type instance RuleResult GetParsedAst = Program Name
data GetParsedAst = GetParsedAst
  deriving stock (Generic, Show, Eq)
  deriving anyclass (NFData, Typeable, Hashable)

type instance RuleResult TypeCheck = TypeCheckResult
data TypeCheck = TypeCheck
  deriving stock (Generic, Show, Eq)
  deriving anyclass (NFData, Typeable, Hashable)

type instance RuleResult SuccessfulTypeCheck = TypeCheckResult
data SuccessfulTypeCheck = SuccessfulTypeCheck
  deriving stock (Generic, Show, Eq)
  deriving anyclass (NFData, Typeable, Hashable)

data TypeCheckResult = TypeCheckResult
  { program :: Program Resolved
  , substitution :: Substitution
  , success :: Bool
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (NFData)

type instance RuleResult Evaluate = ()
data Evaluate = Evaluate
  deriving stock (Generic, Show, Eq)
  deriving anyclass (NFData, Typeable, Hashable)

type instance RuleResult LexerSemanticTokens = [SemanticToken]
data LexerSemanticTokens = LexerSemanticTokens
  deriving stock (Generic, Show, Eq)
  deriving anyclass (NFData, Typeable, Hashable)

type instance RuleResult ParserSemanticTokens = [SemanticToken]
data ParserSemanticTokens = ParserSemanticTokens
  deriving stock (Generic, Show, Eq)
  deriving anyclass (NFData, Typeable, Hashable)

type instance RuleResult GetSemanticTokens = [SemanticToken]
data GetSemanticTokens = GetSemanticTokens
  deriving stock (Generic, Show, Eq)
  deriving anyclass (NFData, Typeable, Hashable)

type instance RuleResult GetRelSemanticTokens = [UInt]
data GetRelSemanticTokens = GetRelSemanticTokens
  deriving stock (Generic, Show, Eq)
  deriving anyclass (NFData, Typeable, Hashable)

data Log
  = ShakeLog Shake.Log
  deriving (Show)

instance Pretty Log where
  pretty = \case
    ShakeLog msg -> pretty msg

jl4Rules :: Recorder (WithPriority Log) -> Rules ()
jl4Rules recorder = do
  define (cmapWithPrio ShakeLog recorder) $ \GetLexTokens f -> do
    (_, mRope) <- use_ GetFileContents f
    case mRope of
      Nothing -> pure ([{- TODO: report internal errors -}], Nothing)
      Just rope -> do
        let
          contents = Rope.toText rope
        case Lexer.execLexer (fromNormalizedFilePath f) contents of
          Left errs -> do
            let
              diags = toList $ fmap (mkSimpleDiagnostic . Parser.mkPError "lexer") errs
            pure (fmap (mkSimpleFileDiagnostic f) diags, Nothing)
          Right ts ->
            pure ([], Just (ts, contents))

  define (cmapWithPrio ShakeLog recorder) $ \GetParsedAst f -> do
    (tokens, contents) <- use_ GetLexTokens f
    case Parser.execParserForTokens Parser.program (fromNormalizedFilePath f) contents tokens of
      Left errs -> do
        let
          diags = toList $ fmap mkSimpleDiagnostic errs
        pure (fmap (mkSimpleFileDiagnostic f) diags , Nothing)
      Right prog -> do
        pure ([], Just prog)

  define (cmapWithPrio ShakeLog recorder) $ \TypeCheck f -> do
    program <- use_ GetParsedAst f
    let
      (errs, resolvedProg, subst) = TypeCheck.doCheckProgram program

    pure
      ( fmap (mkSimpleFileDiagnostic f . checkErrorToDiagnostic) errs
      , Just TypeCheckResult
        { program = resolvedProg
        , substitution = subst
        , success = all ((== TypeCheck.SInfo) . TypeCheck.severity) errs
        }
      )

  define (cmapWithPrio ShakeLog recorder) $ \SuccessfulTypeCheck f -> do
    typeCheckResult <- use_ TypeCheck f
    if typeCheckResult.success
      then pure ([], Just typeCheckResult)
      else pure ([], Nothing)

  define (cmapWithPrio ShakeLog recorder) $ \Evaluate f -> do
    r <- use_ SuccessfulTypeCheck f
    let results = doEvalProgram r.program
    pure (mkSimpleFileDiagnostic f . evalResultToDiagnostic <$> results, Just ())

  define (cmapWithPrio ShakeLog recorder) $ \LexerSemanticTokens f -> do
    (tokens, _) <- use_ GetLexTokens f
    case runSemanticTokensM defaultSemanticTokenCtx tokens of
      Left _err ->
        pure ([{- TODO: Log error -}], Nothing)
      Right tokenized -> do
        pure ([], Just tokenized)

  define (cmapWithPrio ShakeLog recorder) $ \ParserSemanticTokens f -> do
    prog <- use_ GetParsedAst f
    case runSemanticTokensM defaultSemanticTokenCtx prog of
      Left _err ->
        pure ([{- TODO: Log error -}], Nothing)
      Right tokenized -> do
          pure ([], Just tokenized)

  define (cmapWithPrio ShakeLog recorder) $ \GetSemanticTokens f -> do
    mSemTokens <- useWithStale ParserSemanticTokens f
    case mSemTokens of
      Nothing -> do
        -- If we don't even have any old result, just try to use lexer results
        lexToks <- use LexerSemanticTokens f
        pure ([], lexToks)
      Just (progTokens, positionMapping) -> do

        -- Throwing is ok, since if `ParserSemanticTokens` produces a result
        -- so does `LexerSemanticTokens`.
        (lexTokens, lexPositionMapping) <- useWithStale_ LexerSemanticTokens f
        let
          -- We assume that semantic tokens do *not* change its length, no matter whether they
          -- have been lexed, parsed or typechecked.
          -- A rather bold assumption, tbh. It will almost definitely not hold
          -- up in practice, but let's do one step at a time.
          mergeSameLengthTokens :: [SemanticToken] -> [SemanticToken] -> [SemanticToken]
          mergeSameLengthTokens [] bs = bs
          mergeSameLengthTokens as [] = as
          mergeSameLengthTokens (a:as) (b:bs) = case compare a.start b.start of
            -- a.start == b.start
            -- Same token, only print one
            EQ -> a : mergeSameLengthTokens as bs
            -- a.start < b.start
            LT -> a : mergeSameLengthTokens as (b:bs)
            -- a.start > b.start
            GT -> b : mergeSameLengthTokens (a:as) bs

          newPosAstTokens = Maybe.mapMaybe (\t ->
            case toCurrentPosition positionMapping t.start of
              Nothing -> Nothing
              Just newPos -> Just (t { start = newPos })
            ) progTokens

          newPosLexTokens = Maybe.mapMaybe (\t ->
            case toCurrentPosition lexPositionMapping t.start of
              Nothing -> Nothing
              Just newPos -> Just (t { start = newPos })
            ) lexTokens

        pure ([], Just $ mergeSameLengthTokens newPosAstTokens newPosLexTokens)

  define (cmapWithPrio ShakeLog recorder) $ \GetRelSemanticTokens f -> do
    tokens <- use_ GetSemanticTokens f
    let semanticTokens = relativizeTokens $ fmap toSemanticTokenAbsolute tokens
    case encodeTokens defaultSemanticTokensLegend semanticTokens of
      Left _err ->
        pure ([{- TODO: Log error -}], Nothing)
      Right relSemTokens ->
          pure ([], Just relSemTokens)
  where
    mkSimpleFileDiagnostic nfp diag =
      FileDiagnostic
        { fdFilePath = nfp
        , fdShouldShowDiagnostic = ShowDiag
        , fdLspDiagnostic = diag
        }

    mkSimpleDiagnostic parseError =
      Diagnostic
        { _range = LSP.Range start (extendToNextLine start)
        , _severity = Just LSP.DiagnosticSeverity_Error
        , _code = Nothing
        , _codeDescription = Nothing
        , _source = Just parseError.origin
        , _message = parseError.message
        , _tags = Nothing
        , _relatedInformation = Nothing
        , _data_ = Nothing
        }
     where
      start = srcPosToLspPosition parseError.start

    evalResultToDiagnostic :: EvalResult -> Diagnostic
    evalResultToDiagnostic (range, res) =
      Diagnostic
        { _range = srcRangeToLspRange (Just range)
        , _severity = Just LSP.DiagnosticSeverity_Information
        , _code = Nothing
        , _codeDescription = Nothing
        , _source = Just "eval"
        , _message = either (pack . show) renderValue res
        , _tags = Nothing
        , _relatedInformation = Nothing
        , _data_ = Nothing
        }

    checkErrorToDiagnostic :: CheckErrorWithContext -> Diagnostic
    checkErrorToDiagnostic checkError =
      Diagnostic
        { _range = srcRangeToLspRange (rangeOf checkError)
        , _severity = Just (translateSeverity (TypeCheck.severity checkError))
        , _code = Nothing
        , _codeDescription = Nothing
        , _source = Just "check"
        , _message = TypeCheck.prettyCheckErrorWithContext checkError
        , _tags = Nothing
        , _relatedInformation = Nothing
        , _data_ = Nothing
        }

    extendToNextLine p =
      LSP.Position
        { _character = 0
        , _line = p ^. J.line + 1
        }

translateSeverity :: TypeCheck.Severity -> DiagnosticSeverity
translateSeverity TypeCheck.SInfo  = LSP.DiagnosticSeverity_Information
translateSeverity TypeCheck.SWarn  = LSP.DiagnosticSeverity_Warning
translateSeverity TypeCheck.SError = LSP.DiagnosticSeverity_Error

srcRangeToLspRange :: Maybe SrcRange -> LSP.Range
srcRangeToLspRange Nothing = LSP.Range (LSP.Position 0 0) (LSP.Position 0 0)
srcRangeToLspRange (Just range) = LSP.Range (srcPosToLspPosition range.start) (srcPosToLspPosition range.end)

srcPosToLspPosition :: SrcPos -> LSP.Position
srcPosToLspPosition s =
  LSP.Position
    { _character = fromIntegral $ s.column - 1
    , _line = fromIntegral $ s.line - 1
    }

lspPositionToSrcPos :: LSP.Position -> SrcPos
lspPositionToSrcPos (LSP.Position { _character = c, _line = l }) =
  MkSrcPos "" (fromIntegral $ l + 1) (fromIntegral $ c + 1)
