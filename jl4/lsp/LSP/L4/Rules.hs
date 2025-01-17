{-# LANGUAGE TypeFamilies #-}

module LSP.L4.Rules where

import Control.DeepSeq
import Control.Lens
import Data.Foldable (Foldable (..))
import Data.Hashable (Hashable)
import Data.Text (Text)
import qualified Data.Text.Mixed.Rope as Rope
import Data.Typeable
import Development.IDE.Graph
import GHC.Generics (Generic)
import L4.Lexer (PosToken, SrcPos(..))
import qualified L4.Lexer as Lexer
import qualified L4.Parser as Parser
import L4.Syntax
import LSP.Core.RuleTypes
import LSP.Core.Shake hiding (Log)
import qualified LSP.Core.Shake as Shake
import LSP.Core.Types.Diagnostics
import LSP.Logger
import qualified Language.LSP.Protocol.Lens as J
import Language.LSP.Protocol.Types
import qualified Language.LSP.Protocol.Types as LSP
import L4.TypeCheck (CheckErrorWithContext(..), Substitution )
import qualified L4.TypeCheck as TypeCheck
import L4.Lexer (SrcRange)
import L4.ExactPrint (HasSrcRange(..))
import LSP.SemanticTokens
import LSP.L4.SemanticTokens


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

data TypeCheckResult = TypeCheckResult
  { program :: Program Resolved
  , substitution :: Substitution
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (NFData)

type instance RuleResult LexerSemanticTokens = [UInt]
data LexerSemanticTokens = LexerSemanticTokens
  deriving stock (Generic, Show, Eq)
  deriving anyclass (NFData, Typeable, Hashable)

type instance RuleResult ParserSemanticTokens = [UInt]
data ParserSemanticTokens = ParserSemanticTokens
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
        }
      )

  define (cmapWithPrio ShakeLog recorder) $ \LexerSemanticTokens f -> do
    (tokens, _) <- use_ GetLexTokens f
    case runSemanticTokensM defaultSemanticTokenCtx tokens of
      Left _err ->
        pure ([{- TODO: Log error -}], Nothing)
      Right tokenized -> do
        let
          semanticTokens = relativizeTokens $ fmap toSemanticTokenAbsolute tokenized
        case encodeTokens defaultSemanticTokensLegend semanticTokens of
          Left _err ->
            pure ([{- TODO: Log error -}], Nothing)
          Right relSemTokens ->
            pure ([], Just relSemTokens)

  define (cmapWithPrio ShakeLog recorder) $ \ParserSemanticTokens f -> do
    prog <- use_ GetParsedAst f
    case runSemanticTokensM defaultSemanticTokenCtx prog of
      Left _err ->
        pure ([{- TODO: Log error -}], Nothing)
      Right tokenized -> do
        let
          semanticTokens = relativizeTokens $ fmap toSemanticTokenAbsolute tokenized
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

    checkErrorToDiagnostic :: CheckErrorWithContext -> Diagnostic
    checkErrorToDiagnostic checkError =
      Diagnostic
        { _range = srcRangeToLspRange (rangeOf checkError)
        , _severity = Just LSP.DiagnosticSeverity_Error
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
