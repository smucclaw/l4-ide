{-# LANGUAGE TypeFamilies #-}

module LSP.L4.Rules where

import L4.Annotation
import L4.Evaluate
import L4.FindDefinition (toResolved)
import L4.Lexer (PosToken, SrcPos (..), SrcRange)
import qualified L4.Lexer as Lexer
import qualified L4.Parser as Parser
import qualified L4.Parser.ResolveAnnotation as Resolve
import qualified L4.Print as Print
import L4.Citations
import L4.Syntax
import L4.TypeCheck (CheckErrorWithContext (..), CheckResult (..), Substitution)
import qualified L4.TypeCheck as TypeCheck
import L4.Parser.SrcSpan

import Control.DeepSeq
import Control.Lens ((^.))
import Data.Foldable (Foldable (..))
import Data.Hashable (Hashable)
import Data.Functor.Compose (Compose(..))
import Data.Text (Text)
import UnliftIO (liftIO)
import Data.Map.Monoidal (MonoidalMap)
import qualified Data.Map.Monoidal as MonoidalMap
import qualified Data.Maybe as Maybe
import qualified Base.Text as Text
import qualified Data.Text.Mixed.Rope as Rope
import HaskellWorks.Data.IntervalMap.FingerTree (IntervalMap)
import qualified HaskellWorks.Data.IntervalMap.FingerTree as IVMap
import Development.IDE.Graph
import GHC.Generics (Generic, Generically (..))
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
import Optics ((&), (.~))

type instance RuleResult GetLexTokens = ([PosToken], Text)
data GetLexTokens = GetLexTokens
  deriving stock (Generic, Show, Eq)
  deriving anyclass (NFData, Hashable)

type instance RuleResult GetParsedAst = Program Name
data GetParsedAst = GetParsedAst
  deriving stock (Generic, Show, Eq)
  deriving anyclass (NFData, Hashable)

type instance RuleResult TypeCheck = TypeCheckResult
data TypeCheck = TypeCheck
  deriving stock (Generic, Show, Eq)
  deriving anyclass (NFData, Hashable)

type instance RuleResult SuccessfulTypeCheck = TypeCheckResult
data SuccessfulTypeCheck = SuccessfulTypeCheck
  deriving stock (Generic, Show, Eq)
  deriving anyclass (NFData, Hashable)

data TypeCheckResult = TypeCheckResult
  { program :: Program Resolved
  , substitution :: Substitution
  , success :: Bool
  , environment :: TypeCheck.Environment
  , entityInfo :: TypeCheck.EntityInfo
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (NFData)

type instance RuleResult Evaluate = ()
data Evaluate = Evaluate
  deriving stock (Generic, Show, Eq)
  deriving anyclass (NFData, Hashable)

type instance RuleResult LexerSemanticTokens = [SemanticToken]
data LexerSemanticTokens = LexerSemanticTokens
  deriving stock (Generic, Show, Eq)
  deriving anyclass (NFData, Hashable)

type instance RuleResult ParserSemanticTokens = [SemanticToken]
data ParserSemanticTokens = ParserSemanticTokens
  deriving stock (Generic, Show, Eq)
  deriving anyclass (NFData, Hashable)

type instance RuleResult GetSemanticTokens = [SemanticToken]
data GetSemanticTokens = GetSemanticTokens
  deriving stock (Generic, Show, Eq)
  deriving anyclass (NFData, Hashable)

type instance RuleResult GetRelSemanticTokens = [UInt]
data GetRelSemanticTokens = GetRelSemanticTokens
  deriving stock (Generic, Show, Eq)
  deriving anyclass (NFData, Hashable)

-- TODO:
-- in future we want to have SrcPos |-> Uri s.t. we can resolve
-- relative locations based on the scope, i.e. if we have
-- DECLARE foo <<british nationality act>>
--   IF bar <<sec. 3>>
-- then this should assemble the uri into one link based on
-- an uri scheme described in the original file
type instance RuleResult ResolveReferenceAnnotations = IntervalMap SrcPos (Int, Maybe Text)
data ResolveReferenceAnnotations = ResolveReferenceAnnotations
  deriving stock (Generic, Show, Eq)
  deriving anyclass (NFData, Hashable)

type instance RuleResult GetReferences = ReferenceMapping
data GetReferences = GetReferences
  deriving stock (Generic, Show, Eq)
  deriving anyclass (NFData, Hashable)

data ReferenceMapping =
  ReferenceMapping
  { actualToOriginal :: IntervalMap SrcPos Unique
  -- ^ getting the original occurence of a name, based on its source range
  , originalToActual :: MonoidalMap Unique [SrcRange]
  -- ^ getting the source range of all references of an original definition
  }
  deriving stock Generic
  deriving anyclass NFData
  deriving (Semigroup, Monoid) via Generically ReferenceMapping

singletonReferenceMapping :: Unique -> SrcRange -> ReferenceMapping
singletonReferenceMapping originalName actualRange
  = ReferenceMapping
  { actualToOriginal = IVMap.singleton (srcRangeToInterval actualRange) originalName
  , originalToActual = MonoidalMap.singleton originalName [actualRange]
  }

lookupReference :: SrcPos -> ReferenceMapping -> [SrcRange]
lookupReference pos mapping = do
  (_, n) <- IVMap.search pos mapping.actualToOriginal
  Maybe.fromMaybe [] $ MonoidalMap.lookup n mapping.originalToActual

data Log
  = ShakeLog Shake.Log
  deriving (Show)

instance Pretty Log where
  pretty = \case
    ShakeLog msg -> pretty msg

jl4Rules :: Recorder (WithPriority Log) -> Rules ()
jl4Rules recorder = do
  define shakeRecorder $ \GetLexTokens uri -> do
    (_, mRope) <- use_ GetFileContents uri
    case mRope of
      Nothing -> pure ([{- TODO: report internal errors -}], Nothing)
      Just rope -> do
        let
          contents = Rope.toText rope
        case Lexer.execLexer (Text.unpack (fromNormalizedUri uri).getUri) contents of
          Left errs -> do
            let
              diags = toList $ fmap (mkSimpleDiagnostic . Parser.mkPError "lexer") errs
            pure (fmap (mkSimpleFileDiagnostic uri) diags, Nothing)
          Right ts ->
            pure ([], Just (ts, contents))

  define shakeRecorder $ \GetParsedAst uri -> do
    (tokens, contents) <- use_ GetLexTokens uri
    case Parser.execProgramParserForTokens (Text.unpack (fromNormalizedUri uri).getUri) contents tokens of
      Left errs -> do
        let
          diags = toList $ fmap mkSimpleDiagnostic errs
        pure (fmap (mkSimpleFileDiagnostic uri) diags , Nothing)
      Right (prog, warns) -> do
        let
          diags = fmap mkNlgWarning warns
        pure (fmap (mkSimpleFileDiagnostic uri) diags, Just prog)

  define shakeRecorder $ \TypeCheck f -> do
    parsed <- use_ GetParsedAst f
    let result = TypeCheck.doCheckProgram parsed
    pure
      ( fmap (checkErrorToDiagnostic >>= mkFileDiagnosticWithSource f) result.errors
      , Just TypeCheckResult
        { program = result.program
        , substitution = result.substitution
        , environment = result.environment
        , entityInfo = result.entityInfo
        , success = all ((== TypeCheck.SInfo) . TypeCheck.severity) result.errors
        }
      )

  define shakeRecorder $ \SuccessfulTypeCheck f -> do
    typeCheckResult <- use_ TypeCheck f
    if typeCheckResult.success
      then pure ([], Just typeCheckResult)
      else pure ([], Nothing)

  define shakeRecorder $ \Evaluate f -> do
    r <- use_ SuccessfulTypeCheck f
    let results = doEvalProgram r.program
    pure (mkSimpleFileDiagnostic f . evalResultToDiagnostic <$> results, Just ())

  define shakeRecorder $ \LexerSemanticTokens f -> do
    (tokens, _) <- use_ GetLexTokens f
    case runSemanticTokensM (defaultSemanticTokenCtx ()) tokens of
      Left _err ->
        pure ([{- TODO: Log error -}], Nothing)
      Right tokenized -> do
        pure ([], Just tokenized)

  define shakeRecorder $ \ParserSemanticTokens f -> do
    prog <- use_ GetParsedAst f
    case runSemanticTokensM (defaultSemanticTokenCtx CValue) prog of
      Left _err ->
        pure ([{- TODO: Log error -}], Nothing)
      Right tokenized -> do
          pure ([], Just tokenized)

  define shakeRecorder $ \GetSemanticTokens f -> do
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
              Just newPos -> Just (t & #start .~ newPos)
            ) progTokens

          newPosLexTokens = Maybe.mapMaybe (\t ->
            case toCurrentPosition lexPositionMapping t.start of
              Nothing -> Nothing
              Just newPos -> Just (t & #start .~ newPos)
            ) lexTokens

        pure ([], Just $ mergeSameLengthTokens newPosAstTokens newPosLexTokens)

  define shakeRecorder $ \GetRelSemanticTokens f -> do
    tokens <- use_ GetSemanticTokens f
    let semanticTokens = relativizeTokens $ fmap toSemanticTokenAbsolute tokens
    case encodeTokens defaultSemanticTokensLegend semanticTokens of
      Left _err ->
        pure ([{- TODO: Log error -}], Nothing)
      Right relSemTokens ->
          pure ([], Just relSemTokens)
  define shakeRecorder $ \ResolveReferenceAnnotations uri -> case uriToNormalizedFilePath uri of
    -- TODO: this should load citations from a "central place" as long as we don't
    -- support citations directly in the file
    Nothing -> pure ([], Nothing)
    Just f -> do
      ownPath <- normalizedFilePathToOsPath f
      (tokens, _) <- use_ GetLexTokens uri

      -- obtain a valid relative file path from the ref-src annos
      let refSrcs = foldMap (validRelPath ownPath) tokens

      -- read the contents of the filepaths specified
      contents <- traverse (liftIO . readContents) $ Compose refSrcs

      -- parse the file contents from csv into intervalmaps from the sources of
      -- the annos to the reference they represent
      let references = getCompose $ mkReferences tokens <$> contents

      -- report any errors encountered while parsing any of the ref-src annos,
      -- annotate them on the ref-src annos they originated from and finally
      -- union all interval maps
      let (errs, mps) = partitionEithersOnL mkDiagnostic references
          mkDiagnostic loc err =
            FileDiagnostic
              { fdLspDiagnostic =
                Diagnostic
                  { _source = Just "jl4"
                  , _severity = Just DiagnosticSeverity_Warning
                  , _range = srcRangeToLspRange $ Just loc
                  , _message = Text.pack err
                  , _relatedInformation = Nothing
                  , _data_ = Nothing
                  , _codeDescription = Nothing
                  , _tags = Nothing
                  , _code = Nothing
                  }
              , fdFilePath = uri
              , fdShouldShowDiagnostic = ShowDiag
              , fdOriginalSource = NoMessage
              }

      pure (errs, case mps of [] -> Nothing; xs -> Just $ mconcat xs)

  define shakeRecorder $ \GetReferences f -> do
    tcRes <- use_ TypeCheck f

    let spanOf resolved
          = maybe
              mempty
              (singletonReferenceMapping $ getUnique resolved)
              -- NOTE: the source range of the actual Name
              (rangeOf resolved)

        resolveds = foldMap spanOf $ toResolved tcRes.program

    pure ([], Just resolveds)

  where
    shakeRecorder = cmapWithPrio ShakeLog recorder
    mkSimpleFileDiagnostic nfp diag =
      FileDiagnostic
        { fdFilePath = nfp
        , fdShouldShowDiagnostic = ShowDiag
        , fdLspDiagnostic = diag
        , fdOriginalSource = NoMessage
        }

    mkFileDiagnosticWithSource nfp diag orig =
      FileDiagnostic
        { fdFilePath = nfp
        , fdShouldShowDiagnostic = ShowDiag
        , fdLspDiagnostic = diag
        , fdOriginalSource = MkSomeMessage orig
        }

    mkNlgWarning :: Resolve.Warning -> Diagnostic
    mkNlgWarning warn =
        Diagnostic
          { _range = rangeOfResolveWarning warn
          , _severity = Just LSP.DiagnosticSeverity_Warning
          , _code = Nothing
          , _codeDescription = Nothing
          , _source = Just "parser"
          , _message = prettyNlgResolveWarning warn
          , _tags = Nothing
          , _relatedInformation = Nothing
          , _data_ = Nothing
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
        , _message = either Text.show Print.prettyLayout res
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
        , _message = Text.unlines (TypeCheck.prettyCheckError checkError.kind)
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

pointRange :: Position -> Range
pointRange pos = Range pos pos

srcSpanToLspRange :: Maybe SrcSpan -> LSP.Range
srcSpanToLspRange Nothing = LSP.Range (LSP.Position 0 0) (LSP.Position 0 0)
srcSpanToLspRange (Just range) = LSP.Range (srcPosToLspPosition range.start) (srcPosToLspPosition range.end)

srcPosToLspPosition :: SrcPos -> LSP.Position
srcPosToLspPosition s =
  LSP.Position
    { _character = fromIntegral $ s.column - 1
    , _line = fromIntegral $ s.line - 1
    }

lspPositionToSrcPos :: LSP.Position -> SrcPos
lspPositionToSrcPos (LSP.Position { _character = c, _line = l }) =
  MkSrcPos (fromIntegral $ l + 1) (fromIntegral $ c + 1)

prettyNlgResolveWarning :: Resolve.Warning -> Text
prettyNlgResolveWarning = \case
  Resolve.NotAttached _ ->
    "Not attached to any valid syntax node."
  Resolve.UnknownLocation nlg -> Text.unlines
    [ "The following NLG Annotation has no source location. This might be an internal compiler error."
    , "```"
    , Print.prettyLayout nlg
    , "```"
    ]
  Resolve.Ambiguous name nlgs -> Text.unlines $
    [ "More than one NLG annotation attached to: " <> Print.prettyLayout name
    , "The following annotations would be attached:"
    , ""
    ] <> [ "* `" <> Print.prettyLayout n.payload <> "`" | n <- nlgs]

rangeOfResolveWarning :: Resolve.Warning -> LSP.Range
rangeOfResolveWarning = \case
  Resolve.NotAttached nlg ->
    srcSpanToLspRange $ Just nlg.range
  Resolve.UnknownLocation _ ->
    srcSpanToLspRange Nothing
  Resolve.Ambiguous name _ ->
    srcRangeToLspRange $ rangeOf name
