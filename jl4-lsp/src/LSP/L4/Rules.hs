{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PatternSynonyms #-}

module LSP.L4.Rules where

import Base hiding (use)
import L4.Annotation
import L4.Evaluate
import L4.FindDefinition (toResolved)
import L4.Lexer (PosToken, PError)
import L4.Parser.SrcSpan
import qualified L4.Lexer as Lexer
import qualified L4.Parser as Parser
import qualified L4.Parser.ResolveAnnotation as Resolve
import qualified L4.Print as Print
import L4.Citations
import L4.Syntax
import L4.TypeCheck (CheckErrorWithContext (..), CheckResult (..), Substitution)
import qualified L4.TypeCheck as TypeCheck

import Control.Applicative
import Control.Exception (assert)
import Control.Monad.Trans.Maybe
import Data.Hashable (Hashable)
import Data.Monoid (Ap (..))
import qualified Data.Map.Strict as Map
import Data.Map.Monoidal (MonoidalMap)
import qualified Data.Map.Monoidal as MonoidalMap
import qualified Data.Maybe as Maybe
import qualified Base.Text as Text
import qualified Data.Text.Mixed.Rope as Rope
import System.FilePath
import HaskellWorks.Data.IntervalMap.FingerTree (IntervalMap)
import qualified HaskellWorks.Data.IntervalMap.FingerTree as IVMap
import Development.IDE.Graph
import GHC.Generics (Generically (..))
import LSP.Core.PositionMapping
import LSP.Core.RuleTypes
import LSP.Core.Shake hiding (Log)
import qualified LSP.Core.Shake as Shake
import LSP.Core.Types.Diagnostics
import LSP.L4.SemanticTokens
import LSP.Logger
import LSP.SemanticTokens
import Language.LSP.Protocol.Types
import qualified Language.LSP.Protocol.Types as LSP
import Optics ((&), (.~))
import Data.Either (partitionEithers)
import qualified L4.ExactPrint as ExactPrint
import qualified Data.List as List
import qualified L4.Evaluate as Evaluate
import System.Directory

type instance RuleResult GetLexTokens = ([PosToken], Text)
data GetLexTokens = GetLexTokens
  deriving stock (Generic, Show, Eq)
  deriving anyclass (NFData, Hashable)

type instance RuleResult GetParsedAst = Module Name
data GetParsedAst = GetParsedAst
  deriving stock (Generic, Show, Eq)
  deriving anyclass (NFData, Hashable)

type instance RuleResult GetImports = [(Maybe SrcRange, NormalizedUri)]
data GetImports = GetImports
  deriving stock (Generic, Show, Eq)
  deriving anyclass (NFData, Hashable)

type instance RuleResult GetTypeCheckDependencies = [TypeCheckResult]
data GetTypeCheckDependencies = GetTypeCheckDependencies
  deriving stock (Generic, Show, Eq)
  deriving anyclass (NFData, Hashable)

type instance RuleResult TypeCheck = TypeCheckResult
data TypeCheck = TypeCheckNoCallstack
  deriving stock (Generic, Show, Eq)
  deriving anyclass (NFData, Hashable)

pattern TypeCheck :: WithCallStack TypeCheck
pattern TypeCheck = AttachCallStack [] TypeCheckNoCallstack

type instance RuleResult SuccessfulTypeCheck = TypeCheckResult
data SuccessfulTypeCheck = SuccessfulTypeCheck
  deriving stock (Generic, Show, Eq)
  deriving anyclass (NFData, Hashable)

data TypeCheckResult = TypeCheckResult
  { module' :: Module  Resolved
  , substitution :: Substitution
  , success :: Bool
  , environment :: TypeCheck.Environment
  , entityInfo :: TypeCheck.EntityInfo
  , infos :: [TypeCheck.CheckErrorWithContext]
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (NFData)

type instance RuleResult Evaluate = [EvalDirectiveResult]
data Evaluate = Evaluate
  deriving stock (Generic, Show, Eq)
  deriving anyclass (NFData, Hashable)

type instance RuleResult GetEvaluationDependencies = Evaluate.EvalState
data GetEvaluationDependencies = GetEvaluationDependencies
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

type instance RuleResult ExactPrint = Text
data ExactPrint = ExactPrint
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
  | LogTraverseAnnoError !Text !TraverseAnnoError
  | LogRelSemanticTokenError !Text
  deriving (Show)

instance Pretty Log where
  pretty = \case
    ShakeLog msg -> pretty msg
    LogTraverseAnnoError herald msg -> pretty herald <> ":" <+> pretty (prettyTraverseAnnoError msg)
    LogRelSemanticTokenError msg -> "Semantic Token " <+> pretty msg

jl4Rules :: FilePath -> Recorder (WithPriority Log) -> Rules ()
jl4Rules rootDirectory recorder = do
  define shakeRecorder $ \GetLexTokens uri -> do
    mRope <- runMaybeT $
      MaybeT (snd <$> use_ GetFileContents uri)
      <|> do
        -- TODO: how do we actually invalidate this VFS file
        -- (except by opening in the same editor session)
        -- do we check the last modified time or smth like that?
        -- I think basically as it is now we don't do anything like
        -- that and the current time check is basically redundant
        file <- hoistMaybe $ uriToNormalizedFilePath uri
        lift $ addVirtualFileFromFS file

    case mRope of
      Nothing -> pure ([mkSimpleFileDiagnostic uri (mkSimpleDiagnostic (fromNormalizedUri uri).getUri "could not obtain file contents" Nothing)], Nothing)
      Just rope -> do
        let contents = Rope.toText rope
        case Lexer.execLexer uri contents of
          Left errs -> do
            let diags = toList $ fmap mkParseErrorDiagnostic errs
            pure (fmap (mkSimpleFileDiagnostic uri) diags, Nothing)
          Right ts ->
            pure ([], Just (ts, contents))

  define shakeRecorder $ \GetParsedAst uri -> do
    (tokens, contents) <- use_ GetLexTokens uri
    case Parser.execProgramParserForTokens uri contents tokens of
      Left errs -> do
        let
          diags = toList $ fmap mkParseErrorDiagnostic errs
        pure (fmap (mkSimpleFileDiagnostic uri) diags , Nothing)
      Right (prog, warns) -> do
        let
          diags = fmap mkNlgWarning warns
        pure (fmap (mkSimpleFileDiagnostic uri) diags, Just prog)

  define shakeRecorder $ \GetImports uri -> do
    let -- NOTE: we curently don't allow any relative or absolute file paths, just bare module names
        mkImportPath (MkImport a n) = do
          let modName = takeBaseName $ Text.unpack $ rawNameToText $ rawName n
          -- NOTE: if the current URI is a file uri, we first check the directory relative to the current file
          mFileDirectory <- runMaybeT do
            -- TODO: idk if this is the best way of doing it, maybe trying the entire rule that uses the import and then
            -- failing there if the rule fails would be morally better? Seems like it is more incremental than doing it
            -- like this
            dir <- hoistMaybe $ takeDirectory . fromNormalizedFilePath <$> uriToNormalizedFilePath uri
            guard =<< liftIO (doesFileExist (dir </> modName <.> "l4"))
            pure dir

          pure (rangeOf a, fromMaybe rootDirectory mFileDirectory </> modName <.> "l4")

        mkImportUri range fp = do
          e <- doesFileExist fp
          let u = toNormalizedUri $ filePathToUri fp
              diag = do
                guard $ not e
                [mkSimpleFileDiagnostic u $ mkSimpleDiagnostic (fromNormalizedUri uri).getUri ("File does not exist: " <> Text.pack fp) (fromSrcRange <$> range)]
          pure (diag, range, u)

        mkDiagsAndImports = \case
          Import _a i -> Ap do
            (diag, r, u) <- liftIO . uncurry mkImportUri =<< mkImportPath i
            pure [(diag, (r, u))]
          _ -> pure []


    prog <- use_ GetParsedAst uri
    (diags, imports) <- fmap unzip $ getAp $ foldTopDecls mkDiagsAndImports prog
    pure (concat diags, Just imports)

  defineWithCallStack shakeRecorder $ \GetTypeCheckDependencies cs uri -> do
    imports <- use_  GetImports uri
    ress    <- fmap catMaybes $ uses (AttachCallStack cs TypeCheckNoCallstack) $ map snd imports
    pure ([], Just ress)

  defineWithCallStack shakeRecorder $ \TypeCheckNoCallstack cs uri -> do
    parsed <- use_ GetParsedAst uri
    deps   <- use_ (AttachCallStack (uri : cs) GetTypeCheckDependencies) uri
    let unionCheckStates :: TypeCheck.CheckState -> TypeCheckResult -> TypeCheck.CheckState
        unionCheckStates cState tcRes =
          TypeCheck.MkCheckState
          -- NOTE: the environments behave more like sets than like lists, that's why we need to union them
          { environment = Map.unionWith List.union cState.environment tcRes.environment
          -- NOTE: we assume that if we have a mapping from a specific unique then it must have come from the
          -- same module. That means that the rhs of it should be identical.
          , entityInfo = Map.unionWith (\t1 t2 -> assert (t1 == t2) t1) cState.entityInfo tcRes.entityInfo
          , substitution = Map.unionWith (\t1 t2 -> assert (t1 == t2) t1) cState.substitution tcRes.substitution
          , errorContext = cState.errorContext
          , supply = cState.supply
          }
        initial = foldl' unionCheckStates TypeCheck.initialCheckState deps
        result = TypeCheck.doCheckProgramWithDependencies initial uri parsed
        (infos, errors) = partition ((== TypeCheck.SInfo) . TypeCheck.severity) result.errors
    pure
      ( fmap (checkErrorToDiagnostic >>= mkFileDiagnosticWithSource uri) result.errors
      , Just TypeCheckResult
        { module' = result.program
        , substitution = result.substitution
        , environment = result.environment
        , entityInfo = result.entityInfo
        , success = null errors
        , infos
        }
      )

  define shakeRecorder $ \SuccessfulTypeCheck f -> do
    typeCheckResult <- use_ TypeCheck f
    if typeCheckResult.success
      then pure ([], Just typeCheckResult)
      else pure ([], Nothing)

  defineWithCallStack shakeRecorder $ \GetEvaluationDependencies cs f -> do
    imports <- use_  GetImports f
    tcRes   <- use_  SuccessfulTypeCheck f
    -- TODO: when checking for cycles, we should check which one is the
    -- first element in the cycle that is, i.e. which IMPORT, then scan
    -- for the IMPORT again and
    -- put the diagnostic on that IMPORT
    deps    <- fmap catMaybes $ uses (AttachCallStack (f : cs) GetEvaluationDependencies) $ map snd imports
    let environment = Evaluate.unionEnvironments $ map (.environment) deps
        own = execEvalModuleWithEnv environment tcRes.module'
    pure ([], Just own)

  define shakeRecorder $ \Evaluate uri -> do
    res  <- use_ (AttachCallStack [uri] GetEvaluationDependencies) uri
    let results = res.directiveResults
    pure (mkSimpleFileDiagnostic uri . evalResultToDiagnostic <$> results, Just results)

  define shakeRecorder $ \LexerSemanticTokens f -> do
    (tokens, _) <- use_ GetLexTokens f
    case runSemanticTokensM (defaultSemanticTokenCtx ()) tokens of
      Left err -> do
        logWith recorder Error $ LogTraverseAnnoError "Lexer" err
        pure ([], Nothing)
      Right tokenized -> do
        pure ([], Just tokenized)

  define shakeRecorder $ \ParserSemanticTokens f -> do
    prog <- use_ GetParsedAst f
    case runSemanticTokensM (defaultSemanticTokenCtx CValue) prog of
      Left err -> do
        logWith recorder Error $ LogTraverseAnnoError "Parser" err
        pure ([], Nothing)
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
      Left err -> do
        logWith recorder Error $ LogRelSemanticTokenError err
        pure ([], Nothing)
      Right relSemTokens ->
          pure ([], Just relSemTokens)
  define shakeRecorder $ \ResolveReferenceAnnotations uri -> case uriToNormalizedFilePath uri of
    -- TODO: this should load citations from a "central place" as long as we don't
    -- support citations directly in the file
    Nothing -> pure ([], Nothing)
    Just f -> do
      ownPath <- normalizedFilePathToOsPath f
      (tokens, _) <- use_ GetLexTokens uri

      -- obtain a valid relative file path from the ref-src annos and
      -- parse the file contents from csv into intervalmaps from the sources of
      -- the annos to the reference they represent
      refSrcs <- liftIO
        $ traverse
          (\n -> runExceptT do
             refSrc <- withRefSrc ownPath n
             let refMap = withRefMap n
             pure (refSrc <> refMap)
          )
          tokens

      -- report any errors encountered while parsing any of the ref-src annos,
      -- annotate them on the ref-src annos they originated from and finally
      -- union all interval maps
      let (errs, references) = partitionEithers refSrcs

          mkReferencesFromNonempty v
            | null v = Nothing
            | otherwise = Just $ mkReferences tokens v

          mps = case Maybe.mapMaybe mkReferencesFromNonempty references of
            [] -> Nothing
            xs -> Just $ mconcat xs

          diags = map (uncurry mkDiagnostic) errs

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

      pure (diags, mps)

  define shakeRecorder $ \GetReferences f -> do
    tcRes <- use_ TypeCheck f

    let spanOf resolved
          = maybe
              mempty
              (singletonReferenceMapping $ getUnique resolved)
              -- NOTE: the source range of the actual Name
              (rangeOf resolved)

        resolveds = foldMap spanOf $ toResolved tcRes.module'

    pure ([], Just resolveds)

  define shakeRecorder $ \ExactPrint f -> do
    parsed <- use_ GetParsedAst f
    let pfp = prettyFilePath $ fromNormalizedFilePath <$> uriToNormalizedFilePath f
    pure case ExactPrint.exactprint parsed of
      Left trErr -> ([mkSimpleFileDiagnostic f $ mkSimpleDiagnostic pfp (prettyTraverseAnnoError trErr) Nothing], Nothing)
      Right ep'd -> ([], Just ep'd)

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

    mkParseErrorDiagnostic :: PError -> Diagnostic
    mkParseErrorDiagnostic parseError = mkSimpleDiagnostic parseError.origin parseError.message (Just parseError.range)

    mkSimpleDiagnostic :: Text -> Text -> Maybe SrcSpan -> Diagnostic
    mkSimpleDiagnostic origin _message range =
      Diagnostic
        { _range = srcSpanToLspRange range
        , _severity = Just LSP.DiagnosticSeverity_Error
        , _code = Nothing
        , _codeDescription = Nothing
        , _source = Just origin
        , _message
        , _tags = Nothing
        , _relatedInformation = Nothing
        , _data_ = Nothing
        }

    evalResultToDiagnostic :: EvalDirectiveResult -> Diagnostic
    evalResultToDiagnostic (MkEvalDirectiveResult range res _trace) = do
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
