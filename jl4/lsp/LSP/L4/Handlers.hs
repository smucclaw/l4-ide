{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}

module LSP.L4.Handlers where

import Control.Concurrent.STM
import Control.Concurrent.Strict (Chan, writeChan)
import Control.Exception.Safe (MonadCatch, MonadMask, MonadThrow)
import Control.Lens ((^.))
import Control.Monad.Extra (guard, whenJust)
import qualified Control.Monad.Extra as Extra
import Control.Monad.IO.Class
import Control.Monad.Reader (MonadReader (..))
import Control.Monad.Trans.Reader (ReaderT)
import Data.Char (isAlphaNum)
import Data.Maybe (mapMaybe)
import Data.Monoid (Ap (..))
import qualified Control.Monad.Trans.Reader as ReaderT
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Text as Aeson
import qualified Text.Fuzzy as Fuzzy
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Mixed.Rope as Rope
import LSP.Core.FileStore hiding (Log (..))
import qualified LSP.Core.FileStore as FileStore
import LSP.Core.OfInterest hiding (Log (..))
import LSP.Core.PositionMapping
import LSP.Core.Service hiding (Log (..))
import LSP.Core.Shake hiding (Log (..))
import qualified LSP.Core.Shake as Shake
import LSP.Core.Types.Diagnostics
import LSP.Core.Types.Location
import LSP.L4.Base
import LSP.L4.Config
import qualified LSP.L4.Viz.Ladder as Ladder
import LSP.L4.Rules hiding (Log (..))
import LSP.Logger
import qualified Language.LSP.Protocol.Lens as J
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types
import qualified Language.LSP.Protocol.Types as LSP
import Language.LSP.Protocol.Types as CompletionItem (CompletionItem (..)) 
import Language.LSP.Server hiding (notificationHandler, requestHandler)
import qualified Language.LSP.Server as LSP
import Language.LSP.VFS (VFS)
import qualified Optics
import qualified StmContainers.Map as STM
import UnliftIO (MonadUnliftIO)

data ReactorMessage
  = ReactorNotification (IO ())
  | forall m . ReactorRequest (LspId m) (IO ()) (TResponseError m -> IO ())

type ReactorChan = Chan ReactorMessage

data ServerState =
  ServerState
    { reactor :: ReactorChan
    , ideState :: IdeState
    }

newtype ServerM c a = ServerM { runServerT :: ReaderT ServerState (LspT c IO) a }
  deriving (Semigroup, Monoid) via (Ap (ServerM c) a)
  -- ^ 'Ap' lifts the @'Monoid' a@ through the @'Applicative' 'ServerM' c@
  deriving newtype (Functor, Applicative, Monad, MonadReader ServerState)
  deriving newtype (MonadLsp c, MonadCatch, MonadIO, MonadMask, MonadThrow, MonadUnliftIO)

runServerM :: ServerState -> ServerM c a -> LspM c a
runServerM st m = ReaderT.runReaderT m.runServerT st


data Log
  = LogOpenedTextDocument !Uri
  | LogModifiedTextDocument !Uri
  | LogSavedTextDocument !Uri
  | LogClosedTextDocument !Uri
  | LogRequestedCompletionsFor !Text
  | LogFileStore FileStore.Log
  | LogShake Shake.Log
  deriving (Show)

instance Pretty Log where
  pretty = \case
    LogOpenedTextDocument uri ->  "Opened text document:" <+> pretty (getUri uri)
    LogModifiedTextDocument uri -> "Modified text document:" <+> pretty (getUri uri)
    LogSavedTextDocument uri -> "Saved text document:" <+> pretty (getUri uri)
    LogClosedTextDocument uri -> "Closed text document:" <+> pretty (getUri uri)
    LogRequestedCompletionsFor t -> "requesting completions for:" <+> pretty t
    LogFileStore msg -> pretty msg
    LogShake msg -> pretty msg

-- ----------------------------------------------------------------------------
-- Reactor
-- ----------------------------------------------------------------------------

requestHandler
  :: forall (m :: Method ClientToServer Request) c .
     SMethod m
  -> (IdeState -> MessageParams m -> ServerM c (Either (TResponseError m) (MessageResult m)))
  -> Handlers (ServerM c)
requestHandler m k = LSP.requestHandler m $ \TRequestMessage{_method,_id,_params} resp -> do
  st <- (ask :: ServerM c ServerState)
  env <- LSP.getLspEnv
  let resp' :: Either (TResponseError m) (MessageResult m) -> LspM c ()
      resp' = flip (\s -> ReaderT.runReaderT s.runServerT) st . resp

  liftIO $ writeChan st.reactor $ ReactorRequest _id (LSP.runLspT env $ resp' =<< runServerM st (k st.ideState _params)) (LSP.runLspT env . resp' . Left)

notificationHandler
  :: forall (m :: Method ClientToServer Notification) c .
     SMethod m
  -> (IdeState -> VFS -> MessageParams m -> ServerM c ())
  -> Handlers (ServerM c)
notificationHandler m k = LSP.notificationHandler m $ \TNotificationMessage{_params,_method} -> do
  st <- ask
  env <- LSP.getLspEnv
  -- Take a snapshot of the VFS state on every notification
  -- We only need to do this here because the VFS state is only updated
  -- on notifications
  vfs <- LSP.getVirtualFiles
  liftIO $ writeChan st.reactor $ ReactorNotification (LSP.runLspT env $ runServerM st $ k st.ideState vfs _params)

-- ----------------------------------------------------------------------------
-- Handlers
-- ----------------------------------------------------------------------------

handlers :: Recorder (WithPriority Log) -> Handlers (ServerM Config)
handlers recorder =
  mconcat
    [ -- We need these notifications handlers to declare that we handle these requests
      notificationHandler SMethod_Initialized $ \ide _ _ -> do
        liftIO $ shakeSessionInit (cmapWithPrio LogShake recorder) ide
    , -- Handling of the virtual file system
      notificationHandler SMethod_TextDocumentDidOpen $ \ide vfs msg -> liftIO $ do
        let
          doc = msg ^. J.textDocument . J.uri
          version = msg ^. J.textDocument . J.version
        atomically $ updatePositionMapping ide (VersionedTextDocumentIdentifier doc version) []
        whenUriFile doc $ \file -> do
            -- We don't know if the file actually exists, or if the contents match those on disk
            -- For example, vscode restores previously unsaved contents on open
            setFileModified (VFSModified vfs) ide file $
              addFileOfInterest ide file Modified{firstOpen=True}
        logWith recorder Debug $ LogOpenedTextDocument doc
    , notificationHandler SMethod_TextDocumentDidChange $ \ide vfs msg -> liftIO $ do
        let
          doc = msg ^. J.textDocument . J.uri
          version = msg ^. J.textDocument . J.version
          changes = msg ^. J.contentChanges
        atomically $ updatePositionMapping ide (VersionedTextDocumentIdentifier doc version) changes
        whenUriFile doc $ \file -> do
            setFileModified (VFSModified vfs) ide file $
              addFileOfInterest ide file Modified{firstOpen=False}
        logWith recorder Debug $ LogModifiedTextDocument doc
    , notificationHandler SMethod_TextDocumentDidSave $ \ide vfs msg -> liftIO $ do
        let
          doc = msg ^. J.textDocument . J.uri
        whenUriFile doc $ \file -> do
            setFileModified (VFSModified vfs) ide file $
              addFileOfInterest ide file OnDisk
        logWith recorder Debug $ LogSavedTextDocument doc
    , notificationHandler SMethod_TextDocumentDidClose $ \ide vfs msg -> liftIO $ do
        let
          doc = msg ^. J.textDocument . J.uri
        whenUriFile doc $ \file -> do
          let herald = "Closed text document: " <> getUri doc
          setSomethingModified (VFSModified vfs) ide (Text.unpack herald) $ do
            scheduleGarbageCollection ide
            deleteFileOfInterest ide file
          logWith recorder Debug $ LogClosedTextDocument doc

    , notificationHandler SMethod_SetTrace $ \_ _ _msg -> do
        pure ()
    , -- Subscribe to notification changes
      notificationHandler SMethod_WorkspaceDidChangeConfiguration mempty
    , requestHandler SMethod_TextDocumentDefinition $ \ide params -> do
        let
          doc :: Uri
          doc = params ^. J.textDocument . J.uri
          pos :: Position
          pos = params ^. J.position
        mloc <- gotoDefinition ide (LSP.toNormalizedUri doc) pos
        case mloc of
          Nothing  -> pure $ Right $ InR $ InR $ Null
          Just loc -> pure $ Right $ InL $ Definition (InL loc)
    , requestHandler SMethod_TextDocumentHover $ \ ide params -> do
        let
          doc :: Uri
          doc = params ^. J.textDocument . J.uri
          pos :: Position
          pos = params ^. J.position
        mh <- findHover ide (LSP.toNormalizedUri doc) pos
        case mh of
          Nothing  -> pure $ Right $ InR $ Null
          Just h -> pure $ Right $ InL $ h
    , requestHandler SMethod_WorkspaceExecuteCommand $ \ide params -> do
        let
          ExecuteCommandParams _ _cid xdata = params
        case xdata of
          Just [uriJson]
            | Aeson.Success (uri :: Uri) <- Aeson.fromJSON uriJson -> do
                let
                  nfp = fromUri $ toNormalizedUri uri

                mProgram <- liftIO $ runAction "semanticTokens.program" ide $ do
                  use GetParsedAst nfp

                case mProgram of
                  Nothing ->
                    pure $
                      Left $
                        TResponseError
                          { _code = InL LSPErrorCodes_RequestFailed
                          , _message = "Failed to typecheck \"" <> Text.pack (show uri) <> "\"."
                          , _xdata = Nothing
                          }
                  Just prog ->
                    pure $ case Ladder.doVisualize prog of
                      Right vizProgramInfo -> Right $ InL $ Aeson.toJSON vizProgramInfo
                      Left vizError -> Left $ 
                        TResponseError
                          { _code = InL LSPErrorCodes_RequestFailed
                          , _message = "Could not visualize\n" <> getUri uri <> "\n" <> Ladder.prettyPrintVizError vizError
                          , _xdata = Nothing }
          _ ->
            pure $ Left $
              TResponseError
                { _code = InL LSPErrorCodes_RequestFailed
                , _message = "Failed to decode request data: " <> LazyText.toStrict (Aeson.encodeToLazyText xdata)
                , _xdata = Nothing
                }
    , requestHandler SMethod_TextDocumentCodeAction $ \ide params -> do
        let
          uri = toNormalizedUri $ params ^. J.textDocument . J.uri
          rng = params ^. J.range
        diags <- liftIO $ atomically $ do
          activeFileDiagnosticsInRange (shakeExtras ide) uri rng
        cas <- Extra.mapMaybeM (outOfScopeAssumeQuickFix ide) diags
        pure $ Right $ InL $ fmap InR cas
    , requestHandler SMethod_TextDocumentSemanticTokensFull $ \ide req -> do
        let
          SemanticTokensParams _ _ doc = req
          uri = doc ^. J.uri
          nfp = fromUri $ toNormalizedUri uri

        tokens <- liftIO $ runAction "semanticTokens" ide $
          use GetRelSemanticTokens nfp
        case tokens of
          Nothing -> do
            pure $
              Left $
                TResponseError
                  { _code = InL LSPErrorCodes_RequestFailed
                  , _message = "Internal error, failed to produce semantic tokens for \"" <> Text.pack (show uri) <> "\""
                  , _xdata = Nothing
                  }

          Just semanticTokensData -> do
            pure $
              Right $
                InL $
                  SemanticTokens
                    { _resultId = Nothing
                    , _data_ = semanticTokensData
                    }
    , requestHandler SMethod_TextDocumentCompletion $ \ide params -> do
        let LSP.TextDocumentIdentifier uri = params ^. J.textDocument
            Position ln col = params ^. J.position
        liftIO (runAction "completions" ide $ getUriContents $ toNormalizedUri uri) >>= \case
          Nothing -> pure (Right (InL []))
          Just rope -> do
            let completionPrefix =
                  Text.takeWhileEnd isAlphaNum
                  $ Rope.toText
                  $ fst -- we don't care for the rest of the line
                  $ Rope.charSplitAt (fromIntegral col) 
                  $ Rope.getLine (fromIntegral ln) rope

            let filterMatchesOn f is = 
                  map Fuzzy.original $ Fuzzy.filter
                    completionPrefix
                    is
                    mempty
                    mempty
                    f
                    False

            let mkKeyWordCompletionItem kw = (defaultCompletionItem kw)
                  { CompletionItem._kind =  Just CompletionItemKind_Keyword
                  }
                keyWordMatches = filterMatchesOn id $ Map.keys keywords 
                -- FUTUREWORK(mangoiv): we could 
                -- 1 pass through the token here 
                -- 2 check the token category and if the category is COperator 
                -- 3 set the CompletionItemKind to CompletionItemKind_Operator
                keywordItems = map mkKeyWordCompletionItem keyWordMatches

            (typeCheck, _positionMapping) <- liftIO $ runAction "typecheck" ide $
              useWithStale_ TypeCheck (fromUri (toNormalizedUri uri))

            let topDeclItems 
                  = filterMatchesOn CompletionItem._label 
                  $ foldMap 
                      (mapMaybe 
                        (\(_, name, checkEntity) -> 
                          topDeclToCompletionItem name 
                          $ Optics.over' 
                            (Optics.gplate @(Type' Resolved)) 
                            (applyFinalSubstitution @(Type' Resolved) typeCheck.substitution) 
                            checkEntity
                        )
                      ) 
                      typeCheck.environment

            -- TODO: maybe we should sort these as follows
            -- 1 keywords 
            -- 2 toplevel values 
            -- 3 toplevel types 

            let items = keywordItems <> topDeclItems

            logWith recorder Debug $ LogRequestedCompletionsFor completionPrefix

            pure (Right (InL items))
    ]

topDeclToCompletionItem :: Name -> CheckEntity -> Maybe CompletionItem
topDeclToCompletionItem name = \case 
  KnownTerm ty term -> 
    Just (defaultTopDeclCompletionItem ty)
      { CompletionItem._kind = Just $ case (term, ty) of
         (Constructor, _) -> CompletionItemKind_Constructor
         (Selector, _) -> CompletionItemKind_Field
         (_, unrollForall -> Fun {}) -> CompletionItemKind_Function
         _ -> CompletionItemKind_Constant
      }
  KnownType kind tydec -> 
    Just (defaultTopDeclCompletionItem (typeFunction kind))
      { CompletionItem._kind = Just $ case tydec of 
          RecordDecl {} -> CompletionItemKind_Struct
          EnumDecl {} -> CompletionItemKind_Enum
      }
  KnownTypeVariable {} -> Nothing
  where 
    -- a function (but also a constant, in theory) can be polymorphic, so we have to strip 
    -- all the foralls to get to the "actual" type.
    unrollForall :: Type' Resolved -> Type' Resolved
    unrollForall (Forall _ _ ty) = unrollForall ty
    unrollForall ty = ty

    -- a list : Type -> Type should be pretty printed as FUNCTION FROM TYPE TO TYPE
    typeFunction :: Kind -> Type' Resolved
    typeFunction 0 = Type emptyAnno
    typeFunction n | n > 0 = Fun emptyAnno (replicate n (MkOptionallyNamedType emptyAnno Nothing (Type emptyAnno))) (Type emptyAnno)
    typeFunction _ = error "Internal error: negative arity of type constructor"

    defaultTopDeclCompletionItem :: Type' Resolved -> CompletionItem
    defaultTopDeclCompletionItem ty = (defaultCompletionItem $ quoteIfNeeded prepared)
      { CompletionItem._filterText = Just prepared
      , CompletionItem._labelDetails 
        = Just CompletionItemLabelDetails
          { _description = Nothing
          , _detail = Just $ " IS A " <> simpleprint ty
          }
      }
      where 
        prepared :: Text
        prepared = case name of MkName _ raw -> rawNameToText raw

defaultCompletionItem :: Text -> CompletionItem
defaultCompletionItem label = CompletionItem label 
  Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing 
  Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing 

whenUriFile :: Uri -> (NormalizedFilePath -> IO ()) -> IO ()
whenUriFile uri act = whenJust (LSP.uriToFilePath uri) $ act . normalizeFilePath

activeFileDiagnosticsInRange :: ShakeExtras -> NormalizedUri -> Range -> STM [FileDiagnostic]
activeFileDiagnosticsInRange extras nfu rng = do
  mDiags <- STM.lookup nfu (publishedDiagnostics extras)
  pure
    [ diag
    | Just diags <- [mDiags]
    , diag <- diags
    , rangesOverlap rng (diag ^. fdLspDiagnosticL . J.range)
    ]

-- ----------------------------------------------------------------------------
-- LSP Go to Definition
-- ----------------------------------------------------------------------------

gotoDefinition :: IdeState -> NormalizedUri -> Position -> ServerM Config (Maybe Location)
gotoDefinition ide fileUri pos = do
  mTypeCheckedModule <- liftIO $ runAction "gotoDefinition" ide $
    useWithStale TypeCheck nfp
  case mTypeCheckedModule of
    Nothing -> pure Nothing
    Just (m, positionMapping) -> do
      pure $ do
        oldPos <- fromCurrentPosition positionMapping pos
        range <- findDefinition (lspPositionToSrcPos oldPos) m.program
        let lspRange = srcRangeToLspRange (Just range)
        newRange <- toCurrentRange positionMapping lspRange
        pure (Location uri newRange)
  where
    nfp = fromUri fileUri
    uri = LSP.fromNormalizedUri fileUri

-- ----------------------------------------------------------------------------
-- LSP Hover
-- ----------------------------------------------------------------------------

findHover :: IdeState -> NormalizedUri -> Position -> ServerM Config (Maybe Hover)
findHover ide fileUri pos = do
  mTypeCheckedModule <- liftIO $ runAction "findHover" ide $
    useWithStale TypeCheck nfp
  case mTypeCheckedModule of
    Nothing -> pure Nothing
    Just (m, positionMapping) -> do
      pure $ do
        oldPos <- fromCurrentPosition positionMapping pos
        (range, t) <- findType (lspPositionToSrcPos oldPos) m.program
        let lspRange = srcRangeToLspRange (Just range)
        newLspRange <- toCurrentRange positionMapping lspRange
        pure (Hover (InL (mkPlainText (simpleprint (applyFinalSubstitution m.substitution t)))) (Just newLspRange))
  where
    nfp = fromUri fileUri

-- ----------------------------------------------------------------------------
-- LSP Code Actions
-- ----------------------------------------------------------------------------

outOfScopeAssumeQuickFix :: IdeState -> FileDiagnostic -> ServerM Config (Maybe CodeAction)
outOfScopeAssumeQuickFix ide fd = case fd ^. messageOfL @CheckErrorWithContext of
  Nothing -> pure Nothing
  Just ctx -> case ctx.kind of
    OutOfScopeError name ty -> do
      mTypeCheck <- liftIO $ runAction "codeAction.outOfScope" ide $ do
        use TypeCheck nfp
      case mTypeCheck of
        Nothing -> pure Nothing
        Just typeCheck -> do
          let
            assumeExpr =
              Assume emptyAnno
                (MkAssume emptyAnno
                  (MkTypeSig emptyAnno (MkGivenSig emptyAnno []) Nothing)
                  (MkAppForm emptyAnno name [])
                  (Just $ fmap getActual ty)
                )

            topDecls =
              Optics.toListOf (Optics.gplate @(TopDecl Resolved)) typeCheck.program

            enclosingTopDecl = do
              target <- rangeOf name
              List.find
                (\decl -> Maybe.isJust $ do
                  r <- rangeOf decl
                  guard (target.start `inRange` r)
                )
                topDecls

          pure $ do
            -- If the type has any inference variable, we don't want to print it
            -- yet. Maybe it in the future, once LSP has snippet support
            -- for code actions.
            -- This LSP feature is promised in 3.18.
            -- At the time of writing, we are designing this code action against 3.17.
            guard (not $ hasTypeInferenceVars ty)
            decl <- enclosingTopDecl
            srcRange <- rangeOf decl

            let
              edit =
                TextEdit
                  { _range = pointRange $ srcPosToLspPosition srcRange.start
                  , _newText =
                      -- Add 2 newlines for better results.
                      -- Ideally, we "graft" this top level onto our
                      -- AST, at the correct location, and then calculate a diff
                      -- based on the old AST and the new one.
                      -- However, currently we are missing a lot of infrastructure
                      -- to make this possible.
                      Text.strip (prettyLayout assumeExpr) <> "\n\n"
                  }

            Just $ CodeAction
              { _title = "Assume `" <> simpleprint name <> "` is defined"
              , _kind = Just CodeActionKind_QuickFix
              , _diagnostics = Just [fd ^. fdLspDiagnosticL]
              , _isPreferred = Nothing
              , _disabled = Nothing
              , _edit = Just WorkspaceEdit
                { _changeAnnotations = Nothing
                , _documentChanges = Nothing
                , _changes = Just $ Map.singleton uri [edit]
                }
              , _command = Nothing
              , _data_ = Nothing
              }
    _ -> pure Nothing
  where
    nfp = fd ^. fdFilePathL

    uri :: Uri
    uri = fromNormalizedUri $ normalizedFilePathToUri nfp

    pointRange pos = Range pos pos

hasTypeInferenceVars :: Type' Resolved -> Bool
hasTypeInferenceVars = \case
  Type   _ -> False
  TyApp  _ _n ns -> any hasTypeInferenceVars ns
  Fun    _ opts ty -> any hasNamedTypeInferenceVars opts || hasTypeInferenceVars ty
  Forall _ _ ty -> hasTypeInferenceVars ty
  InfVar _ _ _ -> True

hasNamedTypeInferenceVars :: OptionallyNamedType Resolved -> Bool
hasNamedTypeInferenceVars = \case
  MkOptionallyNamedType _ _ ty -> hasTypeInferenceVars ty
