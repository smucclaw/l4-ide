{-# LANGUAGE DataKinds #-}

module LSP.L4.Handlers (
  Log(..),
  ServerM(..),
  ServerState(..),
  ReactorMessage(..),
  handlers,
  requestHandler,
  notificationHandler,
) where

import LSP.L4.Base
import LSP.L4.Config
import qualified LSP.L4.Ladder as Ladder
import LSP.L4.Rules hiding (Log (..))
import LSP.Logger

import Control.Concurrent.STM
import Control.Concurrent.Strict (Chan, writeChan)
import Control.Exception.Safe (MonadCatch, MonadMask, MonadThrow)
import Control.Lens ((^.), Identity (runIdentity))
import Control.Monad.Extra (guard, whenJust)
import qualified Control.Monad.Extra as Extra
import Control.Monad.IO.Class
import Control.Monad.Reader (MonadReader (..))
import Control.Monad.Trans.Reader (ReaderT)
import qualified Control.Monad.Trans.Reader as ReaderT
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Text as Aeson
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LazyText
import qualified Data.List.Extra as Extra
import LSP.Core.FileStore hiding (Log (..))
import qualified LSP.Core.FileStore as FileStore
import LSP.Core.OfInterest hiding (Log (..))
import LSP.Core.PositionMapping
import LSP.Core.Service hiding (Log (..))
import LSP.Core.Shake hiding (Log (..))
import qualified LSP.Core.Shake as Shake
import LSP.Core.Types.Diagnostics
import LSP.Core.Types.Location
import qualified Language.LSP.Protocol.Lens as J
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types
import qualified Language.LSP.Protocol.Types as LSP
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
  deriving newtype (Functor, Applicative, Monad, MonadReader ServerState)
  deriving newtype (MonadLsp c, MonadCatch, MonadIO, MonadMask, MonadThrow, MonadUnliftIO)

-- TODO: can these instances be derived via?
instance (Semigroup a) => Semigroup (ServerM c a) where
  a <> b = liftA2 (<>) a b
--
instance (Monoid a) => Monoid (ServerM c a) where
  mempty = ServerM $ pure mempty

runServerM :: ServerState -> ServerM c a -> LspM c a
runServerM st m = ReaderT.runReaderT m.runServerT st


data Log
  = LogOpenedTextDocument !Uri
  | LogModifiedTextDocument !Uri
  | LogSavedTextDocument !Uri
  | LogClosedTextDocument !Uri
  | LogFileStore FileStore.Log
  | LogShake Shake.Log
  deriving (Show)

instance Pretty Log where
  pretty = \case
    LogOpenedTextDocument uri ->  "Opened text document:" <+> pretty (getUri uri)
    LogModifiedTextDocument uri -> "Modified text document:" <+> pretty (getUri uri)
    LogSavedTextDocument uri -> "Saved text document:" <+> pretty (getUri uri)
    LogClosedTextDocument uri -> "Closed text document:" <+> pretty (getUri uri)
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
                    pure $ Right $ InL $ Aeson.toJSON $ Ladder.visualise prog
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
        cas <- Extra.concatMapM (outOfScopeQuickFix ide) diags
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
    ]

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

outOfScopeQuickFix :: IdeState -> FileDiagnostic -> ServerM Config  [CodeAction]
outOfScopeQuickFix ide fd = case fd ^. messageOfL @CheckErrorWithContext of
  Nothing -> pure []
  Just ctx -> case ctx.kind of
    OutOfScopeError name ty -> do
      mTypeCheck <- liftIO $ runAction "codeAction.outOfScope" ide $ do
        use TypeCheck nfp
      mParseMod <- liftIO $ runAction "codeAction.outOfScope" ide $ do
        use GetParsedAst nfp
      case liftA2 (,) mTypeCheck mParseMod of
        Nothing -> pure []
        Just (typeCheck, parsedMod) -> do
          pure $ concat
            [ assumeVariableCodeAction fd typeCheck name ty
            , addAsParameterCodeAction fd parsedMod name ty
            ]

    _ -> pure []
  where
    nfp = fd ^. fdFilePathL

pointRange :: Position -> Range
pointRange pos = Range pos pos

assumeVariableCodeAction :: FileDiagnostic -> TypeCheckResult -> Name -> Type' Resolved -> [CodeAction]
assumeVariableCodeAction fd typeCheck name ty = do
  let
    -- assumeExpr =
    --   Assume emptyAnno
    --     (MkAssume emptyAnno (MkAppForm emptyAnno name []) (fmap getActual ty))

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

  Maybe.maybeToList $ do
    decl <- enclosingTopDecl
    srcRange <- rangeOf decl

    let
      edit =
        TextEdit
          { _range = pointRange $ srcPosToLspPosition srcRange.start
          , _newText =
              -- simpleprint assumeExpr
              "ASSUME " <> simpleprint name <> " IS A " <> simpleprint ty <> "\n\n"
          }

    Just CodeAction
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
  where
    nfp = fd ^. fdFilePathL

    uri :: Uri
    uri = fromNormalizedUri $ normalizedFilePathToUri nfp

addAsParameterCodeAction :: FileDiagnostic -> Program Name -> Name -> Type' Resolved -> [CodeAction]
addAsParameterCodeAction fd program name ty = do
  let
    topDecls =
      Optics.toListOf (Optics.gplate @(TopDecl Name)) program

    enclosingDecide :: Maybe (Decide Name)
    enclosingDecide = do
      target <- rangeOf name
      Extra.firstJust
        (\decl -> do
          r <- rangeOf decl
          guard (target.start `inRange` r)
          snd <$> Optics.preview #_Decide decl
        )
        topDecls

  Maybe.maybeToList $ do
    decide <- enclosingDecide
    _newDecide <- addNameToDeclareAsParameter decide name ty

    Just CodeAction
      { _title = "Add `" <> simpleprint name <> "` as a parameter"
      , _kind = Just CodeActionKind_QuickFix
      , _diagnostics = Just [fd ^. fdLspDiagnosticL]
      , _isPreferred = Nothing
      , _disabled = Nothing
      , _edit = Just WorkspaceEdit
        { _changeAnnotations = Nothing
        , _documentChanges = Nothing
        , _changes = Just $ Map.singleton uri []
        }
      , _command = Nothing
      , _data_ = Nothing
      }
  where
    nfp = fd ^. fdFilePathL

    uri :: Uri
    uri = fromNormalizedUri $ normalizedFilePathToUri nfp

addNameToDeclareAsParameter :: Decide Name -> Name -> Type' Resolved -> Maybe (Decide Name)
addNameToDeclareAsParameter (MkDecide dAnn typeSig appForm expr) name ty = do
  let
    cleanName = name

    MkAppForm aAnn fName fArgs = appForm

    MkTypeSig tAnn givenSig givethSig = typeSig

    MkGivenSig gAnn opts = givenSig

    newParam = mkOptionallyTypedName cleanName ty

    newGivenSig = MkGivenSig gAnn (opts <> [newParam])

    newTypeSig = MkTypeSig tAnn newGivenSig givethSig

    newAppForm = MkAppForm aAnn fName (fArgs <> [cleanName])

  Just (MkDecide dAnn newTypeSig newAppForm expr)

mkOptionallyTypedName :: Name -> Type' Resolved -> OptionallyTypedName Name
mkOptionallyTypedName name ty = runIdentity $ attachAnno $
  MkOptionallyTypedName emptyAnno
    <$> annoHole (pure name)
    -- TODO: this is not great
    <*  annoLexemes (pure $ fromTokenType [TKIs, TSpace " ", TKA, TSpace " "])
    <*> annoHole (pure $ Just $ fmap getActual ty)

fromTokenType :: [TokenType] -> Lexeme_ PosToken [PosToken]
fromTokenType ps = mkLexeme $ fmap trivialToken ps
