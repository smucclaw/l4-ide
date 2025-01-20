{-# LANGUAGE DataKinds #-}

module LSP.L4.Handlers where

import Control.Concurrent.STM
import Control.Concurrent.Strict (Chan, writeChan)
import Control.Exception.Safe (MonadCatch, MonadMask, MonadThrow)
import Control.Lens ((^.))
import Control.Monad.Extra (whenJust)
import Control.Monad.IO.Class
import Control.Monad.Reader (MonadReader (..))
import Control.Monad.Trans.Reader (ReaderT)
import qualified Control.Monad.Trans.Reader as ReaderT
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Text as Aeson
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LazyText
import L4.TypeCheck
import LSP.Core.FileStore hiding (Log (..))
import qualified LSP.Core.FileStore as FileStore
import LSP.Core.OfInterest hiding (Log (..))
import LSP.Core.Service hiding (Log (..))
import LSP.Core.Shake hiding (Log(..))
import qualified LSP.Core.Shake as Shake
import LSP.Core.Types.Location
import LSP.L4.Config
import qualified LSP.L4.Ladder as Ladder
import LSP.L4.Rules hiding (Log (..))
import LSP.Logger
import qualified Language.LSP.Protocol.Lens as J
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types
import qualified Language.LSP.Protocol.Types as LSP
import Language.LSP.Server hiding (notificationHandler, requestHandler)
import qualified Language.LSP.Server as LSP
import Language.LSP.VFS (VFS)
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
    , requestHandler SMethod_WorkspaceExecuteCommand $ \ide req -> do
        let
          ExecuteCommandParams _ _cid xdata = req
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

-- ----------------------------------------------------------------------------
-- LSP Go to Definition
-- ----------------------------------------------------------------------------

gotoDefinition :: IdeState -> NormalizedUri -> Position -> ServerM Config (Maybe Location)
gotoDefinition ide fileUri pos = do
  mTypeCheckedModule <- liftIO $ runAction "gotoDefinition" ide $
    use TypeCheck nfp
  case mTypeCheckedModule of
    Nothing -> pure Nothing
    Just m -> do
      pure $ do
        range <- findDefinition (lspPositionToSrcPos pos) m.program
        pure (Location uri (srcRangeToLspRange (Just range)))
  where
    nfp = fromUri fileUri
    uri = LSP.fromNormalizedUri fileUri

-- ----------------------------------------------------------------------------
-- LSP Hover
-- ----------------------------------------------------------------------------

findHover :: IdeState -> NormalizedUri -> Position -> ServerM Config (Maybe Hover)
findHover ide fileUri pos = do
  mTypeCheckedModule <- liftIO $ runAction "findHover" ide $
    use TypeCheck nfp
  case mTypeCheckedModule of
    Nothing -> pure Nothing
    Just m -> do
      pure $ do
        (range, t) <- findType (lspPositionToSrcPos pos) m.program
        pure (Hover (InL (mkPlainText (simpleprint (applyFinalSubstitution m.substitution t)))) (Just (srcRangeToLspRange (Just range))))
  where
    nfp = fromUri fileUri
