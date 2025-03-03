{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module LSP.LanguageServer (
  Log (..),
  runLanguageServer,
  setupLSP,
  OnSetup(..),
  ServerM(..),
  requestHandler,
  notificationHandler,
  defHandlers,
) where

import qualified Colog.Core as Colog
import Control.Concurrent.STM
import Control.Exception.Safe (MonadMask, MonadThrow, MonadCatch)
import Control.Monad.Extra
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Trans.Cont (evalContT)
import Data.Aeson (Value)
import Data.Kind (Type)
import Data.Maybe
import Data.Monoid (Ap(..))
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text as Text
import LSP.Core.IdeConfiguration
import LSP.Core.Shake hiding (Log)
import LSP.Core.WorkerThread (withWorkerQueue)
import LSP.Logger
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types
import Language.LSP.Server hiding (requestHandler, notificationHandler)
import qualified Language.LSP.Server as LSP
import Language.LSP.VFS
import System.IO
import UnliftIO (MonadUnliftIO (..))
import UnliftIO.Async
import UnliftIO.Concurrent
import UnliftIO.Directory
import UnliftIO.Exception
import qualified Control.Monad.Trans.Reader as ReaderT
import LSP.Core.OfInterest hiding (Log(..))
import LSP.Core.Types.Location
import qualified LSP.Core.Shake as Shake
import qualified Language.LSP.Protocol.Lens as J
import qualified Language.LSP.Protocol.Types as LSP
import Control.Lens
import LSP.Core.FileStore hiding (Log, LogShake)

data Log
  = LogRegisteringIdeConfig !IdeConfiguration
  | LogReactorThreadException !SomeException
  | LogReactorOtherThreadException !SomeException
  | LogReactorMessageActionException !SomeException
  | LogReactorMessageActionException2 !SomeException
  | LogReactorThreadStopped
  | LogCancelledRequest !SomeLspId
  | LogLspServer LspServerLog

  | LogOpenedTextDocument !Uri
  | LogModifiedTextDocument !Uri
  | LogSavedTextDocument !Uri
  | LogClosedTextDocument !Uri
  | LogShake Shake.Log
  | LogServerShutdownMessage
  deriving Show

instance Pretty Log where
  pretty = \case
    LogRegisteringIdeConfig ideConfig ->
      -- This log is also used to identify if HLS starts successfully in vscode-haskell,
      -- don't forget to update the corresponding test in vscode-haskell if the text in
      -- the next line has been modified.
      "Registering IDE configuration:" <+> viaShow ideConfig
    LogReactorThreadException e ->
      vcat
        [ "ReactorThreadException"
        , pretty $ displayException e ]
    LogReactorMessageActionException e ->
      vcat
        [ "ReactorMessageActionException"
        , pretty $ displayException e ]
    LogReactorMessageActionException2 e ->
      vcat
        [ "ReactorMessageActionException2"
        , pretty $ displayException e ]
    LogReactorThreadStopped ->
      "Reactor thread stopped"
    LogCancelledRequest requestId ->
      "Cancelled request" <+> viaShow requestId
    LogLspServer msg -> pretty msg
    LogOpenedTextDocument uri ->  "Opened text document:" <+> pretty (getUri uri)
    LogModifiedTextDocument uri -> "Modified text document:" <+> pretty (getUri uri)
    LogSavedTextDocument uri -> "Saved text document:" <+> pretty (getUri uri)
    LogClosedTextDocument uri -> "Closed text document:" <+> pretty (getUri uri)
    LogShake msg -> pretty msg
    LogServerShutdownMessage -> "Received shutdown message"
    LogReactorOtherThreadException e ->
      vcat
        [ "LogReactorOtherThreadException"
        , pretty $ displayException e ]

-- ----------------------------------------------------------------------------
-- Running the Language Server
-- ----------------------------------------------------------------------------

data OnSetup m config
  = OnSetup
      (MVar () ->
        IO
          ( LSP.LanguageContextEnv config -> TRequestMessage Method_Initialize -> IO (Either (TResponseError Method_Initialize) (LSP.LanguageContextEnv config, IdeState))
          , LSP.Handlers (m config)
          , (LanguageContextEnv config, IdeState) -> m config <~> IO
          )
      )

runLanguageServer
    :: forall config (m :: Type -> Type -> Type) . (Show config)
    => Recorder (WithPriority Log)
    -> LSP.Options
    -> Handle -- input
    -> Handle -- output
    -> config
    -> (config -> Value -> Either T.Text config)
    -> (config -> m config ())
    -> Text -- ^ Config section name
    -> OnSetup m config
    -> IO ()
runLanguageServer recorder options inH outH defaultConfig parseConfig onConfigChange configSection (OnSetup setup) = do
    -- This MVar becomes full when the server thread exits or we receive exit message from client.
    -- LSP server will be canceled when it's full.
    clientMsgVar <- newEmptyMVar

    (doInitialize, staticHandlers, interpretHandler) <- setup clientMsgVar

    let serverDefinition = LSP.ServerDefinition
            { LSP.parseConfig = parseConfig
            , LSP.onConfigChange = onConfigChange
            , LSP.defaultConfig = defaultConfig
            , LSP.configSection = configSection
            , LSP.doInitialize = doInitialize
            , LSP.staticHandlers = const staticHandlers
            , LSP.interpretHandler = interpretHandler
            , LSP.options = modifyOptions options
            }

    let lspCologAction :: MonadIO m2 => Colog.LogAction m2 (Colog.WithSeverity LspServerLog)
        lspCologAction = toCologActionWithPrio (cmapWithPrio LogLspServer recorder)

    void $ untilMVar clientMsgVar $
          void $ LSP.runServerWithHandles
            lspCologAction
            lspCologAction
            inH
            outH
            serverDefinition

setupLSP ::
     forall config .
     Recorder (WithPriority Log)
  -> FilePath -- ^ root directory, see Note [Root Directory]
  -> LSP.Handlers (ServerM config)
  -> (LSP.LanguageContextEnv config -> FilePath -> ThreadQueue -> IO IdeState)
  -> OnSetup ServerM config
  -- MVar ()
  -- -> IO (LSP.LanguageContextEnv config -> TRequestMessage Method_Initialize -> IO (Either (TResponseError Method_Initialize) (LSP.LanguageContextEnv config, IdeState)),
  --        LSP.Handlers (ServerM config),
  --        (LanguageContextEnv config, IdeState) -> ServerM config <~> IO)
setupLSP recorder defaultRoot userHandlers getIdeState = OnSetup $ \clientMsgVar -> do
  -- Send everything over a channel, since you need to wait until after initialise before
  -- LspFuncs is available
  clientMsgChan :: Chan ReactorMessage <- newChan

  -- An MVar to control the lifetime of the reactor loop.
  -- The loop will be stopped and resources freed when it's full
  reactorLifetime <- newEmptyMVar
  let stopReactorLoop = void $ tryPutMVar reactorLifetime ()

  -- Forcefully exit
  let exit = void $ tryPutMVar clientMsgVar ()

  -- The set of requests ids that we have received but not finished processing
  pendingRequests <- newTVarIO Set.empty
  -- The set of requests that have been cancelled and are also in pendingRequests
  cancelledRequests <- newTVarIO Set.empty

  let cancelRequest reqId = atomically $ do
          queued <- readTVar pendingRequests
          -- We want to avoid that the list of cancelled requests
          -- keeps growing if we receive cancellations for requests
          -- that do not exist or have already been processed.
          when (reqId `Set.member` queued) $
              modifyTVar cancelledRequests (Set.insert reqId)
  let clearReqId reqId = atomically $ do
          modifyTVar pendingRequests (Set.delete reqId)
          modifyTVar cancelledRequests (Set.delete reqId)
      -- We implement request cancellation by racing waitForCancel against
      -- the actual request handler.
  let waitForCancel reqId = atomically $ do
          cancelled <- readTVar cancelledRequests
          unless (reqId `Set.member` cancelled) retry

  let asyncHandlers = mconcat
        [ userHandlers
        , cancelHandler cancelRequest
        , exitHandler exit
        , shutdownHandler recorder stopReactorLoop
        ]
        -- Cancel requests are special since they need to be handled
        -- out of order to be useful. Existing handlers are run afterwards.

  let doInitialize = handleInit recorder defaultRoot getIdeState reactorLifetime exit clearReqId waitForCancel clientMsgChan

  let interpretHandler (env,  st) =
        LSP.Iso
          ((\(s :: ServerM config a) -> LSP.runLspT env $ runReaderT (s.runServerT) (ServerState clientMsgChan st)))
          liftIO

  pure (doInitialize, asyncHandlers, interpretHandler)


handleInit
    :: Recorder (WithPriority Log)
    -> FilePath -- ^ root directory, see Note [Root Directory]
    -> (LSP.LanguageContextEnv config -> FilePath -> ThreadQueue -> IO IdeState)
    -> MVar ()
    -> IO ()
    -> (SomeLspId -> IO ())
    -> (SomeLspId -> IO ())
    -> Chan ReactorMessage
    -> LSP.LanguageContextEnv config -> TRequestMessage Method_Initialize -> IO (Either err (LSP.LanguageContextEnv config, IdeState))
handleInit recorder defaultRoot getIdeState lifetime exitClientMsg clearReqId waitForCancel clientMsgChan env (TRequestMessage _ _ _ params) = do
    -- only shift if lsp root is different from the rootDir
    -- see Note [Root Directory]
    root <- case LSP.resRootPath env of
        Just lspRoot | lspRoot /= defaultRoot -> setCurrentDirectory lspRoot >> return lspRoot
        _ -> pure defaultRoot
    let initConfig = parseConfiguration params
    logWith recorder Info $ LogRegisteringIdeConfig initConfig
    threadQueueMVar <- newEmptyMVar
    let handleServerException (Left e) = do
            logWith recorder Error $ LogReactorThreadException e
            exitClientMsg
        handleServerException (Right _) = pure ()

        exceptionInHandler e = do
            logWith recorder Error $ LogReactorMessageActionException e

        exceptionInHandler2 e = do
            logWith recorder Error $ LogReactorMessageActionException2 e

        checkCancelled :: forall m . LspId m -> IO () -> (TResponseError m -> IO ()) -> IO ()
        checkCancelled _id act k =
            let sid = SomeLspId _id
            in flip finally (clearReqId sid) $
                catch (do
                    -- We could optimize this by first checking if the id
                    -- is in the cancelled set. However, this is unlikely to be a
                    -- bottleneck and the additional check might hide
                    -- issues with async exceptions that need to be fixed.
                    cancelOrRes <- race (waitForCancel sid) act
                    case cancelOrRes of
                        Left () -> do
                            logWith recorder Debug $ LogCancelledRequest sid
                            k $ TResponseError (InL LSPErrorCodes_RequestCancelled) "" Nothing
                        Right res -> pure res
                ) $ \(e :: SomeException) -> do
                    exceptionInHandler e
                    k $ TResponseError (InR ErrorCodes_InternalError) (T.pack $ show e) Nothing
    _ <- flip forkFinally handleServerException $ do
        untilMVar lifetime $ runWithWorkerThreads $ \threadQueue -> do
            putMVar threadQueueMVar threadQueue
            forever $ do
                msg <- readChan clientMsgChan
                -- We dispatch notifications synchronously and requests asynchronously
                -- This is to ensure that all file edits and config changes are applied before a request is handled
                case msg of
                    ReactorNotification act -> handle exceptionInHandler2 act
                    ReactorRequest _id act k -> void $ async $ checkCancelled _id act k
        logWith recorder Info LogReactorThreadStopped
    threadQueue <- takeMVar threadQueueMVar
    ide <- getIdeState env root threadQueue
    registerIdeConfiguration (shakeExtras ide) initConfig
    pure $ Right (env,ide)


-- | runWithWorkerThreads
-- create several threads to run the session, db and session loader
-- see Note [Serializing runs in separate thread]
runWithWorkerThreads :: (ThreadQueue -> IO ()) -> IO ()
runWithWorkerThreads f = evalContT $ do
  sessionRestartTQueue <- withWorkerQueue id
  sessionLoaderTQueue <- withWorkerQueue id
  threadQueue <- withWorkerQueue id
  liftIO $ f (ThreadQueue threadQueue sessionRestartTQueue sessionLoaderTQueue)

-- | Runs the action until it ends or until the given MVar is put.
--   Rethrows any exceptions.
untilMVar :: MonadUnliftIO m => MVar () -> m () -> m ()
untilMVar mvar io = void $
    waitAnyCancel =<< traverse async [ io , readMVar mvar ]

cancelHandler :: (SomeLspId -> IO ()) -> LSP.Handlers (ServerM c)
cancelHandler cancelRequest = LSP.notificationHandler SMethod_CancelRequest $ \TNotificationMessage{_params=CancelParams{_id}} ->
  liftIO $ cancelRequest (SomeLspId (toLspId _id))
  where toLspId :: (Int32 |? Text.Text) -> LspId a
        toLspId (InL x) = IdInt x
        toLspId (InR y) = IdString y

shutdownHandler :: Recorder (WithPriority Log) -> IO () -> LSP.Handlers (ServerM c)
shutdownHandler recorder stopReactor = LSP.requestHandler SMethod_Shutdown $ \_ resp -> do
    ide <- asks (.ideState)
    liftIO $ logWith recorder Debug LogServerShutdownMessage
    -- stop the reactor to free up the hiedb connection
    liftIO stopReactor
    -- flush out the Shake session to record a Shake profile if applicable
    liftIO $ shakeShut ide
    resp $ Right Null

exitHandler ::IO () -> LSP.Handlers (ServerM c)
exitHandler exit = LSP.notificationHandler SMethod_Exit $ const $ liftIO exit

modifyOptions :: LSP.Options -> LSP.Options
modifyOptions x =
  x { LSP.optTextDocumentSync   = Just $ tweakTDS origTDS
    }
    where
        tweakTDS tds = tds
          { _openClose = Just True
          , _change = Just TextDocumentSyncKind_Incremental
          , _save = Just $ InR $ SaveOptions Nothing
          }
        origTDS = fromMaybe tdsDefault $ LSP.optTextDocumentSync x
        tdsDefault = TextDocumentSyncOptions Nothing Nothing Nothing Nothing Nothing

-- ----------------------------------------------------------------------------
-- ServerM
-- ----------------------------------------------------------------------------

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


defHandlers :: Recorder (WithPriority Log) -> Handlers (ServerM config)
defHandlers recorder = mconcat
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
  ]

whenUriFile :: Uri -> (NormalizedFilePath -> IO ()) -> IO ()
whenUriFile uri act = whenJust (LSP.uriToFilePath uri) $ act . normalizeFilePath

