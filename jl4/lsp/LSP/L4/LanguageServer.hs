{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module LSP.L4.LanguageServer (
  Log (..),
  Communication (..),
  runLanguageServer,
  setupLSP,
  requestHandler,
  notificationHandler,
  kick,
) where

import Control.Concurrent.STM
import Control.Monad.Extra
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Aeson (ToJSON (toJSON), Value)
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.Text as T
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types
import qualified Language.LSP.Server as LSP
import UnliftIO.Async
import UnliftIO.Concurrent
import UnliftIO.Directory
import UnliftIO.Exception

import qualified Colog.Core as Colog
import Control.Concurrent.Strict (readVar, writeVar)
import Control.Monad.Trans.Cont (evalContT)
import qualified Data.HashMap.Strict as HashMap
import Data.Kind (Type)
import Data.Proxy
import qualified Data.Text as Text
import Development.IDE.Graph
import GHC.TypeLits
import LSP.Core.IdeConfiguration
import LSP.Core.OfInterest hiding (Log (..))
import LSP.Core.ProgressReporting
import LSP.Core.Shake hiding (Log)
import LSP.Core.Types.Options
import LSP.Core.WorkerThread (withWorkerQueue)
import LSP.L4.Handlers hiding (Log)
import LSP.L4.Rules hiding (Log (..))
import LSP.Logger
import Language.LSP.Server (
  LanguageContextEnv,
  LspServerLog,
  type (<~>),
 )
import UnliftIO (MonadUnliftIO)
import Data.ByteString (StrictByteString)
import Data.ByteString.Lazy (LazyByteString)


data Log
  = LogRegisteringIdeConfig !IdeConfiguration
  | LogReactorThreadException !SomeException
  | LogReactorOtherThreadException !SomeException
  | LogReactorMessageActionException !SomeException
  | LogReactorMessageActionException2 !SomeException
  | LogReactorThreadStopped
  | LogCancelledRequest !SomeLspId
  | LogLspServer LspServerLog
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
    LogServerShutdownMessage -> "Received shutdown message"
    LogReactorOtherThreadException e ->
      vcat
        [ "LogReactorOtherThreadException"
        , pretty $ displayException e ]

-- ----------------------------------------------------------------------------
-- Running the Language Server
-- ----------------------------------------------------------------------------

data Communication
  = Communication
  { inwards :: IO StrictByteString
  , outwards :: LazyByteString -> IO ()
  }

runLanguageServer
    :: forall config (m :: Type -> Type -> Type) a. (Show config)
    => Recorder (WithPriority Log)
    -> LSP.Options
    -> Communication
    -> config
    -> (config -> Value -> Either T.Text config)
    -> (config -> m config ())
    -> (MVar ()
        -> IO (LSP.LanguageContextEnv config -> TRequestMessage Method_Initialize -> IO (Either (TResponseError Method_Initialize) (LSP.LanguageContextEnv config, a)),
               LSP.Handlers (m config),
               (LanguageContextEnv config, a) -> m config <~> IO))
    -> IO ()
runLanguageServer recorder options comm defaultConfig parseConfig onConfigChange setup = do
    -- This MVar becomes full when the server thread exits or we receive exit message from client.
    -- LSP server will be canceled when it's full.
    clientMsgVar <- newEmptyMVar

    (doInitialize, staticHandlers, interpretHandler) <- setup clientMsgVar

    let serverDefinition = LSP.ServerDefinition
            { LSP.parseConfig = parseConfig
            , LSP.onConfigChange = onConfigChange
            , LSP.defaultConfig = defaultConfig
            -- TODO: magic string
            , LSP.configSection = "jl4"
            , LSP.doInitialize = doInitialize
            , LSP.staticHandlers = const staticHandlers
            , LSP.interpretHandler = interpretHandler
            , LSP.options = modifyOptions options
            }

    let lspCologAction :: MonadIO m2 => Colog.LogAction m2 (Colog.WithSeverity LspServerLog)
        lspCologAction = toCologActionWithPrio (cmapWithPrio LogLspServer recorder)

    -- TODO: use runServerWith instead of runServerWithHandles
    void $ untilMVar clientMsgVar $
          void $ LSP.runServerWith
            lspCologAction
            lspCologAction
            comm.inwards
            comm.outwards
            serverDefinition

setupLSP ::
     forall config err.
     Recorder (WithPriority Log)
  -> FilePath -- ^ root directory, see Note [Root Directory]
  -> LSP.Handlers (ServerM config)
  -> (LSP.LanguageContextEnv config -> FilePath -> ThreadQueue -> IO IdeState)
  -> MVar ()
  -> IO (LSP.LanguageContextEnv config -> TRequestMessage Method_Initialize -> IO (Either err (LSP.LanguageContextEnv config, IdeState)),
         LSP.Handlers (ServerM config),
         (LanguageContextEnv config, IdeState) -> ServerM config <~> IO)
setupLSP recorder defaultRoot userHandlers getIdeState clientMsgVar = do
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
          (\(s :: ServerM config a) -> LSP.runLspT env $ runReaderT s.runServerT (ServerState clientMsgChan st))
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
                    k $ TResponseError (InR ErrorCodes_InternalError) (T.pack (show e)) Nothing
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


-- | Typecheck all the files of interest.
kick :: Action ()
kick = do
    files <- HashMap.keys <$> getFilesOfInterestUntracked
    ShakeExtras{ideTesting = IdeTesting testing, lspSink, progress} <- getShakeExtras
    let signal :: KnownSymbol s => Proxy s -> Action ()
        signal msg = when testing $ liftIO $
            case lspSink of
              Nothing -> pure ()
              Just sink ->
                sendNotificationToClient sink (SMethod_CustomMethod msg) $ toJSON $ map fromNormalizedUri files

    signal (Proxy @"kick/start")
    liftIO $ progressUpdate progress ProgressNewStarted
    -- Run rules that produce diagnostics
    void $ uses GetLexTokens files
        <* uses GetParsedAst files
        <* uses TypeCheck files
        <* uses Evaluate files
    liftIO $ progressUpdate progress ProgressCompleted

    GarbageCollectVar var <- getIdeGlobalAction
    garbageCollectionScheduled <- liftIO $ readVar var
    when garbageCollectionScheduled $ do
        void garbageCollectDirtyKeys
        liftIO $ writeVar var False

    signal (Proxy @"kick/done")
