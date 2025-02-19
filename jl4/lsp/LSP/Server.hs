{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module LSP.Server (
  serverMain,
  ServerConfig (..),
  Arguments (..),
  lspOptions,
  getDefaultArguments,
  defOnConfigChange,
  kickRules,
  Log,
) where

import LSP.Core.Debouncer
import LSP.Core.FileStore hiding (Log (..))
import LSP.Core.Service hiding (Log (..))
import qualified LSP.Core.Service as Service hiding (LogShake)
import qualified LSP.Core.Shake as Shake
import LSP.Core.Types.Monitoring
import LSP.Core.Types.Options
import LSP.Logger

import Control.Concurrent.Strict
import Control.Monad (unless)
import Control.Monad.Extra (when)
import Control.Monad.IO.Class
import qualified Data.Aeson as Aeson
import qualified Data.Aeson as J
import Data.Data
import Data.Functor (void)
import qualified Data.HashMap.Strict as HashMap
import Data.Text (Text)
import qualified Data.Text as Text
import Development.IDE.Graph (Action, Rules, action)
import GHC.Conc (getNumProcessors)
import GHC.IO.Encoding
import GHC.TypeLits
import LSP.Core.IdeConfiguration
import LSP.Core.OfInterest hiding (Log (..))
import LSP.Core.ProgressReporting
import qualified LSP.Core.RuleTypes as Rules
import LSP.Core.Shake hiding (Log (..))
import LSP.Core.Types.Shake
import LSP.LanguageServer (runLanguageServer)
import qualified LSP.LanguageServer as LanguageServer
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types hiding (Pattern)
import qualified Language.LSP.Protocol.Types as LSP
import Language.LSP.Server
import qualified Language.LSP.Server as LSP
import System.Directory (getCurrentDirectory)
import System.IO
import System.Time.Extra
import UnliftIO (withRunInIO)

-- ----------------------------------------------------------------------------

-- These settings are important!
syncOptions :: LSP.TextDocumentSyncOptions
syncOptions =
  LSP.TextDocumentSyncOptions
    { LSP._openClose = Just True
    , LSP._change = Just LSP.TextDocumentSyncKind_Incremental
    , LSP._willSave = Just False
    , LSP._willSaveWaitUntil = Just False
    , LSP._save = Just $ LSP.InR $ LSP.SaveOptions $ Just False
    }

lspOptions :: Options
lspOptions =
  defaultOptions
    { optTextDocumentSync = Just syncOptions
    }

-- ----------------------------------------------------------------------------
-- Complicated Init Stuff
-- ----------------------------------------------------------------------------

data Log
  = LogLspStart
  | LogLspStartDuration !Seconds
  | LogShouldRunSubset !Bool
  | LogService Service.Log
  | LogShake Shake.Log
  | LogLanguageServer LanguageServer.Log
  | LogConfigurationChange Text
  deriving (Show)

instance Pretty Log where
  pretty = \case
    LogLspStart ->
      nest 2 $ vsep
        [ "Starting LSP server..."
        ]
    LogLspStartDuration ds ->
      "Started LSP server in" <+> pretty (showDuration ds)
    LogShouldRunSubset shouldRunSubset ->
      "shouldRunSubset:" <+> pretty shouldRunSubset
    LogService msg -> pretty msg
    LogShake msg -> pretty msg
    LogLanguageServer msg -> pretty msg
    LogConfigurationChange msg -> "Configuration changed:" <+> pretty msg

data Arguments = Arguments
  { projectRoot :: FilePath
  , rules :: Rules ()
  , lspOptions :: LSP.Options
  , debouncer :: IO (Debouncer NormalizedUri)
  -- ^ Debouncer used for diagnostics
  , handleIn :: IO Handle
  , handleOut :: IO Handle
  , threads :: Maybe Natural
  , monitoring :: IO Monitoring
  , disableKick :: Bool
  -- ^ flag to disable kick used for testing
  }

getDefaultArguments :: IO Arguments
getDefaultArguments = do
  cwd <- getCurrentDirectory
  pure Arguments
    { projectRoot = cwd
    , rules = pure ()
    , lspOptions = lspOptions
    , debouncer = newAsyncDebouncer
    , handleIn = pure stdin
    , handleOut = pure stdout
    , threads = Nothing
    , monitoring = mempty
    , disableKick = False
    }

data ServerConfig m config = ServerConfig
  { config :: config
  , parseServerConfig :: config -> Aeson.Value -> Either Text config
  , onConfigChange :: MVar IdeState -> config -> m config ()
  , handlers :: Handlers (m config)
  , rules :: Rules ()
  , kick :: Action ()
  , setupLsp :: FilePath -> (LanguageContextEnv config -> FilePath -> Shake.ThreadQueue -> IO IdeState) -> LanguageServer.OnSetup m config
  , keywords :: [Text]
  , configSection :: Text
  , lspOptions :: Options
  }

serverMain :: forall m config . Show config => Recorder (WithPriority Log) -> ServerConfig m config -> Arguments -> IO ()
serverMain recorder serverConf args = do
  setLocaleEncoding utf8
  hSetBuffering stderr LineBuffering

  let
    options = serverConf.lspOptions
    rules = do
      args.rules
      serverConf.rules
      unless args.disableKick $ action serverConf.kick

  debouncer <- args.debouncer
  inH <- args.handleIn
  outH <- args.handleOut

  numProcessors <- getNumProcessors
  let
    numCapabilities = max 1 $ maybe (numProcessors `div` 2) fromIntegral args.threads

  withNumCapabilities numCapabilities $ do
    ioT <- offsetTime
    logWith recorder Info $ LogLspStart

    ideStateVar <- newEmptyMVar
    let
      getIdeState :: LSP.LanguageContextEnv config -> FilePath -> Shake.ThreadQueue -> IO IdeState
      getIdeState env rootPath threadQueue = do
        t <- ioT
        logWith recorder Info $ LogLspStartDuration t
        clientCaps <- LSP.runLspT env LSP.getClientCapabilities

        let ideOpts = defaultIdeOptions
              { optKeywords = serverConf.keywords
              }

        -- disable runSubset if the client doesn't support watched files
        let runSubset = optRunSubset ideOpts && isWatchSupported clientCaps
        logWith recorder Debug $ LogShouldRunSubset runSubset

        let
          ideOptions =
            ideOpts
              { optReportProgress = clientSupportsProgress caps
              , optRunSubset = runSubset
              }
          caps = LSP.resClientCapabilities env
        monitoring <- args.monitoring
        ide <-
          initialise
            (cmapWithPrio LogService recorder)
            rules
            (Just $ lspSinkFromContext env)
            (Just caps)
            debouncer
            ideOptions
            threadQueue
            monitoring
            rootPath
        putMVar ideStateVar ide
        pure ide

    let
      setup :: LanguageServer.OnSetup m config
      setup = serverConf.setupLsp
          args.projectRoot
          getIdeState

    runLanguageServer
      (cmapWithPrio LogLanguageServer recorder) -- Logger for LanguageServer lifecycles
      options                                   -- LSP options
      inH                                       -- Handler for reading LSP messages
      outH                                      -- Handler for writing LSP messages
      serverConf.config                         -- Language Server specific Configuration
      serverConf.parseServerConfig              -- How to parse the LS specific Configuration
      (serverConf.onConfigChange ideStateVar)   -- What to do on LS specific Config changes
      serverConf.configSection                  -- Name of the Config section
      setup                                     -- What to do on LSP initialization requests.


lspSinkFromContext :: LSP.LanguageContextEnv config -> LspSink
lspSinkFromContext lspEnv = LspSink
  { sendNotificationToClient = \m params -> runLspT lspEnv $ LSP.sendNotification m params
  , sendRequestToClient = \m params responseHandler -> runLspT lspEnv $ LSP.sendRequest m params (liftIO . responseHandler)
  , withClientProgress = \title cancellable act -> LSP.runLspT lspEnv $ LSP.withProgress title Nothing cancellable $ \updater -> do
      withRunInIO $ \runInIO -> do
        let
          ioUpdater = \progress ->
              runInIO (updater progress)
        act ioUpdater
  , withIndefiniteClientProgress = \title cancellable act -> LSP.runLspT lspEnv $ LSP.withIndefiniteProgress title Nothing cancellable (const (liftIO act))
  , takeVfsSnapshot = LSP.runLspT lspEnv LSP.getVirtualFiles
  , currentClientCapabilities = LSP.runLspT lspEnv getClientCapabilities
  }

-- See Note [Client configuration in Rules]
defOnConfigChange :: (Show config, J.ToJSON config, Monad m, MonadIO m) => Recorder (WithPriority Log) -> MVar IdeState -> config -> m ()
defOnConfigChange recorder ideStateVar cfg = do
  -- TODO: this is nuts, we're converting back to JSON just to get a fingerprint
  let
    cfgObj = J.toJSON cfg
  mide <- liftIO $ tryReadMVar ideStateVar
  case mide of
    Nothing -> pure ()
    Just ide -> liftIO $ do
      let
        msg = Text.pack $ show cfg
      setSomethingModified Shake.VFSUnmodified ide "config change" $ do
        logWith recorder Debug $ LogConfigurationChange msg
        modifyClientSettings ide (const $ Just cfgObj)
        return [toNoFileKey Rules.GetClientSettings]


kickRules :: ([NormalizedFilePath] -> Action ()) -> Action ()
kickRules runKickRules = do
  files <- HashMap.keys <$> getFilesOfInterestUntracked
  ShakeExtras{ideTesting = IdeTesting testing, lspSink, progress} <- getShakeExtras
  let
    signal :: (KnownSymbol s) => Proxy s -> Action ()
    signal msg = when testing $
      liftIO $
        case lspSink of
          Nothing -> pure ()
          Just sink ->
            sendNotificationToClient sink (SMethod_CustomMethod msg) $ J.toJSON $ map fromNormalizedFilePath files

  signal (Proxy @"kick/start")
  liftIO $ progressUpdate progress ProgressNewStarted
  -- Run rules that produce diagnostics
  runKickRules files
  liftIO $ progressUpdate progress ProgressCompleted

  GarbageCollectVar var <- getIdeGlobalAction
  garbageCollectionScheduled <- liftIO $ readVar var
  when garbageCollectionScheduled $ do
    void garbageCollectDirtyKeys
    liftIO $ writeVar var False

  signal (Proxy @"kick/done")
