{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import LSP.Logger
import LSP.Core.Debouncer
import LSP.Core.FileStore hiding (Log (..))
import LSP.Core.IdeConfiguration
import qualified LSP.Core.RuleTypes as Rules
import LSP.Core.Service hiding (Log (..))
import qualified LSP.Core.Service as Service hiding (LogShake)
import qualified LSP.Core.Shake as Shake
import LSP.Core.Types.Monitoring
import LSP.Core.Types.Options
import LSP.Core.Types.Shake

import LSP.L4.Config
import LSP.L4.Handlers hiding (Log (..))
import qualified LSP.L4.Handlers as Handlers
import qualified L4.Lexer as Lexer
import LSP.L4.LanguageServer (runLanguageServer)
import qualified LSP.L4.LanguageServer as LanguageServer
import qualified LSP.L4.Rules as Rules

import Control.Concurrent.Strict
    ( newEmptyMVar, putMVar, tryReadMVar, withNumCapabilities )
import Control.Monad (unless)
import Control.Monad.IO.Class
import qualified Data.Aeson as Aeson
import qualified Data.Aeson as J
import qualified Data.Map as Map
import Data.Text ( Text )
import qualified Data.Text as Text
import Development.IDE.Graph (Rules, action)
import GHC.Conc (getNumProcessors)
import GHC.IO.Encoding
import GHC.Natural (Natural)
import Language.LSP.Protocol.Types hiding (Pattern)
import qualified Language.LSP.Protocol.Types as LSP
import Language.LSP.Server
import qualified Language.LSP.Server as LSP
import System.Directory (getCurrentDirectory)
import System.IO
import System.Time.Extra
import UnliftIO (withRunInIO)

-- ----------------------------------------------------------------------------

main :: IO ()
main = do
  -- Setup the logger
  recorder <- makeDefaultStderrRecorder Nothing
  let prettyRecorder = cmapWithPrio pretty recorder

  -- Get Arguments.
  -- If we wanted to, here is where we would add argument parsing
  args <- getDefaultArguments prettyRecorder
  -- Run the Language Server in all its glory!
  defaultMain prettyRecorder args

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
    , optExecuteCommandCommands =
        Just $ map snd l4CmdNames
    }

-- ----------------------------------------------------------------------------
-- Complicated Init Stuff
-- ----------------------------------------------------------------------------

data Log
  = LogLspStart
  | LogLspStartDuration !Seconds
  | LogShouldRunSubset !Bool
  | LogConfigurationChange Text.Text
  | LogService Service.Log
  | LogShake Shake.Log
  | LogLanguageServer LanguageServer.Log
  | LogHandlers Handlers.Log
  | LogRules Rules.Log
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
    LogConfigurationChange msg -> "Configuration changed:" <+> pretty msg
    LogService msg -> pretty msg
    LogShake msg -> pretty msg
    LogLanguageServer msg -> pretty msg
    LogHandlers msg -> pretty msg
    LogRules msg -> pretty msg

data Arguments = Arguments
  { projectRoot :: FilePath
  , rules :: Rules ()
  , lspOptions :: LSP.Options
  , defaultConfig :: Config
  , debouncer :: IO (Debouncer NormalizedUri)
  -- ^ Debouncer used for diagnostics
  , handleIn :: IO Handle
  , handleOut :: IO Handle
  , threads :: Maybe Natural
  , monitoring :: IO Monitoring
  , disableKick :: Bool
  -- ^ flag to disable kick used for testing
  }

getDefaultArguments :: Recorder (WithPriority Log) -> IO Arguments
getDefaultArguments recorder = do
  cwd <- getCurrentDirectory
  pure Arguments
    { projectRoot = cwd
    , rules = Rules.jl4Rules (cmapWithPrio LogRules recorder)
    , lspOptions = lspOptions
    , defaultConfig = defConfig
    , debouncer = newAsyncDebouncer
    , handleIn = pure stdin
    , handleOut = pure stdout
    , threads = Nothing
    , monitoring = mempty
    , disableKick = False
    }

defaultMain :: Recorder (WithPriority Log) -> Arguments -> IO ()
defaultMain recorder args = do
  setLocaleEncoding utf8
  hSetBuffering stderr LineBuffering

  let
    options = args.lspOptions
    rules = do
      args.rules
      unless args.disableKick $ action LanguageServer.kick

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
      getIdeState :: LSP.LanguageContextEnv Config -> FilePath -> Shake.ThreadQueue -> IO IdeState
      getIdeState env rootPath threadQueue = do
        t <- ioT
        logWith recorder Info $ LogLspStartDuration t
        clientCaps <- LSP.runLspT env LSP.getClientCapabilities

        let ideOpts = defaultIdeOptions
              { optKeywords = jl4Keywords
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
      setup = LanguageServer.setupLSP
          (cmapWithPrio LogLanguageServer recorder)
          args.projectRoot
          (handlers (cmapWithPrio LogHandlers recorder))
          getIdeState
      -- See Note [Client configuration in Rules]
      onConfigChange :: Config -> ServerM Config ()
      onConfigChange cfg = do
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

    runLanguageServer (cmapWithPrio LogLanguageServer recorder) options inH outH args.defaultConfig parseServerConfig onConfigChange setup

parseServerConfig :: Config -> Aeson.Value -> Either Text Config
parseServerConfig _ v = do
  case J.fromJSON v of
    J.Error e -> Left (Text.pack e)
    J.Success cfg -> Right cfg

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

jl4Keywords :: [Text]
jl4Keywords = Map.keys Lexer.keywords
