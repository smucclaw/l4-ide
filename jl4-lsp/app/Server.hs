{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
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
import LSP.L4.LanguageServer (runLanguageServer, Communication (..))
import qualified LSP.L4.LanguageServer as LanguageServer
import qualified LSP.L4.Rules as Rules
import qualified L4.EvaluateLazy as EvaluateLazy
import L4.TracePolicy (lspDefaultPolicy)
import L4.EvaluateLazy.GraphVizOptions (defaultGraphVizOptions)

import Control.Concurrent.Strict
    ( newEmptyMVar, putMVar, tryReadMVar, withNumCapabilities, writeChan, newChan, readChan )
import Control.Monad (unless, forever)
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
import UnliftIO (withRunInIO, race_, withAsync, finally, MVar)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import Data.ByteString.Builder.Extra (defaultChunkSize)
import qualified Network.WebSockets as WS
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy.Char8 as C8L
import qualified Options.Applicative as Opa

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
  | LogWebsocket WebsocketLog
  | LogWorkingDirectory FilePath

data WebsocketLog
  = WebsocketShutDown
  | WebsocketNewConnection
  | WebsocketConnectionClosed
  deriving stock Show

instance Pretty Log where
  pretty = \ case
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
    LogWebsocket wmsg -> "Websocket:" <+> pretty wmsg
    LogWorkingDirectory wd -> "Starting with working directory:" <+> pretty wd

instance Pretty WebsocketLog where
  pretty = \ case
    WebsocketShutDown -> "shut down server"
    WebsocketNewConnection -> "new connection established"
    WebsocketConnectionClosed -> "closed connection to client"

data Arguments = Arguments
  { projectRoot :: FilePath
  , rules :: Rules ()
  , evalConfig :: EvaluateLazy.EvalConfig
  , lspOptions :: LSP.Options
  , defaultConfig :: Config
  , debouncer :: IO (Debouncer NormalizedUri)
  -- ^ Debouncer used for diagnostics
  , communication :: CommunicationKind
  , threads :: Maybe Natural
  , monitoring :: IO Monitoring
  , disableKick :: Bool
  -- ^ flag to disable kick used for testing
  }

data CliOptions
  = MkCliOptions
  { cwd :: Maybe FilePath
  , communication :: CommunicationKind
  }
  deriving (Eq, Show)

data CommunicationKind
  = StdIO
  | Websocket {host :: String, port :: Int}
  deriving stock (Eq, Show)

parseComm :: Opa.ParserInfo CliOptions
parseComm =
  Opa.info
    (MkCliOptions
      <$> Opa.optional (Opa.strOption (Opa.long "cwd"))
      <*>
      (Opa.helper <*> Opa.hsubparser
        (Opa.command "ws"
          (Opa.info
            (Websocket
              <$> Opa.strOption (Opa.long "host" <> Opa.value  "localhost")
              <*> Opa.option Opa.auto (Opa.long "port" <> Opa.value 8007)
            )
            (Opa.briefDesc <> Opa.progDesc "run the language server over a websocket connection")
          )
        )
        Opa.<|> pure StdIO
      )
    )
    (Opa.briefDesc <> Opa.progDesc "The L4 language server. Invoke to run using StdIO")

getDefaultArguments :: Recorder (WithPriority Log) -> IO Arguments
getDefaultArguments recorder = do
  MkCliOptions
    { communication, cwd } <- Opa.execParser parseComm
  projectRoot <- maybe getCurrentDirectory pure cwd
  fixedNow <- EvaluateLazy.readFixedNowEnv
  -- LSP default: avoid editor noise (TRACE-GRAPHVIZ-ARCHITECTURE.md)
  let tracePolicy = lspDefaultPolicy defaultGraphVizOptions
  evalConfig <- EvaluateLazy.resolveEvalConfig fixedNow tracePolicy
  logWith recorder Debug $ LogWorkingDirectory projectRoot
  pure Arguments
    { projectRoot
    , rules = Rules.jl4Rules evalConfig projectRoot (cmapWithPrio LogRules recorder)
    , evalConfig
    , lspOptions = lspOptions
    , defaultConfig = defConfig
    , debouncer = newAsyncDebouncer
    , threads = Nothing
    , monitoring = mempty
    , disableKick = False
    , communication
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

  numProcessors <- getNumProcessors
  let
    numCapabilities = max 1 $ maybe (numProcessors `div` 2) fromIntegral args.threads

  withNumCapabilities numCapabilities $ do
    ioT <- offsetTime
    logWith recorder Info LogLspStart

    let
      getIdeState :: MVar IdeState -> LSP.LanguageContextEnv Config -> FilePath -> Shake.ThreadQueue -> IO IdeState
      getIdeState ideStateVar env rootPath threadQueue = do
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
          (handlers args.evalConfig (cmapWithPrio LogHandlers recorder))

      -- See Note [Client configuration in Rules]
      onConfigChange :: MVar IdeState -> Config -> ServerM Config ()
      onConfigChange ideStateVar cfg = do
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

    let runServerWithCommunication comm
          = do
            ideStateVar <- newEmptyMVar
            runLanguageServer
              (cmapWithPrio LogLanguageServer recorder)
              options
              comm
              args.defaultConfig
              parseServerConfig
              (onConfigChange ideStateVar)
              (setup (getIdeState ideStateVar))
    case args.communication of
      StdIO -> runServerWithCommunication =<< do
        let hin = stdin
            hout = stdout
        hSetBuffering hin NoBuffering
        hSetEncoding hin utf8
        pure Communication
          { inwards = BS.hGetSome hin defaultChunkSize
          , outwards = \out -> do
              BSL.hPut hout out
              hFlush hout
          }

      Websocket host port -> do
        WS.runServer host port \pending -> do
          -- NOTE: this is where to send back headers if any
          conn <- WS.acceptRequest pending
          logWith recorder Info $ LogWebsocket WebsocketNewConnection

          WS.withPingThread conn 30 (pure ()) do
            outChan <- newChan
            inChan <- newChan

            let comm = Communication {inwards = readChan inChan, outwards = writeChan outChan}

            withAsync (runServerWithCommunication comm) \_lspAsync ->
              -- NOTE: web clients don't add Content-Length headers since
              -- websockets do the chunking for us, since the haskell lsp library
              -- doesn't support this behaviour, we add and remove the header ourselves
              -- We exploit the fact that LSP messages look like this:
              -- <headers> Content-Length: <content length> \r\n\r\n { <json content> }
              race_
                (forever do
                  msg <- readChan outChan
                  let msg' = C8L.dropWhile (/= '{') msg
                  WS.sendTextData conn msg'
                )
                (forever do
                  msg <- WS.receiveData conn
                  let msg' = "Content-Length: " <> C8.pack (show (BS.length msg)) <> "\r\n\r\n" <> msg
                  writeChan inChan msg'
                )
          `finally` do
            logWith recorder Info $ LogWebsocket WebsocketConnectionClosed
        logWith recorder Error $ LogWebsocket WebsocketShutDown

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
