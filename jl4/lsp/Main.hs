{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import L4.Evaluate
import L4.ExactPrint (prettyEPError, HasSrcRange(..))
import L4.Lexer (SrcPos (..), SrcRange(..))
import qualified L4.Parser as Parser
import L4.Syntax
import L4.TypeCheck

import qualified Ladder
import qualified SemanticTokens

import Colog.Core (LogAction (..), Severity (..), WithSeverity (..), (<&))
import qualified Colog.Core as L
import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import qualified Control.Exception as E
import Control.Lens hiding (Iso)
import Control.Monad (forever)
import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import qualified Data.Aeson as Aeson
import qualified Data.Aeson as J
import qualified Data.Foldable as Foldable
import qualified Data.Maybe as Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Utf16.Rope.Mixed as Rope
import qualified GHC.Generics as GHC
import Language.LSP.Diagnostics
import Language.LSP.Logging
import qualified Language.LSP.Protocol.Lens as J
import Language.LSP.Protocol.Message
import qualified Language.LSP.Protocol.Message as LSP
import Language.LSP.Protocol.Types hiding (Pattern)
import qualified Language.LSP.Protocol.Types as LSP
import Language.LSP.Server
import Language.LSP.VFS (VirtualFile (..))
import Prettyprinter
import qualified Prettyprinter.Render.Text as Pretty
import System.Exit
import System.IO

-- ----------------------------------------------------------------------------

main :: IO ()
main = do
  run >>= \case
    0 -> exitSuccess
    c -> exitWith . ExitFailure $ c

-- ----------------------------------------------------------------------------

data Config = Config
  { serverExecutablePath :: Maybe Text
  }
  deriving (GHC.Generic, J.ToJSON, J.FromJSON, Show)

run :: IO Int
run = flip E.catches handlers $ do
  rin <- atomically newTChan :: IO (TChan ReactorInput)

  let
    render = Pretty.renderStrict . layoutPretty defaultLayoutOptions

    prettyMsg :: (Pretty a) => WithSeverity a -> Doc ann
    prettyMsg l = "[" <> viaShow (L.getSeverity l) <> "] " <> pretty (L.getMsg l)
    -- Three loggers:
    -- 1. To stderr
    -- 2. To the client (filtered by severity)
    -- 3. To both
    stderrLogger :: LogAction IO (WithSeverity Text)
    stderrLogger = L.cmap (show . prettyMsg) L.logStringStderr
    clientLogger :: LogAction (LspM Config) (WithSeverity Text)
    clientLogger = defaultClientLogger
    dualLogger :: LogAction (LspM Config) (WithSeverity Text)
    dualLogger = clientLogger <> L.hoistLogAction liftIO stderrLogger

    serverDefinition =
      ServerDefinition
        { defaultConfig = Config{serverExecutablePath = Nothing}
        , parseConfig = \_old v -> do
            case J.fromJSON v of
              J.Error e -> Left (Text.pack e)
              J.Success cfg -> Right cfg
        , onConfigChange = const $ pure ()
        , configSection = "jl4"
        , doInitialize = \env _ -> forkIO (reactor stderrLogger rin) >> pure (Right env)
        , -- Handlers log to both the client and stderr
          staticHandlers = \_caps -> lspHandlers dualLogger rin
        , interpretHandler = \env -> Iso (runLspT env) liftIO
        , options = lspOptions
        }

  let

  runServerWithHandles
    -- Log to both the client and stderr when we can, stderr beforehand
    (L.cmap (fmap (render . pretty)) stderrLogger)
    (L.cmap (fmap (render . pretty)) dualLogger)
    stdin
    stdout
    serverDefinition
 where
  handlers =
    [ E.Handler ioExcept
    , E.Handler someExcept
    ]
  ioExcept (e :: E.IOException) = print e >> return 1
  someExcept (e :: E.SomeException) = print e >> return 1

-- ---------------------------------------------------------------------

-- | The reactor is a process that serialises and buffers all requests from the
-- LSP client, so they can be sent to the backend compiler one at a time, and a
-- reply senText.
newtype ReactorInput
  = ReactorAction (IO ())

-- ---------------------------------------------------------------------

-- | The single point that all events flow through, allowing management of state
--  to stitch replies and requests together from the two asynchronous sides: lsp
--  server and backend compiler
reactor :: LogAction IO (WithSeverity Text) -> TChan ReactorInput -> IO ()
reactor logger inp = do
  logger <& "Started the reactor" `WithSeverity` Info
  forever $ do
    ReactorAction act <- atomically $ readTChan inp
    act

-- | Check if we have a handler, and if we create a haskell-lsp handler to pass it as
--  input into the reactor
lspHandlers :: (m ~ LspM Config) => LogAction m (WithSeverity Text) -> TChan ReactorInput -> Handlers m
lspHandlers logger rin = mapHandlers goReq goNot (handle logger)
 where
  goReq :: forall (a :: LSP.Method LSP.ClientToServer LSP.Request). Handler (LspM Config) a -> Handler (LspM Config) a
  goReq f = \msg k -> do
    env <- getLspEnv
    liftIO $ atomically $ writeTChan rin $ ReactorAction (runLspT env $ f msg k)

  goNot :: forall (a :: LSP.Method LSP.ClientToServer LSP.Notification). Handler (LspM Config) a -> Handler (LspM Config) a
  goNot f = \msg -> do
    env <- getLspEnv
    liftIO $ atomically $ writeTChan rin $ ReactorAction (runLspT env $ f msg)

handle :: (m ~ LspM Config) => LogAction m (WithSeverity Text) -> Handlers m
handle logger =
  mconcat
    [ -- We need these notifications handlers to declare that we handle these requests
      notificationHandler SMethod_Initialized mempty
    , -- Handling of the virtual file system
      notificationHandler SMethod_TextDocumentDidOpen $ \msg -> do
        let
          doc = msg ^. J.params . J.textDocument . J.uri
        sendDiagnostics $ LSP.toNormalizedUri doc
    , notificationHandler SMethod_TextDocumentDidChange $ \msg -> do
        let
          doc = msg ^. J.params . J.textDocument . J.uri
        sendDiagnostics $ LSP.toNormalizedUri doc
    , notificationHandler SMethod_TextDocumentDidSave $ \msg -> do
        let
          doc = msg ^. J.params . J.textDocument . J.uri
        sendDiagnostics $ LSP.toNormalizedUri doc
    , notificationHandler SMethod_TextDocumentDidClose $ \msg -> do
        let
          doc = msg ^. J.params . J.textDocument . J.uri
        sendDiagnostics $ LSP.toNormalizedUri doc
    , notificationHandler SMethod_SetTrace $ \_msg -> do
        pure ()
    , -- Subscribe to notification changes
      notificationHandler SMethod_WorkspaceDidChangeConfiguration mempty
    , requestHandler SMethod_TextDocumentDefinition $ \req responder -> do
        let
          doc :: Uri
          doc = req ^. J.params ^. J.textDocument . J.uri
          pos :: Position
          pos = req ^. J.params ^. J.position
        mloc <- gotoDefinition (LSP.toNormalizedUri doc) pos
        case mloc of
          Nothing  -> responder $ Right $ InR $ InR $ Null
          Just loc -> responder $ Right $ InL $ Definition (InL loc)
    , requestHandler SMethod_TextDocumentHover $ \ req responder -> do
        let
          doc :: Uri
          doc = req ^. J.params ^. J.textDocument . J.uri
          pos :: Position
          pos = req ^. J.params ^. J.position
        logger <& "Called hover" `WithSeverity` Info
        mh <- findHover (LSP.toNormalizedUri doc) pos
        logger <& ("Returned from hover with: " <> Text.pack (show mh)) `WithSeverity` Info
        case mh of
          Nothing  -> responder $ Right $ InR $ Null
          Just h -> responder $ Right $ InL $ h
    , requestHandler SMethod_WorkspaceExecuteCommand $ \req responder -> do
        let
          (TRequestMessage _ _ _ (ExecuteCommandParams _ _cid xdata)) = req
        case xdata of
          Just [uriJson]
            | Aeson.Success (uri :: Uri) <- Aeson.fromJSON uriJson -> do
                let
                  fileUri = toNormalizedUri uri
                mfile <- getVirtualFile fileUri
                case mfile of
                  Nothing -> pure ()
                  Just (VirtualFile _ _ rope) -> do
                    let
                      contents = Rope.toText rope

                    case parseJL4WithWithDiagnostics uri contents of
                      (_, Nothing) ->
                        responder $
                          Left $
                            TResponseError
                              { _code = InL LSPErrorCodes_RequestFailed
                              , _message = "Internal error, failed to find the uri \"" <> Text.pack (show uri) <> "\" in the Virtual File System."
                              , _xdata = Nothing
                              }
                      (_, Just (prog, _, _)) ->
                        responder $ Right $ InL $ Aeson.toJSON $ Ladder.visualise prog
          _ ->
            responder $ Left $ undefined
    , requestHandler SMethod_TextDocumentSemanticTokensFull $ \req responder -> do
        let
          TRequestMessage _ _ _ (SemanticTokensParams _ _ doc) = req
          uri = doc ^. J.uri

        mVirtFile <- getVirtualFile $ toNormalizedUri uri
        case mVirtFile of
          Nothing -> do
            responder $
              Left $
                TResponseError
                  { _code = InL LSPErrorCodes_RequestFailed
                  , _message = "Internal error, failed to find the uri \"" <> Text.pack (show uri) <> "\" in the Virtual File System."
                  , _xdata = Nothing
                  }
          Just (VirtualFile _ _ rope) -> do
            let
              contents = Rope.toText rope

            case parseJL4WithWithDiagnostics uri contents of
              (_, Nothing) ->
                responder $
                  Left $
                    TResponseError
                      { _code = InL LSPErrorCodes_RequestFailed
                      , _message = "Failed to parse \"" <> Text.pack (show uri) <> "\""
                      , _xdata = Nothing
                      }
              (_, Just (prog, _, _)) -> do
                case runExcept $ runReaderT (SemanticTokens.toSemTokens prog) SemanticTokens.defaultSemanticTokenCtx of
                  Left err -> do
                    -- TODO: log error
                    responder $
                      Left $
                        TResponseError
                          { _code = InL LSPErrorCodes_RequestFailed
                          , _message = "Internal error, failed to produce semantic tokens for \"" <> Text.pack (show uri) <> "\", reason:" <> prettyEPError err
                          , _xdata = Nothing
                          }
                  Right semanticTokenstoks -> do
                    let
                      semanticTokens = relativizeTokens $ fmap SemanticTokens.toSemanticTokenAbsolute semanticTokenstoks
                    case encodeTokens defaultSemanticTokensLegend semanticTokens of
                      Left err -> do
                        responder $
                          Left $
                            TResponseError
                              { _code = InL LSPErrorCodes_RequestFailed
                              , _message = "Internal error, failed to encode semantic tokens for \"" <> Text.pack (show uri) <> "\", reason:" <> err
                              , _xdata = Nothing
                              }
                      Right semanticTokensData -> do
                        responder $
                          Right $
                            InL $
                              SemanticTokens
                                { _resultId = Nothing
                                , _data_ = semanticTokensData
                                }
    ]

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
        Just
          [ "viz.showViz"
          ]
    }

-- ----------------------------------------------------------------------------
-- LSP Diagnostics
-- ----------------------------------------------------------------------------

sendDiagnostics :: NormalizedUri -> LspM Config ()
sendDiagnostics fileUri = do
  mfile <- getVirtualFile fileUri
  case mfile of
    Nothing -> pure ()
    Just (VirtualFile version _ rope) -> do
      let
        contents = Rope.toText rope

      diags <- case parseJL4WithWithDiagnostics (fromNormalizedUri fileUri) contents of
        (diags, _) -> pure diags
      publishDiagnostics 100 fileUri (Just version) (partitionBySource diags)

-- ----------------------------------------------------------------------------
-- LSP Go to Definition
-- ----------------------------------------------------------------------------

gotoDefinition :: NormalizedUri -> Position -> LspM Config (Maybe Location)
gotoDefinition fileUri pos = do
  mfile <- getVirtualFile fileUri
  case mfile of
    Nothing -> pure Nothing
    Just (VirtualFile _version _ rope) -> do
      let
        contents = Rope.toText rope
        (_diags, mr) = parseJL4WithWithDiagnostics uri contents

      pure $ do
        (_, rprog, _) <- mr
        range <- findDefinition (lspPositionToSrcPos pos) rprog
        pure (Location uri (srcRangeToLSPRange (Just range)))
  where
    uri = LSP.fromNormalizedUri fileUri

-- ----------------------------------------------------------------------------
-- LSP Hover
-- ----------------------------------------------------------------------------

findHover :: NormalizedUri -> Position -> LspT Config IO (Maybe Hover)
findHover fileUri pos = do
  mfile <- getVirtualFile fileUri
  case mfile of
    Nothing -> pure Nothing
    Just (VirtualFile _version _ rope) -> do
      let
        contents = Rope.toText rope
        (_diags, mr) = parseJL4WithWithDiagnostics uri contents

      pure $ do
        (_, rprog, subst) <- mr
        (range, t) <- findType (lspPositionToSrcPos pos) rprog
        pure (Hover (InL (mkPlainText (simpleprint (applyFinalSubstitution subst t)))) (Just (srcRangeToLSPRange (Just range))))
  where
    uri = LSP.fromNormalizedUri fileUri

-- ----------------------------------------------------------------------------
-- JL4 Parser
-- ----------------------------------------------------------------------------

parseJL4WithWithDiagnostics :: Uri -> Text -> ([Diagnostic], Maybe (Program Name, Program Resolved, Substitution))
parseJL4WithWithDiagnostics uri content = case Parser.execParser Parser.program fp content of
  Left err ->
    (fmap parseErrorToDiagnostic $ Foldable.toList err, Nothing)
  Right prog ->
    -- parsing successful
    case doCheckProgram prog of
      (errs, rprog, subst) ->
        let
          results
            | all ((== SInfo) . severity) errs =
              -- typechecking successful
              doEvalProgram rprog
            | otherwise = []
        in
          (fmap checkErrorToDiagnostic errs <> fmap evalResultToDiagnostic results, Just (prog, rprog, subst))
 where

  fp = Maybe.fromMaybe "in-memory" $ uriToFilePath uri

evalResultToDiagnostic :: (SrcRange, Either EvalException Value) -> Diagnostic
evalResultToDiagnostic (range, res) =
  Diagnostic
    (srcRangeToLSPRange (Just range))
    (Just LSP.DiagnosticSeverity_Information)
    Nothing
    Nothing
    (Just "eval")
    (either (Text.pack . show) renderValue res)
    Nothing
    (Just [])
    Nothing

checkErrorToDiagnostic :: CheckErrorWithContext -> Diagnostic
checkErrorToDiagnostic checkError =
  Diagnostic
    (srcRangeToLSPRange r)
    (Just (translateSeverity (severity checkError))) -- LSP.DiagnosticSeverity_Error) -- severity
    Nothing -- code
    Nothing
    (Just "check") -- source (i.e., a string describing the source component of this diagnostic)
    (prettyCheckErrorWithContext checkError)
    Nothing -- tags
    (Just [])
    Nothing
  where
    r = rangeOf checkError

    translateSeverity SInfo  = LSP.DiagnosticSeverity_Information
    translateSeverity SWarn  = LSP.DiagnosticSeverity_Warning
    translateSeverity SError = LSP.DiagnosticSeverity_Error

srcRangeToLSPRange :: Maybe SrcRange -> LSP.Range
srcRangeToLSPRange Nothing = LSP.Range (LSP.Position 0 0) (LSP.Position 0 0)
srcRangeToLSPRange (Just range) = LSP.Range (srcPosToLSPPosition range.start) (srcPosToLSPPosition range.end)

srcPosToLSPPosition :: SrcPos -> LSP.Position
srcPosToLSPPosition s =
  LSP.Position
    { _character = fromIntegral $ s.column - 1
    , _line = fromIntegral $ s.line - 1
    }

lspPositionToSrcPos :: LSP.Position -> SrcPos
lspPositionToSrcPos (LSP.Position { _character = c, _line = l }) =
  MkSrcPos "" (fromIntegral $ l + 1) (fromIntegral $ c + 1)

parseErrorToDiagnostic :: Parser.PError -> Diagnostic
parseErrorToDiagnostic pError =
  Diagnostic
    (LSP.Range start (nextLine start))
    (Just LSP.DiagnosticSeverity_Error) -- severity
    Nothing -- code
    Nothing
    (Just pError.origin) -- source
    pError.message
    Nothing -- tags
    (Just [])
    Nothing
 where
  start = sourcePosToPosition pError.start

  sourcePosToPosition s =
    LSP.Position
      { _character = fromIntegral $ s.column - 1
      , _line = fromIntegral $ s.line - 1
      }

  nextLine p =
    LSP.Position
      { _character = 0
      , _line = p ^. J.line + 1
      }

