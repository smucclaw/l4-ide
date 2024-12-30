{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import L4.Annotation
import L4.ExactPrint (AnnoFirst, EPError (..), genericToTokens, prettyEPError)
import L4.Lexer (PosToken (..), SrcPos (..), TokenCategory (..))
import qualified L4.Lexer as Lexer
import qualified L4.Parser as Parser
import L4.Syntax

import qualified Ladder

import Colog.Core (LogAction (..), Severity (..), WithSeverity (..), (<&))
import qualified Colog.Core as L
import Control.Applicative (Alternative (..))
import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import qualified Control.Exception as E
import Control.Lens hiding (Iso)
import Control.Monad (forever)
import qualified Control.Monad.Extra as Extra
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import qualified Data.Aeson as Aeson
import qualified Data.Aeson as J
import qualified Data.Foldable as Foldable
import qualified Data.Maybe as Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Utf16.Rope.Mixed as Rope
import Generics.SOP as SOP
import qualified GHC.Generics as GHC
import GHC.Stack
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
handle _logger =
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
                      Left _diags ->
                        responder $
                          Left $
                            TResponseError
                              { _code = InL LSPErrorCodes_RequestFailed
                              , _message = "Internal error, failed to find the uri \"" <> Text.pack (show uri) <> "\" in the Virtual File System."
                              , _xdata = Nothing
                              }
                      Right prog ->
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
              Left _diags ->
                responder $
                  Left $
                    TResponseError
                      { _code = InL LSPErrorCodes_RequestFailed
                      , _message = "Failed to parse \"" <> Text.pack (show uri) <> "\""
                      , _xdata = Nothing
                      }
              Right ds -> do
                case runExcept $ runReaderT (toSemTokens ds) defaultInfo of
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
                      semanticTokens = relativizeTokens $ fmap toSemanticTokenAbsolute semanticTokenstoks
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
        Left diags -> pure diags
        Right _ds -> pure []
      publishDiagnostics 100 fileUri (Just version) (partitionBySource diags)

-- ----------------------------------------------------------------------------
-- JL4 Parser
-- ----------------------------------------------------------------------------

parseJL4WithWithDiagnostics :: Uri -> Text -> Either [Diagnostic] (Program Name)
parseJL4WithWithDiagnostics uri content = case Parser.execParser Parser.program fp content of
  Left err ->
    Left $ fmap doDiagnostic $ Foldable.toList err
  Right ds ->
    pure ds
 where
  doDiagnostic pError =
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

  fp = Maybe.fromMaybe "in-memory" $ uriToFilePath uri

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

-- ----------------------------------------------------------------------------
-- LSP Helpers
-- ----------------------------------------------------------------------------

data SemanticToken = SemanticToken
  { start :: Position
  , length :: UInt
  , category :: SemanticTokenTypes
  , modifiers :: [SemanticTokenModifiers]
  }
  deriving stock (Show, Eq, Ord)

toSemanticTokenAbsolute :: SemanticToken -> SemanticTokenAbsolute
toSemanticTokenAbsolute s =
  SemanticTokenAbsolute
    { _line = s.start ^. J.line
    , _startChar = s.start ^. J.character
    , _length = s.length
    , _tokenType = s.category
    , _tokenModifiers = s.modifiers
    }

standardTokenType :: TokenCategory -> Maybe SemanticTokenTypes
standardTokenType = \case
  CIdentifier -> Just SemanticTokenTypes_Variable
  CStringLit -> Just SemanticTokenTypes_String
  CNumberLit -> Just SemanticTokenTypes_Number
  CSymbol -> Just SemanticTokenTypes_Operator
  COperator -> Just SemanticTokenTypes_Operator
  CKeyword -> Just SemanticTokenTypes_Keyword
  CComment -> Just SemanticTokenTypes_Comment
  CWhitespace -> Nothing
  CDirective -> Just SemanticTokenTypes_Macro
  CEOF -> Nothing

simpleTokenType :: PosToken -> Maybe SemanticTokenTypes
simpleTokenType t = standardTokenType (Lexer.posTokenCategory t.payload)

type HoleFit = [SemanticToken]

data SemanticTokenCtx p = SemanticTokenCtx
  { toSemanticToken :: p -> Maybe SemanticTokenTypes
  , getModifiers :: p -> Maybe [SemanticTokenModifiers]
  }

defaultInfo :: SemanticTokenCtx PosToken
defaultInfo =
  SemanticTokenCtx
    { toSemanticToken = simpleTokenType
    , getModifiers = \_ -> pure []
    }

parameterType :: TokenCategory -> Maybe SemanticTokenTypes
parameterType = \case
  CIdentifier -> Just SemanticTokenTypes_Parameter
  _ -> Nothing

nameIsDirective :: TokenCategory -> Maybe SemanticTokenTypes
nameIsDirective = \case
  CIdentifier -> Just SemanticTokenTypes_Macro
  _ -> Nothing

enumType :: TokenCategory -> Maybe SemanticTokenTypes
enumType = \case
  CIdentifier -> Just SemanticTokenTypes_Enum
  _ -> Nothing

defVar :: TokenCategory -> Maybe [SemanticTokenModifiers]
defVar = \case
  CIdentifier -> Just [SemanticTokenModifiers_Declaration, SemanticTokenModifiers_Definition]
  _ -> Nothing

withModifier :: (TokenCategory -> Maybe [SemanticTokenModifiers]) -> SemanticM a -> SemanticM a
withModifier f act = do
  local (\i -> i{getModifiers = \t -> f (Lexer.posTokenCategory t.payload) <|> i.getModifiers t}) act

withTokenType :: (TokenCategory -> Maybe SemanticTokenTypes) -> SemanticM a -> SemanticM a
withTokenType f act = do
  local (\i -> i{toSemanticToken = \t -> f (Lexer.posTokenCategory t.payload) <|> i.toSemanticToken t}) act

-- ----------------------------------------------------------------------------
-- Simala AST to Semantic Tokens
-- ----------------------------------------------------------------------------

traverseCsnWithHoles :: (HasCallStack) => Anno -> [SemanticM HoleFit] -> SemanticM [SemanticToken]
traverseCsnWithHoles (Anno []) _ = pure []
traverseCsnWithHoles (Anno (AnnoHole : cs)) holeFits = case holeFits of
  [] -> lift $ throwE $ InsufficientHoleFit callStack
  (x : xs) -> do
    toks <- x
    restOfTokens <- traverseCsnWithHoles (Anno cs) xs
    pure $ toks <> restOfTokens
traverseCsnWithHoles (Anno (AnnoCsn m : cs)) xs = do
  ctx <- ask
  let
    transformSyntaxNode token = pack token <$> ctx.toSemanticToken token <*> ctx.getModifiers token
    thisSyntaxNode = Maybe.mapMaybe transformSyntaxNode (csnTokens m)

  restOfTokens <- traverseCsnWithHoles (Anno cs) xs
  pure $ thisSyntaxNode <> restOfTokens
 where
  pack :: PosToken -> SemanticTokenTypes -> [SemanticTokenModifiers] -> SemanticToken
  pack token category modifiers =
    SemanticToken
      { start = srcPosToPosition token.range.start
      , length = fromIntegral token.range.length
      , category = category
      , modifiers = modifiers
      }

srcPosToPosition :: SrcPos -> Position
srcPosToPosition s =
  Position
    { _character = fromIntegral s.column - 1
    , _line = fromIntegral s.line - 1
    }

type SemanticM a = ReaderT (SemanticTokenCtx PosToken) (Except EPError) a

-- I would prefer to avoid the duplication between this class and
-- the ToTokens class.
--
-- We might want to override some functionality here. We should perhaps
-- try to find another way to do this.
class ToSemTokens a where
  toSemTokens :: a -> SemanticM HoleFit

  default toSemTokens ::
       (SOP.Generic a, All (AnnoFirst ToSemTokens) (Code a))
    => a -> SemanticM HoleFit
  toSemTokens =
    genericToTokens (Proxy @ToSemTokens) toSemTokens traverseCsnWithHoles

instance ToSemTokens a => ToSemTokens [a] where
  toSemTokens =
    Extra.concatMapM toSemTokens

instance ToSemTokens a => ToSemTokens (Maybe a) where
  toSemTokens =
    maybe (pure []) toSemTokens

deriving anyclass instance ToSemTokens (Program Name)

-- Generic instance does not apply because we exclude the level and override
-- the token type for the name.
instance ToSemTokens (Section Name) where
  toSemTokens (MkSection ann _lvl name decls) =
    traverseCsnWithHoles ann [withTokenType nameIsDirective $ toSemTokens name, toSemTokens decls]

deriving anyclass instance ToSemTokens (TopDecl Name)
deriving anyclass instance ToSemTokens (Assume Name)
deriving anyclass instance ToSemTokens (Declare Name)
deriving anyclass instance ToSemTokens (TypeDecl Name)
deriving anyclass instance ToSemTokens (ConDecl Name)
deriving anyclass instance ToSemTokens (Type' Name)
deriving anyclass instance ToSemTokens (TypedName Name)
deriving anyclass instance ToSemTokens (OptionallyTypedName Name)
deriving anyclass instance ToSemTokens (Decide Name)
deriving anyclass instance ToSemTokens (AppForm Name)
deriving anyclass instance ToSemTokens (Expr Name)
deriving anyclass instance ToSemTokens (Branch Name)
deriving anyclass instance ToSemTokens (Pattern Name)
deriving anyclass instance ToSemTokens (TypeSig Name)
deriving anyclass instance ToSemTokens (GivethSig Name)
deriving anyclass instance ToSemTokens (GivenSig Name)
deriving anyclass instance ToSemTokens (Directive Name)

instance ToSemTokens Name where
  toSemTokens (Name ann _) =
    traverseCsnWithHoles ann []

