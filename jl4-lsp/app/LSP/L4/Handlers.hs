{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}

module LSP.L4.Handlers where

import Control.Concurrent.Strict (Chan, writeChan)
import Control.Exception.Safe (MonadCatch, MonadMask, MonadThrow)
import Control.Lens ((^.))
import Control.Monad.Extra (guard)
import qualified Control.Monad.Extra as Extra
import Control.Monad.Reader (MonadReader (..))
import Control.Monad.Trans.Reader (ReaderT)
import Data.Either (isRight)
import Data.Monoid (Ap (..))
import Data.Tuple (swap)
import UnliftIO (MonadUnliftIO, atomically, STM, MonadIO(..))
import qualified StmContainers.Map as STM
import Control.Applicative
import Control.Monad.Except (runExceptT)
import Control.Monad.Trans.Maybe
import qualified Control.Monad.Trans.Reader as ReaderT
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Text as Aeson
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import Data.Text (Text)
import qualified Base.Text as Text
import qualified Data.Text.Lazy as LazyText
import GHC.Generics
import LSP.L4.Base
import LSP.L4.Config
import LSP.L4.Rules hiding (Log (..))
import LSP.L4.SemanticTokens (srcPosToPosition)

import LSP.Core.FileStore hiding (Log (..))
import qualified LSP.Core.FileStore as FileStore
import LSP.Core.OfInterest hiding (Log (..))
import LSP.Core.Service hiding (Log (..))
import LSP.Core.Shake hiding (Log (..))
import qualified LSP.Core.Shake as Shake
import LSP.Core.Types.Diagnostics
import LSP.Core.Types.Location
import qualified LSP.L4.Viz.Ladder as Ladder
import LSP.Logger
import qualified Language.LSP.Protocol.Lens as J
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types
import qualified Language.LSP.Protocol.Types as LSP
import Language.LSP.Server hiding (notificationHandler, requestHandler)
import qualified Language.LSP.Server as LSP
import Language.LSP.VFS (VFS)
import LSP.L4.Actions

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
  -- ^ 'Ap' lifts the @'Monoid' a@ through the @'Applicative' ('ServerM' c)@
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
  | LogMultipleDecideClauses !Uri
  | LogSuppliedTooManyArguments [Aeson.Value]
  | LogExecutingCommand !Text
  | LogShake Shake.Log
  deriving (Show)

instance Pretty Log where
  pretty = \case
    LogOpenedTextDocument uri ->  "Opened text document:" <+> pretty (getUri uri)
    LogModifiedTextDocument uri -> "Modified text document:" <+> pretty (getUri uri)
    LogSavedTextDocument uri -> "Saved text document:" <+> pretty (getUri uri)
    LogClosedTextDocument uri -> "Closed text document:" <+> pretty (getUri uri)
    LogRequestedCompletionsFor t -> "requesting completions for:" <+> pretty t
    LogMultipleDecideClauses uri -> "Document contains multiple decide clauses:" <+> pretty (getUri uri)
    LogSuppliedTooManyArguments args -> "Visualization command was passed too many arguments, this is a bug:" <+> pretty (Aeson.encodeToLazyText args)
    LogExecutingCommand cmd -> "Executing command:" <+> pretty cmd
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
          uri = toNormalizedUri doc
        atomically $ updatePositionMapping ide (VersionedTextDocumentIdentifier doc version) []
        -- We don't know if the file actually exists, or if the contents match those on disk
        -- For example, vscode restores previously unsaved contents on open
        setFileModified (VFSModified vfs) ide uri $
          addFileOfInterest ide uri Modified{firstOpen=True}
        logWith recorder Debug $ LogOpenedTextDocument doc
    , notificationHandler SMethod_TextDocumentDidChange $ \ide vfs msg -> liftIO $ do
        let
          doc = msg ^. J.textDocument . J.uri
          uri = toNormalizedUri doc
          version = msg ^. J.textDocument . J.version
          changes = msg ^. J.contentChanges
        atomically $ updatePositionMapping ide (VersionedTextDocumentIdentifier doc version) changes
        setFileModified (VFSModified vfs) ide uri $
          addFileOfInterest ide uri Modified{firstOpen=False}
        logWith recorder Debug $ LogModifiedTextDocument doc
    , notificationHandler SMethod_TextDocumentDidSave $ \ide vfs msg -> liftIO $ do
        let
          doc = msg ^. J.textDocument . J.uri
          uri = toNormalizedUri doc
        setFileModified (VFSModified vfs) ide uri $
          addFileOfInterest ide uri OnDisk
        logWith recorder Debug $ LogSavedTextDocument doc
    , notificationHandler SMethod_TextDocumentDidClose $ \ide vfs msg -> liftIO $ do
        let
          doc = msg ^. J.textDocument . J.uri
          uri = toNormalizedUri doc
        let herald = "Closed text document: " <> getUri doc
        setSomethingModified (VFSModified vfs) ide (Text.unpack herald) $ do
          scheduleGarbageCollection ide
          deleteFileOfInterest ide uri
        logWith recorder Debug $ LogClosedTextDocument doc

    , notificationHandler SMethod_SetTrace $ \_ _ _msg -> do
        pure ()
    , -- Subscribe to notification changes
      notificationHandler SMethod_WorkspaceDidChangeConfiguration mempty
    , requestHandler SMethod_TextDocumentDefinition $ \ide params -> do
        let
          pos :: Position
          pos = params ^. J.position
          uri = params ^. J.textDocument . J.uri
          nuri = LSP.toNormalizedUri uri
        mTypeCheckedModule <- liftIO $ runAction "gotoDefinition" ide $
          useWithStale TypeCheck nuri
        let mloc = uncurry (gotoDefinition pos uri) =<< mTypeCheckedModule
        pure $ Right case mloc of
          Nothing  -> InR $ InR Null
          Just loc -> InL $ Definition (InL loc)
    , requestHandler SMethod_TextDocumentHover $ \ide params -> do
        let
          doc :: Uri
          doc = params ^. J.textDocument . J.uri
          pos :: Position
          pos = params ^. J.position
        mh <- findHover ide (LSP.toNormalizedUri doc) pos
        pure $ Right $ case mh of
          Nothing  -> InR Null
          Just h ->   InL h
    , requestHandler SMethod_WorkspaceExecuteCommand $ \ide params -> do
        let ExecuteCommandParams _ cid xdata = params

        logWith recorder Debug $ LogExecutingCommand cid
        runExceptT case lookup cid (map swap l4CmdNames) of
          Just CmdVisualize -> do
            let decodeXdata
                  | Just ((Aeson.fromJSON -> Aeson.Success uri) :  args) <- xdata
                  , msrcPos <- case args of
                     [GFromJSON srcPos, Aeson.fromJSON -> Aeson.Success simplify] -> Just (srcPos, simplify)
                     _ -> Nothing
                  = do
                    mtcRes <- liftIO $ runAction "l4.visualize" ide $ use TypeCheck $ toNormalizedUri uri
                    visualise mtcRes (atomically $ getMostRecentVisualisation ide, atomically . setMostRecentVisualisation ide) uri msrcPos
                  | otherwise = defaultResponseError $ "Failed to decode request data: " <> LazyText.toStrict (Aeson.encodeToLazyText xdata)
            decodeXdata

          -- NOTE: certain actions reset the state of the visualisation, like clicking it away, in these
          -- cases we don't want to continue rerendering
          Just CmdResetVisualization -> do
            atomically $ clearMostRecentVisualisation ide
            pure (InR Null)
          Nothing -> defaultResponseError ("Unknown command: " <> cid)
    , requestHandler SMethod_TextDocumentCodeAction $ \ide params -> do
        let
          uri = toNormalizedUri $ params ^. J.textDocument . J.uri
          rng = params ^. J.range
        diags <- atomically $ do
          activeFileDiagnosticsInRange (shakeExtras ide) uri rng
        cas <- Extra.mapMaybeM (outOfScopeAssumeQuickFix ide) diags
        pure $ Right $ InL $ fmap InR cas
    , requestHandler SMethod_TextDocumentSemanticTokensFull $ \ide req -> do
        let
          SemanticTokensParams _ _ doc = req
          uri = doc ^. J.uri

        tokens <- liftIO $ runAction "semanticTokens" ide $
          use GetRelSemanticTokens (toNormalizedUri uri)
        case tokens of
          Nothing -> do
            pure $
              Left $
                TResponseError
                  { _code = InL LSPErrorCodes_RequestFailed
                  , _message = "Internal error, failed to produce semantic tokens for " <> Text.show uri.getUri
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
        liftIO (runAction "completions" ide $ getUriContents $ toNormalizedUri uri) >>= \case
          Nothing -> pure (Right (InL []))
          Just rope -> do
            (typeCheck, _positionMapping) <- liftIO $ runAction "typecheck" ide $
              useWithStale_ TypeCheck (toNormalizedUri uri)

            let items = completions rope (toNormalizedUri uri) typeCheck (params ^. J.position)

            pure (Right (InL items))
    , requestHandler SMethod_TextDocumentCodeLens $ \ide params -> do
        let LSP.TextDocumentIdentifier uri = params ^. J.textDocument

        typeCheck <- liftIO $ runAction "typecheck" ide $
          use_ TypeCheck (toNormalizedUri uri)

        let
          mkCodeLens srcPos simplify = CodeLens
            { _command = Just Command
              { _title = t simplify
              , _command = "l4.visualize"
              , _arguments = Just [Aeson.toJSON uri, Aeson.toJSON (Generically srcPos), Aeson.toJSON simplify]
              }
            , _range = pointRange $ srcPosToPosition srcPos
            , _data_ = Nothing
            }
            where
              t False = "Visualize"
              t True  = "Simplify and visualize"

          decideToCodeLens decide =
            -- NOTE: there's a lot of DECIDE/MEANS statements that the visualizer currently doesn't work on
            -- We try to not offer any code lenses for the visualizer if that's the case.
            -- If in future this is too slow, we should think about caching these results or, even better,
            -- make the visualizer work on as many examples as possible.
            case rangeOfNode decide of
              Just node ->
                map (mkCodeLens node.start) (filter (isRight . Ladder.doVisualize decide) [False, True])
              Nothing -> []

          -- adds codelenses to visualize DECIDE or MEANS clauses
          visualizeDecides :: [CodeLens] = foldTopLevelDecides decideToCodeLens typeCheck.module'

        pure (Right (InL visualizeDecides))
    , requestHandler SMethod_TextDocumentReferences $ \ide params -> do
        let doc :: Uri = params ^. J.textDocument . J.uri
            pos :: SrcPos = lspPositionToSrcPos $ params ^. J.position
            nuri :: NormalizedUri = toNormalizedUri doc

        refs <- liftIO $ runAction "getReferences" ide $
          use_ GetReferences nuri

        let locs = map (Location doc . srcRangeToLspRange . Just) $ lookupReference pos refs
        pure (Right (InL locs))
    ]

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
-- LSP Hover
-- ----------------------------------------------------------------------------

findHover :: IdeState -> NormalizedUri -> Position -> ServerM Config (Maybe Hover)
findHover ide fileUri pos = runMaybeT $ refHover <|> tyHover
  where
  refHover = do
    refs <- MaybeT $ liftIO $ runAction "refHover" ide $
      use ResolveReferenceAnnotations fileUri
    hoistMaybe $ referenceHover pos refs

  tyHover = do
    (m, positionMapping) <- MaybeT $ liftIO $ runAction "typeHover" ide $
      useWithStale TypeCheck fileUri
    hoistMaybe $ typeHover pos fileUri m positionMapping

-- ----------------------------------------------------------------------------
-- LSP Code Actions
-- ----------------------------------------------------------------------------

outOfScopeAssumeQuickFix :: IdeState -> FileDiagnostic -> ServerM Config (Maybe CodeAction)
outOfScopeAssumeQuickFix ide fd = case fd ^. messageOfL @CheckErrorWithContext of
  Nothing -> pure Nothing
  Just ctx -> case ctx.kind of
    OutOfScopeError name ty -> do
      mTypeCheck <- liftIO $ runAction "codeAction.outOfScope" ide $ do
        use TypeCheck nuri
      case mTypeCheck of
        Nothing -> pure Nothing
        Just typeCheck -> do
          let
            assumeExpr =
              Assume emptyAnno
                (MkAssume emptyAnno
                  (MkTypeSig emptyAnno (MkGivenSig emptyAnno []) Nothing)
                  (MkAppForm emptyAnno name [] Nothing)
                  (Just $ fmap getActual ty)
                )

            topDecls = foldTopDecls (: []) typeCheck.module'

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
              { _title = "Assume `" <> prettyLayout name <> "` is defined"
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
    nuri = fd ^. fdFilePathL

    uri :: Uri
    uri = fromNormalizedUri nuri

hasTypeInferenceVars :: Type' Resolved -> Bool
hasTypeInferenceVars = \case
  Type   _ -> False
  TyApp  _ _n ns -> any hasTypeInferenceVars ns
  Fun    _ opts ty -> any hasNamedTypeInferenceVars opts || hasTypeInferenceVars ty
  Forall _ _ ty -> hasTypeInferenceVars ty
  InfVar {} -> True

hasNamedTypeInferenceVars :: OptionallyNamedType Resolved -> Bool
hasNamedTypeInferenceVars = \case
  MkOptionallyNamedType _ _ ty -> hasTypeInferenceVars ty

data L4Cmd
  = CmdVisualize
  | CmdResetVisualization
  deriving stock (Eq, Show, Enum, Bounded)

l4CmdNames :: [(L4Cmd, Text)]
l4CmdNames =
  [ (CmdVisualize, "l4.visualize")
  , (CmdResetVisualization, "l4.resetvisualization")
  ]

-- | Given an 'Aeson.Value', matches successfully, if decoding via @a@'s 'Generic' instance is successful
pattern GFromJSON :: forall a. (Generic a, Aeson.GFromJSON Aeson.Zero (Rep a)) => forall. a -> Aeson.Value
pattern GFromJSON a <- (Aeson.fromJSON -> Aeson.Success (Generically a))
