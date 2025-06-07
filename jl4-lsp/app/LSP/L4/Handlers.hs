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
import Control.Monad.Except (runExceptT, throwError, MonadError(..), ExceptT(..))
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
import GHC.TypeLits (Symbol)
import Data.Proxy (Proxy (..))
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
import qualified LSP.L4.Viz.CustomProtocol as Ladder
import LSP.L4.Viz.CustomProtocol (LadderRequestParams)
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
  | LogFileStore FileStore.Log
  | LogMultipleDecideClauses !Uri
  | LogHandlingCustomRequest !Uri !Text          -- ^ Uri CustomMethodName
  | LogSuppliedTooManyArguments [Aeson.Value]
  | LogExecutingCommand !Text
  | LogShake Shake.Log
  deriving (Show)

instance Pretty Log where
  pretty = \ case
    LogOpenedTextDocument uri ->  "Opened text document:" <+> pretty (getUri uri)
    LogModifiedTextDocument uri -> "Modified text document:" <+> pretty (getUri uri)
    LogSavedTextDocument uri -> "Saved text document:" <+> pretty (getUri uri)
    LogClosedTextDocument uri -> "Closed text document:" <+> pretty (getUri uri)
    LogMultipleDecideClauses uri -> "Document contains multiple decide clauses:" <+> pretty (getUri uri)
    LogSuppliedTooManyArguments args -> "Visualization command was passed too many arguments, this is a bug:" <+> pretty (Aeson.encodeToLazyText args)
    LogExecutingCommand cmd -> "Executing command:" <+> pretty cmd
    LogFileStore msg -> pretty msg
    LogShake msg -> pretty msg
    LogHandlingCustomRequest uri method -> "Handling custom request:" <+> pretty (getUri uri) <+> pretty method

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
        let mloc = uncurry (gotoDefinition pos) =<< mTypeCheckedModule
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
                  | Just ((Aeson.fromJSON -> Aeson.Success verTextDocId) :  args) <- xdata
                  , msrcPos <- case args of
                     [GFromJSON srcPos, Aeson.fromJSON -> Aeson.Success simplify] -> Just (srcPos, simplify)
                     _ -> Nothing
                  = do
                    mtcRes <- liftIO $ runAction "l4.visualize" ide $ use TypeCheck $ toNormalizedUri verTextDocId._uri
                    visualise mtcRes (atomically $ getMostRecentVisualisation ide, atomically . setMostRecentVisualisation ide) verTextDocId msrcPos
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
        liftIO (runAction "completions" ide $ getUriContents $ toNormalizedUri uri) >>= \ case
          Nothing -> pure (Right (InL []))
          Just rope -> do
            (typeCheck, _positionMapping) <- liftIO $ runAction "typecheck" ide $
              useWithStale_ TypeCheck (toNormalizedUri uri)

            let items = completions rope (toNormalizedUri uri) typeCheck (params ^. J.position)

            pure (Right (InL items))
    , requestHandler SMethod_TextDocumentCodeLens $ \ide params -> do
        verTextDocId <- liftIO $ runAction "codeLens.getVersionedTextDocId" ide $
          FileStore.getVersionedTextDoc $ params ^. J.textDocument

        typeCheck <- liftIO $ runAction "typecheck" ide $
          use_ TypeCheck (toNormalizedUri verTextDocId._uri)

        let
          mkCodeLens srcPos simplify = CodeLens
            { _command = Just Command
              { _title = t simplify
              , _command = "l4.visualize"
              , _arguments = Just [Aeson.toJSON verTextDocId, Aeson.toJSON (Generically srcPos), Aeson.toJSON simplify]
              }
            , _range = pointRange $ srcPosToPosition srcPos
            , _data_ = Nothing
            }
            where
              t False = "Visualize"
              t True  = "Simplify and visualize"

          --  Check if can make viz with a given simplify flag
          canVisualize decide simplify =
            let cfg = Ladder.mkVizConfig verTextDocId typeCheck.module' typeCheck.substitution simplify
            in isRight (Ladder.doVisualize decide cfg)

          decideToCodeLens decide =
            -- NOTE: there's a lot of DECIDE/MEANS statements that the visualizer currently doesn't work on
            -- We try to not offer any code lenses for the visualizer if that's the case.
            -- If in future this is too slow, we should think about caching these results or, even better,
            -- make the visualizer work on as many examples as possible.
            case rangeOfNode decide of
              Just node ->
                let simplifyFlags = [False, True]
                in map (mkCodeLens node.start) (filter (canVisualize decide) simplifyFlags)
              Nothing -> []

          -- adds codelenses to visualize DECIDE or MEANS clauses
          visualizeDecides :: [CodeLens] = foldTopLevelDecides decideToCodeLens typeCheck.module'

        pure (Right (InL visualizeDecides))
    , requestHandler SMethod_TextDocumentReferences $ \ide params -> do
        let doc :: Uri = params ^. J.textDocument . J.uri
            pos :: SrcPos = lspPositionToSrcPos $ params ^. J.position
            nuri :: NormalizedUri = toNormalizedUri doc

        refs <- liftIO $ runAction "getReferences" ide do
          revDeps <- use_ GetReverseDependencies nuri
          mconcat <$> uses_ GetReferences (nuri : revDeps)

        let locs = map (\range -> Location (fromNormalizedUri range.moduleUri) (srcRangeToLspRange (Just range))) $ lookupReference pos refs
        pure (Right (InL locs))

    -- custom requests
    , requestHandler (SMethod_CustomMethod (Proxy @Ladder.EvalAppMethodName)) $ \ide params ->
        liftIO $ runVizHandlerM $ withVizRequestContext recorder (Proxy @Ladder.EvalAppMethodName) params ide $
          \evalParams tcRes recentViz -> do
            let vizConfig = Ladder.getVizConfig . (.vizState) $ recentViz
            mEvalDeps <- liftIO $ runAction "l4/evalApp" ide $ use (AttachCallStack [vizConfig.moduleUri] GetLazyEvaluationDependencies) vizConfig.moduleUri
            case mEvalDeps of
              Nothing -> throwError $ TResponseError
                { _code = InR ErrorCodes_InvalidRequest
                , _message = "Failed to get evaluation dependencies for " <> (fromNormalizedUri vizConfig.moduleUri).getUri
                , _xdata = Nothing
                }
              Just (evalEnv, _) -> do
                result <- MkVizHandler $ evalApp (evalEnv, tcRes.module') evalParams recentViz
                logWith recorder Debug $
                  LogHandlingCustomRequest evalParams.verDocId._uri
                  ("Eval result: " <> Text.show result)
                pure result

    , requestHandler (SMethod_CustomMethod (Proxy @Ladder.InlineExprsMethodName)) $ \ide params ->
        liftIO $ runVizHandlerM $ withVizRequestContext recorder (Proxy @Ladder.InlineExprsMethodName) params ide $
          \(ieParams :: Ladder.InlineExprsRequestParams) _tcRes recentViz ->
            let postInliningDecide = Ladder.inlineExprs recentViz.vizState recentViz.decide ieParams.uniques
            in MkVizHandler $
              case Ladder.doVisualize postInliningDecide (Ladder.getVizConfig recentViz.vizState) of
                Right (vizProgramInfo, vizState) -> do
                  -- Update RecentlyVisualized's decide with the latest vizState and postInliningDecide,
                  -- so that they can be used for further l4/inlineExprs requests.
                  -- (The usecase here: think of a Decide with multiple inline-able Uniques,
                  -- and where user does the inlining in stages.)
                  -- IMPT: The state synchronization / managing of state (re the stuff in RecentlyVisualized etc) feels potentially complicated:
                  -- I definitely have NOT thought through it carefully.
                  liftIO $ atomically $ setMostRecentVisualisation ide $ recentViz {vizState = vizState, decide = postInliningDecide}
                  pure $ Aeson.toJSON vizProgramInfo
                Left vizError ->
                  defaultResponseError $ Text.unlines
                    [ "Could not visualize:"
                    , getUri ieParams.verDocId._uri
                    , Ladder.prettyPrintVizError vizError
                    ]
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
                      Text.strip (prettyLayout (0, assumeExpr)) <> "\n\n"
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
hasTypeInferenceVars = \ case
  Type   _ -> False
  TyApp  _ _n ns -> any hasTypeInferenceVars ns
  Fun    _ opts ty -> any hasNamedTypeInferenceVars opts || hasTypeInferenceVars ty
  Forall _ _ ty -> hasTypeInferenceVars ty
  InfVar {} -> True

hasNamedTypeInferenceVars :: OptionallyNamedType Resolved -> Bool
hasNamedTypeInferenceVars = \ case
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

-------------------------------------------------------------------------
-- VizHandler
-------------------------------------------------------------------------

-- TODO: Add logging service that wraps Handlers' logWith
-- | Monad for handling custom viz requests
newtype VizHandler (method :: Symbol) a = MkVizHandler
  (ExceptT
    (TResponseError (Method_CustomMethod @ClientToServer @Request method))
    IO a)
  deriving newtype (Functor, Applicative, Monad, MonadError (TResponseError (Method_CustomMethod @ClientToServer @Request method)), MonadIO)

runVizHandlerM :: Ladder.CustomMethod method => VizHandler method a -> IO (Either (TResponseError (Method_CustomMethod @ClientToServer @Request method)) a)
runVizHandlerM (MkVizHandler m) = runExceptT m

-------------------------------------------------------------------------
-- withVizRequestContext
-------------------------------------------------------------------------

withVizRequestContext
  :: forall (method :: Symbol) a params.
       ( Ladder.CustomMethod method
       , LadderRequestParams params )
  => Recorder (WithPriority Log)
  -> Proxy method
  -> MessageParams ('Method_CustomMethod method)
  -> IdeState
  -> (params -> TypeCheckResult -> Shake.RecentlyVisualised -> VizHandler method a)
  -- ^ the core handler
  -> VizHandler method a
withVizRequestContext recorder method params ide handlerKont = do
  decodedParams <- guardDecodeParams @method params
  let verDocId = decodedParams.verDocId
  logWith recorder Debug $ LogHandlingCustomRequest verDocId._uri (Ladder.getMethodName method)

  mtcRes <- liftIO $ runAction (Text.unpack $ Ladder.getMethodName method) ide $ use TypeCheck $ toNormalizedUri verDocId._uri
  mRecentViz <- liftIO $ atomically $ getMostRecentVisualisation ide

  -- The following two cases should be impossible,
  -- since the verTxtDocId that the client sends us in the custom request
  -- corresponds to the one that it got when it was first requested to render the VizExpr.
  case (mtcRes, mRecentViz) of
    (Nothing, _) -> throwError $ TResponseError
      { _code = InR ErrorCodes_InvalidRequest
      , _message = "Failed to get type check for " <> Text.show verDocId._uri
      , _xdata = Nothing
      }
    (_, Nothing) -> throwError $ TResponseError
      { _code = InR ErrorCodes_InvalidRequest
      , _message = "No recent visualisation found, when trying to handle " <> Ladder.getMethodName method <> ". This case should be impossible."
      , _xdata = Nothing
      }
    (Just tcRes, Just recentViz) -> do
      _ <- checkVizVersion decodedParams recentViz
      handlerKont decodedParams tcRes recentViz

-------------------------------------------------------------------------
-- Helpers for withVizRequestContext
-------------------------------------------------------------------------

{- | Helper: Check that the client's verTxtDocId matches the server's.
  Note that the client will have already received
  the verTxtDocId in the original 'please render this VizExpr' request -}
checkVizVersion
  :: forall (method :: Symbol) params.
     (Ladder.CustomMethod method,
      LadderRequestParams params)
  => params
  -> Shake.RecentlyVisualised
  -> VizHandler method ()
checkVizVersion reqParams recentViz = do
  let vizConfig = Ladder.getVizConfig . (.vizState) $ recentViz
  if reqParams.verDocId == vizConfig.verTxtDocId
    then pure ()
    else throwError $ TResponseError
      { _code = InL LSPErrorCodes_ContentModified
      , _message = "Document version mismatch. Visualizer version: " <> Text.show (reqParams.verDocId)._version <>
          ", whereas server's version is: " <> Text.show vizConfig.verTxtDocId._version
      , _xdata = Nothing
      }
      -- TODO: Have the client update accordingly when it gets this error code,
      -- if it doesn't alr do so automatically

-- | Helper to handle JSON decoding errors when decoding params of custom requests
guardDecodeParams
  :: forall (method :: Symbol) a.
     (Ladder.CustomMethod method, LadderRequestParams a)
  => MessageParams ('Method_CustomMethod method)
  -> VizHandler method a
guardDecodeParams params =
  case Aeson.fromJSON params :: Aeson.Result a of
    Aeson.Error err ->
      throwError $ TResponseError
        { _code = InR ErrorCodes_InvalidRequest
        , _message = "Invalid params for " <> Ladder.getMethodName (Proxy @method) <> ": " <> Text.pack err
        , _xdata = Nothing
        }
    Aeson.Success decodedParams ->
      pure decodedParams
