{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}

module LSP.L4.Handlers where

import Control.Concurrent.Strict (Chan, writeChan)
import Control.Exception.Safe (MonadCatch, MonadMask, MonadThrow)
import Control.Lens ((^.))
import Control.Monad.Extra (guard, whenJust)
import qualified Control.Monad.Extra as Extra
import Control.Monad.Reader (MonadReader (..))
import Control.Monad.Trans.Reader (ReaderT)
import Data.Char (isAlphaNum)
import Data.Maybe (mapMaybe, listToMaybe)
import Data.Either (isRight)
import Data.Monoid (Ap (..))
import Data.Tuple (swap)
import UnliftIO (MonadUnliftIO, atomically, STM, MonadIO(..))
import Control.Applicative
import Control.Monad.Except (throwError, runExceptT, ExceptT)
import Control.Monad.Trans.Maybe
import qualified Control.Monad.Trans.Reader as ReaderT
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Text as Aeson
import qualified Text.Fuzzy as Fuzzy
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LazyText
import qualified HaskellWorks.Data.IntervalMap.FingerTree as IVMap
import qualified Data.Text.Mixed.Rope as Rope
import GHC.Generics
import LSP.Core.FileStore hiding (Log (..))
import qualified LSP.Core.FileStore as FileStore
import LSP.Core.OfInterest hiding (Log (..))
import LSP.Core.PositionMapping
import LSP.Core.Service hiding (Log (..))
import LSP.Core.Shake hiding (Log (..))
import qualified LSP.Core.Shake as Shake
import LSP.Core.Types.Diagnostics
import LSP.Core.Types.Location
import LSP.L4.Base
import LSP.L4.Config
import qualified LSP.L4.Viz.Ladder as Ladder
import LSP.L4.Rules hiding (Log (..))
import LSP.Logger
import LSP.L4.SemanticTokens (srcPosToPosition)
import qualified Language.LSP.Protocol.Lens as J
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types
import qualified Language.LSP.Protocol.Types as LSP
import Language.LSP.Protocol.Types as CompletionItem (CompletionItem (..))
import Language.LSP.Server hiding (notificationHandler, requestHandler)
import qualified Language.LSP.Server as LSP
import Language.LSP.VFS (VFS)
import qualified Optics
import qualified StmContainers.Map as STM

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
  | LogDecideMissingInformation
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
    LogDecideMissingInformation -> "Decide that we are visualising is missing type or source location information"
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
          doc :: Uri
          doc = params ^. J.textDocument . J.uri
          pos :: Position
          pos = params ^. J.position
        mloc <- gotoDefinition ide (LSP.toNormalizedUri doc) pos
        case mloc of
          Nothing  -> pure $ Right $ InR $ InR Null
          Just loc -> pure $ Right $ InL $ Definition (InL loc)
    , requestHandler SMethod_TextDocumentHover $ \ ide params -> do
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
                  = visualise recorder ide uri msrcPos

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
                  , _message = "Internal error, failed to produce semantic tokens for " <> Text.pack (show (show uri.getUri))
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
            Position ln col = params ^. J.position
        liftIO (runAction "completions" ide $ getUriContents $ toNormalizedUri uri) >>= \case
          Nothing -> pure (Right (InL []))
          Just rope -> do
            let completionPrefix =
                  Text.takeWhileEnd isAlphaNum
                  $ Rope.toText
                  $ fst -- we don't care for the rest of the line
                  $ Rope.charSplitAt (fromIntegral col)
                  $ Rope.getLine (fromIntegral ln) rope

            let filterMatchesOn f is =
                  map Fuzzy.original $ Fuzzy.filter
                    completionPrefix
                    is
                    mempty
                    mempty
                    f
                    False

            let mkKeyWordCompletionItem kw = (defaultCompletionItem kw)
                  { CompletionItem._kind =  Just CompletionItemKind_Keyword
                  }
                keyWordMatches = filterMatchesOn id $ Map.keys keywords
                -- FUTUREWORK(mangoiv): we could
                -- 1 pass through the token here
                -- 2 check the token category and if the category is COperator
                -- 3 set the CompletionItemKind to CompletionItemKind_Operator
                keywordItems = map mkKeyWordCompletionItem keyWordMatches

            (typeCheck, _positionMapping) <- liftIO $ runAction "typecheck" ide $
              useWithStale_ TypeCheck (toNormalizedUri uri)

            let topDeclItems
                  = filterMatchesOn CompletionItem._label
                  $ mapMaybe
                      (\(name, checkEntity) ->
                        topDeclToCompletionItem name
                        $ Optics.over'
                          (Optics.gplate @(Type' Resolved))
                          (applyFinalSubstitution typeCheck.substitution)
                          checkEntity
                      )
                      (combineEnvironmentEntityInfo
                        typeCheck.environment
                        typeCheck.entityInfo
                      )

            -- TODO: maybe we should sort these as follows
            -- 1 keywords
            -- 2 toplevel values
            -- 3 toplevel types

            let items = keywordItems <> topDeclItems

            logWith recorder Debug $ LogRequestedCompletionsFor completionPrefix

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
          visualizeDecides :: [CodeLens] = foldTopLevelDecides decideToCodeLens typeCheck.program

        pure (Right (InL visualizeDecides))
    , requestHandler SMethod_TextDocumentReferences $ \ide params -> do
        let doc :: Uri = params ^. J.textDocument . J.uri
            pos :: SrcPos = lspPositionToSrcPos $ params ^. J.position
            nfp :: NormalizedUri = toNormalizedUri doc

        refs <- liftIO $ runAction "getReferences" ide $
          use_ GetReferences nfp

        let locs = map (Location doc . srcRangeToLspRange . Just) $ lookupReference pos refs
        pure (Right (InL locs))
    ]

whenUriFile :: Uri -> (NormalizedFilePath -> IO ()) -> IO ()
whenUriFile uri act = whenJust (LSP.uriToFilePath uri) $ act . normalizeFilePath

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
-- Ladder visualisation
-- ----------------------------------------------------------------------------

visualise
  :: MonadIO m
  => Recorder (WithPriority Log)
  -> IdeState
  -> Uri
  -- ^ The document uri whose decides should be visualised
  -> Maybe (SrcPos, Bool)
  -- ^ The location of the `Decide` to visualize and whether or not to simplify it
  -> ExceptT (TResponseError method) m (Aeson.Value |? Null)
visualise recorder ide uri msrcPos = do
  let nfp = toNormalizedUri uri

  mdecide :: Maybe (Decide Resolved, Bool, Substitution) <- case msrcPos of
    -- the command was issued by the button in vscode or autorefresh
    -- NOTE: when we get the typecheck results via autorefresh, we can be lenient about it, i.e. we return 'Nothing
    -- exits by returning Nothing instead of throwing an error
    Nothing -> runMaybeT do
      tcRes <- MaybeT $ liftIO $ runAction "l4.visualize" ide $ use TypeCheck nfp
      recentlyVisualised <- MaybeT $ atomically $ getMostRecentVisualisation ide
      decide <- hoistMaybe $ (.getOne) $  foldTopLevelDecides (matchOnAvailableDecides recentlyVisualised) tcRes.program
      pure (decide, recentlyVisualised.simplify, tcRes.substitution)

    -- the command was issued by a code action or codelens
    Just (srcPos, simp) -> do
      tcRes <- do
        mTcResult <- liftIO $ runAction "l4.visualize" ide $ use TypeCheck nfp
        case mTcResult of
          Nothing -> defaultResponseError $ "Failed to typecheck " <> Text.pack (show uri.getUri) <> "."
          Just tcRes -> pure tcRes
      case foldTopLevelDecides (\d -> [d | decideNodeStartsAtPos srcPos d]) tcRes.program of
        [decide] -> pure $ Just (decide, simp, tcRes.substitution)
        -- NOTE: if this becomes a problem, we should use
        -- https://hackage.haskell.org/package/lsp-types-2.3.0.1/docs/Language-LSP-Protocol-Types.html#t:VersionedTextDocumentIdentifier
        _ -> defaultResponseError "The program was changed in the time between pressing the code lens and rendering the program"

  let recentlyVisualisedDecide (MkDecide Anno {range = Just range, extra = Extension {resolvedInfo = Just (TypeInfo ty)}} _tydec appform _expr) simplify substitution
        = Just RecentlyVisualised {pos = range.start, name = rawName $ getName appform, type' = applyFinalSubstitution substitution ty, simplify}
      recentlyVisualisedDecide _ _ _ = Nothing

  case mdecide of
    Nothing -> pure (InR Null)
    Just (decide, simp, substitution) -> case Ladder.doVisualize decide simp of
      Right vizProgramInfo -> do
        maybe
          (logWith recorder Warning LogDecideMissingInformation)
          (atomically . setMostRecentVisualisation ide)
          (recentlyVisualisedDecide decide simp substitution)
        pure $ InL $ Aeson.toJSON vizProgramInfo
      Left vizError ->
        defaultResponseError $ Text.unlines
          [ "Could not visualize:"
          , getUri uri
          , Ladder.prettyPrintVizError vizError
          ]
  where

    -- TODO: in the future we want to be a bit more clever wrt. which
    -- DECIDE/MEANS we snap to. We can use the type of the 'Decide' here
    -- (by requiring extra = Just ty) or the name of the 'Decide' by the means
    -- of checking its 'Resolved'
    matchOnAvailableDecides :: RecentlyVisualised -> Decide Resolved -> One (Decide Resolved)
    matchOnAvailableDecides v decide = One do
      guard (decideNodeStartsAtPos v.pos decide)
        <|> guard case decide of
               (MkDecide _ _ appform _) -> rawName (getName appform) == v.name
        -- NOTE: this heuristic is wrong if there are ambiguous names in scope. that's why it will
        -- only succeed if there's exactly one match

      pure decide

-- | the 'Monoid' 'Maybe' that returns the only occurrence of 'Just'
newtype One a = One {getOne :: Maybe a}
  deriving stock (Eq, Ord, Show)

instance Semigroup (One a) where
  One (Just a) <> One Nothing = One (Just a)
  One Nothing <> One (Just a) = One (Just a)
  _ <> _ = One Nothing

instance Monoid (One a) where
  mempty = One Nothing

defaultResponseError :: Monad m => Text -> ExceptT (TResponseError method) m a
defaultResponseError _message
  = throwError TResponseError { _code = InL LSPErrorCodes_RequestFailed , _xdata = Nothing, _message }

decideNodeStartsAtPos :: SrcPos -> Decide Resolved -> Bool
decideNodeStartsAtPos pos d = Just pos == do
  node <- rangeOfNode d
  pure node.start

-- ----------------------------------------------------------------------------
-- LSP Autocompletions
-- ----------------------------------------------------------------------------

topDeclToCompletionItem :: Name -> CheckEntity -> Maybe CompletionItem
topDeclToCompletionItem name = \case
  KnownTerm ty term ->
    Just (defaultTopDeclCompletionItem ty)
      { CompletionItem._kind = Just $ case (term, ty) of
         (Constructor, _) -> CompletionItemKind_Constructor
         (Selector, _) -> CompletionItemKind_Field
         (_, unrollForall -> Fun {}) -> CompletionItemKind_Function
         _ -> CompletionItemKind_Constant
      }
  KnownType kind _args tydec ->
    Just (defaultTopDeclCompletionItem (typeFunction kind))
      { CompletionItem._kind = Just $ case tydec of
          RecordDecl {} -> CompletionItemKind_Struct
          EnumDecl {} -> CompletionItemKind_Enum
          SynonymDecl {} -> CompletionItemKind_Reference
      }
  KnownTypeVariable {} -> Nothing
  where
    -- a function (but also a constant, in theory) can be polymorphic, so we have to strip
    -- all the foralls to get to the "actual" type.
    unrollForall :: Type' Resolved -> Type' Resolved
    unrollForall (Forall _ _ ty) = unrollForall ty
    unrollForall ty = ty

    -- a list : Type -> Type should be pretty printed as FUNCTION FROM TYPE TO TYPE
    typeFunction :: Kind -> Type' Resolved
    typeFunction 0 = Type emptyAnno
    typeFunction n | n > 0 = Fun emptyAnno (replicate n (MkOptionallyNamedType emptyAnno Nothing (Type emptyAnno))) (Type emptyAnno)
    typeFunction _ = error "Internal error: negative arity of type constructor"

    defaultTopDeclCompletionItem :: Type' Resolved -> CompletionItem
    defaultTopDeclCompletionItem ty = (defaultCompletionItem $ quoteIfNeeded prepared)
      { CompletionItem._filterText = Just prepared
      , CompletionItem._labelDetails
        = Just CompletionItemLabelDetails
          { _description = Nothing
          , _detail = Just $ " IS A " <> prettyLayout ty
          }
      }
      where
        prepared :: Text
        prepared = case name of MkName _ raw -> rawNameToText raw

defaultCompletionItem :: Text -> CompletionItem
defaultCompletionItem label = CompletionItem label
  Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
  Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- ----------------------------------------------------------------------------
-- LSP Go to Definition
-- ----------------------------------------------------------------------------

gotoDefinition :: IdeState -> NormalizedUri -> Position -> ServerM Config (Maybe Location)
gotoDefinition ide fileUri pos = do
  mTypeCheckedModule <- liftIO $ runAction "gotoDefinition" ide $
    useWithStale TypeCheck fileUri
  case mTypeCheckedModule of
    Nothing -> pure Nothing
    Just (m, positionMapping) -> do
      pure $ do
        oldPos <- fromCurrentPosition positionMapping pos
        range <- findDefinition (lspPositionToSrcPos oldPos) m.program
        let lspRange = srcRangeToLspRange (Just range)
        newRange <- toCurrentRange positionMapping lspRange
        pure (Location uri newRange)
  where
    uri = LSP.fromNormalizedUri fileUri

-- ----------------------------------------------------------------------------
-- LSP Hover
-- ----------------------------------------------------------------------------

findHover :: IdeState -> NormalizedUri -> Position -> ServerM Config (Maybe Hover)
findHover ide fileUri pos = runMaybeT $ refHover <|> typeHover
  where
  refHover = do
    refs <- MaybeT $ liftIO $ runAction "refHover" ide $
      use ResolveReferenceAnnotations fileUri
    hoistMaybe do
      -- NOTE: it's fine to cut of the tail here because we shouldn't ever get overlapping intervals
      let ivToRange (iv, (len, reference)) = (intervalToSrcRange len iv, reference)
      (range, mreference) <- listToMaybe $ ivToRange <$> IVMap.search (lspPositionToSrcPos pos) refs
      let lspRange = srcRangeToLspRange (Just range)
      pure $ Hover
        (InL
          (MarkupContent
            -- TODO: should be more descriptive
            { _value = Maybe.fromMaybe "Reference not found" mreference
            , _kind = MarkupKind_Markdown}
          )
        )
        (Just lspRange)

  typeHover = do
    (m, positionMapping) <- MaybeT $ liftIO $ runAction "typeHover" ide $
      useWithStale TypeCheck fileUri
    hoistMaybe do
      oldPos <- fromCurrentPosition positionMapping pos
      (range, i) <- findInfo (lspPositionToSrcPos oldPos) m.program
      let lspRange = srcRangeToLspRange (Just range)
      newLspRange <- toCurrentRange positionMapping lspRange
      pure (infoToHover m.substitution newLspRange i)

infoToHover :: Substitution -> Range -> Info -> Hover
infoToHover subst r i =
  Hover (InL (mkPlainText x)) (Just r)
  where
    x =
      case i of
        TypeInfo t  -> prettyLayout (applyFinalSubstitution subst t)
        KindInfo k  -> "arity " <> Text.pack (show k)
        KeywordInfo -> "keyword"

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
                  (MkAppForm emptyAnno name [])
                  (Just $ fmap getActual ty)
                )

            topDecls = foldTopDecls (: []) typeCheck.program

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
