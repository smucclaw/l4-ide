{-# LANGUAGE ViewPatterns #-}
module LSP.L4.Actions where

import Base
import qualified Base.Map as Map
import qualified Base.Text as Text

import Control.Applicative
import Control.Monad.Trans.Maybe
import qualified Data.Aeson as Aeson
import Data.Char (isAlphaNum)
import qualified Data.List as List
import Data.Ord (Down (..))
import Data.Text.Mixed.Rope (Rope)
import qualified Data.Text.Mixed.Rope as Rope
import qualified HaskellWorks.Data.IntervalMap.FingerTree as IVMap
import qualified Text.Fuzzy as Fuzzy

import L4.Annotation
import L4.Citations
import L4.FindDefinition
import L4.HoverInfo
import L4.Lexer (annotations, directives, keywords)
import L4.Nlg (simpleLinearizer)
import L4.Parser.SrcSpan
import L4.Print
import L4.Syntax
import L4.TypeCheck
import qualified L4.Evaluate.ValueLazy as EL
import qualified L4.EvaluateLazy       as EL
import LSP.Core.PositionMapping
import LSP.Core.Shake
import LSP.L4.Rules

import qualified LSP.L4.Viz.Ladder as Ladder
import qualified LSP.L4.Viz.VizExpr as Ladder
import qualified LSP.L4.Viz.CustomProtocol as Ladder
import           LSP.L4.Viz.CustomProtocol (EvalAppRequestParams (..),
                                            EvalAppResult (..))

import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types
import Language.LSP.Protocol.Types as CompletionItem (CompletionItem (..))

-- ----------------------------------------------------------------------------
-- LSP Autocompletions
-- ----------------------------------------------------------------------------

topDeclToCompletionItem :: Name -> CheckEntity -> Maybe CompletionItem
topDeclToCompletionItem name = \ case
  KnownTerm ty term ->
    Just (defaultTopDeclCompletionItem ty)
      { CompletionItem._kind = Just $ case (term, ty) of
         (Constructor, _) -> CompletionItemKind_Constructor
         (Selector, _) -> CompletionItemKind_Field
         (_, unrollForall -> Fun {}) -> CompletionItemKind_Function
         _ -> CompletionItemKind_Constant
      }
  KnownType kind _args _tydec ->
    Just (defaultTopDeclCompletionItem (typeFunction kind))
      { CompletionItem._kind = Just CompletionItemKind_Class
      }
  KnownSection (MkSection _ (Just n) _ _) ->
    Just (defaultCompletionItem $  nameToText $ getOriginal n)
      { CompletionItem._kind = Just CompletionItemKind_Module
      }
  KnownSection (MkSection _ Nothing _ _) -> Nothing
  KnownTypeVariable {} -> Nothing
  where
    -- a function (but also a constant, in theory) can be polymorphic, so we have to strip
    -- all the foralls to get to the "actual" type.
    unrollForall :: Type' Resolved -> Type' Resolved
    unrollForall (Forall _ _ ty) = unrollForall ty
    unrollForall ty = ty

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

gotoDefinition :: Position -> TypeCheckResult -> PositionMapping -> Maybe Location
gotoDefinition pos m positionMapping = do
  oldPos <- fromCurrentPosition positionMapping pos
  range <- findDefinition (lspPositionToSrcPos oldPos) m.module'
  let lspRange = srcRangeToLspRange (Just range)
  newRange <- toCurrentRange positionMapping lspRange
  pure (Location (fromNormalizedUri range.moduleUri) newRange)


-- ----------------------------------------------------------------------------
-- Ladder evalApp
-- ----------------------------------------------------------------------------

evalApp
  :: MonadIO m
  => TypeCheckResult
  -> Ladder.EvalAppRequestParams
  -> RecentlyVisualised
  -> EL.Environment
  -> ExceptT (TResponseError method) m Aeson.Value
evalApp tcRes evalParams recentViz evalEnv =
  case Ladder.lookupEvalAppMaker recentViz.vizState evalParams.appExpr of
    Nothing -> defaultResponseError "No eval app directive maker found" -- TODO: Improve error codehere
    Just evalAppMaker -> do
      let evalAppDirective = evalAppMaker evalParams
      (_, results) <- liftIO $ EL.execEvalModuleWithEnv evalEnv (evalAppDirective `prependToModule` tcRes.module')
      Aeson.toJSON <$> getEvalResult results
  where
    prependToModule :: TopDecl Resolved -> Module Resolved -> Module Resolved
    prependToModule newDecl (MkModule ann nuri (MkSection sann sresolved maka decls)) =
      MkModule ann nuri (MkSection sann sresolved maka (newDecl : decls))

    evalResultToLadderEvalAppResult :: Monad m => EL.EvalDirectiveResult -> ExceptT (TResponseError method) m EvalAppResult
    evalResultToLadderEvalAppResult (EL.MkEvalDirectiveResult _ res) = case res of
      Right (EL.MkNF val) -> 
        case val of
          EL.ValConstructor r [] -> EvalAppResult <$> toUBoolValue r
          _                      -> throwExpectBoolResultError
      Right EL.ToDeep    -> throwExpectBoolResultError
      Left err           -> defaultResponseError $ Text.unlines $ EL.prettyEvalException err

    throwExpectBoolResultError :: Monad m => ExceptT (TResponseError method) m a
    throwExpectBoolResultError = defaultResponseError "Ladder visualizer is expecting a boolean result (and it should be impossible to have got a fn with a non-bool return type in the first place)"

    toUBoolValue :: Monad m => Resolved -> ExceptT (TResponseError method) m Ladder.UBoolValue
    toUBoolValue resolved = case getUnique resolved of
      u | u == falseUnique -> pure Ladder.FalseV
        | u == trueUnique  -> pure Ladder.TrueV
        | otherwise        -> throwExpectBoolResultError

    -- | Assumes that the order of the eval results is the same as the order of the eval directives.
    getEvalResult :: Monad m => [EL.EvalDirectiveResult] -> ExceptT (TResponseError method) m EvalAppResult
    getEvalResult results = case results of
      (res : _xs) -> evalResultToLadderEvalAppResult res
      _           -> defaultResponseError "Internal error: No eval results found for some reason"

-- ----------------------------------------------------------------------------
-- Ladder visualisation
-- ----------------------------------------------------------------------------

visualise
  :: Monad m
  => Maybe TypeCheckResult
  -> (m (Maybe RecentlyVisualised), RecentlyVisualised -> m ())
  -> VersionedTextDocumentIdentifier
  -- ^ The VersionedTextDocumentIdentifier of the document whose Decides should be visualised
  -> Maybe (SrcPos, Bool)
  -- ^ The location of the `Decide` to visualize and whether or not to simplify it
  -> ExceptT (TResponseError method) m (Aeson.Value |? Null)
visualise mtcRes (getRecVis, setRecVis) verTextDocId msrcPos = do
  let uri = verTextDocId._uri

  -- Try to pinpoint a Decide (and VizConfig) based on how the command was issued (autorefresh vs code action/code lens)
  mdecide :: Maybe (Decide Resolved, Ladder.VizConfig) <- case msrcPos of
    -- a. the command was issued by the button in vscode or autorefresh
    -- NOTE: when we get the typecheck results via autorefresh, we can be lenient about it, i.e. we return 'Nothing
    -- exits by returning Nothing instead of throwing an error
    Nothing -> runMaybeT do
      tcRes <- hoistMaybe mtcRes
      recentlyVisualised <- MaybeT $ lift getRecVis
      decide <- hoistMaybe $ (.getOne) $  foldTopLevelDecides (matchOnAvailableDecides recentlyVisualised) tcRes.module'
      let updatedVizConfig = updateVizConfig verTextDocId tcRes recentlyVisualised
      pure (decide, updatedVizConfig)

    -- b. the command was issued by a code action or codelens
    Just (srcPos, simp) -> do
      tcRes <- do
        case mtcRes of
          Nothing -> defaultResponseError $ "Failed to typecheck " <> Text.pack (show uri.getUri) <> "."
          Just tcRes -> pure tcRes
      case foldTopLevelDecides (\d -> [d | decideNodeStartsAtPos srcPos d]) tcRes.module' of
        [decide] ->
          let vizConfig = Ladder.mkVizConfig verTextDocId tcRes.substitution simp
          in pure $ Just (decide, vizConfig)
        -- NOTE: if this becomes a problem, we should use
        -- https://hackage.haskell.org/package/lsp-types-2.3.0.1/docs/Language-LSP-Protocol-Types.html#t:VersionedTextDocumentIdentifier
        _ -> defaultResponseError "The program was changed in the time between pressing the code lens and rendering the program"

  -- Makes a 'RecentlyVisualised' iff the given 'Decide' has a valid range and a resolved type.
  -- Assumes the vizConfig in the given vizState is up-to-date.
  let recentlyVisualisedDecide (MkDecide Anno {range = Just range, extra = Extension {resolvedInfo = Just (TypeInfo ty _)}} _tydec appform _expr) vizState
        = Just RecentlyVisualised
          { pos = range.start
          , name = rawName $ getName appform
          , type' = applyFinalSubstitution (Ladder.getVizConfig vizState).substitution (Ladder.getVizConfig vizState).moduleUri ty
          , vizState = vizState
          }
      recentlyVisualisedDecide _ _ = Nothing

  case mdecide of
    Nothing -> pure (InR Null)
    Just (decide, vizConfig) ->
      case Ladder.doVisualize decide vizConfig of
        Right (vizProgramInfo, vizState) -> do
          traverse_ (lift . setRecVis) $ recentlyVisualisedDecide decide vizState
          pure $ InL $ Aeson.toJSON vizProgramInfo
        Left vizError ->
          defaultResponseError $ Text.unlines
            [ "Could not visualize:"
            , getUri uri
            , Ladder.prettyPrintVizError vizError
            ]
  where
    {- | Make a new VizConfig by combining (i) old config (e.g. whether to simplify) from the RecentlyVisualized (which itself contains a VizConfig)
    with (ii) up-to-date versions of potentially stale info (verTxtDocId, tcRes) -}
    updateVizConfig :: VersionedTextDocumentIdentifier -> TypeCheckResult -> RecentlyVisualised -> Ladder.VizConfig
    updateVizConfig verTxtDocId tcRes recentlyVisualised = 
      Ladder.getVizConfig recentlyVisualised.vizState
        & set #verTxtDocId verTxtDocId
        & set #moduleUri (toNormalizedUri verTxtDocId._uri)
        & set #substitution tcRes.substitution

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
-- LSP Code Actions
-- ----------------------------------------------------------------------------

completions :: Rope -> NormalizedUri -> TypeCheckResult -> Position -> [CompletionItem]
completions rope nuri typeCheck (Position ln col) = do
  let completionPrefix =
        Text.takeWhileEnd isAlphaNum
        $ Rope.toText
        $ fst -- we don't care for the rest of the line
        $ Rope.charSplitAt (fromIntegral col)
        $ Rope.getLine (fromIntegral ln) rope

      filterMatchesOn f is =
        map Fuzzy.original $ Fuzzy.filter
          completionPrefix
          is
          mempty
          mempty
          f
          False

      mkKeyWordCompletionItem kw = (defaultCompletionItem kw)
        { CompletionItem._kind =  Just CompletionItemKind_Keyword
        }
      keyWordMatches = filterMatchesOn id
        $ Map.keys keywords
        <> annotations
        <> map snd directives
      -- FUTUREWORK(mangoiv): we could
      -- 1 pass through the token here
      -- 2 check the token category and if the category is COperator
      -- 3 set the CompletionItemKind to CompletionItemKind_Operator
      keywordItems = map mkKeyWordCompletionItem keyWordMatches

      topDeclItems
        = filterMatchesOn CompletionItem._label
        $ mapMaybe
            (\(name, checkEntity) ->
              topDeclToCompletionItem name
              $ applyFinalSubstitution typeCheck.substitution nuri checkEntity
            )
            (combineEnvironmentEntityInfo
              typeCheck.environment
              typeCheck.entityInfo
            )

  -- TODO: maybe we should sort these as follows
  -- 1 keywords
  -- 2 toplevel values
  -- 3 toplevel types

  keywordItems <> topDeclItems

-- ----------------------------------------------------------------------------
-- LSP Hovers
-- ----------------------------------------------------------------------------

referenceHover :: Position -> IVMap.IntervalMap SrcPos (NormalizedUri, Int, Maybe Text) -> Maybe Hover
referenceHover pos refs = do
  -- NOTE: it's fine to cut of the tail here because we shouldn't ever get overlapping intervals
  let ivToRange (iv, (uri, len, reference)) = (intervalToSrcRange uri len iv, reference)
  -- NOTE: this is subtle: if there are multiple results for a location, then we want to
  -- prefer Just's, so we reverse sort the references we get.
  -- Squashing on snd also wouldn't make sense because if we'd had all 'Nothing' that would
  -- mean that we'd get no result, when actually we want to have a list with a single element
  -- that is Nothing, on that range.
  (range, mreference) <- listToMaybe
    $ List.sortOn (Down . snd)
    $ ivToRange
    <$> IVMap.search (lspPositionToSrcPos pos) refs
  let lspRange = srcRangeToLspRange (Just range)
  pure $ Hover
    (InL
      (MarkupContent
        -- TODO: should be more descriptive
        { _value = fromMaybe "Reference not found" mreference
        , _kind = MarkupKind_Markdown}
      )
    )
    (Just lspRange)

typeHover :: Position -> NormalizedUri -> TypeCheckResult -> PositionMapping -> Maybe Hover
typeHover pos nuri tcRes positionMapping = do
  oldPos <- fromCurrentPosition positionMapping pos
  (range, i) <- findInfo (lspPositionToSrcPos oldPos) tcRes.module'
  let lspRange = srcRangeToLspRange (Just range)
  newLspRange <- toCurrentRange positionMapping lspRange
  pure (infoToHover nuri tcRes.substitution newLspRange i)

infoToHover :: NormalizedUri -> Substitution -> Range -> Info -> Hover
infoToHover nuri subst r i =
  Hover (InL (mkMarkdown x)) (Just r)
  where
    x =
      case i of
        TypeInfo t mNlg -> mdCodeBlock (prettyLayout (applyFinalSubstitution subst nuri t)) <>
          case mNlg of
            Nothing -> mempty
            Just nlg -> mdSeparator <> mdCodeBlock (simpleLinearizer nlg)

        KindInfo k      -> mdCodeBlock $ prettyLayout $ typeFunction k
        KeywordInfo     -> mdCodeBlock "keyword"

mdCodeBlock :: Text -> Text
mdCodeBlock c =
  Text.unlines
    [ "```"
    , c
    , "```"
    ]

mdSeparator :: Text
mdSeparator = "\n---\n\n"

-- ----------------------------------------------------------------------------
-- Common utility functions
-- ----------------------------------------------------------------------------

-- a list : Type -> Type should be pretty printed as FUNCTION FROM TYPE TO TYPE
typeFunction :: Kind -> Type' Resolved
typeFunction 0 = Type emptyAnno
typeFunction n | n > 0 = Fun emptyAnno (replicate n (MkOptionallyNamedType emptyAnno Nothing (Type emptyAnno))) (Type emptyAnno)
typeFunction _ = error "Internal error: negative arity of type constructor"
