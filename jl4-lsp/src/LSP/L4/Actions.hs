{-# LANGUAGE ViewPatterns, DataKinds #-}
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
import qualified Text.Fuzzy as Fuzzy

import L4.Annotation
import L4.FindDefinition
import L4.Lexer (annotations, directives, keywords)
import L4.Parser.SrcSpan
import L4.Print
import L4.Syntax
import L4.TypeCheck
import qualified L4.Evaluate.ValueLazy   as EL
import qualified L4.EvaluateLazy         as EL
import qualified L4.EvaluateLazy.Machine as EL
import qualified L4.Utils.IntervalMap as IV
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
import L4.Nlg (simpleLinearizer)

-- ----------------------------------------------------------------------------
-- LSP Autocompletions
-- ----------------------------------------------------------------------------

buildCompletionItem :: RawName -> CheckEntity -> [CompletionItem]
buildCompletionItem raw = \ case
  KnownTerm ty term ->
    pure (defaultTopDeclCompletionItem ty)
      { CompletionItem._kind = Just $ case (term, ty) of
         (Constructor, _) -> CompletionItemKind_Constructor
         (Selector, _) -> CompletionItemKind_Field
         (_, unrollForall -> Fun {}) -> CompletionItemKind_Function
         _ -> CompletionItemKind_Constant
      }
  KnownType kind _args _tydec ->
    pure (defaultTopDeclCompletionItem (typeFunction kind))
      { CompletionItem._kind = Just CompletionItemKind_Class
      }
  KnownSection (MkSection _ (Just n) _ _) ->
    pure (defaultCompletionItem $  nameToText $ getOriginal n)
      { CompletionItem._kind = Just CompletionItemKind_Module
      }
  KnownSection (MkSection _ Nothing _ _) ->
    -- NOTE: a section without name is just the toplevel section - we don't need to
    -- autocomplete anything there
    []
  KnownTypeVariable ->
    pure (defaultCompletionItem prepared)
     { CompletionItem._kind = Just CompletionItemKind_TypeParameter
     }
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

    prepared :: Text
    prepared = rawNameToText raw

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
  :: forall m.
  (MonadIO m)
  => EL.EntityInfo
  -> (EL.Environment, Module Resolved)
  -> Ladder.EvalAppRequestParams
  -> RecentlyVisualised
  -> ExceptT (TResponseError ('Method_CustomMethod Ladder.EvalAppMethodName)) m Aeson.Value
evalApp entityInfo contextModule evalParams recentViz =
  case Ladder.lookupAppExprMaker recentViz.vizState evalParams.appExpr of
    Nothing -> defaultResponseError "No expr maker found" -- TODO: Improve error codehere
    Just evalAppMaker -> do
      let appExpr = evalAppMaker evalParams
      res <- liftIO $ EL.execEvalExprInContextOfModule entityInfo appExpr contextModule
      case res of
        Just evalRes -> Aeson.toJSON <$> evalResultToLadderEvalAppResult evalRes
        Nothing -> defaultResponseError "No eval result found"
  where
    evalResultToLadderEvalAppResult :: EL.EvalDirectiveResult -> ExceptT (TResponseError method) m EvalAppResult
    evalResultToLadderEvalAppResult (EL.MkEvalDirectiveResult _ res _mtrace) = case res of
      EL.Assertion True  -> pure $ EvalAppResult (toUBoolValue True)
      EL.Assertion False -> pure $ EvalAppResult (toUBoolValue False)
      EL.Reduction v ->
        case v of
          Right (EL.MkNF val) ->
            case EL.boolView val of
              Just b  -> pure $ EvalAppResult (toUBoolValue b)
              Nothing -> throwExpectBoolResultError
          Right EL.Omitted   -> defaultResponseError "Evaluation exceeded maximum depth limit"
          Left err           -> defaultResponseError $ Text.unlines $ EL.prettyEvalException err

    throwExpectBoolResultError :: ExceptT (TResponseError method) m a
    throwExpectBoolResultError = defaultResponseError "Ladder visualizer is expecting a boolean result (and it should be impossible to have got a fn with a non-bool return type in the first place)"

    toUBoolValue :: Bool -> Ladder.UBoolValue
    toUBoolValue b = if b then Ladder.TrueV else Ladder.FalseV

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
    -- a. the command was issued by autorefresh
    -- NOTE: when we get the typecheck results via autorefresh, we can be lenient about it, i.e. we return 'Nothing
    -- exits by returning Nothing instead of throwing an error
    Nothing -> runMaybeT do
      tcRes <- hoistMaybe mtcRes
      recentlyVisualised <- MaybeT $ lift getRecVis
      -- Since this is from autorefresh, we want to get the most up-to-date version of the Decide
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
          let vizConfig = Ladder.mkVizConfig verTextDocId tcRes.module' tcRes.substitution simp
          in pure $ Just (decide, vizConfig)
        -- NOTE: if this becomes a problem, we should use
        -- https://hackage.haskell.org/package/lsp-types-2.3.0.1/docs/Language-LSP-Protocol-Types.html#t:VersionedTextDocumentIdentifier
        _ -> defaultResponseError "The program was changed in the time between pressing the code lens and rendering the program"

  -- Makes a 'RecentlyVisualised' iff the given 'Decide' has a valid range and a resolved type.
  -- Assumes the vizConfig in the given vizState is up-to-date.
  let recentlyVisualisedDecide decide@(MkDecide Anno {range = Just range, extra = Extension {resolvedInfo = Just (TypeInfo ty _)}} _tydec appform _expr) vizState
        = Just RecentlyVisualised
          { pos = range.start
          , name = rawName $ getName appform
          , type' = applyFinalSubstitution (Ladder.getVizConfig vizState).substitution (Ladder.getVizConfig vizState).moduleUri ty
          , vizState = vizState
          , decide
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
completions rope nuri typeCheck pos@(Position ln col) = do
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
        <> Map.keys directives
        <> annotations

      keywordItems = map mkKeyWordCompletionItem keyWordMatches

      -- NOTE: combine toplevel check info and info brought in scope
      finalCheckInfos
        = Map.unionsWith (\a b -> nub $ a <> b)
        $ map (uncurry combineEnvironmentEntityInfo)
        $ (typeCheck.environment, typeCheck.entityInfo)
            : map snd (IV.search (lspPositionToSrcPos pos) typeCheck.scopeMap)

      scopedItems
        = filterMatchesOn CompletionItem._label
        $ foldMap
            (\(name, ces) ->
              foldMap
                (buildCompletionItem name . applyFinalSubstitution typeCheck.substitution nuri)
                ces
            )
        $ Map.toList finalCheckInfos

  keywordItems <> scopedItems

-- ----------------------------------------------------------------------------
-- LSP Hovers
-- ----------------------------------------------------------------------------

referenceHover :: Position -> IV.IntervalMap SrcPos (NormalizedUri, Int, Maybe Text) -> Maybe Hover
referenceHover pos refs = do
  -- NOTE: it's fine to cut of the tail here because we shouldn't ever get overlapping intervals
  let ivToRange (iv, (uri, len, reference)) = (IV.intervalToSrcRange uri len iv, reference)
  -- NOTE: this is subtle: if there are multiple results for a location, then we want to
  -- prefer Just's, so we reverse sort the references we get.
  -- Squashing on snd also wouldn't make sense because if we'd had all 'Nothing' that would
  -- mean that we'd get no result, when actually we want to have a list with a single element
  -- that is Nothing, on that range.
  (range, mreference) <- listToMaybe
    $ List.sortOn (Down . snd)
    $ ivToRange
    <$> IV.search (lspPositionToSrcPos pos) refs
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
  let oldLspPos = lspPositionToSrcPos oldPos
  (range, i) <- IV.smallestContaining nuri oldLspPos tcRes.infoMap
  let mNlg = IV.smallestContaining nuri oldLspPos tcRes.nlgMap
  let lspRange = srcRangeToLspRange (Just range)
  newLspRange <- toCurrentRange positionMapping lspRange
  pure (infoToHover nuri tcRes.substitution newLspRange i (fmap snd mNlg))

infoToHover :: NormalizedUri -> Substitution -> Range -> Info -> Maybe Nlg -> Hover
infoToHover nuri subst r i mNlg =
  Hover (InL (mkMarkdown x)) (Just r)
  where
    x =
      case i of
        TypeInfo t _ ->
          mdCodeBlock (prettyLayout (applyFinalSubstitution subst nuri t)) <>
            case mNlg of
              Nothing -> mempty
              Just nlg -> mdSeparator <> mdCodeBlock (simpleLinearizer nlg)
        KindInfo k -> mdCodeBlock $ prettyLayout $ typeFunction k
        TypeVariable -> "TYPE VAR"

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
