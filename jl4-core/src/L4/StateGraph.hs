{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
-- | State Graph extraction and visualization for L4 regulative rules
--
-- This module extracts state transition graphs from L4 regulative rules
-- (obligations with HENCE/LEST chains) and renders them as GraphViz DOT.
--
-- Inspired by Flood & Goodenough's "Contract as Automaton" model.
--
-- The extraction follows these mappings:
--   - Each obligation chain creates states and transitions
--   - PARTY X MUST/MAY action → transition label
--   - HENCE → success transition (green)
--   - LEST → failure/timeout transition (red, dashed)
--   - Fulfilled → terminal success state
--   - Breach → terminal failure state
--   - WITHIN deadline → temporal guard on transition
module L4.StateGraph
  ( -- * Types
    StateGraph(..)
  , ContractState(..)
  , StateId
  , StateType(..)
  , Transition(..)
  , TransitionLabel(..)
  , TransitionType(..)
    -- * Options
  , StateGraphOptions(..)
  , defaultStateGraphOptions
    -- * Extraction
  , extractStateGraph
  , extractStateGraphs
    -- * Rendering
  , stateGraphToDot
  ) where

import Base
import qualified Base.Text as Text
import qualified Data.Text.Lazy as Text.Lazy
import qualified Control.Monad.State.Strict as St

import L4.Syntax
  ( Expr(..)
  , Resolved
  , Module(..)
  , Section(..)
  , TopDecl(..)
  , Decide(..)
  , Deonton(..)
  , RAction(..)
  , DeonticModal(..)
  , Pattern(..)
  , AppForm(..)
  , nameToText
  , getOriginal
  )
import L4.Print (prettyLayout)

-- GraphViz imports
import qualified Data.GraphViz as GV
import qualified Data.GraphViz.Attributes.Complete as GV
import Data.Graph.Inductive.Graph (Node, LNode, LEdge)
import qualified Data.Graph.Inductive.Graph as FGL
import qualified Data.Graph.Inductive.PatriciaTree as FGL

-- | Convert a Resolved name to Text
resolvedToText :: Resolved -> Text
resolvedToText = nameToText . getOriginal

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- | Unique identifier for states in the graph
type StateId = Int

-- | A state in the contract automaton
data ContractState = ContractState
  { stateId   :: StateId
  , stateName :: Text      -- ^ Human-readable name (e.g., "purchase template", "Fulfilled")
  , stateType :: StateType
  } deriving (Eq, Show)

-- | Classification of states for rendering
data StateType
  = InitialState           -- ^ Entry point of the contract
  | IntermediateState      -- ^ Normal intermediate state
  | TerminalFulfilled      -- ^ Success terminal (double circle, green)
  | TerminalBreach         -- ^ Failure terminal (double circle, red)
  deriving (Eq, Show)

-- | A transition between states
data Transition = Transition
  { transFrom  :: StateId
  , transTo    :: StateId
  , transLabel :: TransitionLabel
  , transType  :: TransitionType
  } deriving (Eq, Show)

-- | Label information for a transition
data TransitionLabel = TransitionLabel
  { labelParty    :: Maybe Text    -- ^ Party responsible (e.g., "buyer", "seller")
  , labelModal    :: Maybe DeonticModal  -- ^ Deontic modal (MUST, MAY, etc.)
  , labelAction   :: Text          -- ^ Action description
  , labelDeadline :: Maybe Text    -- ^ Temporal constraint (e.g., "30 days")
  , labelGuard    :: Maybe Text    -- ^ PROVIDED condition
  } deriving (Eq, Show)

-- | Classification of transitions for rendering
data TransitionType
  = HenceTransition        -- ^ Success path (solid, green)
  | LestTransition         -- ^ Failure/timeout path (dashed, red)
  | DefaultTransition      -- ^ Neutral transition
  deriving (Eq, Show)

-- | The complete state graph for a contract
data StateGraph = StateGraph
  { sgName         :: Text           -- ^ Name of the contract/rule
  , sgStates       :: [ContractState]
  , sgTransitions  :: [Transition]
  , sgInitialState :: StateId
  } deriving (Eq, Show)

-- | Options for state graph rendering
data StateGraphOptions = StateGraphOptions
  { showDeadlines   :: Bool   -- ^ Include temporal constraints on edges
  , showGuards      :: Bool   -- ^ Include PROVIDED conditions
  , compactLabels   :: Bool   -- ^ Use shorter labels
  , showModal       :: Bool   -- ^ Show deontic modal in labels
  } deriving (Eq, Show)

-- | Default options with all information shown
defaultStateGraphOptions :: StateGraphOptions
defaultStateGraphOptions = StateGraphOptions
  { showDeadlines = True
  , showGuards    = True
  , compactLabels = False
  , showModal     = True
  }

--------------------------------------------------------------------------------
-- Extraction State Monad
--------------------------------------------------------------------------------

-- | State for graph extraction
data ExtractState = ExtractState
  { esNextId      :: StateId
  , esStates      :: [ContractState]
  , esTransitions :: [Transition]
  } deriving (Show)

type ExtractM = St.State ExtractState

-- | Create a new state and return its ID
newState :: Text -> StateType -> ExtractM StateId
newState name stype = do
  st <- St.get
  let sid = st.esNextId
      s = ContractState sid name stype
  St.put st { esNextId = sid + 1, esStates = s : st.esStates }
  pure sid

-- | Add a transition
addTransition :: StateId -> StateId -> TransitionLabel -> TransitionType -> ExtractM ()
addTransition fromId toId label ttype = St.modify $ \st ->
  st { esTransitions = Transition fromId toId label ttype : st.esTransitions }

-- | Find or create a terminal state
getTerminalState :: Text -> StateType -> ExtractM StateId
getTerminalState name stype = do
  st <- St.get
  case find (\s -> s.stateName == name && s.stateType == stype) st.esStates of
    Just s  -> pure s.stateId
    Nothing -> newState name stype

--------------------------------------------------------------------------------
-- AST Extraction
--------------------------------------------------------------------------------

-- | Extract all state graphs from a module
extractStateGraphs :: Module Resolved -> [StateGraph]
extractStateGraphs (MkModule _ _ section) =
  extractFromSection section

-- | Extract the first state graph from a module (convenience function)
extractStateGraph :: Module Resolved -> Maybe StateGraph
extractStateGraph m = case extractStateGraphs m of
  (sg:_) -> Just sg
  []     -> Nothing

-- | Extract state graphs from a section
extractFromSection :: Section Resolved -> [StateGraph]
extractFromSection (MkSection _ mName _ decls) =
  let sectionName = maybe "contract" resolvedToText mName
  in concatMap (extractFromTopDecl sectionName) decls

-- | Extract from a top-level declaration
extractFromTopDecl :: Text -> TopDecl Resolved -> [StateGraph]
extractFromTopDecl _contextName = \case
  Decide _ (MkDecide _ _ (MkAppForm _ name _ _) body) ->
    case findRegulativeExpr body of
      Just regExpr -> [runExtraction (resolvedToText name) regExpr]
      Nothing      -> []
  Section _ sec -> extractFromSection sec
  _ -> []

-- | Find a regulative expression in an expression tree
findRegulativeExpr :: Expr Resolved -> Maybe (Expr Resolved)
findRegulativeExpr expr = case expr of
  Regulative{} -> Just expr
  RAnd{}       -> Just expr
  ROr{}        -> Just expr
  Where _ e _  -> findRegulativeExpr e
  LetIn _ _ e  -> findRegulativeExpr e
  _            -> Nothing

-- | Run the extraction monad and build a StateGraph
runExtraction :: Text -> Expr Resolved -> StateGraph
runExtraction name expr =
  let initialState = ExtractState
        { esNextId = 0
        , esStates = []
        , esTransitions = []
        }
      finalState = St.execState (extractExpr Nothing expr) initialState
  in StateGraph
       { sgName = name
       , sgStates = reverse finalState.esStates
       , sgTransitions = reverse finalState.esTransitions
       , sgInitialState = 0  -- First created state is initial
       }

-- | Extract states and transitions from an expression
-- The Maybe StateId is the "current" state we're transitioning from
extractExpr :: Maybe StateId -> Expr Resolved -> ExtractM ()
extractExpr mFromState expr = case expr of
  Regulative _ obl -> extractDeonton mFromState obl

  RAnd _ e1 e2 -> do
    -- Parallel composition: both obligations must be fulfilled
    extractExpr mFromState e1
    extractExpr mFromState e2

  ROr _ e1 e2 -> do
    -- Choice: either obligation can be fulfilled
    extractExpr mFromState e1
    extractExpr mFromState e2

  Where _ e _ -> extractExpr mFromState e
  LetIn _ _ e -> extractExpr mFromState e

  -- Terminal states referenced by name
  App _ name [] | isFulfilled name -> do
    _ <- getTerminalState "Fulfilled" TerminalFulfilled
    pure ()

  Breach _ _ _ -> do
    _ <- getTerminalState "Breach" TerminalBreach
    pure ()

  _ -> pure ()  -- Skip other expressions

-- | Check if a name refers to Fulfilled
isFulfilled :: Resolved -> Bool
isFulfilled name = resolvedToText name == "Fulfilled"

-- | Extract an obligation as a state transition
extractDeonton :: Maybe StateId -> Deonton Resolved -> ExtractM ()
extractDeonton mFromState MkDeonton{..} = do
  -- Create or get the source state
  fromState <- case mFromState of
    Just sid -> pure sid
    Nothing  -> newState "initial" InitialState

  -- Build the transition label
  let partyText = Just $ prettyLayout party
      modalVal  = Just (action.modal)
      actionText = prettyPattern action.action
      deadlineText = fmap prettyLayout due
      guardText = fmap prettyLayout action.provided

      label = TransitionLabel
        { labelParty    = partyText
        , labelModal    = modalVal
        , labelAction   = actionText
        , labelDeadline = deadlineText
        , labelGuard    = guardText
        }

  -- Handle HENCE (success path)
  case hence of
    Just henceExpr -> do
      case classifyTarget henceExpr of
        TargetFulfilled -> do
          fulfilledId <- getTerminalState "Fulfilled" TerminalFulfilled
          addTransition fromState fulfilledId label HenceTransition

        TargetBreach -> do
          breachId <- getTerminalState "Breach" TerminalBreach
          addTransition fromState breachId label HenceTransition

        TargetDeonton nextObl -> do
          -- Create intermediate state for the next obligation
          let nextStateName = describeDeonton nextObl
          nextStateId <- newState nextStateName IntermediateState
          addTransition fromState nextStateId label HenceTransition
          -- Recursively extract the next obligation
          extractDeonton (Just nextStateId) nextObl

        TargetOther -> do
          -- Unknown target - create generic next state
          nextStateId <- newState "next" IntermediateState
          addTransition fromState nextStateId label HenceTransition
          extractExpr (Just nextStateId) henceExpr

    Nothing -> do
      -- No HENCE specified - use default based on modal
      case action.modal of
        DMay -> do
          -- MAY without HENCE defaults to Fulfilled
          fulfilledId <- getTerminalState "Fulfilled" TerminalFulfilled
          addTransition fromState fulfilledId label HenceTransition
        _ -> do
          -- MUST/DO without HENCE defaults to Fulfilled
          fulfilledId <- getTerminalState "Fulfilled" TerminalFulfilled
          addTransition fromState fulfilledId label HenceTransition

  -- Handle LEST (failure/timeout path)
  case lest of
    Just lestExpr -> do
      case classifyTarget lestExpr of
        TargetFulfilled -> do
          fulfilledId <- getTerminalState "Fulfilled" TerminalFulfilled
          let timeoutLabel = TransitionLabel Nothing Nothing "timeout" Nothing Nothing
          addTransition fromState fulfilledId timeoutLabel LestTransition

        TargetBreach -> do
          breachId <- getTerminalState "Breach" TerminalBreach
          let timeoutLabel = TransitionLabel Nothing Nothing "timeout" Nothing Nothing
          addTransition fromState breachId timeoutLabel LestTransition

        TargetDeonton nextObl -> do
          let nextStateName = describeDeonton nextObl
          nextStateId <- newState nextStateName IntermediateState
          let timeoutLabel = TransitionLabel Nothing Nothing "timeout" Nothing Nothing
          addTransition fromState nextStateId timeoutLabel LestTransition
          extractDeonton (Just nextStateId) nextObl

        TargetOther -> do
          nextStateId <- newState "failure" IntermediateState
          let timeoutLabel = TransitionLabel Nothing Nothing "timeout" Nothing Nothing
          addTransition fromState nextStateId timeoutLabel LestTransition
          extractExpr (Just nextStateId) lestExpr

    Nothing -> do
      -- No LEST specified - use default based on modal
      case action.modal of
        DMay -> pure ()  -- MAY without LEST: no failure path needed
        DMust -> do
          -- MUST without LEST defaults to Breach
          breachId <- getTerminalState "Breach" TerminalBreach
          let timeoutLabel = TransitionLabel Nothing Nothing "timeout" Nothing Nothing
          addTransition fromState breachId timeoutLabel LestTransition
        DMustNot -> do
          -- SHANT without LEST defaults to Breach (if action IS done)
          breachId <- getTerminalState "Breach" TerminalBreach
          let violationLabel = TransitionLabel Nothing Nothing "violation" Nothing Nothing
          addTransition fromState breachId violationLabel LestTransition
        DDo -> pure ()  -- DO requires explicit HENCE/LEST

-- | Classification of HENCE/LEST targets
data Target
  = TargetFulfilled
  | TargetBreach
  | TargetDeonton (Deonton Resolved)
  | TargetOther

-- | Classify what a HENCE/LEST expression points to
classifyTarget :: Expr Resolved -> Target
classifyTarget = \case
  App _ name [] | resolvedToText name == "Fulfilled" -> TargetFulfilled
  Breach{} -> TargetBreach
  Regulative _ obl -> TargetDeonton obl
  Where _ e _ -> classifyTarget e
  LetIn _ _ e -> classifyTarget e
  _ -> TargetOther

-- | Generate a descriptive name for an obligation (for intermediate states)
describeDeonton :: Deonton Resolved -> Text
describeDeonton MkDeonton{..} =
  let partyT = prettyLayout party
      modalT = case action.modal of
        DMust    -> "must"
        DMay     -> "may"
        DMustNot -> "must not"
        DDo      -> "do"
      actionT = prettyPattern action.action
  in partyT <> " " <> modalT <> " " <> actionT

-- | Pretty-print a pattern to text
prettyPattern :: Pattern Resolved -> Text
prettyPattern = \case
  PatVar _ n      -> resolvedToText n
  PatApp _ n args -> resolvedToText n <> if null args then "" else " ..."
  PatCons _ h _   -> prettyPattern h <> " ..."
  PatExpr _ e     -> prettyLayout e
  PatLit _ lit    -> prettyLayout lit

--------------------------------------------------------------------------------
-- GraphViz DOT Generation
--------------------------------------------------------------------------------

-- | Node attributes for FGL graph
data NodeAttrs = NodeAttrs
  { naLabel     :: Text
  , naFillColor :: Text
  , naShape     :: Text
  , naStyle     :: Text
  } deriving (Eq, Show)

-- | Edge attributes for FGL graph
data EdgeAttrs = EdgeAttrs
  { eaLabel :: Text
  , eaColor :: Text
  , eaStyle :: Text
  } deriving (Eq, Show)

type ContractGraph = FGL.Gr NodeAttrs EdgeAttrs

-- | Convert a StateGraph to GraphViz DOT format
stateGraphToDot :: StateGraphOptions -> StateGraph -> Text
stateGraphToDot opts sg =
  let graph = buildFGLGraph opts sg
      dotGraph = graphToDot opts sg graph
  in Text.Lazy.toStrict $ GV.printDotGraph dotGraph

-- | Build an FGL graph from a StateGraph
buildFGLGraph :: StateGraphOptions -> StateGraph -> ContractGraph
buildFGLGraph opts StateGraph{..} =
  let nodes = map (stateToNode opts) sgStates
      edges = map (transitionToEdge opts) sgTransitions
  in FGL.mkGraph nodes edges

-- | Convert a ContractState to an FGL node
stateToNode :: StateGraphOptions -> ContractState -> LNode NodeAttrs
stateToNode _ ContractState{..} =
  let (fillColor, shape, style) = case stateType of
        InitialState       -> ("#e8f4fd", "ellipse", "filled")
        IntermediateState  -> ("#ffffff", "ellipse", "filled")
        TerminalFulfilled  -> ("#d4edda", "doublecircle", "filled")
        TerminalBreach     -> ("#f8d7da", "doublecircle", "filled")
      attrs = NodeAttrs
        { naLabel     = stateName
        , naFillColor = fillColor
        , naShape     = shape
        , naStyle     = style
        }
  in (stateId, attrs)

-- | Convert a Transition to an FGL edge
transitionToEdge :: StateGraphOptions -> Transition -> LEdge EdgeAttrs
transitionToEdge opts Transition{..} =
  let label = formatTransitionLabel opts transLabel
      (color, style) = case transType of
        HenceTransition   -> ("#28a745", "solid")      -- Green for success
        LestTransition    -> ("#dc3545", "dashed")     -- Red dashed for failure
        DefaultTransition -> ("#6c757d", "solid")      -- Gray for neutral
      attrs = EdgeAttrs
        { eaLabel = label
        , eaColor = color
        , eaStyle = style
        }
  in (transFrom, transTo, attrs)

-- | Format a transition label for display
formatTransitionLabel :: StateGraphOptions -> TransitionLabel -> Text
formatTransitionLabel opts TransitionLabel{..} =
  let parts = catMaybes
        [ labelParty
        , if opts.showModal then fmap formatModal labelModal else Nothing
        , Just labelAction
        , if opts.showDeadlines then fmap (\d -> "[" <> d <> "]") labelDeadline else Nothing
        , if opts.showGuards then fmap (\g -> "IF " <> g) labelGuard else Nothing
        ]
  in Text.intercalate " " parts

-- | Format a deontic modal for display
formatModal :: DeonticModal -> Text
formatModal = \case
  DMust    -> "MUST"
  DMay     -> "MAY"
  DMustNot -> "SHANT"
  DDo      -> "DO"

-- | Convert FGL graph to GraphViz DotGraph
graphToDot :: StateGraphOptions -> StateGraph -> ContractGraph -> GV.DotGraph Node
graphToDot _opts StateGraph{..} graph =
  GV.graphToDot params graph
  where
    params = GV.nonClusteredParams
      { GV.globalAttributes =
          [ GV.GraphAttrs
              [ GV.RankDir GV.FromTop
              , GV.Label (GV.StrLabel (Text.Lazy.fromStrict sgName))
              , GV.LabelLoc GV.VTop
              , GV.FontName "Helvetica"
              , GV.FontSize 14
              ]
          , GV.NodeAttrs
              [ GV.FontName "Helvetica"
              , GV.FontSize 11
              ]
          , GV.EdgeAttrs
              [ GV.FontName "Helvetica"
              , GV.FontSize 10
              ]
          ]
      , GV.fmtNode = \(_, attrs) ->
          [ GV.toLabel attrs.naLabel
          , GV.FillColor [GV.toWC (GV.X11Color GV.White)]
          , GV.style (parseStyle attrs.naStyle)
          , GV.Shape (parseShape attrs.naShape)
          , GV.FillColor [GV.toWC (parseColor attrs.naFillColor)]
          ]
      , GV.fmtEdge = \(_, _, attrs) ->
          [ GV.toLabel attrs.eaLabel
          , GV.Color [GV.toWC (parseColor attrs.eaColor)]
          , GV.style (parseEdgeStyle attrs.eaStyle)
          ]
      }

-- | Parse a style string
parseStyle :: Text -> GV.Style
parseStyle "filled" = GV.filled
parseStyle _        = GV.filled

-- | Parse an edge style string
parseEdgeStyle :: Text -> GV.Style
parseEdgeStyle "dashed" = GV.dashed
parseEdgeStyle "dotted" = GV.dotted
parseEdgeStyle _        = GV.solid

-- | Parse a shape string
parseShape :: Text -> GV.Shape
parseShape "doublecircle" = GV.DoubleCircle
parseShape "box"          = GV.BoxShape
parseShape "diamond"      = GV.DiamondShape
parseShape _              = GV.Ellipse

-- | Parse a hex color to GraphViz color
parseColor :: Text -> GV.Color
parseColor hex = case Text.unpack hex of
  ('#':r1:r2:g1:g2:b1:b2:[]) ->
    let r = read ("0x" ++ [r1, r2]) :: Int
        g = read ("0x" ++ [g1, g2]) :: Int
        b = read ("0x" ++ [b1, b2]) :: Int
    in GV.RGB (fromIntegral r) (fromIntegral g) (fromIntegral b)
  _ -> GV.X11Color GV.Gray
