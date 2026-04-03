{-# LANGUAGE StrictData #-}

-- | Contract Essence extraction for qualia-based classification.
--
-- This module combines the ObligationGraph (who owes what to whom) with
-- the StateGraph (contract automaton) to extract a structural fingerprint
-- called the "Contract Essence". This fingerprint captures the fundamental
-- characteristics of a contract independent of naming conventions.
--
-- The Contract Essence enables classification by structural distance to
-- known archetypes (e.g., PAM, ANN, FXOUT, SWAPS) rather than by
-- pattern-matching on variable names.
module L4.ACTUS.Qualia.Essence (
  -- * Core Types
  ContractEssence (..),
  Symmetry (..),
  FlowTopology (..),
  TemporalShape (..),
  BreachTopology (..),

  -- * Extraction
  extractEssence,

  -- * Individual Analyzers
  computeSymmetry,
  computeFlowTopology,
  computeTemporalShape,
  computeOptionality,
  detectRecursionDepth,
  computeBreachTopology,

  -- * Utilities
  describeEssence,
) where

import Data.List (nub)
import Data.Text (Text)
import qualified Data.Text as T
import L4.ACTUS.Qualia.ObligationGraph
  ( ObligationGraph (..)
  , ObligationEdge (..)
  , PartyNode (..)
  , RecursionPattern (..)
  , FlowRole (NetDebtor, NetCreditor)  -- Exclude Symmetric to avoid clash
  , TemporalPattern (..)
  , flowRole
  , partyCount
  , hasRecursion
  )
import qualified L4.ACTUS.Qualia.ObligationGraph as OG
import L4.StateGraph
  ( ContractState (..)
  , StateGraph (..)
  , StateType (..)
  , Transition (..)
  , TransitionType (..)
  )
import L4.Syntax (DeonticModal (..))

--------------------------------------------------------------------------------
-- Core Types
--------------------------------------------------------------------------------

-- | Symmetry measures how balanced obligations are between parties.
--
-- This is a key structural qualia:
-- - Debt instruments are FullyAsymmetric (one-way flow)
-- - Exchanges/swaps are Symmetric (balanced mutual obligations)
-- - Options are AsymmetricWithRights (one has rights, other has obligations)
data Symmetry
  = FullyAsymmetric
  -- ^ One party has all obligations (debt pattern)
  | AsymmetricWithRights
  -- ^ One has obligations, other has discretionary rights (option pattern)
  | Symmetric
  -- ^ Balanced mutual obligations (exchange, swap pattern)
  | MultiPartyHub
  -- ^ Central counterparty pattern (clearing, netting)
  deriving stock (Eq, Show)

-- | Flow topology captures the direction pattern of value movement.
data FlowTopology
  = Unidirectional
  -- ^ Value flows A → B only (debt, guarantee)
  | Bidirectional
  -- ^ Value flows A ⇄ B (exchange, swap)
  | Circular
  -- ^ Value flows A → B → C → A (complex netting)
  | StarPattern Int
  -- ^ N parties → central hub (clearing house)
  deriving stock (Eq, Show)

-- | Temporal shape captures how obligations evolve over time.
data TemporalShape
  = PointToPoint
  -- ^ Single start, single end (forward, spot)
  | PeriodicUntilTerm
  -- ^ Regular payments until maturity (coupon bonds)
  | PeriodicIndefinite
  -- ^ Perpetual periodic (perpetuities, annuities without term)
  | RecursiveDecay
  -- ^ Self-referential with diminishing value (amortization)
  | EventDriven
  -- ^ Triggered by external events (options, insurance)
  deriving stock (Eq, Show)

-- | Breach topology captures the failure/remedy structure.
data BreachTopology
  = SimpleBreach
  -- ^ Direct terminal breach state on failure
  | EscalationLadder Int
  -- ^ N-step escalation (warning → penalty → acceleration)
  | CureBranch
  -- ^ Opportunity to cure before terminal breach
  | NoBreachPath
  -- ^ No LEST clauses (fully discretionary, e.g., MAY only)
  deriving stock (Eq, Show)

-- | The structural fingerprint of a contract.
--
-- This captures the essential characteristics that determine what
-- type of financial instrument a contract represents, independent
-- of the naming conventions used in the source code.
data ContractEssence = ContractEssence
  { cePartyCount :: Int
  -- ^ Number of distinct parties
  , ceSymmetry :: Symmetry
  -- ^ Obligation balance between parties
  , ceFlowTopology :: FlowTopology
  -- ^ Value movement pattern
  , ceTemporalShape :: TemporalShape
  -- ^ Time structure
  , ceOptionalityDegree :: Double
  -- ^ 0.0 = all MUST, 1.0 = all MAY
  , ceRecursionDepth :: Maybe Int
  -- ^ Self-reference depth (Nothing = no recursion)
  , ceBreachTopology :: BreachTopology
  -- ^ Failure handling structure
  , ceStateCount :: Int
  -- ^ Number of distinct states in automaton
  , ceTerminalModes :: Int
  -- ^ Number of ways contract can end (fulfilled + breach paths)
  }
  deriving stock (Eq, Show)

--------------------------------------------------------------------------------
-- Extraction
--------------------------------------------------------------------------------

-- | Extract the contract essence from obligation graph and state graph.
--
-- This is the main entry point for Phase 2. It combines structural
-- information from both graphs to produce a unified fingerprint.
extractEssence :: ObligationGraph -> StateGraph -> ContractEssence
extractEssence og sg =
  ContractEssence
    { cePartyCount = partyCount og
    , ceSymmetry = computeSymmetry og
    , ceFlowTopology = computeFlowTopology og
    , ceTemporalShape = computeTemporalShape og sg
    , ceOptionalityDegree = computeOptionality og
    , ceRecursionDepth = detectRecursionDepth og
    , ceBreachTopology = computeBreachTopology sg
    , ceStateCount = length sg.sgStates
    , ceTerminalModes = countTerminalStates sg
    }

--------------------------------------------------------------------------------
-- Individual Analyzers
--------------------------------------------------------------------------------

-- | Compute symmetry from obligation balance.
--
-- Analyzes the flow roles of all parties to determine the overall
-- symmetry pattern of the contract.
computeSymmetry :: ObligationGraph -> Symmetry
computeSymmetry og =
  let parties = og.ogParties
      roles = map flowRole parties
      debtorCount = length $ filter (== NetDebtor) roles
      creditorCount = length $ filter (== NetCreditor) roles
      symmetricCount = length $ filter (== OG.Symmetric) roles
      totalParties = length parties

      -- Check if there are MAY clauses (discretionary rights)
      hasMayRights = any (\p -> p.pnMayCount > 0) parties
  in case (debtorCount, creditorCount, symmetricCount, totalParties) of
       -- Classic debt: one debtor, one creditor, no MAY
       (1, 1, 0, 2) | not hasMayRights -> FullyAsymmetric
       -- Option-like: one debtor, one creditor, but creditor has MAY rights
       (1, 1, 0, 2) | hasMayRights -> AsymmetricWithRights
       -- Exchange/swap: both parties symmetric
       (0, 0, 2, 2) -> Symmetric
       (0, 0, n, n') | n == n' && n > 0 -> Symmetric
       -- Hub pattern: multiple debtors, one creditor
       (n, 1, _, _) | n > 1 -> MultiPartyHub
       -- Single creditor with multiple parties having rights
       (_, 1, _, n') | n' > 2 -> MultiPartyHub
       -- Default to symmetric if unclear
       _ -> if debtorCount == creditorCount then Symmetric else FullyAsymmetric

-- | Compute flow topology from edge directions.
--
-- Determines whether value flows in one direction, both directions,
-- or in more complex patterns.
computeFlowTopology :: ObligationGraph -> FlowTopology
computeFlowTopology og =
  let edges = og.ogObligations
      parties = og.ogParties
      numParties = length parties

      -- Get unique (from, to) pairs
      flowPairs = nub [(e.oeFrom, e.oeTo) | e <- edges]
      -- Check for reverse flows
      reversePairs = [(to, from) | (from, to) <- flowPairs]
      hasBidirectional = any (`elem` flowPairs) reversePairs

      -- Check for circular flows (A → B → C → A)
      hasCircular = detectCircularFlow flowPairs

      -- Check for star pattern (multiple parties → one hub)
      hubParties = findHubParties og
  in case (hasBidirectional, hasCircular, hubParties, numParties) of
       (_, _, Just n, _) | n > 2 -> StarPattern n
       (_, True, _, _) -> Circular
       (True, _, _, _) -> Bidirectional
       _ -> Unidirectional

-- | Detect circular flow patterns.
detectCircularFlow :: [(Int, Int)] -> Bool
detectCircularFlow pairs =
  -- Simple heuristic: if we have A→B, B→C, C→A pattern
  let findCycle start visited current =
        if current == start && length visited > 2
          then True
          else
            let nexts = [to | (from, to) <- pairs, from == current, to `notElem` visited]
            in any (findCycle start (current : visited)) nexts
  in any (\(from, _) -> findCycle from [] from) pairs

-- | Find hub parties (parties with many incoming connections).
findHubParties :: ObligationGraph -> Maybe Int
findHubParties og =
  let parties = og.ogParties
      -- A hub has significantly more incoming than outgoing
      hubs = filter (\p -> p.pnInDegree > 2 && p.pnInDegree > p.pnOutDegree * 2) parties
  in if null hubs then Nothing else Just (length parties)

-- | Compute temporal shape from patterns and state machine.
--
-- Combines information from obligation edges (recursion, periodicity)
-- with state machine structure (event triggers, termination).
computeTemporalShape :: ObligationGraph -> StateGraph -> TemporalShape
computeTemporalShape og sg =
  let hasRecursion' = hasRecursion og
      hasPeriodic = any isPeriodicEdge og.ogObligations
      hasEventTrigger = any isEventTransition sg.sgTransitions
      hasDefiniteEnd = any isFulfilledState sg.sgStates
  in case (hasRecursion', hasPeriodic, hasEventTrigger, hasDefiniteEnd) of
       -- Recursive patterns indicate amortization
       (True, _, _, _) -> RecursiveDecay
       -- Periodic with definite end
       (_, True, False, True) -> PeriodicUntilTerm
       -- Periodic without definite end
       (_, True, False, False) -> PeriodicIndefinite
       -- Event-triggered (options, insurance)
       (_, _, True, _) -> EventDriven
       -- Default: point-to-point
       _ -> PointToPoint

-- | Check if an edge represents periodic timing.
isPeriodicEdge :: ObligationEdge -> Bool
isPeriodicEdge e = case e.oeTiming of
  Periodic -> True
  Recursive _ -> True
  _ -> False

-- | Check if a transition is event-driven (not time-based).
isEventTransition :: Transition -> Bool
isEventTransition t =
  -- DefaultTransition often indicates timeout/event trigger
  -- HenceTransition with specific conditions can also be events
  t.transType == DefaultTransition

-- | Check if a state is a fulfilled terminal state.
isFulfilledState :: ContractState -> Bool
isFulfilledState s = s.stateType == TerminalFulfilled

-- | Compute optionality degree (fraction of MAY vs total obligations).
--
-- A value of 0.0 means all obligations are mandatory (MUST).
-- A value of 1.0 means all are discretionary (MAY).
-- Options typically have high optionality; debt has low optionality.
computeOptionality :: ObligationGraph -> Double
computeOptionality og =
  let edges = og.ogObligations
      mayCount = length $ filter (\e -> e.oeModal == DMay) edges
      total = length edges
  in if total == 0
       then 0.0
       else fromIntegral mayCount / fromIntegral total

-- | Detect recursion depth from obligation patterns.
--
-- Returns Nothing if no recursion, or Just depth if recursive.
-- Amortizing loans typically have recursion depth > 0.
detectRecursionDepth :: ObligationGraph -> Maybe Int
detectRecursionDepth og =
  let recursions = og.ogRecursions
  in case recursions of
       [] -> Nothing
       rs -> Just $ maximum $ map (\r -> r.rpDepth) rs

-- | Compute breach topology from state machine structure.
--
-- Analyzes the LEST paths to determine how failures are handled.
computeBreachTopology :: StateGraph -> BreachTopology
computeBreachTopology sg =
  let states = sg.sgStates
      transitions = sg.sgTransitions

      -- Find breach states
      breachStates = filter (\s -> s.stateType == TerminalBreach) states

      -- Find LEST transitions
      lestTransitions = filter (\t -> t.transType == LestTransition) transitions

      -- Count escalation steps (chain of LEST transitions)
      escalationCount = countEscalationChain sg

      -- Check if there's a cure opportunity (HENCE from intermediate after LEST)
      hasCure = detectCureOpportunity sg
  in case (null breachStates, null lestTransitions, escalationCount, hasCure) of
       -- No breach states means no breach path
       (True, _, _, _) -> NoBreachPath
       -- No LEST transitions but has breach state (unusual)
       (_, True, _, _) -> SimpleBreach
       -- Has cure opportunity
       (_, _, _, True) -> CureBranch
       -- Multiple escalation steps
       (_, _, n, _) | n > 1 -> EscalationLadder n
       -- Single breach transition
       _ -> SimpleBreach

-- | Count escalation steps in LEST chain.
--
-- An escalation ladder is when LEST → intermediate → LEST → breach,
-- e.g., late payment → penalty → acceleration → default.
countEscalationChain :: StateGraph -> Int
countEscalationChain sg =
  let lestTransitions = filter (\t -> t.transType == LestTransition) sg.sgTransitions
      -- Count unique intermediate states reached via LEST
      intermediateTargets =
        [ t.transTo
        | t <- lestTransitions
        , let targetState = findState sg t.transTo
        , maybe False (\s -> s.stateType == IntermediateState) targetState
        ]
  in length (nub intermediateTargets) + 1 -- +1 for final breach

-- | Find a state by ID.
findState :: StateGraph -> Int -> Maybe ContractState
findState sg sid =
  case filter (\s -> s.stateId == sid) sg.sgStates of
    (s : _) -> Just s
    [] -> Nothing

-- | Detect if there's a cure opportunity (chance to remedy after LEST).
detectCureOpportunity :: StateGraph -> Bool
detectCureOpportunity sg =
  let transitions = sg.sgTransitions
      -- Find states reached via LEST that have outgoing HENCE
      lestTargets = [t.transTo | t <- transitions, t.transType == LestTransition]
      -- Check if any of these have a HENCE path out (cure)
      hasHenceOut stateId =
        any (\t -> t.transFrom == stateId && t.transType == HenceTransition) transitions
  in any hasHenceOut lestTargets

-- | Count terminal states (fulfilled + breach).
countTerminalStates :: StateGraph -> Int
countTerminalStates sg =
  length $ filter isTerminal sg.sgStates
  where
    isTerminal s = s.stateType `elem` [TerminalFulfilled, TerminalBreach]

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

-- | Generate a human-readable description of the contract essence.
--
-- Useful for debugging and explanation generation.
describeEssence :: ContractEssence -> Text
describeEssence ce =
  T.unlines
    [ "Contract Essence:"
    , "  Parties: " <> T.pack (show ce.cePartyCount)
    , "  Symmetry: " <> describeSymmetry ce.ceSymmetry
    , "  Flow: " <> describeTopology ce.ceFlowTopology
    , "  Temporal: " <> describeTemporal ce.ceTemporalShape
    , "  Optionality: " <> T.pack (show (round (ce.ceOptionalityDegree * 100) :: Int)) <> "%"
    , "  Recursion: " <> maybe "None" (T.pack . show) ce.ceRecursionDepth
    , "  Breach handling: " <> describeBreach ce.ceBreachTopology
    , "  States: " <> T.pack (show ce.ceStateCount)
    , "  Terminal modes: " <> T.pack (show ce.ceTerminalModes)
    ]

describeSymmetry :: Symmetry -> Text
describeSymmetry = \case
  FullyAsymmetric -> "Fully asymmetric (debtor/creditor)"
  AsymmetricWithRights -> "Asymmetric with rights (option-like)"
  Symmetric -> "Symmetric (exchange/swap)"
  MultiPartyHub -> "Multi-party hub (clearing)"

describeTopology :: FlowTopology -> Text
describeTopology = \case
  Unidirectional -> "Unidirectional (one-way flow)"
  Bidirectional -> "Bidirectional (exchange)"
  Circular -> "Circular (complex netting)"
  StarPattern n -> "Star pattern (" <> T.pack (show n) <> " parties)"

describeTemporal :: TemporalShape -> Text
describeTemporal = \case
  PointToPoint -> "Point-to-point (single event)"
  PeriodicUntilTerm -> "Periodic until maturity"
  PeriodicIndefinite -> "Perpetual periodic"
  RecursiveDecay -> "Recursive decay (amortization)"
  EventDriven -> "Event-driven (contingent)"

describeBreach :: BreachTopology -> Text
describeBreach = \case
  SimpleBreach -> "Simple breach"
  EscalationLadder n -> T.pack (show n) <> "-step escalation"
  CureBranch -> "Cure opportunity"
  NoBreachPath -> "No breach path"
