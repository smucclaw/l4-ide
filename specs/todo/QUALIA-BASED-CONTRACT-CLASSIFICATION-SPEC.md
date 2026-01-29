# Qualia-Based Contract Classification

## Status: Proposed

## Overview

This spec describes a structural approach to contract classification that analyzes the **essence** of contracts rather than matching symbolic names. The goal is to correctly classify contracts even when they use non-standard terminology, different languages, or obfuscated variable names.

**Key insight**: A loan is not a loan because it contains a variable named "borrower" — it's a loan because one party has a net negative obligation flow with temporal decay toward another party.

## Problem Statement

The current `jl4-actus-analyzer` uses **symbolic pattern matching** to classify contracts:
- Type names containing "borrower", "lender", "currency"
- Field names matching patterns like "principal", "interestRate"
- String matching against ontology labels

This approach fails when:
- Contracts use non-standard naming conventions
- Variable names are obfuscated or in different languages
- The contract structure is correct but terminology differs

## Proposed Solution: Essence Detection

Analyze the **structural qualia** of contracts independent of naming conventions. Instead of asking "does this contain a type named Borrower?", ask "does this contain an entity with a net negative obligation flow?"

## Existing L4 Infrastructure

The L4 codebase already provides foundational components for structural analysis:

### Deonton (Deontic Particle)

From `L4.Syntax`, the `Deonton` is the atomic unit of normative content:

```haskell
data Deonton n = MkDeonton
  { anno   :: Anno
  , party  :: Expr n        -- Who has the obligation
  , action :: RAction n     -- What they must/may/shant do
  , due    :: Maybe (Expr n)    -- WITHIN deadline
  , hence  :: Maybe (Expr n)    -- Success path
  , lest   :: Maybe (Expr n)    -- Failure path
  }

data DeonticModal = DMust | DMay | DMustNot | DDo
```

### StateGraph Module

`L4.StateGraph` already extracts contract automata (inspired by Flood & Goodenough's "Contract as Automaton"):

```haskell
data StateGraph = StateGraph
  { sgName         :: Text
  , sgStates       :: [ContractState]
  , sgTransitions  :: [Transition]
  , sgInitialState :: StateId
  }

data StateType = InitialState | IntermediateState | TerminalFulfilled | TerminalBreach

data TransitionType = HenceTransition | LestTransition | DefaultTransition
```

This provides the **state machine topology** needed for qualia analysis.

### What's Missing

The current infrastructure extracts structure but doesn't **analyze** it for classification. We need:

1. **Obligation graph** - Who owes what to whom (direction analysis)
2. **Flow analysis** - Value movement patterns over time
3. **Symmetry detection** - Bilateral vs unilateral relationships
4. **Archetype matching** - Structural distance to known contract types

## Architectural Overview

```
┌─────────────────────────────────────────────────────────────────┐
│                    L4 Contract (AST)                            │
└─────────────────────────────┬───────────────────────────────────┘
                              │
                              v
┌─────────────────────────────────────────────────────────────────┐
│                 Structural Extraction Layer                      │
│  ┌──────────────┐  ┌──────────────┐  ┌──────────────────────┐   │
│  │ Obligation   │  │ Value Flow   │  │ Temporal Pattern     │   │
│  │ Graph        │  │ Analysis     │  │ Extraction           │   │
│  └──────────────┘  └──────────────┘  └──────────────────────┘   │
└─────────────────────────────┬───────────────────────────────────┘
                              │
                              v
┌─────────────────────────────────────────────────────────────────┐
│                   Contract Essence Model                         │
│  • Party roles (by obligation direction, not name)              │
│  • Value flows (amounts, directions, timing)                    │
│  • State topology (transitions, termination paths)              │
│  • Symmetry analysis (bilateral vs unilateral)                  │
└─────────────────────────────┬───────────────────────────────────┘
                              │
                              v
┌─────────────────────────────────────────────────────────────────┐
│               Qualia-Based Classifier                            │
│  ┌──────────────────────────────────────────────────────────┐   │
│  │  ACTUS Archetype Signatures (structural, not symbolic)   │   │
│  │  • PAM: asymmetric debt + temporal decay + interest      │   │
│  │  • FXOUT: bilateral exchange + currency asymmetry        │   │
│  │  • SWAPS: periodic bilateral + notional reference        │   │
│  │  • OPTNS: conditional exercise right + premium           │   │
│  └──────────────────────────────────────────────────────────┘   │
└─────────────────────────────────────────────────────────────────┘
```

## Structural Features to Extract

### 1. Obligation Graph

Build a directed graph of obligations between parties:

```
Node: Party (identified by structural role, not name)
Edge: Obligation (amount, condition, timing)
```

**Qualia detected:**
- **Net debtor**: Party with more outgoing than incoming obligation edges
- **Net creditor**: Party with more incoming than outgoing obligation edges
- **Symmetric exchange**: Balanced bidirectional edges (FX, swap)
- **Unilateral debt**: One-way obligation flow (loan, bond)

### 2. Value Flow Patterns

Analyze how value moves through the contract over time:

| Pattern | Essence | ACTUS Type |
|---------|---------|------------|
| Single outflow at T₀, periodic inflows | Loan disbursement + repayment | PAM, ANN, LAM |
| Bilateral exchange at T₁ | Spot/forward transaction | FXOUT |
| Periodic bilateral flows | Swap payments | SWAPS |
| Conditional large outflow | Option exercise | OPTNS |
| Initial premium, contingent payout | Insurance/guarantee | CEG |

### 3. Temporal Topology

Extract the shape of time in the contract:

- **Point events**: Single occurrence (maturity, exercise)
- **Periodic events**: Repeating at intervals (coupon, swap payment)
- **Decay functions**: Value decreasing over time (amortization)
- **Accumulation functions**: Value increasing over time (interest accrual)

### 4. State Machine Structure

Analyze HENCE/LEST clauses to extract:

- **Happy path**: Normal contract progression
- **Breach paths**: Failure conditions and remedies
- **Termination modes**: How the contract can end
- **Optionality**: Choice points (MAY clauses)

```
Topology signatures:
- Linear (start → ... → end): Simple debt
- Branching (choice points): Options, conditions
- Looping (periodic reset): Swaps, revolving credit
- Recursive (self-reference): Amortizing payments
```

### 5. Symmetry Analysis

Measure the structural symmetry between parties:

| Symmetry Type | Characteristics | Contract Family |
|---------------|-----------------|-----------------|
| Asymmetric | One party has most obligations | Debt instruments |
| Symmetric bilateral | Balanced mutual obligations | Exchanges, swaps |
| Asymmetric with optionality | One party has rights, other has obligations | Options, warrants |
| Multi-party asymmetric | Central counterparty | Clearing, netting |

## Concrete Example: Promissory Note Analysis

Consider the L4 promissory note at `jl4/examples/legal/promissory-note.l4`.

### Current Symbolic Analysis (45% confidence)

The symbolic analyzer detects:
- Type `Borrower` → matches "borrower" pattern
- Type `Lender` → matches "lender" pattern
- Field `Interest Rate` in `Penalty` → matches interest pattern

**Problem**: If we renamed `Borrower` to `PartyA` and `Lender` to `PartyB`, confidence would drop to ~0%.

### Proposed Qualia Analysis

Extract the structural essence without looking at names:

```
Obligation Graph:
  PartyA ──MUST──▶ PartyB  (unidirectional)
          │
          └─ action: transfer Money
             timing: periodic (recursive with decay)
             condition: Outstanding > 0

Flow Pattern:
  Initial: PartyA receives nothing explicitly (implicit: money at T₀)
  Periodic: PartyA → PartyB (money, decreasing until exhaustion)

Temporal Shape:
  - Recursive call: `Payment Obligations` (Outstanding - Paid)
  - Termination: FULFILLED when Outstanding ≤ 0
  - Decay function: Outstanding decreases each iteration

State Topology:
  ┌──────────┐    pay on time    ┌──────────────────┐
  │ initial  │ ─────────────────▶│ Payment          │──┐
  └──────────┘                   │ Obligations(n-1) │  │ recursive
                                 └──────────────────┘◀─┘
                                        │
                        timeout         │ Outstanding ≤ 0
                           ▼            ▼
                    ┌───────────┐  ┌───────────┐
                    │ + Penalty │  │ FULFILLED │
                    └─────┬─────┘  └───────────┘
                          │ timeout
                          ▼
                    ┌─────────────┐
                    │ Accelerate  │
                    │ All Debts   │
                    └─────────────┘

Symmetry Analysis:
  - Party count: 2
  - Obligation direction: unidirectional (A → B)
  - Modal distribution: A has MUST, B has no obligations
  - Rights asymmetry: B is pure beneficiary
```

### Qualia Classification Result

```
Structural signature:
  - parties: 2 (asymmetric)
  - flow: unidirectional, periodic, decaying
  - termination: value exhaustion
  - breach_ladder: escalation (penalty → acceleration)

Distance to archetypes:
  - PAM: 0.15 (close - debt with interest)
  - ANN: 0.08 (very close - amortizing debt)  ← Best match
  - FXOUT: 0.85 (far - no bilateral exchange)
  - SWAPS: 0.90 (far - no periodic bilateral)

Classification: ANN (Annuity) with 92% structural confidence
```

This would correctly classify the promissory note even if all variables were renamed to `x1`, `x2`, `fn1`, etc.

## Essence Signatures for ACTUS Types

### PAM (Principal at Maturity)

```
Essence:
  parties: 2 (asymmetric)
  obligation_direction: unidirectional (debtor → creditor)
  principal_flow: single outflow T₀, single inflow T_maturity
  interest_pattern: accumulation function on principal
  temporal_shape: point-to-point with optional periodic interest
```

### ANN (Annuity)

```
Essence:
  parties: 2 (asymmetric)
  obligation_direction: unidirectional
  principal_flow: single outflow T₀, periodic inflows
  payment_pattern: constant periodic (principal + interest blend)
  temporal_shape: periodic until exhaustion
```

### FXOUT (FX Outright)

```
Essence:
  parties: 2 (symmetric)
  obligation_direction: bidirectional simultaneous
  value_types: 2 distinct (currency asymmetry)
  exchange_pattern: bilateral at single point
  temporal_shape: point event (spot or forward)
```

### SWAPS (Swap)

```
Essence:
  parties: 2 (symmetric)
  obligation_direction: bidirectional periodic
  reference_value: notional (not exchanged)
  payment_pattern: periodic netted bilateral
  temporal_shape: periodic over term
```

### OPTNS (Option)

```
Essence:
  parties: 2 (asymmetric with optionality)
  rights_structure: one party has MAY, other has conditional MUST
  premium: initial payment for right
  exercise_pattern: conditional large flow
  temporal_shape: point event (exercise) with expiry boundary
```

## Implementation Approach

### Integration with Existing Code

The implementation should extend `jl4-actus-analyzer` with a new module structure:

```
jl4-actus-analyzer/src/L4/ACTUS/
├── Analyzer.hs              -- Existing entry point
├── FeatureExtractor.hs      -- Existing symbolic extraction
├── Qualia/                  -- NEW: Structural analysis
│   ├── ObligationGraph.hs   -- Build directed obligation graph
│   ├── FlowAnalysis.hs      -- Analyze value movement patterns
│   ├── TemporalShape.hs     -- Extract temporal topology
│   ├── SymmetryDetector.hs  -- Measure party symmetry
│   └── Essence.hs           -- Combine into ContractEssence
├── Matching/
│   ├── Rules.hs             -- Existing symbolic rules
│   ├── Scorer.hs            -- Existing scoring
│   └── Archetypes.hs        -- NEW: Structural archetype definitions
```

### Phase 1: Obligation Graph Extraction

Build on `L4.StateGraph` to extract a directed graph of obligations:

```haskell
-- L4/ACTUS/Qualia/ObligationGraph.hs

-- | An obligation graph captures WHO owes WHAT to WHOM
data ObligationGraph = ObligationGraph
  { ogParties     :: [PartyNode]
  , ogObligations :: [ObligationEdge]
  , ogRecursions  :: [RecursionPattern]   -- Self-referential structures
  }

-- | A party identified by structural role, not name
data PartyNode = PartyNode
  { pnId         :: Int
  , pnExprHash   :: Int                   -- Hash of party expression for identity
  , pnOutDegree  :: Int                   -- Outgoing MUST/DO obligations
  , pnInDegree   :: Int                   -- Incoming obligations (as beneficiary)
  , pnMayCount   :: Int                   -- MAY clauses (rights)
  }

-- | Computed role based on obligation flow
data FlowRole = NetDebtor | NetCreditor | Symmetric | Uninvolved

flowRole :: PartyNode -> FlowRole
flowRole pn
  | pn.pnOutDegree > pn.pnInDegree = NetDebtor
  | pn.pnInDegree > pn.pnOutDegree = NetCreditor
  | pn.pnOutDegree == pn.pnInDegree && pn.pnOutDegree > 0 = Symmetric
  | otherwise = Uninvolved

-- | An edge representing a deontic obligation
data ObligationEdge = ObligationEdge
  { oeFrom      :: Int                    -- Obligor party ID
  , oeTo        :: Int                    -- Beneficiary party ID
  , oeModal     :: DeonticModal           -- MUST/MAY/SHANT
  , oeValueType :: ValueType              -- What flows
  , oeTiming    :: TemporalPattern        -- When/how often
  }

-- | What kind of value flows
data ValueType
  = MoneyValue                            -- Currency/money transfer
  | AssetValue                            -- Non-fungible asset
  | ServiceValue                          -- Action/service
  | UnknownValue

-- | Temporal patterns
data TemporalPattern
  = PointInTime                           -- Single occurrence
  | Periodic                              -- Repeating at intervals
  | Recursive DecayPattern                -- Self-referential with decay
  | Continuous

-- | How recursive patterns decay
data DecayPattern
  = LinearDecay                           -- Fixed reduction each iteration
  | ExponentialDecay                      -- Compound interest pattern
  | UntilExhaustion                       -- Until value reaches zero
  | Unbounded                             -- No natural termination

-- | Extract from L4 module
extractObligationGraph :: Module Resolved -> ObligationGraph
extractObligationGraph mod' =
  let deontons = collectDeontons mod'      -- Reuse from FeatureExtractor
      parties = identifyParties deontons   -- Cluster by expression equivalence
      edges = map (deontonToEdge parties) deontons
      recursions = detectRecursions mod'
  in ObligationGraph parties edges recursions
```

### Phase 2: Essence Extraction

Combine obligation graph with state graph to extract the contract's structural fingerprint:

```haskell
-- L4/ACTUS/Qualia/Essence.hs

-- | Symmetry measures how balanced obligations are between parties
data Symmetry
  = FullyAsymmetric          -- One party has all obligations (debt)
  | AsymmetricWithRights     -- One has obligations, other has rights (options)
  | Symmetric                -- Balanced mutual obligations (swaps, exchanges)
  | MultiPartyHub            -- Central counterparty pattern
  deriving (Eq, Show)

-- | Flow topology captures the direction pattern of value movement
data FlowTopology
  = Unidirectional           -- A → B only
  | Bidirectional            -- A ⇄ B
  | Circular                 -- A → B → C → A
  | StarPattern Int          -- N parties → central hub
  deriving (Eq, Show)

-- | Temporal shape captures how obligations evolve over time
data TemporalShape
  = PointToPoint             -- Single start, single end (forward)
  | PeriodicUntilTerm        -- Regular payments until maturity
  | PeriodicIndefinite       -- Perpetual periodic (perpetuities)
  | RecursiveDecay           -- Self-referential with diminishing value
  | EventDriven              -- Triggered by external events (options)
  deriving (Eq, Show)

-- | Breach topology captures failure/remedy structure
data BreachTopology
  = SimpleBreach             -- Direct terminal breach state
  | EscalationLadder Int     -- N-step escalation (warning → penalty → acceleration)
  | CureBranch               -- Opportunity to cure before terminal breach
  | NoBreachPath             -- No LEST clauses (fully discretionary)
  deriving (Eq, Show)

-- | The structural fingerprint of a contract
data ContractEssence = ContractEssence
  { cePartyCount      :: Int               -- Number of distinct parties
  , ceSymmetry        :: Symmetry          -- Obligation balance
  , ceFlowTopology    :: FlowTopology      -- Value movement pattern
  , ceTemporalShape   :: TemporalShape     -- Time structure
  , ceOptionalityDegree :: Double          -- 0.0 = all MUST, 1.0 = all MAY
  , ceRecursionDepth  :: Maybe Int         -- Self-reference depth (amortization)
  , ceBreachTopology  :: BreachTopology    -- Failure handling structure
  , ceStateCount      :: Int               -- Number of distinct states
  , ceTerminalModes   :: Int               -- Number of ways contract can end
  }
  deriving (Eq, Show)

-- | Extract essence from obligation graph and state graph
extractEssence :: ObligationGraph -> StateGraph -> ContractEssence
extractEssence og sg = ContractEssence
  { cePartyCount      = length og.ogParties
  , ceSymmetry        = computeSymmetry og
  , ceFlowTopology    = computeFlowTopology og
  , ceTemporalShape   = computeTemporalShape og sg
  , ceOptionalityDegree = computeOptionality og
  , ceRecursionDepth  = detectRecursionDepth og
  , ceBreachTopology  = computeBreachTopology sg
  , ceStateCount      = length sg.sgStates
  , ceTerminalModes   = countTerminalStates sg
  }

-- | Compute symmetry from obligation balance
computeSymmetry :: ObligationGraph -> Symmetry
computeSymmetry og =
  let roles = map flowRole og.ogParties
      debtors = filter (== NetDebtor) roles
      creditors = filter (== NetCreditor) roles
      symmetrics = filter (== Symmetric) roles
  in case (length debtors, length creditors, length symmetrics) of
    (1, 1, 0) | hasOnlyMust og -> FullyAsymmetric
    (1, 1, 0)                  -> AsymmetricWithRights
    (0, 0, 2)                  -> Symmetric
    (n, 1, _) | n > 1          -> MultiPartyHub
    _                          -> Symmetric  -- Default

-- | Check if all obligations are MUST (no MAY)
hasOnlyMust :: ObligationGraph -> Bool
hasOnlyMust og = all (\e -> e.oeModal == DMust) og.ogObligations

-- | Compute flow topology from edge directions
computeFlowTopology :: ObligationGraph -> FlowTopology
computeFlowTopology og =
  let edges = og.ogObligations
      forwardEdges = filter (\e -> e.oeFrom < e.oeTo) edges
      backwardEdges = filter (\e -> e.oeFrom > e.oeTo) edges
  in case (null forwardEdges, null backwardEdges) of
    (False, True)  -> Unidirectional
    (True, False)  -> Unidirectional
    (False, False) -> Bidirectional
    (True, True)   -> Unidirectional  -- No edges = trivial

-- | Compute temporal shape from patterns and state machine
computeTemporalShape :: ObligationGraph -> StateGraph -> TemporalShape
computeTemporalShape og sg =
  let hasRecursion = not (null og.ogRecursions)
      hasPeriodic = any isPeriodic og.ogObligations
      hasEventTrigger = any isEventDriven sg.sgTransitions
  in case (hasRecursion, hasPeriodic, hasEventTrigger) of
    (True, _, _)       -> RecursiveDecay
    (_, True, False)   -> PeriodicUntilTerm
    (_, _, True)       -> EventDriven
    _                  -> PointToPoint

isPeriodic :: ObligationEdge -> Bool
isPeriodic e = case e.oeTiming of
  Periodic -> True
  Recursive _ -> True
  _ -> False

isEventDriven :: Transition -> Bool
isEventDriven t = t.transitionType == DefaultTransition  -- Timeout/event trigger

-- | Compute optionality degree (fraction of MAY vs total)
computeOptionality :: ObligationGraph -> Double
computeOptionality og =
  let edges = og.ogObligations
      mayCount = length $ filter (\e -> e.oeModal == DMay) edges
      total = length edges
  in if total == 0 then 0.0 else fromIntegral mayCount / fromIntegral total

-- | Detect recursion depth from HENCE self-references
detectRecursionDepth :: ObligationGraph -> Maybe Int
detectRecursionDepth og =
  case og.ogRecursions of
    [] -> Nothing
    rs -> Just $ maximum $ map recursionDepth rs
  where
    recursionDepth (RecursionPattern depth _) = depth

-- | Compute breach topology from state machine structure
computeBreachTopology :: StateGraph -> BreachTopology
computeBreachTopology sg =
  let breachStates = filter isBreach sg.sgStates
      lestTransitions = filter isLest sg.sgTransitions
      escalationCount = countEscalationSteps sg
  in case (null breachStates, null lestTransitions, escalationCount) of
    (True, _, _)       -> NoBreachPath
    (_, _, n) | n > 1  -> EscalationLadder n
    (_, _, 1)          -> if hasCurePath sg then CureBranch else SimpleBreach
    _                  -> SimpleBreach

isBreach :: ContractState -> Bool
isBreach s = s.stateType == TerminalBreach

isLest :: Transition -> Bool
isLest t = t.transitionType == LestTransition

countEscalationSteps :: StateGraph -> Int
countEscalationSteps sg = length $ filter isLest sg.sgTransitions

hasCurePath :: StateGraph -> Bool
hasCurePath sg = any (\t -> t.transitionType == HenceTransition &&
                           any (\s -> s.stateId == t.targetState &&
                                      s.stateType == IntermediateState) sg.sgStates)
                     sg.sgTransitions

countTerminalStates :: StateGraph -> Int
countTerminalStates sg = length $ filter isTerminal sg.sgStates
  where isTerminal s = s.stateType `elem` [TerminalFulfilled, TerminalBreach]

-- | Classify contract by structural essence
classifyByEssence :: ContractEssence -> [(ACTUSType, Double)]
classifyByEssence essence =
  sortBy (comparing (negate . snd)) $
    map (scoreAgainstArchetype essence) actusArchetypes
```

### Phase 3: Archetype Matching

Instead of string matching, compute **structural distance** between the extracted essence and known archetypes.

```haskell
-- L4/ACTUS/Matching/Archetypes.hs

-- | An ACTUS archetype defined by structural expectations
data ACTUSArchetype = ACTUSArchetype
  { archActusCode      :: Text           -- "PAM", "ANN", "FXOUT", etc.
  , archLabel          :: Text           -- Human-readable name
  , archExpectedSymmetry :: Symmetry
  , archExpectedTopology :: FlowTopology
  , archExpectedTemporal :: TemporalShape
  , archExpectedBreach   :: BreachTopology
  , archPartyCount       :: Int
  , archRequiresRecursion :: Bool        -- Must have recursive structure?
  , archMinOptionality   :: Double       -- Minimum MAY degree
  , archMaxOptionality   :: Double       -- Maximum MAY degree
  }

-- | All ACTUS archetypes defined structurally
actusArchetypes :: [ACTUSArchetype]
actusArchetypes =
  [ -- Debt instruments
    ACTUSArchetype
      { archActusCode = "PAM"
      , archLabel = "Principal at Maturity"
      , archExpectedSymmetry = FullyAsymmetric
      , archExpectedTopology = Unidirectional
      , archExpectedTemporal = PointToPoint
      , archExpectedBreach = SimpleBreach
      , archPartyCount = 2
      , archRequiresRecursion = False
      , archMinOptionality = 0.0
      , archMaxOptionality = 0.1
      }
  , ACTUSArchetype
      { archActusCode = "ANN"
      , archLabel = "Annuity"
      , archExpectedSymmetry = FullyAsymmetric
      , archExpectedTopology = Unidirectional
      , archExpectedTemporal = RecursiveDecay
      , archExpectedBreach = EscalationLadder 2
      , archPartyCount = 2
      , archRequiresRecursion = True
      , archMinOptionality = 0.0
      , archMaxOptionality = 0.1
      }
  , ACTUSArchetype
      { archActusCode = "LAM"
      , archLabel = "Linear Amortizer"
      , archExpectedSymmetry = FullyAsymmetric
      , archExpectedTopology = Unidirectional
      , archExpectedTemporal = PeriodicUntilTerm
      , archExpectedBreach = EscalationLadder 2
      , archPartyCount = 2
      , archRequiresRecursion = True
      , archMinOptionality = 0.0
      , archMaxOptionality = 0.1
      }

    -- Exchange instruments
  , ACTUSArchetype
      { archActusCode = "FXOUT"
      , archLabel = "Foreign Exchange Outright"
      , archExpectedSymmetry = Symmetric
      , archExpectedTopology = Bidirectional
      , archExpectedTemporal = PointToPoint
      , archExpectedBreach = SimpleBreach
      , archPartyCount = 2
      , archRequiresRecursion = False
      , archMinOptionality = 0.0
      , archMaxOptionality = 0.2
      }
  , ACTUSArchetype
      { archActusCode = "SWAPS"
      , archLabel = "Swap"
      , archExpectedSymmetry = Symmetric
      , archExpectedTopology = Bidirectional
      , archExpectedTemporal = PeriodicUntilTerm
      , archExpectedBreach = SimpleBreach
      , archPartyCount = 2
      , archRequiresRecursion = False
      , archMinOptionality = 0.0
      , archMaxOptionality = 0.3
      }

    -- Contingent instruments
  , ACTUSArchetype
      { archActusCode = "OPTNS"
      , archLabel = "Option"
      , archExpectedSymmetry = AsymmetricWithRights
      , archExpectedTopology = Unidirectional
      , archExpectedTemporal = EventDriven
      , archExpectedBreach = NoBreachPath
      , archPartyCount = 2
      , archRequiresRecursion = False
      , archMinOptionality = 0.4
      , archMaxOptionality = 1.0
      }
  , ACTUSArchetype
      { archActusCode = "CEG"
      , archLabel = "Credit Enhancement Guarantee"
      , archExpectedSymmetry = AsymmetricWithRights
      , archExpectedTopology = Unidirectional
      , archExpectedTemporal = EventDriven
      , archExpectedBreach = NoBreachPath
      , archPartyCount = 2
      , archRequiresRecursion = False
      , archMinOptionality = 0.3
      , archMaxOptionality = 0.8
      }
  ]

-- | Compute structural distance (lower = better match)
structuralDistance :: ContractEssence -> ACTUSArchetype -> Double
structuralDistance essence archetype =
  let weights = distanceWeights
      components =
        [ (weights.wSymmetry,    symmetryDistance essence.ceSymmetry archetype.archExpectedSymmetry)
        , (weights.wTopology,    topologyDistance essence.ceFlowTopology archetype.archExpectedTopology)
        , (weights.wTemporal,    temporalDistance essence.ceTemporalShape archetype.archExpectedTemporal)
        , (weights.wBreach,      breachDistance essence.ceBreachTopology archetype.archExpectedBreach)
        , (weights.wPartyCount,  partyCountDistance essence.cePartyCount archetype.archPartyCount)
        , (weights.wRecursion,   recursionDistance essence archetype)
        , (weights.wOptionality, optionalityDistance essence archetype)
        ]
  in sum [w * d | (w, d) <- components]

-- | Weights for each distance component
data DistanceWeights = DistanceWeights
  { wSymmetry    :: Double
  , wTopology    :: Double
  , wTemporal    :: Double
  , wBreach      :: Double
  , wPartyCount  :: Double
  , wRecursion   :: Double
  , wOptionality :: Double
  }

distanceWeights :: DistanceWeights
distanceWeights = DistanceWeights
  { wSymmetry    = 0.25   -- Most important: who owes whom
  , wTopology    = 0.20   -- Direction of flows
  , wTemporal    = 0.20   -- Time structure
  , wBreach      = 0.10   -- Failure handling
  , wPartyCount  = 0.10   -- Number of parties
  , wRecursion   = 0.10   -- Self-reference
  , wOptionality = 0.05   -- MAY vs MUST
  }

-- | Distance between symmetry types (0.0 = exact match)
symmetryDistance :: Symmetry -> Symmetry -> Double
symmetryDistance s1 s2
  | s1 == s2 = 0.0
  | otherwise = case (s1, s2) of
      (FullyAsymmetric, AsymmetricWithRights) -> 0.3
      (AsymmetricWithRights, FullyAsymmetric) -> 0.3
      (Symmetric, _) -> 1.0
      (_, Symmetric) -> 1.0
      _ -> 0.5

-- | Distance between topology types
topologyDistance :: FlowTopology -> FlowTopology -> Double
topologyDistance t1 t2
  | t1 == t2 = 0.0
  | otherwise = case (t1, t2) of
      (Unidirectional, Bidirectional) -> 0.8
      (Bidirectional, Unidirectional) -> 0.8
      (Circular, _) -> 0.5
      (StarPattern _, _) -> 0.6
      _ -> 0.5

-- | Distance between temporal shapes
temporalDistance :: TemporalShape -> TemporalShape -> Double
temporalDistance ts1 ts2
  | ts1 == ts2 = 0.0
  | otherwise = case (ts1, ts2) of
      (PointToPoint, PeriodicUntilTerm) -> 0.5
      (PeriodicUntilTerm, RecursiveDecay) -> 0.3  -- Both involve repetition
      (RecursiveDecay, PeriodicUntilTerm) -> 0.3
      (EventDriven, _) -> 0.7
      (_, EventDriven) -> 0.7
      _ -> 0.5

-- | Distance between breach topologies
breachDistance :: BreachTopology -> BreachTopology -> Double
breachDistance b1 b2
  | b1 == b2 = 0.0
  | otherwise = case (b1, b2) of
      (SimpleBreach, EscalationLadder _) -> 0.4
      (EscalationLadder _, SimpleBreach) -> 0.4
      (NoBreachPath, _) -> 0.6
      (_, NoBreachPath) -> 0.6
      _ -> 0.5

-- | Distance based on party count
partyCountDistance :: Int -> Int -> Double
partyCountDistance actual expected
  | actual == expected = 0.0
  | otherwise = min 1.0 (fromIntegral (abs (actual - expected)) / 2.0)

-- | Distance based on recursion requirement
recursionDistance :: ContractEssence -> ACTUSArchetype -> Double
recursionDistance essence archetype =
  let hasRecursion = isJust essence.ceRecursionDepth
  in case (hasRecursion, archetype.archRequiresRecursion) of
    (True, True) -> 0.0
    (False, False) -> 0.0
    (True, False) -> 0.3   -- Has recursion but not required
    (False, True) -> 1.0   -- Missing required recursion

-- | Distance based on optionality level
optionalityDistance :: ContractEssence -> ACTUSArchetype -> Double
optionalityDistance essence archetype =
  let opt = essence.ceOptionalityDegree
      minOpt = archetype.archMinOptionality
      maxOpt = archetype.archMaxOptionality
  in if opt >= minOpt && opt <= maxOpt
     then 0.0
     else min 1.0 $ min (abs (opt - minOpt)) (abs (opt - maxOpt))

-- | Score essence against archetype (higher = better match)
scoreAgainstArchetype :: ContractEssence -> ACTUSArchetype -> (ACTUSType, Double)
scoreAgainstArchetype essence archetype =
  let distance = structuralDistance essence archetype
      confidence = max 0.0 (1.0 - distance)
  in (archetype.archActusCode, confidence)

-- | Convert distance to confidence (for reporting)
distanceToConfidence :: Double -> Double
distanceToConfidence d = max 0.0 (1.0 - d)
```

### Phase 4: Hybrid Classification

Integrate qualia-based analysis with the existing symbolic analyzer for best-of-both-worlds classification.

```haskell
-- L4/ACTUS/Analyzer.hs (updated)

-- | Classification strategy
data ClassificationStrategy
  = SymbolicOnly           -- Fast path: use existing pattern matching
  | QualiaOnly             -- Pure structural analysis
  | HybridSymbolicFirst    -- Try symbolic, fall back to qualia if low confidence
  | HybridQualiaFirst      -- Try qualia, validate with symbolic
  | Ensemble               -- Run both, combine scores
  deriving (Eq, Show)

-- | Configuration for hybrid analysis
data HybridConfig = HybridConfig
  { hcStrategy           :: ClassificationStrategy
  , hcSymbolicThreshold  :: Double   -- Use symbolic result if confidence >= this
  , hcQualiaWeight       :: Double   -- Weight for qualia score in ensemble
  , hcSymbolicWeight     :: Double   -- Weight for symbolic score in ensemble
  , hcMinConfidence      :: Double   -- Minimum confidence to report
  }

defaultHybridConfig :: HybridConfig
defaultHybridConfig = HybridConfig
  { hcStrategy = HybridSymbolicFirst
  , hcSymbolicThreshold = 0.85
  , hcQualiaWeight = 0.6
  , hcSymbolicWeight = 0.4
  , hcMinConfidence = 0.3
  }

-- | Run hybrid classification
classifyHybrid :: HybridConfig -> L4Features -> Module Resolved -> IO ClassificationResult
classifyHybrid config features mod' = case config.hcStrategy of
  SymbolicOnly -> classifySymbolic features

  QualiaOnly -> classifyQualia mod'

  HybridSymbolicFirst -> do
    symbolic <- classifySymbolic features
    if symbolic.primaryConfidence >= config.hcSymbolicThreshold
      then pure symbolic  -- High confidence symbolic, use it
      else do
        qualia <- classifyQualia mod'
        pure $ selectBest symbolic qualia

  HybridQualiaFirst -> do
    qualia <- classifyQualia mod'
    symbolic <- classifySymbolic features
    pure $ validateWithSymbolic qualia symbolic

  Ensemble -> do
    symbolic <- classifySymbolic features
    qualia <- classifyQualia mod'
    pure $ combineResults config symbolic qualia

-- | Pure qualia-based classification
classifyQualia :: Module Resolved -> IO ClassificationResult
classifyQualia mod' = do
  let og = extractObligationGraph mod'
      sg = buildStateGraph mod'
      essence = extractEssence og sg
      scores = classifyByEssence essence
      (bestType, bestConf) = head scores  -- Already sorted

  pure ClassificationResult
    { primaryActusType = Just bestType
    , primaryConfidence = bestConf
    , evidence = generateQualiaEvidence essence
    , classificationMethod = "qualia"
    , ..
    }

-- | Generate human-readable evidence from essence
generateQualiaEvidence :: ContractEssence -> [Evidence]
generateQualiaEvidence essence =
  [ Evidence
      { evFeature = "Party structure"
      , evDescription = describeSymmetry essence.ceSymmetry
      , evContribution = 0.25
      }
  , Evidence
      { evFeature = "Flow topology"
      , evDescription = describeTopology essence.ceFlowTopology
      , evContribution = 0.20
      }
  , Evidence
      { evFeature = "Temporal shape"
      , evDescription = describeTemporal essence.ceTemporalShape
      , evContribution = 0.20
      }
  , Evidence
      { evFeature = "Breach handling"
      , evDescription = describeBreach essence.ceBreachTopology
      , evContribution = 0.10
      }
  ]

describeSymmetry :: Symmetry -> Text
describeSymmetry = \case
  FullyAsymmetric -> "One party has all obligations (debtor/creditor pattern)"
  AsymmetricWithRights -> "One party has obligations, other has discretionary rights"
  Symmetric -> "Balanced mutual obligations between parties"
  MultiPartyHub -> "Central counterparty with multiple obligors"

describeTopology :: FlowTopology -> Text
describeTopology = \case
  Unidirectional -> "Value flows in one direction only"
  Bidirectional -> "Value flows both directions (exchange pattern)"
  Circular -> "Value flows in a cycle between parties"
  StarPattern n -> "Hub-and-spoke with " <> tshow n <> " parties"

describeTemporal :: TemporalShape -> Text
describeTemporal = \case
  PointToPoint -> "Single event (spot or forward transaction)"
  PeriodicUntilTerm -> "Repeating payments until maturity"
  PeriodicIndefinite -> "Perpetual periodic obligations"
  RecursiveDecay -> "Self-referential with diminishing amounts (amortization)"
  EventDriven -> "Contingent on external events (option exercise)"

describeBreach :: BreachTopology -> Text
describeBreach = \case
  SimpleBreach -> "Direct breach state on failure"
  EscalationLadder n -> tshow n <> "-step escalation process"
  CureBranch -> "Opportunity to cure before terminal breach"
  NoBreachPath -> "No failure path (fully discretionary)"

-- | Select best result between symbolic and qualia
selectBest :: ClassificationResult -> ClassificationResult -> ClassificationResult
selectBest sym qual
  | sym.primaryConfidence > qual.primaryConfidence = sym
  | otherwise = qual

-- | Validate qualia result with symbolic patterns
validateWithSymbolic :: ClassificationResult -> ClassificationResult -> ClassificationResult
validateWithSymbolic qualia symbolic =
  if qualia.primaryActusType == symbolic.primaryActusType
    then qualia { primaryConfidence = min 1.0 (qualia.primaryConfidence * 1.1) }  -- Boost if agreement
    else qualia { evidence = qualia.evidence ++ [disagreementNote symbolic] }

disagreementNote :: ClassificationResult -> Evidence
disagreementNote sym = Evidence
  { evFeature = "Symbolic disagreement"
  , evDescription = "Symbolic analysis suggests " <> fromMaybe "unknown" sym.primaryActusType
  , evContribution = -0.05
  }

-- | Combine results using weighted ensemble
combineResults :: HybridConfig -> ClassificationResult -> ClassificationResult -> ClassificationResult
combineResults config sym qual =
  let wSym = config.hcSymbolicWeight
      wQual = config.hcQualiaWeight
      combinedConf = wSym * sym.primaryConfidence + wQual * qual.primaryConfidence
      -- Use qualia type if confidence is higher, else symbolic
      bestType = if qual.primaryConfidence > sym.primaryConfidence
                 then qual.primaryActusType
                 else sym.primaryActusType
  in ClassificationResult
       { primaryActusType = bestType
       , primaryConfidence = combinedConf
       , evidence = qual.evidence ++ sym.evidence
       , classificationMethod = "ensemble"
       , ..
       }
```

### Supporting Types

Additional types referenced in the implementation:

```haskell
-- | Recursion pattern detected in contract
data RecursionPattern = RecursionPattern
  { rpDepth     :: Int           -- How deep is the recursion?
  , rpDecay     :: DecayPattern  -- How does value change?
  , rpTermination :: Maybe Text  -- Termination condition name
  }
  deriving (Eq, Show)

-- Helper for recursion depth
recursionDepth :: RecursionPattern -> Int
recursionDepth = rpDepth

-- | Transition data (extending StateGraph)
data Transition = Transition
  { transitionType :: TransitionType
  , sourceState    :: StateId
  , targetState    :: StateId
  , condition      :: Maybe Text
  }
  deriving (Eq, Show)
```

## Benefits

1. **Language agnostic**: Works regardless of variable naming conventions
2. **Robust to obfuscation**: Structure reveals essence even with obscured names
3. **Deeper understanding**: Captures *why* something is a loan, not just *that* it looks like one
4. **Novel contract detection**: Can identify contracts that don't fit existing types
5. **Cross-jurisdictional**: Same essence can have different legal terminology

## Challenges

1. **Computational complexity**: Graph analysis is more expensive than string matching
2. **Ambiguity resolution**: Some contracts may have similar structural signatures
3. **Incomplete information**: May need to infer missing structural details
4. **Hybrid contracts**: Real contracts often blend multiple archetypes

## Relationship to Current Implementation

The current `jl4-actus-analyzer` can serve as a **fast path**:
- If symbolic matching yields high confidence (>90%), use that result
- If symbolic matching is uncertain, fall back to qualia-based analysis
- Qualia analysis can also validate/confirm symbolic results

## Testing & Verification

### Unit Tests

```haskell
-- test/L4/ACTUS/Qualia/ObligationGraphSpec.hs

spec :: Spec
spec = describe "ObligationGraph extraction" $ do

  describe "flowRole" $ do
    it "detects NetDebtor when outDegree > inDegree" $ do
      let party = PartyNode 1 0 3 1 0
      flowRole party `shouldBe` NetDebtor

    it "detects NetCreditor when inDegree > outDegree" $ do
      let party = PartyNode 1 0 1 3 0
      flowRole party `shouldBe` NetCreditor

    it "detects Symmetric when degrees are equal and non-zero" $ do
      let party = PartyNode 1 0 2 2 0
      flowRole party `shouldBe` Symmetric

  describe "computeSymmetry" $ do
    it "returns FullyAsymmetric for promissory note pattern" $ do
      let og = mockPromissoryNoteGraph
      computeSymmetry og `shouldBe` FullyAsymmetric

    it "returns Symmetric for FX exchange pattern" $ do
      let og = mockFXExchangeGraph
      computeSymmetry og `shouldBe` Symmetric
```

### Integration Tests

```haskell
-- test/L4/ACTUS/Qualia/ClassificationSpec.hs

spec :: Spec
spec = describe "Qualia-based classification" $ do

  describe "promissory note" $ do
    it "classifies as ANN with high confidence" $ do
      mod' <- parseL4File "jl4/examples/legal/promissory-note.l4"
      result <- classifyQualia mod'
      result.primaryActusType `shouldBe` Just "ANN"
      result.primaryConfidence `shouldSatisfy` (> 0.8)

  describe "obfuscated promissory note" $ do
    it "still classifies correctly with renamed variables" $ do
      -- Same structure as promissory-note.l4, but all names changed
      mod' <- parseL4File "jl4/test/qualia/obfuscated-loan.l4"
      result <- classifyQualia mod'
      result.primaryActusType `shouldBe` Just "ANN"
      result.primaryConfidence `shouldSatisfy` (> 0.7)

  describe "IFEMA FX agreement" $ do
    it "classifies as FXOUT" $ do
      mods <- parseL4Directory "../actus2026/ifema/"
      let combined = combineModules mods
      result <- classifyQualia combined
      result.primaryActusType `shouldBe` Just "FXOUT"

  describe "hybrid classification" $ do
    it "improves confidence when symbolic and qualia agree" $ do
      mod' <- parseL4File "jl4/examples/legal/promissory-note.l4"
      let features = extractFeatures "promissory-note.l4" mod'
      hybrid <- classifyHybrid defaultHybridConfig features mod'
      symbolicOnly <- classifySymbolic features
      hybrid.primaryConfidence `shouldSatisfy` (>= symbolicOnly.primaryConfidence)
```

### Golden Tests

Create golden files capturing expected essence extraction:

```
test/golden/qualia/
├── promissory-note.essence.golden
├── fx-forward.essence.golden
├── interest-rate-swap.essence.golden
└── european-option.essence.golden
```

Example golden file format:
```yaml
# promissory-note.essence.golden
partyCount: 2
symmetry: FullyAsymmetric
flowTopology: Unidirectional
temporalShape: RecursiveDecay
optionalityDegree: 0.0
recursionDepth: 3
breachTopology: EscalationLadder 2
stateCount: 5
terminalModes: 2
classification:
  - type: ANN
    confidence: 0.92
  - type: LAM
    confidence: 0.85
  - type: PAM
    confidence: 0.78
```

## Future Work

### Near-term

- **Explanation generation**: Generate natural language explanations for classifications
  - "This contract is classified as ANN (Annuity) because:
    - Party A has unidirectional outgoing obligations to Party B
    - The obligation amount decreases recursively (amortization pattern)
    - There is a 2-step escalation ladder for breach handling"

- **Visualization**: Render contract essence as diagrams
  - Obligation graph with weighted edges
  - State machine with breach paths highlighted
  - Temporal flow diagram

### Medium-term

- **Machine learning augmentation**: Train ML models to refine archetype boundaries
  - Use extracted essence features as input vectors
  - Learn from corpus of correctly classified contracts
  - Identify new contract patterns that don't fit existing archetypes

- **Contract synthesis**: Given an archetype, generate valid L4 contract skeletons
  - Inverse of classification: archetype → essence → L4 skeleton
  - Useful for contract drafting assistance

- **Anomaly detection**: Flag contracts with unusual structural signatures
  - Contracts that are far from all known archetypes
  - Hybrid contracts blending multiple patterns
  - Potentially novel financial instruments

### Long-term

- **Temporal logic verification**: Use model checking to verify contract properties
  - "Can this contract reach a state where both parties have unfulfilled obligations?"
  - "Is it possible for the debt to never be repaid?"

- **Cross-jurisdictional mapping**: Map structural qualia to different legal frameworks
  - Same essence, different terminology across jurisdictions
  - Regulatory compliance checking by structure rather than labels

- **Contract evolution analysis**: Track how contract structures change over time
  - Amendment impact on essence
  - Migration path between contract types

## References

- ACTUS Algorithmic Contract Types: https://www.actusfrf.org/
- Hohfeld's Fundamental Legal Conceptions (rights/duties structure)
- Category theory for contract composition (functorial relationships between contract types)
- Process algebra for temporal obligation modeling
