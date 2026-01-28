# Qualia-Based Contract Classification

## Status: Proposed

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

### Phase 1: Graph Extraction

```haskell
data ObligationGraph = ObligationGraph
  { parties :: [PartyNode]          -- Structural roles
  , obligations :: [ObligationEdge] -- Directed edges
  , conditions :: [ConditionNode]   -- Branching points
  }

data PartyNode = PartyNode
  { nodeId :: Int
  , netFlow :: FlowDirection        -- Inferred from edges
  , hasOptionalRights :: Bool       -- Has MAY clauses
  }

data ObligationEdge = ObligationEdge
  { from :: PartyNode
  , to :: PartyNode
  , valueType :: ValueType          -- Currency, asset, service
  , timing :: TemporalPattern
  , conditionality :: Conditionality
  }
```

### Phase 2: Pattern Recognition

```haskell
data ContractEssence = ContractEssence
  { partySymmetry :: Symmetry
  , flowTopology :: FlowTopology
  , temporalShape :: TemporalShape
  , optionalityDegree :: Double     -- 0 = no options, 1 = fully optional
  , stateComplexity :: Int          -- Number of distinct states
  }

classifyByEssence :: ContractEssence -> [(ACTUSType, Double)]
classifyByEssence essence =
  map (scoreAgainstArchetype essence) actusArchetypes
```

### Phase 3: Archetype Matching

Instead of string matching, compute **structural distance** between the extracted essence and known archetypes:

```haskell
structuralDistance :: ContractEssence -> ACTUSArchetype -> Double
structuralDistance essence archetype = sum
  [ symmetryDistance essence.partySymmetry archetype.expectedSymmetry
  , topologyDistance essence.flowTopology archetype.expectedTopology
  , temporalDistance essence.temporalShape archetype.expectedTemporal
  ]
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

## Future Work

- **Machine learning**: Train on corpus of classified contracts to learn archetype boundaries
- **Explanation generation**: "This is a loan because Party A has net outgoing obligations with temporal decay"
- **Contract synthesis**: Given an archetype, generate valid L4 contract skeletons
- **Anomaly detection**: Flag contracts with unusual structural signatures

## References

- ACTUS Algorithmic Contract Types: https://www.actusfrf.org/
- Hohfeld's Fundamental Legal Conceptions (rights/duties structure)
- Category theory for contract composition (functorial relationships between contract types)
- Process algebra for temporal obligation modeling
