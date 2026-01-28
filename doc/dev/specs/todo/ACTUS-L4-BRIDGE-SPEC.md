# ACTUS-L4 Compatibility Bridge Specification

## Executive Summary

This document proposes a compatibility bridge between [ACTUS](https://www.actusfrf.org) (Algorithmic Contract Types Unified Standards) and L4, enabling the L4 language to express, validate, and execute ACTUS-compliant financial contracts.

**Why this matters:** ACTUS provides the *what* (standardized financial contract semantics) while L4 provides the *how* (a DSL for legal/financial reasoning with formal verification). Together, they could create a powerful platform where financial contracts are both ACTUS-compliant (ensuring interoperability) and L4-expressed (enabling explainability, testing, and verification).

## Background

### What is ACTUS?

ACTUS is a global open standard that describes financial contracts algorithmically. It emerged from the 2008 financial crisis when neither executives nor regulators could understand the crisis as it unfolded. ACTUS addresses this by providing:

1. **Taxonomy** - 32 contract types covering virtually all financial instruments
2. **Data Dictionary** - ~100+ standardized terms (attributes) for contract parameters
3. **Event Types** - 23 standard events (payments, resets, exercises, etc.)
4. **State Variables** - Contract state tracking (notional, accrued interest, performance)
5. **Algorithmic Specification** - Mathematical definitions for schedule generation, state transitions, and payoff calculations

ACTUS is now used by the US Treasury's Office of Financial Research and is an ISO TC68 liaison member.

### Strategic Fit with L4

L4 and ACTUS share philosophical alignment:
- Both treat contracts as **executable specifications** rather than documents
- Both emphasize **deterministic, unambiguous** semantics
- Both support **temporal reasoning** about contract evolution
- Both enable **formal verification** of contract properties

However, they approach from different angles:
- **ACTUS**: Top-down standardization of financial contract *patterns*
- **L4**: Bottom-up formalization of arbitrary legal/financial *rules*

The bridge creates value in both directions:
- L4 gains access to battle-tested financial contract semantics
- ACTUS gains L4's explainability, verification, and web app generation

## ACTUS Technical Overview

### Contract Types (Taxonomy)

| Family | Contract Type | Acronym | Description |
|--------|---------------|---------|-------------|
| **Basic** | Principal at Maturity | PAM | Bullet bonds, term deposits |
| | Linear Amortizer | LAM | Amortizing loans with constant principal |
| | Negative Amortizer | NAM | ARM mortgages with deferred principal |
| | Annuity | ANN | Level-payment mortgages |
| | Call Money | CLM | Interbank loans with call features |
| | Cash | CSH | Cash positions |
| | Stock | STK | Equity instruments |
| | Undefined Maturity | UMP | Savings accounts, current accounts |
| | Commodity | COM | Physical commodities |
| **Combined** | Swap | SWAPS | Interest rate, currency swaps |
| | Plain Vanilla Swap | SWPPV | Standard fixed-float swaps |
| | Option | OPTNS | European, American, Bermudan |
| | Future | FUTUR | Exchange-traded futures |
| | FX Outright | FXOUT | Currency forwards |
| | Cap/Floor | CAPFL | Interest rate options |
| **Credit Enhancement** | Guarantee | CEG | Personal/government guarantees |
| | Collateral | CEC | Secured lending |
| | Margining | MAR | Variation/initial margin |

### Event Types

| Event | Acronym | Description |
|-------|---------|-------------|
| Initial Exchange | IED | Principal disbursement |
| Interest Payment | IP | Scheduled interest payment |
| Principal Redemption | PR | Scheduled principal repayment |
| Maturity | MD | Contract maturity |
| Rate Reset | RR | Variable rate fixing |
| Fee Payment | FP | Fee payment event |
| Dividend Payment | DV | Dividend distribution |
| Exercise | XD | Option exercise |
| Settlement | STD | Exercise settlement |
| Termination | TD | Early termination |
| Monitoring | AD | State evaluation |

### Key Contract Terms (Data Dictionary)

```
notionalPrincipal      : Real        -- Principal amount
nominalInterestRate    : Real        -- Interest rate
initialExchangeDate    : Timestamp   -- IED
maturityDate           : Timestamp   -- MD
cycleOfInterestPayment : Cycle       -- e.g., "P1ML0" = monthly
dayCountConvention     : Enum        -- A365, A360, 30E360, etc.
contractRole           : Enum        -- RPA (receive principal), RPL (pay principal)
currency               : ISO4217     -- USD, EUR, etc.
```

### State Variables

```
Nt    : Notional Principal        -- Outstanding principal
Ipac  : Accrued Interest          -- Interest accrued since last payment
Ipnr  : Nominal Interest Rate     -- Current applicable rate
Prf   : Contract Performance      -- PF/DL/DQ/DF/MA/TE
Sd    : Status Date               -- Current evaluation date
```

### Core Algorithms

ACTUS defines three core function types:

1. **Schedule Function S(anchor, cycle, end)** - Generates event schedules
2. **State Transition Function STF_event_contract()** - Updates state on events
3. **Payoff Function POF_event_contract()** - Calculates cash flows

## Proposed Bridge Architecture

### Layer 1: Type Mapping (actus-types.l4)

Create L4 types that directly mirror ACTUS data structures:

```l4
§ `ACTUS Core Types`

DECLARE `Contract Type` IS ONE OF
    PAM | LAM | NAM | ANN | CLM | CSH | STK | UMP | COM
    | SWAPS | SWPPV | OPTNS | FUTUR | FXOUT | CAPFL
    | CEG | CEC | MAR

DECLARE `Event Type` IS ONE OF
    IED | IP | PR | MD | RR | FP | DV | XD | STD | TD | AD

DECLARE `Contract Performance` IS ONE OF
    Performing | Delayed | Delinquent | Default | Matured | Terminated

DECLARE `Day Count Convention` IS ONE OF
    `Actual/365` | `Actual/360` | `30E/360` | `Actual/Actual`

DECLARE `Contract Role` IS ONE OF
    `Receive Principal and Pay Interest` AS RPA
    `Pay Principal and Receive Interest` AS RPL
```

### Layer 2: Contract Terms (actus-terms.l4)

```l4
§ `ACTUS Contract Terms`

DECLARE `ACTUS Contract` HAS
    `Contract ID`             IS A STRING
    `Contract Type`           IS A `Contract Type`
    `Status Date`             IS A DATE
    `Contract Deal Date`      IS A DATE
    `Currency`                IS A STRING
    `Notional Principal`      IS A NUMBER
    `Initial Exchange Date`   IS A DATE
    `Maturity Date`           IS A MAYBE DATE
    `Nominal Interest Rate`   IS A NUMBER
    `Day Count Convention`    IS A `Day Count Convention`
    `Contract Role`           IS A `Contract Role`
    -- Cycle terms
    `Interest Payment Cycle`  IS A MAYBE `Cycle`
    `Principal Redemption Cycle` IS A MAYBE `Cycle`
    `Rate Reset Cycle`        IS A MAYBE `Cycle`

DECLARE `Cycle` HAS
    `Period`                  IS A NUMBER
    `Period Unit`             IS A `Period Unit`
    `Stub`                    IS A `Stub Convention`

DECLARE `Period Unit` IS ONE OF
    Days | Weeks | Months | Years
```

### Layer 3: State Variables (actus-state.l4)

```l4
§ `ACTUS State Variables`

DECLARE `Contract State` HAS
    `Notional Principal`      IS A NUMBER      -- Nt
    `Accrued Interest`        IS A NUMBER      -- Ipac
    `Nominal Rate`            IS A NUMBER      -- Ipnr
    `Contract Performance`    IS A `Contract Performance`
    `Status Date`             IS A DATE
    `Maturity Date`           IS A MAYBE DATE
    `Fee Accrued`             IS A NUMBER
    `Interest Calculation Base` IS A NUMBER
```

### Layer 4: Events (actus-events.l4)

```l4
§ `ACTUS Events`

DECLARE `Contract Event` HAS
    `Event Type`              IS A `Event Type`
    `Event Date`              IS A DATE
    `Payoff`                  IS A NUMBER
    `Currency`                IS A STRING
    `Post-Event State`        IS A `Contract State`

§§ `Event Generation`

GIVEN contract IS AN `ACTUS Contract`
      fromDate IS A DATE
      toDate   IS A DATE
GIVETH A LIST OF `Contract Event`
`generate events for` contract fromDate toDate MEANS
    -- Schedule generation per ACTUS spec
    ...
```

### Layer 5: Payoff Functions (actus-payoff.l4)

```l4
§ `ACTUS Payoff Functions`

-- Year Fraction Calculator
GIVEN startDate IS A DATE
      endDate   IS A DATE
      convention IS A `Day Count Convention`
GIVETH A NUMBER
`year fraction` MEANS
    CONSIDER convention
    WHEN `Actual/365` THEN
        (DATE_SERIAL endDate MINUS DATE_SERIAL startDate) DIVIDED BY 365
    WHEN `Actual/360` THEN
        (DATE_SERIAL endDate MINUS DATE_SERIAL startDate) DIVIDED BY 360
    WHEN `30E/360` THEN
        -- 30E/360 calculation
        ...

-- Interest Payment Payoff (POF_IP_PAM)
GIVEN state    IS A `Contract State`
      contract IS AN `ACTUS Contract`
GIVETH A NUMBER
`payoff IP PAM` MEANS
    role TIMES state's `Accrued Interest`
    WHERE
        role MEANS
            IF contract's `Contract Role` EQUALS RPA
            THEN 1
            ELSE -1

-- Principal Redemption Payoff (POF_PR_LAM)
GIVEN state    IS A `Contract State`
      contract IS AN `ACTUS Contract`
GIVETH A NUMBER
`payoff PR LAM` MEANS
    role TIMES contract's `Next Principal Redemption Payment`
    WHERE
        role MEANS
            IF contract's `Contract Role` EQUALS RPA
            THEN 1
            ELSE -1
```

### Layer 6: State Transition Functions (actus-stf.l4)

```l4
§ `ACTUS State Transition Functions`

-- Interest Payment State Transition (STF_IP_PAM)
GIVEN preState  IS A `Contract State`
      eventDate IS A DATE
      contract  IS AN `ACTUS Contract`
GIVETH A `Contract State`
`state transition IP PAM` MEANS
    preState WITH
        `Accrued Interest` IS 0
        `Status Date`      IS eventDate

-- Principal Redemption State Transition (STF_PR_LAM)
GIVEN preState  IS A `Contract State`
      eventDate IS A DATE
      contract  IS AN `ACTUS Contract`
GIVETH A `Contract State`
`state transition PR LAM` MEANS
    preState WITH
        `Notional Principal` IS preState's `Notional Principal`
                                   MINUS contract's `Next Principal Redemption Payment`
        `Accrued Interest`   IS 0
        `Status Date`        IS eventDate
```

### Layer 7: Contract Evaluation Engine (actus-engine.l4)

```l4
§ `ACTUS Evaluation Engine`

GIVEN contract   IS AN `ACTUS Contract`
      riskFactors IS A `Risk Factor Data`
      toDate     IS A DATE
GIVETH A LIST OF `Contract Event`
@export
`evaluate contract` MEANS
    -- 1. Initialize state at status date
    LET initialState = `initialize state` contract
    -- 2. Generate event schedule
    LET schedule = `generate schedule` contract toDate
    -- 3. Apply STF/POF for each event
    foldl (`apply event` contract riskFactors) initialState schedule
```

### Layer 8: JSON Interoperability (actus-json.l4)

```l4
§ `ACTUS JSON Import/Export`

-- Import ACTUS JSON test case format
GIVEN json IS A STRING
GIVETH AN EITHER STRING `ACTUS Contract`
`parse ACTUS contract` MEANS
    CONSIDER JSONDECODE json
    WHEN LEFT err THEN LEFT err
    WHEN RIGHT obj THEN
        -- Extract contract terms from JSON object
        ...

-- Export to ACTUS JSON format
GIVEN contract IS AN `ACTUS Contract`
GIVETH A STRING
`export ACTUS contract` MEANS
    JSONENCODE (contractToJson contract)
```

## Implementation Phases

### Phase 1: Foundation (4 weeks)

**Deliverables:**
1. `actus-types.l4` - Core type definitions
2. `actus-terms.l4` - Contract term structures
3. `actus-state.l4` - State variable definitions
4. `actus-events.l4` - Event type definitions

**Validation:**
- Types compile and typecheck
- Can construct sample PAM contract in L4

### Phase 2: Basic Contracts (6 weeks)

**Deliverables:**
1. PAM (Principal at Maturity) - Full implementation
2. LAM (Linear Amortizer) - Full implementation
3. ANN (Annuity) - Full implementation
4. Test suite using ACTUS reference tests

**Validation:**
- Pass all ACTUS reference tests for PAM, LAM, ANN
- Cash flows match ACTUS Java implementation

### Phase 3: Advanced Contracts (8 weeks)

**Deliverables:**
1. NAM, UMP, CLM - Remaining basic contracts
2. SWPPV - Plain vanilla swap
3. OPTNS - Options

**Validation:**
- Pass reference tests for all implemented contracts
- Cross-validate with ACTUS service API

### Phase 4: Integration (4 weeks)

**Deliverables:**
1. JSON import/export with ACTUS format
2. Decision service integration
3. Web app generation for contract analysis
4. Documentation and examples

**Validation:**
- Round-trip JSON tests
- Web app demonstrates contract evaluation
- User documentation complete

## Technical Considerations

### Year Fraction Calculations

ACTUS supports multiple day count conventions. L4's `daydate` library needs extension:

```l4
DECLARE `Day Count Convention` IS ONE OF
    AA    -- Actual/Actual (ISDA)
    A360  -- Actual/360
    A365  -- Actual/365
    `30E360`  -- 30E/360 (Eurobond)
    `30E360ISDA`  -- 30E/360 ISDA
    `BUS252`  -- Business/252
```

### Cycle Notation

ACTUS uses a compact notation for cycles: `P1ML0` = Period 1 Month Long stub.
L4 should parse and represent this:

```l4
-- Parse "P1ML0" -> Cycle { Period = 1, Unit = Months, Stub = Long }
GIVEN cycleStr IS A STRING
GIVETH A MAYBE `Cycle`
`parse cycle` MEANS ...
```

### Risk Factor Integration

Variable rate contracts require external market data:

```l4
DECLARE `Risk Factor Data` HAS
    `Market Object Code`    IS A STRING
    `Base Date`             IS A DATE
    `Observed Values`       IS A LIST OF `Observation`

DECLARE `Observation` HAS
    `Date`                  IS A DATE
    `Value`                 IS A NUMBER
```

### Formal Verification Opportunities

L4's formal methods can verify ACTUS contracts:

1. **Schedule completeness** - Every event type has required schedule entries
2. **State consistency** - State transitions preserve invariants (e.g., Nt >= 0)
3. **Cash flow balance** - Sum of payoffs matches expected totals
4. **No race conditions** - Events at same timestamp have defined ordering

Example verification property:
```l4
-- Property: Interest never accrues on negative notional
VERIFY FOR ALL contract, state:
    state's `Notional Principal` >= 0
    IMPLIES state's `Accrued Interest` >= 0
```

## Use Cases

### 1. Regulatory Compliance

A bank encodes its loan portfolio in ACTUS-L4:
- L4 generates explainable cash flow projections
- Auditors verify calculations against ACTUS reference
- OFR receives standardized position data

### 2. Smart Contract Development

DeFi protocol uses ACTUS-L4:
- Define bond tokenization as PAM contract
- L4 verifies no edge cases in interest calculation
- Generate Solidity code from verified specification

### 3. Insurance Product Design

Insurance company models investment products:
- Encode guarantee features using ACTUS CEG type
- L4's query planning identifies missing data
- Web app lets actuaries explore scenarios

### 4. Legal Contract Integration

Law firm drafts commercial loan:
- Natural language contract mapped to ACTUS terms
- L4 formal specification attached as schedule
- Disputes resolved by evaluating L4 model

## Comparison with Existing Work

| Approach | Focus | Verification | Explainability |
|----------|-------|--------------|----------------|
| ACTUS Java | Reference implementation | Testing only | Limited |
| Daml/Digital Asset | Smart contracts | Formal proofs | Some |
| Catala | Tax/benefits rules | Type checking | Good |
| **ACTUS-L4** | Financial + Legal | Full formal methods | Excellent |

## Resources

### ACTUS Documentation
- Technical Specification: https://github.com/actusfrf/actus-techspecs
- Data Dictionary: https://github.com/actusfrf/actus-dictionary
- Reference Tests: https://github.com/actusfrf/actus-tests
- Web Demo: https://demo.actusfrf.org

### Academic Background
- [ACTUS Wikipedia](https://en.wikipedia.org/wiki/Algorithmic_Contract_Types_Unified_Standards)
- [ACTUS Financial Protocol (Medium)](https://medium.com/at-par/the-actus-financial-protocol-839a3d8f52dc)
- [ZHAW Research Project](https://www.zhaw.ch/en/research/project/70327)

### Related L4 Modules
- `currency.l4` - ISO 4217 currency support
- `holdings.l4` - Debt/equity (ACTUS-inspired)
- `daydate.l4` - Date arithmetic
- `temporal-prelude.l4` - Temporal operators

## Existing L4 Contract: Promissory Note as ACTUS ANN

The file `jl4/examples/legal/promissory-note.l4` is a **de facto ACTUS ANN (Annuity) contract** already implemented in L4. This provides an excellent starting point for the bridge.

### Mapping: Promissory Note → ACTUS ANN

| L4 Term | ACTUS Term | Acronym | Value |
|---------|------------|---------|-------|
| `Note Date` | Contract Deal Date | CDD | Feb 4 2024 |
| `Principal Amount` | Notional Principal | NT | USD 25,000 |
| `Interest Rate Per Annum` | Nominal Interest Rate | IPNR | 15% |
| `Monthly Installments` | Number of PR Events | - | 12 |
| `Monthly Installment Amount` | Next Principal Redemption Payment | PRNXT | USD 2,256.46 |
| `Default After Days Not Paid Beyond Due` | Delinquency Period | DQP | 30 days |
| `Late Payment Penalty`'s `Grace Period Days` | Grace Period | GRP | 10 days |
| `Late Payment Penalty`'s `Interest Rate` | Penalty Rate | PYRT | 5% |
| `Total Repayment Amount` | Sum of all payoffs | - | USD 27,077.49 |

### Structural Correspondence

```
ACTUS ANN Contract                    L4 Promissory Note
━━━━━━━━━━━━━━━━━━━                   ━━━━━━━━━━━━━━━━━━
contractType: ANN                     implicit (annuity structure)
initialExchangeDate: 2024-02-04       `Note Date`
notionalPrincipal: 25000              `Principal Amount`'s Value
nominalInterestRate: 0.15             `Interest Rate Per Annum`
cycleOfPrincipalRedemption: P1ML0     `Monthly Installments` (12 months)
dayCountConvention: A365              implicit (Days in a year = 365)
nextPrincipalRedemptionPayment: 2256.46  `Monthly Installment Amount`'s Value
```

### Event Generation Correspondence

The L4 `#TRACE` evaluations correspond to ACTUS event sequences:

```
ACTUS Events for ANN              L4 #TRACE AT Day (...)
━━━━━━━━━━━━━━━━━━               ━━━━━━━━━━━━━━━━━━━━━━━
IED @ 2024-02-04                  AT Day (February 4 2025)
PR+IP @ 2024-03-04                DOES ... (USD 2256.46) AT Day (March 4 2025)
PR+IP @ 2024-04-04                DOES ... (USD 2256.46) AT Day (April 4 2025)
...                               ...
MD @ 2025-02-04                   AT Day (February 4 2026)
```

### What ACTUS Adds

The L4 promissory note already implements the core logic. An ACTUS-compliant version would add:

1. **Explicit Contract Type Declaration**
```l4
`This Contract` IS AN `ACTUS Contract` WITH
    `Contract Type`    IS ANN
    `Contract ID`      IS "promissory-note-001"
    ...
```

2. **Standard State Variables**
```l4
-- Current contract state per ACTUS
`Contract State at` date MEANS
    `Contract State` WITH
        `Notional Principal` IS `Outstanding Payment Amount`'s Value
        `Accrued Interest`   IS `accrued since last payment`
        `Contract Performance` IS `current performance status`
```

3. **Reference Test Compatibility**
```l4
-- Validate against ACTUS reference tests
#TEST `Contract Events` EQUALS
    [IED @ 2024-02-04: -25000,
     PR+IP @ 2024-03-04: 2256.46,
     ...]
```

4. **Day Count Convention**
```l4
`Day Count Convention` MEANS `Actual/365`  -- explicit, not assumed
```

### Recommended Refactoring Path

1. **Extract ACTUS types** - Move `Money`, `Payment`, `Penalty` to `actus-types.l4`
2. **Add state tracking** - Implement `Contract State` per ACTUS specification
3. **Generate events** - Implement schedule function that produces event list
4. **Validate** - Compare generated cash flows against `actus-tests-ann.json`

This existing contract demonstrates that **L4 is already capable of expressing ACTUS-level financial logic** - the bridge is primarily about standardizing the vocabulary and adding interoperability.

## Next Steps

1. **Review this specification** with L4 team
2. **Clone ACTUS repos** to ~/src/actusfrf/ (done)
3. **Study ACTUS techspecs PDF** for precise algorithm definitions
4. **Refactor promissory-note.l4** to use ACTUS types as proof of concept
5. **Validate against actus-tests-ann.json** reference tests
6. **Iterate** based on findings

---

*This specification was researched and drafted on 2025-01-28.*

*Sources:*
- [ACTUS Financial Research Foundation](https://www.actusfrf.org)
- [ACTUS Documentation](https://documentation.actusfrf.org/docs/intro)
- [ACTUS GitHub Organization](https://github.com/actusfrf)
- [ACTUS Wikipedia](https://en.wikipedia.org/wiki/Algorithmic_Contract_Types_Unified_Standards)
