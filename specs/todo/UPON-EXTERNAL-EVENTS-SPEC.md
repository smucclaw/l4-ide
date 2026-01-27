# Specification: UPON and External Event Modeling in L4

**Status:** Draft
**Issue:** [#490 - how do we model `UPON Event`](https://github.com/smucclaw/l4-ide/issues/490)
**Branch:** `mengwong/upon-spec`
**Related:** `regulative.md`, `regulative-spec.org`
**Orthogonal to:** `HOMOICONICITY-SPEC.md` (can combine but neither requires the other)

## Executive Summary

This specification proposes a unified model for external events in L4 by treating them as **MAY actions by a distinguished Environment party**. This elegant solution:

1. Unifies internal party actions and external world events under a single formalism
2. Enables reasoning about causality, time, and non-determinism
3. Connects to established formal traditions (event calculus, situation calculus, process algebras)
4. Provides a foundation for the `UPON` keyword in regulative rules

The key insight from issue #490: an `UPON FloodEvent` clause can be modeled as `PARTY Environment MAY FloodEvent`, with subsequent obligations triggered via `HENCE`. The environment "chooses" which events occur, just as parties "choose" which actions to take.

## Motivation

### The Problem: External Events in Contracts

Legal contracts and statutes frequently depend on events beyond any party's control:

```
UPON a flood occurring, the Insurer MUST pay the Customer
UPON the death of the insured, the policy MUST pay out
UPON the expiration of 30 days, the offer lapses
UPON market price exceeding $X, the option becomes exercisable
UPON receipt of notice, the party MUST respond within 14 days
```

These **external events** differ from party actions:

| Party Actions                | External Events                       |
| ---------------------------- | ------------------------------------- |
| Volitional - party chooses   | Non-volitional - "happens to" parties |
| Attributed to specific party | No obvious "actor"                    |
| Can be obligated (MUST)      | Cannot be obligated                   |
| Can be permitted (MAY)       | N/A?                                  |
| Can be prohibited (SHANT)    | Cannot be prohibited                  |

Current L4 lacks a principled way to model external events. The `UPON` keyword appears in documentation but has no formal semantics.

### Issue #490: The Environment Party Solution

The GitHub discussion converged on modeling external events as actions by a special **Environment** party:

```l4
-- Instead of:
UPON FloodEvent
PARTY Insurer MUST pay Customer someMoney

-- We could write:
PARTY Environment MAY FloodEvent
HENCE
  PARTY Insurer MUST pay Customer someMoney
```

This reframes the question: external events are things the Environment **MAY** do (but need not). The Environment is not obligated to cause floods, deaths, or market movements - but it **may** do so, and when it does, consequences follow.

### Why This Works: Philosophical Foundations

The Environment-as-party model has deep roots:

1. **Game Theory:** Nature is a player making "moves" - selecting states of the world
2. **Decision Theory:** Decision problems include "chance nodes" where Nature acts
3. **Modal Logic:** Possible worlds are "chosen" by reality from accessible alternatives
4. **Process Algebra:** The environment is a process that synchronizes with system processes

The model treats reality itself as a (non-obligated, non-blameworthy) agent.

## Theoretical Background

### Event Calculus

The **Event Calculus** (Kowalski & Sergot 1986) models change over time:

- **Fluents:** Properties that can change (e.g., `alive(Person)`, `owes(A, B, Amount)`)
- **Events:** Instantaneous happenings that initiate or terminate fluents
- **Axioms:**
  - `Initiates(event, fluent, time)` - event starts fluent being true
  - `Terminates(event, fluent, time)` - event stops fluent being true
  - `HoldsAt(fluent, time)` - fluent is true at time

**Connection to L4:**

- L4 obligations are fluents: they come into existence and can be discharged
- L4 events (party actions, external events) initiate/terminate obligations
- The `UPON` keyword is an event trigger that initiates a fluent

```l4
-- Event calculus reading:
-- Initiates(FloodEvent, OwesPayment(Insurer, Customer), t)
UPON FloodEvent
PARTY Insurer MUST pay Customer someMoney
```

### Situation Calculus

The **Situation Calculus** (McCarthy 1963) models actions and their effects:

- **Situations:** Complete states of the world at a point in time
- **Actions:** Transform one situation into another: `do(action, situation) = newSituation`
- **Fluents:** Properties indexed by situation: `Holds(fluent, situation)`
- **Preconditions:** `Poss(action, situation)` - action is possible in situation

**Connection to L4:**

- A contract trace is a sequence of situations
- Party actions and Environment actions both transform situations
- Guards (`IF`, `PROVIDED`) are preconditions
- `HENCE`/`LEST` describe the resulting situation

```l4
-- Situation calculus reading:
-- Result of FloodEvent in situation s is situation s' where payment obligation holds
PARTY Environment MAY FloodEvent
  PROVIDED `policy is active`         -- Poss(FloodEvent, s)
HENCE                                 -- In do(FloodEvent, s):
  PARTY Insurer MUST pay Customer     --   Holds(MustPay(Insurer, Customer), s')
```

### Process Calculi: CCS and CSP

**CCS** (Milner 1980) and **CSP** (Hoare 1978) model concurrent communicating processes:

- **Processes:** Entities that perform actions and synchronize
- **Actions:** Events that processes can perform or participate in
- **Synchronization:** Processes "meet" on complementary actions
- **Choice:** Processes can offer alternatives (`P + Q`)
- **Composition:** Processes run in parallel (`P | Q`)

**Connection to L4:**

- Parties are processes
- The Environment is a process that performs uncontrollable events
- Contract execution is parallel composition: `Party1 | Party2 | ... | Environment`
- `UPON` is synchronization: the contract process synchronizes with Environment's action

```
-- CSP reading:
-- Contract synchronizes with Environment on FloodEvent
CONTRACT = FloodEvent -> (Insurer.pay -> FULFILLED)
ENVIRONMENT = FloodEvent -> ENVIRONMENT [] NoFlood -> ENVIRONMENT
SYSTEM = CONTRACT [|{FloodEvent}|] ENVIRONMENT
```

**Key insight from CSP:** The Environment offers **choices** (`[]`). It may or may not perform `FloodEvent`. The contract must be prepared for either possibility. This maps directly to `MAY`:

```l4
PARTY Environment MAY FloodEvent    -- Environment offers this as a choice
PARTY Environment MAY NoFlood       -- Also a valid choice
```

### The Frame Problem and Persistence

Both Event Calculus and Situation Calculus address the **frame problem**: what remains unchanged when an action occurs?

In L4:

- When `FloodEvent` occurs, obligations unrelated to floods persist
- Only the specific fluents initiated/terminated by the event change
- This is handled by L4's obligation registry (see `HOMOICONICITY-SPEC.md`)

## Proposed Design

### The Environment Party

L4 introduces a distinguished party representing external reality:

```l4
-- Built-in declaration (implicit in every module)
DECLARE Environment IS A Party
  WHERE Environment `cannot be obligated`
    AND Environment `cannot breach`
    AND Environment `represents external reality`
```

The Environment party:

| Property                | Value | Rationale                    |
| ----------------------- | ----- | ---------------------------- |
| Can be subject of MAY   | Yes   | Environment may cause events |
| Can be subject of MUST  | No    | Cannot obligate reality      |
| Can be subject of SHANT | No    | Cannot prohibit reality      |
| Can breach              | No    | No deontic force applies     |
| Actions have HENCE      | Yes   | Events have consequences     |
| Actions have LEST       | No    | No deadline/breach semantics |
| Appears in traces       | Yes   | Events are recorded          |

### UPON as Sugar for Environment MAY

The `UPON` keyword is syntactic sugar for an Environment MAY clause:

```l4
-- Surface syntax:
§ `Insurance Payout`
UPON FloodEvent
PARTY Insurer MUST pay Customer someMoney

-- Desugars to:
§ `Insurance Payout`
PARTY Environment MAY FloodEvent
HENCE
  PARTY Insurer MUST pay Customer someMoney
```

This desugaring:

1. Makes the semantics explicit
2. Reuses existing regulative rule infrastructure
3. Enables composition with other Environment events

### Event Parameterization

Events can carry data:

```l4
-- Parameterized event
UPON `market price reaches` threshold
PARTY OptionHolder MAY exercise option

-- Desugars to:
PARTY Environment MAY (`market price reaches` threshold)
HENCE
  PARTY OptionHolder MAY exercise option
```

The Environment "chooses" not just whether an event occurs but its parameters.

### Time as an Environment Action

The passage of time is modeled as Environment actions:

```l4
-- Time passing is an Environment event
PARTY Environment MAY `time passes to` futureDate

-- Deadlines trigger when time reaches them
UPON `time passes to` deadline
HENCE `deadline reached`
```

This unifies:

- Calendar events (specific dates)
- Durations (after 30 days)
- Relative deadlines (within 14 days of notice)

### Multiple Environment Events

The Environment can perform multiple events in various patterns:

```l4
-- Sequential events
PARTY Environment MAY EventA
HENCE
  PARTY Environment MAY EventB
  HENCE consequences

-- Concurrent events
PARTY Environment MAY EventA
PARTY Environment MAY EventB
-- Both may occur independently

-- Mutually exclusive events
PARTY Environment MAY
  EITHER `market rises`
  OR `market falls`
  OR `market unchanged`
```

### Environment Events in Traces

When the contract executes, Environment events appear in the trace:

```
Trace:
  t0: Contract begins
  t1: PARTY Environment DOES FloodEvent AT 2025-01-15
  t2: PARTY Insurer DOES pay Customer $100000 AT 2025-01-20
  t3: Contract FULFILLED
```

This enables:

- Audit trails that record external events
- Blame assignment (Environment cannot be blamed)
- Replay and simulation

## Syntax Specification

### UPON Clause

```
UponClause ::= 'UPON' EventExpr RuleBody

EventExpr ::= Expr                           -- simple event
            | Expr 'WITH' Bindings           -- parameterized event
            | Expr 'FROM' Party              -- event attributed to party (not Environment)

RuleBody ::= PartyClause                     -- standard regulative rule
           | 'THEN' Effects                  -- automatic effects (per HOMOICONICITY-SPEC)
```

### Examples

```l4
-- Simple external event
§ `Force Majeure`
UPON `force majeure event`
PARTY AffectedParty MAY `suspend performance`

-- Parameterized event
§ `Price Trigger`
UPON `market price exceeds` strikePrice
PARTY OptionHolder MAY exercise option

-- Event with temporal bound
§ `Offer Expiration`
UPON `30 days elapse from` offerDate
THEN EXPIRE offer

-- Event from specific source (not Environment)
§ `Notice Receipt`
UPON `notice received` FROM CounterParty
PARTY Recipient MUST respond WITHIN 14 DAYS

-- Combined external and temporal
§ `Insurance Claim Window`
UPON FloodEvent
PARTY PolicyHolder MAY `submit claim`
  WITHIN 90 DAYS
  HENCE `claim submitted`
  LEST `claim window expired`
```

### Time Expressions

```l4
-- Absolute time
UPON `time is` DATE 2025 06 30

-- Relative time (from contract start)
UPON `30 days elapse`

-- Relative time (from another event)
UPON `14 days elapse from` noticeDate

-- Periodic events
UPON `each month end`
PARTY Borrower MUST pay installment

-- Business days
UPON `5 business days elapse from` triggerDate
```

## Semantic Rules

### Rule 1: Environment MAY Semantics

Environment MAY clauses differ from party MAY clauses:

```
⟦ PARTY Environment MAY e HENCE c ⟧ =
  When event e occurs (chosen by Environment):
    Contract continues with c
  When event e does not occur:
    Contract continues without c
    (No breach, no LEST clause)
```

### Rule 2: Environment Cannot Be Obligated

```
⟦ PARTY Environment MUST e ⟧ = TYPE ERROR
⟦ PARTY Environment SHANT e ⟧ = TYPE ERROR

-- The type system rejects these constructs
```

### Rule 3: UPON Desugaring

```
⟦ UPON e RuleBody ⟧ = ⟦ PARTY Environment MAY e HENCE RuleBody ⟧
```

### Rule 4: Time as Environment Event

```
⟦ deadline D passed ⟧ = Environment has performed (time passes to D)

-- Deadline checking reduces to Environment event checking
```

### Rule 5: Event Ordering

When multiple Environment events could occur:

```
-- Non-deterministic choice: Environment picks one
PARTY Environment MAY e1
PARTY Environment MAY e2

-- If both occur, ordering is Environment's choice (unless constrained)
```

### Rule 6: Event-Initiated Obligations

Per Event Calculus semantics:

```
UPON e
PARTY p MUST a

-- Semantics:
Initiates(e, Obligation(p, a), t)
-- Event e at time t initiates obligation for p to do a
```

## Integration with Existing Constructs

### With HENCE/LEST (Core Integration)

The primary use of UPON is with standard regulative rules using HENCE/LEST:

```l4
-- Environment events lead to party obligations with HENCE/LEST
UPON FloodEvent
PARTY Insurer MUST pay Customer $1000000
  WITHIN 30 DAYS
  HENCE `claim paid`
  LEST `insurance breach`
```

The `LEST` applies to the party's failure to act, not to the Environment. This requires no new features beyond UPON itself.

### With Powers

```l4
-- Environment events can enable/disable powers
UPON `company goes public`
PARTY Employee MAY exercise stockOptions
  -- Power only exists after IPO event

UPON `lockup period ends`
PARTY Insider MAY sell shares
  -- Power becomes available
```

### With First-Class Obligations (Optional, Future)

If/when `HOMOICONICITY-SPEC.md` is implemented, UPON can trigger automatic effects:

```l4
-- OPTIONAL: Requires first-class obligations feature
UPON `FX rate published` rate
AUTOMATICALLY
  FOR EACH obligation IN `rate-dependent obligations`
    RECALCULATE obligation WITH `new rate` rate
```

This combination is powerful but **not required** for basic UPON functionality. The two features are orthogonal and can be implemented independently.

## Process Algebraic Semantics

### CSP Model

A contract with external events is modeled as parallel composition:

```
CONTRACT = UPON_EVENT?e -> CONSEQUENCE(e)
ENVIRONMENT = (event1 -> ENVIRONMENT) [] (event2 -> ENVIRONMENT) [] ...
SYSTEM = CONTRACT [|{events}|] ENVIRONMENT
```

Where:

- `[]` is external choice (Environment decides)
- `[|S|]` is parallel composition synchronizing on set S
- `event1`, `event2`, etc. are possible Environment events

### Synchronization Semantics

```l4
-- L4 clause:
UPON FloodEvent
PARTY Insurer MUST pay

-- CSP reading:
-- Contract offers to synchronize on FloodEvent
-- When Environment also offers FloodEvent, they synchronize
-- Then contract proceeds to Insurer.pay obligation
```

### Trace Semantics

A valid trace satisfies:

1. Every Environment event in trace was offered by Environment (always true - Environment can do anything)
2. Every party action in trace was either:
   - Permitted (MAY) and chosen by party, or
   - Required (MUST) and not yet past deadline, or
   - Prohibited (SHANT) - this is a breach
3. Temporal ordering is consistent with deadlines

## Implementation Considerations

UPON can be implemented independently of first-class obligations (HOMOICONICITY-SPEC).

### Phase 1: Basic UPON Support (Standalone)

1. **Add Environment to party system**

   - Built-in party constant
   - Type-level restriction preventing MUST/SHANT
   - Trace support for Environment events

2. **Implement UPON desugaring**

   - Parser support for `UPON expr`
   - Desugar to `PARTY Environment MAY expr HENCE ...`
   - Type check the desugared form

3. **Time events**
   - Model time passing as Environment events
   - Connect deadline checking to time events

**Estimated effort:** ~400 LOC parser, ~200 LOC type checker, ~300 LOC evaluator

### Phase 2: Parameterized Events

1. **Event parameters**

   - `UPON event WITH param = value`
   - Parameters available in rule body

2. **Event pattern matching**
   - `UPON (MarketPrice price) WHERE price > threshold`

**Estimated effort:** ~300 LOC

### Phase 3: Formal Semantics (Optional)

These are optional extensions for formal verification:

1. **Event Calculus integration**

   - Track what fluents each event initiates/terminates
   - HoldsAt queries for decision service

2. **CSP translation**
   - Generate CSP from L4 contracts
   - Model checking for deadlock/livelock

**Estimated effort:** ~800 LOC (optional)

## Examples

### Example 1: Insurance Contract

```l4
§ `Property Insurance`

DECLARE InsuredEvent IS ONE OF
    FloodEvent
    FireEvent
    TheftEvent
    `Other covered peril`

§ `Coverage Trigger`
UPON event IS AN InsuredEvent
  WHERE event `damages` insuredProperty
PARTY Insurer MUST
  pay PolicyHolder (`assessed damage` event)
  WITHIN 30 DAYS OF (`claim submitted` event)
  HENCE `claim paid`
  LEST `insurance breach`
       HENCE PARTY PolicyHolder MAY
               `pursue legal remedies`
               AND `report to regulator`

§ `Claim Window`
UPON event IS AN InsuredEvent
PARTY PolicyHolder MAY `submit claim` event
  WITHIN 90 DAYS
  HENCE `claim submitted` event
  LEST `claim window closed`
       HENCE `no payout required`
```

### Example 2: Option Contract

```l4
§ `Call Option`

-- Market price is an Environment event
§ `Market Price Update`
PARTY Environment MAY (`market price becomes` price)

-- Option becomes exercisable when price exceeds strike
§ `Option Exercise`
UPON (`market price becomes` price)
  WHERE price > strikePrice
PARTY OptionHolder MAY
  exercise option
  WITHIN expirationDate
  HENCE
    PARTY OptionWriter MUST
      sell underlyingAsset
      AT strikePrice
      WITHIN 3 BUSINESS DAYS
  LEST `option expired`
       HENCE `no obligation on writer`

-- Time passing can also expire the option
§ `Option Expiration`
UPON `time passes to` expirationDate
  WHERE NOT (`option exercised`)
THEN
  EXPIRE option
```

### Example 3: Loan with Acceleration

```l4
§ `Loan Agreement`

-- External events that constitute default
§ `Events of Default`
UPON
  EITHER `Borrower becomes insolvent`
  OR `Borrower fails to pay` installment
  OR `Borrower breaches covenant`
  OR `Material adverse change` affecting Borrower
PARTY Lender MAY
  `declare acceleration`
  HENCE
    PARTY Borrower MUST
      pay (`total outstanding balance`)
      WITHIN 5 DAYS
      LEST `loan default`

-- Passage of time triggers regular payments
§ `Monthly Payment`
UPON `each month end`
PARTY Borrower MUST
  pay (`monthly installment amount`)
  HENCE `installment paid`
  LEST `payment default`
       -- This triggers the default event above
```

### Example 4: SAFE Investment

```l4
§ `SAFE Agreement`

-- SAFE sits waiting for qualifying events
DECLARE QualifyingEvent IS ONE OF
    `Equity Financing` HAS amount IS A NUMBER
    `Liquidity Event`
    `Dissolution Event`

§ `Equity Financing Conversion`
UPON (`Equity Financing` WITH amount >= threshold)
PARTY Company MUST
  `issue shares to` Investor (`conversion shares` safeAmount conversionPrice)
  WITHIN 30 DAYS
  HENCE `SAFE converted`
  LEST `conversion breach`

§ `Liquidity Event Payout`
UPON `Liquidity Event`
PARTY Company MUST
  pay Investor (`liquidity payout` safeAmount)
  HENCE `SAFE satisfied`

§ `Dissolution`
UPON `Dissolution Event`
PARTY Company MUST
  pay Investor (`dissolution amount` safeAmount)
  BEFORE `distribution to shareholders`
  HENCE `SAFE satisfied`
```

### Example 5: Constitutional Provisions

```l4
§ `Emergency Powers Act`

-- The state of emergency is an Environment event
-- (or rather, an event that the government declares but
--  the constitution treats as triggering special powers)

§ `Emergency Declaration`
UPON `national emergency declared` BY `Head of State`
  WHERE `emergency conditions exist`
PARTY Executive MAY
  `issue emergency regulations`
  FOR DURATION `emergency period`
  HENCE `regulations in force`

§ `Emergency Termination`
UPON `emergency ends`
  EITHER BY `declaration` BY `Head of State`
  OR BY `90 days elapsing`
  OR BY `parliamentary vote`
PARTY Executive SHANT
  `issue emergency regulations`
  -- Emergency powers no longer available
```

## Open Questions

### Q1: Environment Event Sources

Should we distinguish different "sources" of Environment events?

```l4
-- Option A: Single undifferentiated Environment
PARTY Environment MAY FloodEvent
PARTY Environment MAY `market price changes`

-- Option B: Named environment aspects
PARTY Nature MAY FloodEvent
PARTY Market MAY `price changes`
PARTY Time MAY `deadline passes`
```

**Recommendation:** Start with single Environment, consider extension later.

### Q2: Environment Event Probability

Should Environment events have associated probabilities?

```l4
-- Option A: No probabilities (pure non-determinism)
PARTY Environment MAY FloodEvent

-- Option B: Probabilistic events
PARTY Environment MAY FloodEvent WITH PROBABILITY 0.01 PER YEAR
```

**Recommendation:** Start without probabilities. Add in future for risk modeling.

### Q3: Environment Event Observation

When are Environment events observed?

```l4
-- Option A: Immediate observation (events happen, everyone knows)
UPON FloodEvent  -- Everyone sees it immediately

-- Option B: Delayed/uncertain observation
UPON FloodEvent OBSERVED BY party
  -- Only triggers for party that observed it
```

**Recommendation:** Start with immediate observation. Consider observation semantics for more sophisticated modeling.

### Q4: Counterfactual Events

Can we reason about events that didn't happen?

```l4
-- "If there had been a flood, then..."
IF NOT (`FloodEvent occurred`)
THEN ...
```

**Recommendation:** Support negation of event occurrence in conditions.

### Q5: Event Composition

How do multiple simultaneous events compose?

```l4
-- Both events occur at same time
UPON FloodEvent AND FireEvent
THEN ...

-- At least one event occurs
UPON FloodEvent OR FireEvent
THEN ...
```

**Recommendation:** Support conjunction and disjunction of events.

## Relationship to Other Specifications

### regulative-spec.org

The UPON keyword is documented there but lacked formal semantics. This specification provides:

- Formal semantics via Environment MAY desugaring
- Connection to established formalisms
- Integration with the rest of the regulative rule system

### Temporal Features

UPON integrates with L4's temporal features:

- `UPON time event` triggers time-based rules
- Time deadlines reduce to time events by Environment
- Multi-temporal evaluation contexts work with UPON

### HOMOICONICITY-SPEC.md (Orthogonal)

UPON and first-class obligations are **orthogonal features**:

|                         | UPON alone                          | Homoiconicity alone                    | Combined                                        |
| ----------------------- | ----------------------------------- | -------------------------------------- | ----------------------------------------------- |
| **What it does**        | Triggers rules from external events | Manipulates obligations as values      | External events trigger obligation manipulation |
| **Requires the other?** | No                                  | No                                     | Uses both                                       |
| **Example**             | `UPON flood PARTY p MUST pay`       | `PARTY c MAY WAIVE (PARTY d MUST pay)` | `UPON event AUTOMATICALLY CREATE obligation`    |

Each can be implemented and used independently. The combination is powerful but optional.

## Conclusion

The Environment-as-party model elegantly unifies:

1. Party actions (volitional, deontic)
2. External events (non-volitional, non-deontic)
3. Time passage (universal, mechanical)

By treating external reality as a party that **MAY** (but need not) perform events, we:

- Reuse existing regulative rule infrastructure
- Connect to established formal traditions
- Enable uniform trace representation
- Support composition and formal verification

The `UPON` keyword becomes syntactic sugar for this uniform model, providing a natural way to express event-triggered rules while maintaining formal rigor.

## References

### Foundational

- Kowalski, R. & Sergot, M. (1986). "A Logic-based Calculus of Events." _New Generation Computing_.
- McCarthy, J. (1963). "Situations, Actions, and Causal Laws." Stanford AI Memo.
- Milner, R. (1980). "A Calculus of Communicating Systems." Springer LNCS.
- Hoare, C.A.R. (1978). "Communicating Sequential Processes." _CACM_.

### Contracts and Deontics

- Hvitved, T. (2012). "Contract Formalisation and Modular Implementation of Domain-Specific Languages." PhD Thesis.
- Prisacariu, C. & Schneider, G. (2012). "A Dynamic Deontic Logic for Complex Contracts."

### L4 Internal

- `regulative.md` - Regulative rule design notes
- `regulative-spec.org` - Regulative rule specification
- `HOMOICONICITY-SPEC.md` - First-class obligations
- GitHub Issue #490 - Original UPON discussion
