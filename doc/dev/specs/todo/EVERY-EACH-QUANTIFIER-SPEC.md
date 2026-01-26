# EVERY/EACH Quantifier Specification

**Status:** Draft
**Authors:** Meng Wong, with analysis from concurrency-legal-modeling agent
**Date:** 2025-01-26
**Branch:** mengwong/every-each

## 1. Motivation

### 1.1 The Problem

Legal contracts and legislation frequently contain clauses that range over quantified parties:

- "Each party shall keep confidential..."
- "Every shareholder must vote before the deadline..."
- "All signatories shall be bound by these terms..."

CSL (Hvitved's Contract Specification Language) provides primitives (conjunction ∧, disjunction ∨) to express such obligations, but the expression is verbose, requiring manual template instantiation for each party.

### 1.2 The Solution

L4 should provide first-class quantification constructs that:

1. Mirror natural legal language (isomorphism with source text)
2. Have precise formal semantics (trace-based, following CSL's CSP lineage)
3. Support proper blame attribution
4. Handle temporal forking when permissions are exercised
5. Compose cleanly with existing L4 constructs (HENCE, LEST, IF/THEN/ELSE)

### 1.3 Intellectual Lineage

CSL's trace-based semantics descend primarily from **CSP** (Hoare), not CCS (Milner). Key indicators:

- Denotational semantics via traces (not operational/LTS)
- External choice operator
- Alphabet-based parallel composition
- Trace refinement as the equivalence notion

L4's quantifiers should maintain this CSP lineage while adding legal-domain extensions (blame attribution, deadline handling, multi-party modeling).

## 2. Syntax

### 2.1 Basic Quantified Obligation

```l4
EVERY party IN parties WHO predicate
    MUST action
    WITHIN deadline
    HENCE success_continuation
    LEST failure_continuation
```

### 2.2 Distributive vs Collective: Real-World Legal Patterns

Before defining syntax, we must understand how quantified obligations appear in real legal texts and how they are interpreted.

#### 2.2.1 Common Legal Patterns and Their Interpretations

**Pattern A: Distributive Obligation (Most Common)**

> "Each party shall maintain the confidentiality of all Confidential Information."

**Lawyer's interpretation:** Every individual party has their own separate obligation. If Alice breaches, Alice is liable; Bob and Carol's obligations are unaffected.

**Layperson's interpretation:** Same - "each of us has to keep secrets."

**CSP formalization:**
```
CONF(Alice) ||| CONF(Bob) ||| CONF(Carol)
-- where CONF(p) = (maintain_confidentiality.p → CONF(p)) □ (breach.p → STOP)
```

Pure interleaving - each party's obligation is independent.

**Pattern B: Collective Obligation with Shared Outcome**

> "All directors must approve the resolution before it takes effect."

**Lawyer's interpretation:** The resolution requires unanimous approval. No single director's approval is sufficient; the collective body must achieve full approval.

**Layperson's interpretation:** "Everyone needs to say yes for this to go through."

**CSP formalization:**
```
(APPROVE(d1) ||| APPROVE(d2) ||| APPROVE(d3)) ; resolution_takes_effect
-- Sequential composition: all must complete before continuation
```

This is a **barrier** - the continuation only fires when all have approved.

**Pattern C: Joint and Several Liability**

> "The Guarantors shall be jointly and severally liable for the Debt."

**Lawyer's interpretation:** The creditor can pursue any guarantor for the full amount, or all of them proportionally. Each guarantor is individually liable for 100%, but the creditor can only collect 100% total.

**Layperson's interpretation:** "Any of us can be made to pay the whole thing."

**CSP formalization:** This requires tracking a shared resource (the debt amount):
```
GUARANTEE(debt) =
    (pay.g1?amount → GUARANTEE(debt - amount))
    □ (pay.g2?amount → GUARANTEE(debt - amount))
    □ (pay.g3?amount → GUARANTEE(debt - amount))
    □ ([debt <= 0] → SKIP)
```

**Pattern D: Collective Action (Truly Joint)**

> "The parties shall jointly execute the Closing Documents."

**Lawyer's interpretation:** All parties must participate in a single, coordinated act. This is not multiple independent signings; it's one event requiring all participants.

**Layperson's interpretation:** "We all sign together at the closing."

**CSP formalization:**
```
joint_execution.{Alice, Bob, Carol} → closing_complete
-- A single synchronized event requiring all parties
```

#### 2.2.2 Representing These Patterns in Current L4

**Pattern A (Distributive)** - Expressible but verbose:

```l4
-- Manual expansion for 3 parties:
(PARTY Alice MUST maintain_confidentiality WITHIN contract_term)
RAND
(PARTY Bob MUST maintain_confidentiality WITHIN contract_term)
RAND
(PARTY Carol MUST maintain_confidentiality WITHIN contract_term)
```

**Problems:**
- Verbose: O(n) clauses for n parties
- Error-prone: easy to miss a party or introduce inconsistencies
- Not isomorphic: source text says "each party" once; L4 repeats it n times
- Maintenance burden: adding a party requires adding another clause

**Pattern B (Barrier)** - Expressible but very verbose:

```l4
-- Using recursion over a list:
GIVEN directors IS A LIST OF Director
`all must approve` MEANS
    CONSIDER directors
        WHEN []          THEN resolution_takes_effect
        WHEN (d :: rest) THEN
            PARTY d MUST approve WITHIN 14 days
            HENCE `all must approve` rest
            LEST resolution_fails
```

**Problems:**
- Imposes artificial sequencing (d1 must approve before d2 can)
- The legal text implies parallel, independent approvals converging at a barrier
- HENCE chains don't naturally express "all complete, then continue"
- Blame attribution is per-step, not "who among the set failed"

**Pattern C (Joint and Several)** - Difficult to express:

```l4
-- Would need explicit state tracking:
GIVEN debt IS A NUMBER
      guarantors IS A SET OF Party
`guarantee` MEANS
    IF debt > 0 THEN
        -- But how to express "any of them may pay any amount"?
        -- And track cumulative payments?
        -- This requires external state management
```

**Problems:**
- L4's deontic model doesn't naturally handle shared mutable state
- "Any may satisfy" is disjunctive permission with cumulative effects
- Current primitives don't compose well for this pattern

**Pattern D (Truly Joint Action)** - Not directly expressible:

```l4
-- No way to express "single synchronized action by all parties"
-- Would need to model as:
PARTY (parties_as_collective_entity) MUST execute_closing
-- But L4's PARTY expects an individual, not a set
```

**Problems:**
- L4's PARTY construct takes a single entity
- No primitive for "synchronized multi-party action"
- Would require defining a synthetic collective entity

#### 2.2.3 Why New Syntax is Needed

The analysis above reveals that current L4 primitives are:

| Pattern | Expressible? | Ergonomic? | Isomorphic? |
|---------|--------------|------------|-------------|
| A: Distributive | Yes | No (verbose) | No |
| B: Barrier | Partially | No (forces sequencing) | No |
| C: Joint & Several | With difficulty | No | No |
| D: Truly Joint | No | N/A | N/A |

**The EVERY/EACH syntax addresses patterns A and B directly:**

```l4
-- Pattern A: Distributive (no HENCE/LEST barrier)
EACH party MUST maintain_confidentiality WITHIN contract_term

-- Pattern B: Barrier (with HENCE/LEST)
EVERY director MUST approve WITHIN 14 days
    HENCE resolution_takes_effect
    LEST resolution_fails
```

**Benefits:**
- **Concise:** One clause regardless of party count
- **Isomorphic:** Mirrors source legal text structure
- **Correct semantics:** Barrier behavior for Pattern B is built-in
- **Proper blame:** Non-completers identified automatically

#### 2.2.4 EVERY and EACH as Synonyms

Given the above, we define:

Both `EVERY` and `EACH` are **distributive quantifiers** with identical semantics:

```l4
EVERY party MUST sign    ≡    EACH party MUST sign
```

The choice between them is stylistic, preserving isomorphism with source text that may use either word.

#### 2.2.5 Collective Semantics (Future Work)

Patterns C and D require additional constructs beyond EVERY/EACH:

```l4
-- Pattern C: Joint and Several (proposed future syntax)
JOINTLY AND SEVERALLY guarantors MUST pay debt
    UNTIL debt_satisfied

-- Pattern D: Truly Joint (proposed future syntax)
ALL parties MUST JOINTLY execute closing_documents
```

These collective patterns are deferred to a future specification. For now, EVERY/EACH addresses the most common patterns (A and B) which cover the majority of real-world quantified obligations.

### 2.3 Cross-Party References

```l4
EACH party p_x MAY terminate
    HENCE EVERY party p_y WHO is not p_x
        MUST settle_outstanding_accounts_with p_x
        WITHIN 30 days
```

The `WHO is not p_x` clause filters the inner quantifier's domain relative to the outer binding.

### 2.4 Full Grammar

```
QuantifiedDeonton ::=
    Quantifier Variable 'IN' Set [Filter]
        DeonticModal Action
        [TemporalConstraint]
        [HenceClause]
        [LestClause]

Quantifier ::= 'EVERY' | 'EACH' | 'ALL' | 'NO'

Filter ::= 'WHO' Predicate
         | 'WHERE' Predicate

DeonticModal ::= 'MUST' | 'MAY' | 'SHANT'

TemporalConstraint ::= 'WITHIN' Duration
                     | 'BEFORE' Deadline
                     | 'BY' Deadline

HenceClause ::= 'HENCE' Continuation

LestClause ::= 'LEST' Continuation

Continuation ::= Deonton
               | QuantifiedDeonton
               | 'FULFILLED'
               | 'BREACH'
```

## 3. Semantics Overview

### 3.1 The Barrier Model

Quantified deontons with HENCE/LEST have **barrier semantics**:

- **HENCE fires** when ALL parties complete (join point)
- **LEST fires** when the barrier becomes unachievable (deadline or early failure)

This is NOT equivalent to simple conjunction expansion:

```l4
-- Simple conjunction (WRONG - each has independent continuation):
(p1 MUST X HENCE h1 LEST l1) AND (p2 MUST X HENCE h2 LEST l2)

-- Quantified with barrier (CORRECT - shared continuations):
EVERY p MUST X HENCE shared_h LEST shared_l
```

### 3.2 Process Algebra Correspondence

| Model | Representation |
|-------|----------------|
| **Petri Net** | AND-join: all input places must have tokens for transition to fire |
| **CSP** | `(P1 ||| P2 ||| P3) ; HENCE` — interleaving with sequential composition |
| **Counting** | Semaphore initialized to n; HENCE fires when count reaches 0 |

### 3.3 State Transitions

```
                    Pending
        (completed=∅, pending=parties, failed=∅)
                        │
           ┌────────────┼────────────┐
           │            │            │
     party completes   party fails   deadline passes
           │            │            │
           ▼            ▼            ▼
        Pending      Pending       Failed
    (completed∪={p}) (failed∪={p}) (blame=pending∪failed)
           │                         │
           │ pending=∅               │
           ▼                         ▼
       Achieved                   LEST fires
    (t_last=max(times))          (blame set)
           │
           ▼
      HENCE fires
    (clock starts at t_last)
```

## 4. Formal Semantics

### 4.1 Semantic Domain

```haskell
data BarrierObligation = BarrierObligation
    { boParties      :: Set Party           -- obligated parties
    , boAction       :: Party -> Action     -- parameterized action
    , boDeadline     :: Time                -- shared deadline
    , boCompleted    :: Set Party           -- parties who completed
    , boPending      :: Set Party           -- parties still pending
    , boFailed       :: Set Party           -- parties who cannot complete
    , boEntryTime    :: Time                -- when obligation was entered
    , boHence        :: Maybe Continuation  -- success continuation
    , boLest         :: Maybe Continuation  -- failure continuation
    , boStatus       :: BarrierStatus
    }

data BarrierStatus
    = Pending
    | Achieved Time              -- completion time (for HENCE clock)
    | Failed Time (Set Party)    -- failure time and blamed parties
```

### 4.2 Denotational Semantics

```haskell
⟦EVERY v IN S WHO pred MUST act WITHIN δ HENCE h LEST l⟧ :: Trace -> Time -> Verdict
⟦...⟧ tr t =
    let parties   = { p | p ∈ S, pred(p) }
        completed = { p | p ∈ parties, (p, act(p), t') ∈ tr, t' ≤ δ }
        pending   = parties \ completed

        status =
            if completed == parties
            then Achieved (max { t' | (p, _, t') ∈ completions })
            else if t > δ
            then Failed δ pending
            else Pending

    in case status of
        Achieved t_ach -> (Success, spawn(h, t_ach))
        Failed t_f blame -> (Breach t_f blame, spawn(l, t_f))
        Pending -> (Pending, [])
```

### 4.3 Operational Semantics (SOS Rules)

**Configuration:** `⟨B, σ, t⟩` — barrier B, trace σ, current time t

**Rule 1: Party Completes**
```
    p ∈ B.pending
    (p, B.action(p), t') ∈ σ
    t' ≤ B.deadline
─────────────────────────────────────────────────────────
    ⟨B, σ, t⟩ → ⟨B[completed ∪= {p}, pending \= {p}], σ, t⟩
```

**Rule 2: Barrier Achieved**
```
    B.pending = ∅
    B.failed = ∅
    t_last = max { t' | (p, _, t') ∈ completions }
────────────────────────────────────────────────────────
    ⟨B, σ, t⟩ → ⟨Achieved(t_last), σ, t⟩
    spawn(B.hence, t_last)
```

**Rule 3: Deadline Failure**
```
    t > B.deadline
    B.pending ≠ ∅
    blame = B.pending ∪ B.failed
─────────────────────────────────────────
    ⟨B, σ, t⟩ → ⟨Failed(B.deadline, blame), σ, t⟩
    spawn(B.lest, B.deadline)
```

**Rule 4: Early Failure (optional policy)**
```
    B.failurePolicy = EARLY_FAILURE_DETECTION
    ∃p ∈ B.pending. incapacitated(p)
─────────────────────────────────────────────
    ⟨B, σ, t⟩ → ⟨Failed(t, {p}), σ, t⟩
    spawn(B.lest, t)
```

## 5. Clock and Temporal Semantics

### 5.1 Reference Time for Continuations

When HENCE fires, continuation deadlines are relative to **last completion time**:

```
EVERY party MUST sign WITHIN 30
    HENCE escrow_agent MUST release_funds WITHIN 5

Timeline:
t=0:  Obligation entered
t=10: Party A signs
t=20: Party B signs
t=25: Party C signs  ← barrier achieved, t_last = 25
t=25: HENCE spawns with reference time = 25
      escrow_agent's deadline = 25 + 5 = 30
```

### 5.2 LEST Reference Time

When LEST fires, continuation deadlines are relative to **failure time**:

- If deadline failure: `t_ref = deadline`
- If early failure: `t_ref = detection_time`

### 5.3 Temporal Forking (MAY Exercise)

When a party exercises a MAY, the HENCE obligations activate relative to exercise time:

```l4
EACH party p_x MAY terminate
    HENCE EVERY party p_y WHO is not p_x
        MUST settle_with p_x WITHIN 30
```

If A terminates at t=10 and B terminates at t=15:
- A's termination spawns: C must settle with A by t=40, B must settle with A by t=40
- B's termination spawns: C must settle with B by t=45, A must settle with B by t=45

These are **independent obligation contexts** (CSP interleaving).

## 6. Blame Attribution

### 6.1 Precise Blame

When LEST fires, blame is attributed to exactly those who didn't complete:

```haskell
computeBlame :: BarrierObligation -> Trace -> Set Party
computeBlame bo tr =
    let completed = completedParties bo tr
    in bo.parties `Set.difference` completed
```

**Example:**
- Parties: {A, B, C}
- A and B complete; C doesn't
- Blame: {C}, not {A, B, C}

### 6.2 Causal Blame Analysis

When a party's failure is caused by another:

```haskell
data BlameAttribution = BlameAttribution
    { baDirectBlame   :: Set Party    -- simply didn't perform
    , baCausalBlame   :: Set Party    -- caused others' failure
    , baExcused       :: Set Party    -- excused due to causation
    }
```

**Example:**
- A completes
- B is prevented by A's wrongful interference
- C simply doesn't perform
- Result: Direct={C}, Causal={A}, Excused={B}, Final={A,C}

### 6.3 Indexed Blame for Distributive Obligations

For `EACH party MUST X` (no shared HENCE/LEST), each obligation has independent blame:

```haskell
data Verdict
    = Success
    | Breach Time (Set Party)
    | IndexedBreach Time (Map Party BlameInfo)  -- per-party tracking
```

## 7. MAY and Correlative Obligations

### 7.1 Hvitved's Insight

A contractual MAY is meaningful precisely because it imposes obligations on counterparties. Without this correlative, the MAY could be omitted without loss.

### 7.2 Correlative Structure

```l4
EACH party p_x MAY terminate
    HENCE EVERY party p_y WHO is not p_x
        MUST settle_outstanding_accounts_with p_x
```

The HENCE clause explicitly captures the correlative obligation. This is preferable to implicit correlatives because:

1. Uses existing HENCE machinery
2. Explicit about what the correlative requires
3. Composes naturally with other constructs

### 7.3 Expansion

For parties {A, B, C}:

```l4
A MAY terminate HENCE (B MUST settle_with A AND C MUST settle_with A)
B MAY terminate HENCE (A MUST settle_with B AND C MUST settle_with B)
C MAY terminate HENCE (A MUST settle_with C AND B MUST settle_with C)
```

Total: 3 permissions, each with 2 correlative obligations = 3 + 6 = 9 deontic atoms.

## 8. Compositionality

### 8.1 Nesting Quantifiers

```l4
EVERY seller s MUST deliver(s)
    HENCE EVERY buyer b MUST pay(amount_for s b) WITHIN 30
```

The inner barrier spawns when the outer barrier achieves, with:
- `entryTime = outer.achievementTime`
- Scope chain captures `s` binding for inner body

### 8.2 With IF/THEN/ELSE

```l4
IF condition THEN
    EVERY party MUST X
ELSE
    EVERY party MUST Y
```

Standard conditional; the quantified deontons are in the branches.

### 8.3 With RAND/ROR

```l4
(EVERY seller MUST deliver HENCE ...)
RAND
(EVERY buyer MUST pay HENCE ...)
```

Both quantified obligations must be satisfied (regulative conjunction).

### 8.4 With Pattern Matching

```l4
GIVEN parties IS A LIST OF Party
`sign in order` MEANS
    CONSIDER parties
        WHEN []          THEN FULFILLED
        WHEN (p :: rest) THEN PARTY p MUST sign
                              HENCE `sign in order` rest
```

Recursion over lists to build sequential HENCE chains. This is already expressible in L4 without new syntax.

## 9. Comparison: Barrier vs Simple Expansion

### 9.1 Simple Expansion (No Barrier)

```l4
-- Desugars each obligation independently:
(p1 MUST X HENCE h1 LEST l1) AND (p2 MUST X HENCE h2 LEST l2) AND ...
```

- Each obligation has its own HENCE/LEST
- No synchronization
- Blame per-obligation

### 9.2 Barrier Expansion (Shared HENCE/LEST)

```l4
EVERY p MUST X HENCE h LEST l
```

- Single shared HENCE (fires when all complete)
- Single shared LEST (fires when any fails by deadline)
- Blame is the set of non-completers

### 9.3 When to Use Which

| Pattern | Semantics | Use Case |
|---------|-----------|----------|
| `EACH p MUST X` (no HENCE/LEST) | Independent obligations | Independent compliance |
| `EVERY p MUST X HENCE h LEST l` | Barrier with join | All-or-nothing transactions |
| `EACH p MUST X HENCE h(p)` | Per-party continuations | Individualized consequences |

## 10. Verification

### 10.1 Complexity

For n parties:
- State space: O(2^n) completion states per barrier
- With k nested barriers: O(2^(n*k))
- PSPACE-complete for finite instances

### 10.2 Decidable Fragments

Verification is decidable under:
- Finite party sets
- Bounded temporal depth (HENCE nesting)
- Acyclic HENCE graphs
- Discrete time with bounded horizon

### 10.3 Static Analysis

At contract analysis time, check for:
- **Deadlock**: Cyclic dependencies between barriers
- **Temporal impossibility**: Conflicting deadlines
- **Empty quantification**: Warn if domain might be empty

## 11. Implementation Notes

### 11.1 Runtime State

```haskell
data BarrierRuntime = BarrierRuntime
    { brParties     :: Set Party
    , brCompleted   :: Set Party
    , brPending     :: Set Party
    , brFailed      :: Set Party
    , brDeadline    :: Time
    , brEntryTime   :: Time
    , brCompletions :: [(Party, Time)]  -- for blame/timing
    }
```

### 11.2 Desugaring Strategy

1. **Parse** quantified deonton
2. **Expand** to barrier structure (not simple conjunction)
3. **Track** completion state at runtime
4. **Fire** HENCE/LEST at appropriate transitions
5. **Propagate** reference time to continuation

### 11.3 Scope Handling

For nested quantifiers, maintain scope chain:

```haskell
data Scope = Scope
    { scopeBindings :: Map Variable Value
    , scopeParent   :: Maybe Scope
    }

resolve :: Variable -> Scope -> Value
resolve v scope = case Map.lookup v (scopeBindings scope) of
    Just val -> val
    Nothing  -> maybe (error "unbound") (resolve v) (scopeParent scope)
```

## 12. Examples

### 12.1 Mutual NDA

```l4
GIVEN parties IS A SET OF Party
      confidential_info IS A SET OF Information

EVERY party p MUST keep_confidential confidential_info
    WITHIN contract_duration
    LEST PARTY p MUST pay_damages
```

### 12.2 Document Signing with Barrier

```l4
EVERY director MUST sign_resolution
    WITHIN 14 days
    HENCE company MUST file_with_registrar WITHIN 7 days
    LEST resolution_fails
```

The filing obligation only triggers when ALL directors have signed.

### 12.3 Termination with Settlement

```l4
EACH party p_x MAY terminate upon 30 days notice
    HENCE EVERY party p_y WHO is not p_x
        MUST settle_outstanding_accounts_with p_x
        WITHIN 30 days
        HENCE FULFILLED
        LEST PARTY p_y MUST pay_penalty_to p_x
```

### 12.4 Sequential Signing by Seniority

```l4
GIVEN board IS A LIST OF Director

`sign in order` MEANS
    CONSIDER sortOn seniority board
        WHEN []          THEN FULFILLED
        WHEN (d :: rest) THEN
            PARTY d MUST sign WITHIN 7 days
                HENCE `sign in order` rest
                LEST signing_failed
```

Uses existing recursion; no new syntax needed.

## 13. Open Questions

### 13.1 Early Failure Policy

Should early failure detection (before deadline) be:
- Default behavior?
- Opt-in via modifier?
- Contract-level configuration?

**Recommendation:** Opt-in, as legal systems typically allow cure until deadline.

### 13.2 Partial Completion Visibility

Should intermediate completion state be observable to other parts of the contract?

```l4
IF (count_completed parties action) >= quorum THEN ...
```

**Recommendation:** Yes, via accessor functions on barrier state.

### 13.3 Synchronization Modifiers

For complex coordination, consider explicit sync:

```l4
EVERY party MUST X
    HENCE ...
    COORDINATED WITH other_barrier
    SYNCHRONIZES ON final_settlement
```

**Recommendation:** Defer to future iteration; current design covers common cases.

## 14. Related Work

- **Hvitved's CSL**: Trace-based contract semantics, blame assignment
- **CSP (Hoare)**: Trace semantics, parallel composition, external choice
- **Petri Nets**: AND-join synchronization pattern
- **Hohfeld**: Jural correlatives (privilege/no-right)
- **Deontic Logic**: Obligation, permission, prohibition modalities

## 15. Appendix: Formal Grammar

```ebnf
quantified_deonton ::=
    quantifier variable 'IN' set_expr [filter]
    deontic_modal action_expr
    [temporal_constraint]
    [hence_clause]
    [lest_clause]

quantifier ::= 'EVERY' | 'EACH' | 'ALL' | 'NO'

filter ::= 'WHO' predicate | 'WHERE' predicate

deontic_modal ::= 'MUST' | 'MAY' | 'SHANT'

temporal_constraint ::= 'WITHIN' duration_expr
                      | 'BEFORE' time_expr
                      | 'BY' time_expr

hence_clause ::= 'HENCE' continuation

lest_clause ::= 'LEST' continuation

continuation ::= deonton | quantified_deonton | 'FULFILLED' | 'BREACH'

predicate ::= 'is' 'not' variable
            | 'is' 'in' set_expr
            | 'meets' predicate_name
            | predicate 'AND' predicate
            | predicate 'OR' predicate
            | 'NOT' predicate
            | '(' predicate ')'
```
