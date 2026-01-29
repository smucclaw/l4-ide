# EVERY/EACH Quantifier Specification

**Status:** Draft
**Authors:** Meng Wong, with analysis from concurrency-legal-modeling agent
**Date:** 2025-01-26
**Branch:** mengwong/every-each

---

## Table of Contents

1. [Motivation](#1-motivation)
2. [Quick Reference](#2-quick-reference)
3. [Syntax](#3-syntax)
4. [Semantics](#4-semantics)
5. [Temporal Semantics](#5-temporal-semantics)
6. [Blame Attribution](#6-blame-attribution)
7. [Compositionality](#7-compositionality)
8. [Feature Interactions](#8-feature-interactions)
9. [Implementation](#9-implementation)
10. [Verification](#10-verification)
11. [Examples](#11-examples)
12. [Extended Use Cases](#12-extended-use-cases)
13. [Open Questions](#13-open-questions)
14. [Excluded Designs](#14-excluded-designs)
15. [Related Work](#15-related-work)
16. [Appendix: Legal Pattern Analysis](#appendix-a-legal-pattern-analysis)
17. [Appendix: Formal Grammar](#appendix-b-formal-grammar)

---

## 1. Motivation

### 1.1 The Problem

Legal contracts and legislation frequently contain clauses that range over quantified parties:

- "Each party shall keep confidential..."
- "Every shareholder must vote before the deadline..."
- "All signatories shall be bound by these terms..."

CSL (Hvitved's Contract Specification Language) provides primitives (conjunction ∧, disjunction ∨) to express such obligations, but the expression is verbose, requiring manual template instantiation for each party.

### 1.2 The Solution

L4 provides first-class quantification constructs that:

1. **Mirror natural legal language** (isomorphism with source text)
2. **Have precise formal semantics** (trace-based, following CSL's CSP lineage)
3. **Support proper blame attribution**
4. **Handle temporal forking** when permissions are exercised
5. **Compose cleanly** with existing L4 constructs (HENCE, LEST, IF/THEN/ELSE)

### 1.3 Intellectual Lineage

CSL's trace-based semantics descend primarily from **CSP** (Hoare), not CCS (Milner):

- Denotational semantics via traces (not operational/LTS)
- External choice operator
- Alphabet-based parallel composition
- Trace refinement as the equivalence notion

L4's quantifiers maintain this CSP lineage while adding legal-domain extensions (blame attribution, deadline handling, multi-party modeling).

### 1.4 Legislative Mode vs Contract Mode

L4 supports two distinct styles of regulative rules:

**Contract Mode** — Bilateral/multilateral agreements between **named parties** forming an explicit state machine:

```l4
§ `Sale Agreement`
PARTY buyer MUST `pay` purchasePrice WITHIN 30 days
HENCE PARTY seller MUST `deliver` goods WITHIN 14 days
      HENCE FULFILLED
      LEST  PARTY buyer MAY `claim refund`
LEST  BREACH
```

**Legislative Mode** — Regulations applying to **open classes** who satisfy predicates:

```l4
§ `Income Tax Act s.10`
EVERY Person p
  WHO p's annualIncome > 50000
 MUST `file tax return`
WITHIN `April 15 of following year`
```

| Aspect | Contract Mode | Legislative Mode |
|--------|---------------|------------------|
| **Parties** | Named, finite | Open class, potentially infinite |
| **Activation** | UPON events, HENCE chains | Predicate satisfaction |
| **State machine** | Explicit transitions | Implicit (rules fire when applicable) |
| **Query direction** | "What happens next?" | "What rules apply to entity X?" |
| **Typical keyword** | `PARTY alice` | `EVERY Person p WHO ...` |

**Mixed Mode:** The `EVERY...WHO` syntax can introduce a named party `p` that then participates in contract-mode `HENCE`/`LEST` chains.

---

## 2. Quick Reference

### EVERY vs EACH

| Keyword | Semantics | HENCE fires | Use when |
|---------|-----------|-------------|----------|
| `EVERY` | Barrier/Join | **Once** when all complete | All-or-nothing (resolution passes) |
| `EACH` | Fork/Distributive | **For each** completion | Individualized consequences |

Without HENCE/LEST, both are equivalent (pure distributive).

### WHO Clause Forms

| Form | Example | Notes |
|------|---------|-------|
| **Predicate** | `WHO `is adult`` | Bound variable supplied as **last** argument |
| **Explicit** | `WHO p's age >= 18` | Full boolean expression |

**Disambiguation:** If the bound variable appears in the expression, no implicit application occurs.

---

## 3. Syntax

### 3.1 Basic Form

```l4
EVERY Person p
  WHO predicate
 MUST action
WITHIN deadline
HENCE success_continuation
LEST  failure_continuation
```

The type annotation (`Person`) is required. The `WHO` clause is optional.

### 3.2 WHO Clause Semantics

Two forms are supported:

**Predicate form** — bound variable supplied as **last** argument (curried convention):
```l4
EVERY Person p
  WHO `has green hair`  -- becomes: `has green hair` p
 MUST `wash hair`
```

**Explicit form** — full boolean expression:
```l4
EVERY Person p
  WHO p's age >= 18
 MUST `pay taxes`
```

**Disambiguation rule:** If the bound variable appears anywhere in the WHO expression, no implicit application occurs. This prevents confusion when mixing styles.

**Multi-argument predicates:**
```l4
GIVEN `owes at least` IS A FUNCTION FROM NUMBER TO Person TO BOOLEAN
DECIDE `owes at least` amount p MEANS p's debt >= amount

-- Implicit: p doesn't appear, appended as LAST argument
EVERY Person p
  WHO `owes at least` 100  -- becomes: `owes at least` 100 p
 MUST `pay debt`

-- Explicit: p appears, must typecheck as BOOLEAN directly
EVERY Person p
  WHO `owes at least` 100 AND p's age > 21
 MUST `do something`
```

### 3.3 Cross-Party References

Nested quantifiers can reference outer-bound variables:

```l4
EACH p_x
    MAY    terminate
    HENCE  EVERY p_y
               WHO  `differs from` p_x   -- p_y /= p_x
               MUST settle_outstanding_accounts_with p_x
               WITHIN 30 days
```

### 3.4 Grammar

```
QuantifiedDeonton ::=
    Quantifier TypeName Variable [Filter]
        DeonticModal Action
        [TemporalConstraint]
        [HenceClause]
        [LestClause]

Quantifier ::= 'EVERY' | 'EACH' | 'ALL' | 'NO'
Filter ::= 'WHO' Predicate | 'WHERE' Predicate
DeonticModal ::= 'MUST' | 'MAY' | 'SHANT'
TemporalConstraint ::= 'WITHIN' Duration | 'BEFORE' Deadline | 'BY' Deadline
HenceClause ::= 'HENCE' Continuation
LestClause ::= 'LEST' Continuation
Continuation ::= Deonton | QuantifiedDeonton | 'FULFILLED' | 'BREACH'
```

---

## 4. Semantics

### 4.1 EVERY: Barrier Semantics

`EVERY` with HENCE/LEST collects completions at a barrier:

- **HENCE fires once** when ALL parties complete (join point)
- **LEST fires once** when the barrier becomes unachievable

```l4
EVERY director MAY approve
    HENCE resolution passes    -- fires once when ALL have approved
```

**CSP correspondence:** `(P1 ||| P2 ||| P3) ; HENCE` — interleaving with sequential composition.

### 4.2 EACH: Fork Semantics

`EACH` with HENCE/LEST fires continuations independently:

- **HENCE fires for each** party that completes
- **LEST fires for each** party that fails

```l4
EACH director MAY approve
    HENCE company MUST notify_board WITHIN 1 day  -- fires for EACH approval
```

**CSP correspondence:** `(P1 ; h1) ||| (P2 ; h2) ||| (P3 ; h3)` — each process has its own continuation.

### 4.3 Without Continuations

When there is no HENCE/LEST, EVERY and EACH are semantically equivalent:

```l4
EVERY p MUST sign  ≡  EACH p MUST sign
-- Both: (p1 MUST sign) ||| (p2 MUST sign) ||| ...
```

### 4.4 State Machine (EVERY Barrier)

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

### 4.5 Formal Semantics

**Semantic domain:**

```haskell
data BarrierObligation = BarrierObligation
    { boParties      :: Set Party
    , boAction       :: Party -> Action
    , boDeadline     :: Time
    , boCompleted    :: Set Party
    , boPending      :: Set Party
    , boFailed       :: Set Party
    , boEntryTime    :: Time
    , boHence        :: Maybe Continuation
    , boLest         :: Maybe Continuation
    , boStatus       :: BarrierStatus
    }

data BarrierStatus = Pending | Achieved Time | Failed Time (Set Party)
```

**Denotational semantics:**

```haskell
⟦EVERY v WHO pred MUST act WITHIN δ HENCE h LEST l⟧ :: Trace -> Time -> Verdict
⟦...⟧ tr t =
    let parties   = { p | p ∈ PartyType, pred(p) }
        completed = { p | p ∈ parties, (p, act(p), t') ∈ tr, t' ≤ δ }
        pending   = parties \ completed
        status    = if completed == parties
                    then Achieved (max completionTimes)
                    else if t > δ then Failed δ pending else Pending
    in case status of
        Achieved t_ach      -> (Success, spawn(h, t_ach))
        Failed t_f blame    -> (Breach t_f blame, spawn(l, t_f))
        Pending             -> (Pending, [])
```

---

## 5. Temporal Semantics

### 5.1 Reference Time for HENCE

When HENCE fires, continuation deadlines are relative to **last completion time**:

```
EVERY p MUST sign WITHIN 30 HENCE escrow_agent MUST release_funds WITHIN 5

Timeline:
t=0:  Obligation entered
t=10: Party A signs
t=20: Party B signs
t=25: Party C signs  ← barrier achieved, t_last = 25
t=25: HENCE spawns: escrow_agent's deadline = 25 + 5 = 30
```

### 5.2 Reference Time for LEST

When LEST fires, continuation deadlines are relative to **failure time**:
- Deadline failure: `t_ref = deadline`
- Early failure detection: `t_ref = detection_time`

### 5.3 Temporal Forking (MAY Exercise)

When a party exercises a MAY, HENCE obligations activate relative to exercise time:

```l4
EACH p_x MAY terminate
    HENCE EVERY p_y WHO `differs from` p_x
              MUST settle_with p_x WITHIN 30
```

If A terminates at t=10 and B terminates at t=15:
- A's termination: C & B must settle with A by t=40
- B's termination: C & A must settle with B by t=45

These are **independent obligation contexts** (CSP interleaving).

---

## 6. Blame Attribution

### 6.1 Precise Blame

When LEST fires, blame is attributed to exactly those who didn't complete:

```haskell
computeBlame bo tr = bo.parties `Set.difference` (completedParties bo tr)
```

**Example:** Parties {A, B, C}; A and B complete; C doesn't → Blame: {C}

### 6.2 Causal Blame

When a party's failure is caused by another:

```haskell
data BlameAttribution = BlameAttribution
    { baDirectBlame :: Set Party   -- simply didn't perform
    , baCausalBlame :: Set Party   -- caused others' failure
    , baExcused     :: Set Party   -- excused due to causation
    }
```

### 6.3 Indexed Blame (EACH)

For `EACH p MUST X`, each obligation has independent blame tracking:

```haskell
data Verdict = Success | Breach Time (Set Party) | IndexedBreach Time (Map Party BlameInfo)
```

---

## 7. Compositionality

### 7.1 Nesting Quantifiers

```l4
EVERY seller s MUST deliver(s)
    HENCE EVERY buyer b MUST pay(amount_for s b) WITHIN 30
```

Inner barrier spawns when outer achieves; scope chain captures `s`.

### 7.2 With Conditionals

```l4
IF condition THEN EVERY p MUST X ELSE EVERY p MUST Y
```

### 7.3 With RAND/ROR

```l4
(EVERY seller MUST deliver HENCE ...) RAND (EVERY buyer MUST pay HENCE ...)
```

### 7.4 With Pattern Matching

```l4
GIVEN parties IS A LIST OF Party
`sign in order` MEANS
    CONSIDER parties
        WHEN []          THEN FULFILLED
        WHEN (p :: rest) THEN PARTY p MUST sign HENCE `sign in order` rest
```

### 7.5 MAY and Correlative Obligations

A contractual MAY creates correlative obligations on counterparties (Hvitved's insight):

```l4
EACH p_x MAY terminate
    HENCE EVERY p_y WHO `differs from` p_x
              MUST settle_outstanding_accounts_with p_x
```

Expansion for {A, B, C}:
- A MAY terminate HENCE (B MUST settle_with A AND C MUST settle_with A)
- B MAY terminate HENCE (A MUST settle_with B AND C MUST settle_with B)
- C MAY terminate HENCE (A MUST settle_with C AND B MUST settle_with C)

---

## 8. Feature Interactions

### 8.1 With UPON

```l4
UPON `receiving invoice`
EVERY Customer c WHO `is premium member`
 MUST `pay within` 30 days
```

The `UPON` clause precedes `EVERY`, consistent with existing regulative syntax.

### 8.2 With GIVEN

```l4
GIVEN amount IS A NUMBER
EVERY Person p WHO `owes at least` amount
 MUST pay amount
```

Desugars to:
```l4
GIVEN amount IS A NUMBER, p IS A Person
PARTY p IF `owes at least` amount p  -- p is LAST argument
 MUST pay amount
```

### 8.3 With DEONTIC Reification

The [HOMOICONICITY-SPEC](HOMOICONICITY-SPEC.md) proposes treating deontic positions as first-class values. This affects `EVERY...WHO`:

**Instantiation strategies:**
- **Lazy (Option A):** Evaluate `EVERY` rules on query. Recommended for unbounded types.
- **Eager (Option B):** Pre-populate registry when parties enter. Recommended for bounded enums.

**Higher-order operations** can target quantified deontics:

```l4
§ `Tax Relief Power`
PARTY parliament MAY
  WAIVE (EVERY Person p WHO p's age >= 65 MUST `pay taxes`)
  PROVIDED `budget permits`
```

### 8.4 With Powers Hierarchy

`EVERY...WHO` can be used at multiple levels of power hierarchy:

```l4
-- Base permission
EVERY Person p MAY `move freely`

-- First-order power: officers can restrict
EVERY Officer o WHO `is authorized`
  MAY REVOKE (PARTY person MAY `move freely`)
  PROVIDED `lawful grounds exist`

-- Second-order power: legislature grants officer powers
PARTY parliament MAY
  GRANT (EVERY Officer o WHO `meets criteria`
           MAY REVOKE (PARTY person MAY `move freely`))
```

---

## 9. Implementation

### 9.1 Runtime State

```haskell
data BarrierRuntime = BarrierRuntime
    { brParties     :: Set Party
    , brCompleted   :: Set Party
    , brPending     :: Set Party
    , brFailed      :: Set Party
    , brDeadline    :: Time
    , brEntryTime   :: Time
    , brCompletions :: [(Party, Time)]
    }
```

### 9.2 Desugaring Strategy

1. **Parse** quantified deonton
2. **Expand** to barrier structure (not simple conjunction)
3. **Track** completion state at runtime
4. **Fire** HENCE/LEST at appropriate transitions
5. **Propagate** reference time to continuation

### 9.3 Scope Handling

```haskell
data Scope = Scope { scopeBindings :: Map Variable Value, scopeParent :: Maybe Scope }

resolve v scope = case Map.lookup v (scopeBindings scope) of
    Just val -> val
    Nothing  -> maybe (error "unbound") (resolve v) (scopeParent scope)
```

---

## 10. Verification

### 10.1 Complexity

- State space: O(2^n) completion states per barrier
- With k nested barriers: O(2^(n×k))
- PSPACE-complete for finite instances

### 10.2 Decidable Fragments

Verification is decidable under:
- Finite party sets
- Bounded temporal depth (HENCE nesting)
- Acyclic HENCE graphs
- Discrete time with bounded horizon

### 10.3 Static Analysis

Check for:
- **Deadlock:** Cyclic dependencies between barriers
- **Temporal impossibility:** Conflicting deadlines
- **Empty quantification:** Warn if domain might be empty

---

## 11. Examples

### 11.1 Mutual NDA

```l4
EVERY SigningParty p
 MUST `keep confidential` (`other party` p)'s confidentialInfo
LEST BREACH
```

### 11.2 Document Signing with Barrier

```l4
EVERY Director d WHO `is member of` board
 MUST sign_resolution WITHIN 14 days
HENCE company MUST file_with_registrar WITHIN 7 days
LEST  resolution_fails
```

### 11.3 Termination with Settlement

```l4
EACH p_x
    MAY    terminate upon 30 days notice
    HENCE  EVERY p_y WHO `differs from` p_x
               MUST settle_outstanding_accounts_with p_x WITHIN 30 days
               HENCE FULFILLED
               LEST  PARTY p_y MUST pay_penalty_to p_x
```

### 11.4 Sequential Signing by Seniority

```l4
GIVEN board IS A LIST OF Director

`sign in order` MEANS
    CONSIDER sortOn seniority board
        WHEN []          THEN FULFILLED
        WHEN (d :: rest) THEN PARTY d MUST sign WITHIN 7 days
                              HENCE `sign in order` rest
                              LEST signing_failed
```

---

## 12. Extended Use Cases

### 12.1 Age-Based Regulations

```l4
-- Voting rights
EVERY Person p
  WHO p's age >= 18 AND p's citizenship EQUALS Singapore
  MAY vote

-- Alcohol prohibition
EVERY Person p WHO p's age < 21
SHANT `purchase alcohol`
LEST `commit offense under` LiquorControlAct

-- Mandatory education
EVERY Person p WHO p's age >= 6 AND p's age <= 16
 MUST `attend school`
LEST PARTY p's parents MUST `pay truancy fine`

-- Senior benefits
EVERY Person p WHO p's age >= 65
  MAY `claim` seniorCitizenDiscount
```

### 12.2 Employment Law

```l4
-- Minimum wage
EVERY Employer e
 MUST `pay at least` minimumWage TO `each employee of` e

-- Anti-discrimination
EVERY Employer e
SHANT `discriminate based on` protectedCharacteristics IN `hiring decisions`
LEST `commit offense under` EmploymentAct
```

### 12.3 Tax Law

```l4
-- Progressive taxation
EVERY Person p WHO p's annualIncome > 0 AND p's annualIncome <= 20000
 MUST `pay tax at rate` 0%

EVERY Person p WHO p's annualIncome > 20000 AND p's annualIncome <= 40000
 MUST `pay tax at rate` 2%

EVERY Person p WHO p's annualIncome > 40000
 MUST `pay tax at rate` (progressiveTaxRate p's annualIncome)

-- GST registration
EVERY Business b WHO b's annualRevenue > 1000000
 MUST `register for GST` WITHIN 30 days OF `exceeding threshold`
LEST `pay penalty` (lateRegistrationPenalty b)
```

---

## 13. Open Questions

### 13.1 Early Failure Policy

Should early failure detection (before deadline) be default, opt-in, or contract-level?

**Recommendation:** Opt-in, as legal systems typically allow cure until deadline.

### 13.2 Partial Completion Visibility

Should intermediate completion state be observable?

```l4
IF (count_completed parties action) >= quorum THEN ...
```

**Recommendation:** Yes, via accessor functions on barrier state.

### 13.3 Synchronization Modifiers

For complex coordination:

```l4
EVERY p MUST X HENCE ... COORDINATED WITH other_barrier
```

**Recommendation:** Defer to future iteration.

---

## 14. Excluded Designs

### 14.1 First-Argument Insertion

**Rejected:** Supplying bound variable as **first** argument to predicates.

**Why:** Inconsistent with curried function convention; breaks partial application patterns like `owes_at_least 100 :: Person -> Boolean`.

### 14.2 Leading `'s` Accessor Form

**Rejected:** `WHO 's age >= 18` as shorthand for `WHO p's age >= 18`.

**Why:**
1. Adds third form, increasing cognitive load
2. Inconsistent with L4 where `'s` always follows an expression
3. Marginal benefit (only 2 characters shorter)
4. Predicate form already provides conciseness: `EVERY Person p WHO `is adult` MUST ...`

### 14.3 Implicit Type Inference Only

**Rejected as sole mechanism:** Inferring type from deontic context without explicit annotation.

**Why:**
1. Not all EVERY/EACH uses occur in typed deontic contexts
2. Legislative mode rules often stand alone
3. Explicit `EVERY Person p` is clearer

**However:** Type inference may be used as fallback when type is omitted and context provides it.

---

## 15. Related Work

- **Hvitved's CSL:** Trace-based contract semantics, blame assignment
- **CSP (Hoare):** Trace semantics, parallel composition, external choice
- **Petri Nets:** AND-join synchronization pattern
- **Hohfeld:** Jural correlatives (privilege/no-right)
- **Deontic Logic:** Obligation, permission, prohibition modalities
- **LegalRuleML:** `<Agent>` elements with optional qualifiers
- **Defeasible Deontic Logic:** Universal quantification with applicability conditions

---

## Appendix A: Legal Pattern Analysis

This appendix analyzes how quantified obligations appear in real legal texts.

### A.1 Obligation Patterns (MUST)

**Pattern A: Distributive Obligation (Most Common)**

> "Each party shall maintain the confidentiality of all Confidential Information."

Every individual party has their own separate obligation. If Alice breaches, only Alice is liable.

**CSP:** `CONF(Alice) ||| CONF(Bob) ||| CONF(Carol)` — pure interleaving.

**Pattern B: Constitutive Rule Masquerading as Obligation**

> "All directors must approve the resolution before it takes effect."

This is a **constitutive** rule ("must be"), not **regulative** ("must do"). Directors bear no penalty for not approving—the resolution simply doesn't pass.

**L4 representation using barrier:**
```l4
EVERY director MAY approve the_resolution
    HENCE the_resolution passes
```

**Pattern C: Joint and Several Liability**

> "The Guarantors shall be jointly and severally liable for the Debt."

Creditor can pursue any guarantor for full amount. Requires tracking shared resource.

**Status:** Future work — requires constructs beyond EVERY/EACH.

**Pattern D: Collective Action (Truly Joint)**

> "The parties shall jointly execute the Closing Documents."

Single synchronized event requiring all participants.

**Status:** Future work — proposed syntax: `ALL parties MUST JOINTLY execute closing_documents`

### A.2 Permission Patterns (MAY)

**Pattern E: Distributive Permission**

> "Each party may assign its rights under this Agreement with prior written consent."

Every party individually has permission; Alice's decision doesn't affect Bob's permission.

**Pattern F: First-to-Act Permission**

> "Any party may terminate this Agreement upon 30 days' written notice."

External choice — first event determines outcome for all.

**CSP:** `(terminate.Alice → TERMINATED) □ (terminate.Bob → TERMINATED) □ ...`

**Pattern G: Exhaustible Permission**

> "The Licensee may make up to three copies of the Software."

Permission with quota; each exercise consumes one unit.

### A.3 Prohibition Patterns (SHANT)

**Pattern H: Distributive Prohibition**

> "No party shall disclose Confidential Information to any third party."

Each party individually prohibited. Equivalent to "Each party shall not disclose..."

**Pattern I: Collective Prohibition**

> "The parties shall not collectively hold more than 49% of the voting shares."

Prohibition applies to aggregate; individual holdings fine if sum under threshold.

**Pattern J: Cross-Party Prohibition**

> "No party shall solicit the employees of any other party."

Creates n × (n-1) prohibition instances.

```l4
EACH p_x
    EACH p_y WHO `differs from` p_x
        SHANT `solicit employees of` p_y
```

### A.4 Coverage Summary

| Pattern | EVERY/EACH handles? |
|---------|---------------------|
| A: Distributive | ✓ Yes |
| B: Barrier | ✓ Yes |
| C: Joint & Several | ✗ Future work |
| D: Truly Joint | ✗ Future work |
| E-J: Permissions & Prohibitions | ✓ Yes |

---

## Appendix B: Formal Grammar

```ebnf
quantified_deonton ::=
    quantifier type_name variable [filter]
    deontic_modal action_expr
    [temporal_constraint]
    [hence_clause]
    [lest_clause]

quantifier ::= 'EVERY' | 'EACH' | 'ALL' | 'NO'
filter ::= 'WHO' predicate | 'WHERE' predicate
deontic_modal ::= 'MUST' | 'MAY' | 'SHANT'
temporal_constraint ::= 'WITHIN' duration_expr | 'BEFORE' time_expr | 'BY' time_expr
hence_clause ::= 'HENCE' continuation
lest_clause ::= 'LEST' continuation
continuation ::= deonton | quantified_deonton | 'FULFILLED' | 'BREACH'

predicate ::= boolean_expr
            | predicate 'AND' predicate
            | predicate 'OR' predicate
            | 'NOT' predicate
            | '(' predicate ')'
```
