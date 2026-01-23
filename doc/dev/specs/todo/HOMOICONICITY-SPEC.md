# Specification: Homoiconicity and Higher-Order Obligations for L4

**Status:** Draft
**Issue:** TBD
**Related:** `regulative.md` (conceptual background), `regulative-spec.org`, `PATTERN-MATCHING-SPEC.md`

## Executive Summary

L4 already has rich syntax for expressing obligations (`PARTY p MUST action BY deadline HENCE ... LEST ...`), but these are currently **static declarations** - you write them in source code, and they define state machine transitions. This specification proposes **reifying** that existing syntax so obligations can also be treated as **runtime values** that can be queried, filtered, transformed, and dynamically created or canceled.

This enables legal patterns like novation netting (IFEMA Section 3.3), where obligations must be dynamically matched by criteria, canceled, and replaced with new computed obligations - operations that require treating obligations as manipulable data, not just static declarations.

The core insight is that legal instruments frequently treat obligations themselves as objects of discourse: they can be assigned, transferred, netted, novated, discharged, or converted. A language for computational law must support this level of abstraction.

## Motivation

### The Novation Problem: IFEMA Section 3.3

IFEMA 1997 Section 3.3(a) states:

> "If the Parties enter into an FX Transaction through a pair of Novation Netting Offices giving rise to a Currency Obligation for the same Value Date and in the same Currency as a then existing Currency Obligation between the same pair of Novation Netting Offices, then **immediately upon entering into such FX Transaction, each such Currency Obligation shall automatically and without further action be individually canceled and simultaneously replaced by a new Currency Obligation**..."

This single sentence requires L4 to express:

1. **Pattern matching on obligations** - identifying obligations that share currency, value date, and office pair
2. **Automatic cancellation** - obligations cease to exist without explicit party action
3. **Automatic creation** - a new obligation springs into existence
4. **Netting arithmetic** - the new obligation amount is computed from canceled obligations
5. **All happening simultaneously** - no intermediate states

Current L4 regulative syntax can express individual obligations, but cannot express these _operations on_ obligations because:

- Obligations exist only as **static syntax** (AST nodes like `PARTY p MUST action`), not as **runtime values** that can be inspected, filtered, or transformed
- There is no mechanism to refer to "all obligations matching criteria X"
- There is no mechanism to "cancel" an obligation programmatically
- There is no mechanism to "create" an obligation as the result of a function

### Broader Legal Patterns Requiring First-Class Obligations

Novation is just one instance of a broader pattern. Legal instruments frequently:

| Pattern          | Description                                        | Example                                    |
| ---------------- | -------------------------------------------------- | ------------------------------------------ |
| **Novation**     | Replace existing obligations with new ones         | IFEMA 3.3, ISDA Close-out                  |
| **Assignment**   | Transfer obligations to third parties              | Debt assignment, factoring                 |
| **Set-off**      | Cancel mutual obligations                          | Bankruptcy set-off rights                  |
| **Subrogation**  | Substitute one obligee for another                 | Insurance subrogation                      |
| **Conversion**   | Transform one type of obligation into another      | Convertible notes, warrants                |
| **Discharge**    | Extinguish obligations by performance or agreement | Payment, release agreements                |
| **Acceleration** | Make future obligations immediately due            | Loan default acceleration                  |
| **Modification** | Change terms of existing obligations               | Loan restructuring                         |
| **Procure**      | Obligate controlled third party to perform         | Parent company procuring subsidiary action |

All of these require treating obligations as first-class values that can be:

- Passed as arguments to functions
- Returned from functions
- Stored in data structures (lists, maps)
- Created and destroyed dynamically
- Pattern matched and filtered
- Composed (combined, split, netted)

### The Homoiconicity Connection

The `regulative.md` document identifies this need under "Homoiconicity, Introspection, Reflection, Reification":

> "Annoyingly, some rules are phrased in a way that blurs the boundary between object level and assertion level: 'If any rule in this section would cause undue hardship as a result of tight deadlines, the Commissioner may, upon application, extend deadlines at his discretion.'"

This is exactly the pattern: a rule that refers to other rules as data, inspects their properties (deadlines), and modifies them (extends). The legal text treats rules as first-class objects.

### Jurisprudential Foundations: Higher-Order Legal Relations

The need for higher-order operations in legal formalization has deep roots in analytical jurisprudence. Several legal theorists have developed formal frameworks for understanding how legal powers operate at multiple levels:

#### Hohfeld's Jural Relations (1913)

Wesley Newcomb Hohfeld's _Fundamental Legal Conceptions_ identified eight atomic legal relations, including the crucial distinction between **rights** (first-order) and **powers** (second-order). In Hohfeld's framework:

- A **right** correlates with a duty (A has a right that B do X ↔ B has a duty to A to do X)
- A **power** correlates with a liability (A has a power to change B's legal position ↔ B is liable to have their position changed by A)

Crucially, powers are operations on legal relations themselves - they create, modify, or extinguish rights and duties. This is precisely the higher-order structure we need: powers take legal relations as arguments and return new legal relations.

#### Kelsen's Hierarchy of Norms (1934)

Hans Kelsen's _Pure Theory of Law_ formalized law as a hierarchy of norms (_Stufenbau_), where each norm derives its validity from a higher norm that authorized its creation. The **empowerment chain** runs from individual legal acts up through statutes, constitutions, and ultimately to the _Grundnorm_ (basic norm).

Key insight for L4: A legal actor can only create valid norms if empowered by a higher-level norm. This is a chain of higher-order functions:

- Constitution empowers Parliament to create statutes
- Statutes empower agencies to create regulations
- Regulations empower officers to create individual orders

Each level is a power to create powers (or duties) at the next level down.

#### Hart's Secondary Rules (1961)

H.L.A. Hart's _The Concept of Law_ distinguished **primary rules** (duty-imposing) from **secondary rules** (power-conferring). Secondary rules are explicitly higher-order:

- **Rules of recognition** - identify what counts as valid law
- **Rules of change** - authorize creation/modification/extinction of primary rules
- **Rules of adjudication** - empower determinations about rule violations

Hart's key insight: a legal system without secondary rules would be static, uncertain, and inefficient. Power-conferring rules are essential infrastructure.

#### Raz's Normative Powers (1972-)

Joseph Raz further analyzed normative powers, distinguishing:

- **Basic normative powers** - powers that exist independent of other powers
- **Chained normative powers** - powers created by the exercise of other powers

This directly maps to our need for higher-order operations: a chained power is a power to create powers, formalized as a function from powers to powers.

#### The Hierarchical Pattern: Police Detention Example

Consider the chain of legal authority for police detention:

1. **Base level:** Every person has a **liberty** (Hohfeldian privilege) of freedom of movement
2. **First-order power:** A police officer has a **power** to detain, which creates a duty on the person to remain (negating their liberty)
3. **Second-order power:** The legislature has a **power** to grant detention powers to police officers
4. **Third-order power:** The constitution has a **power** to grant legislative powers to the legislature
5. **Meta-level:** The _Grundnorm_ (or social fact of acceptance) validates the constitution

In L4 terms, using nested composition to show the higher-order structure:

```l4
-- Liberty at base level (first-order: what a party may do)
§ `Freedom of Movement`
PARTY person MAY leave

-- Police power to revoke liberty (second-order: power over what parties may do)
§ `Police Detention`
PARTY officer MAY
  REVOKE (PARTY person MAY leave)
  PROVIDED officer `is on duty`
      AND officer `has reasonable suspicion of` person

-- Legislative power to grant police powers (third-order: power to grant powers)
§ `Police Powers Act`
PARTY parliament MAY
  GRANT (PARTY officer MAY
           REVOKE (PARTY person MAY leave)
           PROVIDED officer `is on duty`
               AND officer `has reasonable suspicion of` person)
  PROVIDED `bill passed by majority`

-- Constitutional power to empower legislature (fourth-order: power over power-granting)
§ `Constitutional Legislative Authority`
PARTY constitution MAY
  GRANT (PARTY parliament MAY
           GRANT (PARTY officer MAY
                    REVOKE (PARTY person MAY leave)))
  SUBJECT TO `Bill of Rights`
```

The nested structure `GRANT (... GRANT (... REVOKE (...)))` directly reflects the Kelsenian hierarchy of norms. Each level wraps the level below it, and the outermost `PROVIDED`/`SUBJECT TO` clauses constrain all inner levels.

This example demonstrates why L4 needs to support arbitrarily nested powers: real legal systems routinely involve powers over powers over powers.

## Requirements

### R1: Deontic Positions as Values

Deontic positions (obligations, permissions, prohibitions) must be representable as L4 values with a concrete type:

```
DEONTIC : Type
```

> **Implementation note:** Internally, this is represented by the Haskell type `Deonton` (from Greek δέον "duty" + -on suffix, analogous to "electron" or "photon" as an atomic unit of normative content). The surface type `PROVISION` remains as a deprecated alias for backward compatibility.

A deontic value should capture:

- The obligor (who must perform)
- The obligee (who is owed performance)
- The action to be performed
- Temporal bounds (deadline, earliest permitted time)
- The current state (pending, performed, breached, discharged)
- Provenance (which rule or transaction created it)

### R2: Collections of Deontic Positions

L4 must support:

- `LIST OF DEONTIC` - ordered collections
- Filtering: select deontic positions matching criteria
- Aggregation: sum amounts, count entries
- Grouping: group by currency, value date, counterparty

### R3: Deontic Position Creation

L4 must support creating new deontic positions programmatically:

```
createDeontic : Party -> Party -> Action -> Deadline -> DEONTIC
```

This is the "reification" direction: turning rule specifications into runtime values.

### R4: Deontic Position Cancellation/Discharge

L4 must support marking deontic positions as discharged:

```
discharge : DEONTIC -> DischargeReason -> DischargedDeontic
cancel : DEONTIC -> CancellationReason -> CancelledDeontic
```

### R5: Deontic Position Introspection

L4 must support inspecting deontic position properties:

```
obligor : DEONTIC -> Party
obligee : DEONTIC -> Party
amount : DEONTIC -> Number  (where applicable)
deadline : DEONTIC -> Date
status : DEONTIC -> DeonticStatus
```

### R6: Deontic Position Composition

L4 must support combining deontic positions:

```
net : DEONTIC -> DEONTIC -> NettingResult
combine : LIST OF DEONTIC -> DEONTIC
split : DEONTIC -> Number -> (DEONTIC, DEONTIC)
```

### R7: Automatic Effects

L4 must support rules that automatically create, cancel, or modify obligations when conditions are met - without requiring explicit party action:

> "automatically and without further action"

This is distinct from `PARTY p MUST ...` which describes what a party should do. Automatic effects describe what happens to the legal state itself.

### R8: Type Safety

All obligation operations must be type-checked:

- Cannot net obligations in different currencies
- Cannot assign obligations to incompatible parties
- Cannot discharge already-discharged obligations

### R9: Procure Obligations (Higher-Order)

L4 must support "procure" clauses where a party undertakes to cause a third party (typically one they control but who is not a signatory to the contract) to perform an action:

```
procure : Party -> Party -> Obligation -> Obligation
```

This is a higher-order function that takes an obligation and returns a new, derived obligation:

- **Input:** An obligation that a third party should perform
- **Output:** An obligation on the controlling party to bring about that performance

Key characteristics:

- The third party (procured party) is typically not a signatory to the contract
- The controlling party has a relationship (ownership, control, influence) with the third party
- The controlling party's obligation is to _effect_ the third party's performance
- Breach may occur if the third party fails to perform, depending on whether the procure clause is:
  - **Best efforts:** Controlling party must try but isn't strictly liable for third party's failure
  - **Strict:** Controlling party is liable regardless of efforts if third party fails

Common legal formulations:

- "Party A shall procure that [Third Party] does X"
- "Party A shall cause [Subsidiary] to perform Y"
- "Party A undertakes to ensure that [Affiliate] complies with Z"

### R10: Rule Graph Introspection (Stretch Goal)

**This requirement is optional for initial implementation but represents the full vision of homoiconicity.**

Beyond treating obligations as runtime values, full homoiconicity would allow introspection and manipulation of the _regulative rule graph itself_ - the HENCE/LEST state machine structure:

```
rules : RuleGraph
queryRules : (Rule -> Boolean) -> RuleGraph -> LIST OF Rule
transitions : Rule -> LIST OF (Condition, Rule)  -- HENCE/LEST edges
```

This would enable:

- **Rule queries:** "Find all rules that could create obligations of type X"
- **Graph traversal:** "What states are reachable from this rule via HENCE transitions?"
- **Meta-rules:** Rules that inspect or modify other rules (like the Commissioner deadline extension example)
- **Impact analysis:** "Which rules would be affected if this obligation type changes?"

Example use cases:

```l4
-- Find all rules that could lead to breach
GIVEN ruleGraph IS A RuleGraph
GIVETH A LIST OF Rule
`rules leading to breach` MEANS
    FILTER ruleGraph's rules BY
        EXISTS transition IN rule's transitions
        WHERE transition's target EQUALS BREACH

-- The Commissioner example from regulative.md, fully homoiconic
§ `Commissioner Deadline Extension`
PARTY Commissioner
  MAY POWER
    WHEN `application for extension` applicant
    FOR EACH rule IN `rules with tight deadlines` applicant ruleGraph
      MODIFY rule's deadline BY extensionDays
```

This is a deeper level of reflection than the obligation registry (R2) - it operates on the _structure of the contract_ rather than just the _active obligations_ created by executing that structure.

## Proposed Syntax

### Deontic Type Declaration

```l4
-- Deontic positions become a built-in type (internal Haskell type: Deonton)
DECLARE DEONTIC OF ActionType HAS
    obligor  IS A Party
    obligee  IS A Party
    action   IS AN ActionType
    deadline IS A DATE
    status   IS AN ObligationStatus

DECLARE ObligationStatus IS ONE OF
    Pending
    Performed  HAS performedAt IS A DATE
    Breached   HAS breachedAt IS A DATE
    Discharged HAS reason IS A DischargeReason
    Cancelled  HAS reason IS A CancellationReason
```

### Deontic Literals / Constructors

```l4
-- Creating a deontic value (reification)
DEONTIC
    FROM partyA
    TO partyB
    DO DeliverCurrency USD 1000000
    BY DATE_FROM_DMY 20 1 2025
```

Or equivalently using record syntax:

```l4
DEONTIC WITH
    obligor IS partyA
    obligee IS partyB
    action IS DeliverCurrency USD 1000000
    deadline IS DATE_FROM_DMY 20 1 2025
    status IS Pending
```

### Automatic Effect Rules

A new rule form for automatic effects that happen "without further action":

```l4
-- When conditions are met, effects happen automatically
§ `Novation Netting`
AUTOMATICALLY
  WHEN `new transaction entered` newTxn
   AND `matching obligations exist` newTxn existingObligations
  THEN
   CANCEL ALL `matching obligations` newTxn existingObligations
   CREATE `novated obligation` newTxn existingObligations
```

Alternative syntax using `UPON` with automatic semantics:

```l4
§ `Novation Netting Effect`
UPON `FX Transaction entered` txn
  WHERE `novation netting applies` txn
EFFECT
  FOR EACH obl IN `matching currency obligations` txn
    CANCEL obl
    WITH REASON NovationNetting
  END
  CREATE OBLIGATION
    FROM `net obligor` txn matchingObls
    TO `net obligee` txn matchingObls
    DO DeliverCurrency (`netted currency` txn) (`netted amount` txn matchingObls)
    BY txn's valueDate
```

### Collection Operations

```l4
-- Filter obligations
GIVEN obligations IS A LIST OF Obligation
      currency IS A Currency
      valueDate IS A DATE
GIVETH A LIST OF Obligation
`matching obligations for netting` MEANS
    FILTER obligations BY
        obl's action's currency EQUALS currency
        AND obl's deadline EQUALS valueDate

-- Aggregate amounts
GIVEN obligations IS A LIST OF Obligation
GIVETH A NUMBER
`total amount payable` MEANS
    SUM (MAP `obligation amount` obligations)

-- Group by currency
GIVEN obligations IS A LIST OF Obligation
GIVETH A LIST OF (Currency, LIST OF Obligation)
`group by currency` MEANS
    GROUP obligations BY (obl's action's currency)
```

### Netting Operations

```l4
-- Net two opposing obligations
GIVEN obl1 IS AN Obligation
      obl2 IS AN Obligation
GIVETH A NettingResult
`net obligations` MEANS
    IF `same currency and date` obl1 obl2
    THEN
      IF `same direction` obl1 obl2
      THEN CombinedObligation (`combine amounts` obl1 obl2)
      ELSE
        CONSIDER `compare amounts` obl1 obl2
        WHEN obl1 GREATER THEN NetResult (residual obl1 obl2) CANCELLED obl2
        WHEN obl2 GREATER THEN NetResult CANCELLED obl1 (residual obl2 obl1)
        WHEN EQUAL THEN BothCancelled obl1 obl2
    ELSE CannotNet "Currency or date mismatch"
```

### Powers Over Obligations

**Definition:** A power is the higher-order permission to create, modify, or extinguish lower-order deontic relations.

This captures the key distinction: while obligations are first-order deontic relations (A must/may/must-not do X), powers are second-order - they are permissions to bring new first-order relations into existence. A party with a power doesn't merely have permission to perform an action; they have permission to create, modify, or extinguish obligations themselves.

Building on `regulative-powers.l4`, powers can now operate on obligations using nested syntax:

```l4
-- Power to waive an obligation entirely
§ `Creditor Waiver Power`
PARTY creditor MAY
  WAIVE (PARTY debtor MUST pay amount)

-- Power to extend a deadline
§ `Creditor Extension Power`
PARTY creditor MAY
  EXTEND (PARTY debtor MUST pay amount) BY 30 DAYS

-- Power to create new obligations (e.g., issuing fines)
§ `Regulatory Fine Power`
PARTY regulator MAY
  CREATE (PARTY violator MUST pay fine BY 30 DAYS)
  PROVIDED `violation detected` violator

-- Power to transfer an obligation to another party
§ `Debt Assignment Power`
PARTY creditor MAY
  ASSIGN (PARTY debtor MUST pay amount) TO newCreditor
  PROVIDED newCreditor `consents to assignment`

-- Meta-power: power to grant powers to others
§ `Delegation of Waiver Authority`
PARTY creditor MAY
  GRANT (PARTY agent MAY
           WAIVE (PARTY debtor MUST pay amount))
  PROVIDED agent `is authorized representative`
```

### Procure Clauses

The `PROCURE` keyword creates a higher-order obligation where one party must cause a third party (not a signatory) to perform. Like `GRANT` and `REVOKE`, it takes a deontic position as its argument:

```l4
-- Basic procure: partyA must ensure thirdParty delivers
§ `Subsidiary Delivery Obligation`
PARTY partyA MUST
  PROCURE (PARTY thirdParty MUST deliver goods BY deadline)

-- The structure mirrors other higher-order operators:
-- GRANT  takes (PARTY x MAY ...)  and creates a power
-- REVOKE takes (PARTY x MAY ...)  and removes a permission
-- PROCURE takes (PARTY x MUST ...) and creates an obligation to ensure it
```

The `PROCURE` construct can be parameterized by effort level:

```l4
-- Strict liability: partyA liable if subsidiary fails, regardless of effort
§ `Strict Compliance Procurement`
PARTY partyA MUST
  PROCURE STRICT (PARTY subsidiary MUST `comply with regulations` BY auditDate)

-- Best efforts: partyA must try, but not strictly liable for failure
§ `Best Efforts Procurement`
PARTY partyA MUST
  PROCURE BEST EFFORTS (PARTY affiliate MUST `provide statements` BY deadline)

-- Reasonable efforts: intermediate standard
§ `Reasonable Efforts Procurement`
PARTY partyA MUST
  PROCURE REASONABLE EFFORTS (PARTY subcontractor MUST deliver BY deadline)
```

### Procure with HENCE/LEST

Procure obligations use the standard `HENCE`/`LEST` structure. The procuring party's fate is tied to the underlying obligation's outcome:

```l4
-- The underlying obligation has its own HENCE/LEST
§ `Third Party Delivery`
PARTY thirdParty MUST deliver goods BY deadline
  HENCE `delivery complete`
  LEST `delivery failed`

-- The procure obligation references the underlying obligation's outcomes
§ `Procure Delivery (Strict)`
PARTY partyA MUST
  PROCURE STRICT (PARTY thirdParty MUST deliver goods BY deadline)
  HENCE `procure satisfied`           -- when thirdParty succeeds
  LEST `procure breached`             -- when thirdParty fails (strict = automatic)

-- Best efforts: procuring party only breaches if they didn't try
§ `Procure Delivery (Best Efforts)`
PARTY partyA MUST
  PROCURE BEST EFFORTS (PARTY thirdParty MUST deliver goods BY deadline)
  HENCE `procure satisfied`
  LEST                                -- when thirdParty fails...
    IF `used best efforts` partyA
    THEN `procure discharged`         -- partyA off the hook
    ELSE `procure breached`           -- partyA didn't try hard enough
```

The semantics of `PROCURE` link the outcomes: when the underlying obligation reaches its `HENCE` state, the procure obligation also reaches `HENCE`. When it reaches `LEST`, the procure obligation evaluates based on effort level.

### Contract Continuation Semantics: YIELDS

Beyond `FULFILLED` and `BREACH`, contracts sometimes terminate by producing a _proposed successor contract_ - a specification that parties may choose to execute separately. This is analogous to Unix `fork && exec`: the current contract terminates, yielding a new contract specification, but that new contract is not automatically executed.

**Syntax note on keyword choice:**

We use `YIELDS` as the keyword, but several legal terms were considered:

| Keyword         | Legal usage                               | Notes                                  |
| --------------- | ----------------------------------------- | -------------------------------------- |
| `YIELDS`        | General                                   | Clear, concise; chosen for readability |
| `GIVES RISE TO` | "Exercise gives rise to an obligation..." | Very common legal phrase; verbose      |
| `ISSUES`        | "The warrant issues shares"               | Securities/financial pedigree; concise |
| `MATURES INTO`  | "The note matures into..."                | Implies automatic at a date            |
| `CONVERTS TO`   | "The note converts into equity"           | Good for transformation                |
| `RIPENS INTO`   | "The interest ripens into a vested right" | Contingent → vested                    |

A future version might support `GIVES RISE TO` as a synonym for `YIELDS` to accommodate legal drafting conventions.

On the other hand, Dijkstra cautioned against analogizing new concepts into old terminology - it creates false comfort and muddies understanding. A novel term like `YIELDS` forces learners to engage with the actual semantics rather than mapping it onto familiar but potentially misleading legal concepts.

**Key distinction from existing constructs:**

| Construct         | Semantics                                                                   |
| ----------------- | --------------------------------------------------------------------------- |
| `HENCE state`     | Automatic transition within same contract                                   |
| `LEST state`      | Automatic transition on breach within same contract                         |
| Subcontract       | Nested execution within parent contract's lifetime                          |
| `YIELDS contract` | **Termination** producing a contract _specification_ for separate execution |

**The `YIELDS` keyword:**

```l4
-- Option contract: terminates by yielding a purchase agreement
§ `Call Option`
PARTY optionHolder MAY
  EXERCISE option BY expiryDate
  YIELDS `Purchase Agreement` WITH
    buyer IS optionHolder
    seller IS optionWriter
    price IS strikePrice
    asset IS underlyingAsset
  LEST EXPIRED  -- option lapses, no successor contract

-- The yielded contract is NOT automatically executed
-- Parties must separately agree to execute it
-- This is the "fork" without automatic "exec"
```

**Comparison to process semantics:**

```
Unix process:           Contract:
─────────────           ─────────
exit(0)                 FULFILLED
exit(1)                 BREACH
fork() && exec(prog)    YIELDS (Contract specification)
                        ↓
                        Returns to parties as proposal
                        NOT automatically binding
```

**Why not just use HENCE?**

`HENCE` creates a continuation _within_ the same contract - it's still the same legal instrument. `YIELDS` terminates the current contract and produces a _separate_ instrument that requires independent acceptance.

```l4
-- HENCE: same contract continues
PARTY buyer MUST pay deposit BY day1
  HENCE PARTY seller MUST deliver BY day30  -- still same contract

-- YIELDS: current contract ends, new contract proposed
§ `Framework Agreement`
PARTY parties MUST negotiate terms BY negotiationDeadline
  YIELDS `Definitive Agreement` WITH negotiatedTerms
  LEST `negotiations failed`
       YIELDS `Termination Agreement` WITH windDownTerms
```

**The yielded contract as a value:**

Since contracts/obligations are first-class values in this proposal, `YIELDS` returns a contract _specification_ - a value of type `Contract`:

```l4
-- YIELDS returns a Contract value
GIVEN option IS AN OptionContract
GIVETH A MAYBE Contract
`exercise outcome` MEANS
    IF option's holder EXERCISES option
    THEN JUST (`Purchase Agreement` WITH option's terms)
    ELSE NOTHING

-- The Contract value can be inspected, modified, or presented to parties
GIVEN proposedContract IS A Contract
GIVETH A Contract
`with arbitration clause` MEANS
    proposedContract WITH
      disputeResolution IS Arbitration ICC Paris
```

**Multiple possible yields:**

A contract might yield different successor contracts depending on outcome:

```l4
§ `Acquisition Agreement`
PARTY buyer MUST
  CLOSE transaction BY outsideDate
  PROVIDED `conditions satisfied`
  YIELDS `Post-Closing Agreements`:
    `Escrow Agreement` WITH escrowTerms
    `Transition Services Agreement` WITH tsaTerms
    `Non-Compete Agreement` WITH nonCompeteTerms
  LEST
    IF `material adverse change`
    THEN TERMINATED
         YIELDS `Termination Fee Agreement` WITH
           PARTY seller MUST pay terminationFee TO buyer BY 10 DAYS
    ELSE IF `conditions not satisfied`
    THEN EXTENDED BY 30 DAYS  -- still same contract, not YIELDS
    ELSE BREACH
```

**Who may execute the yielded contract?**

A critical question: when a contract yields a successor, who has the _power_ to bring that successor into existence? This connects to Hohfeld's power/liability correlative:

| Scenario               | Who may execute                 | Others' position                          |
| ---------------------- | ------------------------------- | ----------------------------------------- |
| Option contract        | Holder alone (unilateral power) | Writer has liability (bound if exercised) |
| Framework agreement    | All parties (mutual consent)    | Each has veto power                       |
| Right of first refusal | Holder, but only if triggered   | Writer bound only on trigger              |

**Syntax for execution power:**

```l4
-- Option: holder has unilateral power to execute yielded contract
§ `Call Option`
PARTY holder MAY EXERCISE option BY expiryDate
  YIELDS `Purchase Agreement` WITH terms
    EXECUTABLE BY holder                    -- unilateral power
    BINDING ON writer                       -- writer has liability, cannot refuse
  LEST EXPIRED

-- Framework: mutual consent required
§ `Framework Agreement`
PARTY parties MUST negotiate BY deadline
  YIELDS `Definitive Agreement` WITH negotiatedTerms
    EXECUTABLE BY MUTUAL CONSENT OF parties -- all must agree
  LEST `negotiations failed`

-- Right of first refusal: conditional power
§ `Right of First Refusal`
PARTY writer MUST
  OFFER asset TO holder BEFORE selling TO thirdParty
  YIELDS `Purchase Agreement` WITH matchingTerms
    EXECUTABLE BY holder                    -- holder may match
    BINDING ON writer                       -- writer must sell to holder if matched
    PROVIDED holder MATCHES thirdPartyOffer
  LEST holder DECLINES
       HENCE PARTY writer MAY sell TO thirdParty
```

**The yielded contract plus execution power:**

`YIELDS` produces two things:

1. A **contract specification** (the value)
2. An **execution power** (who can bring it into existence)

```l4
-- Decomposed view: YIELDS creates contract, MAY grants power
§ `Option Decomposed`
PARTY holder MAY EXERCISE option BY expiryDate
  YIELDS purchaseAgreement = `Purchase Agreement` WITH terms
  HENCE PARTY holder MAY
          EXECUTE purchaseAgreement           -- holder's power
          -- writer has NO corresponding MAY  -- writer's liability
  LEST EXPIRED
```

**PROCURE interacting with YIELDS:**

Sometimes the yielded contract involves a third party brought in via PROCURE:

```l4
-- Option to purchase, but delivery procured from subsidiary
§ `Option with Procured Delivery`
PARTY holder MAY EXERCISE option BY expiryDate
  YIELDS `Purchase Agreement` WITH
    PARTY holder MUST pay price TO writer BY 30 DAYS
    PARTY writer MUST
      PROCURE STRICT (PARTY subsidiary MUST deliver asset TO holder BY 60 DAYS)
    EXECUTABLE BY holder
    BINDING ON writer
  LEST EXPIRED
```

**Relationship to novation:**

Novation (IFEMA 3.3) is _automatic_ replacement - no separate acceptance needed. `YIELDS` is _proposed_ replacement - requires party action:

```l4
-- Novation: automatic, no consent needed at replacement time
AUTOMATICALLY WHEN `matching obligations` THEN REPLACE old WITH new

-- YIELDS: proposes replacement, parties must separately agree
PARTY contract TERMINATES
  YIELDS proposedSuccessor  -- parties must then EXECUTE proposedSuccessor
```

## Semantics

### Evaluation Model Changes

#### From Static to Dynamic Deontic Tracking

Current L4 treats regulative rules as a static labeled transition system compiled from source. The proposed extension requires a **runtime deontic registry**:

```haskell
type DeontRegistry = Map DeontId Deonton

data EvalState = MkEvalState
  { esDeonts :: DeontRegistry
  , esTrace :: [Event]
  , esTime :: Time
  }
```

Deontic positions are created, modified, and removed during evaluation:

```haskell
createDeont :: Deonton -> EvalM DeontId
createDeont deont = do
  id <- freshId
  modify (\s -> s { esDeonts = Map.insert id deont (esDeonts s) })
  return id

cancelDeont :: DeontId -> CancellationReason -> EvalM ()
cancelDeont id reason = do
  modify (\s -> s { esDeonts =
    Map.adjust (\d -> d { status = Cancelled reason }) id (esDeonts s) })
```

#### Automatic Effect Evaluation

When an event occurs (e.g., new transaction entered), the evaluator:

1. Checks all `AUTOMATICALLY WHEN` rules for matching conditions
2. For rules where conditions match, executes the `THEN` effects
3. Effects may cancel/create/modify obligations
4. All effects within one rule happen atomically (no intermediate states)

```haskell
evalAutomaticEffects :: Event -> EvalM ()
evalAutomaticEffects event = do
  rules <- getAutomaticRules
  forM_ rules $ \rule -> do
    matches <- evalCondition event (ruleCondition rule)
    when matches $ do
      withAtomicEffects $ evalEffects (ruleEffects rule)
```

#### Obligation Introspection in Conditions

Conditions can now reference the obligation registry:

```l4
-- Reference existing obligations in conditions
WHEN EXISTS obl IN currentObligations
     WHERE obl's obligor EQUALS partyA
       AND obl's currency EQUALS USD
```

### Type System Implications

#### DEONTIC as a Parameterized Type

Deontic positions should be parameterized by their action type for type safety:

```l4
DEONTIC : ActionType -> Type

-- Type-safe: currency obligations can only net with currency obligations
netCurrencyDeontics :
    DEONTIC DeliverCurrency ->
    DEONTIC DeliverCurrency ->
    NettingResult DeliverCurrency
```

#### Effect Typing

Effects that create/cancel deontic positions need effect typing to track what changes:

```haskell
data Effect
  = CreateDeont Deonton
  | CancelDeont DeontId CancellationReason
  | ModifyDeont DeontId (Deonton -> Deonton)

-- A rule's effect type signature
type AutoRule = Condition -> [Effect]
```

#### Power Typing

Powers over obligations form a hierarchy:

```l4
DECLARE Power OF TargetType IS ONE OF
    -- Power to create obligations of type T
    CreatePower  HAS targetType IS A Type
    -- Power to cancel obligations
    CancelPower  HAS scope IS A ObligationFilter
    -- Power to modify obligation properties
    ModifyPower  HAS property IS A PropertyName, newValue IS AN Expr
    -- Power to delegate powers to others
    DelegatePower HAS delegatedPower IS A Power
```

### Relationship to Existing Constructs

#### HENCE/LEST Still Work

Traditional `HENCE`/`LEST` clauses still function for describing obligations parties must fulfill. The new constructs handle **automatic effects** that happen regardless of party action.

```l4
-- Traditional: party must perform, or face consequences
PARTY buyer
MUST pay amount
WITHIN 30 DAYS
HENCE `payment received`
LEST `payment default`

-- New: automatic effect, no party action required
AUTOMATICALLY
WHEN `30 days elapsed` AND NOT `payment received`
THEN
  CREATE OBLIGATION
    FROM buyer
    TO seller
    DO `pay penalty` (amount * 0.1)
    BY 60 DAYS
```

#### State Variables Still Work

The `PUT`/`GET` state variable mechanism in `regulative.md` can coexist:

```l4
-- Update counter when obligation created
AUTOMATICALLY
WHEN `new obligation created` obl
THEN
  PUT transactionCount = transactionCount + 1
```

#### Powers Compose with Obligations

From `regulative-powers.l4`, powers can now target obligations:

```l4
-- President can empower officers to create obligations
PARTY president
MAY POWER
  GRANT officer
  POWER TO CREATE OBLIGATION
    FROM citizen
    DO `pay fine` amount
```

## Implementation Considerations

### Phase 1: Deontic Values (Foundation)

1. **Add `DEONTIC` type to type system** (internal Haskell: `Deonton`)

   - Parameterized by action type
   - Include all required fields (obligor, obligee, action, deadline, status)

2. **Add deontic literals/constructors**

   - Parser support for `DEONTIC FROM ... TO ... DO ... BY ...`
   - Type checking for deontic construction

3. **Add deontic introspection**
   - Field accessors (`obl's obligor`, etc.)
   - Status checks (`obl IS Pending`, etc.)

**Estimated effort:** ~500 LOC parser, ~300 LOC type checker, ~200 LOC evaluator

### Phase 2: Deontic Collections

1. **Collection operations**

   - `FILTER deontics BY condition`
   - `MAP function deontics`
   - `SUM/COUNT` aggregations
   - `GROUP BY` operations

2. **Deontic registry in runtime state**
   - Track active deontic positions
   - Query deontics by criteria

**Estimated effort:** ~400 LOC

### Phase 3: Automatic Effects

1. **New `AUTOMATICALLY WHEN ... THEN ...` syntax**

   - Parser support
   - Distinct from party obligations

2. **Effect evaluation**

   - `CREATE`, `CANCEL`, `MODIFY` primitives
   - Atomic effect execution
   - Condition checking against current state

3. **Integration with event system**
   - Trigger automatic rules on events
   - Handle obligation lifecycle events

**Estimated effort:** ~800 LOC

### Phase 4: Type Safety for Effects

1. **Effect typing**

   - Track what effects a rule can have
   - Validate effect compatibility

2. **Power typing over obligations**
   - Which parties can create/cancel/modify which obligations
   - Power delegation chains

**Estimated effort:** ~600 LOC

### Phase 5: Procure Deontics

1. **Procure syntax and parsing**

   - `MUST PROCURE (PARTY x MUST ...)` nested syntax
   - Effort level modifiers `STRICT`, `BEST EFFORTS`, `REASONABLE EFFORTS`
   - Nested procure for multi-tier scenarios

2. **Procure deontic type**

   - `ProcureDeonton` as specialized `Deonton` subtype
   - Link to underlying deontic position
   - Effort level tracking

3. **Procure-specific HENCE/LEST semantics**

   - Discharge on underlying performance
   - Breach propagation based on effort level
   - Best efforts evaluation hooks

4. **Type checking for procure**
   - Validate party relationships (controller/controlled)
   - Ensure underlying obligation is well-typed
   - Check effort level compatibility

**Estimated effort:** ~500 LOC

### Phase 6: Contract Continuation (YIELDS)

1. **YIELDS syntax and parsing**

   - `YIELDS Contract WITH ...` clause
   - Multiple yields (list of contracts)
   - Conditional yields in LEST branches

2. **Contract as first-class value**

   - `Contract` type representing executable specifications
   - Contract literals/constructors
   - Contract introspection (parties, obligations, terms)

3. **Termination semantics**

   - `YIELDS` as terminal (like FULFILLED/BREACH)
   - Parent contract cleanup on yield
   - Yielded contract as return value

4. **Contract execution model**
   - `EXECUTE` operation on Contract values
   - Consent tracking for multi-party contracts
   - Relationship between yielded and executed contracts

**Estimated effort:** ~700 LOC

## Examples

### Example 1: IFEMA Novation Netting (Section 3.3)

```l4
§ `IFEMA 3.3 Novation Netting`

-- Type representing the netting configuration
DECLARE NovationContext HAS
    obligationRegistry IS A LIST OF CurrencyObligation
    novationOffices    IS A LIST OF OfficePair

-- Check if two obligations can be novated
GIVEN obl1 IS A CurrencyObligation
      obl2 IS A CurrencyObligation
GIVETH A BOOLEAN
`can novate` MEANS
    `same currency` obl1 obl2
    AND `same value date` obl1 obl2
    AND `matching office pair` obl1 obl2

-- Calculate net obligation from a list of obligations
GIVEN obligations IS A LIST OF CurrencyObligation
      partyA IS A Party
      partyB IS A Party
GIVETH A MAYBE CurrencyObligation
`net result` MEANS
    LET partyAOwes = `sum amounts where obligor` obligations partyA
        partyBOwes = `sum amounts where obligor` obligations partyB
        netAmount = ABS (partyAOwes - partyBOwes)
    IN
    IF netAmount EQUALS 0
    THEN NOTHING  -- "no new Currency Obligation shall arise"
    ELSE JUST (OBLIGATION
        FROM (IF partyAOwes > partyBOwes THEN partyA ELSE partyB)
        TO (IF partyAOwes > partyBOwes THEN partyB ELSE partyA)
        DO DeliverCurrency (obligations's HEAD's currency) netAmount
        BY (obligations's HEAD's valueDate))

-- The automatic novation effect
§ `Automatic Novation`
AUTOMATICALLY
  WHEN `FX Transaction entered` txn
   AND `novation netting applies` txn
   AND matchingObls = `find matching obligations` txn currentObligations
   AND NOT (matchingObls IS EMPTY)
  THEN
    -- Cancel all matching obligations
    FOR EACH obl IN matchingObls
      CANCEL obl WITH REASON (NovatedBy txn)
    END
    -- Create the new netted obligation (if any)
    CONSIDER `net result` (txn's obligation FOLLOWED BY matchingObls)
              txn's buyer txn's seller
    WHEN JUST newObl THEN CREATE newObl
    WHEN NOTHING THEN SKIP  -- amounts equal, no new obligation
```

### Example 2: Debt Set-Off

```l4
§ `Mutual Debt Set-Off`

-- When both parties owe each other, obligations can be set off
AUTOMATICALLY
  WHEN `set-off election made` partyA
   AND oblAtoB = `obligations from to` partyA partyB currentObligations
   AND oblBtoA = `obligations from to` partyB partyA currentObligations
   AND NOT (oblAtoB IS EMPTY)
   AND NOT (oblBtoA IS EMPTY)
  THEN
    LET setOffAmount = MIN (`total amount` oblAtoB) (`total amount` oblBtoA)
    IN
    -- Reduce or cancel obligations in both directions
    FOR EACH obl IN oblAtoB
      IF obl's amount <= remainingSetOff
      THEN CANCEL obl WITH REASON SetOff
      ELSE MODIFY obl SET amount = obl's amount - remainingSetOff
    END
    FOR EACH obl IN oblBtoA
      -- Symmetric reduction
      ...
    END
```

### Example 3: Loan Acceleration

```l4
§ `Loan Acceleration on Default`

-- All future installments become immediately due on default
AUTOMATICALLY
  WHEN `event of default` borrower
   AND futureObls = `future installments` borrower currentObligations
  THEN
    FOR EACH obl IN futureObls
      MODIFY obl SET deadline = TODAY
    END
    -- Optionally, consolidate into single obligation
    CREATE OBLIGATION
      FROM borrower
      TO lender
      DO PayAmount (`total amount` futureObls)
      BY TODAY PLUS 5 DAYS
    FOR EACH obl IN futureObls
      CANCEL obl WITH REASON AcceleratedInto newObl
    END
```

### Example 4: Power to Extend Deadlines

```l4
§ `Commissioner's Power to Extend Deadlines`

-- The Commissioner can extend deadlines causing undue hardship
GIVEN rule IS A Rule
GIVETH A BOOLEAN
`would cause undue hardship` MEANS
    EXISTS obl IN `obligations from rule` rule
    WHERE (`days until` obl's deadline) < 5

PARTY Commissioner
  MAY POWER
    WHEN `application for extension` applicant rule
     AND `would cause undue hardship` rule
    THEN
      FOR EACH obl IN `obligations from rule` rule
        WHERE obl's obligor EQUALS applicant
        MODIFY obl SET deadline = obl's deadline PLUS extensionDays
```

### Example 5: Parent Company Procure Clause

A common pattern in corporate transactions where a parent company procures actions by its subsidiaries:

```l4
§ `Acquisition Agreement - Subsidiary Compliance`

-- Parties to the agreement
DECLARE AcquisitionParties HAS
    buyer IS A Party           -- signatory
    seller IS A Party          -- signatory (parent company)
    targetCompany IS A Party   -- NOT a signatory (subsidiary being sold)

-- The seller (parent) must procure that the target company provides information
-- STRICT: seller breaches automatically if targetCompany fails
§ `Due Diligence Cooperation`
PARTY seller MUST
  PROCURE STRICT (PARTY targetCompany MUST `provide due diligence information`
                  BY dueDiligenceDeadline)
  HENCE `due diligence complete`
  LEST `seller breach`
       HENCE PARTY seller MUST indemnify buyer BY 30 DAYS

-- The seller must procure that target employees don't compete
-- BEST EFFORTS: seller only breaches if they didn't try
§ `Non-Compete Procurement`
PARTY seller MUST
  PROCURE BEST EFFORTS (PARTY `key employees` MUST `execute non-compete agreements`
                        BY closingDate)
  HENCE `non-competes secured`
  LEST
    IF `used best efforts` seller
    THEN `seller discharged`
    ELSE `seller breach`
         HENCE `escalate to seller board`

-- Between signing and closing, seller must procure ordinary course operations
§ `Ordinary Course Covenant`
PARTY seller MUST
  PROCURE STRICT (PARTY targetCompany MUST `operate in ordinary course`
                  THROUGHOUT signingDate TO closingDate)
  HENCE `covenant satisfied`
  LEST `material adverse change`
       HENCE PARTY buyer MAY
               TERMINATE (this Agreement)
               OR CLOSE (this Agreement) WITH `price adjustment`
```

### Example 6: Supply Chain Procure Obligations

Multi-tier procurement where obligations flow down a supply chain:

```l4
§ `Supply Agreement - Compliance Flow-Down`

-- Manufacturer must procure that its suppliers comply with standards
§ `Supplier Compliance Procurement`
FOR EACH supplier IN suppliers
  PARTY manufacturer MUST
    PROCURE STRICT (PARTY supplier MUST `comply with` standard BY annualAuditDate)
    HENCE `supplier compliant`
    LEST `supply chain breach`
         HENCE PARTY manufacturer MUST `remediate or replace` supplier

-- Recursive procure: manufacturer procures that suppliers procure from their suppliers
-- This shows nested PROCURE - a higher-order pattern
§ `Tier 2 Compliance Flow-Down`
FOR EACH supplier IN suppliers
  PARTY manufacturer MUST
    PROCURE REASONABLE EFFORTS
      (PARTY supplier MUST
         PROCURE STRICT (PARTY subSupplier MUST `comply with` standard
                         BY annualAuditDate)
         FOR EACH subSupplier IN supplier's subSuppliers)
    HENCE `tier 2 compliant`
    LEST
      IF `used reasonable efforts` manufacturer
      THEN `manufacturer discharged`
      ELSE `manufacturer breach`

-- The nested structure shows the flow-down:
-- manufacturer MUST PROCURE (supplier MUST PROCURE (subSupplier MUST comply))
```

### Example 7: Constitutional Chain of Powers (Police Detention)

A complete example showing the hierarchy of powers from constitution to individual action, using nested composition:

```l4
§ `Constitutional Framework for Detention Powers`

-- Base level: fundamental liberties (first-order permissions)
§ `Freedom of Movement`
PARTY person MAY leave
PARTY person MAY `travel freely`
PARTY person MAY `refuse to answer questions`

-- First-order power: police can revoke liberty
§ `Police Detention Power`
PARTY officer MAY
  REVOKE (PARTY person MAY leave)
  PROVIDED officer `is on duty`
      AND officer `has reasonable suspicion of` person
  SUBJECT TO `maximum duration` 24 HOURS
  HENCE PARTY person MUST `remain at location`
        UNTIL `released by` officer
        LEST `resisting arrest`

-- Second-order power: legislature grants detention powers to police
§ `Police Powers Act`
PARTY parliament MAY
  GRANT (PARTY officer MAY
           REVOKE (PARTY person MAY leave)
           PROVIDED officer `is on duty`
               AND officer `has reasonable suspicion of` person
           SUBJECT TO `procedural safeguards`)
  PROVIDED `bill passed by majority`
  REVIEWABLE BY judiciary

-- Third-order power: constitution empowers legislature
§ `Constitutional Legislative Authority`
PARTY constitution MAY
  GRANT (PARTY parliament MAY
           GRANT (PARTY officer MAY
                    REVOKE (PARTY person MAY leave)))
  SUBJECT TO `Bill of Rights`
      AND `separation of powers`
      AND `judicial review`

-- The full chain can be written as a single nested expression:
§ `Complete Authorization Chain`
PARTY constitution MAY
  GRANT (PARTY parliament MAY
           GRANT (PARTY officer MAY
                    REVOKE (PARTY person MAY leave)
                    PROVIDED officer `has reasonable suspicion of` person)
           PROVIDED `bill passed`)
  SUBJECT TO `fundamental rights`

-- Validity check: is this detention lawful?
GIVEN officer IS A Party
      person IS A Party
GIVETH A BOOLEAN
`detention is lawful` MEANS
    (PARTY officer MAY REVOKE (PARTY person MAY leave)) `is validly granted`
    AND officer `has reasonable suspicion of` person
    AND `procedural requirements met` officer person

-- Tracing the authorization chain
GIVEN deonticPosition IS A DeonticPosition
GIVETH A LIST OF DeonticPosition
`authorization chain` MEANS
    IF deonticPosition's `granted by` IS NOTHING
    THEN [deonticPosition]
    ELSE deonticPosition FOLLOWED BY
         (`authorization chain` (deonticPosition's `granted by`))
```

### Example 8: Delegated Rulemaking Power

Administrative agencies exercising delegated legislative power:

```l4
§ `Environmental Regulation Delegation`

-- Congress delegates rulemaking power to EPA (second-order)
-- EPA gets power to create duties on emission sources
§ `Clean Air Act Delegation`
PARTY congress MAY
  GRANT (PARTY epa MAY
           CREATE (PARTY facility MUST `limit emissions` BY complianceDate))
  SUBJECT TO `intelligible principle`:
    `protect public health with adequate margin of safety`

-- EPA exercises delegated power to create specific regulations
§ `EPA Emission Standards`
PARTY epa MAY
  CREATE (PARTY facility MUST `achieve emission limit` standard's limit
          BY standard's complianceDate
          LEST `civil penalties`)
  PROVIDED `notice and comment completed`
  CITING cleanAirAct section 111

-- Automatic obligation creation when EPA promulgates standard
§ `Power Plant Compliance`
AUTOMATICALLY
  WHEN epa EXERCISES
         (PARTY epa MAY CREATE (PARTY facility MUST `limit emissions`))
       WITH standard
  THEN
    CREATE (PARTY facility MUST `achieve` standard's limit
            BY standard's complianceDate)
    FOR EACH facility IN `facilities subject to` standard

-- Challenge: regulation exceeds delegated authority
GIVEN regulation IS A Regulation
GIVETH A BOOLEAN
`exceeds delegated authority` MEANS
    NOT (`within intelligible principle` regulation)
    OR NOT (`procedurally valid` regulation)
    OR `arbitrary and capricious` regulation

-- If regulation invalid, obligations created by it are void
§ `Void for Excess of Authority`
AUTOMATICALLY
  WHEN court DECIDES (`exceeds delegated authority` regulation)
  THEN
    FOR EACH obl IN `obligations created by` regulation
      VOID obl WITH REASON `ultra vires`
```

## Comparison with Other Approaches

### Process Algebras (CSP, Pi-Calculus)

Process algebras model concurrent communicating processes but lack:

- Native notion of obligations/duties
- Temporal deadlines
- Automatic transformations on the process structure itself

L4's approach differs by making obligations (not processes) the primitive, with deadlines and automatic effects built-in.

### Smart Contract Languages (Solidity, Plutus)

Smart contracts can manipulate state but:

- Focus on asset transfers, not obligations
- Lack deontic modalities (must/may/must-not)
- No native support for obligation netting/novation

L4's approach maintains the deontic dimension while adding the ability to manipulate obligations as data.

### Contract Languages (CSL, CL)

Hvitved's CSL and similar contract languages:

- Model contracts as traces
- Support composition (AND, OR, THEN)
- But contracts are syntax, not runtime values

L4's extension adds the crucial ability to inspect and transform contracts/obligations during execution.

## Open Questions

### Q1: Obligation Identity

When obligations are created, modified, and netted, what is their identity?

- **Option A:** Obligations have unique IDs; netting creates new ID
- **Option B:** Obligations can be "merged" preserving original IDs
- **Option C:** Identity is determined by content (structural equality)

**Recommendation:** Option A - explicit IDs with clear provenance tracking. The IFEMA language ("individually canceled and simultaneously replaced") suggests distinct before/after identity.

### Q2: Atomicity Boundaries

How are effects grouped atomically?

- **Option A:** All effects in one `THEN` block are atomic
- **Option B:** Each individual effect is atomic
- **Option C:** User-specified transaction boundaries

**Recommendation:** Option A - all effects in one automatic rule fire atomically. This matches the IFEMA "simultaneously" requirement.

### Q3: Effect Ordering

When multiple automatic rules could fire, what order?

- **Option A:** Specified priority order
- **Option B:** Confluent (order doesn't matter)
- **Option C:** Non-deterministic (any order)

**Recommendation:** Require confluence where possible; specify priority for non-confluent cases. Compiler should warn about potential non-confluence.

### Q4: Visibility of Pending Changes

During rule execution, do later effects see earlier effects?

- **Option A:** Yes - sequential visibility within rule
- **Option B:** No - all effects see pre-rule state

**Recommendation:** Option A - effects within a rule see previous effects. This allows dependent effects (e.g., cancel then create replacement).

### Q5: Procure Obligation Semantics

How should procure obligations relate to their underlying obligations?

- **Option A:** Procure creates a separate, independent obligation that references the underlying one
- **Option B:** Procure wraps the underlying obligation (composition pattern)
- **Option C:** Procure is syntactic sugar that desugars to a standard obligation with special action type

**Sub-question:** Should the underlying obligation exist in the registry?

- **Option A1:** Yes - third party's obligation is tracked even though they're not a signatory
- **Option A2:** No - only the procure obligation exists; underlying is notional/hypothetical

**Sub-question:** How to handle "best efforts" evaluation?

- **Option B1:** Fact-based - runtime must be told whether efforts were sufficient
- **Option B2:** Process-based - L4 tracks effort actions and evaluates sufficiency
- **Option B3:** Hybrid - default to fact-based but allow process specification

**Recommendation:** Option A (separate obligations) with Option A1 (track underlying). This provides maximum flexibility and auditability. For best efforts, Option B3 (hybrid) allows both simple and sophisticated usage.

### Q6: YIELDS Semantics

How should `YIELDS` interact with contract execution and the runtime?

**Sub-question:** What is the type of a yielded contract?

- **Option A:** `Contract` - a fully specified, executable contract value
- **Option B:** `ContractTemplate` - a parameterized specification requiring instantiation
- **Option C:** `ContractProposal` - includes metadata about who must accept, deadlines, etc.

**Sub-question:** When does the parent contract terminate?

- **Option A1:** Immediately when `YIELDS` is reached
- **Option A2:** When the yielded contract is either accepted or rejected
- **Option A3:** Configurable per-contract (some yield-and-terminate, others yield-and-wait)

**Sub-question:** What if yielded contracts are never executed?

- **Option B1:** They simply expire (stateless - no tracking)
- **Option B2:** They remain as "pending proposals" indefinitely
- **Option B3:** Configurable expiry deadline on the yield

**Sub-question:** Can yielded contracts reference state from the parent?

- **Option C1:** Yes - closure semantics, yielded contract captures parent's bindings
- **Option C2:** No - yielded contract must be self-contained
- **Option C3:** Explicit parameter passing only

**Sub-question:** How to model execution power vs. liability?

- **Option D1:** `EXECUTABLE BY` and `BINDING ON` as separate clauses
- **Option D2:** `EXECUTABLE BY x` implies all others are bound (Hohfeldian correlative)
- **Option D3:** Distinguish unilateral (`EXECUTABLE BY holder`) from mutual (`EXECUTABLE BY MUTUAL CONSENT OF parties`)

**Sub-question:** What if the bound party refuses despite liability?

- **Option E1:** Automatic breach of the original (parent) contract
- **Option E2:** Automatic breach of a new "refusal" obligation
- **Option E3:** The yielded contract is deemed executed despite refusal (specific performance semantics)

**Recommendation:** Option C (ContractProposal with metadata) including execution power specification. Option D2 (EXECUTABLE BY implies others bound) keeps it simple. Option E1 (breach of parent) creates proper incentives - the option writer's liability means their refusal is a breach.

## Future Extensions

### Obligation Templates

Parameterized obligation patterns that can be instantiated:

```l4
TEMPLATE MonthlyPayment (debtor, creditor, amount, dayOfMonth) IS
    OBLIGATION
      FROM debtor
      TO creditor
      DO PayAmount amount
      BY dayOfMonth OF EACH MONTH
```

### Obligation Constraints

Invariants that must hold across all obligations:

```l4
CONSTRAINT `no circular obligations`
  FOR ALL obl IN obligations
    obl's obligor NOT IN `transitive obligees` obl
```

### Formal Verification of Effects

Prove properties about automatic effects:

```l4
-- After novation, total exposure is unchanged
ASSERT FOR ALL txn IN transactions
  `total exposure before` txn + txn's amount
  EQUALS `total exposure after` txn
```

## References

### Legal Theory (Higher-Order Relations)

- Hohfeld, W.N. (1913). _Fundamental Legal Conceptions as Applied in Judicial Reasoning_. Yale Law Journal. [The foundational work identifying eight atomic jural relations including the power/liability correlative.]
- Hohfeld, W.N. (1917). _Fundamental Legal Conceptions as Applied in Judicial Reasoning: Second Article_. Yale Law Journal.
- Kelsen, H. (1934/1967). _Pure Theory of Law_ (Reine Rechtslehre). [Hierarchy of norms, empowerment chains, and the Grundnorm.]
- Hart, H.L.A. (1961). _The Concept of Law_. Oxford University Press. [Primary vs. secondary rules; power-conferring rules.]
- Raz, J. (1972). _Practical Reason and Norms_. Princeton University Press. [Basic vs. chained normative powers.]
- Raz, J. (1979). _The Authority of Law: Essays on Law and Morality_. Oxford University Press.
- Raz, J. (2022). _Normative Powers_. In _The Roots of Normativity_. Oxford University Press.

### Contract and Deontic Logic

- IFEMA 1997 Master Agreement, Section 3.3 (Novation Netting)
- Hvitved, T. (2012). _Contract Formalisation and Modular Implementation of Domain-Specific Languages_. PhD Thesis, University of Copenhagen.
- Prisacariu, C. & Schneider, G. (2012). _A Dynamic Deontic Logic for Complex Contracts_. Journal of Logic and Algebraic Programming.

### L4 Internal Documentation

- `regulative.md`: L4 regulative rule design notes
- `regulative-spec.org`: L4 regulative rule specification
- `regulative-powers.l4`: Powers vs. obligations experiments
