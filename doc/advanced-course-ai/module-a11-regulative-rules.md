# Module A11: Regulative Rules & Contract Logic

**What you'll learn:**

- Theoretical foundations of contract formalization (trace-based semantics, blame assignment)
- How L4's regulative rules derive from academic research (CSL)
- Writing obligations with `PARTY`, `MUST`, `WITHIN`, `HENCE`, `LEST`
- Modeling real contracts: the promissory note example
- Testing contract execution with `#TRACE`
- Current limitations and workarounds for prohibitions

**Key takeaway:** L4 can model executable contracts where obligations, deadlines, and consequences are formally specified and automatically monitored.

---

## Prerequisites

Before starting this module, you should understand:

- L4 types and records (`DECLARE`, `HAS`, `IS A`)
- Functions and recursion (`GIVEN`, `MEANS`, `WHERE`)
- Conditional expressions (`IF`/`THEN`/`ELSE`)
- The `daydate` library for temporal arithmetic

---

## 1. Theoretical Foundations

### 1.1 Why Formalize Contracts?

Contracts are legally binding agreements between parties. In business, automatically checking conformance to contracts is crucial for minimizing financial penalties. Studies show that transactions compliant with contracts save an average of 22% compared to non-compliant ones.

Yet most Contract Lifecycle Management (CLM) systems treat contracts as documents rather than executable specifications. L4 takes a different approach: **contracts as code**.

### 1.2 The Academic Foundation: CSL

L4's regulative rules are based on the **Contract Specification Language (CSL)**, developed by Tom Hvitved and colleagues at the IT University of Copenhagen. The key insights from this research:

**Traces and Verdicts**

A contract execution is modeled as a _trace_ — a sequence of timestamped events (actions taken by parties). Given a trace, the contract produces a _verdict_:

- **Conformance** (✓) — All obligations have been met
- **Breach** (τ, B) — A breach occurred at time τ, with parties B to blame

**Blame Assignment**

A critical feature: every breach is attributed to specific parties. This isn't just "the contract failed" but "Party X failed to do Y by time Z."

**Fundamental Breach**

CSL distinguishes _violations_ from _breaches_:

- A **violation** is failing to meet a primary obligation (e.g., missing a payment deadline)
- A **breach** occurs only when recovery is impossible (e.g., missing the penalty payment deadline too)

This allows reparation clauses: "Pay by the 1st, or pay with 5% penalty by the 15th." Missing the 1st is a violation; missing the 15th is a breach.

**Compositional Contracts**

Contracts compose via:

- **Conjunction** (AND) — Both sub-contracts must be fulfilled
- **Disjunction** (OR) — At least one sub-contract must be fulfilled

### 1.3 From CSL to L4

L4 implements CSL's semantics with more readable syntax:

| CSL Construct | L4 Syntax         | Meaning                          |
| ------------- | ----------------- | -------------------------------- |
| `⟨p⟩`         | `PARTY p`         | Party responsible for obligation |
| `k(x̄)`        | action expression | The required action              |
| `where e`     | `PROVIDED e`      | Condition on the action          |
| `due d`       | `WITHIN d`        | Deadline (in days)               |
| `then c`      | `HENCE c`         | Continuation if fulfilled        |
| `else c₂`     | `LEST c₂`         | Consequence if not fulfilled     |
| `fulfilment`  | `FULFILLED`       | Contract successfully completed  |

---

## 2. The Obligation Pattern

### 2.1 Basic Structure

An L4 obligation has this structure:

```l4
PARTY   <who>
MUST    <action>
WITHIN  <deadline>
HENCE   <what happens if fulfilled>
LEST    <what happens if not fulfilled>
```

Each component:

- **PARTY** — The entity responsible. If they fail, they're blamed.
- **MUST** — The action required. Can include constraints with `PROVIDED` or `EXACTLY`.
- **WITHIN** — Deadline in days (a number). Without a deadline, there's no obligation.
- **HENCE** — The continuation clause. What obligations arise after fulfillment?
- **LEST** — The reparation clause. What happens if the deadline is missed?

### 2.2 Action Constraints

Actions can be constrained in two ways:

**EXACTLY** — The action must match a specific value:

```l4
MUST `pay to` EXACTLY `The Lender`
```

**PROVIDED** — The action must satisfy a predicate:

```l4
MUST `pay amount`
     `Amount Transferred` PROVIDED
         `Amount Transferred` AT LEAST `Minimum Payment`
```

### 2.3 Termination States

Every obligation chain must eventually terminate in one of:

- **FULFILLED** — The contract completed successfully
- **Another obligation** — The chain continues
- **Implicit breach** — If LEST is omitted and the deadline passes, the contract breaches

### 2.4 Recursive Obligations

For repeating obligations (like monthly payments), use recursion:

```l4
GIVEN `Remaining Balance` IS A Money
`Payment Schedule` MEANS
    IF `Remaining Balance`'s Value GREATER THAN 0
        THEN PARTY `Borrower`
             MUST  `make payment`
             WITHIN `Next Due Date`
             HENCE `Payment Schedule` (`Remaining Balance` MINUS `Payment Amount`)
             LEST  -- penalty clause
        ELSE FULFILLED
```

---

## 3. Worked Example: The Promissory Note

Let's examine `jl4/examples/legal/promissory-note.l4` — a complete loan agreement.

### 3.1 The Business Terms

```l4
§ `Promissory Note`

§§ `Basic Definitions`

`Note Date` MEANS February 4 2024
`Principal Amount` MEANS USD 25000
`Interest Rate Per Annum` MEANS 15%
`Monthly Installments` MEANS 12
`Default After Days Not Paid Beyond Due` MEANS 30

`Late Payment Penalty` MEANS Penalty WITH
    `Interest Rate`     IS 5%
    `Grace Period Days` IS 10
```

Note:

- Currency helper `USD 25000` creates a Money record
- Native percentage support with `15%`
- Structured penalty with interest rate and grace period

### 3.2 The Parties

```l4
§§ `Parties`

`The Borrower` MEANS
  `Commercial Borrower` OF
    Company WITH
      `Name`           IS "Jane Dough Pte Ltd"
      `Address`        IS "42 Jane Doe Rd #04-20, Singapore 420000"
      `Jurisdiction`   IS "Singapore"
      `Company Number` IS "UEN 200424242N"
      `Account`        IS `Bank Account` WITH ...

`The Lender` MEANS
  `Individual Lender` OF
    `Natural Person` WITH
      `Name`          IS "John Doe"
      ...
```

The type system distinguishes:

- `Commercial Borrower` (company) vs `Individual Borrower` (natural person)
- `Commercial Lender` vs `Individual Lender`

This matters legally — different rules may apply to each.

### 3.3 The Payment Calculation

```l4
§§ `Repayment Terms`

`Monthly Interest Rate` MEANS
    `Interest Rate Per Annum` DIVIDED BY `Months in a year`

`Monthly Installment Amount` MEANS
    Money WITH
        Currency  IS `Principal Amount`'s Currency
        Value     IS `Principal Amount`'s Value
                      TIMES (`Monthly Interest Rate` TIMES `Compound Factor`)
                          DIVIDED BY (`Compound Factor` MINUS 1)
    WHERE
        `Compound Factor` MEANS
            `Base to the power of`
                (1 PLUS `Monthly Interest Rate`)
                `Monthly Installments`
```

This implements the standard loan amortization formula (PMT):

```
Payment = Principal × (Rate × (1+Rate)^n) / ((1+Rate)^n - 1)
```

### 3.4 The Core Obligation

```l4
GIVEN `Outstanding Payment Amount` IS A Money
`Payment Obligations` MEANS
    IF `Outstanding Payment Amount`'s Value GREATER THAN 0
        THEN  PARTY   `The Borrower`
              MUST    `pay monthly installment to`
                          EXACTLY `The Lender`
                          `Amount Transferred` PROVIDED
                             `is money at least equal within error`
                                `Amount Transferred`
                                (`Next Payment Due`'s Amount)
              WITHIN  `Next Payment Due Date`
              HENCE   `Payment Obligations`
                          (Money WITH
                              Currency  IS `Monthly Installment Amount`'s Currency
                              Value     IS `Outstanding Payment Amount`'s Value
                                               MINUS `Amount Transferred`'s Value)
              LEST    -- Late payment with penalty
                      PARTY   `The Borrower`
                      MUST    `pay monthly installment to`
                                  EXACTLY `The Lender`
                                  `Amount Transferred` PROVIDED
                                      `is money at least equal within error`
                                          `Amount Transferred`
                                          `Next Payment Due Amount With Penalty`
                      WITHIN  `Default After Days Beyond Commencement`
                      LEST    -- Default: full amount due
                              PARTY  `The Borrower`
                              MUST   `pay monthly installment to`
                                          EXACTLY `The Lender`
                                          `Amount Transferred` PROVIDED
                                              `is money at least equal within error`
                                                  `Amount Transferred`
                                                  `All Outstanding Debts`
        ELSE  FULFILLED
```

**The structure:**

1. **Primary obligation**: Pay the installment by the due date
2. **First LEST** (grace period): If missed, pay installment + 5% penalty within grace period
3. **Second LEST** (default): If still missed, entire outstanding balance becomes due immediately

This is CSL's reparation pattern: violation → reparation → breach.

### 3.5 Private Definitions with WHERE

The `WHERE` clause defines helper values scoped to `Payment Obligations`:

```l4
    WHERE
        `Installments Covered Since Commencement` MEANS
            (`Total Repayment Amount`'s Value
                MINUS `Outstanding Payment Amount`'s Value)
                    DIVIDED BY `Monthly Installment Amount`'s Value

        `Next Payment Due` MEANS
            Payment WITH
                Amount IS
                    Money WITH
                        Currency  IS `Monthly Installment Amount`'s Currency
                        Value     IS `The lesser of`
                                         (`Monthly Installment Amount`'s Value)
                                         (`Outstanding Payment Amount`'s Value)
                `Days Beyond Commencement` IS
                    (`Installments Covered Since Commencement` PLUS 1)
                        TIMES `Days in a month`

        `Next Payment Due Date` MEANS
            CEILING (`Next Payment Due`'s `Days Beyond Commencement`)
                PLUS `Late Payment Penalty`'s `Grace Period Days`

        `Next Payment Due Amount With Penalty` MEANS
            Money WITH
                Currency  IS `Next Payment Due`'s Amount's Currency
                Value     IS `Next Payment Due`'s Amount's Value
                                 PLUS `Next Payment Due`'s Amount's Value
                                      TIMES `Late Payment Penalty`'s `Interest Rate`
```

---

## 4. Testing Contracts with #TRACE

### 4.1 The #TRACE Command

`#TRACE` simulates contract execution with a sequence of events:

```l4
#TRACE <contract> <initial-args> AT Day <start-date> WITH
    PARTY <who> DOES <action> <args> AT Day <when>
    PARTY <who> DOES <action> <args> AT Day <when>
    ...
```

### 4.2 The Happy Path

```l4
#TRACE `Payment Obligations` `Total Repayment Amount` AT Day (February 4 2025) WITH
    PARTY `The Borrower` DOES `pay monthly installment to` `The Lender` (USD 2256.46) AT Day (March 4 2025)
    PARTY `The Borrower` DOES `pay monthly installment to` `The Lender` (USD 2256.46) AT Day (April 4 2025)
    PARTY `The Borrower` DOES `pay monthly installment to` `The Lender` (USD 2256.46) AT Day (May 4 2025)
    PARTY `The Borrower` DOES `pay monthly installment to` `The Lender` (USD 2256.46) AT Day (June 4 2025)
    PARTY `The Borrower` DOES `pay monthly installment to` `The Lender` (USD 2256.46) AT Day (July 4 2025)
    PARTY `The Borrower` DOES `pay monthly installment to` `The Lender` (USD 2256.46) AT Day (August 4 2025)
    PARTY `The Borrower` DOES `pay monthly installment to` `The Lender` (USD 2256.46) AT Day (September 4 2025)
    PARTY `The Borrower` DOES `pay monthly installment to` `The Lender` (USD 2256.46) AT Day (October 4 2025)
    PARTY `The Borrower` DOES `pay monthly installment to` `The Lender` (USD 2256.46) AT Day (November 4 2025)
    PARTY `The Borrower` DOES `pay monthly installment to` `The Lender` (USD 2256.46) AT Day (December 4 2025)
    PARTY `The Borrower` DOES `pay monthly installment to` `The Lender` (USD 2256.46) AT Day (January 4 2026)
    PARTY `The Borrower` DOES `pay monthly installment to` `The Lender` (USD 2256.46) AT Day (February 4 2026)
```

**Result:** `FULFILLED`

All 12 payments made on time → contract successfully completed.

### 4.3 A Late Payment Scenario

```l4
#TRACE `Payment Obligations` `Total Repayment Amount` AT Day (February 4 2025) WITH
    PARTY `The Borrower` DOES `pay monthly installment to` `The Lender` (USD 2256.46) AT Day (April 3 2025)
```

The borrower pays once, but late (April 3 instead of March). The trace returns the **residual obligation** — what's still required:

```
PARTY `The Borrower`
MUST `pay monthly installment to` EXACTLY `The Lender`
     `Amount Transferred` PROVIDED `is money at least equal within error`
         OF `Amount Transferred`, `Next Payment Due Amount With Penalty`
WITHIN `Default After Days Beyond Commencement`
LEST PARTY `The Borrower`
     MUST `pay monthly installment to` EXACTLY `The Lender`
          `Amount Transferred` PROVIDED `is money at least equal within error`
              OF `Amount Transferred`, `All Outstanding Debts`
```

The contract is now in the "penalty" state — borrower must pay the penalty amount or face default.

### 4.4 Using #EVAL for Calculations

Test individual calculations before running traces:

```l4
#EVAL `Total Interest Amount`        -- [Money OF "USD", 2077.49]
#EVAL `Total Repayment Amount`       -- [Money OF "USD", 27077.49]
#EVAL `Monthly Interest Rate`        -- [0.0125]
#EVAL `Monthly Installment Amount`   -- [Money OF "USD", 2256.46]
```

---

## 5. Composing Obligations

### 5.1 Conjunction (AND)

Both obligations must be fulfilled:

```l4
`Complete Transaction` MEANS
    PARTY `Seller`
    MUST  `deliver goods`
    WITHIN 14
    AND
    PARTY `Buyer`
    MUST  `make payment`
    WITHIN 30
```

If either party breaches, the earliest breach determines the overall verdict.

### 5.2 Disjunction (OR)

At least one obligation must be fulfilled:

```l4
`Payment Options` MEANS
    PARTY `Buyer`
    MUST  `pay in full`
    WITHIN 30
    OR
    PARTY `Buyer`
    MUST  `pay first installment`
    WITHIN 30
    HENCE `Installment Schedule`
```

**Important:** For disjunction, L4 requires that the same party be blamed in both branches (deterministic blame assignment).

### 5.3 The Promissory Note's Structure

The promissory note uses conjunction implicitly:

```
Primary payment obligation
  └── HENCE: recursive call (next installment)
  └── LEST: penalty obligation
              └── LEST: default (full amount due)
```

And the penalty vs. full payment is essentially a sequential attempt:

- Try to pay with penalty → if successful, continue
- If that fails too → breach

---

## 6. Type Definitions for Contracts

### 6.1 Core Types

```l4
DECLARE Money
    HAS Currency IS A STRING
        Value    IS A NUMBER

DECLARE Payment
    HAS Amount                     IS A Money
        `Days Beyond Commencement` IS A NUMBER

DECLARE Penalty
    HAS `Interest Rate`     IS A NUMBER
        `Grace Period Days` IS A NUMBER
```

### 6.2 Party Types with Variants

```l4
DECLARE Borrower IS ONE OF
   `Individual Borrower`
        HAS Individual IS A `Natural Person`
   `Commercial Borrower`
        HAS Entity     IS A Company

DECLARE Lender IS ONE OF
   `Individual Lender`
        HAS Individual IS A `Natural Person`
   `Commercial Lender`
        HAS Entity     IS A Company
```

### 6.3 Action Types

Actions that appear in obligations should be declared:

```l4
DECLARE `pay monthly installment to`
    HAS Recipient IS A Lender
        Amount    IS A Money
```

**Convention:** Action names start lowercase and often contain verbs (`pay to`, `deliver goods`, `sign document`).

---

## 7. Prohibitions with MUST NOT / SHANT

### 7.1 Native Prohibition Syntax

L4 supports prohibitions with `MUST NOT` or `SHANT` keywords:

```l4
PARTY Employee
MUST NOT disclose
WITHIN 365
LEST PARTY Employee
     MUST `pay damages`
     WITHIN 30
```

Or equivalently with `SHANT`:

```l4
PARTY Employee
SHANT disclose
WITHIN 365
LEST BREACH BY Employee BECAUSE "violated NDA"
```

### 7.2 Flipped Polarity for Natural Reading

For prohibitions, HENCE and LEST have **flipped meanings** compared to MUST:

| Modal      | HENCE triggers when...      | LEST triggers when...          |
| ---------- | --------------------------- | ------------------------------ |
| `MUST`     | action is taken             | deadline passes without action |
| `MUST NOT` | deadline passes (respected) | action is taken (violation!)   |

This matches natural English: "Don't smoke, **lest** you face consequences."

```l4
PARTY Alice
SHANT smoke
WITHIN 30
HENCE FULFILLED    -- 30 days pass without smoking → success
LEST BREACH        -- Alice smokes → breach!
```

### 7.3 The BREACH Terminal Clause

`BREACH` can be used as an explicit terminal clause, parallel to `FULFILLED`:

```l4
LEST BREACH                                    -- Simple form
LEST BREACH BY `The Employee`                  -- With explicit party
LEST BREACH BY `The Employee` BECAUSE "violated policy"  -- With reason
```

When evaluated, `BREACH` produces `ValBreached` containing:

- The party who breached (explicit or inferred from enclosing PARTY)
- The reason for breach (explicit or generated from context)

### 7.4 MAY is Syntactic Sugar

`MAY` exists in the lexer but is essentially equivalent to an external choice:

```l4
-- These are semantically similar:
PARTY Buyer MAY `return goods` WITHIN 14 HENCE `Refund Process`

-- Desugars conceptually to:
IF `return goods` happens WITHIN 14
   THEN `Refund Process`
   ELSE FULFILLED  -- No penalty for not exercising the option
```

A `MAY` without consequences for counter-parties is just a no-op.

---

## 8. Best Practices

### 8.1 Naming Conventions

- **Parties**: Title case, often with article (`The Borrower`, `The Lender`)
- **Actions**: Lowercase with verb (`pay to`, `deliver`, `sign`)
- **Types**: Title case (`Money`, `Payment`, `Penalty`)
- **Calculated values**: Descriptive (`Next Payment Due Date`, `All Outstanding Debts`)

### 8.2 Always Specify LEST

Every `MUST` should have a `LEST`:

```l4
-- Good: explicit consequence
PARTY Borrower
MUST  pay
WITHIN 30
LEST  PARTY Borrower
      MUST pay-with-penalty
      WITHIN 45

-- Risky: implicit breach
PARTY Borrower
MUST  pay
WITHIN 30
-- If deadline passes, immediate breach with no recovery option
```

### 8.3 Use WHERE for Complex Calculations

Keep the obligation structure readable by moving calculations to `WHERE`:

```l4
`Payment Obligation` MEANS
    PARTY `Borrower`
    MUST  `pay` `Amount Due`
    WITHIN `Due Date`
    HENCE `Next Obligation`
    WHERE
        `Amount Due` MEANS ...complex calculation...
        `Due Date` MEANS ...complex calculation...
        `Next Obligation` MEANS ...
```

### 8.4 Test Incrementally

1. First, `#EVAL` your calculations
2. Then, `#TRACE` the happy path
3. Then, `#TRACE` edge cases (late payments, missed payments, overpayments)
4. Finally, `#TRACE` breach scenarios

### 8.5 Document Assumptions

The promissory note includes helpful comments:

```l4
`Late Payment Penalty` MEANS Penalty WITH
    `Interest Rate`     IS 5%   -- Notice the importance of indentation
    `Grace Period Days` IS 10

-- This function just enables more readable writing of Money amounts
GIVEN a IS A NUMBER
USD MEANS
   Money WITH
      Currency IS "USD"
      Value    IS a
```

---

## 9. Exercises

### Exercise 1: Simple Sale Contract

Create a sale contract where:

- Seller must deliver goods within 14 days
- Upon delivery, Buyer must pay within 7 days
- If Buyer doesn't pay, they must pay with 10% penalty within 14 more days

### Exercise 2: Subscription Service

Model a monthly subscription where:

- Customer pays $50/month
- Payment due on the 1st of each month
- 5-day grace period before late fee ($5)
- Service cancelled after 2 consecutive missed payments

### Exercise 3: Non-Disclosure Agreement

Using the workaround pattern, model an NDA where:

- Employee must not disclose confidential information for 2 years
- If they do, they owe $100,000 in damages within 30 days

### Exercise 4: Escrow Transaction

Model a three-party escrow:

- Buyer deposits funds with Escrow Agent
- Seller delivers goods to Buyer
- Upon confirmed delivery, Escrow Agent releases funds to Seller
- If goods not delivered within 14 days, Escrow Agent returns funds to Buyer

---

## 10. Further Reading

### Academic Papers

- **Hvitved, Tom. "Contract Formalisation and Modular Implementation of Domain-Specific Languages"** — PhD thesis containing the full CSL specification (Chapter 2)

- **Hvitved, Klaedtke, Zalinescu. "A Trace-Based Model for Multiparty Contracts"** — The published paper version

### Related L4 Documentation

- [Regulative Rules Spec](../regulative-spec.org) — Technical specification
- [Regulative Deontics](../regulative-deontics.md) — Philosophy of MUST/MAY in L4
- [Promissory Note Example](../../jl4/examples/legal/promissory-note.l4) — Full source code

### Conceptual Background

- **Deontic Logic** — The logic of obligation, permission, and prohibition
- **Temporal Logic** — Reasoning about time and deadlines
- **Labeled Transition Systems** — Formal model for state machines

---

## Summary

L4's regulative rules bring academic rigor to contract formalization:

| Concept               | L4 Implementation                   |
| --------------------- | ----------------------------------- |
| Trace-based semantics | `#TRACE` command                    |
| Blame assignment      | `PARTY` specifies who's responsible |
| Deadlines             | `WITHIN` (days as number)           |
| Continuations         | `HENCE` (on fulfillment)            |
| Reparations           | `LEST` (on violation)               |
| Termination           | `FULFILLED`                         |
| Composition           | Implicit AND, explicit OR           |

The promissory note example demonstrates that real financial contracts can be fully specified in L4, with automatic calculation of payments, interest, and penalties.

**Current limitations** (prohibitions, explicit breach) will be addressed in future L4 versions, but the core obligation pattern is stable and production-ready.

---

**Next steps:**

- Try the exercises above
- Read the promissory note source code in full
- Experiment with `#TRACE` scenarios
- Consider what contracts in your domain could be formalized

[← Back to Course Overview](README.md)
