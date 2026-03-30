# Regulative Rule Keywords

Regulative keywords express legal obligations, permissions, prohibitions, and their consequences. They form the core of L4's contract and regulation modeling.

## Overview

### Deontic Modalities

| Keyword           | Meaning                   |
| ----------------- | ------------------------- |
| [MUST](MUST.md)   | Obligation (required)     |
| [MAY](MAY.md)     | Permission (allowed)      |
| [SHANT](SHANT.md) | Prohibition (forbidden)   |
| DO                | Possibility (optionality) |

**[DEONTIC](DEONTIC.md)** - Regulative Type

### Rule Structure

| Keyword               | Purpose                           |
| --------------------- | --------------------------------- |
| [PARTY](PARTY.md)     | Who has the obligation/permission |
| WITHIN                | Temporal deadline (relative)      |
| HENCE                 | Consequence on fulfillment        |
| LEST                  | Consequence on breach             |
| PROVIDED              | Guard condition on action         |
| EXACTLY               | Exact value matching on action    |
| BREACH                | Terminal violation state          |
| [BECAUSE](BECAUSE.md) | Reason for breach                 |
| FULFILLED             | Terminal success state            |

### Parallel Obligation Combinators

| Keyword | Purpose                               |
| ------- | ------------------------------------- |
| RAND    | Parallel AND -- all must be fulfilled |
| ROR     | Parallel OR -- any one sufficient     |

### Planned Keywords

| Keyword | Purpose                      | Status          |
| ------- | ---------------------------- | --------------- |
| BEFORE  | Temporal deadline (absolute) | Not implemented |

## Basic Rule Structure

```l4
PARTY partyName
MUST/MAY/SHANT/DO action
WITHIN deadline
```

### Example

```l4
DECLARE Person IS ONE OF Alice, Bob
DECLARE Action IS ONE OF pay HAS amount IS A NUMBER

paymentObligation MEANS
  PARTY Alice
  MUST pay 100
  WITHIN 30
```

## WITHIN (Temporal Deadline)

Specifies a relative time duration within which an action must/may be performed.

### Syntax

```l4
PARTY ...
MUST action
WITHIN duration
```

The duration can optionally be anchored to an event with `OF`:

```l4
WITHIN 5 days OF notice
```

### Examples

```l4
-- Simple deadline
PARTY Alice MUST pay 100 WITHIN 30

-- Anchored to an event
PARTY Seller MUST deliver WITHIN 5 days OF `order confirmation`
```

### See Also

- **BEFORE** (planned, not yet implemented -- will support absolute deadlines)

## HENCE (Fulfillment Consequence)

Specifies what happens on the "success" path of a deontic rule. Chains obligations sequentially so that fulfilling one triggers the next.

The meaning of "success" depends on the deontic modal:

| Modal   | HENCE triggers when...                  | Default if omitted |
| ------- | --------------------------------------- | ------------------ |
| `DO`    | action is taken                         | _(required)_       |
| `MUST`  | action is taken                         | `FULFILLED`        |
| `MAY`   | action is taken                         | `FULFILLED`        |
| `SHANT` | deadline passes (prohibition respected) | `FULFILLED`        |

### Syntax

```l4
PARTY ...
MUST action
WITHIN deadline
HENCE consequentRule
```

### Examples

```l4
-- Chain of obligations
PARTY Alice
MUST pay 500
WITHIN 7
HENCE (
  PARTY Bob
  MUST deliver "goods"
  WITHIN 14
)

-- Explicit fulfillment
PARTY Seller
MUST deliver
WITHIN 14
HENCE FULFILLED
```

### See Also

- **LEST** -- the "failure" path counterpart
- **FULFILLED** -- terminal success state

## LEST (Breach Consequence)

Specifies what happens on the "failure" path of a deontic rule. Typically used for penalty clauses or fallback obligations.

The meaning of "failure" depends on the deontic modal:

| Modal   | LEST triggers when...                      | Default if omitted |
| ------- | ------------------------------------------ | ------------------ |
| `DO`    | deadline passes                            | _(required)_       |
| `MUST`  | deadline passes without action             | `BREACH`           |
| `MAY`   | deadline passes (permission not exercised) | `FULFILLED`        |
| `SHANT` | action is taken (prohibition violated)     | `BREACH`           |

Note that SHANT flips the polarity: for prohibitions, the action happening is the failure case (LEST), while the deadline passing without action is the success case (HENCE).

### Syntax

```l4
PARTY ...
MUST action
WITHIN deadline
LEST breachConsequence
```

### Examples

```l4
-- Simple breach
PARTY Alice
MUST pay 100
WITHIN 30
LEST BREACH

-- Penalty clause
PARTY Alice
MUST pay 100
WITHIN 30
LEST (
  PARTY Alice
  MUST pay 150
  WITHIN 60
)
```

### See Also

- **HENCE** -- the "success" path counterpart
- **BREACH** -- terminal failure state

## PROVIDED (Guard Condition)

Adds a guard condition to a deontic action. After an event matches the action pattern, the PROVIDED expression is evaluated. If it returns FALSE, the match is rejected and the system tries the next event. Defaults to TRUE if omitted.

Think of it as a pattern-match guard: the action shape must match first, then the guard condition is checked.

### Syntax

```l4
MUST action parameter PROVIDED condition
```

### Examples

```l4
-- Conditional payment
PARTY Bob
MUST payment price PROVIDED price >= 20
WITHIN 3

-- Guard on transferred amount
PARTY borrower
MUST `Amount Transferred`
  PROVIDED `is money at least equal` `Amount Transferred` `Payment Due`
```

### See Also

- **EXACTLY** -- controls pattern vs equality matching of the action itself

## EXACTLY (Exact Action Matching)

Changes how the action is matched against incoming events during contract execution. Without EXACTLY, the action is a pattern (matched structurally, like WHEN in CONSIDER -- can bind variables). With EXACTLY, the action is an expression that is evaluated to a value and compared for equality against the event.

### Syntax

```l4
MUST EXACTLY expression
```

### Examples

```l4
-- Without EXACTLY: "pay" is a pattern, matches any pay-shaped event
PARTY buyer MUST pay

-- With EXACTLY: expression is evaluated, event must equal the result
PARTY lender MUST EXACTLY send capital to borrower

-- Exact value match
PARTY Alice
MUST pay price EXACTLY 100
WITHIN 30
```

### See Also

- **PROVIDED** -- guard condition evaluated after the pattern matches

## BREACH (Terminal Violation State)

Terminal deontic value indicating that an obligation has been violated. Used as the consequence in LEST clauses. Can optionally specify the responsible party and a reason.

### Syntax

```l4
LEST BREACH
LEST BREACH BY party
LEST BREACH BECAUSE reason
LEST BREACH BY party BECAUSE reason
```

### Examples

```l4
-- Simple breach
LEST BREACH

-- With responsible party
LEST BREACH BY Seller

-- With reason
LEST BREACH BECAUSE "delivery deadline exceeded"

-- Full form
LEST BREACH BY Seller BECAUSE "failed to deliver within 14 days"
```

See **[BECAUSE](BECAUSE.md)** for detailed documentation on breach reasons.

### See Also

- **LEST** -- the clause where BREACH typically appears
- **FULFILLED** -- the success counterpart

## FULFILLED (Terminal Success State)

Terminal deontic value indicating that all obligations have been satisfied and no further action is needed. Commonly used as the consequence in HENCE clauses, and as the base case in conditional deontic rules.

### Syntax

```l4
HENCE FULFILLED
```

### Examples

```l4
-- After successful delivery
PARTY Seller
MUST deliver
WITHIN 14
HENCE FULFILLED

-- Base case in conditional rule
IF NOT `conditions precedent are met`
THEN FULFILLED
ELSE PARTY lender MUST ...
```

### See Also

- **HENCE** -- the clause where FULFILLED typically appears
- **BREACH** -- the failure counterpart

## RAND (Parallel AND of Obligations)

Parallel conjunction of deontic obligations. ALL component obligations must be fulfilled for the compound to be fulfilled. If either side breaches, the whole compound breaches (short-circuit).

In concurrency theory terms, this is parallel composition where all threads must complete successfully.

### Syntax

```l4
deonton1 RAND deonton2
```

### Examples

```l4
-- Both obligations must be fulfilled
(PARTY seller MUST deliver WITHIN 14 HENCE FULFILLED LEST BREACH)
RAND
(PARTY buyer MUST pay WITHIN 30 HENCE FULFILLED LEST BREACH)
```

### See Also

- **ROR** -- disjunctive choice (any one sufficient)
- **HENCE**, **LEST** -- consequence clauses within each component

---

## ROR (Parallel OR of Obligations)

Disjunctive choice between deontic obligations. EITHER obligation being fulfilled suffices for the compound to be fulfilled. If either side fulfills, the whole compound fulfills (short-circuit).

In concurrency theory terms, this is a race where the first to complete determines the outcome.

### Syntax

```l4
deonton1 ROR deonton2
```

### Precedence

RAND binds tighter than ROR, so `A ROR B RAND C` means `A ROR (B RAND C)`.

### Examples

```l4
-- Either obligation can fulfill the contract
(PARTY seller MUST ship WITHIN 14 HENCE FULFILLED LEST BREACH)
ROR
(PARTY seller MUST `arrange pickup` WITHIN 7 HENCE FULFILLED LEST BREACH)
```

### See Also

- **RAND** -- parallel conjunction (all must be fulfilled)
- **HENCE**, **LEST** -- consequence clauses within each component

---

## BEFORE (NOT YET IMPLEMENTED)

Planned temporal keyword for specifying absolute deadlines in deontic rules (as opposed to WITHIN, which specifies relative durations).

**Status:** Planned but not yet in the parser. Use WITHIN for relative durations in the meantime.

### Intended Syntax

```l4
PARTY ...
MUST action
BEFORE deadline
```

### See Also

- **WITHIN** -- implemented, for relative durations

---

## Testing with #TRACE

Use `#TRACE` to simulate contract execution.

### Syntax

```l4
#TRACE contractName AT startTime WITH
  PARTY partyName DOES action AT eventTime
  ...
```

### Example

```l4
#TRACE paymentObligation AT 0 WITH
  PARTY Alice DOES pay 100 AT 15
```

## Complete Example

```l4
DECLARE Person IS ONE OF Seller, Buyer
DECLARE Action IS ONE OF
  delivery
  payment HAS amount IS A NUMBER

saleContract MEANS
  PARTY Seller
  MUST delivery
  WITHIN 3
  HENCE (
    PARTY Buyer
    MUST payment 100
    WITHIN 7
  )
  LEST BREACH

#TRACE saleContract AT 0 WITH
  PARTY Seller DOES delivery AT 2
  PARTY Buyer DOES payment 100 AT 5
```

## Related Pages

- **[PARTY](PARTY.md)** - Party declarations
- **[MUST](MUST.md)** - Obligations
- **[MAY](MAY.md)** - Permissions
- **[SHANT](SHANT.md)** - Prohibitions

## See Also

- **[Foundation Course: Regulative Rules](../../courses/foundation/module-5-regulative.md)** - Tutorial
- **[Regulative Rules Concept](../../concepts/legal-modeling/regulative-rules.md)** - Conceptual overview
