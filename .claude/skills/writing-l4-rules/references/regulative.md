# Regulative Rules Reference

Deep dive on L4's regulative machinery: obligations, permissions, prohibitions, deadlines, consequences, and contract-trace simulation. This is L4's unique strength and the part most likely to trip a general-purpose LLM.

**Canonical reference:** <https://legalese.com/l4/reference/regulative.md>

---

## Contents

- [The five-keyword skeleton](#the-five-keyword-skeleton)
- [Deontic modals: MUST, MAY, SHANT, DO](#deontic-modals-must-may-shant-do)
- [HENCE and LEST — the success and failure paths](#hence-and-lest--the-success-and-failure-paths)
- [BREACH, FULFILLED, and BECAUSE](#breach-fulfilled-and-because)
- [PROVIDED and EXACTLY — action matching](#provided-and-exactly--action-matching)
- [WITHIN — deadlines](#within--deadlines)
- [Composition: RAND and ROR](#composition-rand-and-ror)
- [Recursive obligations](#recursive-obligations)
- [#TRACE — simulating contract execution](#trace--simulating-contract-execution)
- [Complete example](#complete-example)

---

## The five-keyword skeleton

```l4
PARTY   actor
WHO     qualifier              -- optional
MUST    action parameters      -- or MAY / SHANT / DO
WITHIN  deadline
HENCE   nextState              -- optional; consequence on success
LEST    penaltyState           -- optional; consequence on failure
```

Only `PARTY` + modal + action are required. `WITHIN`, `HENCE`, and `LEST` all have sensible defaults (see the tables below).

---

## Deontic modals: MUST, MAY, SHANT, DO

| Keyword | Meaning                   | Reference                                               |
| ------- | ------------------------- | ------------------------------------------------------- |
| `MUST`  | Obligation (required)     | <https://legalese.com/l4/reference/regulative/MUST.md>  |
| `MAY`   | Permission (allowed)      | <https://legalese.com/l4/reference/regulative/MAY.md>   |
| `SHANT` | Prohibition (forbidden)   | <https://legalese.com/l4/reference/regulative/SHANT.md> |
| `DO`    | Optionality / possibility |                                                         |

```l4
PARTY Alice MUST pay 100 WITHIN 30                      -- obligation
PARTY Bob   MAY  withdraw funds                         -- permission
PARTY Alice SHANT smoke WITHIN 30                       -- prohibition
```

`SHANT` is fully supported. If anything you have seen says "no MUST NOT in L4" it is out of date.

---

## HENCE and LEST — the success and failure paths

`HENCE` is the consequence on success; `LEST` is the consequence on failure. **What counts as "success" depends on the modal**, and this is the single most non-obvious thing about L4 regulative rules.

| Modal   | HENCE fires when                        | LEST fires when                        | HENCE default | LEST default |
| ------- | --------------------------------------- | -------------------------------------- | ------------- | ------------ |
| `DO`    | action is taken                         | deadline passes                        | _(required)_  | _(required)_ |
| `MUST`  | action is taken                         | deadline passes without action         | `FULFILLED`   | `BREACH`     |
| `MAY`   | action is taken                         | deadline passes (permission unused)    | `FULFILLED`   | `FULFILLED`  |
| `SHANT` | deadline passes (prohibition respected) | action is taken (prohibition violated) | `FULFILLED`   | `BREACH`     |

**`SHANT` flips the polarity**: for a prohibition, doing the action is the failure. This is why `SHANT … HENCE` fires when the deadline passes quietly — that is the good outcome.

`LEST` can chain another obligation for a reparation clause:

```l4
-- Pay-or-pay-more penalty clause
PARTY `The Borrower`
MUST  pay `outstanding amount`
WITHIN `due date`
HENCE FULFILLED
LEST (
    PARTY `The Borrower`
    MUST  pay `outstanding amount with 5% penalty`
    WITHIN `default deadline`
    -- no LEST: missing this deadline is a breach
)
```

Without a `LEST` clause on the inner obligation, missing that deadline is a terminal breach.

---

## BREACH, FULFILLED, and BECAUSE

Both are **keywords**, not just outcomes from the evaluator.

```l4
HENCE FULFILLED
LEST  BREACH
LEST  BREACH BY Seller
LEST  BREACH BECAUSE "delivery deadline exceeded"
LEST  BREACH BY Seller BECAUSE "failed to deliver within 14 days"
```

`BECAUSE` attaches a reason to a breach and is reported in the trace output. Use it — breach reasons are the thing a legal reviewer or downstream system actually wants to read.

Reference: <https://legalese.com/l4/reference/regulative/BECAUSE.md>

---

## PROVIDED and EXACTLY — action matching

### PROVIDED — guard condition

`PROVIDED` adds a boolean guard to an action. After an event matches the action pattern structurally, the guard is evaluated; if it returns `FALSE`, the match is rejected and the system tries the next event.

```l4
-- Conditional payment: only counts if >= 20
PARTY Bob
MUST payment price PROVIDED price AT LEAST 20
WITHIN 3

-- Guard on a transferred amount
PARTY borrower
MUST `Amount Transferred`
     PROVIDED `Amount Transferred` AT LEAST `Payment Due`
```

### EXACTLY — equality match

Without `EXACTLY`, the action is a **pattern** (matched structurally, with variable binding). With `EXACTLY`, the action is an **expression** that is evaluated and compared for equality.

```l4
-- Pattern: matches any pay-shaped event
PARTY buyer MUST pay

-- Expression equality: the event must equal the result of this expression
PARTY lender MUST EXACTLY send capital to borrower

-- Exact value
PARTY Alice MUST pay price EXACTLY 100 WITHIN 30
```

---

## WITHIN — deadlines

`WITHIN` takes a duration. The duration can optionally be anchored to an event with `OF`:

```l4
PARTY Alice  MUST pay 100 WITHIN 30
PARTY Seller MUST deliver WITHIN 5 days OF `order confirmation`
```

**`BEFORE` (absolute deadlines) is planned but not yet implemented** — use `WITHIN` for now.

---

## Composition: RAND and ROR

`RAND` and `ROR` compose obligations in parallel.

- **`RAND`** — parallel AND. All components must be fulfilled; if any side breaches, the compound breaches.
- **`ROR`** — parallel OR. Fulfilling any one side fulfills the compound.
- **Precedence:** `RAND` binds tighter than `ROR`, so `A ROR B RAND C` means `A ROR (B RAND C)`.

```l4
-- Both parties must fulfill their halves
(PARTY seller MUST deliver WITHIN 14 HENCE FULFILLED LEST BREACH)
RAND
(PARTY buyer  MUST pay     WITHIN 30 HENCE FULFILLED LEST BREACH)

-- Seller has two ways to satisfy the obligation
(PARTY seller MUST ship              WITHIN 14 HENCE FULFILLED LEST BREACH)
ROR
(PARTY seller MUST `arrange pickup`  WITHIN 7  HENCE FULFILLED LEST BREACH)
```

`AND` and `OR` at the top level of a regulative rule are also accepted as composition forms in many programs; the authoritative semantics live at <https://legalese.com/l4/reference/regulative.md>.

---

## Recursive obligations

For recurring payments (loans, subscriptions, installments), define a function that emits the next period's obligation:

```l4
GIVEN remainingBalance IS A NUMBER
`monthly payments` remainingBalance MEANS
    IF remainingBalance GREATER THAN 0
    THEN PARTY `The Borrower`
         MUST pay `monthly installment`
         WITHIN `next due date`
         HENCE `monthly payments` (remainingBalance MINUS `monthly installment`)
         LEST  `monthly payments` (remainingBalance PLUS `late penalty`)
    ELSE FULFILLED
```

The `HENCE` branch reduces the balance; the `LEST` branch increases it with a penalty and re-emits the obligation. The recursion base is `FULFILLED`.

---

## #TRACE — simulating contract execution

`#TRACE` runs a contract against a sequence of timestamped events and reports either `FULFILLED`, a `BREACH`, or a **residual obligation** (what's still owed).

### Syntax

```l4
#TRACE contractName AT startTime WITH
    PARTY partyName DOES action AT eventTime
    PARTY partyName DOES action AT eventTime
    ...
```

Timestamps are numbers on a shared timeline. For date-based contracts, the canonical docs show a `Day (…)` form; use whatever form your rule uses for `WITHIN`.

### Happy-path example

```l4
#TRACE paymentObligation AT 0 WITH
    PARTY Alice DOES pay 100 AT 15
-- Result: FULFILLED
```

### Residual-obligation example

```l4
#TRACE paymentObligation AT 0 WITH
    -- no events within deadline
-- Result: the LEST branch — either BREACH, or the reparation obligation
-- if the rule has a LEST clause
```

The residual is the most useful output from a trace: it is the contract in its current state, as a machine-readable value, showing exactly what is still owed by whom.

Reference: <https://legalese.com/l4/reference/regulative.md>

---

## Complete example

From the canonical README — a two-party sale with delivery and payment:

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
    PARTY Buyer  DOES payment 100 AT 5
-- Result: FULFILLED
```

---

## See also

- <https://legalese.com/l4/reference/regulative.md> — full keyword reference
- <https://legalese.com/l4/reference/regulative/MUST.md>
- <https://legalese.com/l4/reference/regulative/MAY.md>
- <https://legalese.com/l4/reference/regulative/SHANT.md>
- <https://legalese.com/l4/reference/regulative/PARTY.md>
- <https://legalese.com/l4/reference/regulative/BECAUSE.md>
- <https://legalese.com/l4/reference/regulative/DEONTIC.md>
- <https://legalese.com/l4/concepts/legal-modeling/regulative-rules.md> — conceptual overview
- <https://legalese.com/l4/courses/foundation/module-5-regulative.md> — foundation course module
