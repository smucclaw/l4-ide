# Regulative Rules

How L4 models obligations, permissions, and prohibitions — and how those rules play out against a stream of events.

---

## What Are Regulative Rules?

**Regulative rules** govern behaviour. They tell parties what they must, may, or must not do. They are the foundation of contracts, regulations, and laws.

Compare to **constitutive rules**, which define what things _are_ (DECLARE and DECIDE statements). A constitutive rule answers "is the borrower a small business?". A regulative rule answers "what is the borrower obligated to do now, given everything that has happened so far?".

### Examples

| Rule Type   | Legal Text                                              | L4 Concept |
| ----------- | ------------------------------------------------------- | ---------- |
| Obligation  | "The seller shall deliver the goods"                    | MUST       |
| Permission  | "The buyer may inspect the goods"                       | MAY        |
| Prohibition | "Employees shall not disclose confidential information" | SHANT      |

---

## The Five Slots of a Regulative Rule

L4 represents a regulative rule as a single atomic unit — a _deontic particle_. Every such particle has the same five slots:

1. **Who** has the duty (`PARTY`)
2. **What** they must, may, or must not do (`MUST` / `MAY` / `SHANT`)
3. **When** the duty applies (`WITHIN`)
4. **What follows on success** (`HENCE`)
5. **What follows on failure** (`LEST`)

```l4
GIVETH A DEONTIC Actor Action
`delivery obligation` MEANS
    PARTY  Seller
    MUST   `deliver goods`
    WITHIN 14
    HENCE  `payment due`
    LEST   BREACH BY Seller
```

`PARTY`, the modal, and the action are required. `WITHIN`, `HENCE`, and `LEST` all have sensible defaults (covered below). Larger contracts are built by composition — every `HENCE` or `LEST` branch can itself be another five-slot particle.

The `DEONTIC` type parameterises over the actor type and the action type, so different contracts can use different domain-specific parties and actions while sharing the same machinery.

---

## The Mental Model: A Contract Awaiting Events

This is the single most important idea in the regulative system, and the one most often missed.

> **A regulative rule by itself does not decide anything. It is a contract that lies dormant until you feed it a starting time and a sequence of events.**

In other words, a `DEONTIC` value behaves like a function with two inputs:

```
contract  ⨯  (start time, events)  →  verdict
```

- **Start time** — a number that locates "now" on a shared timeline. All deadlines and event timestamps are measured against the same timeline.
- **Events** — an ordered list of things that actually happened. Each event records _who acted_, _what they did_, and _when_.
- **Verdict** — one of `FULFILLED`, `BREACH`, or a _residual obligation_ (a contract that is still alive but partially advanced). See [The Three Possible Outcomes](#the-three-possible-outcomes) below.

This is why a regulative rule is more than just a boolean check. It is a stateful object that watches a timeline, advances as events arrive, and decides on success, failure, or "still waiting" — all while preserving enough information to be resumed later.

---

## Events and the Timeline

An event is a triple:

```
(party, action, timestamp)
```

- **party** — who did it (must match a value of the contract's actor type)
- **action** — what they did (matched against the contract's action shape — see [Conditional obligations with PROVIDED](#conditional-obligations-with-provided))
- **timestamp** — a number on the same timeline as the start time and `WITHIN` deadlines

Events are consumed **in order**. As each event is examined, two pieces of internal state move:

- the contract's **current time** advances to the event's timestamp
- the **remaining window** under `WITHIN` shrinks by however much time has passed

If an event sits beyond the deadline, the contract takes its failure branch (or, for `SHANT`, its success branch — see below) without even examining what the event was. If the event is within time, the contract checks whether _this_ event matches _this_ obligation (right party, right action shape, guard satisfied). If it matches, the contract advances along `HENCE` (or `LEST` for a violated prohibition). If it doesn't match, the event is skipped and the next one is tried — with the clock now further along and the remaining window smaller.

The timeline is purely numeric. It can stand for days, seconds, business hours, or any other unit — L4 doesn't care, as long as deadlines and event timestamps use the same units.

---

## Obligations: MUST

An **obligation** requires a party to perform an action by a deadline.

```l4
PARTY  Seller
MUST   `deliver goods`
WITHIN 14
HENCE  FULFILLED          -- if delivered in time, the duty is discharged
LEST   BREACH BY Seller   -- if not delivered in time, the seller breaches
```

A matching event before the deadline takes the `HENCE` branch. The deadline passing without a matching event takes the `LEST` branch.

### Conditional obligations with PROVIDED

`PROVIDED` adds a boolean guard to the action. After an event matches the action's shape, the guard is evaluated; if false, the match is rejected and the next event is tried.

```l4
PARTY  Buyer
MUST   `pay amount` PROVIDED amount AT LEAST `Payment Due`
WITHIN 30
HENCE  FULFILLED
LEST   BREACH
```

Use `PROVIDED` to express "the action only counts if …".

---

## Permissions: MAY

A **permission** allows a party to perform an action without requiring it.

```l4
PARTY Buyer
MAY   `inspect goods`
HENCE FULFILLED
```

The crucial difference from `MUST`: **not exercising a permission is not a breach.**

| What happened              | MUST               | MAY              |
| -------------------------- | ------------------ | ---------------- |
| Action performed in time   | HENCE (compliance) | HENCE (used)     |
| Deadline passed, no action | LEST (breach)      | HENCE (declined) |

Permissions are used for optional rights ("buyer may return within 30 days"), discretionary powers ("the commissioner may issue a notice"), and conditional entitlements ("may claim a refund if defective").

---

## Prohibitions: SHANT — and the Polarity Flip

A **prohibition** forbids a party from performing an action during a window.

```l4
PARTY Employee
SHANT `disclose confidential information`
HENCE FULFILLED
LEST  BREACH BY Employee
```

For prohibitions, **the meaning of "success" and "failure" is inverted compared to `MUST`**:

|                            | MUST       | SHANT          |
| -------------------------- | ---------- | -------------- |
| Action performed           | Compliance | **Breach**     |
| Deadline passed, no action | Breach     | **Compliance** |

This is the _polarity flip_ of `SHANT`: for a prohibition, **the deadline passing quietly is the good outcome**, and a matching event is the violation. That's why `SHANT … HENCE` fires when the timer runs out without anything happening, while `MUST … HENCE` fires when something _does_ happen.

A useful way to remember it: `HENCE` always names the legally desirable outcome, and `LEST` always names the legally undesirable one. Whether that corresponds to "event happened" or "deadline passed" depends on the modal.

---

## Deadlines: WITHIN

`WITHIN` sets a relative time window — how long the duty remains live.

```l4
PARTY  Seller
MUST   `deliver goods`
WITHIN 14
```

The deadline is `start_time + 14`. As the contract advances through events, the remaining window shrinks: after consuming an event at time 12, what was originally `WITHIN 14` is effectively `WITHIN 2` of any subsequent event.

A duration can optionally be anchored to a specific event with `OF`:

```l4
PARTY  Seller
MUST   deliver
WITHIN 5 days OF `order confirmation`
```

Without a `WITHIN`, the obligation is _ongoing_ and only fires (or breaches) when an explicit fulfilment or breach is reached:

```l4
PARTY Employee
MUST  `maintain confidentiality`
-- no WITHIN: open-ended duty
HENCE FULFILLED
LEST  BREACH
```

`BEFORE` (absolute deadlines, like "by 1 January 2026") is planned but not yet implemented — use `WITHIN` and a relative duration for now.

---

## Consequences: HENCE and LEST

Every regulative rule names two outgoing branches.

### HENCE — the desirable path

```l4
HENCE FULFILLED                       -- contract complete
HENCE `next obligation`               -- chain to another rule
HENCE obligation1 RAND obligation2    -- chain to several at once
```

### LEST — the undesirable path

```l4
LEST BREACH                                    -- terminal breach
LEST BREACH BY Seller                          -- breach by a specific party
LEST BREACH BY Seller BECAUSE "late delivery"  -- breach with a reason
LEST `penalty clause`                          -- alternative obligation (reparation)
```

`BECAUSE` attaches a human-readable reason to a breach. Use it. Breach reasons are the thing a legal reviewer or downstream system actually reads.

### What "HENCE" and "LEST" trigger on, per modal

| Modal   | HENCE fires when                        | LEST fires when                        | HENCE default | LEST default |
| ------- | --------------------------------------- | -------------------------------------- | ------------- | ------------ |
| `DO`    | action is taken                         | deadline passes                        | _(required)_  | _(required)_ |
| `MUST`  | action is taken                         | deadline passes without action         | `FULFILLED`   | `BREACH`     |
| `MAY`   | action is taken                         | deadline passes (permission unused)    | `FULFILLED`   | `FULFILLED`  |
| `SHANT` | deadline passes (prohibition respected) | action is taken (prohibition violated) | `FULFILLED`   | `BREACH`     |

If you omit `HENCE` or `LEST`, the default above is supplied. `DO` is the bare form: it neither requires nor forbids the action, and both branches must be given explicitly.

---

## The Three Possible Outcomes

Once a contract has consumed an event stream, it returns one of three things.

### 1. `FULFILLED`

Every obligation was discharged. There is nothing left for any party to do under this contract.

### 2. `BREACH`

Some party failed an obligation (or violated a prohibition) and there was no reparation clause to absorb the failure. A `BREACH` records _who_ breached and, if `BECAUSE` was used, _why_.

### 3. A **residual obligation**

This is the most powerful outcome. If the event stream runs out _before_ the contract has fulfilled or breached, the result is a new `DEONTIC` value — the contract in its current state, with:

- the current time advanced to where the event stream stopped
- any `WITHIN` window shrunk by however much time was consumed
- any partially-completed `HENCE` chain ready to resume from its current step

The residual obligation is the contract, frozen mid-flight, as a first-class value. You can:

- inspect it (what's still owed by whom, with how much time left?)
- store it (persist a long-running contract between sessions)
- feed it more events later (resume execution where you left off)

This is why L4 is a _contract_ language rather than just a _checking_ language: contracts can survive partial execution and be resumed indefinitely.

---

## Reparation Clauses: LEST as Another Obligation

`LEST` does not have to mean "the contract dies here". It can name _another_ obligation — a fallback duty that fires when the primary one fails. This is how reparation, penalties, and grace periods are expressed.

```l4
-- Pay on time, or pay more later
PARTY  `The Borrower`
MUST   pay `outstanding amount`
WITHIN `due date`
HENCE  FULFILLED
LEST (
    PARTY  `The Borrower`
    MUST   pay `outstanding amount with 5% penalty`
    WITHIN `default deadline`
    -- no LEST here: missing this deadline is a terminal breach
)
```

The inner obligation has no `LEST`, so missing _that_ deadline is the true breach. The outer `LEST` is a softening — the law gives the borrower another chance at a higher price.

You can stack reparation clauses several deep. The contract walks down the chain until something fulfills, or until a leaf with no `LEST` is reached and breaches.

---

## Chaining Obligations with HENCE

Real contracts have sequences of duties. `HENCE` strings them together:

```l4
PARTY  Seller
MUST   `deliver goods`
WITHIN 14
HENCE
    PARTY  Buyer
    MUST   `pay invoice`
    WITHIN 30
    HENCE  FULFILLED
    LEST   BREACH BY Buyer
LEST BREACH BY Seller
```

The behaviour:

1. Seller has 14 time units to deliver.
2. _If they deliver_, the buyer is now obliged to pay within 30 of that delivery.
3. _If the seller does not deliver_, the contract terminates in breach.
4. If the buyer then pays in time, the contract is `FULFILLED`. If not, the buyer breaches.

The same event stream is threaded through the whole chain — once the seller's delivery event is consumed, the contract advances to the buyer's payment obligation and continues reading from the next event.

---

## Recursive Obligations

For recurring duties — instalments, subscriptions, periodic reporting — define a function that emits the _next_ period's obligation:

```l4
GIVEN remainingBalance IS A NUMBER
`monthly payments` remainingBalance MEANS
    IF remainingBalance GREATER THAN 0
    THEN PARTY  `The Borrower`
         MUST   pay `monthly installment`
         WITHIN `next due date`
         HENCE  `monthly payments` (remainingBalance MINUS `monthly installment`)
         LEST   `monthly payments` (remainingBalance PLUS `late penalty`)
    ELSE FULFILLED
```

`HENCE` reduces the balance and re-issues the same obligation for the following period. `LEST` adds a penalty and tries again. The recursion bottoms out at `FULFILLED` when the balance is paid down.

Recursive obligations let a single rule represent open-ended duties without exploding into a fixed-length sequence.

---

## Parallel Composition: RAND and ROR

Two obligations can be active at the same time.

### RAND — both must be fulfilled

```l4
(PARTY Seller MUST `deliver goods`    WITHIN 14 HENCE FULFILLED LEST BREACH)
RAND
(PARTY Seller MUST `provide warranty` WITHIN 14 HENCE FULFILLED LEST BREACH)
```

Both sides run on the same event stream. The compound `FULFILLS` only if both sides fulfil. If _either_ side breaches, the compound breaches.

### ROR — either one is enough

```l4
(PARTY Seller MUST `ship goods`     WITHIN 14 HENCE FULFILLED LEST BREACH)
ROR
(PARTY Seller MUST `arrange pickup` WITHIN 7  HENCE FULFILLED LEST BREACH)
```

The first side to fulfil determines the outcome — a race. Only if _both_ sides breach does the compound breach.

**Precedence:** `RAND` binds tighter than `ROR`, so `A ROR B RAND C` means `A ROR (B RAND C)`.

---

## The Regulative Rule Lifecycle

```
                  ┌──────────────────────────┐
                  │      Contract starts     │
                  │ (with a start time and   │
                  │  an event stream)        │
                  └────────────┬─────────────┘
                               │
                               ▼
                  ┌──────────────────────────┐
                  │   Awaiting next event    │◄─────┐
                  └────────────┬─────────────┘      │
                               │                    │
              ┌────────────────┼────────────────┐   │
              ▼                ▼                ▼   │
        ┌──────────┐    ┌─────────────┐  ┌────────────────┐
        │ Matching │    │  Deadline   │  │ Non-matching   │
        │  event   │    │   passes    │  │  event arrives │
        │ arrives  │    │             │  │                │
        └────┬─────┘    └──────┬──────┘  └──────┬─────────┘
             │                 │                │
   (MUST/MAY: HENCE)   (MUST: LEST,            │
   (SHANT:    LEST)     SHANT/MAY: HENCE)      │
             │                 │           (skip, try next ─┘
             ▼                 ▼            with clock advanced)
       ┌──────────┐      ┌──────────┐
       │  HENCE   │      │   LEST   │
       │  branch  │      │  branch  │
       └────┬─────┘      └────┬─────┘
            │                 │
            ▼                 ▼
   ┌────────────────────────────────────┐
   │   FULFILLED  /  BREACH  /  another │
   │  obligation (which becomes the     │
   │  new waiting state — go to top)    │
   └────────────────────────────────────┘
```

If the event stream is exhausted before this loop terminates, the entire in-progress contract is returned as a residual obligation.

---

## Testing with #TRACE

`#TRACE` is the primary way to exercise a regulative rule. It runs the contract against a fixed start time and event sequence and reports the verdict.

```l4
-- Happy path: delivery happens in time
#TRACE `delivery obligation` AT 0 WITH
    PARTY Seller DOES `deliver goods` AT 10
-- Result: FULFILLED

-- Breach: no event in the deadline window
#TRACE `delivery obligation` AT 0 WITH
    -- (no events)
-- Result: BREACH (after day 14)

-- Wrong party: the obligated party did not act
#TRACE `delivery obligation` AT 0 WITH
    PARTY Buyer DOES `deliver goods` AT 10
-- Result: BREACH — Buyer is not the obligated party, so the event is skipped,
--                  and the deadline passes with no matching event.

-- Residual: events run out before the deadline
#TRACE `delivery obligation` AT 0 WITH
    -- (no events, and the contract was never advanced past WITHIN 14)
-- Result: a residual obligation, with the same duty but clock partly used up
```

The starting number after `AT` and the event timestamps after `AT` (inside the `WITH` block) sit on the same numeric timeline as any `WITHIN` durations in the contract.

You can also drive a contract programmatically with the underlying evaluation function, which has the same shape as the mental model:

```
EVALTRACE  :  DEONTIC party action          -- the contract
           →  NUMBER                        -- start time
           →  LIST (EVENT party action)     -- events
           →  DEONTIC party action          -- result (terminal or residual)
```

This signature is the formal version of "a contract is a function of (start time, events)". Calling `EVALTRACE` repeatedly with the residual from a previous call is how a long-running contract is advanced piece by piece.

---

## State Graphs

L4 can visualise regulative rules as state machines, showing the possible paths through `HENCE` and `LEST`:

```bash
l4 state-graph mycontract.l4
# Or, from a Haskell checkout:
cabal run l4 -- state-graph mycontract.l4
```

The graph shows the initial state, intermediate obligations, deadline transitions, and terminal `FULFILLED` / `BREACH` nodes.

---

## Best Practices

### 1. Always name both paths

```l4
-- ✅ Both paths specified
MUST  action
HENCE FULFILLED
LEST  BREACH

-- ❌ Missing LEST — relies on defaults; intent is less clear to readers
MUST  action
HENCE FULFILLED
```

The defaults are sensible, but writing both branches explicitly makes legal review easier.

### 2. Attach blame and reasons

```l4
-- ✅ Clear who breached, and why
LEST BREACH BY Seller BECAUSE "failed to deliver within 14 days"

-- Less useful in downstream reports
LEST BREACH
```

### 3. Name actions like the legal text

```l4
-- ✅ Reads like the contract it formalises
MUST `deliver goods in accordance with specification`

-- ❌ Loses the legal nuance
MUST deliver
```

### 4. Prefer reparation chains to silent defaults

When a deadline failure has a remedy ("pay within 30 or pay extra within 60"), express the remedy as a nested obligation in `LEST` rather than letting the contract terminate in an unrecoverable breach.

---

## Summary

| Concept   | Purpose                                 | Syntax                                              |
| --------- | --------------------------------------- | --------------------------------------------------- |
| MUST      | Obligation                              | `PARTY x MUST action`                               |
| MAY       | Permission                              | `PARTY x MAY action`                                |
| SHANT     | Prohibition (polarity flips!)           | `PARTY x SHANT action`                              |
| DO        | Bare action (no defaults)               | `PARTY x DO action`                                 |
| WITHIN    | Relative deadline                       | `WITHIN duration`                                   |
| HENCE     | Desirable-outcome branch                | `HENCE nextRule`                                    |
| LEST      | Undesirable-outcome branch              | `LEST consequence` (terminal or another obligation) |
| PROVIDED  | Guard on action matching                | `MUST action PROVIDED condition`                    |
| EXACTLY   | Equality match instead of pattern match | `MUST EXACTLY expr`                                 |
| BREACH    | Terminal failure state                  | `LEST BREACH BY party BECAUSE reason`               |
| FULFILLED | Terminal success state                  | `HENCE FULFILLED`                                   |
| RAND      | Parallel: all must fulfil               | `rule1 RAND rule2`                                  |
| ROR       | Parallel: any one suffices              | `rule1 ROR rule2`                                   |
| #TRACE    | Simulate against a timeline             | `#TRACE rule AT t WITH events…`                     |

Key conceptual takeaways:

- A regulative rule is a _deontic particle_ with five slots: party, modal-and-action, deadline, hence, lest.
- A regulative rule is _dormant_ until it is given a start time and an event stream.
- Events are `(party, action, timestamp)` triples on a shared numeric timeline.
- The contract advances through events in order; the clock moves forward and any `WITHIN` window shrinks.
- The result is one of three things: `FULFILLED`, `BREACH`, or a _residual obligation_ that can be resumed later.
- `SHANT` flips the polarity: for a prohibition, the deadline passing quietly is the success path.
- `LEST` can name another obligation rather than a terminal breach — that is how reparation clauses, penalties, and grace periods are expressed.

---

## Further Reading

- [Foundation Course Module 5](../../courses/foundation/module-5-regulative.md) — Hands-on tutorial
- [Regulative Rule Keywords](../../reference/regulative/README.md) — Full keyword reference
- [DEONTIC](../../reference/regulative/DEONTIC.md) — The regulative type, in detail
- [PARTY](../../reference/regulative/PARTY.md), [MUST](../../reference/regulative/MUST.md), [MAY](../../reference/regulative/MAY.md), [SHANT](../../reference/regulative/SHANT.md), [BECAUSE](../../reference/regulative/BECAUSE.md) — Individual keyword pages
