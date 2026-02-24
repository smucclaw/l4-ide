# Specification: Deontic Deadline Elaboration

**Status:** Draft
**Date:** 2026-02-25 (rev 3; rev 2 2026-02-25; originally 2026-02-24)
**Author:** Meng Wong
**Branch:** `mengwong/deadlines`
**Related:** `TEMPORAL-MASTER.md`, `TEMPORAL_EVAL_SPEC.md`, `TEMPORAL_PRELUDE_MACROS.md`, `BOUNDED-DEONTICS-SPEC.md`, `UPON-EXTERNAL-EVENTS-SPEC.md`

## Executive Summary

The `WITHIN` keyword in L4's deontic construct (`PARTY/MUST/HENCE/LEST`) currently accepts only a `NUMBER`, hand-waved as "some number of days." Real-world contracts and regulations require richer temporal deadlines: absolute dates, relative durations with explicit units, business-day calculations, and jurisdictional calendars.

Additionally, the `due` field in the AST is currently `Maybe (Expr n)`, making deadlines optional. This diverges from Hvitved's CSL semantics where deadlines are structurally mandatory. This spec makes deadlines non-optional, introducing `EVENTUALLY` as an explicit escape hatch for perpetual obligations. The LSP flags `EVENTUALLY` with a yellow warning diagnostic to draw attention to unbounded obligations.

This spec proposes a small set of compiler changes that push all temporal richness into the L4 library/mixfix layer, so that jurisdiction-specific deadline vocabulary can be extended by application and library developers without recompiling jl4.

## Motivation

Enterprise contracts express deadlines in diverse ways:

| Pattern             | Example                                 | Current L4                                              |
| ------------------- | --------------------------------------- | ------------------------------------------------------- |
| Absolute date       | "by March 15, 2026"                     | Not expressible in deontic syntax                       |
| Calendar days       | "within 30 days"                        | `WITHIN 30` (implicit, no unit)                         |
| Business days       | "within 5 business days"                | No concept of business days                             |
| Duration with units | "within 3 months"                       | No DURATION type, no calendar-aware month arithmetic    |
| Relative to event   | "within 14 days of signing"             | Must be manually computed outside the deontic construct |
| Combined            | "by the earlier of 30 days or March 15" | Not expressible                                         |
| Perpetual           | "shall not disclose, ever"              | Omit `WITHIN` (silently becomes unbounded — a bug trap; should be `EVENTUALLY`) |

The current implementation has a single temporal slot in the AST (`due :: Maybe (Expr n)`) typed as `number`. The `Maybe` makes the deadline optional: a `PARTY X MUST Y` without `WITHIN` is legal, producing `due = Nothing`. The parser matches only the `WITHIN` keyword. The type checker constrains the expression to `number`. Users who need richer deadlines must manually reduce everything to a day-count, often with awkward `Date to days` conversions.

**The `Maybe` problem:** In Hvitved's CSL — the formal semantics L4's runtime descends from — every clause has the form `⟨party⟩ ⟨action⟩ where ⟨predicate⟩ due ⟨deadline⟩ then ⟨residual⟩ else ⟨residual⟩`. The deadline is **structurally mandatory**. It is what makes trace evaluation decidable: without a deadline, the runtime cannot determine when an obligation transitions from "waiting" to "breached," and contract traces cannot be classified as successful or violated in finite time. The current `Maybe` creates a fourth semantic category — obligations outside time — that CSL's model does not admit.

## Current State

### AST (`Syntax.hs`)

```haskell
data Deonton n = MkDeonton
  { anno :: Anno
  , party :: Expr n
  , action :: RAction n
  , due :: Maybe (Expr n)    -- ← PROBLEM: Maybe makes deadline optional
  , hence :: Maybe (Expr n)
  , lest :: Maybe (Expr n)
  }
```

When `due = Nothing`, the evaluator (`Machine.hs:830`) skips the timing check entirely — the obligation stays pending indefinitely. It can be fulfilled by a matching event but never breached by timeout.

### Parser (`Parser.hs`)

```haskell
deadline current =
  annoLexeme (spacedKeyword_ TKWithin) *> annoHole (indentedExpr current)
```

### Type Checker (`TypeCheck.hs`)

```haskell
dueR <- traverse (\e -> checkExpr ExpectRegulativeDeadlineContext e number) due
```

### Supporting Infrastructure

- `DATE`, `TIME`, and `DATETIME` are now builtin types in the evaluator.
- `daydate.l4` provides date constructors, weekday predicates (`is weekday`, `is weekend`), relative phrases (`the day after`, `the month after`), operator overloads (`DATE PLUS NUMBER`), and combinators (`the earlier of`, `the later of`).
- The `TemporalContext` scaffolding tracks system time, valid time, and rule-effective time. `TODAY`/`NOW` builtins read from it.

## Design: Core Insight

**A deadline is a time window.** In Hvitved's CSL, a clause's temporal constraint has both a lower bound (earliest the action counts) and an upper bound (latest before breach). A "simple" deadline like "within 30 days" is really the window `[trigger, trigger+30]`. A vesting schedule like "exercisable after 1 year but within 3 years" is the window `[trigger+1y, trigger+3y]`.

The overwhelmingly common case has a trivial lower bound (the trigger time itself), so the surface syntax stays clean — `WITHIN 30` still works. The lower bound only appears when the author explicitly adds it via the `after` combinator.

## Design Decisions

### Decision 1: Lean on Library/Mixfix, Not the Parser

Duration units (`DAYS`, `WEEKS`, `MONTHS`, `YEARS`, `BUSINESS DAYS`), calendar support, and anchoring syntax all live in the L4 library/mixfix layer rather than as parser keywords. This means:

- Jurisdiction-specific calendars are `.l4` files, not GHC code.
- Domain authors can define `TRADING DAYS`, `SCHOOL DAYS`, `BANKING DAYS` without compiler changes.
- The platform play is preserved: an ecosystem of temporal libraries can grow independently.

### Decision 2: BY and WITHIN as Parser-Level Keywords

Only two keywords introduce the deadline slot:

- **`BY`** (new) -- introduces an absolute `DATE` expression.
- **`WITHIN`** (existing) -- introduces a relative deadline expression that depends on an anchor.

Both produce a `DATE`-typed expression in the `due` field. The semantic distinction (absolute vs relative) is resolved in the library layer.

### Decision 3: Implicit Anchor via `TRIGGER_DATE` Builtin

L4's runtime is ultimately inspired by Hvitved's trace-based model of contracts (descending from CSP). The runtime evaluates a trace of timestamped events to determine contract residuation and validity. Each event in the trace carries a timestamp, so when a deontic obligation activates (via an event matching a `HENCE` or `UPON` clause), the triggering event's timestamp is naturally available.

A new builtin `TRIGGER_DATE` exposes this timestamp to the deadline expression. It reads the date of the event that activated the current obligation from the trace, rather than requiring a separate context field. This is analogous to how `TODAY` reads `tcSystemTime`, but grounded in the event trace semantics.

Default-anchored durations (the overwhelmingly common case) resolve against `TRIGGER_DATE`. Explicitly-anchored durations use the `OF` mixfix form to name a different anchor.

### Decision 4: Backward Compatibility via Implicit Coercion

Existing `WITHIN <number>` (bare number, no unit) is treated as `WITHIN <number> DAYS` via implicit coercion: if the expression after `WITHIN` has type `NUMBER`, the compiler wraps it as `TRIGGER_DATE PLUS n`. A deprecation warning encourages migration to the explicit unit form.

### Decision 5: Mandatory Deadlines (`due` is Non-Optional)

Following CSL's design, the deadline slot becomes **structurally mandatory**. The AST changes from `Maybe (Expr n)` to `Expr n`:

```haskell
-- Before:
  , due :: Maybe (Expr n)
-- After:
  , due :: Expr n
```

Every deontic construct must specify `WITHIN <expr>`, `BY <expr>`, or `EVENTUALLY`. This brings L4 into alignment with Hvitved's trace semantics and catches drafting errors where an author forgets to specify a deadline.

**Perpetual obligations** — those intended to have no temporal bound — use the `EVENTUALLY` keyword:

```l4
-- Marriage vows: perpetual obligations
PARTY Spouse1
  MUST `love and cherish`
  EVENTUALLY
  HENCE FULFILLED
  LEST BREACH

-- Non-compete: bounded prohibition (this one has a real deadline)
PARTY Employee
  SHANT `compete with employer`
  WITHIN 730
  HENCE FULFILLED
  LEST BREACH
```

`EVENTUALLY` is a parser-level keyword (a third alternative alongside `WITHIN` and `BY`) that desugars to `DeadlineWindow { notBefore = TRIGGER_DATE, notAfter = FOREVER }`. The name comes from temporal logic (LTL's `◇φ` / `Fφ`), where it means "at some point in the future, without specifying when."

`EVENTUALLY` carries documentary weight: the drafter has **deliberately** chosen an unbounded obligation, rather than accidentally omitting a deadline. The LSP reinforces this by flagging every `EVENTUALLY` with a **modal-sensitive yellow warning diagnostic** (see Decision 6).

#### Achievement vs Maintenance Obligations

`EVENTUALLY` interacts with the deontic modal in a way that reveals a fundamental dichotomy in legal obligations:

**Achievement obligations** are one-time events that discharge the obligation once performed. "Pay $1000" is fulfilled by a single matching event in the trace. They pair naturally with deadlines: "pay within 30 days."

**Maintenance obligations** are continuous states that must hold throughout the entire window. "Do not disclose confidential information" is fulfilled by the *absence* of any violating event over the duration. Prohibitions (`SHANT`/`MUST NOT`) are inherently maintenance obligations.

| Modal | + `WITHIN 30` | + `EVENTUALLY` |
|-------|---------------|----------------|
| **MUST** (achievement) | "Do X within 30 days" — natural | "Do X... sometime?" — suspicious, likely a drafting gap |
| **SHANT** (maintenance) | "Don't do X for 30 days" — bounded prohibition | "Never do X" — natural for NDAs, fidelity clauses |
| **MAY** (permission) | "You may do X within 30 days" — option window | "You may do X at any time" — perpetual option, worth noting |

The evaluator already handles this correctly through the modal:
- `MUST` + deadline passes without action → `BREACH`
- `SHANT` + deadline passes without prohibited action → `FULFILLED` (prohibition respected)
- `MAY` + deadline passes without exercise → `FULFILLED` (permission expired unexercised)

With `EVENTUALLY`, these become:
- `MUST` + `EVENTUALLY` → the obligation can be fulfilled at any time, but can *never* be breached by timeout (only by explicit `LEST` logic). This is almost always a drafting error for achievement obligations.
- `SHANT` + `EVENTUALLY` → the prohibition never expires. Any violation at any future time is a breach. This is the natural formalization of perpetual NDAs, fidelity clauses, and similar maintenance obligations.
- `MAY` + `EVENTUALLY` → the permission/option never expires. The party can exercise it at any time. Less common but meaningful (e.g., a lifetime warranty).

This dichotomy should be taught as a core concept: **deadlines bound achievements; `EVENTUALLY` is natural for maintenance, suspicious for achievements.** The LSP diagnostic reflects this (see Decision 6).

### Decision 6: `EVENTUALLY` as Parser Keyword, `FOREVER` as Library Constant

`EVENTUALLY` is a parser-level keyword — the third deadline alternative:

```
deadline  ::=  WITHIN      <expr>
            |  BY          <expr>
            |  EVENTUALLY  [AFTER <expr>]
```

The parser desugars `EVENTUALLY` into an expression that produces `DeadlineWindow { notBefore = TRIGGER_DATE, notAfter = FOREVER }`. With the optional `AFTER` clause, the lower bound shifts: `EVENTUALLY AFTER <dur>` desugars to `DeadlineWindow { notBefore = TRIGGER_DATE + <dur>, notAfter = FOREVER }`.

`FOREVER` itself remains a library-defined `DATE` constant (not a compiler builtin):

```l4
GIVETH A DATE
FOREVER MEANS December 31 9999
```

The evaluator does not need special handling — `FOREVER` is just a very large `DATE`. The deadline comparison `stamp > deadline` will never be true for any realistic event timestamp, so the obligation effectively never times out.

This is preferable to an `Infinity` sentinel because:

- It keeps the type system clean: `FOREVER` has type `DATE`, not a special value.
- The evaluator needs zero changes — existing comparison logic works.
- It is inspectable: traces, visualizations, and debugging output show a concrete date.
- It parallels the "year 9999" convention used in SQL, insurance, and financial systems for open-ended intervals.

**LSP diagnostic:** The language server emits a **modal-sensitive** warning diagnostic (yellow squiggly) on every `EVENTUALLY` keyword. The message varies based on the deontic modal, reflecting the achievement/maintenance dichotomy:

| Modal | Severity | Code | Message |
|-------|----------|------|---------|
| **MUST** / **DO** | Warning | `unbounded-achievement` | "Achievement obligation with no deadline — when must this be performed?" |
| **SHANT** / **MUST NOT** | Info | `perpetual-prohibition` | "Perpetual prohibition — is this restriction truly unbounded?" |
| **MAY** | Info | `perpetual-permission` | "Perpetual permission — does this option ever expire?" |

The `MUST` + `EVENTUALLY` case gets `Warning` severity (yellow) because unbounded achievement obligations are almost always drafting errors — the author likely forgot to specify when the action must happen. The `SHANT` and `MAY` cases get `Info` severity (blue) because perpetual prohibitions and permissions are common and often intentional, but still worth flagging for review.

All three appear in the Problems panel and as underlines in the editor, making them visible during drafting and code review without blocking compilation.

### Decision 7: Waiting Periods via Library-Level `DeadlineWindow` (Hybrid Approach)

CSL's `due` supports both lower and upper bounds on timing. Real contracts express this as waiting periods, vesting schedules, and exercise windows:

| Pattern | Example |
|---------|---------|
| Vesting window | "exercisable after 1 year but within 3 years" |
| Insurance waiting period | "claims payable after 30 days, within 90 days" |
| Notice window | "not earlier than 60 days and not later than 90 days before termination" |
| Cooling-off period | "may cancel after 7 days, within 14 days" |

**Why this matters at the evaluator level:** When an event arrives before the lower bound, the evaluator must **skip it** (the obligation persists, try next event). This is semantically distinct from both "event matches" (within the window) and "deadline missed" (after the upper bound). The evaluator currently has a two-way branch (`stamp > deadline` vs else); with a lower bound it becomes three-way.

**The hybrid design:**

1. **Internally**, the `due` field holds a `DeadlineWindow` — a record with two `DATE` fields. The evaluator unpacks both bounds and implements three-way event classification.

2. **At the parser level**, nothing changes — still just `WITHIN <expr>` and `BY <expr>`. The parser remains thin per Decision 1.

3. **At the library level**, `after` is a mixfix combinator that attaches a lower bound to a deadline expression. A bare `DATE` auto-coerces to a `DeadlineWindow` with `notBefore = TRIGGER_DATE`.

**Library definitions:**

```l4
DECLARE DeadlineWindow HAS
  `not before` IS A DATE
  `not after`  IS A DATE

-- Auto-coercion: bare DATE → window with trivial lower bound
GIVEN upper IS A DATE
GIVETH A DeadlineWindow
`deadline by` upper MEANS
    DeadlineWindow WITH
        `not before` IS TRIGGER_DATE
        `not after`  IS upper

-- The `after` combinator: attaches a lower bound
GIVEN upper IS A DATE
      lower IS A DATE
GIVETH A DeadlineWindow
upper `after` lower MEANS
    DeadlineWindow WITH
        `not before` IS lower
        `not after`  IS upper

-- Convenience: duration-based lower bound (default-anchored)
GIVEN upper IS A DATE
      n     IS A NUMBER
GIVETH A DeadlineWindow
upper `after` n `days` MEANS
    DeadlineWindow WITH
        `not before` IS TRIGGER_DATE PLUS n
        `not after`  IS upper
```

**Surface syntax examples:**

```l4
-- Simple deadline (no waiting period) — common case unchanged
PARTY Seller
MUST `deliver goods`
WITHIN 14 `days`
-- auto-coerces to DeadlineWindow { notBefore = TRIGGER_DATE, notAfter = TRIGGER_DATE + 14 }

-- Stock option vesting window
PARTY Holder
MAY `exercise option`
WITHIN 3 `years` `after` 1 `year`
-- DeadlineWindow { notBefore = TRIGGER_DATE + 1y, notAfter = TRIGGER_DATE + 3y }

-- Insurance waiting period
PARTY Insurer
MUST `pay claim`
WITHIN 90 `days` `after` 30 `days`
-- DeadlineWindow { notBefore = TRIGGER_DATE + 30, notAfter = TRIGGER_DATE + 90 }

-- Absolute bounds
PARTY Tenant
MUST `give notice`
BY December 31 2026 `after` October 1 2026
-- DeadlineWindow { notBefore = Oct 1 2026, notAfter = Dec 31 2026 }

-- Unbounded with waiting period (e.g., stock option with vesting cliff)
PARTY Employee
MAY `exercise option`
EVENTUALLY AFTER 1 `year`
-- DeadlineWindow { notBefore = TRIGGER_DATE + 1y, notAfter = FOREVER }
```

**Evaluator change** (in `Contract5 CheckTiming`):

```haskell
-- Before (two-way):
if stamp > deadline then {breach/fulfill} else {check party/action}

-- After (three-way):
if stamp > upperBound then {breach/fulfill based on modal}
else if stamp < lowerBound then {skip event, try next — obligation persists}
else {within window — check party/action}
```

Events before the lower bound are treated identically to events for the wrong party — the obligation persists and the evaluator moves on to the next event in the trace. The obligation's remaining window shrinks as time advances (just as the remaining deadline does today).

**Sugarless form for advanced library authors:**

Since `DeadlineWindow` is a plain library-defined record type, advanced users can construct arbitrarily complex temporal predicates without using the `after` combinator:

```l4
-- Direct construction with computed bounds
WITHIN DeadlineWindow WITH
    `not before` IS `the later of` (`vesting date`) AND (`regulatory clearance date`)
    `not after`  IS `the earlier of` (`expiry date`) AND (`termination date`)

-- Library author defining a custom temporal combinator
GIVEN exercise_start IS A DATE
      exercise_end   IS A DATE
      blackout_calendar IS A FUNCTION FROM DATE TO BOOLEAN
GIVETH A DeadlineWindow
`exercise window excluding blackouts` MEANS
    DeadlineWindow WITH
        `not before` IS `next non-blackout day` exercise_start blackout_calendar
        `not after`  IS exercise_end
```

This preserves the platform play: the compiler only needs to understand `DeadlineWindow` as a record with two `DATE` fields. All temporal sophistication — blackout calendars, regulatory waiting periods, multi-condition triggers — lives in the library layer where domain experts can extend it without compiler changes.

## Compiler Changes

Five changes to Haskell source:

### 1. Make `due` non-optional in the AST

In `Syntax.hs`:

```haskell
-- Before:
  , due :: Maybe (Expr n)
-- After:
  , due :: Expr n
```

All downstream consumers of `Deonton` (`TypeCheck.hs`, `Machine.hs`, `Desugar.hs`, pretty-printers) must be updated to remove `Maybe` handling. In the evaluator, the `Left Nothing` branch (`Machine.hs:830`) that skips timing is deleted — every obligation now has a deadline.

### 2. Change `due` type from `number` to `DeadlineWindow`

In `TypeCheck.hs`:

```haskell
-- Before:
dueR <- traverse (\e -> checkExpr ExpectRegulativeDeadlineContext e number) due
-- After:
dueR <- checkExpr ExpectRegulativeDeadlineContext due deadlineWindow
```

With layered auto-coercion:
- If the expression has type `DeadlineWindow`, accept as-is (the `after` combinator was used).
- If the expression has type `DATE`, auto-coerce to `DeadlineWindow { notBefore = TRIGGER_DATE, notAfter = expr }`.
- If the expression has type `NUMBER`, auto-coerce to `DeadlineWindow { notBefore = TRIGGER_DATE, notAfter = TRIGGER_DATE PLUS n }` and emit a deprecation warning.

### 3. Make `deadline` mandatory in the parser

In `Parser.hs`, the `deadline` parser currently returns `Maybe (Expr n)` via `optional`. It must become non-optional — a missing `WITHIN`/`BY` is now a parse error:

```haskell
-- Before (returns Maybe):
deadline current = optional $
  annoLexeme (spacedKeyword_ TKWithin) *> annoHole (indentedExpr current)

-- After (returns Expr, with BY and EVENTUALLY alternatives):
deadline current =
      annoLexeme (spacedKeyword_ TKWithin)      *> annoHole (indentedExpr current)
  <|> annoLexeme (spacedKeyword_ TKBy)           *> annoHole (indentedExpr current)
  <|> annoLexeme (spacedKeyword_ TKEventually)   *> pure eventuallyExpr
```

The parser error message should guide the user: `"expected WITHIN, BY, or EVENTUALLY — use EVENTUALLY for perpetual obligations"`.

Add `TKBy` to the lexer's keyword table. (Note: `TKBy` already exists for `BREACH BY` -- context-sensitive disambiguation is needed, or a distinct token like `TKByDate`.)

### 4. Add `TRIGGER_DATE` Builtin

In `TypeCheck/Environment.hs`, add `TRIGGER_DATE` as a builtin of type `DATE`. In the evaluator, it reads the timestamp of the event that activated the current obligation from the obligation evaluation context. This aligns with Hvitved's trace-based contract semantics: each event in the trace is timestamped, and when a deontic obligation fires (via `HENCE` or `UPON`), the triggering event's timestamp becomes `TRIGGER_DATE`.

Implementation: `TRIGGER_DATE` reads from the evaluator's obligation context — specifically, it maps to `time'` in `Machine.hs`'s `Contract5 CheckTiming`. The evaluator injects this value into the environment when processing a deontic construct. For `UPON`-triggered obligations, it is the event's timestamp; for `HENCE`-chained obligations, it is the timestamp of the fulfilling event from the predecessor. This is implemented by adding a new `NullaryTriggerDateSerial` case to the evaluator's builtin dispatch (alongside `NullaryTodaySerial` and `NullaryNowSerial`), which reads from the obligation's reference time rather than from `TemporalContext`.

### 5. Three-Way Timing Check in the Evaluator

In `Machine.hs`, `Contract5 CheckTiming` currently implements a two-way branch. It must become three-way to handle the lower bound from `DeadlineWindow`:

```haskell
-- Before (two-way):
if stamp > deadline
  then {breach/fulfill based on modal}
  else {check party/action, update remaining deadline}

-- After (three-way):
if stamp > upperBound
  then {breach/fulfill based on modal}
else if stamp < lowerBound
  then {skip event — too early, obligation persists with updated window}
else {within window — check party/action}
```

When an event arrives before the lower bound, the evaluator skips it identically to how it handles party mismatches — the obligation persists and the next event is examined. Both bounds shrink as time advances: `newUpper = upper - (stamp - time')` and `newLower = max 0 (lower - (stamp - time'))`.

## Library Design

### Duration Vocabulary (durations.l4 or extension of daydate.l4)

Duration expressions are mixfix functions with overloaded signatures for default-anchored and explicitly-anchored forms:

```l4
-- Default anchor (reads TRIGGER_DATE from context)
GIVEN n IS A NUMBER
GIVETH A DATE
n `days` MEANS
    TRIGGER_DATE PLUS n

-- Explicit anchor
GIVEN n      IS A NUMBER
      anchor IS A DATE
GIVETH A DATE
n `days of` anchor MEANS
    anchor PLUS n

-- Weeks
GIVEN n IS A NUMBER
GIVETH A DATE
n `weeks` MEANS
    TRIGGER_DATE PLUS (n TIMES 7)

GIVEN n      IS A NUMBER
      anchor IS A DATE
GIVETH A DATE
n `weeks of` anchor MEANS
    anchor PLUS (n TIMES 7)

-- Calendar months (calendar-aware, not just 30 days)
GIVEN n IS A NUMBER
GIVETH A DATE
n `months` MEANS
    `add months` n TRIGGER_DATE

GIVEN n      IS A NUMBER
      anchor IS A DATE
GIVETH A DATE
n `months of` anchor MEANS
    `add months` n anchor

-- Years
GIVEN n IS A NUMBER
GIVETH A DATE
n `years` MEANS
    `add years` n TRIGGER_DATE

GIVEN n      IS A NUMBER
      anchor IS A DATE
GIVETH A DATE
n `years of` anchor MEANS
    `add years` n anchor
```

### Business Days (parameterized by calendar)

A **calendar** is a predicate `DATE -> BOOLEAN`:

```l4
GIVEN d IS A DATE
GIVETH A BOOLEAN
`is a Singapore business day` MEANS
    `is weekday` d
    AND NOT `is a Singapore public holiday` d
```

Business day iteration is a higher-order library function:

```l4
GIVEN n        IS A NUMBER
      calendar IS A FUNCTION FROM DATE TO BOOLEAN
      anchor   IS A DATE
GIVETH A DATE
`nth business day after` MEANS
    IF n AT MOST 0
    THEN anchor
    ELSE IF calendar (`the day after` anchor)
         THEN `nth business day after` (n MINUS 1) calendar (`the day after` anchor)
         ELSE `nth business day after` n calendar (`the day after` anchor)
```

Surface forms via mixfix overloading:

```l4
-- Default anchor, default calendar
GIVEN n IS A NUMBER
GIVETH A DATE
n `business days` MEANS
    `nth business day after` n `default calendar` TRIGGER_DATE

-- Default anchor, explicit calendar
GIVEN n   IS A NUMBER
      cal IS A FUNCTION FROM DATE TO BOOLEAN
GIVETH A DATE
n `business days under` cal MEANS
    `nth business day after` n cal TRIGGER_DATE

-- Explicit anchor, explicit calendar
GIVEN n      IS A NUMBER
      cal    IS A FUNCTION FROM DATE TO BOOLEAN
      anchor IS A DATE
GIVETH A DATE
n `business days under` cal `of` anchor MEANS
    `nth business day after` n cal anchor
```

### Jurisdictional Libraries (Examples)

These are standalone `.l4` files that users `IMPORT`:

```l4
-- sg-calendar.l4
§ `Singapore Business Calendar`

GIVEN d IS A DATE
GIVETH A BOOLEAN
`SG business day` MEANS
    `is weekday` d
    AND NOT `is SG public holiday` d

GIVEN d IS A DATE
GIVETH A BOOLEAN
`is SG public holiday` MEANS
       d EQUALS January 1 (DATE_YEAR d)
    OR d EQUALS August 9 (DATE_YEAR d)
    OR `is SG Chinese New Year` d
    -- ... etc
```

```l4
-- ksa-calendar.l4
§ `Saudi Business Calendar`

GIVEN d IS A DATE
GIVETH A BOOLEAN
`KSA business day` MEANS
    `Weekday of` d NOT EQUALS Friday
    AND `Weekday of` d NOT EQUALS Saturday
    AND NOT `is KSA public holiday` d
```

```l4
-- isda-calendars.l4
§ `ISDA Business Day Conventions`

GIVEN n      IS A NUMBER
      anchor IS A DATE
GIVETH A DATE
n `TARGET business days of` anchor MEANS
    `nth business day after` n `TARGET calendar` anchor

GIVEN n      IS A NUMBER
      anchor IS A DATE
GIVETH A DATE
n `London business days of` anchor MEANS
    `nth business day after` n `London banking calendar` anchor
```

## Surface Syntax Examples

```l4
-- Simple relative (default anchor = trigger event, backward compat)
PARTY Seller
MUST `deliver goods`
WITHIN 14 `days`
HENCE FULFILLED
LEST BREACH

-- Business days with default calendar and default anchor
PARTY Buyer
MUST `make payment`
WITHIN 5 `business days`
HENCE FULFILLED
LEST `late payment penalty`

-- Explicit anchor, different from triggering event
PARTY Auditor
MUST `file report`
WITHIN 30 `days of` `the initial closing date` PLUS 2 `years`

-- Absolute deadline
PARTY Commission
MUST `render decision`
BY March 15 2026

-- Cross-border override of calendar
PARTY `London Branch`
MUST `confirm settlement`
WITHIN 2 `business days under` `UK banking calendar`

-- Combined deadline using existing daydate combinators
PARTY Borrower
MUST `repay principal`
BY `the earlier of` (`the maturity date`) AND (`the acceleration date`)

-- Perpetual obligation (yellow flag in LSP)
PARTY Spouse1
MUST `love and cherish`
EVENTUALLY                    -- ⚠ LSP: "Unbounded obligation"
HENCE FULFILLED
LEST BREACH

-- Perpetual prohibition (yellow flag in LSP)
PARTY Employee
SHANT `disclose trade secrets`
EVENTUALLY                    -- ⚠ LSP: "Unbounded obligation"
HENCE FULFILLED
LEST BREACH BY Employee BECAUSE "violated NDA"

-- Backward compat: bare number still works (with deprecation warning)
PARTY Tenant
MUST `vacate premises`
WITHIN 30
```

## Grammar (Informative)

At the parser level, the grammar is deliberately thin. The deadline clause is **mandatory** (not wrapped in `optional`):

```
deonton   ::=  PARTY <expr> <modal> <action> <deadline> [HENCE <expr>] [LEST <expr>]
deadline  ::=  WITHIN      <expr>              -- expr : DeadlineWindow | DATE | NUMBER (with auto-coercion)
            |  BY          <expr>              -- expr : DeadlineWindow | DATE (with auto-coercion)
            |  EVENTUALLY  [AFTER <expr>]      -- desugars to DeadlineWindow { TRIGGER_DATE [+ expr], FOREVER }; LSP flag
```

A missing `deadline` is a **parse error**, not a valid program. The error message should suggest `EVENTUALLY` for intentionally unbounded obligations.

The `<expr>` after WITHIN/BY is an ordinary L4 expression resolved by the mixfix layer. It may evaluate to a `DeadlineWindow` (if the `after` combinator or direct construction was used), a `DATE` (auto-coerced to a window with trivial lower bound), or a `NUMBER` (auto-coerced to `TRIGGER_DATE PLUS n`, with deprecation warning).

All further structure -- duration units, anchoring, calendar selection -- is resolved by the mixfix layer from library definitions. The parser sees only `WITHIN <expr>` or `BY <expr>`.

## Interaction with Existing Temporal Specs

This spec is **orthogonal to** but **compatible with** the multi-temporal EVAL system described in `TEMPORAL_EVAL_SPEC.md` and `TEMPORAL_PRELUDE_MACROS.md`:

- `TRIGGER_DATE` reads from the obligation evaluation context (the triggering event's timestamp), not from `TemporalContext`. However, `EVAL AS OF SYSTEM TIME ...` can still influence deadline reasoning by overriding the `TODAY`/`NOW` builtins used in calendar computations.
- Duration library functions operate on `DATE` values, which the temporal EVAL system can manipulate.
- The `temporal-prelude.l4` macros (when implemented) can compose with deadline expressions.

## Layer Responsibility Summary

| Layer                                        | Responsibility                                                                                                          |
| -------------------------------------------- | ----------------------------------------------------------------------------------------------------------------------- |
| **Compiler**                                 | `BY`/`EVENTUALLY` keywords, `due : DeadlineWindow` (non-optional), `TRIGGER_DATE` builtin, NUMBER/DATE auto-coercion, three-way timing, LSP yellow flag |
| **Core library** (daydate.l4 / durations.l4) | `DeadlineWindow`, `FOREVER`, `after`, `days`, `weeks`, `months`, `years`, `business days`, `of`, `under`, `nth business day after`, `add months`, `add years` |
| **Jurisdiction libraries**                   | Calendar predicates, holiday lists, weekend definitions                                                                 |
| **Domain libraries**                         | ISDA conventions, insurance calendars, government fiscal calendars                                                      |

## Conceptual Framework: Achievement vs Maintenance Obligations

This spec introduces a dichotomy that should be taught as a core L4 concept, alongside the existing deontic modals (MUST/MAY/SHANT).

### Achievement Obligations

An **achievement obligation** is discharged by a single event. The obligation exists to bring about a specific state change: a payment, a delivery, a filing. Once the event occurs, the obligation is `FULFILLED`.

Achievement obligations pair naturally with deadlines:

```l4
-- Achievement: one-time payment event
PARTY Buyer
MUST `pay invoice`
WITHIN 30 `days`
HENCE FULFILLED
LEST BREACH
```

The evaluator fulfills this when it finds a matching event (right party, right action) within the deadline window. `MUST` + `EVENTUALLY` is suspicious for achievements — if there's no deadline, when does the obligation bind?

### Maintenance Obligations

A **maintenance obligation** requires a continuous state to hold over the entire duration. It is fulfilled by the *absence* of any violating event, not by the presence of a satisfying one. Prohibitions (`SHANT`/`MUST NOT`) are inherently maintenance obligations.

Maintenance obligations pair naturally with `EVENTUALLY` for perpetual cases:

```l4
-- Maintenance: perpetual prohibition (NDA)
PARTY Employee
SHANT `disclose confidential information`
EVENTUALLY                    -- ℹ LSP: perpetual prohibition
HENCE FULFILLED
LEST BREACH BY Employee BECAUSE "violated NDA section 3.2"

-- Maintenance: bounded prohibition (non-compete)
PARTY Employee
SHANT `compete with employer`
WITHIN 2 `years`
HENCE FULFILLED               -- prohibition respected after 2 years
LEST BREACH
```

The evaluator fulfills `SHANT` when the deadline passes *without* a matching event. `SHANT` + `EVENTUALLY` means the prohibition never expires — any violation at any future time is a breach.

### The Modeling Gap

Some real-world obligations are semantically maintenance but can only be modeled with `MUST` in current L4:

```l4
-- "Love and cherish" is continuous, not one-time
PARTY Spouse1
MUST `love and cherish`       -- achievement modal for a maintenance obligation
EVENTUALLY                    -- ⚠ LSP: unbounded-achievement
```

The LSP warning here is *correct and useful* — it flags a genuine modeling tension. The drafter should consider whether:
- The obligation is truly one-time (add a deadline)
- The obligation is perpetual maintenance (keep `EVENTUALLY`, document intent)
- A future `MUST CONTINUOUSLY` or `SHALL MAINTAIN` modal would better express the intent

This tension is a known limitation and a candidate for future language extension (see Open Questions).

## Conformance Audit: Existing L4 Files

An audit of all `.l4` files in the repository reveals that most deontic constructs already have `WITHIN` clauses. The following files require changes to conform to mandatory deadlines:

### Files Requiring `EVENTUALLY` (perpetual obligations/prohibitions)

**`jl4/experiments/wedding.l4`** — 10 deontic constructs without deadlines:

| Line | Construct | Type | Resolution |
|------|-----------|------|------------|
| 50 | `PARTY Spouse1 MUST love and cherish` | Maintenance | `EVENTUALLY` — LSP: ⚠ `unbounded-achievement` (MUST) |
| 57 | `PARTY Spouse2 MUST love and cherish` | Maintenance | `EVENTUALLY` — LSP: ⚠ `unbounded-achievement` (MUST) |
| 64 | `PARTY Spouse1 MUST have and hold` | Maintenance | `EVENTUALLY` — LSP: ⚠ `unbounded-achievement` (MUST) |
| 71 | `PARTY Spouse2 MUST have and hold` | Maintenance | `EVENTUALLY` — LSP: ⚠ `unbounded-achievement` (MUST) |
| 82 | `PARTY Spouse1 MUST support` | Maintenance | `EVENTUALLY` — LSP: ⚠ `unbounded-achievement` (MUST) |
| 89 | `PARTY Spouse1 MUST care for` | Maintenance | `EVENTUALLY` — LSP: ⚠ `unbounded-achievement` (MUST) |
| 100 | `PARTY Spouse1 MAY exchange vows` | Permission | `EVENTUALLY` — LSP: ℹ `perpetual-permission` |
| 110 | `PARTY Spouse1 MUST NOT abandon` | Maintenance | `EVENTUALLY` — LSP: ℹ `perpetual-prohibition` |
| 117 | `PARTY Spouse1 MUST NOT be unfaithful` | Maintenance | `EVENTUALLY` — LSP: ℹ `perpetual-prohibition` |
| 128-131 | 4x inline `PARTY X MUST Y` in `RAND` block | Maintenance | `EVENTUALLY` — LSP: ⚠ `unbounded-achievement` |

Note: The wedding vows expose an interesting modeling challenge. "Love and cherish" is semantically a **maintenance** obligation (continuous state), but L4 models it with `MUST` (achievement modal). The LSP will correctly flag these as `unbounded-achievement` (yellow warning). This is arguably the right signal — it prompts the drafter to consider whether the obligation is truly one-time (achievement) or ongoing (maintenance), and to document their intent. A future L4 extension might introduce explicit maintenance obligation syntax.

**`doc/reference/regulative/because-example.l4`** — 3 prohibitions without deadlines:

| Line | Construct | Type | Resolution |
|------|-----------|------|------------|
| 66 | `PARTY Employee SHANT disclose information` | Maintenance | `EVENTUALLY` — LSP: ℹ `perpetual-prohibition` |
| 73 | `PARTY Tenant SHANT disclose information` | Maintenance | `EVENTUALLY` — LSP: ℹ `perpetual-prohibition` |
| 100 | `PARTY Employee SHANT smoke` | Maintenance | `EVENTUALLY` — LSP: ℹ `perpetual-prohibition` |

### Files Requiring Deadline Determination (legal examples)

**`jl4/examples/legal/ceo-performance-award.l4`** — 3 deontic constructs without deadlines:

| Line | Construct | Resolution |
|------|-----------|------------|
| 357-359 | `PARTY Elon Musk MUST forfeit unvested shares` | Needs domain analysis: immediate action after forfeiture trigger? `WITHIN 1` or `WITHIN 30 `days``? |
| 361-363 | `PARTY Elon Musk MAY vest immediately` | Semantically immediate — `WITHIN 1` (or a short window for administrative processing) |
| 369-371 | LEST `PARTY Elon Musk MUST forfeit unvested shares` | Same as line 357 — consequence of failing to maintain eligible service |

Note: the existing `WITHIN` clauses in this file use `Date to days` conversions (e.g., line 368: `` WITHIN `Date to days` (`Performance End Date` MINUS `award state`'s `Current Date`) ``), which is exactly the kind of awkward workaround that the `BY` keyword and DATE-typed deadlines eliminate. Post-migration these become `BY `Performance End Date``.

**`jl4/examples/legal/promissory-note.l4`** — 1 deontic construct without deadline:

| Line | Construct | Resolution |
|------|-----------|------------|
| 111-117 | LEST (innermost) `PARTY The Borrower MUST pay monthly installment to...` | Terminal default obligation — borrower must pay all outstanding debts. Needs domain analysis: acceleration clause typically has a deadline (e.g., `WITHIN 30` for a demand notice period), or `EVENTUALLY` if it becomes an open-ended judgment debt. |

Note: the existing `WITHIN` clauses use computed day-counts relative to commencement (e.g., line 97: `WITHIN Next Payment Due Date`). These are already parameterized correctly but would benefit from the DATE-typed deadline to avoid the day-count indirection.

### Files Requiring Broader Review

**`jl4/experiments/deontic-may.l4`** — exploratory notes file with ~12 deontic constructs, most lacking deadlines. This file is not a compilable L4 program (contains pseudo-code, prose comments, and incomplete fragments). Options:

1. **Formalize**: Convert to compilable L4 with proper deadlines.
2. **Exclude**: Rename to `.l4.draft` or move to a notes directory so it doesn't fail the parser.
3. **Comment out**: Wrap non-compilable fragments in block comments.

### Files Already Conformant (no changes needed)

| File | Deontics | Status |
|------|----------|--------|
| `doc/reference/regulative/must-example.l4` | 27 | All have `WITHIN` |
| `doc/reference/regulative/may-example.l4` | 15+ | All have `WITHIN` |
| `doc/reference/regulative/shant-example.l4` | 8 | All have `WITHIN` |
| `doc/reference/regulative/party-example.l4` | 8 | All have `WITHIN` |
| `doc/reference/regulative/deontic-example.l4` | 25+ | All have `WITHIN` |
| `doc/courses/advanced/module-a3-contracts-examples.l4` | 10+ | All have `WITHIN` |
| `doc/tutorials/getting-started/wedding-vows-example.l4` | 2 | All have `WITHIN` |
| `jl4/examples/ok/contracts.l4` | 6 | All have `WITHIN` |
| `jl4/examples/ok/prohibition.l4` | 8 | All have `WITHIN` |
| `jl4/experiments/restaurant2.l4` | 5 | All have `WITHIN` |

## Implementation Roadmap

### Phase 0: Conformance and FOREVER

- [ ] Define `FOREVER` constant in `daydate.l4` (or new `durations.l4`)
- [ ] Define `DeadlineWindow` type and `after` combinator in library
- [ ] Add `EVENTUALLY` to all perpetual obligations in `jl4/experiments/wedding.l4`
- [ ] Add `EVENTUALLY` to unbounded prohibitions in `doc/reference/regulative/because-example.l4`
- [ ] Add deadlines to `jl4/examples/legal/ceo-performance-award.l4` (3 missing)
- [ ] Add deadline to `jl4/examples/legal/promissory-note.l4` (1 missing, innermost LEST)
- [ ] Migrate `Date to days` workarounds in CEO award to `BY <date>` form
- [ ] Triage `jl4/experiments/deontic-may.l4` (formalize, exclude, or comment out)
- [ ] Verify all `.l4` files pass parser after making `deadline` mandatory

### Phase 1: Compiler Foundation

- [ ] Change `due :: Maybe (Expr n)` to `due :: Expr n` in `Syntax.hs`
- [ ] Remove `Left Nothing` branch from evaluator (`Machine.hs:830`)
- [ ] Update all downstream `Maybe` handling (TypeCheck, Desugar, pretty-printers)
- [ ] Make `deadline` non-optional in parser (remove `optional` wrapper)
- [ ] Add helpful parse error: `"expected WITHIN, BY, or EVENTUALLY — use EVENTUALLY for perpetual obligations"`
- [ ] Add `TKEventually` and `TKAfter` to lexer
- [ ] Reuse existing `TKBy` token (parser context disambiguates — see Resolved Q6)
- [ ] Extend `deadline` parser to accept `BY`, `EVENTUALLY`, and `EVENTUALLY AFTER` alternatives
- [ ] Change `due` type constraint from `number` to `DeadlineWindow` (with layered auto-coercion)
- [ ] Add NUMBER→DATE→DeadlineWindow auto-coercion chain with deprecation warning for NUMBER
- [ ] Implement three-way timing check in evaluator (`Contract5 CheckTiming`)
- [ ] Add `TRIGGER_DATE` builtin (type `DATE`, reads triggering event timestamp from trace)
- [ ] Wire deontic obligation activation to expose event timestamp during deadline evaluation
- [ ] Add modal-sensitive LSP diagnostics for `EVENTUALLY`:
  - MUST/DO + EVENTUALLY → Warning `unbounded-achievement`
  - SHANT/MUST NOT + EVENTUALLY → Info `perpetual-prohibition`
  - MAY + EVENTUALLY → Info `perpetual-permission`

### Phase 2: Core Duration Library

- [ ] Implement `add months` and `add years` (calendar-aware date arithmetic)
- [ ] Implement duration mixfix forms: `days`, `weeks`, `months`, `years` (default-anchored)
- [ ] Implement explicit-anchor forms: `days of`, `weeks of`, `months of`, `years of`
- [ ] Implement `nth business day after` (higher-order, calendar-parameterized)
- [ ] Implement `business days` / `business days under` / `business days under ... of` mixfix forms
- [ ] Add `default calendar` binding (set via import convention — see Resolved Q5)
- [ ] Implement `CountingConvention` type (`exclude trigger date` / `include trigger date`)
- [ ] Default counting convention: `exclude trigger date` (T+1), overridable per library/construct

### Phase 3: Jurisdictional Libraries

- [ ] Singapore business calendar (`sg-holidays.l4`) — holiday LIST OF DATE, imports register `default calendar`
- [ ] UK banking calendar
- [ ] US federal calendar
- [ ] ISDA business day conventions
- [ ] `jl4-calendar-import` CLI tool for generating `.l4` from iCalendar/CSV/API sources

### Phase 4: Testing and Migration

- [ ] Golden tests for `BY` and `WITHIN` with DATE expressions
- [ ] Golden tests for `EVENTUALLY` (perpetual obligations, desugaring to FOREVER)
- [ ] Golden tests for `EVENTUALLY AFTER` (unbounded with lower bound)
- [ ] Golden tests for `DeadlineWindow` with `after` combinator (three-way timing)
- [ ] Golden tests for sugarless `DeadlineWindow` direct construction
- [ ] Golden tests for duration mixfix resolution
- [ ] Golden tests for business day computation
- [ ] Golden test for parse error on missing deadline
- [ ] Verify all existing `.l4` files compile with mandatory deadlines
- [ ] Migration guide for existing `WITHIN <number>` usage
- [ ] Update documentation in `doc/reference/` and `doc/courses/`
- [ ] Write `doc/concepts/achievement-vs-maintenance.md` explaining the obligation dichotomy
- [ ] Add achievement/maintenance examples to `doc/tutorials/`

## Open Questions

*All original open questions have been resolved. See Resolved Questions below.*

~~1. `TRIGGER_DATE` naming and provenance → Resolved #4~~
~~2. Module-level default calendar → Resolved #5~~
~~3. `TKBy` token disambiguation → Resolved #6~~
~~4. Holiday data format → Resolved #7~~
~~5. Counting convention → Resolved #8~~
~~6. FOREVER representation → Resolved #9~~
~~7. `EVENTUALLY` + `after` → Resolved #10~~
~~8. Explicit maintenance obligation syntax → Resolved #3~~

## Resolved Questions

1. **Should `due` be optional (`Maybe`)?** No. Following CSL's design, deadlines are structurally mandatory. Perpetual obligations use the `EVENTUALLY` keyword as an explicit escape hatch, which desugars to `DeadlineWindow { TRIGGER_DATE, FOREVER }`. The LSP flags `EVENTUALLY` with a yellow warning to draw attention to unbounded obligations. This catches drafting errors, aligns with Hvitved's trace semantics, and forces authors to be deliberate. (Resolved 2026-02-25)

2. **How should waiting periods / lower bounds be expressed?** Hybrid approach: internally, `due` holds a `DeadlineWindow` record with `not before` and `not after` fields. The `after` combinator is a library-level mixfix (not a parser keyword), and advanced users can construct `DeadlineWindow` records directly for arbitrarily complex temporal predicates. The parser stays thin (WITHIN/BY/EVENTUALLY), and the evaluator implements three-way timing (too early / within window / deadline missed). (Resolved 2026-02-25)

3. **Explicit maintenance obligation syntax**: No new modal needed. The achievement/maintenance distinction can be modeled with existing primitives: maintenance obligations are `SHANT` prohibitions (e.g., `SHANT breach confidentiality EVENTUALLY`), which the LSP already flags as `ℹ perpetual-prohibition` rather than `⚠ unbounded-achievement`. For the rare case where a positive maintenance obligation is needed (e.g., "must continuously maintain insurance"), the idiomatic pattern is a `MUST` with a recurring `HENCE` chain or a loop construct, which keeps the language orthogonal. A dedicated `MUST CONTINUOUSLY` modal remains a candidate for future extension but is not needed for the current design. (Resolved 2026-02-25)

4. **`TRIGGER_DATE` naming and provenance**: Use `TRIGGER_DATE` as the builtin name. Rationale: (a) it parallels existing all-caps builtins (`TODAY`, `NOW`); (b) its primary audience is library authors writing sugarless `DeadlineWindow` records, for whom technical precision outweighs natural-language readability; (c) L4's backtick convention allows `` `the trigger date` `` as a natural-language alias if desired. **Provenance:** `TRIGGER_DATE` is a new nullary builtin (like `TODAY`/`NOW`) but reads from the obligation evaluation context rather than `TemporalContext`. The evaluator injects it into the environment when processing a deontic construct: for an `UPON`-triggered obligation, it is the event's timestamp; for a `HENCE`-chained obligation, it is the timestamp of the fulfilling event from the predecessor. This maps directly to `time'` in `Machine.hs`'s `Contract5 CheckTiming`. A field-accessor approach (e.g., `THIS.startDate`) was rejected because obligations lack a reified "self" in the current evaluator; adding one would be a larger change. (Resolved 2026-02-25)

5. **Module-level default calendar**: Use the import convention, not a new `USING` keyword. When a module imports a calendar library (e.g., `IMPORT sg-holidays`), the library registers a default `Calendar` value that `business days` and other calendar-aware combinators pick up implicitly. This follows L4's existing pattern where imported libraries provide defaults through the module system (as `daydate.l4` already provides the default `DATE` constructors). Multiple calendar imports are resolved by normal scope rules: the last import wins, or explicit qualification disambiguates (e.g., `sg-holidays.calendar` vs `us-federal.calendar`). No parser change needed; this is purely a library/runtime convention. A `USING` keyword could be added later as syntactic sugar for documentation clarity, but is not required for MVP. (Resolved 2026-02-25)

6. **`TKBy` token disambiguation**: No new token needed. The parser already disambiguates `BY` by context: `BREACH BY` is parsed as a two-token sequence in the `breach` production (Parser.hs:1706), `FOLLOWED BY` in the operator table (Parser.hs:1199), and `DIVIDED BY` (Parser.hs:1203). The new `BY <date>` deadline syntax appears in the `deadline` production, which is syntactically disjoint from all existing `BY` uses — `deadline` is parsed after `MUST/MAY/SHANT/DO action [PROVIDED guard]`, while `BREACH BY` starts with `TKBreach`, and `FOLLOWED BY`/`DIVIDED BY` are binary operators in expression context. The parser will try `WITHIN`, `BY`, and `EVENTUALLY` as alternatives in the `deadline` production, with `BY` consuming `TKBy` followed by an indented expression. No ambiguity arises. (Resolved 2026-02-25)

7. **Holiday data format**: Use L4 data declarations as the canonical format, with tooling for import from external sources. A calendar library (e.g., `sg-holidays.l4`) declares holidays as a `LIST OF DATE`:

    ```l4
    DECLARE `SG Public Holidays 2026` IS A LIST OF DATE
    `SG Public Holidays 2026` MEANS
      [ January 1 2026      -- New Year's Day
      , January 29 2026     -- Chinese New Year
      , January 30 2026     -- Chinese New Year
      ...
      ]
    ```

    This keeps holidays in the L4 type system (type-checked, version-controlled, documentable). External sources (iCalendar `.ics` files, CSV, government APIs) are consumed by **offline tooling** (a `jl4-calendar-import` CLI or build step) that generates `.l4` files, rather than by the runtime. This avoids adding external-format parsers to `jl4-core` and keeps the library layer self-contained. The generated `.l4` files are committed to version control so that builds are reproducible without network access. (Resolved 2026-02-25)

8. **Counting convention**: Configurable per calendar library, with a sensible default. The `business days` mixfix combinator accepts an optional `CountingConvention` parameter:

    ```l4
    DECLARE CountingConvention IS ONE OF
      `exclude trigger date`    -- T+1 convention (most common: UK, SG, US federal)
      `include trigger date`    -- T+0 convention (some civil law jurisdictions)
    ```

    The default is `exclude trigger date` (T+1), matching the most common convention in major jurisdictions (a deadline of "5 business days" starting Monday means the count begins Tuesday and ends the following Monday). Calendar libraries override this default when the jurisdiction requires T+0. Individual deontic constructs can override the convention explicitly if needed:

    ```l4
    WITHIN 5 `business days` `including trigger date`
    ```

    This pushes jurisdiction-specific policy into the library layer where it belongs, while keeping the evaluator convention-agnostic. (Resolved 2026-02-25)

9. **FOREVER representation**: Use the library-level sentinel `December 31 9999`, with evaluator awareness for short-circuiting. The sentinel is defined as a library constant in `daydate.l4`:

    ```l4
    DECLARE FOREVER IS A DATE
    FOREVER MEANS December 31 9999
    ```

    The evaluator *may* optimize by recognizing this specific value and skipping the deadline comparison (an optional optimization, not a semantic requirement). This is the pragmatic choice: (a) it keeps `FOREVER` in the library layer, not the compiler; (b) the year-9999 sentinel is a well-established convention (SQL `DATE '9999-12-31'`, ISO 8601 open-ended intervals); (c) no L4 contract will plausibly run in the year 9999; (d) a compiler-level special value would require changes to the type checker, evaluator, and serialization layers for marginal benefit. If a principled `+∞` representation is ever needed, it can be added as a `DATE` variant (`DATE IS ONE OF ... | FOREVER`) without changing the API surface. (Resolved 2026-02-25)

10. **`EVENTUALLY` + `after`**: Yes, support `EVENTUALLY AFTER <duration>` as parser-level sugar. This is a natural and common pattern (e.g., "the option may be exercised at any time after the vesting period"). The desugaring is straightforward:

    ```
    EVENTUALLY AFTER <dur>  →  DeadlineWindow { notBefore = TRIGGER_DATE + <dur>, notAfter = FOREVER }
    ```

    The parser grammar for `deadline` becomes:

    ```
    deadline ::= WITHIN <expr>
               | BY <expr>
               | EVENTUALLY [AFTER <expr>]
    ```

    Without `AFTER`, `EVENTUALLY` desugars as before (lower bound = `TRIGGER_DATE`, upper bound = `FOREVER`). With `AFTER`, the lower bound shifts. The `AFTER` keyword in this context is unambiguous because it only appears immediately after `EVENTUALLY` in the deadline production. This avoids the awkward `WITHIN FOREVER `after` 30 `days`` form that mixes sugar levels. The LSP's `EVENTUALLY` diagnostic still fires (warning or info depending on modal) — the `AFTER` clause doesn't suppress it, since the obligation is still unbounded. (Resolved 2026-02-25)
