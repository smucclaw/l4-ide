# Specification: Deontic Deadline Elaboration

**Status:** Draft
**Date:** 2026-02-24
**Author:** Meng Wong
**Branch:** `mengwong/deadlines`
**Related:** `TEMPORAL-MASTER.md`, `TEMPORAL_EVAL_SPEC.md`, `TEMPORAL_PRELUDE_MACROS.md`, `BOUNDED-DEONTICS-SPEC.md`, `UPON-EXTERNAL-EVENTS-SPEC.md`

## Executive Summary

The `WITHIN` keyword in L4's deontic construct (`PARTY/MUST/HENCE/LEST`) currently accepts only a `NUMBER`, hand-waved as "some number of days." Real-world contracts and regulations require richer temporal deadlines: absolute dates, relative durations with explicit units, business-day calculations, and jurisdictional calendars.

This spec proposes a minimal compiler change (three touches) that pushes all temporal richness into the L4 library/mixfix layer, so that jurisdiction-specific deadline vocabulary can be extended by application and library developers without recompiling jl4.

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

The current implementation has a single temporal slot in the AST (`due :: Maybe (Expr n)`) typed as `number`. The parser matches only the `WITHIN` keyword. The type checker constrains the expression to `number`. Users who need richer deadlines must manually reduce everything to a day-count, often with awkward `Date to days` conversions.

## Current State

### AST (`Syntax.hs`)

```haskell
data Deonton n = MkDeonton
  { anno :: Anno
  , party :: Expr n
  , action :: RAction n
  , due :: Maybe (Expr n)    -- the only temporal slot
  , hence :: Maybe (Expr n)
  , lest :: Maybe (Expr n)
  }
```

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

**A deadline is a DATE.** Whether expressed as an absolute date, a relative duration, or a complex combination, the end result is always a specific `DATE` by which the obligated party must act. Everything reduces to computing a `DATE`.

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

## Compiler Changes (Minimal)

Three changes to Haskell source, totalling roughly 20 lines:

### 1. Change `due` type from `number` to `date`

In `TypeCheck.hs`:

```haskell
-- Before:
dueR <- traverse (\e -> checkExpr ExpectRegulativeDeadlineContext e number) due
-- After:
dueR <- traverse (\e -> checkExpr ExpectRegulativeDeadlineContext e date) due
```

With a fallback: if the expression has type `number`, auto-coerce to `TRIGGER_DATE PLUS n` and emit a deprecation warning.

### 2. Add `BY` as Alternative Keyword in Deadline Slot

In `Parser.hs`, alongside the existing `WITHIN`:

```haskell
deadline current =
      annoLexeme (spacedKeyword_ TKWithin) *> annoHole (indentedExpr current)
  <|> annoLexeme (spacedKeyword_ TKBy)     *> annoHole (indentedExpr current)
```

Add `TKBy` to the lexer's keyword table. (Note: `TKBy` may already exist for `BREACH BY` -- if so, context-sensitive disambiguation is needed, or a different token like `TKByDate`.)

### 3. Add `TRIGGER_DATE` Builtin

In `TypeCheck/Environment.hs`, add `TRIGGER_DATE` as a builtin of type `DATE`. In the evaluator, it reads the timestamp of the event that activated the current obligation from the trace. This aligns with Hvitved's trace-based contract semantics: each event in the trace is timestamped, and when a deontic obligation fires (via `HENCE` or `UPON`), the triggering event's timestamp becomes `TRIGGER_DATE`.

Implementation options:

- **Via TemporalContext**: Add a `tcTriggerDate :: Maybe Day` field, set by the deontic activation logic before evaluating the deadline expression.
- **Via trace inspection**: The evaluator already walks the event trace for residuation; `TRIGGER_DATE` could read the current event's timestamp directly from the trace cursor.

The latter is more principled but depends on the trace machinery being available during deadline evaluation.

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

-- Backward compat: bare number still works (with deprecation warning)
PARTY Tenant
MUST `vacate premises`
WITHIN 30
```

## Grammar (Informative)

At the parser level, the grammar is deliberately thin:

```
deadline  ::=  WITHIN  <expr>    -- expr must have type DATE (or NUMBER with auto-coercion)
            |  BY      <expr>    -- expr must have type DATE
```

All further structure -- duration units, anchoring, calendar selection -- is resolved by the mixfix layer from library definitions. The parser sees only `WITHIN <expr>` or `BY <expr>`.

## Interaction with Existing Temporal Specs

This spec is **orthogonal to** but **compatible with** the multi-temporal EVAL system described in `TEMPORAL_EVAL_SPEC.md` and `TEMPORAL_PRELUDE_MACROS.md`:

- `TRIGGER_DATE` reads from `TemporalContext`, so `EVAL AS OF SYSTEM TIME ...` can override it for counterfactual deadline reasoning.
- Duration library functions operate on `DATE` values, which the temporal EVAL system can manipulate.
- The `temporal-prelude.l4` macros (when implemented) can compose with deadline expressions.

## Layer Responsibility Summary

| Layer                                        | Responsibility                                                                                                          |
| -------------------------------------------- | ----------------------------------------------------------------------------------------------------------------------- |
| **Compiler**                                 | `BY` keyword, `due : DATE`, `TRIGGER_DATE` builtin, NUMBER auto-coercion                                                |
| **Core library** (daydate.l4 / durations.l4) | `days`, `weeks`, `months`, `years`, `business days`, `of`, `under`, `nth business day after`, `add months`, `add years` |
| **Jurisdiction libraries**                   | Calendar predicates, holiday lists, weekend definitions                                                                 |
| **Domain libraries**                         | ISDA conventions, insurance calendars, government fiscal calendars                                                      |

## Implementation Roadmap

### Phase 1: Compiler Foundation

- [ ] Add `TKBy` to lexer (or disambiguate existing `TKBy`)
- [ ] Extend `deadline` parser to accept `BY` alternative
- [ ] Change `due` type constraint from `number` to `date`
- [ ] Add NUMBER-to-DATE auto-coercion with deprecation warning
- [ ] Add `TRIGGER_DATE` builtin (type `DATE`, reads triggering event timestamp from trace)
- [ ] Wire deontic obligation activation to expose event timestamp during deadline evaluation

### Phase 2: Core Duration Library

- [ ] Implement `add months` and `add years` (calendar-aware date arithmetic)
- [ ] Implement duration mixfix forms: `days`, `weeks`, `months`, `years` (default-anchored)
- [ ] Implement explicit-anchor forms: `days of`, `weeks of`, `months of`, `years of`
- [ ] Implement `nth business day after` (higher-order, calendar-parameterized)
- [ ] Implement `business days` / `business days under` / `business days under ... of` mixfix forms
- [ ] Add `default calendar` binding (configurable per module)

### Phase 3: Jurisdictional Libraries

- [ ] Singapore business calendar (`sg-calendar.l4`)
- [ ] UK banking calendar
- [ ] US federal calendar
- [ ] ISDA business day conventions
- [ ] Module-level `USING` convention for default calendar selection

### Phase 4: Testing and Migration

- [ ] Golden tests for `BY` and `WITHIN` with DATE expressions
- [ ] Golden tests for duration mixfix resolution
- [ ] Golden tests for business day computation
- [ ] Migration guide for existing `WITHIN <number>` usage
- [ ] Update documentation in `doc/reference/` and `doc/courses/`

## Open Questions

1. **`TRIGGER_DATE` naming and provenance**: Should it be `TRIGGER_DATE`, `THE TRIGGER DATE`, `OBLIGATION DATE`, or something else? Must be unambiguous and readable in context. Additionally, should it be surfaced as a standalone builtin, or as a field accessor on the current event in the trace (e.g., `THE EVENT DATE`)?

2. **Module-level default calendar**: Should there be a `USING` keyword, or should the default calendar be set via an import convention (e.g., `IMPORT sg-calendar` sets the default)?

3. **`TKBy` token**: The lexer may already have `TKBy` for `BREACH BY party`. If so, does the parser context disambiguate, or do we need a distinct token?

4. **Holiday data format**: Should holiday lists be L4 data declarations, or imported from external sources (YAML, CSV, iCalendar)?

5. **Counting convention**: "Within 5 business days" -- does the count start on the trigger date or the next business day? Different jurisdictions have different conventions. This should probably be configurable per calendar library.
