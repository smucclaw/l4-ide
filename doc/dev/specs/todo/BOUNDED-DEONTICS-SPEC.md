# Bounded Deontics Specification

**Status:** Planning
**Author:** Meng Wong
**Date:** 2026-01-26
**Related:** [regulative.md](../../../regulative.md), [module-a11-regulative-rules.md](../../../advanced-course-ai/module-a11-regulative-rules.md)

## Overview

This specification describes L4's approach to deontic modality ("must", "may", "must not") and its relationship to formal verification via model checking. The key insight is that **deontic force is always bounded by context** — what looks like an absolute obligation is always relative to some goal or consequence.

This design enables:
1. Compositionality of contracts (à la Hvitved's CSL)
2. Model checking at the LTL/CTL assertion level
3. Natural expression of both constitutive and regulative rules
4. Disambiguation of legal ambiguity through formal verification

## Philosophical Background

### Searle's Constitutive vs Regulative Rules

John Searle distinguished two kinds of rules:

| Type | Form | Example | Non-compliance |
|------|------|---------|----------------|
| **Constitutive** | "X counts as Y in context C" | "All directors must vote yes for resolution to pass" | Institutional fact doesn't come into being |
| **Regulative** | "If C, do X" | "Directors must vote within 7 days" | Breach, penalty, liability |

Constitutive rules **define** institutional reality. The "must" in "all directors must vote yes for it to pass" is really a "must be" — a necessary condition — not an obligation. There's no breach if directors vote no; the resolution simply doesn't pass.

Regulative rules **prescribe** behaviour. The "must" in "directors must vote within 7 days" creates an obligation that can be fulfilled or breached.

### Kant's Hypothetical Imperatives

Kant distinguished:

| Type | Example | Binding force |
|------|---------|---------------|
| **Categorical** | "You must not lie" | Unconditional, applies to all rational agents |
| **Hypothetical** | "If you want wine, you must call the waiter" | Conditional on your goals |

Hypothetical imperatives are goal-directed. Non-compliance isn't breach — you simply don't achieve your goal. The patron who doesn't raise their hand isn't violating anything; they just don't get wine.

### Gneezy & Rustichini: A Fine Is A Price

In their classic paper ["A Fine Is A Price"](https://www.jstor.org/stable/10.1086/468061), Gneezy and Rustichini showed that penalties have multiple dimensions:
- Economic/financial/monetary
- Moral/social

When a "must" has only a small financial penalty, rational actors may treat it as a pricing structure rather than a genuine obligation. Consider:

> "Businesses must file within 30 days."
>
> Inquiring lawyer: "What's the penalty for late filing?"
>
> Regulator: "The filing fee increases from $30 to $35."

This "must" dissolves under scrutiny — it's really just a menu of options with associated costs. The business can rationally choose the "late" path if $5 is worth the convenience.

## The Bounded Deontics Framework

### Core Insight

Every deontic statement is **bounded** by its consequences. The force of a "must" depends on what's at stake:

| Apparent rule | Actual structure | Real "must"? |
|---------------|------------------|--------------|
| "Must file within 30 days or $5 penalty" | Pricing / menu of options | No — just pay the premium |
| "Must file within 30 days or license suspended" | Real consequence with teeth | Probably, for most actors |
| "Must file within 30 days or criminal prosecution" | Serious enforcement | Yes, for risk-averse actors |
| "Must get all director votes for resolution to pass" | Constitutive — definitional | N/A — it's a necessary condition |

### The Primitive: DO

L4's primitive form is neutral about deontic valence:

```l4
PARTY p DO action
  HENCE outcome_if_done
  LEST outcome_if_not_done
```

This simply describes a choice point with two branches. No moral weight is assigned.

### Deontic Sugar

The familiar deontic operators are syntactic sugar over `DO` with conventional default outcomes:

| Operator | HENCE triggers when... | Default HENCE | LEST triggers when... | Default LEST |
|----------|------------------------|---------------|----------------------|--------------|
| `DO` | action is taken | (required) | deadline passes | (required) |
| `MUST` | action is taken | `FULFILLED` | deadline passes | `BREACH` |
| `MAY` | action is taken | `FULFILLED` | deadline passes | `FULFILLED` |
| `SHANT` | deadline passes | `FULFILLED` | action is taken | `BREACH` |

The key insight: **"breach" in an inner contract isn't inherently bad** — it just means "this path wasn't taken." The valence (good/bad) comes from the enclosing context.

### Compositional Structure (à la Hvitved's CSL)

Contracts compose like Unix processes with exit codes. Each subcontract terminates with a status that the outer contract can inspect:

```l4
-- Inner contract: patron's choice
inner MEANS
  PARTY patron MUST raise_hand AND ask_for_wine
    HENCE fulfilled    -- exit 0
    LEST not_pursued   -- exit 1 (not a moral failing!)

-- Outer contract: conditional on inner
outer MEANS
  inner
    HENCE              -- if inner succeeds
      PARTY waiter MUST bring_wine
        HENCE satisfied
        LEST patron_goes_thirsty
    LEST               -- if inner "fails" (patron chose not to pursue)
      nothing_happens  -- waiter has no obligation
```

The "breach" of the inner contract (patron doesn't raise hand) carries no moral weight — it's simply "patron chose not to pursue wine." The waiter incurs no obligation.

### Why This Matters

This design allows **constitutive rules to be expressed as regulative rules** when that fits human intuition. The sentence "all directors must vote yes for it to pass" *sounds* regulative (it uses "must"), even though it's constitutive. L4 permits this phrasing because:

1. At the object level, we write the mechanics: choice points and branches
2. At the assertion level, model checking reveals the actual semantics

## Two-Level Architecture

### Object Level: Contract Specification

The object level describes **what the choices are** and **what follows from each choice**. It's mechanical, non-judgmental:

```l4
-- Object level: just the mechanics
§ library_rules

UPON borrow_book
  PARTY patron DO return_book WITHIN 14 days
    HENCE no_fine
    LEST  fine_applies

UPON fine_applies
  PARTY patron DO pay_fine
    HENCE borrowing_restored
    LEST  borrowing_suspended
```

This is descriptive, not prescriptive. It says "if you don't return the book, a fine applies" — but doesn't moralize about it.

### Assertion Level: Property Specification (LTL/CTL)

The assertion level expresses **properties about paths through the system**:

```
-- LTL assertion: "you must return the book to avoid a fine"
AG(borrowed_book → ¬E[¬returned_book U ¬fine_applies])

-- English: "It's always the case that if you borrowed a book,
--           there's no path to avoiding a fine that doesn't
--           go through returning the book"
```

This is where "must" acquires its force — as path necessity. The model checker can:
1. Verify that the object-level contract satisfies the assertion
2. Discover implicit "musts" (actions that are necessary for any goal-achieving path)
3. Find counterexamples (loopholes, race conditions, impossible requirements)

### Disambiguation Through Model Checking

When natural language is ambiguous between constitutive and regulative readings, the two-level architecture disambiguates:

| Natural language | Object level | Assertion level reveals |
|------------------|--------------|------------------------|
| "Directors must vote yes for it to pass" | `IF all_yes THEN passed ELSE not_passed` | Constitutive: `passed` is unreachable without `all_yes` |
| "Directors must vote within 7 days" | `DO vote WITHIN 7 days HENCE ok LEST breach` | Regulative: `breach` is reachable if deadline missed |
| "You must raise your hand to get wine" | `DO raise_hand HENCE waiter_obligated LEST nothing` | Hypothetical: `wine` unreachable without `raise_hand`, but `nothing` is also acceptable |

## Implementation Roadmap

### Phase 1: Object-Level Deontic Syntax

- [ ] Implement `DO` as primitive with required `HENCE`/`LEST`
- [ ] Implement `MUST`/`MAY`/`SHANT` as sugar with default outcomes
- [ ] Support `FULFILLED` and `BREACH` as terminal states
- [ ] Support contract composition via `HENCE`/`LEST` wiring

### Phase 2: Transpilation to Verification Backends

- [ ] Transpile to UPPAAL (timed automata, CTL)
- [ ] Transpile to SPIN/Promela (LTL model checking)
- [ ] Transpile to NuSMV (symbolic model checking)
- [ ] Transpile to Maude (rewriting logic)

### Phase 3: Assertion Language

- [ ] Design L4 syntax for LTL/CTL property assertions
- [ ] Integrate model checker results into IDE (counterexample visualization)
- [ ] Support "loophole detection" as security exploit search

### Phase 4: Advanced Features

- [ ] Deontic reasoning with multiple agents (game-theoretic analysis)
- [ ] Penalty weighting and utility functions
- [ ] Integration with decision service for runtime state tracking

## Examples

### Example 1: The Wine and Waiter (Hypothetical Imperative)

Natural language: "If you want wine, you must raise your hand and call the waiter."

```l4
-- This is NOT a true obligation — it's goal-directed advice
inner MEANS
  PARTY patron DO raise_hand AND call_waiter
    HENCE waiter_attends
    LEST  no_service        -- acceptable outcome, not breach

outer MEANS
  inner
    HENCE
      PARTY waiter MUST bring_wine
        HENCE patron_satisfied
        LEST  patron_complains
    LEST
      FULFILLED  -- patron simply chose not to pursue wine
```

Model checking reveals: "If goal is `patron_satisfied`, then `raise_hand` is necessary" — but this necessity is bounded by the goal.

### Example 2: Director Voting (Constitutive Rule)

Natural language: "All directors must vote in favour for the resolution to pass."

```l4
DECIDE resolution_passed MEANS
  all_directors_voted_yes

-- Or, phrased regulatively (same semantics):
PARTY board DO achieve_unanimous_yes
  HENCE resolution_passed
  LEST  resolution_failed
```

Model checking reveals: This is constitutive — there's no "breach", just a definitional relationship. The `LEST` branch is "resolution doesn't pass", not a penalty.

### Example 3: Filing Deadline (Regulative with Teeth)

Natural language: "Businesses must file within 30 days or face license suspension."

```l4
UPON triggering_event
  PARTY business MUST file WITHIN 30 days
    HENCE compliant
    LEST
      PARTY regulator MAY suspend_license
        HENCE license_suspended
        LEST  warning_issued
```

Model checking can verify: "Is there any path to continued operation that doesn't go through timely filing?" If `suspend_license` always follows, the answer is no — this is a genuine "must."

### Example 4: Filing Deadline (Regulative without Teeth)

Natural language: "Businesses must file within 30 days."

```l4
UPON triggering_event
  PARTY business DO file
    WITHIN 30 days
      HENCE fee = $30
      LEST  fee = $35  -- that's it, just $5 more
```

Model checking reveals: This isn't really a "must" — it's a pricing structure. Both paths lead to acceptable outcomes; one is just slightly more expensive.

## Theoretical Connections

### Hvitved's CSL

L4's deontic composition is inspired by Hvitved's Contract Specification Language, where every deontic subcontract terminates in `fulfilled` or `breach`, and these can be wired into larger contracts.

### Process Algebra (CSP, CCS)

The compositional structure resembles communicating sequential processes, with `HENCE`/`LEST` acting as synchronization points.

### Temporal Logic (LTL, CTL)

The assertion level uses temporal operators:
- **A** (all paths), **E** (exists path)
- **G** (globally), **F** (finally), **X** (next), **U** (until)

Path necessity ("you must do X to achieve Y") emerges from quantification over paths.

### Deontic Logic

Traditional deontic logic (O for obligation, P for permission, F for prohibition) maps to:
- O(action) ≈ `MUST action` with negative `LEST`
- P(action) ≈ `MAY action` (both branches acceptable)
- F(action) ≈ `SHANT action` with negative `LEST`

But L4 makes the consequences explicit rather than relying on abstract modal semantics.

## References

1. Searle, J. (1969). *Speech Acts*. Cambridge University Press.
2. Searle, J. (1995). *The Construction of Social Reality*. Free Press.
3. Hvitved, T. (2012). *Contract Formalisation and Modular Implementation of Domain-Specific Languages*. PhD thesis, University of Copenhagen.
4. Gneezy, U. & Rustichini, A. (2000). "A Fine Is A Price." *Journal of Legal Studies*, 29(1).
5. Kant, I. (1785). *Groundwork of the Metaphysics of Morals*.
6. Hoare, C.A.R. (1978). "Communicating Sequential Processes." *Communications of the ACM*, 21(8).
7. Clarke, E., Grumberg, O., & Peled, D. (1999). *Model Checking*. MIT Press.

## Appendix: The Unix Analogy

Just as Unix processes return exit codes (0 for success, non-zero for various failure modes), L4 subcontracts return outcomes:

```
grep "pattern" file.txt
echo $?  # 0 if found, 1 if not found, 2 if error
```

`grep` returning 1 (pattern not found) isn't an *error* — it's just stating a fact. Similarly, an inner contract reaching its `LEST` branch isn't necessarily *breach* — it might just be "this path wasn't taken."

The outer script decides what to do with that exit code:

```bash
if grep -q "pattern" file.txt; then
  echo "Found it, proceeding..."
else
  echo "Not found, but that's okay"
fi
```

L4's compositional deontics work the same way: the inner contract produces an outcome, and the outer contract decides what that outcome *means*.
