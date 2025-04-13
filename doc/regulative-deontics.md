# Deontics in Regulative Rules

Confused by the distinction (from the [Regulative Rules Spec](./regulative-spec.org) about the difference between **object-level
state transitions** and **property-level deontic assertions**?

You're not alone. Even legal drafters [ponder the multiple meanings of "May"](https://lawschoolpolicyreview.com/2025/02/05/issues-with-the-use-and-coding-of-may-in-commonwealth-legislation-a-legislative-drafters-perspective/), and wring their hands over how to clarify the differences in practice.

This document revisits the [regulative examples](./regulative-examples.md) to show the pragmatics of L4's approach to deontics.

## The Intuition

A set of regulative, or normative, rules is actually two things in
one.

On one level, the rules lay out a clockwork mechanism describing what
actions will lead to what consequences.

On a mechanical watch: a mainspring drives a gear connected to an
escapement connected to an oscillator which governs a wheel train
which turns the hands.

In a financial loan: a lender disburses principal to a borrower. Every
month, so long as the loan is outstanding, if the borrower makes
payment on time, great, see you next installment! If they don't pay on
time, extra interest begins to accrue, plus a penalty fee. Pay off the
penalty and the interest, and things are back on track: we'll pretend
the late payment had never happened.

On another level, the set of rules can be described from a perspective
standing outside the basic rules themselves.

1. If the watch isn't wound, after a week it will not keep correct time.

2. Twice a year, Daylight Savings requires a manual reset.

3. Even under ideal conditions, the clock may drift by up to a minute
a day.

None of these ideas are built into the metal of the clock itself: they
are statements *about* the clock.

1. If payments are not made for six months in a row, the penalties
could begin to exceed the regular payment amount.

2. This contract needs to be amended or redrafted because it
references LIBOR, which is deprecated in favour of SOFR.

These are assertions *about* the contract

### Background: Labeled Transition Systems

### Background: LTL and CTL Temporal Logic Assertions

### Background: Answering Queries by way of Model Checking



### In L4, Deontics Are Squeezed Out from the Object Level

### They End Up Rising to the Property Level

### They Are Reintroduced Purely as Syntactic Sugar

### So what's the benefit of the syntactic sugar?

If a MUST isn't always a MUST, and can be sometimes considered a MAY...

If a MAY isn't always a MAY, but can be sometimes considered a MUST...

What's the point of even allowing the MUST and MAY?

Well, at compile-time, we can statically analyze that every MUST is associated with some eventual penalty in case of breach.

## Borrowing a book from the library

## Picking up kids from school

## Flood & Goodenough loan agreement

## Fighting International Crime

## Other Legal Idioms, reinterpreted into L4



