# README

## Examples

Dec 18 2024: I've focused on examples that seemed trickier to me, though the examples I've highlighted also don't seem high-priority.

Examples that don't seem tricky but that would be worth using as examples in our cookbook / docs include, e.g.,:

### Matt Waddington's rewrite of part of the British Nationality Act (<https://osf.io/mt78r>):

```text
1(1) A person is a British citizen if –
  (a) the person is born –
      (i) in the United Kingdom after commencement, or
      (ii) in a qualifying territory on or after the appointed day; and
  (b) when the person is born, the person’s father or mother is –
      (i) a British citizen;
      (ii) settled in the United Kingdom; or
      (ii) settled in the qualifying territory in which the person is born.
```

#### Everything in good time

Let's break out the temporal aspects of this decision logic. Let us say that every truth value is indexed to a point in time: in 1936 Edward VIII is King of UK; in 1937 he is no longer King but merely Duke of Windsor.

So our first-order logic equips every predicate with a timepoint: instead of

``` prolog
is_king(edward_viii).
```

We can only say

``` prolog
is_king(edward_viii, 1936).
```

This is enough for us to start thinking with temporals:

1. A person (`Person`) is a British citizen (`AS OF Time_Of_Birth`) if:
   1. the person was born
      1. in the UK (`born_in(P, uk)`) after commencement of this act (`Time_Of_Birth > T_Act_Commencement`)
      2. in a qualifying territory (`born_in(P, QT), qualifying_territory(QT)`) on or after the appointed day (`Time_Of_Birth >= T_BCIA_Commencement`)
   and
   2. `AS AT Time_Of_Birth`, some parent (`parentOf(Parent, P)`)
      1. was a British citizen (`is_British(Parent)`); or
      2. was settled in the UK (`is_settled_in(Parent, uk)`); or
      3. *(1.i.b and)* was settled in a qualifying territory in which the person is born (`is_settled(QT, Parent)`).

The Prolog bits are in [bna.pl](bna.pl).

There is much more to the act; we know (from world knowledge) that a person can stop being a British citizen if, for example, they renounce their citizenship and go off to marry an American divorcee. So we know that the fragment above confers British citizenship at a point in time, but that state can change subsequently; it is not permanent.

Regarding 1.b.i, we are willing to accept _prima facie_ evidence, without having to derive the parent's own citizenship from first principles, particularly since we don't have, in scope, a way to decide citizenship before the time this act commenced (T_act) -- this is a bit like the soteriological problem of the fate of the unevangelized.

What is an appointed day? Farther down in the legislation it says: _The relevant day for the purposes of subsection (1A) or (3A) is the day appointed for the commencement of section 42 of the Borders, Citizenship and Immigration Act 2009 (which inserted those subsections)._

So we'll log that as `T_bcia`.

The BNA material deserves a tutorial in its own right, but we won't get into that here. Here we want to focus on the temporals, affording a concise vocabulary for saying that something was true at a certain time in the past; due to a particular version of the legislation that was in effect at a certain time; or due to imperfect knowledge about events as they reported in; or due to encoding errors.

In the above treatment, our method of indexing temporals into predicates is a good start. There are other methods, which employ more sophisticed logics like the event calculus; see also [TempLog](https://core.ac.uk/outputs/202985978/). In this case we will restrict our model of time and events to say that if a thing was true at time T it continues to be true at subsequent times; and if we need to override that thing we just introduce an overriding predicate. So we would say that someone is a British citizen by birth forever, but they are no longer a British citizen since the date of renunciation of citizenship, which also goes on forever. So you can think of it as layering on new events, and at any point in time we can take a core sample of the layers and use the top one.

A properly bitemporal, or multitemporal, database would allow us to go further. We have to deal with the temporality of events as they are reported into the system: the classic example deals with events that change someone's state, and with the practical problems with the fact that those events are imperfectly recorded to the database, with some delays. Due to these practical issues, decisions made with imperfect information may be inaccurate; but for audit purposes we need to record those decisions as having been made at a certain time in a certain way, so that we do not later question our own sanity; instead we can account discrepancies to simply not knowing all the facts at the time. So we achieve a bitemporal model and [tritemporal history](https://en.wikipedia.org/wiki/Temporal_database#Using_three_axes:_valid_time,_decision_time,_and_transaction_time).

How do we apply this thinking to law? Legislation and regulations are subject to *versioning*: changing rulesets are indexed by commencement date, much as changing repositories are indexed by commit. So we need to be multitemporal with respect to events in the world, and we need to be multitemporal with respect to the rules which govern those events, and we need to allow the rules to also be sensitive to the history of previous applications of rules. In recent news the Australian Robodebt matter, and in the UK the Windrush scandal, have been the settings for logics which say, "if a mistake was made through incorrect application of the rules that were in effect at that time, that mistake is now to be corrected by offering remedy to the concerned parties."

Let's pretend that the very first version of the BNA did not allow qualified territories.

Maybe that came in subsequently, say, in 2002, retroactively granting citizenship to a whole raft of people who previously did not qualify.

The people didn't change; only their institutional constitution did.

So we want to be able to say, in 2001, prior to the commencement of the British Overseas Territories Act, someone born in Gibraltar to parents settled there, would not have qualified.

Was Eva a British citizen in 1960, according to rules in effect in 2001? No.

Was Eva a British citizen in 1960, according to rules in effect in 2005? Yes.

What if we were late to encode the legislation? Maybe only got around to encoding the 2005 legislation in 2024. So the decision history shows inaccuracy due to system latency.

Was Eva a British citizen in 1960, according to rules in effect in 2005, under the encoding in use by the system in 2023? No.

Was Eva a British citizen in 1960, according to rules in effect in  2005, under the encoding in use by the system in 2025? No.

Meng proposes the following system of operators to handle the above complexity.

Valid time refers to our record of an event in the real world.

Transaction time allows us to update those records, while reflecting past misunderstandings: see [the example from Wikipedia.](https://en.wikipedia.org/wiki/Temporal_database#Using_two_axes:_valid_time_and_transaction_time)

However, there are two categories of events in the real world: the physical events, and the constitutive interpretations of those events which are given by rulesets which are themselves subject to change.

So we now need to have a similar notion of valid and transaction time *but applying to legislation*.

I propose to use:
- *version time* to represent valid time for rules
- *encoding time* to represent transaction time for rules. If an encoding was in error, we need to fix it, without necessarily implying that the underlying version changed; only our encoding of it did.

So we can ask questions like

- In 1984, what did the system know about Alice's date and place of birth?
  - Query: transaction time = 1984, person = alice, event = birth
  - Response: valid time = 1970, place = UK

- In 2025, what did the system know about the nationality act in effect in 1988? (version time = 1988, encoding time = 2025)
  
  - Query: transaction time = 2025, act = BNA, valid = 1988
  - Response: BNA_1988

- The 2025 encoding of the 1988 version of the nationality act, would then take as an input the valid-time data about Alice, and return an answer about her citizenship in 1988, due to events of 1984.

- A subsequent act, say the 2020 Windrush Compensation Scheme (Expenditure) Act, might take as a further input the history of whether the rules (of a certain version-time) were applied in a certain way due to incorrect valid-time records; and if they were, then other rules kick in.

Composing these ideas, 

"AS AT T" means "taking the version of legislation in effect at time T, with the information known to the system at time T"

the absence of "AS AT" is shorthand for "taking the current version of legislation, using the latest information known to the system"

These operators `in`, `as at`, and `with system` can themselves nest, as understandings of the past, and of retroactive evaluations, are increasingly refined.

* The other examples from `examples_for_parsing`

## Tech stack

The intention behind structured-examples.yaml is that it should be relatively easy to, e.g., render this in a webpage with functionality for filtering or sorting the examples. (See, e.g., Simon Willison's suite of datasette tools.)

But of course, there is always a tradeoff between how making the data more structured for future use and making it easier for a human to input it.
