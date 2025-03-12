# About Multi-Temporal (and Bi-Temporal) Database Modelling

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


Let's break out the temporal aspects of this decision logic. Let us say that every truth value is indexed to a point in time: in 1936 Edward VIII is King of UK; in 1937 he is no longer King but merely Duke of Windsor.

So our first-order logic equips every predicate with a timepoint: instead of

``` prolog
is_king(edward_viii).
```

We can only say

``` prolog
is_king(edward_viii, 1936).
```

giving

``` prolog
:- is_king(edward_viii, 1936).
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

So we'll log that as `T_ad42`.

That method of indexing temporals into predicates is a good start.

But a properly bitemporal database would allow us to go further:

Let's pretend that the very first version of the BNA did not allow qualified territories.

Maybe that came in subsequently, say, in 2002, retroactively granting citizenship to a whole raft of people who previously did not qualify.

The people didn't change; only their institutional constitution did.

So we want to be able to say, in 2001, prior to the commencement of the British Overseas Territories Act, someone born in Gibraltar to parents settled there, would not have qualified.

Was Eva a British citizen in 1960, as at 2001? No.

Was Eva a British citizen in 1960, as at 2005? Yes.

Now we need to distinguish valid time from system time from transaction time.

What if we were late to encode the legislation? We only got around to encoding the 2005 legislation in 2025.

Was Eva a British citizen in 1960, as at 2005, with system 2023? No.

Was Eva a British citizen in 1960, as at 2005, with system 2026? Yes.

So the "as at LT" indexes the legislative rules in effect at time LT.

The "in VT" indexes the valid time VT.

The "with system DT" refers to the decision time DT.

These operators `in`, `as at`, and `with system` can themselves nest, as understandings of the past, and of retroactive evaluations, are increasingly refined.

* The other examples from `examples_for_parsing`

## Tech stack

The intention behind structured-examples.yaml is that it should be relatively easy to, e.g., render this in a webpage with functionality for filtering or sorting the examples. (See, e.g., Simon Willison's suite of datasette tools.)

But of course, there is always a tradeoff between how making the data more structured for future use and making it easier for a human to input it.
