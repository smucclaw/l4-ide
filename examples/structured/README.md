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

#### An Introduction to Temporal Logic in Legal Settings

Let's examine the temporal aspects of this decision logic. To start, we'll use a simple temporal predicate logic. Let us say that every truth value is indexed to a point in time: in 1936 Edward VIII is King of UK; in 1937 he is no longer King but merely Duke of Windsor.

So our first-order logic equips every predicate with a timepoint: instead of

``` prolog
is_king(edward_viii).
```

We add a date to the end. Now we can say

``` prolog
is_king(edward_viii, 1936).
```

You can verbalize this as "In 1936, Edward the Eighth is king". Maybe in future we will flip this convention around so the date appears at the front.

Further down in this document you will see a predicate `is_British(Person, Time_Of_Birth)` which you can verbalize as "at their time of birth, a person is a British citizen if ..."

It really should be `is_British_citizen` but we leave out the `_citizen` part for concision.

This convention is enough for us to start thinking with temporals.

Let's set up some key dates from the world, like:

| `Time_Of_Birth`       | the date of birth of the person in question                                   |
| `T_Act_Commencement`  | the date of commencement of the British Nationality Act                       |
| `T_BCIA_Commencement` | the date of commencement of the Borders, Citizenship and Immigration Act 2009 |

A person (`Person`) is a British citizen (`is_British(Person, Time_Of_Birth`) if:
1. the person was born
   1. in the UK (`born_in(Person, uk, Time_Of_Birth)`) after commencement of this act (`Time_Of_Birth > T_Act_Commencement`)
   2. in a qualifying territory (`born_in(P, QT, Time_Of_Birth), qualifying_territory(QT)`) on or after the appointed day (`Time_Of_Birth >= T_BCIA_Commencement`)
AND
2. some parent (`parentOf(Parent, P, Time_Of_Birth)`)
   1. was a British citizen (`is_British(Parent, Time_Of_Birth)`); or
   2. was settled in the UK (`is_settled_in(Parent, uk, Time_Of_Birth)`); or
   3. *(1.2 and)* was settled in a qualifying territory in which the person is born (`is_settled(QT, Parent, Time_Of_Birth)`).

Note that we liberally scatter `Time_Of_Birth` at the end of each predicate, because we are examining these facts at a particular point in time.

A Prolog treatment is attempted in [bna.pl](bna.pl). 

Of course, there is much more to the act than just this first paragraph. We know (from world knowledge) that a person can stop being a British citizen if, for example, they renounce their citizenship and go off to marry an American divorcee. So we know that the fragment above confers British citizenship at a point in time, but that state can change subsequently; it is not permanent. For our purposes though, this example is limited enough that we can assume that once a thing is true it continues to be true. Once a parent, always a parent. Once a citizen, always a citizen. There are fuller treatments which employ more sophisticated logics like the event calculus; see also [TempLog](https://core.ac.uk/outputs/202985978/). In this example we will restrict our model of time and events to say that if a thing was true at time T it continues to be true at subsequent times; and if we need to override that thing we just introduce an overriding predicate. So we would say that someone is a British citizen by birth forever, but they are no longer a British citizen since the date of renunciation of citizenship, which also goes on forever. So you can think of it as layering on new events, and at any point in time we can take a core sample of the layers and use the top one.

Regarding 2.1, we are willing to accept _prima facie_ evidence, without having to derive the parent's own citizenship from first principles, particularly since we don't have, in scope, a way to decide citizenship before the time this act commenced (T_act) -- this is a bit like the soteriological problem of the fate of the unevangelized.

What is an appointed day? Farther down in the legislation it says: _The relevant day for the purposes of subsection (1A) or (3A) is the day appointed for the commencement of section 42 of the Borders, Citizenship and Immigration Act 2009 (which inserted those subsections)._

That's why we track `T_BCIA_Commencement`.

The predicate logic of the BNA material deserves a tutorial in its own right, but we won't get into that here.

##### Time Axes

Let's develop a concise vocabulary that allows us to deal with different aspects of time:

- some event occurred in the real world at a certain time;
- our system possessed imperfect knowledge about events as they reported in;
- a particular version of legislation was passed at a certain time, turning from a Bill into an Act;
- a particular version of legislation/regulation commenced at a certain time, taking effect on a certain date;
- a particular provision is aware of time and makes decisions differently based on when events occurred; this is the notion of commencement, just at a finer granularity within an Act;
- a certain piece of legislation was encoded in a certain way at a certain time, and a different way at a different time;
- a judge decided that even though a certain decision should have been, or was made a certain way, they overrode the decision and applied that override retroactively.

Composing these notions, we want to be able to say:
- a piece of legislation that was in effect at time T1
- specifies a certain decision that refers to how some other piece of legislation that was in effect at time T2 would have made a decision about
- some event that occurred, or some state of affairs that held true, at some time T3

In the future we might add a decision axis:
- the above logics led to a decision at time T4
- which might later be overridden at time T5
- with an instruction to take effect retroactively dating back to time T6

We also want to handle support for audit:
- a decision was made at a certain time in accordance with various grounds that were relevant at that time (or, perhaps, in violation of those grounds, which is why we need to record these decisions -- for audit purposes -- in the first place).
- And if we want to reconstruct that decision we need to be able to "rewind the clock" to that time and revisit those grounds as they stood at that time.

Here's a concrete example:

1. A person (“P”) is entitled to be registered as a British citizen on an application made under this section if—
   1. P meets the general conditions; and
   2. at any time in the period after commencement, P would have automatically become a British citizen at birth by the operation of any provision of this Act or the British Nationality (Falkland Islands) Act 1983, had P's mother been married to P's natural father at the time of P's birth.

[British Nationality Act 1981, s.4G(1)](https://www.legislation.gov.uk/ukpga/1981/61/section/4G) \[as inserted by the Immigration Act 2014, s.65 and amended by the Nationality and Borders Act 2022, s.7(3)\]

There's subtle logic here. Subsection 6 says:
6. The reference in this section to the period after commencement does not include the time of commencement (and, accordingly, this section does not apply to any case in which a person was unable to become a British citizen at commencement).

So we need to distinguish **at** a particular time, *vs* **after** a particular time.

The above example also deals with counterfactuals -- which we will not deal with at the moment, other than to say that the semantics of the Reader Monad and the `local` function are sufficient to represent simple counterfactuals.

[Section 4L](https://www.legislation.gov.uk/ukpga/1981/61/section/4L) provides a recent politically charged example of a counterfactual which requires more sophisticated metaprogramming, and leads to the idea of homoiconicity.

##### Temporal Databases

To handle the above temporal challenges, we fortunately don't need to develop theory from scratch. The existing theory of *multi-temporal databases* provides a language for us to talk about *valid time*, *system (or transactional) time*, and *decision time*. See the examples from https://en.wikipedia.org/wiki/Temporal_database, then come back.

How do we apply this thinking to law? By analogy to software, legislation and regulations are subject to *versioning*: Acts are indexed by the date they pass (typically recorded in the title), much as changing Git repositories are indexed by authoring and commit date. From the perspective of a legal drafter, that date of passage is a *transaction time* (also called *system time*). We can use the same notion to record amendments to Acts.

Acts also contain *commencement* dates: when they take effect in the real world. Borrowing terminology from temporal databases, let's call that a *rule valid time*. Here, the code itself contains date logic: `if (current_date() > commencement_date + 6 months) { penalties increase }`

Meng proposes the following vocabulary and system of operators to handle the above complexity.

**Event Valid Time** refers to an understanding of an event in the real world. The corresponding operators are `ON` / `AT`: Alice was born `ON` January 3, 1950 `AT` 12:10am. Sometimes only a date is available, so we leave out the `AT` part.

**Event System Time** describes the creation, update, and deletion of records regarding those understandings. The corresponding operators are `KNOWN ON` / `KNOWN AT`. System time allows us to update event records, while reflecting past misunderstandings: see [the example from Wikipedia.](https://en.wikipedia.org/wiki/Temporal_database#Using_two_axes:_valid_time_and_transaction_time)

Suppose Alice was recorded on January 5 to have been born on January 2, 2022. That's "Born `ON` January 2 `KNOWN ON` January 5".

But on January 8 a clerk realized that the birth happened after midnight, and corrected the date of birth to January 3.

So, we update: "Born `ON` January 3 `KNOWN ON` January 8".

Subsequently, we can say `AS KNOWN ON` and `AS KNOWN AT` to refer to system knowledge at that time: this is a decision-time axis.

Searle would point out that there are actually two categories of events in the real world: the physical events, and the constitutive interpretations of those events -- what Searle calls "institutional facts" -- which are given by rulesets which are themselves subject to change.

So we now need to have a similar notion of valid and transaction time *but deriving from legislation*.

I propose to use:
- **Rule Effective Time** to refer to a rule being in effect with respect to a particular matter; at the level of an act, that's commencement; but certain provisions may apply with their own set of validity dates. Contracts likewise have an *effective date*, which can be backdated to before the date of execution.
- **Rule Source Version** to represent system, or transaction, time for rules. This encompasses drafts of bills, final versions passed by a legislature, subsequent [amendments](https://www.legislation.gov.uk/ukpga/2022/36/section/8) and [errata](https://www.legislation.gov.uk/ukpga/2022/36/pdfs/ukpgacs_20220036_en_001.pdf), and versions identified as "in force" or not. Contracts have draft versions of their own, and *dates of execution* which are not necessarily the same as the effective date, thanks to Conditions Precedent or explicit start dates in the contract.
- **Rule Encoding Version** to represent the L4 encoding of the rules. If an encoding was in error, we need to fix it, without necessarily implying that the underlying version changed; only our encoding of it did.

Legal rules, indexed by the above times, can now be articulated with precision: an Act passed in 2023, errataed in 2024, amended in 2025, encoded into L4 in 2026, and set to commence in 2027, can be unambiguously retrieved and referenced, variant by variant. If the L4 encoding was corrected in 2028, we know that the encoding time changed, but the version and valid times did not.

These things interact: every Act that is in effect is associated with a particular Rule Source Version and a Rule Encoding Version.

The operator `AT` can refer not just to an event in the real world, but also to an institutional fact derived from a combination of real-world events, and rules that are in effect `AS AT` that time:

`Alice Is Not A British Citizen` `AT` `June 1 2010` `AS AT` `July 1 2017` means

- Alice's birth record `AS KNOWN ON` July 1 2017 == `{ Born ON January 3, 1950; Born in Caribbean }`
- We retrieve the ruleset that was in effect July 1 2017; this comprises one or more Acts, each of which has its own Source Version Time and Encoding Version Time.
- We run those rules, querying about Alice's citizenship status on the date June 1 2010.
- Those rules return the decision that Alice is not a British citizen on July 1 2017.

Subsequently, however, a new set of legislation might revise the institutional fact of Alice's citizenship.

Suppose in 2020 an Act was passed that decided Alice should have been a citizen at that time after all.

`Alice Is A British Citizen` `AT` `June 1 2010` `AS AT` `October 1 2022`.

Composing these ideas, 

"AS AT T" means "taking the version of legislation in effect at time T, with the information known to the system at time T"

The absence of "AS AT" is shorthand for "taking the current version of legislation, using the latest information known to the system". But a decision record will always extensionalize those dates by filling them in.

For extra confusion, we may add the notion that these operators `ON/AT` and `AS AT` can themselves nest, as understandings of the past, and of retroactive evaluations, are increasingly refined. This is a controversial idea and possibly not a good one: let's proceed with this carefully, and only if people arrive intuitively at the same semantics for something like

Institutional_Fact `AT` Event Valid Time `AS AT` (Event System Time := Rule Effective Time) `AS AT` Rule Source Version Time `AS AT` Rule Encoding Version Time

I bring this up mostly to have an obvious target for agreeing to let's not do this.

* The other examples from `examples_for_parsing`

## Tech stack

The intention behind structured-examples.yaml is that it should be relatively easy to, e.g., render this in a webpage with functionality for filtering or sorting the examples. (See, e.g., Simon Willison's suite of datasette tools.)

But of course, there is always a tradeoff between how making the data more structured for future use and making it easier for a human to input it.
