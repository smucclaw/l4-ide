# Examples of Regulative Rules

## By Contrast with Constitutive Rules

Constitutive rules talk about what things must **be** -- usually to
qualify, or be valid, in some way. For example, "for the purposes of a
certain contract, for a day of the week to be considered a weekend
day, it must be a Saturday or a Sunday."

Regulative rules talk about what people must **do**, mustn't do, or may do
-- and what happens to them if they do, and if they don't. Sometimes
there are deadlines associated.

As a special case, some rules talk about what persons must **have
done**, to qualify for some future action. These hybrid rules blend both
constitutive and regulative aspects, because a person who followed a
certain path described by regulative rules, may now qualify in a
certain way as having a special status, which is a constitutive.

## Borrowing a book from the library

## Picking up kids from school

A Fine is a Price.

## Flood & Goodenough loan agreement

## Fighting International Crime

[MACMA](https://sso.agc.gov.sg/Act/MACMA2000) says:

> Where the appropriate authority of a foreign country makes a request
> that evidence be taken in Singapore for the purposes of any criminal
> proceedings pending in a court in the foreign country, the
> Attorney-General may, by written notice, authorise a Magistrate to take
> the evidence.

Imagine a busy and tired Attorney-General, faced with a fax that just
randomly showed up ("I didn't know we even have a fax machine!"),
skeptically examining the request, looking for any reason at all to
turn it down:

- does this come from a foreign country?
- is the sender some appropriate authority of that country?
- are they requesting that evidence be taken in Singapore?
- is the evidence for the purposes of a criminal proceeding
  - that is pending in a court
  - in that foreign country?

If any of those questions comes short of a "yes", then the AG would
say "fax them back and tell them to go look at the MACMA
requirements."

### Reasons To Say No

Maybe it's a civil proceeding. Maybe the fax didn't show any proof
that the criminal proceeding is pending in a court. Maybe the
authority who sent the fax is a president whose impeachment is very
likely to conclude successfully within the next week. Any one of these
facts could give the AG grounds to reject the request.

Suppose the volume of faxes grows from a trickle to a flood. The AG
doesn't want to have to think through all these questions each time.
She could delegate the screening to a staff member. Or to a computer.
Either way the AG has to export the above questions from her head into
a checklist that an eager but not very experienced staff member can
understand.

How might the AG teach their team to handle these faxes?

### Constitutive Elements: Black And White Decision Logic

The "where" part
of the sentence is phrased as a *constitutive rule* that determines if a
request is **valid**, or **qualifying**.

The AG might rearrange the sentence on a whiteboard:

- [ ] the appropriate authority of a foreign country
- [ ] makes a request
- [ ] that evidence be taken in Singapore
- [ ] for the purposes of
- [ ] any criminal proceedings
- [ ] pending in a court
- [ ] in the foreign country

The AG might draw a checkbox at the left of each line above, and say
to her staff, "every fax that comes in, file it into the Go pile if
all the boxes get checked; file it into the Review pile if any of the
boxes isn't checked."

That deals with the *decision logic* of the request.

### Regulative Elements: The Moving Parts

Next we deal with the *regulative* aspect of the sentence:
the behaviours of actors, the moving parts, events happening in time.

If the request is valid, the AG moves on to the next step:
- write a letter
- to some magistrate
- instructing them to take the evidence specified in the request

At the top level, there are two connected events: the foreign
authority makes a (valid) request; then the local Attorney-General
authorises a Magistrate.

How does L4 connect those two events? With `HENCE`.

```l4
GIVEN  fc    IS A Country
       ag    IS A Person -- the attorney general
PARTY  auth  IS A Person
  WHO  `is an appropriate authority`
  MAY  `make a request` -- details to be fleshed out later

HENCE  PARTY  ag
         MAY  `authorise a Magistrate` -- details later
```

### About `HENCE`

`HENCE` connects two regulative stanzas.

If the first stanza is fulfilled -- if the preconditions are met and
the party does the action properly by the deadline -- then the plot of
the story proceeds to the stanza under `HENCE`.

Indeed, you can think of `HENCE` as a special kind of `THEN`.

You can trace the "happy path" of a contract by following the `HENCE`
connections: everybody does what they should, and before you know it,
the game is over and everyone is happy.

For every `THEN`, there's an `ELSE`. What's `ELSE` in L4?

`LEST`.

### About `LEST`

`LEST` also connects two regulative stanzas.

If the first stanza is not fulfilled -- if the preconditions are met,
but the party does *not* perform the action properly by the deadline
-- then the plot of the story turns to the `LEST` branch. Penalties
and redemption.

This is the structure of many stories: some original sin occurs, and
the story concerns itself not with what should have happened ideally,
but how the hero strives to repair the damage and restore the world to
its original Edenic state. This gives us the *Odyssey*, not to mention
the *Avengers: Infinity War and Endgame* couplet, *and *Spider-Man:
Into the Multiverse*. It also gives the `except` branch of every
`try/except` exception handler. And it gives us that part of contracts
which deal with reparations.

Most `MAY` stanzas don't have an explicit `LEST`, because if an actor
chooses not to do something that was optional in the first place,
that's fine. Technically speaking, an implicit default `LEST
Fulfilled` is automatically inserted to fill the gap.

Many `MAY` stanzas follow the pattern "Party A MAY do X, hence Party B
MUST do Y" -- because an optional action is only interesting if it
eventually creates a new obligation. This MACMA example breaks that
pattern, but that's sovereignty for you.

*Theory note:* This is a good illustration of the special nature of
official communication. Searle identified *speech acts* as utterances that
constitute institutional events. Many labeled transition systems
formalize these speech acts as messages passed between actors:
requests, notices, authorisations, demands, apologies, and so on are
types of such communications.

### Constitutives Within Regulatives

The two top-level actions -- by the foreign appropriate authority, and
by the Attorney-General -- are themselves qualified with constitutive
elements. For the incoming request to be valid, it must meet certain
criteria, as seen above. And the action taken by the AG is narrowly
specified: to authorise a Magistrate, in writing, to take the evidence.

So that twines us back to the *decision logic* of validity and
qualification. In other words, within Searle's regulative rules, we
find embedded constitutive rules.

Let's flesh out those details.

```l4
GIVEN  fc    IS A Country
       ag    IS A Person -- the attorney general

PARTY  aa    IS A Person
  WHO  `is an appropriate authority`
  MAY  `make a request`
         that: `evidence be taken in Singapore`
         for:  `the purposes of`
               .. `any criminal proceedings`
               .. `pending in`
               .. `a court in` fc
HENCE  PARTY  ag
         MAY  `authorise a Magistrate`
              by: `written notice`
              to: `take the evidence`
```

### Syntax For Parameterizing and Conjoining Components

We've introduced a couple of new syntactic constructs here: the
**parametric colon** `:` gives detail to actions; and the
**asyndetic conjunction** `..` breaks up a long sentence into
shorter chunks, each of which is a Boolean.

Those chunks matter: they form the constitutive conditions for a thing
to qualify.

### Expanding and Substituting "An Appropriate Authority"

Some of these questions may expand further. From MACMA:

> “appropriate authority”, in relation to a foreign country, means a person or authority whom the Attorney‑General is satisfied is authorised under the law of that country —
> (a)	in the case of a request by that country to Singapore for assistance in a criminal matter, to make the request; or
> (b)	in the case of a request by Singapore to that country for assistance in a criminal matter, to receive the request;

That turns into the following L4:

```
DECLARE Direction IS ONE OF Inbound, Outbound
GIVEN  fc    IS A Country
       ag    IS A Person -- the attorney general
       aa    IS A Person -- someone claiming to be an appropriate authority
       dir   IS A Direction
       `to make request`     IS A BOOLEAN
       `to receive request`  IS A BOOLEAN
DECIDE `is an appropriate authority` IF
          `is a person`     aa
       OR `is an authority` aa
  AND `is satisfied is authorised under the law of` ag aa fc
      ..  CONSIDER dir
              WHEN Inbound  THEN `to make request`
              WHEN Outbound THEN `to receive request`
```

... Or does it?

### Ambiguity in Appropriate Authority

The process of formalization lays bare another lexical ambiguity in
the original statute.

There is another possible reading of the same text:
```
DECIDE `is an appropriate authority of` IF
       `is a person`
    OR `is an authority` aa
        AND `is satisfied is authorised under the law of` ag aa fc
            ..  CONSIDER dir
                   WHEN Inbound  THEN `to make request`
                   WHEN Outbound THEN `to receive request`
```

In other words, authorities need to be authorised, but persons are enough.

Let's look at what happens if the request originates from:

1. the Department of International Crimes

2. Lex Luthor, Special Master appointed by some judge in a criminal case

3. Jonah Jameson, attorney for the defense

In the first case, the fax contains, in Annex A, an excerpt from some
law of the foreign country establishing the department jointly under
the Ministry of Law and the Ministry of Foreign Affairs, and
empowering it to make requests of foreign governments. So the AG is
satisfied that the department is an appropriate authority, authorised
under the law of the foreign country to make such requests.

In the second case, some research reveals that the criminal case in
the foreign country has multiple international elements; the fax
includes, in Annex A, a court order from the relevant judge appointing
Lex Luthor as a special representative of the court instructed to
liaise with foreign governments to gather relevant evidence. So the AG
is satisfied that Lex is a person authorised under the law of the
foreign country to make the request.

### Any Person!?

In the third case, the AG can't find any law in the foreign country
that authorizes attorneys for the defense to request evidence from
international sources. So Lex Luthor is just a person, and he is
looking like an unauthorised person at that. The AG gets ready to
write back, saying, sorry, you don't seem to be an appropriate
authority. But Lex Luthor, having anticipated this, has attached, in
Annex A, text from our statute:

> “appropriate authority”, in relation to a foreign country, means

> **a person**

> **or authority** whom the Attorney‑General is satisfied is authorised under the law of that country —

>   (a) in the case of a request by that country to Singapore for assistance in a criminal matter, to make the request; or

>   (b) in the case of a request by Singapore to that country for assistance in a criminal matter, to receive the request;

Lex has made a small circle, in pen, around "a person"; and a large
circle around "or authority ..." through to the end of the paragraph.

In this admittedly preposterous reading, an authority such as a
department under the Ministry of Law would need to be explicitly
authorised under the law of the foreign country ... but an individual
person doesn't need to be so authorized; indeed, any natural person
could be the source of a request.

Preposterous as this may be, the Attorney-General's staffer handling
this case does not speak English as a first language; if they did,
they would formulate an argument that if Lex's interpretation were
what Parliament had intended, the text of the definition would have
said "a person or **an** authority whom..." ... but this distinction
is too subtle, and eludes them.

Instead what happens is the staffer just goes along with Lex's
interpretation.

And Lex Luthor wins the right to collect their evidence.

It might be obvious to the average native speaker of English that this
can't possibly be what Parliament intended; but Lex got to be the
richest man on the planet precisely by outsmarting the average native
speaker of English!

If this law had been drafted in L4 in the first place, it would have
been clear which interpretation was intended.

## Discretion to Proceed

Note that the AG isn't forced to do proceed to instruct the
Magistrate: the law says that they *may*. It doesn't say that they
*must*. Maybe the request comes from some pariah state which the
international community proscribes? Then the AG could ignore the fax
without consequence: that's why the `LEST` of a `MAY` is `Fulfilled`.

## Putting it all together in L4

The L4 could be even more specific about the evidence and the
magistrate, and general about the countries involved:

```l4
GIVEN  fc    IS A Country
       sg    IS A Country
       ag    IS A Person -- the attorney general
       mm    IS A Person -- the magistrate
       e     IS AN Evidence

   IF  sg EQUALS Singapore
PARTY  aa    IS A Person
  WHO  `is an appropriate authority`
  MAY  `make a request` -- this will become a action datatype
         that: `evidence` e
         be:   `taken in` sg
         for:  `the purposes of`
               .. `any criminal proceedings`
               .. `pending in a court`
               .. `in` fc

HENCE  PARTY  ag
         WHO  `is the attorney general of` sg
         MAY  `authorise` mm
              by: `written notice`
              to: `take the evidence` e
          IF  `is a magistrate of` mm sg
```

This structure now lays bare the necessary parameters to the moving parts: we have room to explicitly identify

- the foreign country
- the responding country (which is quickly required to be solely Singapore)
- the attorney general
- some magistrate in Singapore
- the evidence

And the end-user filling in this form will have to click through the
following fields to confirm their truth value:

- [ ] `is an appropriate authority`
- [ ] `make a request`
- for
- [ ] `evidence` e
- [ ] `taken in` sg
- [ ] `the purposes of`
- [ ] `any criminal proceedings`
- [ ] `pending in a court`
- [ ] `in` fc

You could imagine the AG checklisting this in her head.

## Desugaring the parametric colon

The colon `:` was chosen to match current usage in Javascript, Python, JSON, and YAML: key/value syntax.

How would an L4 implementation desugar and interpret this syntax?

We know from CSL that the idea is to have an `action` with `action_parameters`.

```
   IF  sg EQUALS Singapore
PARTY  aa    IS A Person
  WHO  `is an appropriate authority`
  MAY  `make a request` -- this will become a action datatype
         that: `evidence` e
         be:   `taken in` sg
         for:  `the purposes of`
               .. `any criminal proceedings`
               .. `pending in a court`
               .. `in` fc
```

We can use named record fields for a lot of this:

```
DECLARE requestParams
    HAS that  IS A BOOLEAN
        be    IS A BOOLEAN
        for   IS A Record
          HAS `the purposes of`          IS A BOOLEAN
              `any criminal proceedings` IS A BOOLEAN
              `pending in a court`       IS A BOOLEAN
              `in`                       IS A FUNCTION FROM Country TO BOOLEAN

DECLARE Actions
  IS ONE OF
    `make a request` HAS params  IS A requestParams
    `authorise`      HAS params  IS A authoriseParams
    
... MAY `make a request`
        that IS `evidence e`
        be   IS `taken in` sg
        for  IS `the purposes of`
                `any criminal proceedings`
                `pending in a court`
                `in` fc
```





