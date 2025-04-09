# Examples of Regulative Rules

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

Let's rearrange the sentence:

1. Where the appropriate authority of a foreign country
2. makes a request that
   1. evidence be taken in Singapore
      1. for the purposes of
         1. any criminal proceedings pending in a court in the foreign country
4. the Attorney-General may
   1. by written notice
   2. authorise a Magistrate
   3. to take the evidence

*Comment:* Already we detect a lexical ambiguity: "... for the purposes
of criminal proceedings" makes sense. But is it the *request* that is
made for the purposes of criminal proceedings? Or is it the *evidence*
that is to be taken for the purposes of criminal proceedings? Maybe it
doesn't matter. We soldier on.

At the top level, there are two events: the foreign authority makes a
request, and the local Attorney-General authorises a Magistrate.

Let's follow that two-event pattern as we start sketching our encoding
into L4's regulative syntax.

```l4
GIVEN  fc    IS A Country
       ag    IS A Person -- the attorney general
PARTY  auth  IS A Person
  WHO  `is an appropriate authority of` fc
  MAY  `make a request` -- details to be fleshed out later	

HENCE  PARTY  ag
         MAY  `authorise a Magistrate` -- details later
```

*Theory note:* This is a good illustration of the special nature of
communication. Searle identified *speech acts* as utterances that
constitute institutional events. Many labeled transition systems
formalize these speech acts as messages passed between actors:
requests, notices, authorisations, demands, apologies, and so on are
types of such communications.

The two top-level actions -- by the foreign appropriate authority, and
by the Attorney-General, are themselves qualified. For the incoming
request to be valid, it needs to have a particular purpose. And the
action taken by the AG is narrowly defined to authorise a Magistrate,
in writing, to take the evidence.

So that twines us back to the *decision logic* of validity and
qualification. In other words, within Searle's regulative rules, we
find embedded constitutive rules.

Let's flesh out those details.

```l4
GIVEN  fc    IS A Country
       ag    IS A Person -- the attorney general

PARTY  aa    IS A Person
  WHO  `is an appropriate authority of` fc
  MAY  `make a request`
         that: `evidence be taken in Singapore`
         for:  `the purposes of`
               .. `any criminal proceedings`
               .. `pending in a court`
               .. `in` fc
HENCE  PARTY  ag
         MAY  `authorise a Magistrate`
              by: `written notice`
              to: `take the evidence`
```

We've introduced a couple of new syntactic constructs here: the
**parametric colon** `:` used to give detail to actions; and the
**asyndetic conjunction** `..` used to break up a long sentence into
shorter chunks.

Those chunks matter: they form the constitutive conditions for a thing
to qualify.

Imagine a busy and tired AG, faced with some random fax that just
showed up ("I didn't know we even have a fax machine!"), skeptically
examining the request, looking for any reason at all to turn it down:

- does this come from a foreign country?
- is the sender some appropriate authority of that country?
- are they requesting that evidence be taken in Singapore?
- is the evidence for the purposes of a criminal proceeding
  - that is pending in a court
  - in that foreign country?

If any of those questions comes short of a "yes", then the AG would
say "fax them back and tell them to go look at the MACMA
requirements."

But if all the requirements are met, the AG moves on to the next step:

- write a letter
- to some magistrate
- instructing them to take the evidence specified in the request

Note that the AG isn't forced to do this: the law says that they
*may*. It doesn't say that they *must*. For instance, maybe the
request comes from some pariah state which the international community
proscribes? Then the AG would ignore the fax without consequence:
that's why the `LEST` of a `MAY` is `Fulfilled`.

The L4 could be even more specific about the evidence and the magistrate, and general about the countries involved:

```l4
GIVEN  fc    IS A Country
       sg    IS A Country
       ag    IS A Person -- the attorney general
       mm    IS A Person -- the magistrate
       e     IS An Evidence

   IF  sg EQUALS Singapore
PARTY  aa    IS A Person
  WHO  `is an appropriate authority of` fc
  MAY  `make a request`
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

- `is an appropriate authority`
- `make a request`
- `evidence` e
- `taken in` sg
- `the purposes of`
- `any criminal proceedings`
- `pending in a court`
- `in` fc

You could imagine the AG checklisting this in her head.
