# JL4 Haskell implementation 

JL4 as implemented here is a purely functional, statically typed, lazy programming language. Additionally, though, it has a
component that makes it possible to evaluate contracts, similarly to how they were described in [Tom Hvitved's PhD thesis](https://di.ku.dk/english/research/phd/phd-theses/2011/hvitved12phd.pdf). 

## Technical documentation for the contract evaluation feature of JL4

### Introduction to CSL

Hvitved introduces the so called "Contract Specification Language", CSL, which is described in chapter 2.3 of their thesis.
Since L4 already was a functional programming language, only the "contract specific" parts of the language were implemented,
more specifically 
- external choice 
- internal choice
- obligations
- fulfilment 
- contract conjunction
- contract disjunction
- deadline expressions

We did not implement after Hvitved
- instantiation (replaced by normal function application in L4)
- expressions (replaced by L4's expression language)

We additionally deviated from Hvitved by always having to make Party and Action explicit separately, not only in the case of 
external choice.

### Contracts

Contracts consist of the following two components: 

1. A so called "provision" which describes obligations to be fulfilled by the actions that it consumes, to reach
   one of three possible states: 
   - the contract is "stuck", i.e. there's a remaining obligation that awaits an event
   - the contract is "fulfilled", i.e. there's no remaining obligations needing to be fulfilled and successfully so
   - the contract is "breached", i.e. there's no remaining obligations needing to be fulfilled but unsuccessfully so 
2. A "trace" of events that (hopefully) match the obligations in the provision. Each event consists of
   - a "party" *who* is responsible for emitting the event
   - an "action" which describes *what* the party is doing
   - a "timestamp" which describes *when* the event was emitted

### Contracts and CSL in L4

#### Provisions

In L4, a typical provision would look as follows: 

```
-- declare yourself helpful datatypes that 
-- model party and action
DECLARE Person IS ONE OF B, S
DECLARE Action IS ONE OF 
  delivery 
  payment HAS amount IS A NUMBER

-- now, a contract is defined by declaring a value of type 
-- DEONTIC Person Action
aContract MEANS 
  -- this is a very simple obligation: 
  -- "the party S must deliver within 3"
  -- it is fulfilled, if an event "delivery" comes in at 
  -- some point that is less or equal than three time stamps 
  -- from the current time in the contract
  PARTY S -- this has to e of type Person
  MUST delivery -- this has to be of type Action
  WITHIN 3 -- this has to be of type (positive) Number
  -- "HENCE" is like the "then" in Hvitved's obligations
  HENCE
    -- after "HENCE", you can insert a new thing of type DEONTIC Person Action
    PARTY B
    MUST payment price PROVIDED price >= 20 
    -- payment price is a pattern of type Action, which binds "price" as a variable and brings it into scope 
    -- on the RHS of the PROVIDED clause, which allows your to further specify the events that match the obligation.
    -- The RHS of the PROVIDED clause needs to be of type BOOLEAN. In this case, it makes sure that the price paid is 
    -- at least 20. 
    -- Since a successful match on the pattern means we proceed in the HENCE clause, price also stays in scope there.
    -- Obviously it mustn't be in scope in the LEST clause, because that implies we didn't have a successful match.
    WITHIN 3
    -- since DEONTIC's are normal values, we can also return them as results of the usual expressions - in this case, 
    -- the result is FULFILLED if the price is 20 and otherwise a further obligation
    HENCE (IF price = 20 THEN FULFILLED ELSE PARTY B MUST return WITHIN 10)
    -- the LEST clause is entered, if the specific obligation can't be fulfilled anymore. This is the case if a non-matching
    -- event arrived after the deadline.
    LEST -- after a LEST, something of type DEONTIC Person Action has to follow, again
      PARTY B
      MUST EXACTLY payment fine 
      -- EXACTLY is a keyword in patterns that makes them a boolean guard using equality, i.e. the pattern matches iff the 
      -- expression after EXACTLY is equal to the value that is being strutinised. Mind that this means that we can use values
      -- in patterns where normally "fine" would just be bound as a new variable and brought into scope for the HENCE and PROVIDED
      -- parts of the obligation
      WITHIN 3
  WHERE
  fine MEANS 10
  -- of course we may also use our usual WHERE clauses
```

As you may have noticed, you may leave out most of the constructs. The semantics for leaving them out are precisely as follows: 
- leaving out `HENCE`: works like `HENCE FULFILLED` i.e. if the action and party match, the contract is fulfilled
- leaving out `LEST`: the clause is an obligation (in contrast to an external choice) and thus the contract is in the breached state
  if a breaching event arrives (one that is after the deadline but doesn't match the party and action)
- leaving out `WITHIN`: this is like "whenever", i.e. there's no deadline. Mind that this means that an obligation without a deadline 
  may never result in a breach itself, because there can never be an event that arrives after the deadline
- leaving out `PROVIDED` after matching on an action: like `PROVIDED TRUE`, i.e. we always proceed to `HENCE` if the pattern 
  itself matches.

Wrt syntactic sugar, mind that you may also write 
- `MAY` or `DO` instead of `MUST`
- `{MAY,MUST} DO` instead of `{MAY,MUST}`

This does not change semantics *at all*, it's merely sugar.

#### Traces

Now for evaluating traces against this provision, there are two ways of doing it: 

- `EVALTRACE` which is simply a function of type 
  `FORALL party action. DEONTIC party action -> NUMBER -> LIST (EVENT party action) -> DEONTIC party action`
- `#TRACE` which (conceptually) desugars into `#EVAL EVALTRACE`

```
#TRACE aContract AT 0 WITH
  PARTY S DOES delivery AT 2
  PARTY B DOES payment 30 AT 6
  PARTY B DOES payment 10 AT 9

-- is equivalent to

#EVAL EVALTRACE aContract 5 (LIST EVENT S delivery 2, EVENT B (payment 30) 6, EVENT B (payment 10) 9)
```

`#TRACE` is thus slightly more convenient, whereas `EVALTRACE` is more flexible.

#### Some particularities that might cause hiccups

- "Regulative Or" (`ROR`) and "Regulative And" (`RAND`) do not apply to specific obligations, but to *entire provisions*. 
  This is because they evaluate last. If you want to build a mental model, imagine that the traces are duplicated and 
  each of the provisions get sent their own set of events.
- Contracts never make any progress unless they get sent events. This is especially the case for breaching the contracts:
  a contract never breaches because there's "no events left" because the provision doesn't know that - there may always be 
  new events later. As a helper, `` `WAIT UNTIL` `` exists which allows you to make some time pass and advance the contract 
  until that timestamp.
- Event traces are assumed to be ordered by time
- All time stamps in the event traces are assumed to happen after (or at the same time) the starting time of the trace was
- Times in obligations / external choices are supposed to be 0 or greater.
- There's no *explicit* difference between external choice and obligations, this is because the language ought to be usable
  even for people who don't know CSL. The difference arises by use (or not use) of `LEST`. `LEST` being used means "this is an 
  external choice", `LEST` not being used means "this is just an obligation".
- While provisions could be imagined as a state machine (and are vaguely described as such in Hvitved's thesis), in this  
  implementation, they're internally *not defined as such*. They're defined in terms of the stack machine that also evaluates 
  the rest of the expression language. More specifically, there's also no state space reduction going on or similar when using
  (expression language) recursion in contracts.

### Further reading

- [Hvitved's thesis](https://di.ku.dk/english/research/phd/phd-theses/2011/hvitved12phd.pdf), more specifically chapter 2.3
- in this repository: 
  - `jl4-core/src/L4/EvaluateLazy/Machine.hs`
  - `jl4-core/src/L4/EvaluateLazy/ContractFrame.hs`
  - `jl4/examples/ok/contracts.l4`

## How to use annotations for citations

Similarly to the NLG annotations, there are annotations to allow for citing (and making hover annotations available) within JL4 source code.

```
@ref-src citations.csv
@ref-map foo bar baz https://example.com
GIVEN x IS A BOOLEAN, y IS A BOOLEAN @ref foo bar baz
GIVETH A BOOLEAN @ref 1981/61 sec. 2
DECIDE xor x y IS  @ref SG-c-2025-sghcf-14
     x AND NOT y <<SG-c-2025-sghcf-12>>
  OR NOT x AND y @nlg Locally overwrite the annotation for %y%
```

Citation files can be loaded via the `@ref-src` annotation at any toplevel location of an L4 file.

These csv files have the following shape:

```csv
"regex:sg-c-(\d{4})-([a-z]+)-(\d+)","https://www.elitigation.sg/gd/s/$1_$2_$3"
"1981/61 sec. 2","https://www.legislation.gov.uk/ukpga/1981/61/section/2"
"1981/61 sec. 3","https://www.legislation.gov.uk/ukpga/1981/61/section/3"
"2002/8 sec. 1","https://www.legislation.gov.uk/ukpga/2002/8/section/1"
```

I.e. you can put verbatim citations (and a link that belongs to it), or you can put regexes by first prefixing your regex with `regex:` and then putting a link where the captures are reinserted again.
This can be used to programatically generate reference mapping links.

The `@ref-map` annotations can be used to amend the existing mapping in the same vein.

Now the annotations can be initiated using either `@ref` or `<<` and `>>` as shown in the example above.

When using this feature in the IDE, it will show the generated or verbatim links in the reference mapping as a hover link that can be clicked on.
