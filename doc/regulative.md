# Meng's thoughts on L4's syntax and semantics for regulative rules

Unlike constitutive rules which create institutional facts, and have
the pattern "for the purposes of P, X counts as a Y if conditions C are met",

regulative rules govern the behaviour of parties: "if a person meets
criteria C, then they must, may, or must-not, perform some action; if
they do, then this happens, and if they don't, then that happens."

## Requirements / Real-World Patterns

"In the wild", certain patterns recur in legal drafting.

We seek to equip L4 with a semantics and idiomatic syntax that allows
a legal drafter to express those patterns, and a compiler developer to
translate L4 code into downstream formalizations without loss of
semantic validity.

### Party A does something, then Party B, then Party A, and we're done.

Simple contracts typically look like:

1. Party A tells Party B what she wants to buy
2. Party B tells Party A what it costs
3. Party A transfers that amount of money, by a certain deadline.
4. Party B transfers the thing, by a certain deadline.
5. Party A signs a receipt
6. Everybody's happy

We consider this to be a multi-agent system, which can be modeled as
communicating sequential processes, synchronizing over notices,
payment, and delivery -- three possible actions, of many.

The above specification is given in an ordered sequence of events that
represent the "happy path".

Legal contracts also deal with what happens when parties fall off the
happy path -- a required action is not done by the deadline.
_Reparations_ can be made to restore relations.

This is an important point of departure from computer systems which
are often written to "do the right thing". Such operational systems
tend to focus on the happy path. Programs do handle exceptions, but
any departure from intended, desired behaviour is considered a bug.

In legal specification, "rules are made to be broken". A law or
contract must give equal weight to the possibility that things might
go wrong, intentionally or not. The criminal code is all about leaving
the happy path.

So we need a way to represent not just a linear sequence, but a graph
of events, choices, and consequences.

### An Action Expression Language

We need a way to represent parties taking certain actions, like
sending money, or delivering goods, or otherwise communicating with
each other.

Meng has previously suggested a "prepositional logic" to describe
actions complexly -- parameters to actions which clarify when exactly
those actions have been performed to spec. The most general parameter is "such that".

This logic overlaps with the decision logic aspect of the language.
The question of whether an action was taken or not can be expressed as
a Boolean circuit.

### A Decision Expression Language

In any programming language with `if (...)` statements, we need to be
able to evaluate the conditions between the parentheses.

Internally, L4 reframes the binary operators `OR` and `AND` into
disjunctive `ANY` lists and conjunctive `ALL` lists. We also have the
usual `NOT` negation.

We extend this basic logic with unknown and default values.

See <default-logic-1.md> and <default-logic-2.md>

### Contract Composition and State Transitions

If reparations fail, the contract ends in breach. Whose fault is the
breach? Hvitved's CSL likes to ask and answer that question. In CSL,
the "fulfilled" and "breach" primitives are the final states of any
contract -- in the language of finite automata, they are the
"accepting states".

Composition means that a completed subcontract is merely one step
along the path of a larger contract. Wiring up the "fulfilled" and
"breach" outputs of a subcontract to the inputs of a larger contract
happens through our `HENCE` and `LEST` operators.

CSL contracts can be composed from subcontracts using contract
disjunction, conjunction, and sequencing.

### Applicability: Preconditions for a state transition to be enabled

In legal drafting, certain clauses or sections are said to "apply" if
certain conditions are met. If they do not apply, then the rules
within them are, in some sense, inactive.

In the language of state transition modelling, conditions are located
in "guards"; when the conditions are met, the transition is "enabled".

When conditions are not met, even if a party attempts to perform some
action described in that section, the action is not recognized as
valid. As far as the contract or law is concerned, nothing significant
has happened, and nobody needs to respond to that action.

### Temporals for Deadlines

Actions need to be completed by a certain deadline. So time is a factor, and we need a way to model it.

In law, certain regulative rules are always in effect. Usually it is
only when a rule is triggered by noncompliance that a clock starts to
tick.

But sometimes, as with borrowing a library book, a voluntary action
starts the clock.

There is a rich literature and a wide variety of software that models
and verifies temporal automata, concurrent processes, and timed Petri
Nets.

### Updating State Variables

A contract, or law, that computes a certain numeric value may refer to
the history of "how we got here", and perform arithmetic on a formula
whose terms are set in the course of execution.

Sometimes introspection to the history of the trace is appropriate.

Sometimes it is more appropriate to update a symbol table of variables.

For example, Party A tells Party B she wants to buy a bicycle. Later
she realizes she also needs a helmet. And a bike lock. These state
transitions loop over the "order" subcontract, and write values to a
data structure somewhere that tracks a list of dicts, or something
like that. When we move to the cash register we take the sum over the
elements, and apply further logic: some of the items may be taxable
and other items may not.

These ideas of updating state variables, and filtering a list, need to
be expressible in the language.

Haskell's State monad offers `get` and `put` operators.

#### State Variables allow long-distance cause-and-effect

The immediate consequences of noncompliance are often spelled out in close proximity to the regulative rule that prescribes the required behaviour.

But sometimes the penalties are written in a different section.

"A person who commits an offence under sections 10 through 20 is liable to the following penalties, according to the number of offences committed ..."

The offences are first enumerated, the way a diner might speak to a
waiter, who writes down the order on a notepad; later, the penalties
are assessed, the way the bill is calculated at the end of the meal.

The notepad relays the order from the patron via the waiter to the bill.

In L4, the State mechanism relays information about offences to the penalty section. We `put` information into state variables, and `get` them out later.

### Data Modeling

We already have a `DECLARE` data modelling expression minilanguage. We
can instantiate those classes (as records) and use `get` and `put` to
update those records.

Type inference becomes ontology inference: when a human hears "if a
person has a pet, and that pet is male" we can infer the following:

```l4
 DECLARE Person
     HAS OPTIONAL Pet
	     HAS sex OPTIONAL ONE OF Male, Female
```

Which is really two `DECLARE` stanzas sugared into one.

### Temporal Modeling of Rules and Instance Data

A fuller treatment of multi-temporal databases is given in <multitemporals.md>

### Master Contracts and Transaction Instances

The above example contract only really begins in earnest at step 3. But if it
is an of many routine transactions occurring under some sort of Master
Services Agreement, then the larger contract is the MSA and the
particular concrete purchase -- the instance of the transaction --
could be said to start at step 1.

We want to be able to instantiate particular `person`s into the class
of `Person`, and use those instances as part of an operationalization
of a contract.

## Background

### Hvitved is an inspiration

See Hvitved's PhD thesis on a contract language, CSL, with a
trace-based evaluation and blame assignment model; if we analyze its
DNA we can detect traces of CSP (Communicating Sequential Processes)
as well as FSA (Finite-State Automata) and ECA
(Event-Condition-Action) systems.

Process algebras typically allow synchronization. We would extend CSL
to express legal "notices" to serve the function of message passing.
Synchronization could occur over other events as well.

Previously we have extracted a Petri Net representation of a contract,
though some other formalism may be preferred for our next iteration.

### Model Checking is a desired goal

Model checking systems like UPPAAL illustrate the notion of testing
contracts for satisfaction of assertions, or properties, expressed in
CTL. SPIN could be used to model-check assertions in LTL.

See original inspiration [Model Checking Contracts](https://drive.google.com/file/d/1X9NB5itJjXeZCZcJ4vsmZhm3EoTLD2_Z/view?usp=drive_link).

See [previous paper published by Joe Seng
Watt](https://ink.library.smu.edu.sg/sol_research/4367), where much of
the heavy lifting was done in Maude. This allowed us to detect a race
condition.

This model checking becomes relevant below, because it allows us to
"squeeze out the deontics" into an **object-level contract** vs
**property-level assertions** _about_ the contract. More on that later.

### Hypotheticals

Sometimes a contract will want to run a subcontract in a modified environment.

The Reader Monad's `local` operator may suffice for this.

It will likely be necessary to expose the "call stack" to a function, because some functions are defined in terms of the context in which they are being evaulated.

For example, see the definition of "Conversion Price" in the [500 Startups' KISS instrument](https://kindrik.sg/template/se-asia-convertible-note-kiss-terms-2021-05-25.pdf)

## Bugs in contracts and legislation can be due to a number of factors

### Ambiguities

Lexical and syntactic ambiguities (e.g. steak and fries or salad)

Definitional ambiguities (Section A does not apply if Section B applies. If Section K later refers to Section A in a situation where it is unknown if Section B applies ... how do we evaluate it?)

Incomplete Pattern Matches. Sometimes a particular edge case is not addressed.

### Type Errors

Sometimes two passages, which are supposed to be syntactic
transformations of each other, may fail to align, leading to
difficulties with interpretation.

Or some equivocation occurs, causing a two instances of the same
variable to be overloaded with different types, in a pathological case
of shadowing.

# A Two-Level Framework for Regulative Formalization

Model checkers like UPPAAL and SPIN help to verify properties about
object-level timed automata or process models of communicating FSMs.
Those properties are written in CTL and LTL, at a higher level than
the system descriptions they verify.

When humans ask questions about legal scenarios, they often want to know:

- based on what has happened so far, what situation am I in?
- based on the situation I am in, what immediate obligations do I face?
- if I want to achieve a particular outcome state, what courses of action should I consider?
- if I want to achieve any outcome state that satisfies certain properties, what courses of action should I consider?
- given those courses of action, what immediate next steps should I take?
- given multiple courses of action, optimize / order them by some value function.
- how might other players in the game seek to frustrate my courses of action?
- is it true that if I want to achieve X goal, I must do Y? How much time do I have?
- what if I want to achieve X goal without doing Y?

Encoding regulative rules in L4 should facilitate machine-assisted answering of the above questions.

## Squeezing out the deontics

From the perspective of Gneezy & Rustichini's classic [A Fine Is A
Price](https://www.jstor.org/stable/10.1086/468061), we see that
penalties have at least two dimensions: economic/financial/monetary,
and moral/social.

What does "must" really mean?

When we tell a child "you must wash your hands" a born rebel will ask "or else what?"

If there is no consequence, is the "must" really a "must", or more of a "should"? Or a "may"?

These questions are endlessly debated by philosophers.

L4's position is set out below.

## Distinguishing the letter of the law from the spirit of the law

The spirit of a law may claim to allow certain small businesses to receive some sort of relief.

The letter of the law may introduce requirements that are so onerous nobody can actually receive that relief.

For example, a circular requirement:

1. to obtain relief, submit a form C.
2. to obtain a form C, file a form B.
3. to obtain a form B, file a form A.
4. to obtain a form A, file a form C.

More sophisticated versions of such rules may hide the fact that a particular rule is not meant to be "winnable".

Or, requirements that may turn out to be impossible to satisfy, depending on the whims of the bureaucracy:

1. Relief will be available if requested in June. Requests after June will be disregarded.
2. To request relief, file a form C. It may take up to three months to receive a response after filing form C.
3. To obtain a form C, file a form B. It may take up to two months to receive a response after filing form B.
4. To obtain a form B, file a form A. It may take up to one month to receive a response after filing form A.
5. Form A filings will be accepted no sooner than March.

Static analysis methods and formal verification allow software to automatically detect such scenarios.

This is done by writing the object level program separately from a
property-level assertion _about_ the program. A formal verification
engine then statically analyzes that program to see if the assertions
hold or fail. Counterexamples can be automatically generated to show
how it is possible, or impossible, to "win" the game.

At the "spirit" level we may say "you must pay a fine if you return a book late".

At the "letter" level we set out the exact fee schedules, definition
of lateness according to which category the book is in, and mechanics
of fine assessment and payment.

Then we can see if the spirit of the law matches the letter of the
law. Is there some loophole that allows someone to get away with not
paying the fine?

## The Object Level: from a coldly dispassionate perspective, an automaton simply executes a trace.

> When you borrow a book from a library, a clock starts ticking.

> You could return the book after a day. This is choice A.

> You could return the book after six months. This is choice B.

> One of those choices (B) leads to a fine, and restricted borrowing privileges.

> One of those choices (A) does not.

> If you pay the fine (choice C), the library will let you check out more books.

> If you do not pay the fine (choice D), the library will not.

The above wording may appear oddly non-judgmental; it focuses on the mechanics. There is a sort of Sartrean existentialism here.

Most library rules tend to use more weighted phrasing: "you **must** return borrowed books within 14 days, or be subject to a fine."

"You **may not** borrow books if a fine is outstanding."

"When no fines are outstanding, you **may** borrow books for up to 14 days, unless they are in the reserve."

These are "deontic modals" used in "normative statements". They
indicate that a certain choice is strongly preferable, and that
alternative choices lead to negative consequences.

The "if" and "unless" keywords are "conditional operators".

## Object-Level versus Assertion-Level

L4 uses a state transition formalism to represent the moving parts of
a regulative rule.

Whenever somebody has to do something -- or refrain from doing
something -- within a certain time period, and face consequences for
noncompliance, we use L4's regulative statements to express those
rules.

That formalism _could_ use the bloodless, mechanical form of the
rules: one thing leads to another and another and another. We could
render a finite state automaton, or a state transition system, using
the most unopinionated, nonjudgemental language imaginable. This is
the "object level" representation of a normative system.

"If this action is not taken, a penalty fee will be added to the invoice."

"If the penalty fee is not paid, borrowing privileges will be suspended."

Maybe a library user doesn't care if they lose borrowing privileges
forever. Then the "you must pay a fine" is a toothless rule: it is not
strongly _enforceable_.

## Deontic Statements are Bounded

The statement "you must pay a fine" carries with it an unspoken
complement: "or else you will not be allowed to borrow again."

These complements usually exist in informal discourse, yet they usually go unspoken.

When you hear a "must" but don't hear the explicit consequence, the
consequence is frequently "or you commit an offence, and are subject
to some sort of legal penalty."

Or it might be "or you sin against your fellows, and are subject to
social misapprobation."

Rustichini & Gneezy asked what the complement -- the bound -- was, for
the rule "You must pick up your child from childcare before 7pm."

In L4, every deontic statement is bounded. Even a "may" permission is
bounded, in that taking that course of action that eventually causes
some other party to assume some obligation that they would otherwise
not.

## Deontic consequences are explicated at the Assertion Level

"You must do this thing if you don't want to pay a monetary penalty"

"There is no way to reach a FULFILLED outcome without paying a penalty, other than by doing this thing."

These kinds of statements are formalized in LTL / CTL as assertions,
or properties, _over_ the object level of the state transition system.

## We can treat `MUST`, `MAY`, and `SHANT` as sugar over a simple `DO`

The purely mechanistic object-level form of a regulative rule is structured like this:

```
    §  clause 1
   IF  preconditions
 UPON  trigger event  -- only used at top-level, otherwise this clause follows from some other clause
PARTY  p
   DO  action
       with  certain criteria
	   to    some recipient
HENCE  clause 2 (... AND clause 4 AND clause 6 OR clause 8)
 LEST  clause 3
```

That `DO` represents an action that party P needs to take or not take;
or, as a special case, procure that someone else take. So `PROCURE`
could also be a special action with its own keyword? Or not.

We can replace the `DO` with the following operators. Each one differs
in that, if the `HENCE` or `LEST` elements are omitted from the
stanza, default values are interpolated.

| deontic modal | if the action | default `HENCE` | if the action | default `LEST` |
| ------------- | ------------- | --------------- | ------------- | -------------- |
| DO            | is taken      | required        | is not taken  | required       |
| MUST          | is taken      | FULFILLED       | is not taken  | BREACH         |
| MAY           | is taken      | FULFILLED       | is not taken  | FULFILLED      |
| SHANT         | is not taken  | FULFILLED       | is taken      | BREACH         |

## Is UPON sufficient or do we need a WHENCE?

L4's state transition model is oriented around the regulative clause, typically structured `PARTY p MUST Action BEFORE D HENCE c1 LEST c2`.

The `c1` and `c2` are themselves clauses, or combinations of clauses `c1a AND c1b`.

However we may want to dip into a State monad that allows us to update certain variables.

```l4
§ 1 abuse of dogs
 GIVEN p IS A Person
       d IS A Dog
 EVERY p
 SHANT abuse d
  LEST `an offence is committed`
       PUT p.points = p.points + 3

§ 2 abuse of cats
 GIVEN p IS A Person
       c IS A Cat
 EVERY p
 SHANT abuse c
  LEST `an offence is committed`
       PUT p.points = p.points + 2

§ 9 penalties
 UPON `an offence is committed` `under`  § 1
                                         § 2
 PARTY p
  MUST  `go to jail` "for"
          SUM p.points
	    "years"
```

## Introspection

We have previously suggested that a decision function have access to its call stack.

What if we allow regulative clauses to examine the history trace so far?

We could say "if we got here via path A, vs if we got here via path B".

```
§ 9 penalties
   UPON `an offence is committed` `under`  § 1
                                           § 2
   CONSIDER history
     WHENCE § 1 abuse of dogs THEN p.points += 3
            § 2 abuse of cats THEN p.points += 2
```

## Homoiconicity, Introspection, Reflection, Reification

Annoyingly, some rules are phrased in a way that blurs the boundary
between object level and assertion level: "If any rule in this section
would cause undue hardship as a result of tight deadlines, the
Commissioner may, upon application, extend deadlines at his
discretion."

The notion of a "tight deadline" is something that could be evaluated
at the assertion level: is it true that every path that leads to
compliance with this rule, involves at least one tight deadline,
defined as a situation where an obligation arises less than five
working days before it needs to be met?"

This is the sort of question we can phrase in LTL/CTL.

But the rule then wants to introspect that property at the object
level, in something like reflection or reification. Hoo boy.

## Textual Homoiconicity

[Section 8 of the Nationality and Borders Act 2022 (UK)](https://www.legislation.gov.uk/ukpga/2022/36/section/8) inserts a section 4L into the British Nationality Act 1981.

This is basically a Git commit, but the textual change is itself recorded in the form of a legal rule -- what we might call an "outer" legal rule.

The "inner" legal rule is a good example of homoiconicity:

> For the purposes of subsection (1)(a), “historical legislative unfairness” includes circumstances where P would have become, or would not have ceased to be, a British subject, a citizen of the United Kingdom and Colonies or a British citizen, if an Act of Parliament or subordinate legislation (within the meaning of the Interpretation Act 1978) had, for the purposes of determining a person’s nationality status—

> a. treated males and females equally,

> b. treated children of unmarried couples in the same way as children of married couples, or

> c. treated children of couples where the mother was married to someone other than the natural father in the same way as children of couples where the mother was married to the natural father.

These propositions could, in theory, be evaluated by a reasoner engine, operating purely against syntax.
