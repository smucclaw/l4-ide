# L4 Language Reference

## Quickstart and Installation



## Demo of Features

### Integrated Development Environment

### Visualizers

The decision logic visualizer 

### Natural Language Generator

### 

## Semantics

The expressive domain of L4 covers the majority of current legal documents.

## Basic data types: true/false, numbers, strings, and records

The operation of law frequently involves the reduction of a complex
world, with many shades of grey, to a black and white decision.
"Bright line" tests do this. In L4, **Boolean logic** is used to work
with Yes/No values. These values are combined using the operators
`AND`, `OR`, `NOT`, `ANY`, `ALL`. (See: Boolean logic)

Besides "yes" or "no" values, one often has to deal with numbers, or
strings, or enums, or combinations of these **types**. L4's **data
modelling** offers familiar record and OOP-class syntax for
representing reality, backed by static type inference. (See: data types)

Sometimes the "yes" or "no" is not known, or does not apply. L4's
**ternary logic** extends Boolean logic with "unknown" or "null"
values to handle those situations. L4 supports a "negation as failure"
mode of evaluation which treats "unknown" values as "false". This is
useful to represent ideas like "innocent until proven guilty". *Is
someone guilty? So far, "unknown"; then for now, we'll take that as a
"no".* (The general mechanism for representing unknown values works
for Boolean types and other types too. See: algebraic data types)

L4 also offers **libraries** for notions of person, place, time,
currency, and so on. (See: modules and libraries)

When we talk about "persons", we typically assume that we're talking
about natural persons, over the age of majority, with mental capacity.
L4 allows optional **default values** on terms to reflect such
assumptions. L4 can make these assumptions explicit, but also save
end-users the trouble of clicking those checkboxes by hand each time;
the boxes will be pre-filled. (See: default values)

**Default logic** allows the concise expression of special cases and
default values. Most of the time, weekday rules apply, but weekends
could be a special case. (See: default logic)

Building on the above foundation, L4 supports **constitutive rules**
by way of **predicate logic**, or **first-order logic**: you can
express **institutional facts** as a **decision** as to whether an "X"
counts as a "Y" for purposes "Z". To assist in comprension, L4 offers
a visualizer for decision logic. (See: decision logic)

As many decisions involve time, L4 offers syntactic sugar to express
ideas like, "according to the version of the **legislation in effect
at time L**, as **the facts were known at time F** and as **the law
was understood at time U**, would a baby **born at time B** have been
considered a British citizen?" (See: temporals)

Contracts involve multiple parties who perform certain actions, like
making payment and delivering goods, by certain deadlines. For every
"happy path", there is the possibility of failure. Well-written
contracts describe both the desired scenarios and the mechanisms of
reparation. L4 uses **labeled transition systems** to represent these
moving parts. Conversion to Petri Nets, Non-deterministic Finite
Automata, finite state machines, BPMN diagrams, and the like are
supported by the L4 toolchain. (See: regulative rules)

Legislation and regulation can be thought of a special kind of rule
where every qualifying person is a party to the rules. (See: the `WHO` keyword)

Legal texts are structured into act, chapter, part, section,
paragraph, sub-paragraph, and so on. **Defined terms** are **scoped**
according to those structures. L4 offers mechanisms for representing
writing like, "for the purposes of sections 1 and 3, a tomato means
..." (See: scope)

Scopes often interact according to notions of priority and
transformation. "**Notwithstanding**" and "**despite**" are used in legal
writing, and in L4, to establish priorities between sections. One
section can also be "**subject to**" another. This relation is
surprisingly complex to unpack. (See: meta-rules)

Legal writing frequently uses modals like "must" and "may", in deontic
and non-deontic senses (typically, alethic, but also qualifying).
Legal writers frequently disagree about the use of "shall". L4
resolves these difficulties by introducing the notion of
**property-level specification**, from the world of formal verification
and temporal logic, and the notion of "bounded deontics" which makes
explicit the idea of "or else what?" (See: deontics over regulatives)

**Decision tables** are a syntactic form borrowed from the DMN
standard to complement the more frequently seen **decision tree**.
(See: decision table syntax)

**Tests** are a mechanism from the world of programming which help
legal drafters quickly gain confidence that drafting and negotiation
activity have not inadvertently broken anything. (See: tests)

## Constitutive Rules using Decision Logic

## Regulative Rules using a Labeled State Transition System

## Deontics as Assertions

## Temporal Logic

## Metaprogramming

## Design Principles



## Related Languages



