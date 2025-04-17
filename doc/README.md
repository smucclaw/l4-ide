# L4 Language Reference

L4 is designed for isomorphic formalization of the better part of most
legal documents. This includes legislation, regulations, and
contracts. Contract genres to which L4 is suited include insurance
policies, rental leasing, employment agreements, financial agreements
(e.g. loans).

This guide explains both what L4 is and how to use it, using legal
examples and legal idioms as a point of reference. Along the way we
show how L4 borrows ideas from software engineering to improve
drafting and help end-users make sense of legal situations.

## Basic data types: true/false, numbers, strings, and records

Laws and contracts must reduce shades of grey to a black
and white decision. Think "bright line" tests. In L4, **Boolean
logic** is used to work with those black-and-white, Yes/No, True/False
values. These values are combined using the operators `AND`, `OR`,
`NOT`, `ANY`, `ALL`. (See: [Boolean logic](./10-boolean-logic.md))

Besides "yes" or "no" values, one often has to deal with numbers,
strings, enums, or a combination of these basic **types**. L4's **data
modelling** syntax offers familiar record and OOP-class syntax for
representing the real world, backed by static type inference. (See:
[Basic Data Types](./10-data-types.md))

As an enhancement, operators for `XOR`, `AT LEAST N OF`, `EXACTLY N
OF`, and `IMPLICATION` augment the core operator set.

## Unknown values; treating unknowns as false

Sometimes the "yes" or "no" is not known, or does not apply. L4's
**ternary logic** extends Boolean logic with "unknown" or "null"
values to handle those situations. L4 supports a "negation as failure"
mode of evaluation which treats "unknown" values as "false". This is
useful to represent ideas like "innocent until proven guilty". _Is
someone guilty? So far, "unknown"; then for now, we'll take that as a
"no"._ (The general mechanism for representing unknown values works
for Boolean types and other types too. See: [algebraic data types](./30-algebraic-types.md))

## Standard Libraries and the Prelude

L4 also offers **libraries** for notions of person, place, time,
currency, and so on. (See: modules and libraries)

## Default Values: convenient assumptions

When we talk about "persons", we typically assume that we're talking
about natural persons, over the age of majority, with mental capacity.
L4 allows optional **default values** on terms to reflect such
assumptions. L4 can make these assumptions explicit, but also save
end-users the trouble of clicking those checkboxes by hand each time;
the boxes will be pre-filled. (See: [default values](./default-values.md))

## Default Logic: General Cases and Special-Case Exceptions

**Default logic** allows the concise expression of special cases and
default values. Most of the time, weekday rules apply, but weekends
could be a special case. (See: [default logic](./default-logic.md))

## Constitutive Rules as First-Order Predicate Logic

Building on the above foundation, L4 supports **constitutive rules**
by way of **predicate logic**, or **first-order logic**: you can
express **institutional facts** as a **decision function** as to
whether an "X" counts as a "Y" for purposes "Z". To assist in
comprension, L4 offers a visualizer for decision logic. (See:
[functions](./25-functions.md))

## Level 10 Unlocked! Basic Web Wizards, SAT/SMT Verification

The above feature set is sufficient to deliver the following classes of functionality:

- automated generation of interactive visualizations of the decision logic

- automated generation of user-facing decision applications
  - typically, a web wizard form which engages end-user for input
  
- automated generation of reasoner backend
  - typically exposed as an API endpoint
  - given inputs and a ruleset, returns a decision verdict
  - with an explanation trace necesary for audit trails

- a chatbot UI which wraps the decision engine in an oral user interface
  - uses the explanation trace to answer user questions

- SAT/SMT optimization and counterfactual synthesis
  - calling a back-end solver/reasoner powered by a SAT/SMT solver e.g. Z3, PySAT
  - answers queries like "what inputs would have to change for the answer to be Yes instead of No"?

- automated detection/prevention of syntactic and referential ambiguities

## Temporal Logic of Databases

As many decisions involve time, L4 offers syntactic sugar to express
ideas like, "according to the version of the **legislation in effect
at time L**, as **the facts were known at time F** and as **the law
was understood at time U**, would a baby **born at time B** have been
considered a British citizen?" (See: [temporals](./multitemporals.md))

## Regulative Rules as a Labeled State Transition System

Contracts involve multiple parties who perform certain actions, like
making payment and delivering goods, by certain deadlines. For every
"happy path", there is the possibility of failure. Well-written
contracts describe both the desired scenarios and the mechanisms of
reparation. L4 uses **labeled transition systems** to represent these
moving parts. Conversion to Petri Nets, Non-deterministic Finite
Automata, finite state machines, BPMN diagrams, and the like are
supported by the L4 toolchain. (See: [regulative rules](./regulative-proposal.org))

Legislation and regulation can be thought of a special kind of rule
where every qualifying person is a party to the rules. (See: [the `WHO` keyword](./who.md))

## Deontics as Property Assertions About An LTS

Legal writing frequently uses modals like "must" and "may", in deontic
and non-deontic senses (typically, alethic, but also qualifying).
Legal writers frequently disagree about the use of "shall". L4
resolves these difficulties by introducing the notion of
**property-level specification**, from the world of formal verification
and temporal logic, and the notion of "bounded deontics" which makes
explicit the idea of "or else what?" (See: deontics over regulatives)

The temporal logics involved in model checking are different from the
version-oriented temporal logics of databases introduced above.

## Example translations of regulative rules

See [Regulative Examples](./regulative-examples.md)

## Level 20 Unlocked! Planning and Abductive Reasoning

The above feature set is sufficient to deliver the following classes of functionality:

- planning problems
  - "given my current situation and a desired outcome, how do I get there from here?"
  - e.g. OptaPlanner / Timefold, OR-Tools, gurobi

- bounded deontic property queries
  - if I want to avoid paying a fine, must I file paper P before deadline D?
  - must I do Obligation O1?
    - Only if you want to avoid consequence P1.
    - No, you can achieve avoidance of consequence P1 by performing Alternative O2.

- Recognizing and Designing Unwinnable States
  - if the meta-goal is to design a policy set which gives the appearance of satisfiability but is in practice "unwinnable", applications can be developed to assist with such constructions.
  - dually, L4 allows the automated detection of such unwinnable policies

## Sections and Scopes

Legal texts are structured into act, chapter, part, section,
paragraph, sub-paragraph, and so on. **Defined terms** are **scoped**
according to those structures. L4 offers mechanisms for representing
writing like, "for the purposes of **sections** 1 and 3, a tomato means
..." (See: [scope](./scope.md))

## Meta-Rules Establish a Partial Priority Ordering

Scopes often interact according to notions of priority and
transformation. "**Notwithstanding**" and "**despite**" are used in legal
writing, and in L4, to establish priorities between sections. One
section can also be "**subject to**" another. This relation is
surprisingly complex to unpack. (See: [meta-rules](./modifiers.md))

## Syntax for Decision Tables

**Decision tables** are a syntactic form borrowed from the DMN
standard to complement the more frequently seen **decision tree**.

## Automated Tests

**Tests** are a mechanism from the world of programming which help
legal drafters quickly gain confidence that drafting and negotiation
activity have not inadvertently broken anything. (See: tests)

## Level 30 Unlocked! Integration With Legacy Enterprise Systems

The above feature set is sufficient to deliver the following classes of functionality:

- backward compatibility with the majority of existing natural
  language legal texts, including contracts, legislations, and
  regulations

- automated detection of lexical and scope ambiguities

## Roadmap for future development

Roadmap: see the Github Issues and the [Future Features](./future-features.md) document.

## IDE support

Legal drafters may also appreciate VS Code's native "jump to definition" and "jump to references" features, available with a right-click on an expression of interest.

## See Also

### Design Principles

- [Design Principles](./principles.md)

### Related Languages

- AustLii / DataLex
- Blawx
- CSL
- Catala
- FormaLex
- Logical English / Logical Contracts
- SCL
- Stipula
- Symboleo
