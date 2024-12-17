
# <Does this project have a title? Should we invent one?>

In the following, we are describing our current vision for the development of the
<...> tool. The features we describe will be developed to different degrees of
polish, and most of them are as of now in the middle of active development. This
is why we are only including descriptions, and sometimes (mockup) screenshots, in
our current report.

On the other hand, we are only including in the current report the parts that we
have started actively working on. Some other features of the proposal are being
left to later. (TODO: check validity of this statement, make it more concrete)

The tool we are planning to build is centered around a two-window view:

- On the left, we edit legislation using a domain-specific language. This language
  is called JL4 (for Jersey-L4) and is a variant of the L4 line of languages previously
  developed by SMU CCLAW.

  The language is reminiscent of natural language, but follows rigid rules and has
  clear and unambiguous semantics.

- On the right, we can obtain various other views of (parts of) the legislation
  text open on the left. In particular, we aim for:

  - A natural language representation; while still stilted in nature, this version
    should nevertheless read as plain English and be understandable to an untrained
    reader.

  - A visualisation of the Boolean decision logic encoded in the legislation as an
    interactive "ladder circuit diagram".

  - A visualisation of the Boolean decision logic encoded in the legislation as a
    questionnaire.

- We are planning to add more information views at later stages, in particular:

  - A visualisation of the state transition system encoded in the legislation as an
    automaton.

In the following, we are describing each of these views and visualisations in more
detail.

## 1K Words

https://docs.google.com/drawings/d/1CaJtpHDnRPQAfC2q3jdXjuXObJu9fN2WrJNfuqawHOk/edit

These deliverables come from
https://docs.google.com/document/d/1hPuBnqTcGm5gicwo06m68GqsP_YSUn8VoZfd7-cbcfI/edit?tab=t.0

``` haskell
[ d
| d <- deliverables
, deadline d ~= "Dec 2024"
]
```

## JL4 window

The contents of the left window are essentially programs in a (domain-specific)
programming language, but we try our best to make such programs pleasant to read
to a relatively untrained eye, and to make the language keywords and concepts
reminiscent of words a legal drafter would be using and attaches meaning to.

In programming language terms, the language features the following concepts:

- A syntax that uses plain English keywords and avoids the use of symbols and
  parentheses in many places in favour of descriptive words and layout.

- A way to declare new concepts (corresponding to datatypes in a programming language).
  In particular, concepts can be enumerations and records.

- A way to define new values. Values can be Boolean, numeric (including e.g.
  monetary amounts), and can be elements of the previously mentioned user-defined
  concepts. Values can be parameterised, i.e., they can depend on other inputs
  that are part of the overall situation.

In a second iteration, the language also features:

- A way to define parties as well as obligations or prohibitions that apply to
  a particular party.

There are further features of the JL4 language:

- Any concept or value can be annotated with natural language phrases that
  explain them further and which will be used in the natural language view.

- Whenever a concept or value is referred to, the tool will check *where* it is
  being defined, and allow to display information about the location of definition
  and the kind of definition it is by hovering over the identifier.

- If a concept or a value is mentioned without having a definition, this will
  be highlighted as an error.

- If a concept or a value is used in a way that doesn't match the nature of
  its definition, this will also be highlighted as an error. (TODO: needs an
  example).

TODO: snippets / refactorings?

TODO: error messages?

## Visualisation window

As described above, the right window of the tool will be able to display various
pieces of additional information about the contents of the left window. This
information can be requested by clicking on particular buttons in the user
interface.

As an approximation, the right window is informational, i.e., while some of the
visualisations are interactive, the results of such interactions do not affect
the contents of the left window.

In the more distant future, it is conceivable that this interactivity of the
visualisations is expanded on, and that we will allow changes to the visualisations
to be made that result in changes to the program.

### Natural language view

### Ladder circuit diagrams

### Questionnaires

### Transition systems

...
