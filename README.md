# L4 with IDE

An implementation of the L4 language for law, with an emphasis on IDE extensions for Visual Studio Code.

This repository includes language examples, a compiler toolchain, a visualizer, and integrations with VS Code.

## Gallery

Syntax Highlighting for Boolean-oriented decision logic

![Syntax Highlighting Example](./doc/images/bna-code.png)

Decision Logic Visualization as a ladder diagram circuit

![Syntax Highlighting Example](./doc/images/bna-viz.png)

## Status (25 Feb 2025)

- ✅ Basic language features (functional core, layout parsing)
- ✅ Basic IDE features (highlighting, CHECK, EVAL)
- ✅ Decision logic visualizer (as ladder diagram)
- 🚧 resource libraries for Date, Place, Entity, Currency
- 🚧 Language support for state transition modals
- 🚧 State transition logic visualizer
- 🚧 Web App Auto-Generation for Decision Logic
- 🚧 Web App Auto-Generation for State Transition Logic, abductive reasonign, planning problems
- 🚧 Formal Verification Tooling, enhanced compiler error messages and warnings

## Download, Install and Build

[Web Demo](https://jl4.well-typed.com): for legal engineers to experiment with writing L4 code. Click the link and get started.

[Dev Build](Dev.md): for Haskell and JS developers to improve the toolchain and IDE developer experience. Requires Haskell and Typscript.

[Quickstart for a local build](Quickstart.md): for legal engineers to experiment with writing L4 code locally. Download the VS Code extension and get started.

## Language Tutorials

All this documentation is still under construction.

- Tutorial: Hello, World

  - Must Sing
  - Vermin

- Tutorial: British Nationality Act

  - References and Legal Citations
  - Temporals

- [Tutorial](doc/apps.md): Automatically Building User-Facing Apps

- Tutorial: Automatically Generating an AI Chatbot

- Tutorial: Contracts involving Parties, Obligations, and Deadlines

  - visualizing a contract
  - abductive reasoning and planning problems

- Tutorial: Automatically Finding Loopholes
  - underspecification, vagueness, and ambiguity

## Language Reference

- Decision Logic

- State Transitions

- Functional and Logical Paradigms

- Source Citations and Versioning

- Multi-Temporal Logic

  - a remark on "shall" as an unfortunate consequence of negotiation-time vs run-time interpretation

- Operationalization to Web and Mobile Apps

- Natural Language Generation: exporting to Word and PDF
