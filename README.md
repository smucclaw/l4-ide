# L4 with IDE

An implementation of the L4 language for law, with an emphasis on IDE extensions for Visual Studio Code.

This repository includes language examples, a compiler toolchain, a visualizer, and integrations with VS Code.

## Gallery

Syntax Highlighting for Boolean-oriented decision logic

![Syntax Highlighting Example](./doc/images/doc-screenshot-1.png)

Decision Logic Visualization as a ladder diagram circuit

Annotation mechanism to assist with natural language generation (the yellow bits in square brackets)

## Status (25 Feb 2025)

- WORKING: Basic language features (functional core, layout parsing)
- WORKING: Basic IDE features (highlighting, CHECK, EVAL)
- WORKING: Decision logic visualizer (as ladder diagram)
- ROADMAP: resource libraries for Date, Place, Entity, Currency
- ROADMAP: Language support for state transition modals
- ROADMAP: State transition logic visualizer
- ROADMAP: Web App Auto-Generation for Decision Logic
- ROADMAP: Web App Auto-Generation for State Transition Logic, abductive reasonign, planning problems
- ROADMAP: Formal Verification Tooling, enhanced compiler error messages and warnings

## Download, Install and Build

[Dev Build](Dev.md): for Haskell and JS developers to improve the toolchain and IDE developer experience. Requires Haskell and Typscript.

[Quickstart](Quickstart.md): for legal engineers to experiment with writing L4 code. Download the VS Code extension and get started.

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
