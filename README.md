# L4 with IDE

An implementation of the L4 language for law, with an emphasis on IDE extensions for Visual Studio Code, and a web-only version of the L4 editor.

This repository includes language examples, a compiler toolchain, a visualizer, and integrations with VS Code.

## Documentation

### For Developers

- **[dev-config.md](./dev-config.md)** - Local development guide (running with `cabal`)
- **[dev-start.sh](./dev-start.sh)** - Helper script for starting services locally

### For DevOps

- **[PROVISIONING.md](./PROVISIONING.md)** - Setting up new servers from scratch (nixos-anywhere)
- **[DEPLOYMENT.md](./DEPLOYMENT.md)** - Deploying to existing dev/prod servers
- **[nix/README.md](./nix/README.md)** - NixOS configuration reference

## Gallery

Syntax Highlighting for Boolean-oriented decision logic

![Syntax Highlighting Example](./doc/images/bna-code.png)

Decision Logic Visualization as a ladder diagram circuit

![Syntax Highlighting Example](./doc/images/bna-viz.png)

## Trace Visualization

Curious how an L4 decision actually unfolded? Every tool in this repo can show you the evaluation trace as a GraphViz diagram:

- **CLI:** `jl4-cli --graphviz myfile.l4 > trace.dot`, then render with `dot -Tsvg trace.dot > trace.svg`.
- **REPL:** Load a file, run `:trace your expression`, or turn on `:tracefile traces/session` to capture numbered `.dot` files with timestamps, imports, and the final result right in the header.
- **Decision Service:** Call `POST /functions/<name>/evaluation?trace=full&graphviz=true` to get a `graphviz` object containing the DOT plus relative PNG/SVG URLs, or hit `/evaluation/trace.png` / `/trace.svg` directly for ready-to-share images. Batch requests set `graphviz=true` to attach an `@graphviz` blob (same `{dot,png,svg}` shape) per case.

These traces act like explainability receipts for deterministic logic: you can follow each node from the top-level question down to the exact condition or branch that determined the answer. Install GraphViz (`brew install graphviz` or `apt-get install graphviz`) to unlock the PNG/SVG outputs.

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

## The Web Editor

An experimental prototype offers a lightweight web-based alternative to VS Code.

https://jl4.legalese.com/

## VS Code Extension: Download, Install and Build

[Dev Build](Dev.md): for Haskell and JS developers to improve the toolchain and IDE developer experience. Requires Haskell and Typscript.

[Quickstart for a local build](Quickstart.md): for legal engineers to experiment with writing L4 code locally. Download the VS Code extension and get started.

## Language Tutorials

All this documentation is still under construction.

- Tutorial: Hello, World

  - The `DECIDE` stanza
  - The `ASSUME` environment
  - Term Substitution
  - Example: Must Sing
  - Example: [The Dog Act](jl4/experiments/dogs.l4)
  - Example: Vermin

- Tutorial: British Nationality Act

  - [Default Logic](./doc/default-logic.md) and `Optional` types with the `Maybe` monad
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
