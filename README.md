# L4 with IDE

**L4** is a domain-specific programming language for law. It formalizes legal rules and contracts as executable specifications, bringing software engineering rigor to legal drafting and analysis.

This repository contains the L4 compiler, IDE tooling (VS Code extension, LSP, REPL, web editor), and a decision service that exposes L4 rules as REST APIs and MCP tools.

## What You Get From a Single L4 File

- **REST APIs** — Expose functions as HTTP endpoints with `@export` annotations
- **AI tool integration** — MCP server for Claude, Cursor, and VS Code Copilot; WebMCP for in-browser agents
- **Interactive visualizations** — Ladder diagrams and evaluation traces rendered via GraphViz
- **Audit-grade explainability** — Every evaluation produces a trace you can follow from the top-level question down to the deciding condition
- **Test suites** — Golden-file tests and assertions
- **Generated schemas** — OpenAPI 3.0 specs and JSON schemas for integration

## Getting Started

- **Install the VS Code extension:** [L4 Rules-as-code on the Marketplace](https://marketplace.visualstudio.com/items?itemName=Legalese.l4-vscode)
- **Try the web editor:** <https://jl4.legalese.com/>
- **Learn L4:** [Foundation Course](doc/courses/foundation/README.md) — no prior programming experience required
- **Full documentation:** [doc/README.md](doc/README.md)

## Repository Layout

**Haskell (Cabal):**

| Package                             | Purpose                                        |
| ----------------------------------- | ---------------------------------------------- |
| [jl4-core](jl4-core/)               | Core language (parser, typechecker, evaluator) |
| [jl4](jl4/)                         | CLI tool and JSON schema generator             |
| [jl4-lsp](jl4-lsp/)                 | Language Server Protocol for IDE support       |
| [jl4-repl](jl4-repl/)               | Interactive REPL                               |
| [jl4-service](jl4-service/)         | REST API for decision evaluation               |
| [jl4-websessions](jl4-websessions/) | Session persistence service                    |
| [jl4-query-plan](jl4-query-plan/)   | Query planning utilities                       |
| [jl4-actus-analyzer](jl4-actus-analyzer/) | Static analyzer classifying L4 contracts by ACTUS / FIBO |
| [jl4-wasm](jl4-wasm/)               | WebAssembly build of L4 for browser/Node.js    |

**TypeScript (npm workspaces + Turborepo):**

| Package                             | Purpose                                    |
| ----------------------------------- | ------------------------------------------ |
| [ts-apps/vscode](ts-apps/vscode/)   | VS Code extension                          |
| [ts-apps/jl4-web](ts-apps/jl4-web/) | Web-based editor (Svelte)                  |
| [ts-shared](ts-shared/)             | Shared libraries (RPC client, visualizers) |

**Documentation:** See [doc/](doc/) for the language reference, tutorials, courses, and concept guides.

## Building From Source

```bash
cabal build all          # Haskell
npm ci && npm run build  # TypeScript
```

See [AGENTS.md](AGENTS.md) for repository conventions and workflow, and [dev-start.sh](dev-start.sh) for running services locally.

**Requirements:** GHC 9.10.2, Cabal 3.10+, Node.js ≥ 20, and GraphViz (`dot`). Nix users can run `nix-shell nix/shell.nix` for a ready environment.

## The REPL

An interactive Read-Eval-Print Loop for exploring L4 code:

```bash
cabal run jl4-repl -- path/to/file.l4
```

The REPL provides live evaluation, module reloading (`:load`, `:reload`), query planning (`:decides`, `:queryplan`), and trace output (`:trace`, `:traceascii`, `:tracefile`). See [jl4-repl/README.md](jl4-repl/README.md) for the full command list.

## VS Code Extension

Provides syntax highlighting, type checking, inline evaluation, `@export` previews, deployment management, and an automatically-registered local MCP server that bridges Claude Code, Cursor, and VS Code Copilot to your L4 code. Connects to [Legalese Cloud](https://legalese.cloud) or a self-hosted `jl4-service` instance. See [ts-apps/vscode/README.md](ts-apps/vscode/README.md).

## Trace Visualization

Every tool in this repo can render an L4 evaluation as a GraphViz diagram:

- **CLI:** `jl4-cli --graphviz myfile.l4 > trace.dot`, then `dot -Tsvg trace.dot > trace.svg`
- **REPL:** `:trace <expression>`, or `:tracefile traces/session` to capture numbered `.dot` files
- **Decision Service:** `POST /deployments/{id}/functions/{fn}/evaluation?trace=full&graphviz=true`

Install GraphViz with `brew install graphviz` or `apt-get install graphviz` to render PNG/SVG outputs.

## Application Libraries

L4 ships with foundational libraries for building legal and commercial applications:

- [Jurisdiction](jl4-core/libraries/jurisdiction.l4) — ISO 3166 country codes, US states, Canadian provinces, EU
- [Currency](jl4-core/libraries/currency.l4) — ISO 4217 currency codes with integer minor-unit storage
- [Legal Persons](jl4-core/libraries/legal-persons.l4) — individuals, corporations, partnerships, LLCs, trusts
- [Holdings](jl4-core/libraries/holdings.l4) — ownership structures and beneficial ownership

## Real-World Impact

L4 has been piloted with organizations in both public and private sectors:

- **Government regulatory compliance** — encoded secondary legislation to auto-generate citizen-facing web wizards; formal verification discovered a double-bind where contradictory clauses required and prohibited the same action.
- **Insurance policy analysis** — formalized contracts from major global providers, uncovering payout-formula ambiguities linked to significant claims leakage.
- **Legislative drafting** — working with government drafting offices on rules-as-code initiatives.
- **Commercial agreements** — transformed complex fee schedules and payment terms into L4, served via REST for enterprise integration.

## Community

- **[Discord](https://discord.gg/Q7a7NSEdNy)** — chat with the community
- **[GitHub Issues](https://github.com/smucclaw/l4-ide/issues)** — report bugs, request features
- **[Legalese](https://legalese.com)** — professional implementation services

L4 is published under the [Apache-2.0 License](LICENSE).
