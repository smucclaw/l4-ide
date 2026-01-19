# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## About L4

**L4 is a domain-specific programming language for law.** It treats legal rules and contracts as executable specifications, bringing software engineering rigor (IDEs, compilers, debuggers, test suites, formal verification) to legal drafting and contract analysis.

L4 enables:

- Formalizing legal rules with mathematical precision
- Testing contracts against scenarios before deployment
- Finding logical contradictions and loopholes automatically
- Generating user-facing web applications from legal specifications
- Explaining decisions with audit-grade evaluation traces
- Integrating with enterprise systems via REST APIs

**Design Philosophy:**

- Layout-sensitive syntax (like Python) for approachability
- Strongly typed with algebraic data types (inspired by Haskell)
- Isomorphic to legal text (closely follows original document structure)
- Machine-written, human-reviewed (LLMs can ingest legal documents into L4)

## Repository Structure

**Typical setup:** Developers use git worktrees, so the working directory is typically one level below `src/smucclaw/l4-ide/` (e.g., `src/smucclaw/l4-ide/main/` for the main branch, `src/smucclaw/l4-ide/fix-deployment/` for a feature branch).

This is a **dual-stack monorepo**:

### Haskell Stack (Cabal Multi-Package Project)

- **`jl4-core/`** - Core L4 language implementation (parser, typechecker, evaluator)
  - `src/L4/Parser.hs` - Layout-sensitive parser
  - `src/L4/TypeCheck.hs` - Bidirectional type checker
  - `src/L4/EvaluateLazy.hs` - Lazy evaluation with trace generation
  - `libraries/` - Standard library modules (.l4 files: currency, jurisdiction, temporal-prelude, etc.)
- **`jl4/`** - CLI tool (`jl4-cli`) and JSON schema generator (`jl4-schema`)
- **`jl4-lsp/`** - Language Server Protocol implementation for IDE support
- **`jl4-repl/`** - Interactive Read-Eval-Print Loop
- **`jl4-decision-service/`** - REST API for decision evaluation (Swagger/OpenAPI)
- **`jl4-websessions/`** - Session persistence service (SQLite CRUD)
- **`jl4-query-plan/`** - Query planning utilities for elicitation

**Cabal packages:** Listed in `cabal.project` - all build together with `cabal build all`

### TypeScript Stack (Turborepo + npm Workspaces)

- **`ts-apps/vscode/`** - VS Code extension
- **`ts-apps/jl4-web/`** - Web-based editor (Svelte)
- **`ts-apps/webview/`** - Webview components
- **`ts-shared/`** - Shared TypeScript libraries
  - `jl4-client-rpc/` - RPC client for LSP/decision service
  - `l4-ladder-visualizer/` - Decision logic ladder diagram visualization
  - `viz-expr/` - Expression visualization
  - `boolean-analysis/` - Boolean minimization utilities

**Package management:** Uses npm workspaces + Turborepo for incremental builds

## Git Workflow & Branch Conventions

### Feature Branch Worktrees

**Standard convention:** Create feature branch worktrees under `l4-ide/`. For example:

```
src/smucclaw/l4-ide/
├── main/                    # main branch worktree
├── fix-deployment/          # feature branch worktree
├── mengwong/gen-web-app/    # another feature branch
└── ...
```

To create a new feature branch worktree:

```bash
cd src/smucclaw/l4-ide/main
git worktree add ../my-feature -b my-feature
cd ../my-feature
```

### Never Commit Directly to Main

**IMPORTANT:** If you find yourself working on the `main` branch, do NOT commit directly or use admin privileges to bypass branch protection. Instead:

1. Create a new branch: `git checkout -b feature-name`
2. Make your commits on that branch
3. Push and create a PR as usual

This ensures all changes go through code review and CI.

### Pre-Commit and Pre-PR Checklist

**Before every commit**, run:

```bash
cabal test all && npm ci && npm run format
```

This ensures:

- All Haskell tests pass
- TypeScript dependencies are in sync
- All TypeScript/frontend code is formatted (CI will reject unformatted code)

### After Creating a PR

1. Push your branch and create the PR
2. **Wait ~10 minutes** for CI to complete
3. Check the PR for:
   - Reviewer comments
   - CI status (green checks vs red failures)
4. **If there are comments or CI failures:** Proceed to fix them immediately without waiting for user input. Address all feedback, push fixes, and verify CI passes.

### PR Workflow Summary

```
1. Create feature branch worktree
2. Make changes
3. Run: cabal test all && npm ci && npm run format
4. Commit and push
5. Create PR
6. Wait 10 min, check CI + comments
7. Fix any issues immediately
8. Merge when green and approved
```

## Common Commands

### Initial Setup

```bash
# Install TypeScript dependencies (use ci for reproducible builds)
npm ci && npm run build

# Install Haskell toolchain (GHCup required)
cabal update
cabal install exe:jl4-lsp --overwrite-policy=always

# Ensure ~/.cabal/bin/ is on PATH
```

**Important:** Use `npm ci` (not `npm install`) for regular development - it's faster, matches CI, and never modifies package-lock.json.

### Building

```bash
# Build all Haskell packages
cabal build all

# Build TypeScript packages (uses Turborepo)
npm run build

# Build specific Haskell package
cabal build jl4-core
```

### Testing

```bash
# Run all Haskell tests (ALWAYS run before committing)
cabal test all

# Run specific test suite
cabal test jl4-test

# Filter tests by pattern (faster iteration)
cabal test jl4-test --test-options='--match "tdnr|type-coercion"'

# Run TypeScript tests
npm test
```

**Test-Driven Development:** Write tests first when adding features. The test suite uses golden files for snapshot testing - first run creates golden files (and fails), second run validates against them.

### Running Tools

```bash
# CLI tool - evaluate L4 file
cabal run jl4-cli -- path/to/file.l4

# CLI with GraphViz trace visualization
cabal run jl4-cli -- --graphviz myfile.l4 > trace.dot
dot -Tsvg trace.dot > trace.svg

# REPL - interactive exploration
cabal run jl4-repl -- path/to/file.l4

# REPL commands:
# :load <file> / :reload - load/reload file
# :type <expr> - show inferred type
# :decides - list DECIDE declarations
# :qp <decide> [k=v ...] - query planning with bindings
# :trace <expr> - show GraphViz evaluation trace
# :tracefile <path|off> - save traces to files

# JSON Schema generator
cabal run jl4-schema -- path/to/file.l4
```

### Local Development (3 Services)

Use `./dev-start.sh` helper script or run manually:

```bash
# Terminal 1: Decision service
cd jl4-decision-service
cabal run jl4-decision-service-exe -- --port 8001 \
  --sourcePaths ../jl4/experiments/britishcitizen5.l4 \
  --crudServerName localhost --crudServerPort 8002

# Terminal 2: Websessions (after decision service starts)
cd jl4-websessions
cabal run jl4-websessions -- 8002 /tmp/sessions.db http://localhost:8001

# Terminal 3: Web frontend
cd ts-apps/jl4-web
npm run dev
```

Then open http://localhost:5173

**Helper script modes:**

- `./dev-start.sh full` - Show commands for all services
- `./dev-start.sh decision-only` - Just decision service
- `./dev-start.sh websessions-with-push` - Websessions with push enabled

### Formatting and Linting

```bash
# Format all TypeScript code
npm run format

# Check formatting without changes
npm run format:check

# Lint TypeScript
npm run lint
```

## Pre-Commit Checklist

**IMPORTANT: Run these checks before every commit to avoid CI failures.**

### For TypeScript/frontend changes:

```bash
npm ci && npm run format   # REQUIRED - CI will reject unformatted code
npm run lint               # Recommended - catch issues early
```

### For Haskell changes:

```bash
cabal test all             # REQUIRED - run full test suite
```

### For mixed changes (both TypeScript and Haskell):

```bash
npm ci && npm run format && cabal test all
```

**Why this matters:** CI runs `npm run format:check` which fails if any files aren't formatted. Running `npm run format` locally before committing prevents these failures.

## Architecture Overview

### L4 Language Pipeline

```
Source (.l4) → Parser → AST → Desugarer → Type Checker → Evaluator → Results
                                  ↓                           ↓
                            Mixfix Resolution          Trace Generation
```

**Key modules:**

- `L4.Parser` - Layout-sensitive parsing (indentation-based)
- `L4.Syntax` - AST definitions
- `L4.Desugar` - Mixfix operator resolution, syntactic sugar expansion
- `L4.TypeCheck` - Bidirectional type checking with TDNR (Type-Directed Name Resolution)
- `L4.EvaluateLazy` - Lazy evaluation with optional trace collection
- `L4.Export` - Extract `@export` declarations for API exposure
- `L4.JsonSchema` - Generate JSON schemas from type definitions

### Service Architecture (Production)

```
Internet → nginx (443) → /session → jl4-websessions (8002)
                      ↘ /decision → jl4-decision-service (8001)
                                          ↕ localhost
                                    jl4-websessions (8002)
```

Services bind to 0.0.0.0, communicate via localhost, nginx proxies public traffic.

**Development:** All services on localhost without nginx.

### VS Code Extension Flow

```
VS Code Extension → jl4-lsp (WebSocket/stdio) → jl4-core → Type checking + Evaluation
                                                          ↘ Hover info, diagnostics
```

## L4 Language Features

### File Anatomy

- `GIVEN` - Function parameters with types
- `DECLARE` - Enums and record types
- `DECIDE` / `MEANS` / `GIVETH` - Function definitions
- `CONSIDER` - Pattern matching / conditional logic
- `WHEN` - Guards in branches
- `WHERE` / `LET...IN` - Local bindings
- `@export` - Mark functions for API exposure
- `@desc` - Semantic annotations for documentation/hover

### Type System

- Primitive types: `NUMBER`, `STRING`, `BOOLEAN`, `DATE`
- Algebraic types: Records, Enums, Optional (Maybe monad)
- Lists: `LIST OF <type>`
- Ternary logic: `TRUE | FALSE | UNKNOWN`
- Type coercion builtins: `TOSTRING`, `TONUMBER`, `TODATE`, `TRUNC`

### Temporal Logic

Multi-temporal evaluation contexts for time-dependent rules. See `jl4-core/libraries/temporal-prelude.l4`.

### Mixfix Operators

Custom operators with mixed notation (prefix/infix/postfix/closed). Example: `_IS ELIGIBLE FOR_` becomes `person IS ELIGIBLE FOR benefit`.

## Query Planning & Symbolic Evaluation

**Status:** ✅ Complete (as of 2025-11) - See `doc/todo/SYMBEVAL-QUERY-PLANNING-STATUS.md`

### What It Does

When a user provides **partial input** to a boolean decision function, the system performs **symbolic evaluation** to:

1. Determine which parameters are **relevant** given known inputs
2. Return a **prioritized list** of parameters still needed to reach a decision
3. Detect **don't-care** variables that can't affect the outcome
4. Support **iterative refinement** as more information becomes available

This enables conversational interfaces where an LLM chatbot asks only relevant questions rather than requiring all inputs upfront.

### Example Use Case

Consider a legal rule:

```l4
DECIDE `may purchase alcohol` IF
           you are 21+ years old
      AND     you are unmarried
           OR your spouse approved
           OR buying only beer
  OR       you are under 21
      AND  your parent approved
           OR you are legally emancipated
```

If user says they're 30 years old, the system automatically determines:

- **Don't care:** parental approval, legal emancipation (can't affect outcome)
- **Still needed:** marital status, spousal approval, beverage type
- **Ranking:** Which question to ask next based on impact

### Architecture

**Haskell Backend (`jl4-query-plan/`):**

- `L4.Decision.BooleanDecisionQuery` - BDD (Binary Decision Diagram) implementation
  - Compiles boolean expressions into reduced ordered BDDs
  - Supports cofactoring (variable elimination under partial assignments)
  - Detects don't-care variables via BDD analysis
- `L4.Decision.QueryPlan` - Query planning logic
  - Builds `CachedDecisionQuery` with variable dependencies
  - Generates `QueryPlanResponse` with `asks`, `stillNeeded`, `impact`, `dontCare`
  - Computes stable `atomId` (UUIDv5) for frontend caching
  - Tracks provenance: which input parameters does each atom depend on?

**TypeScript Frontend (`ts-shared/`):**

- `boolean-analysis/` - ROBDD (Reduced Ordered BDD) implementation in TypeScript
  - Used when no `App` nodes present (pure boolean logic)
  - Falls back to simpler analysis when App nodes exist
- `l4-ladder-visualizer/src/lib/eval/partial-eval.ts` - Partial evaluation analyzer
  - Computes `PartialEvalAnalysis` (relevance, next questions, don't-care detection)
  - Annotates ladder diagram nodes with relevance status
- `decision-service-types/` - Shared TypeScript types for `/query-plan` API

**Integration Points:**

- **Decision Service API:** `POST /functions/{id}/query-plan` endpoint
  - Accepts bindings: `{"label": {"age": true}, "unique": {42: false}, "atomId": {"uuid...": true}}`
  - Returns: `{asks: [{atoms, label, path, schema}], stillNeeded, impact, inputs, outcome}`
- **REPL:** `:decides` lists functions, `:queryplan` / `:qp` shows ranking under partial bindings
- **Ladder UI:** Highlights "next questions" in diagram, fades irrelevant/short-circuited nodes
- **VSCode/jl4-web:** Auto-upserts current buffer to decision service, queries for next asks

### Key Features (All Implemented ✅)

- **Stable atom IDs:** UUIDv5 derived from function + label + transitive input refs
- **Nested schema exposure:** Record params expose `properties`, arrays expose `items`
- **Provenance tracking:** Each atom includes `inputRefs` (which top-level params it depends on)
- **Cache invalidation:** Query-plan cache self-invalidates on PUT/DELETE
- **Schema-aware ordering:** Asks ordered by declaration field order + numeric indices
- **In-diagram annotations:** Ladder nodes show ask labels + schema summaries (e.g. "Number", "enum(3)")
- **Multi-ask atoms:** Tooltip shows `(+N)` with list of all questions for that atom

### Theory Background

**Three-Valued Logic (Kleene):**

- Extends boolean logic with `UNKNOWN` for partial information
- `False AND Unknown = False` (short-circuit)
- `True AND Unknown = Unknown` (need more info)

**Binary Decision Diagrams (BDDs):**

- Canonical representation of boolean functions
- Ordered variables for efficient operations
- Cofactoring: substitute variable value, simplify
- Don't-care detection: variable doesn't appear in reduced BDD after cofactoring

See `doc/todo/BOOLEAN-MINIMIZATION-SPEC.md` for detailed specification.

### Working with Query Planning Code

**Adding query-plan support to a new component:**

1. Import `@repo/decision-service-types` for TypeScript types
2. Call `/query-plan` endpoint with current bindings
3. Use `elicitationOverrideFromQueryPlan()` to map response to ladder UI
4. Display `asks[0].atoms` as "next questions"
5. Use `asks[*].schema` for form generation/validation

**Debugging query planning:**

```bash
# REPL: Test query planning interactively
cabal run jl4-repl -- file.l4
> :decides
> :qp function_name age=true married=false

# Decision service: Direct API call
curl -X POST "http://localhost:8001/functions/uuid:function/query-plan" \
  -H "Content-Type: application/json" \
  -d '{"label": {"age": true}}'
```

**Related modules:**

- `jl4-decision-service/src/Backend/DecisionQueryPlan.hs` - API endpoint implementation
- `jl4-lsp/src/LSP/L4/Viz/QueryPlan.hs` - LSP query-plan builder (reuses `jl4-query-plan`)
- `ts-shared/l4-ladder-visualizer/src/lib/eval/query-plan-override.ts` - UI mapping logic

## Testing Strategy

### Golden File Testing

Test outputs are captured in `.golden/` directories. When adding new tests:

1. First run creates golden file (test fails: "failFirstTime is set to True")
2. Second run compares against golden file (should pass)
3. To update: delete golden files and re-run twice

### Test Files Location

- **Passing tests:** `jl4/ok/` - Files that should parse, typecheck, evaluate successfully
- **Failing tests:** `jl4/not-ok/` - Files that should fail (parse errors, type errors)
- **Experiments:** `jl4/experiments/` - Real-world examples (parking, citizenship, etc.)

### Test Performance

The test suite is comprehensive but slow (~hundreds of examples). For faster iteration:

```bash
# Filter to specific tests
cabal test jl4-test --test-options='--match "tdnr"'

# Pipe to file for easier analysis
cabal test all 2>&1 | tee /tmp/test-output.txt
grep -i "fail" /tmp/test-output.txt
```

## Documentation

### For AI/LLM Context

- **`AGENTS.md`** - AI agent development notes, design principles, testing guide
- **`CLAUDE.md`** (this file) - Repository guide for Claude Code

### For Developers

- **`README.md`** - High-level overview, real-world impact, status
- **`doc/dev/setup.md`** - Developer setup (Haskell, TypeScript, GraphViz)
- **`doc/dev/local-config.md`** - Running services locally
- **`doc/dev/deployment/`** - NixOS provisioning and deployment

### For Learning L4

- **`doc/foundation-course-ai/`** - Complete introduction (Modules 0-6)
- **`doc/advanced-course-ai/`** - Production-grade development (Modules A1-A11)
- **`doc/README.md`** - Language reference and theoretical foundations

### Specifications

- **`doc/todo/`** - Planned features and in-progress tasks
- **`doc/done/`** - Completed specifications for reference

**Always update docs alongside code changes.** Move completed specs from `todo/` to `done/`.

## Common Development Workflows

### Adding a New Language Feature

1. Write specification in `doc/todo/FEATURE-NAME-SPEC.md`
2. Add test files in `jl4/ok/` or `jl4/not-ok/`
3. Update `L4.Syntax` (AST)
4. Update `L4.Parser` (parsing)
5. Update `L4.TypeCheck` (type checking)
6. Update `L4.EvaluateLazy` (evaluation)
7. Update `L4.Print` (pretty printing)
8. Run `cabal test all` twice (golden files)
9. Move spec to `doc/done/` when complete

### Debugging Type Errors

```bash
# Use REPL to check types interactively
cabal run jl4-repl -- problematic-file.l4
> :type some_expression
> :info function_name
```

### Viewing Evaluation Traces

```bash
# CLI with GraphViz
cabal run jl4-cli -- --graphviz file.l4 > trace.dot
dot -Tsvg trace.dot > trace.svg

# REPL interactive tracing
cabal run jl4-repl -- file.l4
> :trace expression
> :tracefile /tmp/traces  # Saves numbered .dot files
```

### Developing the VS Code Extension

```bash
# Build TypeScript + install LSP
npm ci && npm run build
cabal install exe:jl4-lsp --overwrite-policy=always

# Launch VS Code and press F5 to start extension host
code .
# Press F5 in VS Code
```

Open `jl4/examples/` folder to test syntax highlighting and language features.

## Important Notes

### Shell Quoting with L4

**Backticks (`` ` ``) are meaningful in L4** (quoted identifiers) but trigger command substitution in bash/zsh. When running shell commands with L4 snippets containing backticks, quote or escape them:

```bash
rg 'foo `bar`'  # Good
rg "foo \`bar\`"  # Good
rg foo `bar`  # BAD - executes bar command
```

### Package Manager Discipline

- **Use `npm ci`** for regular development (reproducible, faster)
- **Only use `npm install`** when adding/updating packages
- If `npm ci` fails, investigate before running `npm install`

### Performance Considerations

- Excel date/temporal tests are slow due to large library imports
- Use test filtering during development: `--test-options='--match "pattern"'`
- Decision service module precompilation gives 10-100x speedup

### Deployment Environments

- **Local:** `cabal run` with command-line args, localhost URLs
- **Dev Server:** NixOS flake target `jl4-dev`, domain `dev.jl4.legalese.com`
- **Production:** NixOS flake target `jl4-aws-2505`, domain `jl4.legalese.com`

**Deploy to dev server** (from local machine via lhs jump host):

```bash
ssh lhs "cd src/smucclaw/l4-ide/main && git pull && nixos-rebuild switch --flake '.#jl4-dev' --target-host root@dev.jl4.legalese.com"
```

The `lhs` host has the NixOS build environment. The `--target-host` flag deploys directly to the dev server as root.

## Requirements

- **Haskell:** GHC 9.10.2, Cabal 3.10+ (via GHCup)
- **Node.js:** >= 20 (via nvm or corepack)
- **GraphViz:** `dot` + `xdot` for trace visualization
- **System libs:** pkg-config, liblzma-dev, libgmp-dev

On macOS: `xcode-select --install && brew install graphviz xdot`
On Debian/Ubuntu: `apt install pkg-config liblzma-dev libgmp-dev graphviz xdot`

**Nix users:** `nix-shell nix/shell.nix` provides all dependencies.
