# AGENTS.md

Coding guidelines for AI agents working in this repository.

## About L4

**L4 is a domain-specific programming language for law.** It formalizes legal rules and contracts as executable specifications.

**Design Philosophy:**

- Layout-sensitive syntax (like Python)
- Strongly typed with algebraic data types (inspired by Haskell)
- Isomorphic to legal text structure

## Repository Structure

**Dual-stack monorepo:**

### Haskell (Cabal)

| Package                 | Purpose                                        |
| ----------------------- | ---------------------------------------------- |
| `jl4-core/`             | Core language (parser, typechecker, evaluator) |
| `jl4/`                  | CLI tool and JSON schema generator             |
| `jl4-lsp/`              | Language Server Protocol for IDE support       |
| `jl4-repl/`             | Interactive REPL                               |
| `jl4-decision-service/` | REST API for decision evaluation               |
| `jl4-websessions/`      | Session persistence service                    |
| `jl4-query-plan/`       | Query planning utilities                       |

### TypeScript (npm workspaces + Turborepo)

| Package            | Purpose                                    |
| ------------------ | ------------------------------------------ |
| `ts-apps/vscode/`  | VS Code extension                          |
| `ts-apps/jl4-web/` | Web-based editor (Svelte)                  |
| `ts-shared/`       | Shared libraries (RPC client, visualizers) |

### Documentation

| Location      | Content                                                             |
| ------------- | ------------------------------------------------------------------- |
| `doc/`        | L4 language documentation (reference, courses, tutorials, concepts) |
| `specs/`      | Development specifications (todo, done, proposals, roadmap)         |
| `*/README.md` | Component-specific setup and usage                                  |

## Essential Commands

```bash
# Build
cabal build all          # Haskell
npm run build            # TypeScript

# Test (REQUIRED before commits)
cabal test all           # Haskell tests
npm test                 # TypeScript tests

# Format (REQUIRED before commits)
npm run format           # TypeScript formatting

# Run tools
cabal run jl4-cli -- file.l4
cabal run jl4-repl -- file.l4
```

## Pre-Commit Checklist

**Always run before committing:**

```bash
cabal test all && npm ci && npm run format
```

CI will reject unformatted TypeScript code.

## Git Workflow

### Never Commit to Main

Always create a feature branch:

```bash
git checkout -b feature-name
# Make changes
# Run tests and format
git push -u origin feature-name
# Create PR
```

### After Creating PR

1. Wait ~10 minutes for CI
2. Check for reviewer comments and CI failures
3. Fix issues immediately without waiting for input
4. Merge when green and approved

## Testing

### Golden Files

- First run creates golden file (test fails)
- Second run validates against golden file (should pass)
- To update: delete golden files, run tests twice

### Test Locations

- `jl4/ok/` - Files that should succeed
- `jl4/not-ok/` - Files that should fail
- `jl4/experiments/` - Real-world examples

### Filtering Tests

```bash
cabal test jl4-test --test-options='--match "pattern"'
```

## Adding Features

1. Write spec in `specs/todo/FEATURE-NAME-SPEC.md`
2. Add tests in `jl4/ok/` or `jl4/not-ok/`
3. Implement in relevant `L4.*` modules
4. Run `cabal test all` twice (golden files)
5. Move spec to `specs/done/`
6. Update `doc/reference/` if adding keywords/types/operators
7. Validate generated L4 code with CLI or MCP to be valid!

## L4 Language Pipeline

```
Source (.l4) → Parser → AST → Desugarer → Type Checker → Evaluator → Results
```

**Key modules in `jl4-core/src/L4/`:**

- `Parser.hs` - Layout-sensitive parsing
- `Syntax.hs` - AST definitions
- `Desugar.hs` - Mixfix resolution, sugar expansion
- `TypeCheck.hs` - Bidirectional type checking
- `EvaluateLazy.hs` - Lazy evaluation with traces

## Shell Quoting

**Backticks are meaningful in L4** (quoted identifiers) but trigger command substitution in bash/zsh:

```bash
rg 'foo `bar`'     # Good - single quotes
rg "foo \`bar\`"   # Good - escaped
rg foo `bar`       # BAD - executes bar command
```

## Package Manager

- Use `npm ci` for regular development (reproducible, faster)
- Only use `npm install` when adding/updating packages

## Requirements

- **Haskell:** GHC 9.10.2, Cabal 3.10+ (via GHCup)
- **Node.js:** >= 20
- **GraphViz:** `dot` for trace visualization

**Nix users:** `nix-shell nix/shell.nix` provides all dependencies.

## Writing Documentation

### Documentation Structure

The `doc/` folder has four distinct sections with different purposes:

| Folder           | Purpose                                                        | Audience                            |
| ---------------- | -------------------------------------------------------------- | ----------------------------------- |
| `doc/reference/` | Precise technical specs for keywords, types, operators, syntax | Developers needing exact details    |
| `doc/courses/`   | Structured learning paths with modules                         | Learners studying L4 systematically |
| `doc/tutorials/` | Task-oriented guides ("How do I do X?")                        | Users with specific goals           |
| `doc/concepts/`  | Explanations of ideas and design decisions                     | Anyone wanting to understand "why"  |

### Important Index Files

Always keep these files accurate and up-to-date:

- `doc/README.md` - Main documentation entry point
- `doc/reference/GLOSSARY.md` - Master index of all language features
- `doc/*/README.md` - Section overviews with navigation
- `doc/*/SUMMARY.md` - Table of contents for mdBook rendering

### L4 Code in Documentation

**Prefer separate `.l4` files over inline code blocks:**

````markdown
<!-- ✅ Good: Link to validated file -->

**Example:** [eligibility-example.l4](eligibility-example.l4)

<!-- ❌ Avoid: Inline code that can't be validated -->

```l4
DECIDE `is eligible` IF ...
```
````

````

**All L4 code must be valid.** Use the MCP validator or CLI:

```bash
cabal run jl4-cli -- path/to/file.l4
````

### Writing L4 for Legal Audiences

L4's target users are **policy writers and legal authors**, not programmers. Write code that reads like natural language:

```l4
-- ✅ Good: Reads like legal text
GIVEN person IS A Person
GIVETH A BOOLEAN
DECIDE `the person is eligible for benefits` IF
    `the person is a citizen`
    AND `the person has resided for at least 5 years`
    AND NOT `the person has been disqualified`

-- ❌ Poor: Reads like programmer code
GIVEN p IS A Person
GIVETH A BOOLEAN
isEligible p MEANS p's citizen && p's years >= 5 && !p's disqualified
```

**Use backtick identifiers liberally:**

- `` `the applicant` `` not `applicant`
- `` `has valid identification` `` not `hasValidID`
- `` `the person must not sell alcohol` `` not `alcoholSaleProhibited`

### Validating Documentation

**Always run after documentation changes:**

```bash
./doc/test-docs.sh
```

This script:

1. Checks all markdown links are valid (no broken references)
2. Validates all `.l4` files in `doc/` compile without errors
3. Detects orphaned files (`.md` and `.l4` files not linked from anywhere)

**Fix all errors before committing.** Common issues:

- Links to non-existent files (check paths carefully)
- Links to planned-but-not-written pages (use "Coming soon" text instead)
- Invalid L4 syntax in example files
- Orphaned files that need to be linked from a tutorial or reference page

> Note: README.md and SUMMARY.md files are exempt from the orphan check since they are index files.

### Cross-Linking Guidelines

- Link to existing pages only (run `./doc/test-docs.sh` to verify)
- Use relative paths: `../concepts/README.md` not absolute paths
- Reference the GLOSSARY for keyword/type lookups
- Each page should link to related content in other sections

### Documentation Checklist

When adding or modifying documentation:

1. [ ] Place content in the correct section (reference/courses/tutorials/concepts)
2. [ ] Create `.l4` example files, not inline code blocks
3. [ ] Validate L4 files: `cabal run jl4-cli -- file.l4` or use MCP
4. [ ] Use backtick identifiers that read like natural language
5. [ ] Update relevant README.md and SUMMARY.md files
6. [ ] Update GLOSSARY.md if adding new language features
7. [ ] Run `./doc/test-docs.sh` and fix all errors
8. [ ] Verify links point to existing files only
