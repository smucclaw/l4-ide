# AI Agent Development Notes for L4 IDE

## About L4

**L4 is a domain-specific language (DSL) for law.** It enables computer-readable formalizations of contracts, legislation, and regulations, translating legal documents into precise, executable code.

### Design Principles

1. **Optimized for editability and learnability** - Most human uses involve tweaking existing text rather than drafting from scratch
2. **Machine-written, human-reviewed** - LLMs can ingest legal documents into L4; humans review and revise
3. **Isomorphic to legal text** - L4 encoding closely follows the original legal document structure and verbiage
4. **Layout-sensitive syntax** - Uses indentation for grouping (like Python), making it approachable for non-programmers
5. **Strongly typed** - Algebraic data types reduce ambiguity; L4 is written in, and inspired by, Haskell and functional programming.
6. **Both specification and programming language** - Can express both "letter of the law" and "spirit of the law"

### Target Users

1. **Legal professionals** - Lawyers and legal drafters (no programming experience required)
2. **Legal engineers** - Those who translate legal text to L4 code
3. **End-users of L4-based applications** - People asking questions like:
   - "Do I qualify? Why/Why not?"
   - "How much do I have to pay/get paid?"
   - "What do I need to do to achieve a goal?"
4. **AI/LLMs** - Copilot can automatically translate legal text into L4
5. **Developers** - Improving the toolchain and IDE experience

### Project Goals

The project aims to answer common legal questions computationally:

- Current state given events
- Immediate obligations
- Paths to achieve/avoid outcomes
- Basis for calculations (audit trails)
- Detect potential loopholes and ambiguities

Key applications include:

- IDE support for legal drafters
- Automated generation of end-user web/mobile apps
- Visualizations (ladder diagrams) for comprehension
- Chatbot interfaces for conversational engagement
- SAT/SMT verification and formal reasoning

---

## Development Guide

### Testing

**We aspire to follow Test-Driven Development (TDD) practices.** This means:

1. **Write tests first** when adding new features - define expected behavior before implementation
2. **Update tests** when modifying existing behavior - tests should reflect the current spec
3. **Add regression tests** when fixing bugs - prevent the same bug from recurring

TDD helps us catch issues early and ensures our code meets specifications before we consider a feature complete.

Before committing changes, always run the full test suite to ensure nothing is broken:

```bash
cabal test all
```

The test suite should pass before creating a git commit.

**Note:** The test output is extensive (hundreds of examples). For easier analysis, pipe to a file:

```bash
cabal test all 2>&1 | tee /tmp/test-output.txt
```

Then you can:

- Check summary: `tail -50 /tmp/test-output.txt`
- Search for failures: `grep -i "fail\|error" /tmp/test-output.txt`
- Check specific tests: `grep "ok/factorial" /tmp/test-output.txt`
- Read incrementally: `less /tmp/test-output.txt`

Also before committing changes, run `npm run format` so that Github's CI doesn't fail PRs on formatting errors.

#### Golden Files

The test suite uses golden files for snapshot testing. Golden files capture the expected output of tests.

**Important behaviors:**

- **First run**: If a golden file doesn't exist, it will be created automatically on the first test run. The test will fail with "Failed because failFirstTime is set to True".
- **Second run**: Running the test again will compare against the newly created golden file and should pass.
- **When adding new test files**: You typically need to run `cabal test all` twice - once to create the golden files, and once to verify they match.

**Updating golden files:**

If you've made intentional changes that affect test output and you're confident the new behavior is correct, you can:

1. Delete the relevant golden files (usually in `.golden/` directories)
2. Run `cabal test all` twice to regenerate and verify the golden files

This is preferable to manually editing golden files, as it ensures the test output exactly matches what the system produces.

### Building

To build the entire project:

```bash
cabal build all
```

To run the CLI tool:

```bash
cabal run jl4-cli -- <file.l4>
```

To install the LSP tool:

```bash
cabal install exe:jl4-lsp --overwrite-policy=always
```

---

## Task Management

### Specifications

Task specifications and feature designs are organized in the `doc/` directory:

- **`doc/todo/`** - Specifications for planned features and tasks in progress (e.g., `EXPORT-SYNTAX-SPEC.md`, `BOOLEAN-MINIMIZATION-SPEC.md`)
- **`doc/done/`** - Completed specifications for reference (e.g., `BIDIRECTIONAL-TYPE-CHECKING-SPEC.md`, `STRING-CONCAT-SPEC.md`)

When working on a feature, check if there's an existing spec in `doc/todo/` that describes the requirements.

### GitHub Issues

Consult the GitHub issues for bug reports, feature requests, and ongoing discussions:

**https://github.com/smucclaw/l4-ide/issues**

Issues often contain important context, design decisions, and acceptance criteria that may not be fully captured in the spec documents.

### Pull Requests

To understand recent project activity and context, review recent pull requests:

**https://github.com/smucclaw/l4-ide/pulls**

Reviewing merged PRs helps understand what work has been done recently, ongoing architectural decisions, and the current state of the codebase.

### Keeping Documentation Current

**Always update documentation alongside code changes.** When completing work:

1. **Update the relevant spec** in `doc/todo/` to reflect what was implemented, any deviations from the original plan, and remaining work
2. **Move completed specs** from `doc/todo/` to `doc/done/` when a feature is fully implemented
3. **Update related docs** (e.g., `doc/README.md`, tutorials) if the change affects user-facing behavior
4. **Note any new limitations or known issues** discovered during implementation

Documentation is a first-class deliverable, not an afterthought.
