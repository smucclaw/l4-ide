# L4 GitHub Resources

## Main Repository

**l4-ide**: https://github.com/smucclaw/l4-ide

The primary L4 implementation with IDE support, compiler toolchain, and visualizer.

## Documentation

### Foundation Course (Recommended Starting Point)

- **README**: https://raw.githubusercontent.com/smucclaw/l4-ide/main/doc/foundation-course-ai/README.md
- **Quickstart**: https://raw.githubusercontent.com/smucclaw/l4-ide/main/doc/foundation-course-ai/quickstart.md
- **Module 0 - Orientation**: https://raw.githubusercontent.com/smucclaw/l4-ide/main/doc/foundation-course-ai/module-0-orientation.md
- **Module 1 - Enums & Records**: https://raw.githubusercontent.com/smucclaw/l4-ide/main/doc/foundation-course-ai/module-1-enums-records.md
- **Module 2 - Functions**: https://raw.githubusercontent.com/smucclaw/l4-ide/main/doc/foundation-course-ai/module-2-functions.md
- **Module 3 - Control Flow**: https://raw.githubusercontent.com/smucclaw/l4-ide/main/doc/foundation-course-ai/module-3-control-flow.md
- **Module 4 - Lists & Maybe**: https://raw.githubusercontent.com/smucclaw/l4-ide/main/doc/foundation-course-ai/module-4-lists-maybe.md
- **Module 5 - WPA Pipeline**: https://raw.githubusercontent.com/smucclaw/l4-ide/main/doc/foundation-course-ai/module-5-wpa-pipeline.md
- **Module 6 - Test Data**: https://raw.githubusercontent.com/smucclaw/l4-ide/main/doc/foundation-course-ai/module-6-test-data-eval.md

### Advanced Course

- **README**: https://raw.githubusercontent.com/smucclaw/l4-ide/main/doc/advanced-course-ai/README.md

Covers professional workflows, temporal logic, API integration, JSON handling, testing, and multi-file architecture.

## Code Examples

### jl4/examples Directory Structure

**Location**: `https://github.com/smucclaw/l4-ide/tree/main/jl4/examples`

Examples are organized into:

- **ok/**: Working examples that demonstrate correct L4 usage
- **not-ok/**: Examples showing common errors and how to fix them

### Accessing Examples

To view a specific example:

```
https://raw.githubusercontent.com/smucclaw/l4-ide/main/jl4/examples/ok/[filename].l4
```

Example files include:

- **factorial.l4**: Recursive functions
- **parking.l4**: Decision logic with CONSIDER/WHEN
- Various unit test files demonstrating language features

## Standard Library

### Prelude

**Location**: `libraries/prelude.l4` (or `jl4-core/libraries/prelude.l4`)

The Haskell-style prelude contains:

- List operations (map, filter, fold, etc.)
- Maybe operations (fromMaybe, isJust, etc.)
- Pair operations
- Common utility functions

### Other Libraries

- **daydate**: Date and time operations
- **jurisdiction**: Jurisdiction-specific rules
- **entities**: Common entity definitions
- **currency**: Currency handling

## Language Implementation

### Core Implementation

**Location**: `jl4-core/` in the l4-ide repository

Written in Haskell, following functional programming principles inspired by Haskell's type system and evaluation model.

## Related Repositories

### l4-lp (Alternative Implementation)

**Repository**: https://github.com/smucclaw/l4-lp

Fully in-browser IDE and rule engine for L4 with Clojure/ClojureScript parser and SWI-Prolog execution engine. Includes language bindings for Python, Java, and JavaScript.

### baby-l4 (Miniature Implementation)

**Repository**: https://github.com/smucclaw/baby-l4

Simplified version for experimentation and learning about L4's internals.

## Tools & Validation

### jl4-cli

The command-line tool for type-checking and executing L4 programs.

**Usage**:

```bash
# Build the project (first time only)
cabal build all

# Run jl4-cli on a file
cabal run jl4-cli -- your-file.l4

# With fixed evaluation time
cabal run jl4-cli -- --fixed-now=2025-01-01T00:00:00Z your-file.l4
```

### VS Code Extension

For enhanced development experience with syntax highlighting, type checking, and visualization.

Setup instructions in: https://github.com/smucclaw/l4-ide/blob/main/Dev.md

## Web-Based IDE

**URL**: https://jl4.legalese.com/

Lightweight web-based alternative to VS Code for quick experimentation.

## Finding Specific Information

### To Learn Basic Syntax

1. Start with Quickstart
2. Read Modules 1-3
3. Reference syntax-quick-ref.md (in this skill)

### To See Working Examples

Browse `jl4/examples/ok/` directory

### To Understand Type System

Read Module 1 (Enums & Records)

### To Learn Pattern Matching

Read Module 3 (Control Flow)

### To Work with Lists and Optional Values

Read Module 4 (Lists, Maybe)

### To Build Complex Applications

Read Module 5 (WPA Pipeline) and Advanced Course

## Getting Help

- **Issues**: https://github.com/smucclaw/l4-ide/issues
- **Discussions**: GitHub Discussions in the repository
- **Examples**: Study the unit tests in `jl4/examples/`

## Key Insight

L4 is heavily inspired by Haskell's:

- Type system (algebraic data types, type inference)
- Evaluation model (lazy, pure functional)
- Syntax (layout-sensitive, pattern matching)

If you know Haskell, many concepts transfer directly. If you don't, the Foundation Course teaches everything you need.
