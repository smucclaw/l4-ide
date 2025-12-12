# Module 0 — Orientation & File Anatomy

## What L4 Is

L4 is a **programming language for law**. It brings the rigor and tooling of software engineering to legal work—contracts, regulations, policies, and compliance logic.

Think of L4 as:

- **PostScript for law**: Just as PostScript became the foundation for desktop publishing, L4 aims to be the foundational language for legal computation
- **A compiler for legal rules**: It translates legal requirements into executable, testable, verifiable code
- **A bridge between humans and machines**: Legal professionals can express rules clearly, while computers can reason about them formally

## Why L4 Exists

### The Problem

Legal work today faces significant challenges:

- **Microsoft Word as the only tool**: Unlike programmers (who have IDEs, compilers, debuggers, test suites) or graphic designers (who have Adobe's creative suite), lawyers work in general-purpose word processors
- **No systematic testing**: Contracts and regulations are rarely tested against edge cases before deployment
- **Hidden ambiguities**: Legal documents often contain undetected contradictions, loopholes, and ambiguities that only emerge when disputes arise
- **Fragmented LegalTech**: The 3,000+ LegalTech startups mostly focus on document management, not on making law computable

### The Vision

L4 enables a paradigm shift from "contracts as documents" to "contracts as executable specifications":

1. **Write rules once, deploy everywhere**: From the same L4 source, generate:
   - Human-readable documentation (Markdown, PDF)
   - Web applications for end-users
   - API services for integration
   - Test suites for validation

2. **Formal verification**: Test rules against scenarios, find contradictions, verify properties (like security researchers test code for exploits)

3. **AI + Logic = Guardrails**: LLMs provide natural language understanding, while L4 provides rigorous "left-brain" logic to prevent hallucinations

4. **Targeting underserved markets first**: Rather than competing for law firm business, L4 targets:
   - Individuals who don't understand their insurance policies
   - Startups that can't afford $500/hour legal fees
   - Government agencies modernizing regulation delivery

## How an L4 File is Structured

### Basic Anatomy

An L4 file consists of:

1. **Sections** (optional organizational headers)
2. **Import statements** (to use libraries and other modules)
3. **Type declarations** (enums, records, algebraic data types)
4. **Function definitions** (reusable logic)
5. **Decision rules** (Boolean-returning functions)
6. **Assumptions** (type hints for external functions)
7. **Directives** (compiler instructions like #EVAL, #ASSERT, #TRACE)

### A Minimal Example

```l4
§ `My First L4 File`

§§ `Type Definitions`

DECLARE Person
    HAS name IS A STRING
        age  IS A NUMBER

§§ `Business Logic`

GIVEN person IS A Person
GIVETH A BOOLEAN
DECIDE `is adult` IF
    person's age AT LEAST 18

§§ `Test Execution`

#EVAL `is adult` (Person WITH name IS "Alice", age IS 25)
```

### Key Syntax Elements

#### Sections

```l4
§ `Top-Level Section`
§§ `Subsection`
```

Sections provide hierarchy and navigation. They're purely organizational—like markdown headers.

#### Backticks for Multi-Word Names

```l4
`can transfer ownership`  -- multi-word identifier
canTransferOwnership      -- alternative style (no spaces)
```

Use backticks when you want identifiers that read like natural language.

#### Comments

```l4
-- This is a single-line comment

{-
This is a
multi-line comment
-}
```

#### Directives

```l4
#EVAL expression           -- Evaluate and display result
#ASSERT boolean_expr       -- Assert truth (for testing)
#TRACE contract_provision  -- Simulate contract execution
```

### Layout-Sensitive Syntax

L4 uses **indentation** to denote structure (like Python or Haskell):

```l4
GIVEN x IS A NUMBER
GIVETH A BOOLEAN
isPositive x MEANS
    IF x GREATER THAN 0    -- indented: part of the expression
    THEN TRUE              -- indented: part of IF
    ELSE FALSE             -- aligned with THEN
```

Incorrect indentation will cause parse errors:

```l4
-- WRONG:
isPositive x MEANS
IF x > 0  -- Error! Not indented
THEN TRUE
```

### Import System

```l4
IMPORT prelude    -- Standard library (lists, maybe, etc.)
IMPORT daydate    -- Date arithmetic library
```

Libraries provide reusable functions. The prelude includes:

- List operations: `map`, `filter`, `fold`, `any`, `all`
- Maybe/Optional handling: `JUST`, `NOTHING`, `fromMaybe`
- Pair operations: `PAIR`, `fst`, `snd`

### Type System

L4 is **statically typed** (like Java, not like Python). Every expression has a type known at compile time.

Common types:

- `NUMBER` - integers and floating-point
- `STRING` - text
- `BOOLEAN` - TRUE or FALSE
- `DATE` - calendar dates
- `LIST OF T` - homogeneous lists
- `MAYBE T` - optional values (like Java's Optional or Haskell's Maybe)
- Custom types declared with `DECLARE`

### Execution Model

L4 is:

- **Purely functional**: Functions have no side effects
- **Lazy**: Expressions are evaluated only when needed
- **Traceable**: Execution produces detailed audit trails (crucial for legal reasoning)

When you run `#EVAL`, L4:

1. Type-checks your expression
2. Evaluates it lazily
3. Produces a value and execution trace
4. Displays the result

## Development Workflow

1. **Write** your L4 code in a `.l4` file
2. **Compile** it to check for type errors
3. **Test** it with `#EVAL` and `#ASSERT`
4. **Visualize** decision logic as ladder diagrams
5. **Generate** web apps, APIs, or documentation

## Next Steps

In **Module 1**, we'll dive into declaring types—enums and records—using a concrete legal domain example: the WorkPass Authority.

## Exercise

Create a file `hello.l4` with:

```l4
§ `Hello World in L4`

GIVEN name IS A STRING
GIVETH A STRING
greet name MEANS "Hello, " APPEND name

#EVAL greet "World"
```

Run it with:

```bash
cabal run jl4-cli -- hello.l4
```

You should see the result: `"Hello, World"`
