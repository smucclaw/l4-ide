---
name: l4
description: Write legal and regulatory rules as executable, type-checked code in L4, a functional programming language for computational law. Use when formalizing contracts, legislation, regulations, policies, or compliance logic into computable form. Triggered when users need to encode legal rules, validate formalized law, generate web apps from legal logic, or translate natural language legal text into formal specifications. The skill covers L4 syntax, type system, pattern matching, validation with jl4-cli, and the complete workflow from analysis to deployment.
---

# L4 — Programming Language for Law

## Overview

L4 is a statically-typed functional programming language for computational law, inspired by Haskell. It enables legal professionals and developers to encode contracts, regulations, and policies as executable, testable, verifiable programs. L4 bridges the gap between human-readable legal text and machine-executable logic.

**Cloud Validation Available**: This skill includes a cloud-based validator that connects to `wss://jl4.legalese.com/lsp`, allowing you to validate L4 code without installing the Haskell toolchain locally. See Section 5 for details.

## Core Workflow

When working with L4, follow this systematic approach:

### 1. Analyze Source Material

When given natural language legal text (PDFs, URLs, legislation):

- **Identify the domain ontology**: What entities, statuses, and categories exist? (Often unstated in source documents—use world knowledge)
- **Extract decision logic**: What are the business rules and conditions?
- **Map state transitions**: What obligations, deadlines, and modalities govern parties over time?

### 2. Model the Domain (Types)

Use `DECLARE` to define the type system:

```l4
-- Enums: Fixed sets of values
DECLARE RiskCategory IS ONE OF
    LowRisk
    MediumRisk
    HighRisk
    Uninsurable

-- Records: Types with named fields
DECLARE Driver HAS
    name            IS A STRING
    age             IS A NUMBER
    yearsLicensed   IS A NUMBER
    accidentCount   IS A NUMBER
    hasTickets      IS A BOOLEAN
```

**Isomorphic encoding principle**: Match the structure of the source text. If legal text has sections 1.1, 1.2, 1.3, your L4 code should reflect that hierarchy with corresponding logical structure (ANDs/ORs forming the same tree).

### 3. Encode Decision Logic

Use `GIVEN`/`GIVETH`/`MEANS` for functions, `DECIDE ... IF` for rules:

```l4
-- Simple decision rule
GIVEN driver IS A Driver
GIVETH A BOOLEAN
DECIDE `meets minimum age` IF
    driver's age AT LEAST 18

-- Pattern matching with CONSIDER
GIVEN driver IS A Driver
GIVETH A RiskCategory
`assess risk` driver MEANS
    CONSIDER driver's accidentCount
    WHEN 0 THEN
        IF driver's hasTickets
        THEN MediumRisk
        ELSE LowRisk
    WHEN 1 THEN MediumRisk
    WHEN 2 THEN HighRisk
    OTHERWISE Uninsurable
```

### 4. Formalize Modal Logic (Advanced)

For multi-party contracts with obligations and deadlines, use L4's deontic, temporal, and epistemic modalities (see Advanced Course for details).

### 5. Validate with jl4-cli or Cloud Validation

**Option A: Local validation (requires jl4-cli installation)**

```bash
jl4-cli your-file.l4
```

**Option B: Cloud validation (no installation required)**

```bash
node l4/scripts/validate-cloud.mjs your-file.l4
```

The cloud validator connects to `wss://jl4.legalese.com/lsp` and provides the same validation as local jl4-cli, without requiring Haskell toolchain installation. This makes L4 validation accessible from any environment.

Options for cloud validation:

- `--url <wss://...>`: Use a different LSP server
- `--debug`: Show detailed protocol messages

Type errors will be reported with line numbers. Iterate until "checking successful".

### 6. Test with #EVAL and #ASSERT

```l4
-- Sample data
`Alice` MEANS Driver WITH
    name          IS "Alice"
    age           IS 25
    yearsLicensed IS 7
    accidentCount IS 0
    hasTickets    IS FALSE

-- Execute tests
#EVAL `meets minimum age` `Alice`
#EVAL `assess risk` `Alice`
#ASSERT `assess risk` `Alice` EQUALS LowRisk
```

### 7. Generate Outputs (Optional)

From L4 source, you can generate:

- Web applications (decision trees, forms)
- API services (for integration)
- Documentation (Markdown, PDF)
- Visualizations (ladder diagrams)

## Essential L4 Syntax

### File Structure

```l4
§ `Top-Level Section Title`
§§ `Subsection Title`

IMPORT prelude    -- Standard library
IMPORT daydate    -- Date arithmetic

-- Type declarations
DECLARE TypeName ...

-- Function definitions
GIVEN param IS A Type
GIVETH A ReturnType
functionName param MEANS ...

-- Test execution
#EVAL expression
#ASSERT boolean_expression
```

### Type Declarations

```l4
-- Enum (closed set)
DECLARE Status IS ONE OF Value1, Value2, Value3

-- Enum with data (algebraic data type)
DECLARE Outcome IS ONE OF
    Success HAS value IS A NUMBER
    Failure HAS reason IS A STRING

-- Record (product type)
DECLARE Person HAS
    name IS A STRING
    age  IS A NUMBER

-- Lists
LIST a, b, c                 -- list literal
EMPTY                        -- empty list
x FOLLOWED BY xs             -- cons pattern

-- Optional values
MAYBE Type
JUST value                   -- has value
NOTHING                      -- no value
```

### Function Definitions

```l4
-- Standard form
GIVEN param1 IS A Type1
      param2 IS A Type2
GIVETH A ReturnType
functionName param1 param2 MEANS expression

-- Mixfix notation (natural language syntax)
GIVEN employee IS AN Employee
      employer IS A Company
GIVETH A BOOLEAN
`employee` `works for` `employer` MEANS ...

-- Decision rules
DECIDE ruleName IF condition1 AND condition2

-- WHERE clauses (local helpers)
mainExpression
WHERE
    helper1 MEANS expression1
    helper2 MEANS expression2
```

### Pattern Matching

```l4
-- CONSIDER/WHEN (like switch but type-safe)
CONSIDER value
WHEN Pattern1 THEN Result1
WHEN Pattern2 THEN Result2
OTHERWISE DefaultResult

-- With data extraction
CONSIDER outcome
WHEN Success WITH value THEN
    "Got: " APPEND (STRING value)
WHEN Failure WITH reason THEN
    "Error: " APPEND reason

-- List patterns
CONSIDER list
WHEN EMPTY THEN 0
WHEN x FOLLOWED BY xs THEN
    x PLUS (sum xs)

-- BRANCH (flat multi-way decisions)
BRANCH IF status EQUALS Active   THEN "Running"
       IF ^      EQUALS Inactive THEN "Stopped"
       OTHERWISE "Unknown"
```

### Operators

**Boolean Logic:**

```l4
condition1 AND condition2
condition1 OR condition2
NOT condition
IF cond THEN expr1 ELSE expr2
```

**Comparisons:**

```l4
x EQUALS y
x GREATER THAN y
x LESS THAN y
x AT LEAST y              -- >=
x AT MOST y               -- <=
```

**Arithmetic:**

```l4
x PLUS y                  -- +
x MINUS y                 -- -
x TIMES y                 -- *
x DIVIDED BY y            -- /
x MODULO y                -- %
SQRT x                    -- square root
x EXPONENT y  or  x ^ y   -- exponentiation
```

**Strings:**

```l4
STRINGLENGTH str          -- length
TOUPPER str               -- to uppercase
TOLOWER str               -- to lowercase
TRIM str                  -- remove whitespace
CONTAINS str substring    -- check contains
STARTSWITH str prefix     -- check starts with
ENDSWITH str suffix       -- check ends with
SPLIT str delimiter       -- split into list
SUBSTRING str start len   -- extract substring
REPLACE str old new       -- replace all
CHARAT str index          -- character at index
str1 APPEND str2          -- concatenate
```

### Record Construction and Access

```l4
-- Construction
Person WITH
    name IS "Alice"
    age  IS 30

-- Field access
person's name
person's age

-- Chaining
application's employee's nationality
```

### Common List Operations

```l4
map function list         -- apply function to each element
filter predicate list     -- keep elements matching predicate
fold function init list   -- reduce list to single value
any predicate list        -- TRUE if any element matches
all predicate list        -- TRUE if all elements match
`length of` list          -- count elements
at list index             -- get element at index
```

## Best Practices

### Isomorphic Encoding

Match the structure of source legal text:

**Source text:**

```
Section 1.2: Coverage applies if:
  (a) Damage is not caused by rodents, insects, vermin, or birds, and
  (b) Damage is to contents caused by birds, or
  (c) An animal causes water escape from appliance/pool/plumbing
```

**L4 encoding (isomorphic structure):**

```l4
DECIDE `coverage applies` IF
        NOT `caused by excluded pests` damage
    AND (   `bird damage to contents` damage
         OR `animal caused water escape` damage)
```

### Type-Driven Development

1. Start with types (domain model)
2. Write function signatures
3. Implement function bodies
4. Add tests with #EVAL/#ASSERT
5. Validate with jl4-cli

### Mixfix for Readability

Use mixfix notation to make code read like legal prose:

```l4
-- Instead of: eligible(employee, employer)
-- Write:
`employee` `eligible for work pass with` `employer`
```

### Exhaustiveness

Always handle all cases in pattern matching. The compiler will warn about missing cases:

```l4
CONSIDER status
WHEN Active THEN "Running"
WHEN Inactive THEN "Stopped"
-- Add OTHERWISE or handle Suspended if it exists
OTHERWISE "Unknown"
```

### Test Coverage

For each rule, provide:

- Positive test cases (rule holds)
- Negative test cases (rule fails)
- Edge cases (boundary conditions)

```l4
#ASSERT `is adult` (Person WITH age IS 18)
#ASSERT NOT `is adult` (Person WITH age IS 17)
```

## Common Patterns

### Decision Trees

```l4
GIVEN application IS A Application
GIVETH A String
`eligibility decision` application MEANS
    IF NOT `age requirement met`
    THEN "Rejected: Age"
    ELSE IF NOT `education requirement met`
    THEN "Rejected: Education"
    ELSE IF NOT `salary requirement met`
    THEN "Rejected: Salary"
    ELSE "Approved"
    WHERE
        `age requirement met` MEANS ...
        `education requirement met` MEANS ...
        `salary requirement met` MEANS ...
```

### Multi-Stage Pipelines

```l4
GIVEN input IS A InputData
GIVETH A FinalResult
`process application` input MEANS
    FinalResult WITH
        stage1Result IS result1
        stage2Result IS result2
        stage3Result IS result3
    WHERE
        result1 MEANS `stage 1 processing` input
        result2 MEANS `stage 2 processing` result1
        result3 MEANS `stage 3 processing` result2
```

### Recursive List Processing

```l4
GIVEN list IS A LIST OF T
GIVETH A ReturnType
processRecursive list MEANS
    CONSIDER list
    WHEN EMPTY THEN baseCase
    WHEN head FOLLOWED BY tail THEN
        combineResults (process head) (processRecursive tail)
```

## Troubleshooting

### Type Errors

**Error:** "Expected NUMBER but got STRING"
**Fix:** Check field types in records, ensure arithmetic only on numbers

**Error:** "Pattern match not exhaustive"
**Fix:** Add OTHERWISE clause or handle all enum constructors

### Layout Errors

**Error:** "Parse error: unexpected token"
**Fix:** Check indentation—L4 is layout-sensitive like Python/Haskell

### Undefined Functions

**Error:** "Not in scope: `function name`"
**Fix:** Ensure function is defined before use, or add IMPORT statement

## Resources

This skill includes comprehensive reference materials about L4:

### references/

- **syntax-quick-ref.md**: Concise syntax reference for all L4 constructs
- **github-resources.md**: Links to documentation, examples, and source code in the l4-ide repository
- **workflow-guide.md**: Detailed step-by-step workflow from legal text to deployed application

Consult these references when you need:

- Syntax reminders for specific constructs
- Links to example programs
- Guidance on the complete formalization workflow

### scripts/

- **validate.sh**: Wrapper script for jl4-cli validation

### assets/

- **template.l4**: Basic L4 file template with common structure
- **example-parking.l4**: Complete working example (parking fee calculation)

## Key Takeaways

1. **L4 is functional and type-safe** like Haskell, designed for legal reasoning
2. **DECLARE defines types** (enums with `IS ONE OF`, records with `HAS`)
3. **GIVEN/GIVETH/MEANS defines functions** with mandatory type signatures
4. **Pattern matching with CONSIDER/WHEN** is more powerful than switch/case
5. **Mixfix notation** enables natural language function names
6. **Validate with jl4-cli** before testing
7. **#EVAL and #ASSERT** for comprehensive testing
8. **Isomorphic encoding**: Match legal text structure in code structure
9. **Import prelude and daydate** for standard library functions
10. **Use WHERE clauses** for local helpers and cleaner code

For complete tutorials, see `https://github.com/smucclaw/l4-ide/tree/main/doc/foundation-course-ai`
