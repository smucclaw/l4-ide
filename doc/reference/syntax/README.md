# L4 Syntax Reference

L4's syntax is designed to be readable by legal professionals while maintaining the precision needed for computation. This section documents syntax patterns, rules, and special features.

## Overview

Key syntax features:

- **Layout-sensitive** - Uses indentation like Python
- **Natural language style** - Reads like structured English
- **Annotations** - Metadata for documentation and generation
- **Directives** - Compiler commands for testing and evaluation
- **Special symbols** - Backticks, ditto marks, ellipsis, etc.

---

## Core Syntax Features

### Layout Rules

Indentation-based grouping instead of braces.

**Key Concepts:**

- Blocks defined by indentation level
- Consistent indentation required
- Replaces `{` `}` and `;` separators
- Pythonic style

**Example:** [layout-example.l4](layout-example.l4)

---

### Comments

Documentation and notes in code.

**Example:** [comment-example.l4](comment-example.l4)

**Note:** Both `{- -}` and `/* */` styles work for block comments.

---

### Identifiers

Names for variables, functions, and types.

**Example:** [identifier-example.l4](identifier-example.l4)

**Regular:** Start with letter, continue with letters/numbers/underscore  
**Quoted:** Use backticks for spaces/special characters

**Case Sensitivity:**

- Keywords: UPPERCASE only
- Identifiers: Case-sensitive (`age` ≠ `Age`)

---

## Annotations

Metadata attached to declarations.

### @desc

Human-readable descriptions.

**Example:** [annotation-example.l4](annotation-example.l4)

### @nlg

Natural language generation hints.

**Inline form:** `The applicant [is %age% years old].`

See [annotation-example.l4](annotation-example.l4)

### @ref

Cross-references to legal sources.

**Inline form:** `The applicant <<must be at least 18 years old>>.`

See [annotation-example.l4](annotation-example.l4)

### @ref-src / @ref-map

Source references and mappings.

See [annotation-example.l4](annotation-example.l4)

### @export

Mark declarations for export.

Example: `@export "public_api"`

---

## Directives

Compiler commands for testing and evaluation.

### #EVAL

Evaluate and print expression.

**Example:** [directive-example.l4](directive-example.l4)

### #EVALTRACE

Evaluate with execution trace.

Example: `#EVALTRACE complexCalculation`

### #TRACE

Generate state graph traces.

Example: `#TRACE contractStateMachine`

### #CHECK

Type check expression.

Example: `#CHECK age > 18`

### #ASSERT

Assert truth value (for testing).

Example: `#ASSERT 2 PLUS 2 EQUALS 4`

---

## Special Syntax

### Ditto

Copy from line above using `^`.

**Example:** [ditto-example.l4](ditto-example.l4)

**Rules:**

- One `^` per token to copy
- Copies tokens from line directly above
- Useful for repetitive declarations

---

### Genitive

Record field access using `'s`.

**Example:** [genitive-example.l4](genitive-example.l4)

---

### Section Markers

Organize code into sections using `§`.

**Example:** [section-example.l4](section-example.l4)

**Features:**

- `§` - Section
- `§§` - Subsection
- `§§§` - Sub-subsection
- Optional section names

---

## Literals

### Numbers

**Integers:** `42`, `-17`, `0`  
**Rationals:** `3.14`, `-0.5`, `2.718`

### Strings

**Basic:** `"hello world"`, `"L4 language"`  
**Escape Sequences:** `\n` (newline), `\"` (quotes), `\\` (backslash)

### Booleans

`TRUE`, `FALSE`

### Lists

`LIST 1, 2, 3`, `EMPTY`, `1 FOLLOWED BY 2 FOLLOWED BY EMPTY`

---

## Symbols

### Parentheses

`( )` - Grouping and tuples.

Examples: `(age PLUS 5) TIMES 2`, `PAIR OF 1, 2`

### Brackets

`[ ]` - Inline NLG annotations.

Example: `The applicant [is %age% years old].`

### Angles

`<< >>` - Inline reference annotations.

Example: `The applicant <<must be 18 or over>>.`

### Braces

`{ }` - Block comments (alternative).

Example: `{- Block comment -}`

### Other Symbols

- `,` - Separator
- `;` - Statement separator (rarely needed with layout)
- `.` - Decimal point
- `:` - Type signature separator
- `%` - Percent numbers, NLG expression delimiter
- `^` - Ditto (copy above)
- `...` - Asyndetic AND
- `..` - Asyndetic OR

---

## Syntax Conventions

### Naming Conventions

**Variables and Functions:**

- camelCase: `taxRate`, `calculateTotal`
- snake_case: `tax_rate`, `calculate_total`
- Spaces with backticks: `` `tax rate` ``

**Types:**

- PascalCase: `Person`, `TaxBracket`
- Prefer nouns

**Constants:**

- UPPERCASE: `MAX_AGE`, `DEFAULT_RATE`

### Indentation

- **2 spaces** or **4 spaces** (choose one, be consistent)
- No tabs
- Align related items vertically

### Line Length

- Recommended: 80-100 characters
- Legal text may be longer for readability

---

## Style Guide

### Readability

- Use whitespace liberally
- Add comments for complex logic
- Prefer textual operators in legal contexts

### Consistency

- Follow project conventions
- Use linter/formatter when available
- Be consistent within a file

### Legal Isomorphism

- Structure code to mirror legal text
- Use legal terminology in identifiers
- Preserve section/subsection hierarchy

---

## Common Patterns

### Type Declarations

**Example:** [record-example.l4](../types/record-example.l4)

### Function Definitions

**Example:** [function-type-example.l4](../types/function-type-example.l4)

### Pattern Matching

**Example:** [enum-example.l4](../types/enum-example.l4)

---

## See Also

- **[GLOSSARY](../GLOSSARY.md)** - Complete feature index
- **[Functions](../functions/README.md)** - Function keywords
- **[Specifications](https://github.com/smucclaw/l4-ide/tree/main/specs)** - Technical specifications
