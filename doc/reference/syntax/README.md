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

Compiler commands for testing and evaluation. Directives begin with `#` and appear at the top level of a file.

### #EVAL

Evaluate an expression and display the result. Used for testing functions and inspecting computed values.

**Syntax:**

```l4
#EVAL expression
```

**Examples:**

```l4
#EVAL `is adult` 21        -- evaluates to TRUE
#EVAL `tax owed` 75000     -- evaluates to the computed number
#EVAL 2 PLUS 2             -- evaluates to 4
```

**See also:** [directive-example.l4](directive-example.l4)

### #EVALTRACE

Evaluate an expression and display the full execution trace, showing each step of the evaluation.

**Syntax:**

```l4
#EVALTRACE expression
```

### #TRACE

Evaluate a deontic (regulative) expression and display the obligation trace. Shows the sequence of obligations, which parties must act, deadlines, and the resulting state (FULFILLED or BREACH).

**Syntax:**

```l4
#TRACE deonticExpression AT startTime WITH
  PARTY partyName DOES action AT eventTime
  ...
```

**Examples:**

```l4
#TRACE paymentObligation AT 0 WITH
  PARTY Alice DOES pay 100 AT 15

#TRACE saleContract AT 0 WITH
  PARTY Seller DOES delivery AT 2
  PARTY Buyer DOES payment 100 AT 5
```

**See also:** [Regulative Rules](../regulative/README.md) for the deontic keywords used with #TRACE

### #CHECK

Type check an expression without evaluating it.

**Syntax:**

```l4
#CHECK expression
```

### #ASSERT

Assert that an expression evaluates to TRUE. Used for automated testing.

**Syntax:**

```l4
#ASSERT expression
```

**Example:**

```l4
#ASSERT 2 PLUS 2 EQUALS 4
```

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

### OF (Positional Argument Syntax)

Multi-purpose structural keyword that introduces comma-separated argument lists. Without OF, arguments must be space-separated on the same line or indented on subsequent lines.

**Contexts where OF appears:**

| Context              | Example                      |
| -------------------- | ---------------------------- |
| Function application | `add OF 3, 4`                |
| Sum type declaration | `IS ONE OF Red, Green, Blue` |
| Type constructor     | `LIST OF Person`             |
| Record construction  | `Pair OF 10, 20`             |
| Pattern matching     | `WHEN Pair OF x, y THEN ...` |

**Examples:**

```l4
-- With OF: comma-separated args on one line
result1 MEANS add OF 3, 4
result2 MEANS foldr OF add, 0, numbers

-- Without OF: space-separated on same line
result3 MEANS add 3 4
```

OF is optional in function application -- `add OF 3, 4` and `add 3 4` are equivalent. But for multi-argument calls, OF with commas is often clearer than relying on whitespace parsing.

**See also:** [Types reference](../types/keywords.md) for OF in type contexts

---

### TO (Function Type Syntax)

Used in function type annotations to separate input types from the return type.

**Syntax:**

```l4
FUNCTION FROM Type1 AND Type2 TO ReturnType
```

**Note:** `TO` is a reserved keyword used only in function type annotations. In deontic rules, `to` appearing in an action (e.g. `deliver goods to buyer`) is part of the mixfix expression, not the TO keyword.

---

### Genitive

Record field access using `'s`.

**Example:** [genitive-example.l4](genitive-example.l4)

---

### Section Markers (§)

Organize code into named, nested scopes using `§`, similar to sections in legislation. Definitions in different sections do not shadow each other; the compiler creates fully qualified name bindings for disambiguation.

**Levels:**

- `§` -- Top-level section
- `§§` -- Subsection
- `§§§` -- Sub-subsection

**Example:** [section-example.l4](section-example.l4)

**Qualified access:** When the same name exists in multiple sections, consumers must qualify to disambiguate. This parallels how legislation scopes definitions ("for purposes of subsection 2, X means ...").

```l4
§ `Part VII`

§§ `Subsection 2`
`age of majority` MEANS 18

§§ `Subsection 3`
`age of majority` MEANS 21

-- Consumer must qualify:
DECIDE `is adult under sub 2` IF
    age >= `Part VII`'s `Subsection 2`'s `age of majority`
```

**Section aliases:** Use AKA to create shorter names for qualified references:

```l4
§ `Definitions for Part VII` AKA defs
  taxableIncome MEANS 50000

result MEANS defs.taxableIncome   -- via alias
```

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
- **[Specifications](https://github.com/legalese/l4-ide/tree/main/specs)** - Technical specifications
