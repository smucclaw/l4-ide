# L4 Operators Reference

Operators in L4 perform operations on values. Most operators have both textual (keyword) and symbolic forms to balance readability for legal professionals and conciseness for programmers.

## Overview

L4 operators are organized into categories:

- **Arithmetic** - Mathematical operations (PLUS, MINUS, TIMES, etc.)
- **Comparison** - Value comparisons (EQUALS, GREATER THAN, etc.)
- **Logical** - Boolean operations (AND, OR, NOT, IMPLIES)
- **String** - Text manipulation (CONCAT, APPEND)
- **List** - List construction and manipulation (FOLLOWED BY, EMPTY)
- **Temporal** - Time-based operations (AT, WITHIN)

---

## Arithmetic Operators

Mathematical operations on numbers.

### Addition

- **Keyword:** PLUS
- **Symbol:** `+`
- **Type:** `NUMBER -> NUMBER -> NUMBER`
- **Example:** `5 PLUS 3` or `5 + 3` → `8`

### Subtraction

- **Keyword:** MINUS
- **Symbol:** `-`
- **Type:** `NUMBER -> NUMBER -> NUMBER`
- **Example:** `10 MINUS 4` or `10 - 4` → `6`

### Multiplication

- **Keyword:** TIMES
- **Symbol:** `*`
- **Type:** `NUMBER -> NUMBER -> NUMBER`
- **Example:** `6 TIMES 7` or `6 * 7` → `42`

### Division

- **Keywords:** DIVIDED BY
- **Symbol:** `/`
- **Type:** `NUMBER -> NUMBER -> NUMBER`
- **Example:** `15 DIVIDED BY 3` or `15 / 3` → `5`
- **Note:** Returns rational numbers for non-integer results

### Modulo

- **Keyword:** MODULO
- **Type:** `NUMBER -> NUMBER -> NUMBER`
- **Example:** `17 MODULO 5` → `2`
- **Note:** Remainder after division

---

## Comparison Operators

Compare values and return BOOLEAN results.

### Equality

- **Keyword:** EQUALS
- **Symbol:** `=`
- **Type:** `a -> a -> BOOLEAN`
- **Example:** `5 EQUALS 5` or `5 = 5` → `TRUE`

### Greater Than

- **Keywords:** GREATER THAN, ABOVE
- **Symbol:** `>`
- **Type:** `NUMBER -> NUMBER -> BOOLEAN`
- **Example:** `10 GREATER THAN 5` or `10 > 5` → `TRUE`

### Less Than

- **Keywords:** LESS THAN, BELOW
- **Symbol:** `<`
- **Type:** `NUMBER -> NUMBER -> BOOLEAN`
- **Example:** `3 LESS THAN 7` or `3 < 7` → `TRUE`

### Greater or Equal

- **Keywords:** AT LEAST
- **Symbol:** `>=`
- **Type:** `NUMBER -> NUMBER -> BOOLEAN`
- **Example:** `5 AT LEAST 5` or `5 >= 5` → `TRUE`

### Less or Equal

- **Keywords:** AT MOST
- **Symbol:** `<=`
- **Type:** `NUMBER -> NUMBER -> BOOLEAN`
- **Example:** `4 AT MOST 10` or `4 <= 10` → `TRUE`

### Inequality

- **Expression:** `NOT (x EQUALS y)`
- **Example:** `NOT (5 EQUALS 3)` → `TRUE`

---

## Logical Operators

Boolean operations for conditions and logic.

### Conjunction

- **Keyword:** [AND](AND.md)
- **Symbol:** `&&` / `...`
- **Type:** `BOOLEAN -> BOOLEAN -> BOOLEAN`
- **Example:** `TRUE AND FALSE` → `FALSE`
- **Truth table:**
  - `TRUE AND TRUE` → `TRUE`
  - `TRUE AND FALSE` → `FALSE`
  - `FALSE AND TRUE` → `FALSE`
  - `FALSE AND FALSE` → `FALSE`

### Disjunction

- **Keyword:** [OR](OR.md)
- **Symbol:** `||` \ `..`
- **Type:** `BOOLEAN -> BOOLEAN -> BOOLEAN`
- **Example:** `TRUE OR FALSE` → `TRUE`
- **Truth table:**
  - `TRUE OR TRUE` → `TRUE`
  - `TRUE OR FALSE` → `TRUE`
  - `FALSE OR TRUE` → `TRUE`
  - `FALSE OR FALSE` → `FALSE`

### Negation

- **Keyword:** [NOT](NOT.md)
- **Type:** `BOOLEAN -> BOOLEAN`
- **Example:** `NOT TRUE` → `FALSE`

### Implication

- **Keyword:** [IMPLIES](IMPLIES.md)
- **Symbol:** `=>`
- **Type:** `BOOLEAN -> BOOLEAN -> BOOLEAN`
- **Example:** `FALSE IMPLIES TRUE` → `TRUE`
- **Note:** `p IMPLIES q` is equivalent to `NOT p OR q`

### Exception

- **Keyword:** [UNLESS](UNLESS.md)
- **Type:** `BOOLEAN -> BOOLEAN -> BOOLEAN`
- **Example:** `TRUE UNLESS FALSE` → `TRUE`
- **Note:** `p UNLESS q` is equivalent to `p AND NOT q`
- **Precedence:** Binds looser than OR (applies to entire expression)
- **Truth table:**
  - `TRUE UNLESS TRUE` → `FALSE`
  - `TRUE UNLESS FALSE` → `TRUE`
  - `FALSE UNLESS TRUE` → `FALSE`
  - `FALSE UNLESS FALSE` → `FALSE`

---

## String Operators

Operations on text strings.

### Concatenation

- **Keyword:** CONCAT
- **Type:** `STRING -> STRING -> STRING`
- **Example:** `CONCAT "hello", " world"` → `"hello world"`

### Append

- **Keyword:** APPEND
- **Type:** `STRING -> STRING -> STRING`
- **Example:** `"hello" APPEND " world"` → `"hello world"`
- **Note:** Infix wrapper for CONCAT

---

## List Operators

Construct and manipulate lists.

### Cons

- **Keywords:** FOLLOWED BY
- **Type:** `a -> LIST OF a -> LIST OF a`
- **Example:** `1 FOLLOWED BY LIST 2, 3` → `LIST 1, 2, 3`
- **Note:** Prepends element to list

### Empty List

- **Keyword:** EMPTY
- **Type:** `LIST OF a`
- **Example:** `EMPTY` → Empty list
- **Note:** Used in pattern matching

---

## Temporal Operators

Time-based operations for dates and deadlines.

### At

- **Keyword:** AT
- **Type:** Used during tracing of regulative statements to track points in time
- **Example:** `event AT date`

### Within

- **Keyword:** WITHIN
- **Type:** Time duration constraint in regulative statements
- **Example:** `PARTY MUST DO action WITHIN 30 DAYS`

---

## Operator Precedence

Operators are evaluated in the following order (highest to lowest precedence):

1. **Function application** (highest)
2. **Unary operators** (NOT)
3. **Multiplicative** (TIMES, DIVIDED BY, MODULO)
4. **Additive** (PLUS, MINUS)
5. **Comparison** (EQUALS, GREATER THAN, LESS THAN, etc.)
6. **Logical AND**
7. **Logical OR**
8. **UNLESS** (exception clause)
9. **IMPLIES** (lowest)

Use parentheses `()` to override precedence.

### Examples

[precedence-example.l4](precedence-example.l4)

---

## Associativity

### Left-Associative Operators

Most binary operators associate left-to-right.

### Right-Associative Operators

List cons (FOLLOWED BY) and IMPLIES associate right-to-left.

[associativity-example.l4](associativity-example.l4)

---

## Overloading

Some operators work on multiple types:

### EQUALS

Works on most types:

- `5 EQUALS 5` (NUMBER)
- `"hello" EQUALS "hello"` (STRING)
- `TRUE EQUALS TRUE` (BOOLEAN)

### Comparison Operators

Work on ordered types:

- NUMBER: `5 > 3`
- STRING: `"b" > "a"` (lexicographic)
- DATE: `date1 > date2` (chronological)

### PLUS

Overloaded for different contexts:

- Arithmetic: `5 + 3`
- Can be extended via libraries

---

## Special Syntax

### Asyndetic Operators

Implicit operators using punctuation:

- **`...`** (ellipsis) - Implicit AND
- **`..`** (double dot) - Implicit OR

**Example:** [asyndetic-example.l4](asyndetic-example.l4)

### Genitive

Field access using possessive:

- **`'s`** - Record field access
- Example: `person's age`

See the syntax reference for details.

---

## Textual vs. Symbolic Forms

### Why Both?

- **Textual forms** - More readable for legal professionals unfamiliar with programming
- **Symbolic forms** - More concise for experienced developers

Both are equivalent and can be mixed.

**Example:** [textual-vs-symbolic-example.l4](textual-vs-symbolic-example.l4)

### Recommendations

- **Legal documents:** Prefer textual forms
- **Technical code:** Use symbolic forms for conciseness
- **Mixed audiences:** Choose based on primary readers

---

## See Also

- **[GLOSSARY](../GLOSSARY.md)** - All operators indexed
- **[Functions](../functions/README.md)** - Function keywords
- **[Types](../types/README.md)** - Types used with operators
- **[Syntax](../syntax/README.md)** - Operator syntax rules

---

## Examples

Operator examples are included as `.l4` files in this directory.

---

## Contributing

To add operator documentation:

1. Check [Lexer.hs](https://github.com/smucclaw/l4-ide/blob/main/jl4-core/src/L4/Lexer.hs) for operators
2. Add examples as `.l4` files in this directory
3. Document textual and symbolic forms
4. Include type signatures
5. Submit pull request
