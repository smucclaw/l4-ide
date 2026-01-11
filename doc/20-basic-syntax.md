# Basic Syntax

## Identifiers

Identifiers, such as function names and record attributes, are either single words such as `identifier` or `X`, or are quoted using backticks if they are more complex. Where other programming languages might use `camelCase` or `snake_case`, L4 allows the use of `` `space separated words` `` to form a single identifier.

## Comments

Comments are written using the `--` syntax. Such comments extend to the end of the current line.

```l4
-- This is a comment
```

You can also use `{- .... -}` syntax to comment out an entire range of lines.

## Textual Annotations

Annotations are paratextual: they are not an essential part of an L4
program, but inform various transformations of the program.

Annotations are enclosed within `[ square brackets ]`.

Annotations can be added to sections and other constructs.

```l4
§ `Section Head` [NLG annotation to section names are valid.]
```

## References

References can be added to various constructs using three
related annotation commands.

References are inserted into the code using the `@ref` annotation.
Such an annotation takes a "reference identifier" as an argument,
e.g.

```l4
@ref FOO-Section2
```

The reference identifier has to be mapped to a URL. The URL is then
available as a link in the IDE. The mapping from reference identifiers
to URLs can be specified in an external file or directly in the source
code:

- @ref-src@ takes a filename of a CSV file as an argument. The CSV
  file should contain a mapping from reference identifiers to URLs.
- @ref-map@ takes a reference identifier and a URL as an argument and
  defines one such mapping directly within the source file.

## Indentation in Expressions

Like most languages, L4 supports the use of `( parentheses )` for grouping, and `,` commas as a list separator.

Unlike most languages, L4 encourages the use of indentation for grouping, and newlines as a list separator.

In a conventional language, parentheses are used to group boolean and arithmetic expressions:

```typescript
/**
 * Determines if the given numbers are considered "big".
 * @param {number} x - The first number.
 * @param {number} y - The second number.
 * @returns {boolean} - True if the numbers are big, false otherwise.
 */
function numbersAreBig(x: number, y: number): boolean {
  return (x > 1000 && y > 250 * (2 + 2)) || x > 10000 || y > 20000;
}
```

In L4, indentation replaces parentheses:

```l4
GIVEN x IS A NUMBER
      y IS A NUMBER
DECIDE `numbers are big`
    IF     x GREATER THAN 1000
       AND y GREATER THAN   250
                          *   2
                            + 2
    OR x GREATER THAN 10000
    OR y GREATER THAN 20000
```

This method of grouping was inspired by legal sub-paragraphs and sub-lists. Generally, operators bind stronger
if they are indented more, and arguments to operators should be indented more than the operators themselves.

Example:

```l4
    FALSE
AND    TRUE
    OR TRUE
```

yields `FALSE`, whereas

```l4
       FALSE
   AND TRUE
OR TRUE
```

yields `TRUE`.

### Indented LIST Literals

L4 supports multiple syntaxes for list literals. The most natural and recommended approach uses vertical block indentation:

**✅ Recommended - Vertical block syntax (inline LET):**

```l4
LET myList BE
  LIST
    "first"
    "second"
    "third"
IN length myList
```

**✅ Also correct - Horizontal alignment:**

```l4
LET myList BE
      LIST "first"
           "second"
           "third"
IN length myList
```

**✅ Multiple bindings - Use multiline LET:**

```l4
LET
  list1 BE
    LIST
      "a"
      "b"
  list2 BE
    LIST
      1
      2
IN ...
```

**❌ Wrong - Using FOLLOWED BY for multi-element lists:**

```l4
LET myList BE
  LIST "first"
  FOLLOWED BY "second"
  FOLLOWED BY "third"
IN ...
```

This creates nested list types (`LIST OF LIST OF ...`) instead of a flat list.

**Note:** `FOLLOWED BY` is the cons operator for building lists recursively, not for writing list literals. The vertical block syntax is preferred for its clarity and consistency with other L4 constructs like `CONSIDER` and `DECLARE ... HAS`.

## Asyndetic Conjunction (`...`)

An **asyndeton** is a rhetorical device where conjunctions are deliberately omitted between items in a list (e.g., "I came, I saw, I conquered" instead of "I came and I saw and I conquered").

L4 supports **asyndetic conjunction**—continuing an AND chain without repeating the conjunction keyword. The **syntax** for this is the **ellipsis** (`...`), three dots that represent an implicit AND:

- **Ellipsis (`...`)** — the _syntax_: the three-dot symbol you type
- **Asyndetic conjunction** — the _semantics_: the implicit AND meaning

Using the ellipsis syntax, you can continue boolean expressions without repeating the `AND` keyword:

```l4
DECIDE `rule applies` IF
        "subject to"
    ... `condition one`
    ... `condition two`
    AND `condition three`
```

This is equivalent to:

```l4
DECIDE `rule applies` IF
        "subject to"
    AND `condition one`
    AND `condition two`
    AND `condition three`
```

The `...` syntax is particularly useful when combined with [inert elements](10-boolean-logic.md#inert-elements-grammatical-scaffolding) to create readable, natural-language-like boolean expressions that mirror the structure of legal documents.

## Asyndetic Disjunction (`..`)

For symmetry, L4 also provides a **two-dot ellipsis (`..`)** for **asyndetic disjunction** (implicit OR):

- **Two-dot ellipsis (`..`)** — the _syntax_: two dots for implicit OR
- **Asyndetic disjunction** — the _semantics_: continuing an OR chain without repeating the keyword

```l4
DECIDE `qualifies` IF
        `has license`
    ..  `has permit`
    OR  `has exemption`
```

| Syntax             | Semantics                            |
| ------------------ | ------------------------------------ |
| `...` (three dots) | Implicit AND (asyndetic conjunction) |
| `..` (two dots)    | Implicit OR (asyndetic disjunction)  |

## Ditto Syntax

Strunk & White said: "Omit needless words". Edward Tufte talked about "data-ink".

L4 introduces "ditto syntax". A caret (`^`) expands to the word appearing directly above it, in the same column.

Judicious use of this convention improves the readability of multiline
expressions that would otherwise be over-noised with boilerplate.
Linguistically, this reads as an example of "conjunction reduction",
or "ellipsis". We use a caret instead of a literal ellipsis ("...").

## AKA

Aliases can be created inline by inserting an `AKA xxx` after an expression.
