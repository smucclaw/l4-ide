# Basic Syntax


## Identifiers

Identifiers, such as function names and record attributes, are quoted using backticks. Where other programming languages might use `camelCase` or `snake_case`, L4 allows the use of `` `space separated words` `` to form a single identifier.

## Comments

Comments are written using the `--` syntax.

```l4
-- This is a comment
```

You can also use `{- .... -}` syntax.

## Textual Annotations

Annotations are paratextual: they are not an essential part of an L4
program, but inform various transformations of the program.

Annotations are enclosed within `[ square brackets ]`.

Annotations can be added to sections and other constructs.

```l4
§ `Section Head` [NLG annotation to section names are valid.]
```


## References

References can be added to various constructs using the `@`
symbol. Typically they are used to cite original sources.

```l4
@ref-src citations.csv
@ref url https://...
```




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

This method of grouping was inspired by legal sub-paragraphs and sub-lists. The main difference is in where the "OR" and "AND" words appear.


## Ditto Syntax

Strunk & White said: "Omit needless words". Edward Tufte talked about "data-ink".

L4 introduces "ditto syntax". A caret (`^`) expands to the word appearing directly above it, in the same column.

Judicious use of this convention improves the readability of multiline
expressions that would otherwise be over-noised with boilerplate.
Linguistically, this reads as an example of "conjunction reduction",
or "ellipsis". We use a caret instead of a literal ellipsis ("...").


## AKA

Aliases can be created inline by inserting an `AKA xxx` after an expression.

