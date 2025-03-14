# L4 Language Guide

## Introduction

L4 is a functional programming language inspired by Haskell. It is indentation-sensitive, meaning that layout is used for grouping constructs. This guide introduces the syntax and semantics of L4.

## Basic Syntax

### Strings and Terms

Strings are quoted using double-quotes: `"Alice Avocado"` is a string.

Function names and record attributes are quoted using backticks. Where other programming languages might use `camelCase` or `snake_case`, L4 allows the use of `space separated words` to form a single token.

### Sections

Sections are denoted by the `§` symbol followed by the section title. Subsections can be created by repeating the `§` symbol.

```l4
§ Section Title
§§ Subsection Title
§§§ Sub-subsection Title
```

### Declarations

#### Type Declarations

Types can be declared using the `DECLARE` keyword.

```l4
DECLARE Bool IS BOOLEAN
DECLARE Colour IS ONE OF red, green, blue
```

#### Function Declarations

Functions are declared using the `GIVEN` and `GIVETH` keywords.

```l4
GIVEN x IS A NUMBER
      y IS A NUMBER
GIVETH A NUMBER
add x y MEANS x + y
```

### Expressions

Expressions are written in a straightforward manner, similar to Haskell.

```l4
x MEANS 2 + 2
y MEANS x * x
```

### Conditionals

Conditionals are written using the `IF`, `THEN`, and `ELSE` keywords.

```l4
GIVEN n IS A NUMBER
GIVETH A NUMBER
factorial n MEANS
  IF n EQUALS 0
  THEN 1
  ELSE n * factorial (n - 1)
```

### Lists

Lists are created using the `LIST` keyword and can be manipulated using standard list operations.

```l4
GIVETH A LIST OF NUMBER
primes MEANS LIST 2, 3, 5, 7, 11
```

### Comments

Comments are written using the `--` syntax.

```l4
-- This is a comment
```

### Records

Records in L4 are used to group related data together. They can be defined using the `WITH ... IS ...` syntax or the shorter `Constructor OF ...` syntax.

#### Record Definition

Record types are declared using `DECLARE ... HAS ... IS A ...` syntax.

```l4
DECLARE Person
  HAS name IS A STRING
      age  IS A NUMBER
```

Once the type has been declared, you can instantiate it by defining variables using `MEANS` syntax.

There are two ways to give the attributes.

Using `WITH ... IS ...` syntax:

```l4
alice MEANS
  Person WITH name IS "Alice Avocado"
              age  IS 21
```

This can also be written using the shorter `Constructor OF ...` syntax:

```l4
alice MEANS Person OF "Alice Avocado", 21
```

#### Accessing Record Fields

Fields of a record can be accessed using the possessive apostrophe-s (`'s`) notation:

```l4
GIVEN john IS A Person
GIVETH A STRING
johnsName MEANS john's name

GIVETH A NUMBER
johnsAge MEANS john's age
```

This is analogous to the use of a dot (`.`) used in most other languages for record accessors: `john.age` becomes `john's age`


## Advanced Features

### Ditto Syntax

Strunk & White said: "Omit needless words".

Edward Tufte talked about "data-ink".

Following these principles, L4's ditto syntax will expand a caret (`^`) to the word appearing directly above it.

Judicious use of this convention improves the readability of multiline expressions that would otherwise be over-noised with boilerplate.


### Annotations

Annotations can be added to various constructs using the `@` symbol.

```l4
@ref-src citations.csv
@ref-map
```

### Natural Language Generation (NLG)

NLG annotations can be added to sections and other constructs.

```l4
§ `Section Head` [NLG annotation to section names are valid.]
```

### Type-Directed Name Resolution (TDNR)

TDNR allows the same identifier to be in scope multiple times with different types.

```l4
ASSUME foo IS A NUMBER
ASSUME foo IS A BOOLEAN
ASSUME foo IS A STRING
```

## Examples

### Example 1: XOR Function

```l4
§ xor

GIVEN x IS A BOOLEAN, y IS A BOOLEAN
GIVETH A BOOLEAN
DECIDE xor x y IS
     x AND NOT y
  OR NOT x AND y
```

### Example 2: Type Declarations

```l4
§ `Type declarations`

DECLARE bool IS ONE OF
  true
  false

DECLARE bool2 IS ONE OF true, false

DECLARE BOOL IS ONE OF
  TRUE
  FALSE
```

### Example 3: Fibonacci Function

```l4
§ `Fibonacci function`

GIVEN n IS A NUMBER
GIVETH A NUMBER
DECIDE fibNaive n IS
  IF n EQUALS 0
  THEN 0
  ELSE IF n EQUALS 1
  THEN 1
  ELSE fibNaive (n - 1) + fibNaive (n - 2)

#EVAL fibNaive 20
```

## Conclusion

This guide provides an overview of the L4 language, including its syntax and semantics. For more detailed examples and advanced features, refer to the sample programs provided in the `examples` directory.

