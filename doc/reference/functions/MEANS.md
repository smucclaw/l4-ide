# MEANS

Introduces the body of a function or decision. Connects a function name to its definition.

## Syntax

```l4
name MEANS expression
DECIDE name MEANS expression
```

## Purpose

MEANS separates the function signature (name and parameters) from its implementation (the expression that computes the result).

## Examples

**Example file:** [means-example.l4](means-example.l4)

### Basic Usage

```l4
greeting MEANS "Hello, World!"

GIVEN x IS A NUMBER
double x MEANS x TIMES 2
```

### With DECIDE

```l4
DECIDE answer MEANS 42

GIVEN n IS A NUMBER
DECIDE square n MEANS n TIMES n
```

### Multi-line Bodies

```l4
GIVEN n IS A NUMBER
factorial n MEANS
  IF n EQUALS 0
  THEN 1
  ELSE n TIMES factorial (n MINUS 1)
```

### With WHERE Clauses

```l4
circumference radius MEANS
  2 TIMES pi TIMES radius
  WHERE
    pi MEANS 3.14159
```

### In Record Fields (Computed Fields / Methods)

MEANS can also appear inside DECLARE HAS to define computed fields — derived attributes that are calculated automatically from other fields in the record:

```l4
DECLARE Person HAS
    `birth year` IS A NUMBER
    `as at year` IS A NUMBER
    `age`        IS A NUMBER
        MEANS `as at year` - `birth year`
    `adult`      IS A BOOLEAN
        MEANS `age` >= 18
```

The computed field's MEANS body can reference any sibling field by name. It can also call external functions (using OF for multi-argument calls), and use WHERE or LET/IN for local bindings.

See **[DECLARE - Computed Fields](../types/DECLARE.md#computed-fields-methods)** for full documentation.

## Alternative Forms

### Using IS

For simple definitions, IS can replace MEANS:

```l4
answer IS 42
DECIDE pi IS 3.14159
```

### Using BE or MEAN

Alternative keywords (for isomorphism with legal text):

```l4
-- In LET expressions
LET x BE 5 IN x TIMES 2
LET y MEAN 10 IN y PLUS 1
```

## Related Keywords

- **[DECIDE](DECIDE.md)** - Declares a decision
- **[WHERE](WHERE.md)** - Local definitions

## Tutorials

- **[Using Infix, Postfix, and Mixfix Functions](../../tutorials/natural-language-functions/natural-language-functions.md)** - Define functions that read like natural language
