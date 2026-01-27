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
- **[IS](IS.md)** - Alternative for simple definitions
- **[WHERE](WHERE.md)** - Local definitions
- **[BE](BE.md)** - Alternative form
