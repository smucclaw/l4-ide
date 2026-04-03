# YIELD

The `YIELD` keyword creates **anonymous functions** (also known as lambdas). It connects parameters declared with `GIVEN` to an expression body, without naming the function.

## Syntax

```
GIVEN parameters YIELD expression
```

## Basic Usage

### Single Parameter Lambda

```l4
GIVEN x YIELD x + 1
```

This creates a function that takes a number and returns that number plus one.

### Multiple Parameters

Parameters can be separated by commas:

```l4
GIVEN x, y YIELD x + y
```

Or using layout (one per line):

```l4
GIVEN a
      b
YIELD a * b
```

### Typed Parameters

You can specify types for lambda parameters:

```l4
GIVEN n IS A NUMBER YIELD n * 2
```

With multiple typed parameters:

```l4
GIVEN name IS A STRING
      age IS A NUMBER
YIELD name
```

## Common Use Cases

### With Higher-Order Functions

Lambdas are most commonly used with higher-order functions from the prelude:

**Universal quantification (all elements satisfy condition):**

```l4
GIVEN numbers IS A LIST OF NUMBER
GIVETH A BOOLEAN
`all positive` MEANS all (GIVEN n YIELD n > 0) numbers
```

**Existential quantification (some element satisfies condition):**

```l4
GIVEN numbers IS A LIST OF NUMBER
GIVETH A BOOLEAN
`has negative` MEANS any (GIVEN n YIELD n < 0) numbers
```

**Transforming lists (map):**

```l4
GIVEN numbers IS A LIST OF NUMBER
GIVETH A LIST OF NUMBER
`doubled` MEANS map (GIVEN n YIELD n * 2) numbers
```

**Filtering lists:**

```l4
GIVEN numbers IS A LIST OF NUMBER
GIVETH A LIST OF NUMBER
`only positive` MEANS filter (GIVEN n YIELD n > 0) numbers
```

### With Record Types

Access record fields in lambda bodies:

```l4
DECLARE Person HAS
  name IS A STRING
  age IS A NUMBER

GIVEN people IS A LIST OF Person
GIVETH A BOOLEAN
`all adults` MEANS all (GIVEN p YIELD p's age >= 18) people
```

### Function Composition

Return functions from functions:

```l4
GIVEN a IS A TYPE
      b IS A TYPE
      c IS A TYPE
      f IS A FUNCTION FROM b TO c
      g IS A FUNCTION FROM a TO b
GIVETH A FUNCTION FROM a TO c
compose f g MEANS GIVEN x IS AN a YIELD f (g x)
```

## Parentheses Requirements

When passing lambdas to functions, you typically need parentheses around:

1. The lambda expression itself
2. The list being processed (especially with `'s` accessor)

```l4
-- ✅ Correct: parentheses around lambda and list
all (GIVEN p YIELD p's age >= 18) (people's members)

-- ❌ Wrong: missing parentheses around list
all (GIVEN p YIELD p's age >= 18) people's members
```

## YIELD vs MEANS

| Aspect        | YIELD                             | MEANS                       |
| ------------- | --------------------------------- | --------------------------- |
| Creates       | Anonymous function (lambda)       | Named function              |
| Used with     | GIVEN (inline)                    | GIVEN (top-level) or DECIDE |
| Function name | None                              | Required                    |
| Typical use   | Passing to higher-order functions | Defining reusable functions |

**Named function with MEANS:**

```l4
GIVEN n IS A NUMBER
GIVETH A NUMBER
double n MEANS n * 2
```

**Anonymous function with YIELD:**

```l4
DECIDE double IS GIVEN n YIELD n * 2
```

Both create equivalent functions, but YIELD is preferred for inline use.

## Full Example

**Example:** [yield-example.l4](yield-example.l4)

## See Also

- **[GIVEN](GIVEN.md)** - Parameter declarations (used with YIELD)
- **[MEANS](MEANS.md)** - Named function definitions
- **[Functions Reference](README.md)** - Function overview
- **[Libraries: prelude](../libraries/prelude.md)** - Higher-order functions (`all`, `any`, `map`, `filter`)
