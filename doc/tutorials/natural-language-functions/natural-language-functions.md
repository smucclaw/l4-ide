# Using Infix, Postfix, and Mixfix Functions

This tutorial shows how to define and call functions that read like natural language. L4's **mixfix operators** let you write code that mirrors the structure of legal prose.

## Overview

Traditional programming languages use **prefix notation** where the function name comes first:

```
add(3, 5)
isEligible(person, program)
```

L4 supports multiple calling patterns that read more naturally:

| Pattern     | Example                              | Natural Reading                  |
| ----------- | ------------------------------------ | -------------------------------- |
| **Prefix**  | `` `add` 3 5 ``                      | "add 3 and 5"                    |
| **Infix**   | ``3 `plus` 5``                       | "3 plus 5"                       |
| **Postfix** | `` 50 `percent` ``                   | "50 percent"                     |
| **Mixfix**  | ``alice `is eligible for` medicare`` | "alice is eligible for medicare" |

## Prerequisites

- Basic understanding of L4 functions (see [Your First L4 File](../getting-started/first-l4-file.md))
- Familiarity with GIVEN and MEANS keywords (see [MEANS](../../reference/functions/MEANS.md))

## Infix Functions

An **infix** function takes two arguments with the function name between them.

### Definition

**Example file:** [natural-language-example.l4](natural-language-example.l4)

```l4
GIVEN a IS A NUMBER, b IS A NUMBER
GIVETH A NUMBER
a `plus` b MEANS a + b
```

Notice the pattern: `param `keyword` param`. The parameters `a` and `b` come from the GIVEN clause. The backticked `` `plus` `` becomes the function name.

### Calling Infix Functions

You can call infix functions in two ways:

```l4
-- Infix style (natural reading)
#EVAL 3 `plus` 5

-- Prefix style (also works)
#EVAL `plus` 3 5
```

Both return `8`.

### Multi-Word Keywords

Backticks allow spaces in function names, enabling very natural syntax:

```l4
GIVEN person IS A STRING, program IS A STRING
GIVETH A BOOLEAN
person `is eligible for` program MEANS TRUE

-- Call site reads like English
#EVAL "Alice" `is eligible for` "Medicare"
```

## Postfix Functions

A **postfix** function takes one argument that appears before the function name.

### Definition

```l4
GIVEN amount IS A NUMBER
GIVETH A NUMBER
amount `percent` MEANS amount / 100
```

### Calling Postfix Functions

```l4
-- Postfix style
#EVAL 50 `percent`

-- Prefix style alternative
#EVAL `percent` 50
```

Both return `0.5`.

### Single-Word Operators

For single-word function names, backticks are optional at the call site:

```l4
GIVEN n IS A NUMBER
n squared MEANS n * n

-- Both work:
#EVAL 5 squared
#EVAL 5 `squared`
```

## Mixfix Functions (Ternary and Beyond)

**Mixfix** functions have keywords interspersed with multiple arguments. They can have any number of parameters.

### Ternary Mixfix (Three Arguments)

```l4
GIVEN cond IS A BOOLEAN, thenVal IS A NUMBER, elseVal IS A NUMBER
GIVETH A NUMBER
`myif` cond `mythen` thenVal `myelse` elseVal MEANS
  IF cond THEN thenVal ELSE elseVal

-- Call with full mixfix syntax
#EVAL `myif` TRUE `mythen` 42 `myelse` 0
```

This function starts with a keyword (`` `myif` ``), so the first token isn't an argument.

### Quaternary Mixfix (Four Arguments)

```l4
GIVEN mummy IS A STRING, daddy IS A STRING, baby IS A STRING, nickname IS A STRING
GIVETH A STRING
mummy `and` daddy `had` baby `called` nickname MEANS baby

-- Call site
#EVAL "Alice" `and` "Bob" `had` "Charlie" `called` "Chuck"
```

### Higher Arity

You can define functions with any number of parameters:

```l4
GIVEN a IS A NUMBER, b IS A NUMBER, c IS A NUMBER, d IS A NUMBER, e IS A NUMBER
GIVETH A NUMBER
a `op1` b `op2` c `op3` d `op4` e MEANS a + b + c + d + e

#EVAL 1 `op1` 2 `op2` 3 `op3` 4 `op4` 5
-- Returns: 15
```

## Multi-Line Layout

Mixfix operators can span multiple lines when keywords align with the operand's indentation:

```l4
GIVEN base IS A NUMBER, exponent IS A NUMBER
base `raised to` exponent MEANS base * exponent

DECIDE demo_multiline base exponent IS
  base
  `raised to`
  exponent
```

This is useful for complex expressions with long keywords.

## Practical Examples

### Legal Eligibility Rules

```l4
DECLARE Person HAS
  name IS A STRING
  age IS A NUMBER
  income IS A NUMBER

GIVEN person IS A Person, threshold IS A NUMBER
GIVETH A BOOLEAN
person `has income above` threshold MEANS
  person's income > threshold

GIVEN person IS A Person
GIVETH A BOOLEAN
person `is an adult` MEANS
  person's age >= 18
```

### Financial Calculations

```l4
GIVEN principal IS A NUMBER, rate IS A NUMBER, years IS A NUMBER
GIVETH A NUMBER
principal `at` rate `percent for` years `years` MEANS
  principal * (1 + rate / 100) * years
```

### Range Checks

```l4
GIVEN lower IS A NUMBER, value IS A NUMBER, upper IS A NUMBER
GIVETH A BOOLEAN
lower `<=` value `<=` upper MEANS
  lower <= value AND value <= upper

#EVAL 0 `<=` 5 `<=` 10
-- Returns: TRUE
```

## Best Practices

### Use Natural Language Names

L4's target audience is legal professionals. Write functions that read like legal prose:

```l4
-- ✅ Good: reads like legal text
person `is eligible for benefits` MEANS ...
amount `exceeds the threshold` MEANS ...

-- ❌ Poor: reads like code
isEligible person MEANS ...
exceedsThreshold amount MEANS ...
```

### Use Backticks for Multi-Word Names

```l4
-- ✅ Good: clear, readable
`the applicant` `has submitted a valid claim` MEANS ...

-- ❌ Avoid: hard to read
theApplicant hasSubmittedAValidClaim MEANS ...
```

### Define Mixfix Operators in WHERE Clauses

When defining helper mixfix operators inside a function, use WHERE (not LET):

```l4
-- ✅ Correct: mixfix in WHERE
GIVEN radius IS A NUMBER
GIVETH A NUMBER
`circle area` radius MEANS
  radius `squared` TIMES pi
  WHERE
    pi MEANS 3.14159
    GIVEN r IS A NUMBER
    r `squared` MEANS r * r

-- ❌ Incorrect: mixfix operators in LET won't work
-- LET blocks don't register mixfix operators
```

### Use Parentheses for Complex Expressions

When nesting mixfix calls, use parentheses for clarity:

```l4
#EVAL (1 + 2) `plus` (3 + 4)
#EVAL `myif` (x > 0) `mythen` (x * 2) `myelse` 0
```

## Complete Example File

See the full working example: [natural-language-example.l4](natural-language-example.l4)

## Summary

| Pattern | Definition                      | Call Site             |
| ------- | ------------------------------- | --------------------- |
| Infix   | ``a `keyword` b MEANS ...``     | ``x `keyword` y``     |
| Postfix | ``a `keyword` MEANS ...``       | `` x `keyword` ``     |
| Prefix  | `` `keyword` a b MEANS ... ``   | `` `keyword` x y ``   |
| Mixfix  | ``a `kw1` b `kw2` c MEANS ...`` | ``x `kw1` y `kw2` z`` |

## Related Documentation

- **[DECIDE](../../reference/functions/DECIDE.md)** - Function declaration keyword
- **[MEANS](../../reference/functions/MEANS.md)** - Function body keyword
- **[GIVEN](../../reference/functions/GIVEN.md)** - Parameter declaration
- **[Identifiers](../../reference/syntax/identifier-example.l4)** - Backtick syntax
