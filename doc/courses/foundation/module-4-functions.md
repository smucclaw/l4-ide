# Module 4: Functions

In this module, you'll learn how to define reusable functions in L4.

## Learning Objectives

By the end of this module, you will be able to:

- Define functions with GIVEN and GIVETH
- Use DECIDE and MEANS appropriately
- Create local definitions with WHERE
- Write recursive functions
- Understand function application

---

## Function Basics

The complete working examples are in [module-4-examples.l4](module-4-examples.l4).

### The Structure of a Function

Every L4 function has these parts:

```l4
GIVEN x IS A NUMBER           -- Parameters (inputs)
      y IS A NUMBER
GIVETH A NUMBER               -- Return type (output)
`the sum of x and y` MEANS    -- Name and definition
    x + y
```

### DECIDE vs MEANS

Both define the same thing—use whichever reads better:

```l4
-- These are equivalent
DECIDE `the person is an adult` IF age >= 18
`the person is an adult` MEANS age >= 18
DECIDE `the person is an adult` IS age >= 18
```

| Syntax                | Best for                     |
| --------------------- | ---------------------------- |
| `DECIDE name IS expr` | Rules, decisions, conditions |
| `DECIDE name IF expr` | Boolean predicates           |
| `name MEANS expr`     | Definitions, computations    |

### Type Signatures

The `GIVETH` clause declares what type the function returns:

- `GIVETH A NUMBER` — Returns a number
- `GIVETH A STRING` — Returns a string
- `GIVETH A BOOLEAN` — Returns true/false
- `GIVETH A Person` — Returns a Person record
- `GIVETH A LIST OF NUMBER` — Returns a list of numbers
- `GIVETH A MAYBE NUMBER` — Returns a number or nothing

---

## Function Parameters

### Multiple Parameters

List parameters with `GIVEN`, separated by newlines or commas:

```l4
GIVEN firstName IS A STRING
      lastName IS A STRING
      age IS A NUMBER
GIVETH A Person
`create person` MEANS Person firstName lastName age
```

### Polymorphic Functions

Use `TYPE` for functions that work with any type:

```l4
GIVEN a IS A TYPE
      x IS AN a
      y IS AN a
GIVETH AN a
`the first of` MEANS x
```

---

## Calling Functions

### Simple Function Calls

```l4
-- Define a function
GIVEN n IS A NUMBER
GIVETH A NUMBER
`the square of` MEANS n * n

-- Call it
#EVAL `the square of` 5        -- Result: 25
#EVAL `the square of` (3 + 2)  -- Result: 25
```

### Multi-Argument Calls

```l4
-- Arguments separated by spaces
#EVAL `add` 3 5       -- Result: 8
```

---

## Local Definitions with WHERE

Use `WHERE` to define helper values and functions:

```l4
GIVEN principal IS A NUMBER
      rate IS A NUMBER
      years IS A NUMBER
GIVETH A NUMBER
`the compound interest` MEANS
    principal * (factor ^ years)
    WHERE
        factor MEANS 1 + rate
```

### Multiple Local Definitions

```l4
`the monthly payment` MEANS
    loan * (monthlyRate * compoundFactor) / (compoundFactor - 1)
    WHERE
        monthlyRate MEANS annualRate / 12
        compoundFactor MEANS (1 + monthlyRate) ^ months
```

---

## Recursive Functions

L4 supports recursion—functions that call themselves:

### Simple Recursion

```l4
GIVEN n IS A NUMBER
GIVETH A NUMBER
`the factorial of` MEANS
    IF n <= 1
    THEN 1
    ELSE n * `the factorial of` (n - 1)

#EVAL `the factorial of` 5  -- Result: 120
```

### List Recursion

```l4
GIVEN xs IS A LIST OF NUMBER
GIVETH A NUMBER
`the sum of` MEANS
    CONSIDER xs
    WHEN EMPTY THEN 0
    WHEN x FOLLOWED BY rest THEN x + `the sum of` rest
```

---

## Higher-Order Functions

Functions that take or return other functions.

**Note:** You must `IMPORT prelude` to use `map`, `filter`, `all`, `any`, etc.

### Anonymous Functions (Lambdas)

Use `GIVEN ... YIELD`:

```l4
-- Double each number
map (GIVEN n YIELD n * 2) (LIST 1, 2, 3)
-- Result: LIST 2, 4, 6

-- Filter positive numbers
filter (GIVEN n YIELD n > 0) (LIST -1, 2, -3, 4)
-- Result: LIST 2, 4
```

---

## Real-World Example: Loan Calculator

For a complete real-world example, see the capstone module: [module-6-examples.l4](module-6-examples.l4).

---

## Function Composition

Build complex functions from simpler ones by defining helpers in WHERE blocks and calling other functions.

---

## Exercises

Try creating your own L4 files based on these requirements. Use [module-4-examples.l4](module-4-examples.l4) as a reference.

### Exercise 1: Simple Function

Write a function that calculates the area of a rectangle.

### Exercise 2: Function with WHERE

Write a function that calculates the area of a circle using π ≈ 3.14159.

### Exercise 3: Recursive Function

Write a recursive function to calculate the sum of a list of numbers.

### Exercise 4: Higher-Order Function

Use `filter` to get all numbers greater than 10 from a list. (Remember to `IMPORT prelude`.)

---

## Common Mistakes

### 1. Missing Return Type

```l4
-- ❌ Wrong: No GIVETH
GIVEN n IS A NUMBER
`the square of` MEANS n * n

-- ✅ Right: Include GIVETH
GIVEN n IS A NUMBER
GIVETH A NUMBER
`the square of` MEANS n * n
```

### 2. Wrong Argument Order in Calls

```l4
-- Definition
GIVEN x IS A NUMBER
      y IS A NUMBER
GIVETH A NUMBER
`subtract` MEANS x - y

-- ❌ Wrong: Gets 3 - 10 = -7, not 10 - 3
#EVAL `subtract` 3 10

-- ✅ Right: Match the order in GIVEN
#EVAL `subtract` 10 3  -- Gets 10 - 3 = 7
```

### 3. Missing Parentheses in Function Calls

```l4
-- ❌ Wrong: f applied to g, not to result of g x
f g x

-- ✅ Right: Apply g to x, then f to result
f (g x)
```

---

## Summary

| Concept              | Syntax                                     |
| -------------------- | ------------------------------------------ |
| Function definition  | `GIVEN params GIVETH Type name MEANS expr` |
| Decision function    | `DECIDE name IF condition`                 |
| Local definitions    | `expr WHERE localDef MEANS value`          |
| Recursion            | Function calls itself in definition        |
| Lambda               | `GIVEN x YIELD expression`                 |
| Function application | `functionName arg1 arg2`                   |

---

## What's Next?

In [Module 5: Regulative Rules](module-5-regulative.md), you'll learn how to model legal obligations, permissions, and prohibitions using L4's deontic constructs.
