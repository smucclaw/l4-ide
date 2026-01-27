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

### The Structure of a Function

Every L4 function has these parts:

```l4
GIVEN x IS A NUMBER           -- Parameters (inputs)
      y IS A NUMBER
GIVETH A NUMBER               -- Return type (output)
DECIDE `add numbers` IS       -- Name and definition
    x + y
```

Or equivalently with `MEANS`:

```l4
GIVEN x IS A NUMBER
      y IS A NUMBER
GIVETH A NUMBER
`add numbers` MEANS x + y
```

### DECIDE vs MEANS

Both define the same thing—use whichever reads better:

| Syntax                | Best for                     |
| --------------------- | ---------------------------- |
| `DECIDE name IS expr` | Rules, decisions, conditions |
| `DECIDE name IF expr` | Boolean predicates           |
| `name MEANS expr`     | Definitions, computations    |

```l4
-- These are equivalent
DECIDE `is adult` IF age >= 18
`is adult` MEANS age >= 18
DECIDE `is adult` IS age >= 18
```

### Type Signatures

The `GIVETH` clause declares what type the function returns:

```l4
GIVETH A NUMBER      -- Returns a number
GIVETH A STRING      -- Returns a string
GIVETH A BOOLEAN     -- Returns true/false
GIVETH A Person      -- Returns a Person record
GIVETH A LIST OF NUMBER  -- Returns a list of numbers
GIVETH A MAYBE NUMBER    -- Returns a number or nothing
```

---

## Function Parameters

### Multiple Parameters

List parameters with `GIVEN`, separated by newlines or commas:

```l4
-- Newline style (preferred for many parameters)
GIVEN firstName IS A STRING
      lastName IS A STRING
      age IS A NUMBER
GIVETH A Person
`create person` MEANS Person firstName lastName age

-- Comma style
GIVEN firstName IS A STRING, lastName IS A STRING
GIVETH A STRING
`full name` MEANS CONCAT firstName (CONCAT " " lastName)
```

### Polymorphic Functions

Use `TYPE` for functions that work with any type:

```l4
GIVEN a IS A TYPE
      x IS AN a
      y IS AN a
GIVETH AN a
`first of` MEANS x
```

This function works with any type—numbers, strings, records, etc.

---

## Calling Functions

### Simple Function Calls

```l4
-- Define a function
GIVEN n IS A NUMBER
GIVETH A NUMBER
square MEANS n * n

-- Call it
#EVAL square 5        -- Result: 25
#EVAL square (3 + 2)  -- Result: 25
```

### Multi-Argument Calls

```l4
-- Define
GIVEN x IS A NUMBER
      y IS A NUMBER
GIVETH A NUMBER
`add` MEANS x + y

-- Call (arguments separated by spaces)
#EVAL `add` 3 5       -- Result: 8
```

### Named Argument Syntax

For clarity, use `OF` with single arguments or `WITH` for named arguments:

```l4
-- Using OF
`is adult` OF person

-- Using WITH (for record-like calls)
`create order` WITH
    buyer IS alice
    seller IS bob
    amount IS 1000
```

---

## Local Definitions with WHERE

Use `WHERE` to define helper values and functions:

```l4
GIVEN principal IS A NUMBER
      rate IS A NUMBER
      years IS A NUMBER
GIVETH A NUMBER
`compound interest` MEANS
    principal * (factor `to the power of` years)
    WHERE
        factor MEANS 1 + rate
```

### Multiple Local Definitions

```l4
GIVEN loan IS A NUMBER
      annualRate IS A NUMBER
      months IS A NUMBER
GIVETH A NUMBER
`monthly payment` MEANS
    loan * (monthlyRate * compoundFactor) / (compoundFactor - 1)
    WHERE
        monthlyRate MEANS annualRate / 12
        compoundFactor MEANS (1 + monthlyRate) `to the power of` months
```

### Local Functions

You can define local functions too:

```l4
GIVEN numbers IS A LIST OF NUMBER
GIVETH A NUMBER
`sum of squares` MEANS
    sumList (map square numbers)
    WHERE
        GIVEN n IS A NUMBER
        GIVETH A NUMBER
        square MEANS n * n

        GIVEN xs IS A LIST OF NUMBER
        GIVETH A NUMBER
        sumList MEANS
            CONSIDER xs
            WHEN EMPTY THEN 0
            WHEN x FOLLOWED BY rest THEN x + sumList rest
```

---

## Recursive Functions

L4 supports recursion—functions that call themselves:

### Simple Recursion

```l4
GIVEN n IS A NUMBER
GIVETH A NUMBER
factorial MEANS
    IF n <= 1
    THEN 1
    ELSE n * factorial (n - 1)

#EVAL factorial 5  -- Result: 120
```

### List Recursion

```l4
GIVEN a IS A TYPE
      xs IS A LIST OF a
GIVETH A NUMBER
length MEANS
    CONSIDER xs
    WHEN EMPTY THEN 0
    WHEN x FOLLOWED BY rest THEN 1 + length rest

#EVAL length (LIST 1, 2, 3, 4, 5)  -- Result: 5
```

### Mutual Recursion

Functions can call each other:

```l4
GIVEN n IS A NUMBER
GIVETH A BOOLEAN
isEven MEANS
    IF n = 0 THEN TRUE
    ELSE isOdd (n - 1)

GIVEN n IS A NUMBER
GIVETH A BOOLEAN
isOdd MEANS
    IF n = 0 THEN FALSE
    ELSE isEven (n - 1)
```

---

## Higher-Order Functions

Functions that take or return other functions:

### Taking Functions as Arguments

```l4
-- map: apply a function to each element
GIVEN a IS A TYPE
      b IS A TYPE
      f IS A FUNCTION FROM a TO b
      xs IS A LIST OF a
GIVETH A LIST OF b
map MEANS
    CONSIDER xs
    WHEN EMPTY THEN EMPTY
    WHEN x FOLLOWED BY rest THEN (f x) FOLLOWED BY (map f rest)
```

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

```l4
IMPORT daydate

-- Type definitions
DECLARE Money
    HAS amount IS A NUMBER
        currency IS A STRING

DECLARE LoanTerms
    HAS principal IS A Money
        annualRate IS A NUMBER
        months IS A NUMBER

-- Helper: Currency constructor
GIVEN amount IS A NUMBER
GIVETH A Money
USD MEANS Money amount "USD"

-- Calculate monthly payment using amortization formula
GIVEN terms IS A LoanTerms
GIVETH A Money
`monthly payment` MEANS
    Money monthlyAmount (terms's principal's currency)
    WHERE
        p MEANS terms's principal's amount
        r MEANS terms's annualRate / 12
        n MEANS terms's months

        -- PMT formula: P * (r(1+r)^n) / ((1+r)^n - 1)
        compoundFactor MEANS `power` (1 + r) n
        monthlyAmount MEANS p * (r * compoundFactor) / (compoundFactor - 1)

-- Power function
GIVEN base IS A NUMBER
      exp IS A NUMBER
GIVETH A NUMBER
`power` MEANS
    IF exp = 0 THEN 1
    ELSE IF exp = 1 THEN base
    ELSE base * (`power` base (exp - 1))

-- Total interest over loan life
GIVEN terms IS A LoanTerms
GIVETH A Money
`total interest` MEANS
    Money interestAmount (terms's principal's currency)
    WHERE
        monthly MEANS `monthly payment` terms
        totalPaid MEANS monthly's amount * terms's months
        interestAmount MEANS totalPaid - terms's principal's amount

-- Example
exampleLoan MEANS LoanTerms (USD 100000) 0.06 360  -- $100k at 6% for 30 years

#EVAL `monthly payment` exampleLoan
-- Result: approximately $599.55

#EVAL `total interest` exampleLoan
-- Result: approximately $115,838
```

---

## Function Composition

Build complex functions from simpler ones:

```l4
-- Validate a charity application
GIVEN charity IS A RegisteredCharity
GIVETH A BOOLEAN
`is valid application` MEANS
    `has valid name` charity
    AND `has charitable purposes` charity
    AND `has required financials` charity
    WHERE
        `has valid name` MEANS NOT charity's name EQUALS ""

        `has charitable purposes` MEANS
            length (charity's purposes) > 0
            AND all (GIVEN p YIELD `is charitable` p) (charity's purposes)

        `has required financials` MEANS
            charity's latestIncome >= 0
```

---

## Exercises

### Exercise 1: Simple Function

Write a function that calculates the area of a rectangle.

<details>
<summary>Solution</summary>

```l4
GIVEN width IS A NUMBER
      height IS A NUMBER
GIVETH A NUMBER
`rectangle area` MEANS width * height

#EVAL `rectangle area` 5 10  -- Result: 50
```

</details>

### Exercise 2: Function with WHERE

Write a function that calculates the area of a circle using π ≈ 3.14159.

<details>
<summary>Solution</summary>

```l4
GIVEN radius IS A NUMBER
GIVETH A NUMBER
`circle area` MEANS pi * radius * radius
    WHERE pi MEANS 3.14159

#EVAL `circle area` 5  -- Result: ~78.54
```

</details>

### Exercise 3: Recursive Function

Write a recursive function to calculate the sum of a list of numbers.

<details>
<summary>Solution</summary>

```l4
GIVEN numbers IS A LIST OF NUMBER
GIVETH A NUMBER
`sum` MEANS
    CONSIDER numbers
    WHEN EMPTY THEN 0
    WHEN n FOLLOWED BY rest THEN n + `sum` rest

#EVAL `sum` (LIST 1, 2, 3, 4, 5)  -- Result: 15
```

</details>

### Exercise 4: Higher-Order Function

Use `filter` to get all numbers greater than 10 from a list.

<details>
<summary>Solution</summary>

```l4
GIVEN numbers IS A LIST OF NUMBER
GIVETH A LIST OF NUMBER
`greater than ten` MEANS
    filter (GIVEN n YIELD n > 10) numbers

#EVAL `greater than ten` (LIST 5, 15, 8, 20, 3)
-- Result: LIST 15, 20
```

</details>

---

## Common Mistakes

### 1. Missing Return Type

```l4
-- ❌ Wrong: No GIVETH
GIVEN n IS A NUMBER
square MEANS n * n

-- ✅ Right: Include GIVETH
GIVEN n IS A NUMBER
GIVETH A NUMBER
square MEANS n * n
```

### 2. Wrong Argument Order in Calls

```l4
-- Definition
GIVEN x IS A NUMBER
      y IS A NUMBER
GIVETH A NUMBER
`subtract` MEANS x - y

-- ❌ Wrong: Arguments in wrong order
#EVAL `subtract` 3 10  -- Gets 3 - 10 = -7, not 10 - 3

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
