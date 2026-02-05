# Module 3: Control Flow

In this module, you'll learn how to handle conditional logic, work with lists, and use boolean operators in L4.

## Learning Objectives

By the end of this module, you will be able to:

- Write conditional expressions with IF/THEN/ELSE
- Use pattern matching with CONSIDER
- Work with lists using prelude functions
- Combine boolean conditions with AND, OR, NOT
- Understand operator precedence

---

## Conditional Expressions

This is the complete working example to work along.

[module-3-examples.l4](module-3-examples.l4)

### Basic IF/THEN/ELSE

```l4
GIVEN age IS A NUMBER
GIVETH A STRING
`the age category` MEANS
    IF age < 18
    THEN "minor"
    ELSE "adult"
```

Every `IF` must have both `THEN` and `ELSE` branches—L4 requires you to handle all cases.

### Nested Conditionals with BRANCH

```l4
GIVEN score IS A NUMBER
GIVETH A STRING
`grade` MEANS
    BRANCH
        IF score >= 90 THEN "A"
        IF score >= 80 THEN "B"
        IF score >= 70 THEN "C"
        IF score >= 60 THEN "D"
        OTHERWISE "F"
```

### Conditional in Context

Conditionals are expressions—they can be used anywhere a value is expected:

```l4
message MEANS IF isActive THEN "Active" ELSE "Inactive"
```

---

## Pattern Matching with CONSIDER

For more complex branching, especially with enumeration types, use `CONSIDER`:

```l4
GIVEN status IS A Status
GIVETH A STRING
`the status message` MEANS
    CONSIDER status
    WHEN Active THEN "The account is active"
    WHEN Suspended reason THEN "Suspended: " + reason
    WHEN Closed THEN "The account has been closed"
```

### Pattern Matching on Lists

```l4
GIVEN items IS A LIST OF STRING
GIVETH A STRING
`describe the list` MEANS
    CONSIDER items
    WHEN EMPTY THEN "no items"
    WHEN first FOLLOWED BY EMPTY THEN "one item"
    WHEN first FOLLOWED BY rest THEN "multiple items"
```

The pattern `first FOLLOWED BY rest` destructures a list into its head and tail.

---

## Boolean Logic

### Basic Operators

| Operator          | Meaning           | Example       |
| ----------------- | ----------------- | ------------- |
| `AND` or `&&`     | Both must be true | `a AND b`     |
| `OR` or `\|\|`    | At least one true | `a OR b`      |
| `NOT`             | Negation          | `NOT a`       |
| `IMPLIES` or `=>` | If-then           | `a IMPLIES b` |

### Examples

```l4
-- AND: Both conditions must be true
DECIDE `the person can vote` IF age >= 18 AND isRegistered

-- OR: At least one condition must be true
DECIDE `the case needs review` IF amount > 10000 OR isHighRisk

-- NOT: Negation
DECIDE `the person is a minor` IF NOT age >= 18

-- IMPLIES: If first is true, second must be true
DECIDE `the rule holds` IF hasPermit IMPLIES paidFee
```

### Operator Precedence

From highest to lowest precedence:

1. `NOT` (tightest)
2. `AND`
3. `OR`
4. `IMPLIES` (loosest)

This means:

```l4
a OR b AND c        -- means: a OR (b AND c)
NOT a AND b         -- means: (NOT a) AND b
```

Use parentheses to make precedence explicit:

```l4
(a OR b) AND c      -- clear: OR first, then AND
```

---

## Comparison Operators

| Operator | Alternative    | Meaning               |
| -------- | -------------- | --------------------- |
| `=`      | `EQUALS`       | Equal to              |
| `>`      | `GREATER THAN` | Greater than          |
| `<`      | `LESS THAN`    | Less than             |
| `>=`     | `AT LEAST`     | Greater than or equal |
| `<=`     | `AT MOST`      | Less than or equal    |

### Examples

```l4
-- Numeric comparisons
age >= 18
amount LESS THAN 1000
score AT LEAST 70

-- String comparison
status EQUALS "Active"
name = "Alice"

-- Boolean comparison (often unnecessary)
isActive EQUALS TRUE    -- same as just: isActive
isActive EQUALS FALSE   -- same as: NOT isActive
```

---

## Working with Lists

L4 provides prelude functions for working with lists. **Note:** You must `IMPORT prelude` to use these functions.

### Creating Lists

```l4
numbers MEANS LIST 1, 2, 3, 4, 5
empty MEANS EMPTY
```

### Common List Functions

| Function | Purpose          | Example           |
| -------- | ---------------- | ----------------- |
| `null`   | Is empty?        | `null myList`     |
| `map`    | Transform each   | `map f myList`    |
| `filter` | Keep matching    | `filter f myList` |
| `all`    | All satisfy?     | `all f myList`    |
| `any`    | Any satisfy?     | `any f myList`    |
| `elem`   | Check membership | `elem 3 myList`   |

### Using Quantifiers

```l4
-- All elements positive?
all (GIVEN n YIELD n > 0) numbers

-- Any element negative?
any (GIVEN n YIELD n < 0) numbers
```

The `GIVEN ... YIELD` creates an anonymous function (lambda).

### Important: Parentheses with Field Access

When passing field access to functions, use parentheses:

```l4
-- ❌ Wrong: Parser confusion
all (GIVEN g YIELD g's age >= 18) charity's governors

-- ✅ Right: Parentheses around the list argument
all (GIVEN g YIELD g's age >= 18) (charity's governors)
```

This is one of the most common mistakes in L4!

---

## Combining Conditions in Legal Rules

```l4
GIVEN person IS A Person
      charity IS A `Registered Charity`
DECIDE `the person can be a governor` IF
    person's age >= 18
    AND NOT person's `is bankrupt`
    AND NOT any (GIVEN c YIELD NOT c's `is spent`) (person's convictions)
```

---

## Real-World Example: Eligibility Check

```l4
-- Helper: Check for unspent convictions
GIVEN person IS A Person
GIVETH A BOOLEAN
`the person has an unspent conviction` MEANS
    any (GIVEN c YIELD c's `is spent` EQUALS FALSE) (person's convictions)

-- Main eligibility rule
GIVEN person IS A Person
GIVETH A BOOLEAN
DECIDE `the person is eligible for the position` IF
    person's age >= 21
    AND NOT person's `is bankrupt`
    AND NOT `the person has an unspent conviction` person
```

---

## Exercises

### Exercise 1: Nested Conditional

Write a function that categorizes an amount as "small" (< 100), "medium" (100-999), or "large" (>= 1000).

### Exercise 2: List Validation

Write a function that checks if all items in a list of numbers are positive. (Remember to `IMPORT prelude`.)

### Exercise 3: Complex Condition

Write a rule: "A person can purchase alcohol if they are at least 21, have valid ID, and are not on the banned list."

---

## Common Mistakes

### 1. Missing ELSE

```l4
-- ❌ Wrong: No ELSE branch
result MEANS IF condition THEN "yes"

-- ✅ Right: Both branches required
result MEANS IF condition THEN "yes" ELSE "no"
```

### 2. Precedence Confusion

```l4
-- ❌ Confusing: What does this mean?
a OR b AND c IMPLIES d

-- ✅ Clear: Use parentheses
(a OR (b AND c)) IMPLIES d
```

### 3. Forgetting Parentheses with Functions

```l4
-- ❌ Wrong: Parser error
length person's items > 0

-- ✅ Right: Parentheses around field access
length (person's items) > 0
```

### 4. Forgetting to IMPORT prelude

Functions like `all`, `any`, `filter`, `map` require `IMPORT prelude`.

---

## Summary

| Concept          | Syntax                                   |
| ---------------- | ---------------------------------------- |
| Conditional      | `IF condition THEN result1 ELSE result2` |
| Pattern match    | `CONSIDER expr WHEN pattern THEN result` |
| List destructure | `WHEN first FOLLOWED BY rest THEN ...`   |
| Boolean AND      | `condition1 AND condition2`              |
| Boolean OR       | `condition1 OR condition2`               |
| Boolean NOT      | `NOT condition`                          |
| Check all        | `all (GIVEN x YIELD condition) list`     |
| Check any        | `any (GIVEN x YIELD condition) list`     |
| List membership  | `elem item list`                         |

---

## What's Next?

In [Module 4: Functions](module-4-functions.md), you'll learn how to define reusable functions, use local definitions with WHERE, and build more complex computations.
