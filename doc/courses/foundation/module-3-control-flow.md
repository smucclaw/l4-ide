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

### Basic IF/THEN/ELSE

```l4
GIVEN age IS A NUMBER
GIVETH A STRING
`age category` MEANS
    IF age < 18
    THEN "minor"
    ELSE "adult"
```

Every `IF` must have both `THEN` and `ELSE` branches—L4 requires you to handle all cases.

### Nested Conditionals

```l4
GIVEN score IS A NUMBER
GIVETH A STRING
`grade` MEANS
    IF score >= 90
    THEN "A"
    ELSE IF score >= 80
         THEN "B"
         ELSE IF score >= 70
              THEN "C"
              ELSE IF score >= 60
                   THEN "D"
                   ELSE "F"
```

Note how indentation shows the nesting structure.

### Conditional in Context

Conditionals are expressions—they can be used anywhere a value is expected:

```l4
DECIDE bonus IS IF performance EQUALS "excellent" THEN 5000 ELSE 1000

DECIDE message IS CONCAT "Status: " (IF isActive THEN "Active" ELSE "Inactive")
```

---

## Pattern Matching with CONSIDER

For more complex branching, especially with enumeration types, use `CONSIDER`:

```l4
GIVEN status IS A Status
GIVETH A STRING
`status message` MEANS
    CONSIDER status
    WHEN Active THEN "The account is active"
    WHEN Suspended reason THEN CONCAT "Suspended: " reason
    WHEN Closed THEN "The account has been closed"
    OTHERWISE "Unknown status"
```

### Pattern Matching on Lists

```l4
GIVEN items IS A LIST OF STRING
GIVETH A STRING
`describe list` MEANS
    CONSIDER items
    WHEN EMPTY THEN "No items"
    WHEN first FOLLOWED BY rest THEN CONCAT "First item: " first
```

The pattern `first FOLLOWED BY rest` destructures a list into its head and tail.

### Multiple Patterns

```l4
GIVEN items IS A LIST OF NUMBER
GIVETH A STRING
`list size` MEANS
    CONSIDER items
    WHEN EMPTY THEN "empty"
    WHEN x FOLLOWED BY EMPTY THEN "one item"
    WHEN x FOLLOWED BY y FOLLOWED BY rest THEN "two or more items"
```

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
DECIDE `can vote` IS age >= 18 AND isRegistered

-- OR: At least one condition must be true
DECIDE `needs review` IS amount > 10000 OR isHighRisk

-- NOT: Negation
DECIDE `is minor` IS NOT age >= 18

-- IMPLIES: If first is true, second must be true
DECIDE `rule holds` IS hasPermit IMPLIES paidFee
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
a AND b IMPLIES c   -- means: (a AND b) IMPLIES c
```

Use parentheses to make precedence explicit:

```l4
(a OR b) AND c      -- clear: OR first, then AND
a AND (b IMPLIES c) -- clear: IMPLIES first, then AND
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

L4 provides prelude functions for working with lists.

### Creating Lists

```l4
-- Inline list
numbers MEANS LIST 1, 2, 3, 4, 5

-- Empty list
empty MEANS LIST

-- Building lists with FOLLOWED BY (cons)
DECIDE myList IS 1 FOLLOWED BY 2 FOLLOWED BY 3 FOLLOWED BY EMPTY
```

### Common List Functions

| Function | Purpose          | Example           |
| -------- | ---------------- | ----------------- |
| `length` | Count elements   | `length myList`   |
| `elem`   | Check membership | `elem 3 myList`   |
| `head`   | First element    | `head myList`     |
| `tail`   | All but first    | `tail myList`     |
| `null`   | Is empty?        | `null myList`     |
| `map`    | Transform each   | `map f myList`    |
| `filter` | Keep matching    | `filter f myList` |
| `all`    | All satisfy?     | `all f myList`    |
| `any`    | Any satisfy?     | `any f myList`    |

### Using Quantifiers

To check if all or any elements satisfy a condition:

```l4
-- All elements positive?
all (GIVEN n YIELD n > 0) numbers

-- Any element negative?
any (GIVEN n YIELD n < 0) numbers

-- No elements zero?
NOT any (GIVEN n YIELD n = 0) numbers
```

The `GIVEN ... YIELD` creates an anonymous function (lambda).

### Important: Parentheses with Field Access

When passing field access to functions, use parentheses:

```l4
-- ❌ Wrong: Parser confusion
length charity's purposes > 0

-- ✅ Right: Parentheses around field access
length (charity's purposes) > 0

-- ❌ Wrong: Parser sees (all ... charity)'s governors
all (GIVEN g YIELD g's age >= 18) charity's governors

-- ✅ Right: Parentheses around the list argument
all (GIVEN g YIELD g's age >= 18) (charity's governors)
```

This is one of the most common mistakes in L4!

---

## Combining Conditions in Legal Rules

### Multiple Conditions

```l4
GIVEN person IS A Person
      charity IS A RegisteredCharity
DECIDE `can be governor` IF
    person's age >= 18
    AND NOT person's isBankrupt
    AND NOT any (GIVEN c YIELD NOT c's isSpent) (person's convictions)
    AND elem person (charity's nominatedGovernors)
```

### Complex Legal Test

```l4
GIVEN charity IS A RegisteredCharity
GIVETH A BOOLEAN
DECIDE `meets filing requirements` IF
    charity's status EQUALS Active
    AND length (charity's purposes) > 0
    AND all (GIVEN p YIELD `is charitable` p) (charity's purposes)
    AND charity's latestFinancials's year = currentYear - 1
```

---

## Real-World Example: Eligibility Check

```l4
-- Types
DECLARE Conviction
    HAS description IS A STRING
        isSpent IS A BOOLEAN

DECLARE Person
    HAS name IS A STRING
        age IS A NUMBER
        isBankrupt IS A BOOLEAN
        convictions IS A LIST OF Conviction

-- Helper: Check for unspent convictions
GIVEN person IS A Person
GIVETH A BOOLEAN
`has unspent conviction` MEANS
    any (GIVEN c YIELD c's isSpent EQUALS FALSE) (person's convictions)

-- Main eligibility rule
GIVEN person IS A Person
GIVETH A BOOLEAN
DECIDE `is eligible for position` IF
    person's age >= 21
    AND NOT person's isBankrupt
    AND NOT `has unspent conviction` person
```

---

## Exercises

### Exercise 1: Nested Conditional

Write a function that categorizes an amount as "small" (< 100), "medium" (100-999), or "large" (>= 1000).

<details>
<summary>Solution</summary>

```l4
GIVEN amount IS A NUMBER
GIVETH A STRING
`categorize amount` MEANS
    IF amount < 100
    THEN "small"
    ELSE IF amount < 1000
         THEN "medium"
         ELSE "large"
```

</details>

### Exercise 2: List Validation

Write a function that checks if all items in a list of numbers are positive.

<details>
<summary>Solution</summary>

```l4
GIVEN numbers IS A LIST OF NUMBER
GIVETH A BOOLEAN
`all positive` MEANS
    all (GIVEN n YIELD n > 0) numbers
```

</details>

### Exercise 3: Complex Condition

Write a rule: "A person can purchase alcohol if they are at least 21, have valid ID, and are not on the banned list."

<details>
<summary>Solution</summary>

```l4
DECLARE Person
    HAS name IS A STRING
        age IS A NUMBER
        hasValidID IS A BOOLEAN

ASSUME bannedList IS A LIST OF STRING

GIVEN person IS A Person
GIVETH A BOOLEAN
DECIDE `can purchase alcohol` IF
    person's age >= 21
    AND person's hasValidID
    AND NOT elem (person's name) bannedList
```

</details>

---

## Common Mistakes

### 1. Missing ELSE

```l4
-- ❌ Wrong: No ELSE branch
DECIDE result IS IF condition THEN "yes"

-- ✅ Right: Both branches required
DECIDE result IS IF condition THEN "yes" ELSE "no"
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
