# Module 1: Your First Legal Rule

In this module, you'll write your first legal rule in L4—a simple legal obligation with conditions, deadlines, and consequences.

## Learning Objectives

By the end of this module, you will be able to:

- Write a basic legal obligation using PARTY and MUST
- Add conditions using IF
- Set deadlines using WITHIN
- Define consequences using HENCE and LEST
- Test your rule using #EVAL

---

## A Simple Legal Obligation

Let's start with something every lawyer understands: a simple legal obligation. Find the complete working example later in this document.

**The annual return obligation:** "A registered charity must file an annual return."

In L4, we write this as:

```l4
GIVEN charity IS A RegisteredCharity
`The annual return obligation` MEANS
    PARTY charity
    MUST `file annual return`
```

Let's break this down:

| Code                                   | Meaning                                    |
| -------------------------------------- | ------------------------------------------ |
| `GIVEN charity IS A RegisteredCharity` | This rule applies to registered charities  |
| `\`The annual return obligation\` MEANS` | The name of this rule                      |
| `PARTY charity`                        | The charity is the one with the obligation |
| `MUST`                                 | This creates a legal obligation            |
| `\`file annual return\``               | This is what they must do                  |

### Backtick Names

Notice the backticks around `file annual return`. In L4, backticks let you use spaces and special characters in names. These are called **quoted identifiers**.

```l4
-- These are equivalent ways to name things:
fileReturn          -- camelCase (no spaces)
`file return`       -- quoted identifier (with spaces)
`file annual return`  -- more descriptive
```

Use backticks when you want names that read like natural language.

---

## Try It Yourself

Write a rule that says "A solicitor must maintain client confidentiality."

Hint: You'll need to:
1. Define a `DECLARE` for the Solicitor type
2. Define `DECLARE` for the Actor and Action types
3. Use `PARTY ... MUST ...` to create the obligation

---

## Adding Conditions

Real legal rules have conditions. Let's add one:

```l4
IF charity's status EQUALS Active
    PARTY charity
    MUST `file annual return`
```

The `IF` keyword adds a condition that must be true for the obligation to apply.

### Multiple Conditions

Use `AND` and `OR` for multiple conditions:

```l4
IF charity's status EQUALS Active
   AND charity's income > 10000
```

### Accessing Fields

The `'s` syntax accesses fields of a record:

```l4
charity's status      -- the status field of charity
charity's income      -- the income field of charity
```

---

## Setting Deadlines

Legal obligations usually have deadlines. Use `WITHIN`:

```l4
PARTY charity
MUST `file annual return`
WITHIN 60
```

`WITHIN 60` means "within 60 days." L4 uses days as the default time unit.

---

## Consequences: HENCE and LEST

What happens when someone complies or doesn't comply? Use `HENCE` and `LEST`:

```l4
PARTY charity
MUST `file annual return`
WITHIN 60
HENCE FULFILLED
LEST BREACH
```

| Keyword | Meaning                               |
| ------- | ------------------------------------- |
| `HENCE` | What happens if they **comply**       |
| `LEST`  | What happens if they **don't comply** |

### Chaining Obligations

`HENCE` can trigger another obligation:

```l4
PARTY seller
MUST `deliver goods`
WITHIN 14
HENCE
    PARTY buyer
    MUST `pay invoice`
    WITHIN 30
    HENCE FULFILLED
    LEST BREACH
LEST BREACH
```

This creates a chain: if the seller delivers, the buyer must pay.

---

## Complete Example

[module-1-examples.l4](module-1-examples.l4)

Included are:

- Type definitions for charities, actors, and actions
- The annual return obligation with conditions and deadlines
- A chained sale contract
- Test data and `#TRACE` simulations

### Understanding GIVETH A DEONTIC

When a function returns an obligation (not just a value), we use `GIVETH A DEONTIC`:

- `GIVETH A BOOLEAN` - returns true/false
- `GIVETH A NUMBER` - returns a number
- `GIVETH A DEONTIC Actor Action` - returns an obligation (specifying actor and action types)

---

## Testing with #EVAL

Use `#EVAL` to test expressions:

```l4
#EVAL testCharity's name
-- Result: "Animal Welfare Society"

#EVAL testCharity's status EQUALS Active
-- Result: TRUE
```

In VS Code with the L4 extension, hover over `#EVAL` to see the result.

---

## Testing with #TRACE

For regulative rules (obligations), use `#TRACE` to simulate scenarios:

```l4
#TRACE `annual return obligation` testCharity AT 0 WITH
    PARTY testCharity DOES `file annual return` AT 30
```

This simulates:

- Starting at day 0
- The charity filing their return at day 30

The result shows whether the obligation was `FULFILLED` or `BREACH`.

---

## Common Mistakes

### 1. Missing Type Declaration

```l4
-- ❌ Wrong: Type not declared
GIVEN charity IS A RegisteredCharity

-- ✅ Right: Declare the type first
DECLARE RegisteredCharity
    HAS name IS A STRING
```

### 2. Wrong Field Access

```l4
-- ❌ Wrong: Missing 's
IF charity status EQUALS Active

-- ✅ Right: Use 's for field access
IF charity's status EQUALS Active
```

### 3. Missing Backticks for Multi-Word Names

```l4
-- ❌ Wrong: Spaces without backticks
MUST file annual return

-- ✅ Right: Use backticks
MUST `file annual return`
```


## Exercises

### Exercise 1: Simple Obligation

Write an L4 rule for: "An employee must submit a timesheet every week."

### Exercise 2: Conditional Obligation

Write an L4 rule for: "If a tenant is more than 14 days late on rent, the landlord may issue an eviction notice."

### Exercise 3: Chained Obligations

Write L4 rules for: "The seller must deliver goods within 14 days. If delivered, the buyer must pay within 30 days."

---

## Summary

In this module, you learned:

| Concept               | Syntax                            |
| --------------------- | --------------------------------- |
| Declare parameters    | `GIVEN name IS A Type`            |
| Create obligation     | `PARTY actor MUST action`         |
| Add condition         | `IF condition`                    |
| Set deadline          | `WITHIN days`                     |
| Compliance result     | `HENCE consequence`               |
| Non-compliance result | `LEST consequence`                |
| Test expression       | `#EVAL expression`                |
| Simulate scenario     | `#TRACE rule AT time WITH events` |

---

## What's Next?

In [Module 2: Legal Entities](module-2-entities.md), you'll learn how to model complex legal entities with proper types, including records, enums, and relationships between entities.
