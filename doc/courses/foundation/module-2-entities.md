# Module 2: Legal Entities and Relationships

In this module, you'll learn how to model structured legal entities using L4's type system.

## Learning Objectives

By the end of this module, you will be able to:

- Define record types with DECLARE and HAS
- Create enumeration types with IS ONE OF
- Model relationships between entities
- Access fields using the possessive syntax
- Construct record values

---

## From Strings to Structured Types

### The Problem with Strings

Using simple text leads to errors and ambiguity. You can't distinguish a charity from a person, access parts (name, address, etc.), or detect typos.

### Structured Types: The Solution

The complete working example:

[module-2-examples.l4](module-2-examples.l4)

```l4
DECLARE `Registered Charity`
    HAS `the charity's name` IS A STRING
        `the registration number` IS A STRING
        `the registration date` IS A Date
        `the charity's address` IS A STRING
        `the charity's purposes` IS A LIST OF Purpose
```

Benefits of structured types:

- **Prevents errors:** Can't use a person where you need a charity
- **Captures relationships:** Links charities to their purposes, addresses
- **Enables validation:** L4 checks if all required information is present

---

## Declaring Record Types

Use `DECLARE` with `HAS` to define record types:

```l4
DECLARE Person
    HAS `the person's name` IS A STRING
        `the person's age` IS A NUMBER
        `the person is an adult` IS A BOOLEAN
```

### Syntax Rules

1. **Indentation matters:** All fields must be indented consistently under `HAS`
2. **Each field on its own line** (or separated by commas)
3. **Field names must be unique** within the type
4. **Use natural language field names** with backticks: `` `the person's name` ``

### Creating Record Values

Use the type name followed by values in field order:

```l4
alice MEANS Person "Alice" 30 TRUE
```

---

## Enumeration Types

### The Problem with Free Text

Using strings for categories allows mistakes—typos and inconsistencies go undetected.

### Enumerations: The Solution

Use `IS ONE OF` to define exact legal categories:

```l4
DECLARE Purpose IS ONE OF
    `prevention or relief of poverty`
    `advancement of education`
    `advancement of religion`
    `advancement of health`
    `advancement of animal welfare`
```

Now only these exact values are allowed—typos are compile-time errors!

### Enumerations with Data

Some enum variants can carry additional data:

```l4
DECLARE Purpose IS ONE OF
    `advancement of education`
    `advancement of health`
    `other purpose` HAS `the description` IS A STRING
```

The `other purpose` variant includes a description for edge cases.

---

## Pattern Matching on Enumerations

Use `CONSIDER` to handle different variants:

```l4
GIVEN purpose IS A Purpose
GIVETH A BOOLEAN
DECIDE `the purpose is charitable` IS
    CONSIDER purpose
    WHEN `advancement of education` THEN TRUE
    WHEN `advancement of health` THEN TRUE
    WHEN `other purpose` description THEN FALSE
```

### CONSIDER Syntax

The general pattern is:

- `CONSIDER expression`
- `WHEN pattern THEN result` (one for each variant)
- `OTHERWISE defaultResult` (optional catch-all)

---

## Connecting Multiple Entities

Legal systems involve relationships between entities. A field can reference another type, creating relationships.

See the `Company` type in [module-2-examples.l4](module-2-examples.l4) which has a `Bank Account` field.

The `elem` function (from prelude) checks if an item is in a list.

---

## Field Access

### Possessive Syntax

Use `'s` to access record fields:

```l4
charity's name              -- the name field
charity's registrationNumber -- the registration number
charity's governors         -- the list of governors
```

### Chained Access

Access nested fields by chaining:

```l4
company's account's `bank name`
-- Gets the bank name field of the account field of company
```

### Function Application Alternative

You can also write field access as function application:

```l4
name charity              -- same as: charity's name
```

This is useful in complex expressions.

---

## Nested Types

Types can contain other types:

```l4
DECLARE `Bank Account`
    HAS `bank name` IS A STRING
        `account number` IS A STRING

DECLARE Company
    HAS name IS A STRING
        account IS A `Bank Account`
```

---

## Union Types for Alternatives

When something can be one of several different types:

```l4
DECLARE `Legal Entity` IS ONE OF
    Individual HAS person IS A Person
    Corporation HAS company IS A Company
```

A `Legal Entity` can be an individual or corporation—each with different data.

### Using Union Types

```l4
GIVEN entity IS A `Legal Entity`
GIVETH A STRING
`the entity's name` MEANS
    CONSIDER entity
    WHEN Individual p THEN p's `the person's name`
    WHEN Corporation c THEN c's name
```

---

## Real-World Example: Charity Registration

For a complete charity registration system with types, see [module-6-examples.l4](module-6-examples.l4) in the capstone module.

---

## Common Mistakes

### 1. Inconsistent Indentation

```l4
-- ❌ Wrong: Fields not aligned
DECLARE Person
    HAS name IS A STRING
      age IS A NUMBER

-- ✅ Right: All fields aligned
DECLARE Person
    HAS name IS A STRING
        age IS A NUMBER
```

### 2. Circular Type References

```l4
-- ❌ Problematic: A refers to B before B is defined
DECLARE A HAS b IS A B
DECLARE B HAS a IS A A
```

### 3. Missing Field Values

```l4
-- ❌ Wrong: Missing age field
alice MEANS Person "Alice" TRUE

-- ✅ Right: Provide all fields
alice MEANS Person "Alice" 30 TRUE
```

---

## Exercises

### Exercise 1: Define a Contract Type

Define types for a simple contract with parties and an amount.

### Exercise 2: Employment Status Enumeration

Define an enumeration for employment status (full-time, part-time, contractor, terminated).

### Exercise 3: Pattern Matching

Write a function that returns a person's employment description based on their status.

---

## Summary

| Concept               | Syntax                                             |
| --------------------- | -------------------------------------------------- |
| Record type           | `DECLARE Type HAS field IS A FieldType`            |
| Enumeration           | `DECLARE Type IS ONE OF Variant1, Variant2`        |
| Enum with data        | `Variant HAS field IS A Type`                      |
| Create record         | `Type value1 value2` or `Type WITH field IS value` |
| Field access          | `record's field`                                   |
| Pattern match         | `CONSIDER expr WHEN pattern THEN result`           |
| Check list membership | `elem item list`                                   |

---

## What's Next?

In [Module 3: Control Flow](module-3-control-flow.md), you'll learn how to handle conditional logic, work with lists, and use boolean operators effectively.
