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

Using simple text leads to errors and ambiguity:

```l4
-- ❌ Bad approach: Just text, no structure
charity MEANS "Some Charity Name"
```

Problems:

- Can't distinguish a charity from a person
- No way to access parts (name, address, etc.)
- Typos go undetected

### Structured Types: The Solution

```l4
-- ✅ Better approach: Structured type
DECLARE RegisteredCharity
    HAS name IS A STRING
        registrationNumber IS A STRING
        registrationDate IS A Date
        address IS A STRING
        purposes IS A LIST OF Purpose
```

Benefits:

- **Prevents errors:** Can't use a person where you need a charity
- **Captures relationships:** Links charities to their purposes, addresses
- **Enables validation:** L4 checks if all required information is present

---

## Declaring Record Types

Use `DECLARE` with `HAS` to define record types:

```l4
DECLARE RegisteredCharity
    HAS name IS A STRING
        registrationNumber IS A STRING
        registrationDate IS A Date
        address IS A STRING
```

### Syntax Rules

1. **Indentation matters:** All fields must be indented consistently under `HAS`
2. **Each field on its own line** (or separated by commas)
3. **Field names must be unique** within the type

```l4
-- Correct indentation (4 spaces for HAS, fields aligned)
DECLARE Person
    HAS name IS A STRING
        age IS A NUMBER
        isAdult IS A BOOLEAN
```

### Creating Record Values

Use the type name followed by values in field order:

```l4
-- Create a person
alice MEANS Person "Alice" 30 TRUE
```

Or use `WITH` for named fields (clearer for complex records):

```l4
alice MEANS Person WITH
    name IS "Alice"
    age IS 30
    isAdult IS TRUE
```

---

## Enumeration Types

### The Problem with Free Text

Using strings for categories allows mistakes:

```l4
-- ❌ Bad: Allows typos and inconsistencies
DECLARE Charity
    HAS purposes IS A LIST OF STRING

-- Could write "education", "Education", "educational", etc.
```

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
    `prevention or relief of poverty`
    `advancement of education`
    `advancement of religion`
    `advancement of health`
    `advancement of animal welfare`
    otherPurpose HAS description IS A STRING
```

The `otherPurpose` variant includes a description for edge cases.

### Using Enumerations

```l4
-- Simple variant (no data)
purpose1 MEANS `advancement of education`

-- Variant with data
purpose2 MEANS otherPurpose "local community support"
```

---

## Pattern Matching on Enumerations

Use `CONSIDER` to handle different variants:

```l4
GIVEN p IS A Purpose
GIVETH A BOOLEAN
DECIDE `is charitable purpose` IS
    CONSIDER p
    WHEN `advancement of education` THEN TRUE
    WHEN `advancement of animal welfare` THEN TRUE
    WHEN `prevention or relief of poverty` THEN TRUE
    WHEN otherPurpose description THEN
        `is analogous to charitable purpose` description
    OTHERWISE FALSE
```

### CONSIDER Syntax

```l4
CONSIDER expression
WHEN pattern1 THEN result1
WHEN pattern2 THEN result2
OTHERWISE defaultResult
```

---

## Connecting Multiple Entities

Legal systems involve relationships between entities:

```l4
DECLARE Person
    HAS name IS A STRING
        address IS A STRING
        isGovernor IS A BOOLEAN

DECLARE RegisteredCharity
    HAS name IS A STRING
        registrationNumber IS A STRING
        governors IS A LIST OF Person
        address IS A STRING
        purposes IS A LIST OF Purpose
```

The `governors` field creates a relationship: a charity has many governors.

### Using Relationships in Rules

```l4
GIVEN governor IS A Person
      charity IS A RegisteredCharity
IF governor's isGovernor EQUALS TRUE
   AND elem governor (charity's governors)
PARTY governor
MUST `act in best interests of charity`
```

The `elem` function checks if `governor` is in the list of governors.

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
charity's constitution's isWritten
-- Gets the isWritten field of the constitution field of charity
```

### Function Application Alternative

You can also write field access as function application:

```l4
name charity              -- same as: charity's name
registrationNumber charity -- same as: charity's registrationNumber
```

This is useful in complex expressions (see Module 7 for details).

---

## Nested Types

Types can contain other types:

```l4
DECLARE BankAccount
    HAS bankName IS A STRING
        accountNumber IS A STRING
        swift IS A STRING

DECLARE Company
    HAS name IS A STRING
        jurisdiction IS A STRING
        account IS A BankAccount
```

Creating nested values:

```l4
myCompany MEANS Company WITH
    name IS "Acme Corp"
    jurisdiction IS "Singapore"
    account IS BankAccount WITH
        bankName IS "DBS Bank"
        accountNumber IS "123-456789-0"
        swift IS "DBSSSGSG"
```

---

## Union Types for Alternatives

When something can be one of several different types:

```l4
DECLARE LegalEntity IS ONE OF
    Individual HAS person IS A Person
    Corporation HAS company IS A Company
    Partnership HAS partners IS A LIST OF Person
```

A `LegalEntity` can be an individual, corporation, or partnership—each with different data.

### Using Union Types

```l4
-- Create different kinds of entities
john MEANS Individual (Person "John Doe" "123 Main St" FALSE)
acme MEANS Corporation (Company "Acme Corp" "Delaware" myAccount)

-- Handle them with CONSIDER
GIVEN entity IS A LegalEntity
GIVETH A STRING
`entity name` MEANS
    CONSIDER entity
    WHEN Individual p THEN p's name
    WHEN Corporation c THEN c's name
    WHEN Partnership ps THEN "Partnership"
```

---

## Real-World Example: Charity Registration

Let's model a charity registration system:

```l4
-- Status enumeration
DECLARE Status IS ONE OF
    Active
    Suspended HAS reason IS A STRING
    Deregistered

-- Charitable purposes
DECLARE Purpose IS ONE OF
    `prevention of poverty`
    `advancement of education`
    `advancement of health`
    `advancement of religion`
    `environmental protection`
    other HAS description IS A STRING

-- Financial record
DECLARE FinancialRecord
    HAS year IS A NUMBER
        income IS A NUMBER
        expenditure IS A NUMBER

-- Complete charity record
DECLARE RegisteredCharity
    HAS name IS A STRING
        registrationNumber IS A STRING
        status IS A Status
        purposes IS A LIST OF Purpose
        financials IS A LIST OF FinancialRecord

-- Example charity
animalCharity MEANS RegisteredCharity WITH
    name IS "Jersey Animal Welfare"
    registrationNumber IS "CH001"
    status IS Active
    purposes IS LIST `advancement of health`, `environmental protection`
    financials IS LIST
        FinancialRecord 2023 50000 45000,
        FinancialRecord 2022 48000 42000
```

---

## Common Mistakes

### 1. Inconsistent Indentation

```l4
-- ❌ Wrong: Fields not aligned
DECLARE Person
    HAS name IS A STRING
      age IS A NUMBER
        isAdult IS A BOOLEAN

-- ✅ Right: All fields aligned
DECLARE Person
    HAS name IS A STRING
        age IS A NUMBER
        isAdult IS A BOOLEAN
```

### 2. Circular Type References

```l4
-- ❌ Wrong: A refers to B, B refers to A (before A is defined)
DECLARE A HAS b IS A B
DECLARE B HAS a IS A A

-- ✅ Right: Use forward references carefully or restructure
```

### 3. Missing Field Values

```l4
-- ❌ Wrong: Missing age field
DECIDE alice IS Person "Alice" TRUE

-- ✅ Right: Provide all fields
DECIDE alice IS Person "Alice" 30 TRUE
```

---

## Exercises

### Exercise 1: Define a Contract Type

Define types for a simple contract with parties and an amount.

<details>
<summary>Solution</summary>

```l4
DECLARE Party
    HAS name IS A STRING
        address IS A STRING

DECLARE Contract
    HAS buyer IS A Party
        seller IS A Party
        amount IS A NUMBER
        currency IS A STRING
```

</details>

### Exercise 2: Employment Status Enumeration

Define an enumeration for employment status (full-time, part-time, contractor, terminated).

<details>
<summary>Solution</summary>

```l4
DECLARE EmploymentStatus IS ONE OF
    FullTime
    PartTime HAS hoursPerWeek IS A NUMBER
    Contractor HAS contractEndDate IS A Date
    Terminated HAS terminationDate IS A Date
                  reason IS A STRING
```

</details>

### Exercise 3: Pattern Matching

Write a function that returns a person's employment description based on their status.

<details>
<summary>Solution</summary>

```l4
GIVEN status IS A EmploymentStatus
GIVETH A STRING
`describe employment` MEANS
    CONSIDER status
    WHEN FullTime THEN "Full-time employee"
    WHEN PartTime hours THEN "Part-time employee"
    WHEN Contractor endDate THEN "Contractor"
    WHEN Terminated date reason THEN "Former employee"
```

</details>

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
