# Algebraic Types in L4

How L4 uses algebraic data types to model legal concepts.

---

## What Are Algebraic Types?

**Algebraic data types** (ADTs) are types built from two fundamental operations:

1. **Product types**: Combine multiple values (AND)
2. **Sum types**: Choose between alternatives (OR)

L4 uses ADTs extensively because they naturally map to legal structures.

---

## Product Types: Records

A **product type** combines multiple values together. In L4, we call these **records**.

### Syntax

```l4
DECLARE Person
    HAS name IS A STRING
        age IS A NUMBER
        email IS A STRING
```

### Why "Product"?

The set of possible `Person` values is the **product** (cross-product) of all field types:

```
Person = STRING × NUMBER × STRING
       = {all possible names} × {all possible ages} × {all possible emails}
```

### Creating Records

```l4
-- Create a Person value
alice MEANS Person "Alice Smith" 30 "alice@example.com"
```

### Accessing Fields

Use `'s` (possessive):

```l4
alice's name   -- "Alice Smith"
alice's age    -- 30
```

### Legal Application

Records model legal entities:

```l4
DECLARE Contract
    HAS parties IS A LIST OF Party
        effectiveDate IS A Date
        terminationDate IS A MAYBE Date
        obligations IS A LIST OF Obligation
```

---

## Sum Types: Enumerations

A **sum type** represents a choice between alternatives. In L4, we use `IS ONE OF`.

### Syntax

```l4
DECLARE Status IS ONE OF
    Active
    Suspended
    Terminated
```

### Why "Sum"?

The set of possible `Status` values is the **sum** (union) of the variants:

```
Status = Active ∪ Suspended ∪ Terminated
       = 3 possible values
```

### Creating Sum Values

Just use the variant name:

```l4
currentStatus MEANS Active
anotherStatus MEANS Suspended
```

### Legal Application

Sum types model legal categories:

```l4
-- Legal entity types
DECLARE LegalEntity IS ONE OF
    NaturalPerson
    Corporation
    Partnership
    Trust

-- Contract states
DECLARE ContractState IS ONE OF
    Draft
    Executed
    InForce
    Terminated
    Expired
```

---

## Sum Types with Data

Variants can carry data:

```l4
DECLARE PaymentStatus IS ONE OF
    Pending
    Paid HAS date IS A Date
             amount IS A Money
    Rejected HAS reason IS A STRING
             date IS A Date
```

### Creating Values

```l4
status1 MEANS Pending
status2 MEANS Paid (Date 2024 1 15) (USD 1000)
status3 MEANS Rejected "Insufficient funds" (Date 2024 1 16)
```

### Legal Application

```l4
-- Outcome of a legal proceeding
DECLARE Outcome IS ONE OF
    Upheld
    Quashed HAS reason IS A STRING
    Varied HAS modifications IS A STRING
    Remitted HAS toWhom IS A String
              forWhat IS A STRING
```

---

## Pattern Matching

To work with sum types, use `CONSIDER`:

```l4
GIVEN status IS A PaymentStatus
GIVETH A STRING
`describe status` MEANS
    CONSIDER status
    WHEN Pending THEN "Payment pending"
    WHEN Paid d a THEN CONCAT "Paid " (SHOW a's amount) " on " (SHOW d)
    WHEN Rejected r d THEN CONCAT "Rejected on " (SHOW d) ": " r
```

### Exhaustiveness

L4 checks that you handle all cases:

```l4
-- ❌ Error: Missing case 'Rejected'
CONSIDER status
WHEN Pending THEN "..."
WHEN Paid d a THEN "..."
```

This catches bugs where you forget to handle a case.

### Otherwise

Use `OTHERWISE` as a catch-all:

```l4
CONSIDER status
WHEN Pending THEN "Not yet processed"
OTHERWISE "Already processed"
```

---

## Combining Product and Sum

Real models combine both:

```l4
-- Sum type with product data
DECLARE Employment IS ONE OF
    Employed HAS employer IS A Company
                 startDate IS A Date
                 salary IS A Money
    SelfEmployed HAS businessName IS A STRING
                     registrationNumber IS A STRING
    Unemployed HAS since IS A Date

-- Product type containing sum types
DECLARE Person
    HAS name IS A STRING
        employmentStatus IS A Employment
        residencyStatus IS A ResidencyStatus
```

### Pattern Match Within Records

```l4
GIVEN person IS A Person
GIVETH A BOOLEAN
`is currently employed` MEANS
    CONSIDER person's employmentStatus
    WHEN Employed e s sal THEN TRUE
    OTHERWISE FALSE
```

---

## Recursive Types

Types can reference themselves:

```l4
-- Tree structure
DECLARE OrgUnit IS ONE OF
    Individual HAS person IS A Person
    Team HAS name IS A STRING
             members IS A LIST OF OrgUnit

-- Linked list (conceptually)
DECLARE IntList IS ONE OF
    Empty
    Cons HAS head IS A NUMBER
             tail IS A IntList
```

### Legal Application: Document Structure

```l4
DECLARE LegalDocument IS ONE OF
    Section HAS number IS A STRING
                title IS A STRING
                content IS A LIST OF LegalDocument
    Paragraph HAS text IS A STRING
    Definition HAS term IS A STRING
                   meaning IS A STRING
```

---

## Why ADTs for Law?

### 1. Legal Categories Are Sums

Legal text often defines categories:

> A "person" means—
> (a) a natural person; or
> (b) a body corporate; or
> (c) a partnership.

This maps directly to:

```l4
DECLARE Person IS ONE OF
    NaturalPerson HAS ...
    BodyCorporate HAS ...
    Partnership HAS ...
```

### 2. Legal Entities Are Products

Legal text defines what properties entities have:

> A "registered charity" must have—
>
> - a registered name
> - a registration number
> - at least one charitable purpose

This maps to:

```l4
DECLARE RegisteredCharity
    HAS name IS A STRING
        registrationNumber IS A STRING
        purposes IS A LIST OF Purpose
```

### 3. Exhaustive Analysis

Legal analysis must cover all cases. Pattern matching ensures this:

```l4
-- Must handle all entity types
CONSIDER entity
WHEN NaturalPerson p THEN `personal tax rules` p
WHEN BodyCorporate c THEN `corporate tax rules` c
WHEN Partnership p THEN `partnership tax rules` p
-- Compiler error if a case is missing
```

---

## Common Patterns

### Optional Values: MAYBE

```l4
DECLARE Person
    HAS name IS A STRING
        middleName IS A MAYBE STRING  -- Optional
```

`MAYBE` is a sum type:

```l4
-- Built-in, but conceptually:
DECLARE MAYBE a IS ONE OF
    NOTHING
    JUST HAS value IS AN a
```

### Lists

Lists are also algebraic:

```l4
-- Built-in, but conceptually:
DECLARE LIST a IS ONE OF
    EMPTY
    FOLLOWED BY HAS head IS AN a
                    tail IS A LIST OF a
```

Pattern match with:

```l4
CONSIDER items
WHEN EMPTY THEN "no items"
WHEN x FOLLOWED BY rest THEN CONCAT "first: " (SHOW x)
```

---

## Summary

| Concept       | L4 Syntax                      | Mathematical View |
| ------------- | ------------------------------ | ----------------- |
| Product type  | `DECLARE X HAS field1, field2` | X = A × B         |
| Sum type      | `DECLARE X IS ONE OF A, B, C`  | X = A + B + C     |
| Sum with data | `A HAS field IS A Type`        | Tagged union      |
| Pattern match | `CONSIDER x WHEN ...`          | Case analysis     |
| Optional      | `MAYBE Type`                   | 1 + Type          |
| List          | `LIST OF Type`                 | 1 + (Type × List) |

---

## Further Reading

- [Foundation Course Module 2](../../courses/foundation/module-2-entities.md) - Hands-on with types
- [Types Reference](../../reference/types/README.md) - Detailed type reference
