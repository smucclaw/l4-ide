# Module 6: Putting It Together

In this capstone module, you'll build a complete legal model combining everything you've learned.

## Learning Objectives

By the end of this module, you will be able to:

- Design a complete L4 model from requirements
- Organize code with sections
- Apply best practices
- Debug common issues
- Know where to go next

---

## Capstone Project: Charity Registration

We'll build a simplified charity registration system based on real legislation. This combines:

- Type definitions
- Eligibility rules
- Regulative obligations
- Testing

The complete working implementation:

[module-6-examples.l4](module-6-examples.l4)


### Requirements

1. **Charities** have names, purposes, governors, and financial records
2. **Purposes** must be from an approved list
3. **Governors** must be adults without disqualifying convictions
4. **Registered charities** must file annual returns within 60 days of year-end
5. **Late filing** triggers a Required Steps Notice

---

## Step 1: Define Types

Start by modeling the domain:

```l4
-- Charitable purposes (from legislation)
DECLARE Purpose IS ONE OF
    `prevention or relief of poverty`
    `advancement of education`
    `advancement of religion`
    `advancement of health`
    `advancement of animal welfare`
    other HAS description IS A STRING

-- Governor of a charity
DECLARE Governor
    HAS name IS A STRING
        age IS A NUMBER
        isBankrupt IS A BOOLEAN
        convictions IS A LIST OF Conviction
```

Key design decisions:

1. **Use natural language field names:** `` `the governor's name` `` reads like legal text
2. **Use enumerations for fixed categories:** Prevents typos and invalid values
3. **Use lists for multiple items:** governors, purposes, convictions

---

## Step 2: Define Eligibility Rules

Rules for who can be a governor and what makes a valid charity:

```l4
-- A governor must be an adult
GIVEN governor IS A Governor
GIVETH A BOOLEAN
DECIDE `is adult` IF governor's age >= 18

-- Check for disqualifying convictions
GIVEN governor IS A Governor
GIVETH A BOOLEAN
DECIDE `has disqualifying conviction` IF
    any (GIVEN c YIELD c's isSpent EQUALS FALSE) (governor's convictions)

-- Combined eligibility check
GIVEN governor IS A Governor
GIVETH A BOOLEAN
DECIDE `can be governor` IF
    `is adult` governor
    AND NOT governor's isBankrupt
    AND NOT `has disqualifying conviction` governor
```

Key patterns:

1. **Simple predicates:** `` `is adult` ``
2. **Using `any` with predicates:** checking for disqualifying convictions
3. **Combining conditions:** using `AND` and `NOT`

**Note:** These functions require `IMPORT prelude` for `any`, `all`, etc.

---

## Step 3: Define Regulative Rules

The filing obligations:

```l4
-- Annual return filing obligation
GIVEN charity IS A RegisteredCharity
GIVETH A DEONTIC Actor Action
`annual return obligation` MEANS
    IF charity's status EQUALS Active
    THEN
        PARTY Charity charity
        MUST `file annual return`
        WITHIN 60
        HENCE FULFILLED
        LEST
            PARTY Commissioner
            MUST `issue Required Steps Notice` 30
            WITHIN 14
            HENCE `correction period` charity
            LEST BREACH BY Commissioner BECAUSE "failed to issue notice"
    ELSE FULFILLED
```

Key patterns:

1. **Actor and Action types:** Define who can act and what actions exist
2. **Conditional obligations:** Only active charities must file
3. **Chained obligations:** Non-compliance triggers Commissioner action
4. **Clear blame assignment:** `BREACH BY ... BECAUSE ...`

---

## Step 4: Create Test Data

```l4
-- Valid governor
validGovernor MEANS Governor "Jane Smith" 45 FALSE (LIST)

-- Governor with issues
bankruptGovernor MEANS Governor "John Doe" 50 TRUE (LIST)

-- Valid charity
validCharity MEANS RegisteredCharity
    "Jersey Animal Welfare"
    "CH001"
    Active
    (LIST `advancement of animal welfare`, `advancement of education`)
    (LIST validGovernor)
    (LIST FinancialRecord 2023 50000 45000)
    365
```

Create governors and charities with various characteristics for testing.

---

## Step 5: Test Everything

### Unit Tests with #EVAL

```l4
#EVAL `is adult` validGovernor              -- TRUE
#EVAL `can be governor` validGovernor       -- TRUE
#EVAL `can be governor` bankruptGovernor    -- FALSE
#EVAL `is valid charity` validCharity       -- TRUE
```

### Scenario Tests with #TRACE

```l4
-- Happy path: charity files on time
#TRACE `annual return obligation` validCharity AT 0 WITH
    PARTY (Charity validCharity) DOES `file annual return` AT 30
-- Result: FULFILLED

-- Late filing: notice issued, then charity corrects
#TRACE `annual return obligation` validCharity AT 0 WITH
    PARTY Commissioner DOES `issue Required Steps Notice` 30 AT 70
    PARTY (Charity validCharity) DOES `correct deficiencies` AT 90
-- Result: FULFILLED
```

---

## Organizing Code with Sections

Use sections (`§`) to organize larger files:

```l4
§ `Charity Registration System`

§§ `Type Definitions`
-- Types go here

§§ `Eligibility Rules`
-- Eligibility functions go here

§§ `Filing Obligations`
-- Regulative rules go here

§§ `Tests`
-- Test cases go here
```

Sections create a hierarchy:

- `§` - Top-level section
- `§§` - Sub-section
- `§§§` - Sub-sub-section

---

## Best Practices

### 1. Start with Types

```l4
-- ✅ Good: Clear domain model
DECLARE Application
    HAS applicant IS A LegalEntity
        purposes IS A LIST OF Purpose
        documents IS A LIST OF Document
```

### 2. Small, Focused Functions

```l4
-- ✅ Good: Single responsibility
DECIDE `is adult` IF person's age >= 18
DECIDE `is not bankrupt` IF NOT person's isBankrupt

-- Combine them
DECIDE `can be governor` IF
    `is adult` person
    AND `is not bankrupt` person
```

### 3. Test Every Path

```l4
-- Happy path
#EVAL `can be governor` validGovernor        -- TRUE

-- Error cases
#EVAL `can be governor` bankruptGovernor     -- FALSE
#EVAL `can be governor` minorGovernor        -- FALSE
```

### 4. Use Descriptive Names

```l4
-- ✅ Good: Clear, readable names
DECIDE `charity meets filing requirements` IF ...

-- ❌ Bad: Cryptic names
DECIDE check1 IF ...
```

### 5. Document Complex Logic

```l4
-- The charity test requires:
-- 1. All purposes must be from the statutory list (Art 5)
-- 2. The charity must provide public benefit (Art 7)
-- 3. All governors must be fit and proper (Art 19)
DECIDE `meets charity test` IF ...
```

---

## Debugging Checklist

When something doesn't work:

| Error                  | Likely Cause            | Fix                                         |
| ---------------------- | ----------------------- | ------------------------------------------- |
| "Type not in scope"    | Type not declared       | Add DECLARE before use                      |
| "Unexpected 's"        | Missing parentheses     | `length (x's field)` not `length x's field` |
| "Expected BOOLEAN"     | Wrong type in condition | Check IF conditions return BOOLEAN          |
| "Not enough arguments" | Missing function args   | Count parameters in GIVEN                   |
| "Indentation error"    | Inconsistent spacing    | Align all fields under HAS                  |

---

## Where to Go Next

### Immediate Next Steps

1. **[Advanced Course](../advanced/README.md)** - Complex patterns, real legislation, multi-instrument integration

2. **[Tutorials](../../tutorials/README.md)** - Task-focused guides:

   - Building web forms
   - LLM integration
   - Contract automation

3. **Practice** - Try encoding a real document:
   - Your employment contract
   - Your lease agreement
   - A regulation you work with

### Reference Materials

- **[Reference Guide](../../reference/README.md)** - Complete keyword and syntax reference
- **[Concepts](../../concepts/README.md)** - Deep dives into design principles

### Example Code

Explore the examples in the repository:

- `jl4/examples/legal/` - Real legal documents
- `jl4/examples/ok/` - Working syntax examples
- `jl4/experiments/` - Experimental features

---

## Summary: What You've Learned

### Module 1: First Legal Rule

- GIVEN, PARTY, MUST
- IF conditions
- WITHIN deadlines
- HENCE and LEST consequences

### Module 2: Legal Entities

- DECLARE with HAS for records
- IS ONE OF for enumerations
- Field access with 's

### Module 3: Control Flow

- IF/THEN/ELSE
- CONSIDER pattern matching
- AND, OR, NOT operators
- List operations

### Module 4: Functions

- GIVEN and GIVETH
- DECIDE and MEANS
- WHERE for local definitions
- Recursion

### Module 5: Regulative Rules

- MUST, MAY, SHANT
- HENCE, LEST
- PROVIDED conditions
- RAND and ROR combinators
- #TRACE testing

### Module 6: Putting It Together

- Project structure
- Best practices
- Debugging

---

## Congratulations! 🎉

You've completed the L4 Foundation Course. You now have the skills to:

- Model legal entities and relationships
- Write legal rules and eligibility criteria
- Create contracts with obligations and consequences
- Test your models with simulations

**Next recommended step:** Try the [Advanced Course](../advanced/README.md) to learn about real regulatory schemes, cross-cutting concerns, and production patterns.
