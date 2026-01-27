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
§ `Type Definitions`

-- Charitable purposes (from legislation)
DECLARE Purpose IS ONE OF
    `prevention or relief of poverty`
    `advancement of education`
    `advancement of religion`
    `advancement of health`
    `advancement of citizenship`
    `advancement of arts and culture`
    `advancement of amateur sport`
    `advancement of human rights`
    `advancement of environmental protection`
    `relief of those in need`
    `advancement of animal welfare`
    other HAS description IS A STRING

-- Legal status
DECLARE Status IS ONE OF
    Active
    Suspended HAS reason IS A STRING
    Deregistered HAS date IS A NUMBER
                    reason IS A STRING

-- Criminal conviction record
DECLARE Conviction
    HAS description IS A STRING
        isSpent IS A BOOLEAN

-- Governor of a charity
DECLARE Governor
    HAS name IS A STRING
        age IS A NUMBER
        isBankrupt IS A BOOLEAN
        convictions IS A LIST OF Conviction

-- Financial year record
DECLARE FinancialRecord
    HAS year IS A NUMBER
        income IS A NUMBER
        expenditure IS A NUMBER

-- The main charity record
DECLARE RegisteredCharity
    HAS name IS A STRING
        registrationNumber IS A STRING
        status IS A Status
        purposes IS A LIST OF Purpose
        governors IS A LIST OF Governor
        financials IS A LIST OF FinancialRecord
        yearEnd IS A NUMBER  -- day of year (1-365)
```

---

## Step 2: Define Eligibility Rules

Rules for who can be a governor and what makes a valid charity:

```l4
§ `Eligibility Rules`

-- A governor must be an adult
GIVEN governor IS A Governor
GIVETH A BOOLEAN
DECIDE `is adult` IF governor's age >= 18

-- Check for disqualifying convictions (unspent convictions)
GIVEN governor IS A Governor
GIVETH A BOOLEAN
DECIDE `has disqualifying conviction` IF
    any (GIVEN c YIELD c's isSpent EQUALS FALSE) (governor's convictions)

-- A person can be a governor if they're an adult, not bankrupt,
-- and have no disqualifying convictions
GIVEN governor IS A Governor
GIVETH A BOOLEAN
DECIDE `can be governor` IF
    `is adult` governor
    AND NOT governor's isBankrupt
    AND NOT `has disqualifying conviction` governor

-- A purpose is charitable if it's from the approved list
GIVEN purpose IS A Purpose
GIVETH A BOOLEAN
DECIDE `is charitable purpose` IS
    CONSIDER purpose
    WHEN `prevention or relief of poverty` THEN TRUE
    WHEN `advancement of education` THEN TRUE
    WHEN `advancement of religion` THEN TRUE
    WHEN `advancement of health` THEN TRUE
    WHEN `advancement of citizenship` THEN TRUE
    WHEN `advancement of arts and culture` THEN TRUE
    WHEN `advancement of amateur sport` THEN TRUE
    WHEN `advancement of human rights` THEN TRUE
    WHEN `advancement of environmental protection` THEN TRUE
    WHEN `relief of those in need` THEN TRUE
    WHEN `advancement of animal welfare` THEN TRUE
    WHEN other desc THEN FALSE  -- "other" requires special approval

-- A charity is valid if it has valid purposes and valid governors
GIVEN charity IS A RegisteredCharity
GIVETH A BOOLEAN
DECIDE `is valid charity` IF
    charity's status EQUALS Active
    AND length (charity's purposes) > 0
    AND all (GIVEN p YIELD `is charitable purpose` p) (charity's purposes)
    AND length (charity's governors) > 0
    AND all (GIVEN g YIELD `can be governor` g) (charity's governors)
```

---

## Step 3: Define Regulative Rules

The filing obligations:

```l4
§ `Filing Obligations`

-- Actors and actions for the regulatory system
DECLARE Actor IS ONE OF
    Charity HAS charity IS A RegisteredCharity
    Commissioner

DECLARE Action IS ONE OF
    `file annual return`
    `issue Required Steps Notice` HAS deadline IS A NUMBER
    `correct deficiencies`
    `deregister`

-- Annual return filing obligation
GIVEN charity IS A RegisteredCharity
GIVETH A DEONTIC Actor Action
`annual return obligation` MEANS
    IF charity's status EQUALS Active
    THEN
        PARTY Charity charity
        MUST `file annual return`
        WITHIN 60  -- 60 days from year end
        HENCE FULFILLED
        LEST
            PARTY Commissioner
            MUST `issue Required Steps Notice` 30
            WITHIN 14
            HENCE `correction period` charity
            LEST BREACH BY Commissioner BECAUSE "failed to issue notice"
    ELSE FULFILLED

-- After notice is issued, charity has time to correct
GIVEN charity IS A RegisteredCharity
GIVETH A DEONTIC Actor Action
`correction period` MEANS
    PARTY Charity charity
    MUST `correct deficiencies`
    WITHIN 30
    HENCE FULFILLED
    LEST
        PARTY Commissioner
        MAY `deregister`
        HENCE BREACH BY (Charity charity) BECAUSE "failed to file after notice"
```

---

## Step 4: Create Test Data

Define example charities for testing:

```l4
§ `Test Data`

-- Valid governor
validGovernor MEANS Governor "Jane Smith" 45 FALSE (LIST)

-- Governor with issues
bankruptGovernor MEANS Governor "John Doe" 50 TRUE (LIST)

unspentConviction MEANS Conviction "Fraud conviction 2020" FALSE
governorWithConviction MEANS Governor "Bob Jones" 40 FALSE (LIST unspentConviction)

-- Minor (under 18)
minorGovernor MEANS Governor "Young Person" 16 FALSE (LIST)

-- Valid charity
validCharity MEANS RegisteredCharity
    "Jersey Animal Welfare"
    "CH001"
    Active
    (LIST `advancement of animal welfare`, `advancement of education`)
    (LIST validGovernor)
    (LIST FinancialRecord 2023 50000 45000, FinancialRecord 2022 48000 42000)
    365  -- Year end Dec 31

-- Charity with invalid governor
charityWithBadGovernor MEANS RegisteredCharity
    "Problem Charity"
    "CH002"
    Active
    (LIST `advancement of education`)
    (LIST bankruptGovernor)
    (LIST FinancialRecord 2023 10000 8000)
    365

-- Suspended charity
suspendedCharity MEANS RegisteredCharity
    "Suspended Charity"
    "CH003"
    (Suspended "Financial irregularities")
    (LIST `advancement of health`)
    (LIST validGovernor)
    (LIST)
    365
```

---

## Step 5: Test Everything

Verify the rules work correctly:

```l4
§ `Tests`

-- Governor eligibility tests
#EVAL `is adult` validGovernor                      -- TRUE
#EVAL `is adult` minorGovernor                      -- FALSE
#EVAL `can be governor` validGovernor               -- TRUE
#EVAL `can be governor` bankruptGovernor            -- FALSE (bankrupt)
#EVAL `can be governor` governorWithConviction      -- FALSE (unspent conviction)
#EVAL `can be governor` minorGovernor               -- FALSE (under 18)

-- Charity validity tests
#EVAL `is valid charity` validCharity               -- TRUE
#EVAL `is valid charity` charityWithBadGovernor     -- FALSE
#EVAL `is valid charity` suspendedCharity           -- FALSE (suspended)

-- Test filing obligation scenarios
-- Happy path: charity files on time
#TRACE `annual return obligation` validCharity AT 0 WITH
    PARTY (Charity validCharity) DOES `file annual return` AT 30

-- Late filing: notice issued, then charity corrects
#TRACE `annual return obligation` validCharity AT 0 WITH
    -- Charity doesn't file, Commissioner issues notice
    PARTY Commissioner DOES `issue Required Steps Notice` 30 AT 70
    -- Charity corrects within notice period
    PARTY (Charity validCharity) DOES `correct deficiencies` AT 90

-- Worst case: deregistration
#TRACE `annual return obligation` validCharity AT 0 WITH
    PARTY Commissioner DOES `issue Required Steps Notice` 30 AT 70
    -- Charity fails to correct, Commissioner deregisters
    PARTY Commissioner DOES `deregister` AT 110
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

Define your domain model before writing rules:

```l4
-- ✅ Good: Clear domain model
DECLARE Application
    HAS applicant IS A LegalEntity
        purposes IS A LIST OF Purpose
        documents IS A LIST OF Document
```

### 2. Small, Focused Functions

Each function should do one thing:

```l4
-- ✅ Good: Single responsibility
DECIDE `is adult` IF person's age >= 18
DECIDE `is not bankrupt` IF NOT person's isBankrupt
DECIDE `has no disqualifying convictions` IF NOT any (GIVEN c YIELD NOT c's isSpent) (person's convictions)

-- Combine them
DECIDE `can be governor` IF
    `is adult` person
    AND `is not bankrupt` person
    AND `has no disqualifying convictions` person
```

### 3. Test Every Path

Write tests for happy paths, edge cases, and error cases:

```l4
-- Happy path
#EVAL `can be governor` validGovernor        -- TRUE

-- Edge cases
#EVAL `can be governor` (Governor "Edge" 18 FALSE (LIST))  -- TRUE (exactly 18)

-- Error cases
#EVAL `can be governor` bankruptGovernor     -- FALSE
#EVAL `can be governor` minorGovernor        -- FALSE
```

### 4. Use Descriptive Names

```l4
-- ✅ Good: Clear, readable names
DECIDE `charity meets filing requirements` IF ...
DECIDE `governor has unspent conviction` IF ...

-- ❌ Bad: Cryptic names
DECIDE check1 IF ...
DECIDE validate IF ...
```

### 5. Document Complex Logic

```l4
-- The charity test requires:
-- 1. All purposes must be from the statutory list (Art 5)
-- 2. The charity must provide public benefit (Art 7)
-- 3. All governors must be fit and proper (Art 19)
DECIDE `meets charity test` IF
    `has charitable purposes` charity
    AND `provides public benefit` charity
    AND `has fit governors` charity
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
