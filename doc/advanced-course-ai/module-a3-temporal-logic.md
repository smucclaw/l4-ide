# Module A3 — Temporal Logic in L4 [WIP]

> ⚠️ **Work in Progress**: This module describes temporal features that are partially implemented. The `EVAL ... DO ...` construct described in the second half is under active development. See [TEMPORAL_EVAL_SPEC.md](../../../opm2l4/docs/TEMPORAL_EVAL_SPEC.md) for the latest specification.

## Overview

Time is fundamental to law. Legal rules specify:
- **When they come into effect** (commencement dates)
- **When they cease to apply** (sunset clauses, repeal dates)
- **Time-based conditions** ("within 30 days", "before age 65")
- **Retroactive application** ("as if it had been in force since...")
- **Historical queries** ("was this legal in 2010?")

L4 provides temporal logic features to handle these scenarios correctly.

## The Four Dimensions of Time in Legal Systems

Legal computation involves multiple temporal dimensions:

1. **Valid Time (tcValidTime)** — When is something true in the real world?
   - "Alice's age on January 1, 2025"
   - "The company's revenue in Q4 2024"

2. **System Time (tcSystemTime)** — When did the system learn about it?
   - "Document uploaded on March 15, 2025"
   - "Application submitted on February 1, 2025"

3. **Rule Version Time (tcRuleVersionTime)** — When were the rules in effect?
   - "Under the 2015 amendment to the Act..."
   - "Before the 2020 regulations came into force..."

4. **Rule Encoding Time (tcRuleEncodingTime)** — When was the L4 code written?
   - "Git commit abc123 from 2024-03-15"
   - "Version 2.1 of our model"

Traditional databases handle (1) and (2) as "bitemporal" data. L4 extends this with (3) and (4) to handle the full complexity of legal temporal reasoning.

## Part 1: Basic Date Arithmetic (Available Now)

### The daydate Library

L4 ships with a comprehensive date library:

```l4
IMPORT daydate

-- Creating dates
`today` MEANS DATE OF 1, 12, 2025  -- Example current date
`launch date` MEANS DATE OF 15, 3, 2025
`deadline` MEANS DATE OF 30, 6, 2025

-- Date arithmetic
`days until deadline` MEANS
    `deadline` MINUS `today`  -- Returns number of days

`three months from now` MEANS
    `today` PLUS 90  -- Add 90 days

-- Date comparisons
`before deadline` date MEANS
    date LESS THAN `deadline`

`after launch` date MEANS
    date GREATER THAN `launch date`

-- Date parts
`year of` date MEANS date's year
`month of` date MEANS date's month
`day of` date MEANS date's day
```

### Age Calculations

```l4
IMPORT daydate

GIVEN birthDate IS A DATE
      asOfDate IS A DATE
GIVETH A NUMBER
`age as of` asOfDate `for person born` birthDate MEANS
    FLOOR ((asOfDate MINUS birthDate) DIVIDED BY 365.2425)

-- Using it
GIVEN employee IS AN Employee
GIVETH A NUMBER
`current age of` employee MEANS
    `age as of` `today` `for person born` employee's dateOfBirth
```

### Relative Dates

```l4
IMPORT daydate

-- The day after/before
`tomorrow` MEANS `today` PLUS 1
`yesterday` MEANS `today` MINUS 1

-- Weeks
`next week` MEANS `the week after` `today`
`last week` MEANS `the week before` `today`

-- Months (approximate)
`next month` MEANS `today` PLUS 30
`last month` MEANS `today` MINUS 30

-- Years
`next year` MEANS `today` PLUS 365
`last year` MEANS `today` MINUS 365
```

### Date Ranges and Windows

```l4
IMPORT daydate

-- Check if date is within a range
GIVEN date IS A DATE
      startDate IS A DATE
      endDate IS A DATE
GIVETH A BOOLEAN
`date` `is between` `startDate` `and` `endDate` MEANS
        date AT LEAST startDate
    AND date AT MOST endDate

-- Check if within N days
GIVEN date IS A DATE
      referenceDate IS A DATE
      days IS A NUMBER
GIVETH A BOOLEAN
`date` `within` `days` `days of` `referenceDate` MEANS
    `distance` AT MOST days
    WHERE
        `distance` MEANS
            IF date GREATER THAN referenceDate
            THEN date MINUS referenceDate
            ELSE referenceDate MINUS date
```

### WPA Example: Application Deadlines

```l4
IMPORT prelude
IMPORT daydate

§ `Application Deadline Logic`

DECLARE Application HAS
    submissionDate IS A DATE
    proposedStartDate IS A DATE

-- Rule: Must apply at least 30 days before start date
GIVEN app IS AN Application
GIVETH A BOOLEAN
DECIDE `meets advance notice requirement` IF
    daysInAdvance AT LEAST 30
    WHERE
        daysInAdvance MEANS
            app's proposedStartDate MINUS app's submissionDate

-- Rule: Cannot apply more than 6 months in advance
GIVEN app IS AN Application
GIVETH A BOOLEAN
DECIDE `not too far in advance` IF
    daysInAdvance AT MOST 180
    WHERE
        daysInAdvance MEANS
            app's proposedStartDate MINUS app's submissionDate

-- Combined check
GIVEN app IS AN Application
GIVETH A BOOLEAN
DECIDE `application timing is valid` IF
        `meets advance notice requirement` app
    AND `not too far in advance` app
```

### Effective Dates and Commencement

```l4
IMPORT daydate

§ `Rule Commencement Dates`

-- Constants for key legislative dates
`Employment Act 2020 Commencement` MEANS DATE OF 1, 1, 2020
`Amendment Act 2023 Commencement` MEANS DATE OF 15, 6, 2023
`Transitional Period End` MEANS DATE OF 31, 12, 2024

-- Check which rules apply
GIVEN applicationDate IS A DATE
GIVETH A BOOLEAN
`new rules apply` applicationDate MEANS
    applicationDate AT LEAST `Amendment Act 2023 Commencement`

GIVEN applicationDate IS A DATE
GIVETH A BOOLEAN
`in transitional period` applicationDate MEANS
        applicationDate AT LEAST `Amendment Act 2023 Commencement`
    AND applicationDate AT MOST `Transitional Period End`
```

### Expiry and Sunset Clauses

```l4
IMPORT daydate

DECLARE Permit HAS
    permitNumber IS A STRING
    issueDate IS A DATE
    durationMonths IS A NUMBER

GIVEN permit IS A Permit
      checkDate IS A DATE
GIVETH A BOOLEAN
`permit is valid on` checkDate MEANS
    checkDate AT MOST expiryDate
    WHERE
        expiryDate MEANS
            permit's issueDate PLUS (permit's durationMonths TIMES 30)

GIVEN permit IS A Permit
      checkDate IS A DATE
GIVETH A NUMBER
`days until expiry on` checkDate MEANS
    IF expired
    THEN 0
    ELSE expiryDate MINUS checkDate
    WHERE
        expiryDate MEANS
            permit's issueDate PLUS (permit's durationMonths TIMES 30)
        expired MEANS checkDate GREATER THAN expiryDate
```

## Part 2: Multi-Temporal Reasoning [WIP - Under Development]

> ⚠️ The following features are specified but not yet fully implemented. See the [TEMPORAL_EVAL_SPEC.md](../../../opm2l4/docs/TEMPORAL_EVAL_SPEC.md) for implementation status.

### The EVAL Construct

The `EVAL ... DO ...` construct allows you to evaluate expressions under different temporal contexts:

```l4
-- Evaluate under specific valid time
EVAL UNDER VALID TIME January 1 2010
     DO `is eligible` applicant

-- Evaluate under historical rules
EVAL UNDER RULES EFFECTIVE AT January 1 2010
     DO `is eligible` applicant

-- Evaluate under historical code version
EVAL UNDER COMMIT "abc123"
     DO `is eligible` applicant

-- Combine multiple temporal contexts
EVAL UNDER VALID TIME January 1 2010
     UNDER RULES EFFECTIVE AT January 1 2010
     UNDER COMMIT "abc123"
     AS OF SYSTEM TIME July 1 2015
     DO `is eligible` applicant
```

### Mixfix Syntax (Preferred)

L4 provides natural-language syntax for temporal evaluation:

```l4
EVAL `retroactive to` January 1 2010
     `under commit` "abc123"
     `as of system time` July 1 2015
     `evaluate` (`may purchase alcohol` applicant)
```

### Use Case 1: Historical Eligibility Checks

Legal systems sometimes need to determine "would this person have been eligible under the old rules?"

```l4
IMPORT daydate

§ `Retroactive Eligibility Assessment`

-- Check eligibility under rules as they were in 2010
GIVEN applicant IS AN Employee
GIVETH A BOOLEAN
`was eligible in 2010` applicant MEANS
    EVAL `retroactive to` DATE OF 1, 1, 2010
         `evaluate` (`is eligible for work pass` applicant)

-- Compare old vs new rules
GIVEN applicant IS AN Employee
GIVETH A BOOLEAN
`eligibility changed since 2010` applicant MEANS
    oldResult NOT EQUALS currentResult
    WHERE
        oldResult MEANS `was eligible in 2010` applicant
        currentResult MEANS `is eligible for work pass` applicant
```

### Use Case 2: Transitional Provisions

When laws change, there are often transitional rules: "If you were eligible under the old rules, you remain eligible for 2 years."

```l4
IMPORT daydate

§ `Transitional Protection`

`New Rules Commencement` MEANS DATE OF 1, 1, 2023
`Transitional Period End` MEANS DATE OF 31, 12, 2024

GIVEN applicant IS AN Employee
      applicationDate IS A DATE
GIVETH A BOOLEAN
`protected by transitional provisions` applicant applicationDate MEANS
    inTransitionalPeriod AND wasEligibleUnderOldRules
    WHERE
        inTransitionalPeriod MEANS
                applicationDate AT LEAST `New Rules Commencement`
            AND applicationDate AT MOST `Transitional Period End`

        wasEligibleUnderOldRules MEANS
            EVAL `under rules effective at`
                    DATE OF 31, 12, 2022  -- Day before new rules
                 `evaluate`
                    (`is eligible for work pass` applicant)

-- Main eligibility with transitional protection
GIVEN applicant IS AN Employee
      applicationDate IS A DATE
GIVETH A BOOLEAN
`is eligible considering transition` applicant applicationDate MEANS
    eligibleUnderCurrentRules OR protectedByTransition
    WHERE
        eligibleUnderCurrentRules MEANS
            `is eligible for work pass` applicant

        protectedByTransition MEANS
            `protected by transitional provisions` applicant applicationDate
```

### Use Case 3: Regulatory Change Analysis

Compare outcomes across different versions of the rules:

```l4
§ `Regulatory Impact Analysis`

DECLARE RuleVersion HAS
    commitHash IS A STRING
    effectiveDate IS A DATE
    description IS A STRING

GIVEN applicant IS AN Employee
      versions IS A LIST OF RuleVersion
GIVETH A LIST OF BOOLEAN
`eligibility across versions` applicant versions MEANS
    map checkVersion versions
    WHERE
        checkVersion version MEANS
            EVAL `under commit` version's commitHash
                 `evaluate`
                    (`is eligible for work pass` applicant)

-- Example usage
`all rule versions` MEANS LIST
    RuleVersion WITH
        commitHash IS "abc123"
        effectiveDate IS DATE OF 1, 1, 2020
        description IS "Original rules",
    RuleVersion WITH
        commitHash IS "def456"
        effectiveDate IS DATE OF 1, 1, 2023
        description IS "2023 Amendment",
    RuleVersion WITH
        commitHash IS "ghi789"
        effectiveDate IS DATE OF 1, 7, 2024
        description IS "Mid-year update"

#EVAL `eligibility across versions` `Alice - Tech Professional` `all rule versions`
```

### Use Case 4: Audit Trails and Historical Queries

"What did the system conclude on this application when it was first submitted?"

```l4
§ `Historical Decision Reconstruction`

DECLARE AuditRecord HAS
    applicationId IS A STRING
    evaluationDate IS A DATE
    commitUsed IS A STRING
    systemTimeSnapshot IS A DATE

GIVEN app IS AN Application
      auditRecord IS AN AuditRecord
GIVETH A BOOLEAN
`reconstruct historical decision` app auditRecord MEANS
    EVAL `under commit` auditRecord's commitUsed
         `as of system time` auditRecord's systemTimeSnapshot
         `evaluate`
            (`process application` app)'s approved
```

### Nested Temporal Evaluation

You can nest `EVAL` constructs for counterfactual reasoning:

```l4
-- "If we had used the 2010 rules to evaluate
--  what we would have decided in 2015..."
EVAL `retroactive to` DATE OF 1, 1, 2010
     `evaluate`
        (EVAL `under rules effective at` DATE OF 1, 1, 2015
              `evaluate`
                 (`grant compensation` applicant))
```

## Practical Patterns

### Pattern 1: Rolling Windows

"Valid for 12 months from issue date"

```l4
GIVEN issueDate IS A DATE
      checkDate IS A DATE
GIVETH A BOOLEAN
`still valid on` checkDate `issued on` issueDate MEANS
        checkDate AT LEAST issueDate
    AND checkDate LESS THAN expiryDate
    WHERE
        expiryDate MEANS issueDate PLUS 365
```

### Pattern 2: Anniversary-Based Rules

"Eligible if employed for 3 full years"

```l4
GIVEN employee IS AN Employee
      checkDate IS A DATE
GIVETH A BOOLEAN
`completed three years on` checkDate MEANS
    checkDate AT LEAST thirdAnniversary
    WHERE
        hireDate MEANS employee's hireDate
        thirdAnniversary MEANS hireDate PLUS (365 TIMES 3)
```

### Pattern 3: As-At Queries

"What was their age as at December 31, 2023?"

```l4
GIVEN person IS A Person
      asAtDate IS A DATE
GIVETH A NUMBER
`age as at` asAtDate MEANS
    `age as of` asAtDate `for person born` person's dateOfBirth
```

### Pattern 4: Effective Date Logic

"Rules effective from date X, but only for applications submitted after date Y"

```l4
DECLARE RuleSet IS ONE OF
    LegacyRules
    TransitionalRules
    ModernRules

GIVEN applicationDate IS A DATE
GIVETH A RuleSet
`applicable rule set for` applicationDate MEANS
    IF applicationDate LESS THAN `Legacy Cutoff`
    THEN LegacyRules
    ELSE IF applicationDate LESS THAN `Modern Rules Commencement`
    THEN TransitionalRules
    ELSE ModernRules
    WHERE
        `Legacy Cutoff` MEANS DATE OF 31, 12, 2019
        `Modern Rules Commencement` MEANS DATE OF 1, 7, 2023
```

### Pattern 5: Grace Periods

"Must comply within 90 days of notice"

```l4
GIVEN noticeDate IS A DATE
      complianceDate IS A DATE
GIVETH A BOOLEAN
`complied within grace period` MEANS
    complianceDate AT MOST deadline
    WHERE
        gracePeriodDays MEANS 90
        deadline MEANS noticeDate PLUS gracePeriodDays
```

## Testing Temporal Logic

### Date Boundary Tests

```l4
§ `Temporal Logic Tests`

-- Test exact boundaries
#ASSERT `still valid on` (DATE OF 31, 12, 2024)
                        `issued on` (DATE OF 1, 1, 2024)

#ASSERT NOT `still valid on` (DATE OF 1, 1, 2026)
                            `issued on` (DATE OF 1, 1, 2024)

-- Test one day before/after
#ASSERT `still valid on` (DATE OF 31, 12, 2024)
                        `issued on` (DATE OF 1, 1, 2024)

#ASSERT NOT `still valid on` (DATE OF 1, 1, 2025)
                            `issued on` (DATE OF 1, 1, 2024)
```

### Leap Year Tests

```l4
-- February 29 in leap years
`leap year birthdate` MEANS DATE OF 29, 2, 2000

#EVAL `age as of` (DATE OF 1, 3, 2024)
               `for person born` `leap year birthdate`
-- Should be 24

#EVAL `age as of` (DATE OF 28, 2, 2025)
               `for person born` `leap year birthdate`
-- Should be 24 (not 25 yet)
```

### Temporal EVAL Tests [WIP]

```l4
-- Once EVAL is implemented:

#ASSERT
    (EVAL `retroactive to` DATE OF 1, 1, 2020
          `evaluate` (`minimum salary for` TechProfessional))
    EQUALS 4500  -- Old minimum

#ASSERT
    (`minimum salary for` TechProfessional)
    EQUALS 5000  -- Current minimum
```

## Common Pitfalls

### 1. Date Arithmetic Precision

```l4
-- WRONG: Assumes all months have 30 days
`six months later` date MEANS date PLUS 180

-- BETTER: Use actual month arithmetic when available
-- (daydate library provides month-aware arithmetic)
```

### 2. Timezone Confusion

L4 dates are currently timezone-naive. Document assumptions:

```l4
{-
ASSUMPTION: All dates are in local jurisdiction time (SGT)
For cross-border applications, convert to SGT before evaluation
-}
```

### 3. Inclusive vs Exclusive Ranges

Be explicit about boundaries:

```l4
-- "Valid from Jan 1 to Dec 31" - inclusive on both ends
`valid in 2025` date MEANS
        date AT LEAST DATE OF 1, 1, 2025
    AND date AT MOST DATE OF 31, 12, 2025

-- "Valid for 12 months starting Jan 1" - inclusive start, exclusive end
`valid for 12 months from Jan 1` date MEANS
        date AT LEAST DATE OF 1, 1, 2025
    AND date LESS THAN DATE OF 1, 1, 2026
```

## Key Takeaways

1. **Four temporal dimensions**: Valid time, system time, rule version time, rule encoding time
2. **daydate library** provides comprehensive date arithmetic (available now)
3. **EVAL construct** enables multi-temporal queries (under development)
4. **Test boundary conditions** explicitly (last day of validity, leap years)
5. **Document assumptions** about timezones and date semantics
6. **Use constants** for significant dates (commencement, repeal, etc.)
7. **Transitional logic** often requires comparing old vs new rules

## Exercises

### Exercise 1: Date Arithmetic
Write functions for:
- "Is this person old enough to vote?" (18 years)
- "How many days until their next birthday?"
- "What is the third Tuesday of June 2025?"

### Exercise 2: Rolling Windows
Model a professional certification that:
- Is valid for 3 years from issue
- Has a 6-month grace period after expiry
- Can be renewed up to 3 months before expiry

### Exercise 3: Effective Dates
Model legislation that:
- Comes into force on July 1, 2025
- Has different transitional rules for existing vs new applications
- Contains a sunset clause (expires December 31, 2030)

### Exercise 4: Temporal EVAL (Once Implemented)
Write tests that verify:
- Minimum salary requirements changed between 2020 and 2023
- A specific applicant would have been eligible in 2020 but not in 2023
- Decision outcomes across 5 different Git commits

## Future Developments

Watch for these upcoming temporal features:

- **Tagged rule versions**: `UNDER RULES "latest-in-force"` instead of commit hashes
- **Git blame integration**: `EVAL HISTORY OF rule` to see when a rule changed
- **Streaming evidence**: `WITH FACTS AS OF t` to update knowledge over time
- **Better month arithmetic**: Precise "3 months later" handling
- **Timezone support**: Explicit timezone handling for international applications

## Next Steps

In **Module A4**, we'll build regression testing frameworks to ensure that rule changes don't inadvertently alter historical decisions.
