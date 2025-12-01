# Module 3 — Control Flow: CONSIDER, WHEN, BRANCH

## Overview

In JavaScript or Python, you'd use `switch` or `if/elif/else` chains to handle different cases. L4 provides **pattern matching** with `CONSIDER/WHEN`, which is more powerful and safer than traditional control flow.

Pattern matching lets you:
- Handle different enum values
- Destructure data
- Ensure you've covered all cases (exhaustiveness checking)

## Basic CONSIDER/WHEN: Switch on Steroids

### JavaScript Switch Equivalent

JavaScript:
```javascript
function getRiskLevel(category) {
    switch(category) {
        case 'TechProfessional':
            return 'Low';
        case 'HealthcareWorker':
            return 'Medium';
        case 'Researcher':
            return 'Low';
        default:
            return 'High';
    }
}
```

L4:
```l4
GIVEN category IS AN EmploymentCategory
GIVETH A STRING
`risk level for` category MEANS
    CONSIDER category
    WHEN TechProfessional  THEN "Low"
    WHEN HealthcareWorker  THEN "Medium"
    WHEN Researcher        THEN "Low"
    OTHERWISE "High"
```

### Key Advantages Over Switch

1. **Type-safe**: The compiler knows all possible enum values
2. **Exhaustiveness checking**: Warns if you miss a case
3. **No fall-through bugs**: Each case is independent
4. **Return values**: Every branch returns a value (no break statements)

## Pattern Matching Fundamentals

### Matching on Enums

```l4
DECLARE ApplicationStatus IS ONE OF
    Draft
    Submitted
    Approved
    Rejected

GIVEN status IS AN ApplicationStatus
GIVETH A STRING
`status message` status MEANS
    CONSIDER status
    WHEN Draft     THEN "Application is being prepared"
    WHEN Submitted THEN "Application is under review"
    WHEN Approved  THEN "Work pass has been granted"
    WHEN Rejected  THEN "Application was not successful"
```

### Matching on Numbers

```l4
GIVEN score IS A NUMBER
GIVETH A STRING
`grade for` score MEANS
    CONSIDER score
    WHEN 0 THEN "F"
    WHEN 1 THEN "F"
    WHEN 2 THEN "D"
    WHEN 3 THEN "C"
    WHEN 4 THEN "B"
    WHEN 5 THEN "A"
    OTHERWISE "Invalid score"
```

### OTHERWISE: The Default Case

`OTHERWISE` is like `default` in JavaScript's switch:

```l4
CONSIDER value
WHEN Pattern1 THEN Result1
WHEN Pattern2 THEN Result2
OTHERWISE DefaultResult
```

**Best practice**: Always include `OTHERWISE` unless you've explicitly covered all cases.

## Destructuring in Patterns

When an enum carries data, you can **extract** that data in the pattern:

```l4
DECLARE ApplicationOutcome IS ONE OF
    Approved HAS
        permitNumber IS A STRING
        expiryDate   IS A DATE
    Rejected HAS
        reason       IS A STRING
    Pending HAS
        daysRemaining IS A NUMBER

GIVEN outcome IS AN ApplicationOutcome
GIVETH A STRING
`outcome summary` outcome MEANS
    CONSIDER outcome
    WHEN Approved WITH permitNumber, expiryDate THEN
        "Approved: " APPEND permitNumber
    WHEN Rejected WITH reason THEN
        "Rejected: " APPEND reason
    WHEN Pending WITH daysRemaining THEN
        "Pending: " APPEND (STRING daysRemaining) APPEND " days left"
```

The pattern `Approved WITH permitNumber, expiryDate` **binds** those fields to local variables you can use in the THEN branch.

## Nested Pattern Matching

You can nest `CONSIDER` expressions:

```l4
GIVEN employee IS AN Employee
      employer IS A Company
GIVETH A STRING
`application advice` employee employer MEANS
    CONSIDER employee's category
    WHEN TechProfessional THEN
        CONSIDER employee's yearsExperience
        WHEN 0 THEN "Entry-level tech position"
        WHEN 1 THEN "Junior developer"
        WHEN 2 THEN "Mid-level developer"
        OTHERWISE "Senior developer"
    WHEN HealthcareWorker THEN
        "Healthcare professional application"
    OTHERWISE
        "Standard application process"
```

But often, it's clearer to use helper functions:

```l4
`application advice` employee employer MEANS
    CONSIDER employee's category
    WHEN TechProfessional THEN
        `tech role advice` employee's yearsExperience
    WHEN HealthcareWorker THEN
        "Healthcare professional application"
    OTHERWISE
        "Standard application process"
    WHERE
        `tech role advice` years MEANS
            CONSIDER years
            WHEN 0 THEN "Entry-level tech position"
            WHEN 1 THEN "Junior developer"
            OTHERWISE "Experienced developer"
```

## Pattern Matching on Lists

Lists have two patterns:
- `EMPTY` — the empty list
- `head FOLLOWED BY tail` — a list with at least one element

```l4
GIVEN list IS A LIST OF NUMBER
GIVETH A NUMBER
`sum of` list MEANS
    CONSIDER list
    WHEN EMPTY THEN 0
    WHEN x FOLLOWED BY xs THEN
        x PLUS `sum of` xs
```

This is **recursive**—the function calls itself on the tail of the list until reaching the empty list.

### List Pattern Examples

```l4
-- Get first element (if exists)
GIVEN list IS A LIST OF Employee
GIVETH A MAYBE Employee
`first employee in` list MEANS
    CONSIDER list
    WHEN EMPTY THEN NOTHING
    WHEN e FOLLOWED BY others THEN JUST e

-- Check if list has at least 3 elements
GIVEN list IS A LIST OF STRING
GIVETH A BOOLEAN
`has at least three items` list MEANS
    CONSIDER list
    WHEN a FOLLOWED BY b FOLLOWED BY c FOLLOWED BY rest THEN TRUE
    OTHERWISE FALSE

-- Get second element
GIVEN list IS A LIST OF NUMBER
GIVETH A MAYBE NUMBER
`second element of` list MEANS
    CONSIDER list
    WHEN first FOLLOWED BY second FOLLOWED BY rest THEN
        JUST second
    OTHERWISE
        NOTHING
```

## IF/THEN/ELSE: Simple Branching

For simple binary decisions, use `IF/THEN/ELSE`:

```l4
GIVEN age IS A NUMBER
GIVETH A STRING
`life stage` age MEANS
    IF age LESS THAN 18
    THEN "Minor"
    ELSE IF age LESS THAN 65
    THEN "Working age"
    ELSE "Retirement age"
```

### When to Use IF vs CONSIDER

- **Use IF** for binary decisions or simple numeric comparisons
- **Use CONSIDER** for matching on enum values, deconstructing data, or handling multiple distinct cases

```l4
-- Good use of IF
IF score AT LEAST 50 THEN "Pass" ELSE "Fail"

-- Good use of CONSIDER
CONSIDER applicationStatus
WHEN Draft THEN ...
WHEN Submitted THEN ...
WHEN Approved THEN ...
```

## Decision Tables in L4

Legal and business rules often appear as decision tables. L4's pattern matching naturally expresses these:

### Example: WPA Processing Fee

| Category | Experience | Fee |
|----------|-----------|------|
| Tech | < 5 years | $200 |
| Tech | >= 5 years | $150 |
| Healthcare | Any | $175 |
| Researcher | PhD | $100 |
| Researcher | Other | $150 |
| Other | Any | $200 |

In L4:

```l4
GIVEN employee IS AN Employee
GIVETH A NUMBER
`application fee for` employee MEANS
    CONSIDER employee's category
    WHEN TechProfessional THEN
        IF employee's yearsExperience LESS THAN 5
        THEN 200
        ELSE 150
    WHEN HealthcareWorker THEN
        175
    WHEN Researcher THEN
        IF employee's educationLevel EQUALS Doctorate
        THEN 100
        ELSE 150
    OTHERWISE
        200
```

### Multi-Dimensional Decision Table

```l4
GIVEN employee IS AN Employee
      employer IS A Company
GIVETH A STRING
`priority level` employee employer MEANS
    CONSIDER employee's category
    WHEN TechProfessional THEN
        CONSIDER employer's industry
        WHEN "Technology" THEN "High"
        WHEN "Finance" THEN "High"
        OTHERWISE "Medium"
    WHEN HealthcareWorker THEN
        "High"
    WHEN Researcher THEN
        IF employer's paidUpCapital AT LEAST 1000000
        THEN "High"
        ELSE "Medium"
    OTHERWISE
        "Standard"
```

## Pattern Guards: Adding Conditions to Patterns

Sometimes you want to match a pattern *and* check a condition. Use `PROVIDED`:

```l4
DECLARE PaymentStatus IS ONE OF
    Paid HAS
        amount IS A NUMBER
        date IS A DATE
    Pending
    Failed

GIVEN status IS A PaymentStatus
      minimumAmount IS A NUMBER
GIVETH A BOOLEAN
`payment is sufficient` status minimumAmount MEANS
    CONSIDER status
    WHEN Paid WITH amount, date PROVIDED amount AT LEAST minimumAmount THEN
        TRUE
    OTHERWISE
        FALSE
```

The pattern matches `Paid`, extracts `amount` and `date`, *then* checks if `amount AT LEAST minimumAmount`.

## Exhaustiveness Checking

L4's compiler checks if you've covered all cases:

```l4
DECLARE Status IS ONE OF Active, Inactive, Suspended

GIVEN status IS A Status
GIVETH A STRING
getMessage status MEANS
    CONSIDER status
    WHEN Active THEN "Account is active"
    WHEN Inactive THEN "Account is inactive"
    -- Missing Suspended case! Compiler warns.
```

This prevents bugs where you add a new enum value but forget to handle it everywhere.

## WPA Control Flow Examples

### Complete Eligibility Decision Tree

```l4
§ `WPA Application Processing Logic`

IMPORT prelude
IMPORT daydate

GIVEN application IS A WorkPassApplication
GIVETH A STRING
`eligibility decision` application MEANS
    IF NOT `age eligibility` employee
    THEN "Rejected: Age requirement not met"
    ELSE IF NOT `education eligibility` employee
    THEN "Rejected: Education requirement not met"
    ELSE IF NOT `salary eligibility` employee
    THEN "Rejected: Salary below minimum"
    ELSE IF NOT `quota eligibility` employer
    THEN "Rejected: Employer over foreign worker quota"
    ELSE
        "Approved: All requirements met"
    WHERE
        employee MEANS application's employee
        employer MEANS application's employer

        `age eligibility` emp MEANS
                `age of` emp `as of` `today` AT LEAST 18
            AND `age of` emp `as of` `today` AT MOST 65

        `education eligibility` emp MEANS
            `education meets requirement` emp

        `salary eligibility` emp MEANS
            emp's monthlySalary AT LEAST
                `minimum monthly salary for` emp's category

        `quota eligibility` comp MEANS
            comp's localEmployeeCount AT LEAST
                comp's foreignWorkerQuota
```

### Processing Timeline Based on Category

```l4
GIVEN category IS AN EmploymentCategory
GIVETH A NUMBER
`standard processing days for` category MEANS
    CONSIDER category
    WHEN TechProfessional THEN
        5  -- Fast-track for tech
    WHEN HealthcareWorker THEN
        7
    WHEN Researcher THEN
        10  -- More documentation needed
    WHEN FinancialServices THEN
        14  -- Extensive background checks
    OTHERWISE
        21  -- Standard processing
```

### Risk-Based Document Requirements

```l4
DECLARE Document IS ONE OF
    Passport
    Diploma
    EmploymentContract
    FinancialStatements
    ReferenceLetter
    BackgroundCheck

GIVEN employee IS AN Employee
      employer IS A Company
GIVETH A LIST OF Document
`required documents for` employee `employed by` employer MEANS
    baseDocuments APPEND additionalDocuments
    WHERE
        baseDocuments MEANS LIST
            Passport,
            Diploma,
            EmploymentContract

        additionalDocuments MEANS
            CONSIDER employee's category
            WHEN FinancialServices THEN
                LIST FinancialStatements, BackgroundCheck
            WHEN Researcher THEN
                LIST ReferenceLetter
            WHEN TechProfessional THEN
                IF employer's foundedYear AT LEAST 2020
                THEN LIST ReferenceLetter
                ELSE EMPTY
            OTHERWISE
                EMPTY
```

## Key Takeaways

1. **CONSIDER/WHEN** is pattern matching—more powerful than switch/case
2. **Patterns can destructure data** (extract fields from enums)
3. **OTHERWISE** is your default case
4. **List patterns**: `EMPTY` and `head FOLLOWED BY tail`
5. **IF/THEN/ELSE** for simple binary decisions
6. **Decision tables** map naturally to nested CONSIDER expressions
7. **Exhaustiveness checking** catches missing cases at compile time
8. **Pattern guards (PROVIDED)** add conditions to matches

## Exercises

### Exercise 1: Simple Pattern Match
Write a function that takes an `EducationLevel` and returns years of study typically required.

### Exercise 2: Nested Patterns
Write a function that determines application fee based on both category and years of experience (as a decision table).

### Exercise 3: List Patterns
Write a function that checks if a list of employees has any TechProfessional in it (hint: use recursion with list patterns).

## Next Steps

In **Module 4**, we'll explore `MAYBE` (optional values) and list operations in depth, building on the pattern matching skills from this module.
