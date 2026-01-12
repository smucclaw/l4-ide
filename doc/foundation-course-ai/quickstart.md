# Quickstart — L4 By Example

This chapter shows L4's most frequently used syntax working together to solve a real problem: determining insurance eligibility and calculating premiums.

## The Complete Example

```l4
IMPORT prelude
IMPORT daydate

§ `Auto Insurance Eligibility & Premium Calculator`

§§ `Type Definitions`

-- Enum: A type with a fixed set of values
DECLARE RiskCategory IS ONE OF
    LowRisk
    MediumRisk
    HighRisk
    Uninsurable

-- Record: A type with named fields
DECLARE Driver HAS
    name            IS A STRING
    age             IS A NUMBER
    yearsLicensed   IS A NUMBER
    accidentCount   IS A NUMBER
    hasTickets      IS A BOOLEAN

DECLARE Policy HAS
    driver          IS A Driver
    vehicleValue    IS A NUMBER
    coverageLevel   IS A STRING

DECLARE Quote HAS
    eligible        IS A BOOLEAN
    basePremium     IS A MAYBE NUMBER
    riskCategory    IS A RiskCategory
    reason          IS A STRING

§§ `Core Business Logic`

-- Simple decision rule
GIVEN driver IS A Driver
GIVETH A BOOLEAN
DECIDE `meets minimum age` IF
    driver's age AT LEAST 18

-- Another decision rule
GIVEN driver IS A Driver
GIVETH A BOOLEAN
DECIDE `has sufficient experience` IF
    driver's yearsLicensed AT LEAST 2

-- Pattern matching with CONSIDER
GIVEN driver IS A Driver
GIVETH A RiskCategory
`assess risk` driver MEANS
    CONSIDER driver's accidentCount
    WHEN 0 THEN
        IF driver's hasTickets
        THEN MediumRisk
        ELSE LowRisk
    WHEN 1 THEN MediumRisk
    WHEN 2 THEN HighRisk
    OTHERWISE Uninsurable

-- Working with MAYBE (optional values)
GIVEN category IS A RiskCategory
      vehicleValue IS A NUMBER
GIVETH A MAYBE NUMBER
`calculate base premium` category vehicleValue MEANS
    CONSIDER category
    WHEN LowRisk     THEN JUST (vehicleValue TIMES 0.02)
    WHEN MediumRisk  THEN JUST (vehicleValue TIMES 0.04)
    WHEN HighRisk    THEN JUST (vehicleValue TIMES 0.08)
    WHEN Uninsurable THEN NOTHING

-- Lists and higher-order functions
GIVEN drivers IS A LIST OF Driver
GIVETH A LIST OF Driver
`filter eligible drivers` drivers MEANS
    filter `is eligible for insurance` drivers

-- Composing it all together with WHERE clause
GIVEN driver IS A Driver
      vehicleValue IS A NUMBER
GIVETH A Quote
`generate quote` driver vehicleValue MEANS
    Quote WITH
        eligible      IS isEligible
        basePremium   IS premium
        riskCategory  IS risk
        reason        IS message
    WHERE
        isEligible MEANS `is eligible for insurance` driver
        risk       MEANS `assess risk` driver
        premium    MEANS
            IF isEligible
            THEN `calculate base premium` risk vehicleValue
            ELSE NOTHING
        message    MEANS
            IF isEligible
            THEN "Policy can be issued"
            ELSE `ineligibility reason` driver

§§ `Helper Functions`

GIVEN driver IS A Driver
GIVETH A BOOLEAN
`is eligible for insurance` driver MEANS
        `meets minimum age` driver
    AND `has sufficient experience` driver
    AND driver's accidentCount AT MOST 3

GIVEN driver IS A Driver
GIVETH A STRING
`ineligibility reason` driver MEANS
    IF NOT `meets minimum age` driver
    THEN "Driver is under 18 years old"
    ELSE IF NOT `has sufficient experience` driver
         THEN "Driver needs at least 2 years of experience"
         ELSE IF driver's accidentCount GREATER THAN 3
              THEN "Too many accidents on record"
              ELSE "Unknown reason"
-- this cascade of nesting is not ideal; BRANCH syntax works better.

-- List operations example
GIVEN drivers IS A LIST OF Driver
GIVETH A NUMBER
`count high risk drivers` drivers MEANS
    `length of` highRiskDrivers
    WHERE
        highRiskDrivers MEANS
            filter isHighRisk drivers
        isHighRisk d MEANS
            `assess risk` d EQUALS HighRisk
        `length of` list MEANS
            CONSIDER list
            WHEN EMPTY THEN 0
            WHEN x FOLLOWED BY xs THEN 1 PLUS `length of` xs

§§ `Test Data`

-- Creating sample data
`Alice` MEANS Driver WITH
    name          IS "Alice"
    age           IS 25
    yearsLicensed IS 7
    accidentCount IS 0
    hasTickets    IS FALSE

`Bob` MEANS Driver WITH
    name          IS "Bob"
    age           IS 19
    yearsLicensed IS 3
    accidentCount IS 2
    hasTickets    IS TRUE

`Charlie` MEANS Driver WITH
    name          IS "Charlie"
    age           IS 17
    yearsLicensed IS 1
    accidentCount IS 0
    hasTickets    IS FALSE

`Diana` MEANS Driver WITH
    name          IS "Diana"
    age           IS 45
    yearsLicensed IS 25
    accidentCount IS 5
    hasTickets    IS FALSE

§§ `Test Execution`

-- Simple evaluations
#EVAL `meets minimum age` `Alice`
#EVAL `assess risk` `Alice`
#EVAL `assess risk` `Bob`

-- Complex evaluation
#EVAL `generate quote` `Alice` 25000

-- Testing with lists
#EVAL `filter eligible drivers` (LIST `Alice`, `Bob`, `Charlie`, `Diana`)

-- Multiple test cases
#EVAL LIST  `generate quote` `Alice`   25000,
            `generate quote` `Bob`     30000,
            `generate quote` `Charlie` 20000,
            `generate quote` `Diana`   40000

-- Assertions for testing
#ASSERT `is eligible for insurance` `Alice`
#ASSERT NOT `is eligible for insurance` `Charlie`
#ASSERT `assess risk` `Alice` EQUALS LowRisk
```

## Key Syntax Patterns Demonstrated

### 1. Type Declarations

```l4
DECLARE EnumType IS ONE OF Value1, Value2

DECLARE RecordType HAS
    field1 IS A TYPE1
    field2 IS A TYPE2
```

### 2. Function Definitions

```l4
GIVEN param1 IS A Type1
      param2 IS A Type2
GIVETH A ReturnType
functionName param1 param2 MEANS expression
```

### 3. Decision Rules

```l4
DECIDE ruleName IF condition1 AND condition2
```

### 4. Pattern Matching

```l4
CONSIDER value
WHEN Pattern1 THEN Result1
WHEN Pattern2 THEN Result2
OTHERWISE DefaultResult
```

### 5. Lists

```l4
EMPTY                      -- empty list
LIST a, b, c              -- list literal
x FOLLOWED BY xs          -- cons pattern
filter predicate list     -- filter function
map function list         -- map function
```

### 6. Maybe (Optional Values)

```l4
NOTHING                   -- no value
JUST value               -- has value
CONSIDER maybe
WHEN NOTHING THEN ...
WHEN JUST x THEN ...
```

### 7. Records

```l4
RecordType WITH           -- construction
    field1 IS value1
    field2 IS value2

record's fieldName        -- field access
```

### 8. WHERE Clauses

```l4
mainExpression
WHERE
    helper1 MEANS expression1
    helper2 MEANS expression2
```

`WHERE` can be attached to any expression (not just top-level definitions), so you can place local helpers close to where they are used and even nest `WHERE` blocks for smaller scopes.

### 9. Boolean Logic

```l4
condition1 AND condition2
condition1 OR condition2
NOT condition
IF cond THEN expr1 ELSE expr2
```

### 10. Comparisons

```l4
x EQUALS y
x GREATER THAN y
x LESS THAN y
x AT LEAST y              -- >=
x AT MOST y               -- <=
```

## What Happens When You Run This?

```bash
cabal run jl4-cli -- insurance.l4
```

The compiler will:

1. **Type-check** all definitions
2. **Execute** each #EVAL directive
3. **Display** results
4. **Verify** each #ASSERT directive

Sample output:

```
TRUE
LowRisk
HighRisk
Quote { eligible = True, basePremium = Just 500.0, riskCategory = LowRisk, reason = "Policy can be issued" }
[Driver { name = "Alice", ... }, Driver { name = "Bob", ... }]
...
```

## Next Steps

Now that you've seen L4 in action, dive deeper:

- **Module 0**: Understanding L4's purpose and file structure
- **Module 1**: Enums and Records in detail
- **Module 2**: Functions and type signatures
- **Module 3**: Control flow patterns
- **Module 4**: Lists and Maybe operations
- **Module 5**: Building multi-step pipelines
- **Module 6**: Comprehensive testing strategies
