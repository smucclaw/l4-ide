# Module A5 — Rebuttable Presumptions with TYPICALLY [WIP]

> ⚠️ **Work in Progress**: The `TYPICALLY` keyword is under active development. See [TYPICALLY-DEFAULTS-SPEC.md](../todo/TYPICALLY-DEFAULTS-SPEC.md) for the latest specification. This module describes the design and intended usage.

## Overview

Legal reasoning often relies on **rebuttable presumptions**—assumptions that hold true unless proven otherwise:

- A person is *typically* of sound mind and has legal capacity
- A contract party is *typically* not acting under duress
- A transaction is *typically* conducted at arm's length
- A taxpayer is *typically* a resident of their stated jurisdiction

The `TYPICALLY` keyword brings this legal concept into L4, providing:

1. **Default values** for parameters and fields
2. **Reduced question burden** in interactive applications
3. **Explicit assumptions** for formal verification
4. **Clearer code** by making implicit assumptions visible

## The Problem: Information Overload

### Without Defaults

Consider a function to determine alcohol purchase eligibility:

```l4
GIVEN
    age IS A NUMBER
    married IS A BOOLEAN
    has_spousal_approval IS A BOOLEAN
    beer_only IS A BOOLEAN
    has_parental_approval IS A BOOLEAN
    is_emancipated IS A BOOLEAN
GIVETH A BOOLEAN
DECIDE `may purchase alcohol` IF
        age AT LEAST 21
    OR  (age AT LEAST 18 AND married AND has_spousal_approval)
    OR  (age AT LEAST 18 AND beer_only)
    OR  (age LESS THAN 18 AND has_parental_approval)
    OR  (age LESS THAN 18 AND is_emancipated)
```

**Problem:** An interactive chatbot must ask 6 questions even for a simple case (30-year-old adult).

**User experience:**
```
Bot: How old are you?
User: 30
Bot: Are you married?
User: No
Bot: Do you have spousal approval?
User: [confused - I just said I'm not married]
Bot: Are you buying beer only?
User: [frustrated] Why does this matter?
...
```

### With TYPICALLY Defaults

```l4
GIVEN
    age IS A NUMBER
    married IS A BOOLEAN TYPICALLY FALSE
    has_spousal_approval IS A BOOLEAN TYPICALLY FALSE
    beer_only IS A BOOLEAN TYPICALLY FALSE
    has_parental_approval IS A BOOLEAN TYPICALLY FALSE
    is_emancipated IS A BOOLEAN TYPICALLY FALSE
GIVETH A BOOLEAN
DECIDE `may purchase alcohol` IF
        age AT LEAST 21
    OR  (age AT LEAST 18 AND married AND has_spousal_approval)
    OR  (age AT LEAST 18 AND beer_only)
    OR  (age LESS THAN 18 AND has_parental_approval)
    OR  (age LESS THAN 18 AND is_emancipated)
```

**Improved experience:**
```
Bot: How old are you?
User: 30
Bot: You can purchase alcohol! ✓
```

The bot **assumes** the typical case (unmarried, no special circumstances) unless the user indicates otherwise.

## Part 1: TYPICALLY in Record Types (DECLARE)

### Basic Syntax

```l4
DECLARE TypeName HAS
    fieldName IS A FieldType TYPICALLY defaultValue
```

### Example: Person with Legal Presumptions

```l4
DECLARE Person HAS
    name IS A STRING
    age IS A NUMBER
    has_capacity IS A BOOLEAN TYPICALLY TRUE
    is_natural_person IS A BOOLEAN TYPICALLY TRUE
    is_under_duress IS A BOOLEAN TYPICALLY FALSE
    jurisdiction IS A STRING TYPICALLY "Singapore"
```

**Interpretation:**
- Unless proven otherwise, a person has legal capacity
- Unless proven otherwise, a person is a natural person (not a corporation)
- Unless proven otherwise, a person is not acting under duress
- Unless specified, jurisdiction defaults to Singapore

### Example: Contract Terms

```l4
DECLARE Contract HAS
    contractId IS A STRING
    effectiveDate IS A DATE
    terminationDate IS A MAYBE DATE TYPICALLY NOTHING
    governingLaw IS A STRING TYPICALLY "Singapore"
    hasArbitrationClause IS A BOOLEAN TYPICALLY FALSE
    atArmsLength IS A BOOLEAN TYPICALLY TRUE
    inWriting IS A BOOLEAN TYPICALLY TRUE
```

**Interpretation:**
- Contracts typically have no termination date (perpetual or project-based)
- Governed by Singapore law unless stated otherwise
- Typically no arbitration clause
- Typically conducted at arm's length
- Typically documented in writing

### Creating Records with Defaults

```l4
-- Minimal specification (uses defaults)
`simple person` MEANS Person WITH
    name IS "Alice"
    age IS 30
    -- All other fields use TYPICALLY values

-- Override specific defaults
`corporation` MEANS Person WITH
    name IS "TechCorp Pte Ltd"
    age IS 10  -- Years since incorporation
    is_natural_person IS FALSE  -- Override default
    -- Other fields still use defaults
```

## Part 2: TYPICALLY in Function Parameters (GIVEN)

### Basic Syntax

```l4
GIVEN
    paramName IS A ParamType TYPICALLY defaultValue
GIVETH A ReturnType
functionName paramName MEANS ...
```

### Example: Loan Eligibility

```l4
IMPORT daydate

DECLARE LoanApplication HAS
    applicantAge IS A NUMBER
    annualIncome IS A NUMBER
    requestedAmount IS A NUMBER
    hasCollateral IS A BOOLEAN TYPICALLY FALSE
    hasCoSigner IS A BOOLEAN TYPICALLY FALSE
    isCitizen IS A BOOLEAN TYPICALLY TRUE
    hasGoodCredit IS A BOOLEAN TYPICALLY TRUE

GIVEN
    app IS A LoanApplication
    currentDate IS A DATE TYPICALLY `today`
GIVETH A BOOLEAN
DECIDE `qualifies for loan` IF
        app's annualIncome GREATER THAN minIncome
    AND app's applicantAge AT LEAST 21
    AND app's applicantAge AT MOST 70
    AND creditRequirement
    WHERE
        minIncome MEANS app's requestedAmount TIMES 0.4

        creditRequirement MEANS
            IF app's hasCollateral OR app's hasCoSigner
            THEN TRUE  -- Relaxed requirement
            ELSE app's hasGoodCredit
```

**Effect:**
- If `currentDate` is omitted, uses today's date
- Most applicants don't have collateral or co-signers (default FALSE)
- Most applicants are citizens with good credit (default TRUE)

### Example: Tax Calculation

```l4
DECLARE TaxCalculation HAS
    grossIncome IS A NUMBER
    deductions IS A NUMBER TYPICALLY 0
    taxCredits IS A NUMBER TYPICALLY 0
    isResident IS A BOOLEAN TYPICALLY TRUE
    dependents IS A NUMBER TYPICALLY 0

GIVEN
    calc IS A TaxCalculation
GIVETH A NUMBER
`calculate tax owed` calc MEANS
    taxableIncome TIMES taxRate MINUS calc's taxCredits
    WHERE
        taxableIncome MEANS
            calc's grossIncome MINUS calc's deductions

        taxRate MEANS
            IF calc's isResident
            THEN `resident tax rate` taxableIncome
            ELSE `non-resident tax rate` taxableIncome
```

**Effect:**
- Most taxpayers don't have deductions or credits (default 0)
- Most taxpayers are residents (default TRUE)
- Most taxpayers have no dependents (default 0)

## Part 3: TYPICALLY in Assumptions (ASSUME)

### Basic Syntax

```l4
ASSUME `description` IS A Type TYPICALLY defaultValue
```

### Example: External System Assumptions

```l4
§ `System Assumptions`

-- Database availability
ASSUME `database is accessible` IS A BOOLEAN TYPICALLY TRUE

-- Default jurisdiction
ASSUME `applicable law` IS A STRING TYPICALLY "Singapore"

-- External service availability
ASSUME `credit bureau reachable` IS A BOOLEAN TYPICALLY TRUE

-- Standard business days
ASSUME `is business day` IS A FUNCTION FROM DATE TO BOOLEAN
    TYPICALLY (GIVEN d YIELD `weekday of` d AT LEAST 1 AND `weekday of` d AT MOST 5)
```

**Effect:**
- Explicit about what the system assumes
- Makes dependencies visible
- Enables "what if" reasoning ("what if the credit bureau is down?")

## Part 4: Intelligent Question Flow

### Conditional Questioning Based on Defaults

With TYPICALLY, interactive applications can ask questions **only when they matter**:

```l4
DECLARE Applicant HAS
    age IS A NUMBER
    married IS A BOOLEAN TYPICALLY FALSE
    spouse_age IS A MAYBE NUMBER TYPICALLY NOTHING
    dependents IS A NUMBER TYPICALLY 0

GIVEN app IS An Applicant
GIVETH A BOOLEAN
DECIDE `eligible for family plan` IF
        app's age AT LEAST 18
    AND (NOT app's married OR spouse_eligible)
    AND dependency_bonus
    WHERE
        spouse_eligible MEANS
            CONSIDER app's spouse_age
            WHEN NOTHING THEN FALSE
            WHEN JUST age THEN age AT LEAST 18

        dependency_bonus MEANS app's dependents AT LEAST 1
```

**Smart questioning:**
```
Bot: How old are you?
User: 30

Bot: Are you married?
User: No
[Bot skips spouse questions - defaults apply]

Bot: Do you have dependents?
User: No
[Bot uses default 0]

Bot: You qualify for the individual plan!
```

**Vs. if user says "Yes" to married:**
```
Bot: How old are you?
User: 30

Bot: Are you married?
User: Yes

Bot: How old is your spouse?
User: 28
[Bot now asks about spouse since it matters]

Bot: Do you have dependents?
User: 2

Bot: You qualify for the family plan!
```

## Part 5: Formal Verification with Explicit Assumptions

### Making Assumptions Auditable

```l4
§ `Contract Validity Rules`

DECLARE ContractValidityCheck HAS
    parties_have_capacity IS A BOOLEAN TYPICALLY TRUE
    free_consent IS A BOOLEAN TYPICALLY TRUE
    lawful_consideration IS A BOOLEAN TYPICALLY TRUE
    lawful_object IS A BOOLEAN TYPICALLY TRUE
    not_void IS A BOOLEAN TYPICALLY TRUE

GIVEN check IS A ContractValidityCheck
GIVETH A BOOLEAN
DECIDE `contract is valid` IF
        check's parties_have_capacity
    AND check's free_consent
    AND check's lawful_consideration
    AND check's lawful_object
    AND check's not_void
```

**When evaluating:**
```l4
#EVAL `contract is valid` (ContractValidityCheck WITH {})
```

**Output includes assumptions:**
```
Result: TRUE

Assumptions applied (from TYPICALLY):
  ✓ parties_have_capacity = TRUE
  ✓ free_consent = TRUE
  ✓ lawful_consideration = TRUE
  ✓ lawful_object = TRUE
  ✓ not_void = TRUE

This result holds under the above presumptions.
To challenge the result, provide evidence that any presumption is false.
```

## Part 6: Type Safety and Validation

### TYPICALLY Must Match Type

```l4
-- ✓ OK: Boolean default for Boolean field
DECLARE Foo HAS
    flag IS A BOOLEAN TYPICALLY TRUE

-- ✓ OK: Number default for Number field
DECLARE Foo HAS
    count IS A NUMBER TYPICALLY 0

-- ✓ OK: String default for String field
DECLARE Foo HAS
    name IS A STRING TYPICALLY "unknown"

-- ✗ ERROR: Type mismatch
DECLARE Foo HAS
    flag IS A BOOLEAN TYPICALLY 42
    -- Type error: Expected BOOLEAN, got NUMBER

-- ✗ ERROR: Type mismatch
DECLARE Foo HAS
    age IS A NUMBER TYPICALLY TRUE
    -- Type error: Expected NUMBER, got BOOLEAN
```

### TYPICALLY Must Be a Literal

```l4
-- ✓ OK: Literal values
DECLARE Foo HAS
    x IS A NUMBER TYPICALLY 42
    y IS A BOOLEAN TYPICALLY FALSE
    z IS A STRING TYPICALLY "default"

-- ✗ ERROR: Not a literal (expression)
DECLARE Foo HAS
    x IS A NUMBER TYPICALLY (40 PLUS 2)
    -- Error: TYPICALLY value must be a literal, not an expression

-- ✗ ERROR: Not a literal (function call)
DECLARE Foo HAS
    today IS A DATE TYPICALLY `current date`
    -- Error: TYPICALLY value must be a literal
```

**Workaround for complex defaults:**
```l4
-- Define the default separately
`default date` MEANS DATE OF 1, 1, 2025

-- Use it in logic, not TYPICALLY
GIVEN d IS A MAYBE DATE
process d MEANS
    CONSIDER d
    WHEN NOTHING THEN `use default` `default date`
    WHEN JUST date THEN `use given` date
```

## Part 7: MAYBE Types with TYPICALLY

### NOTHING as Default

```l4
DECLARE Document HAS
    title IS A STRING
    expiryDate IS A MAYBE DATE TYPICALLY NOTHING
    arbitrationClause IS A MAYBE STRING TYPICALLY NOTHING
```

**Interpretation:**
- By default, documents don't expire
- By default, no arbitration clause

### JUST as Default

```l4
DECLARE Config HAS
    debugMode IS A MAYBE BOOLEAN TYPICALLY JUST FALSE
    maxRetries IS A MAYBE NUMBER TYPICALLY JUST 3
```

**Interpretation:**
- Debug mode defaults to explicitly disabled (not unknown)
- Retries default to 3 (not unlimited or unknown)

## Part 8: Nested Records and Default Propagation

### Defaults Apply at Each Level

```l4
DECLARE Address HAS
    street IS A STRING
    city IS A STRING
    country IS A STRING TYPICALLY "Singapore"
    postalCode IS A STRING TYPICALLY "000000"

DECLARE Person HAS
    name IS A STRING
    age IS A NUMBER
    address IS An Address
    notifyByEmail IS A BOOLEAN TYPICALLY TRUE
```

**Creating a person:**
```l4
`alice` MEANS Person WITH
    name IS "Alice"
    age IS 30
    address IS Address WITH
        street IS "123 Main St"
        city IS "Central"
        -- country and postalCode use TYPICALLY defaults
    -- notifyByEmail uses TYPICALLY default (TRUE)
```

**Result:**
```l4
Person {
    name: "Alice",
    age: 30,
    address: Address {
        street: "123 Main St",
        city: "Central",
        country: "Singapore",  -- from TYPICALLY
        postalCode: "000000"   -- from TYPICALLY
    },
    notifyByEmail: TRUE  -- from TYPICALLY
}
```

## Part 9: Overriding Defaults

### Innermost Wins

```l4
DECLARE Person HAS
    age IS A NUMBER TYPICALLY 30

GIVEN
    p IS A Person
    age IS A NUMBER TYPICALLY 21
GIVETH A BOOLEAN
someFunction p age MEANS
    -- Which default for 'age'?
    -- Answer: The GIVEN's TYPICALLY 21 wins (innermost)
```

**Rule:** More specific declarations override less specific ones.

### Explicit Values Override Everything

```l4
-- With defaults
DECLARE Config HAS
    timeout IS A NUMBER TYPICALLY 30
    retries IS A NUMBER TYPICALLY 3

-- Explicit value provided
`custom config` MEANS Config WITH
    timeout IS 60  -- Override default
    retries IS 5   -- Override default

-- Some defaults, some overrides
`mixed config` MEANS Config WITH
    timeout IS 60  -- Override
    -- retries uses TYPICALLY default (3)
```

## Part 10: Best Practices

### 1. Use TYPICALLY for Common Cases

```l4
-- GOOD: Most people are not under duress
DECLARE Person HAS
    under_duress IS A BOOLEAN TYPICALLY FALSE

-- AVOID: Don't use TYPICALLY for properties that vary widely
DECLARE Person HAS
    annual_income IS A NUMBER TYPICALLY 50000
    -- BAD: Income varies too much for a meaningful default
```

### 2. Document Assumptions

```l4
§ `Legal Presumptions`

{-
The following TYPICALLY defaults represent rebuttable presumptions
under Singapore law. They may not apply in other jurisdictions.
-}

DECLARE ContractParty HAS
    has_capacity IS A BOOLEAN TYPICALLY TRUE
        -- Presumption: Adults have legal capacity (Contracts Act s10)
    free_consent IS A BOOLEAN TYPICALLY TRUE
        -- Presumption: Consent is free unless proven otherwise (s14)
```

### 3. Align with Legal Standards

```l4
-- GOOD: Reflects actual legal presumptions
DECLARE Taxpayer HAS
    is_resident IS A BOOLEAN TYPICALLY TRUE
    -- Most taxpayers in system are residents

-- AVOID: Arbitrary business logic
DECLARE Customer HAS
    prefers_email IS A BOOLEAN TYPICALLY TRUE
    -- This is a business preference, not a presumption
    -- Better to ask explicitly or track user preference
```

### 4. Use for Reducing Friction

```l4
-- GOOD: Reasonable defaults for optional enrichment
DECLARE PolicyApplication HAS
    dependents IS A NUMBER TYPICALLY 0
    has_pre_existing_conditions IS A BOOLEAN TYPICALLY FALSE

-- AVOID: Critical information shouldn't have defaults
DECLARE MedicalClaim HAS
    diagnosis_code IS A STRING TYPICALLY "UNKNOWN"
    -- BAD: Medical claims MUST have a diagnosis
    -- Don't hide missing critical data with defaults
```

## Part 11: Testing with TYPICALLY

### Test Both Explicit and Default Cases

```l4
§ `Test Cases`

-- Test with explicit values
#EVAL `qualifies for loan` (LoanApplication WITH
    applicantAge IS 30
    annualIncome IS 80000
    requestedAmount IS 150000
    hasCollateral IS TRUE
    hasCoSigner IS FALSE
    isCitizen IS TRUE
    hasGoodCredit IS TRUE)

-- Test with defaults (minimal input)
#EVAL `qualifies for loan` (LoanApplication WITH
    applicantAge IS 30
    annualIncome IS 80000
    requestedAmount IS 150000
    -- All other fields use TYPICALLY defaults
)

-- Test overriding specific defaults
#EVAL `qualifies for loan` (LoanApplication WITH
    applicantAge IS 30
    annualIncome IS 80000
    requestedAmount IS 150000
    hasGoodCredit IS FALSE  -- Override default
    hasCollateral IS TRUE   -- Override default
    -- Other fields use TYPICALLY defaults
)
```

### Assert Behavior Under Defaults

```l4
-- Verify that typical case passes
#ASSERT `qualifies for loan` (LoanApplication WITH
    applicantAge IS 35
    annualIncome IS 100000
    requestedAmount IS 200000)
    -- Implicitly uses TYPICALLY defaults, should qualify

-- Verify edge case behavior
#ASSERT NOT `qualifies for loan` (LoanApplication WITH
    applicantAge IS 20
    annualIncome IS 30000
    requestedAmount IS 200000
    hasGoodCredit IS FALSE
    hasCollateral IS FALSE
    hasCoSigner IS FALSE)
```

## Key Takeaways

1. **TYPICALLY encodes rebuttable presumptions** from legal reasoning
2. **Reduces question burden** in interactive applications
3. **Makes assumptions explicit** for auditing and verification
4. **Must be literals**, not expressions
5. **Must match declared type**
6. **Applies to DECLARE fields, GIVEN parameters, and ASSUME declarations**
7. **Innermost declaration wins** when multiple TYPICALLY apply
8. **Explicit values always override** TYPICALLY defaults
9. **Use for common cases**, not critical required information
10. **Test both with and without** explicit values

## Exercises

### Exercise 1: Basic TYPICALLY
Add TYPICALLY defaults to a Contract type with standard presumptions (arm's length, written, etc.).

### Exercise 2: Smart Questionnaire
Design a multi-step eligibility checker that uses TYPICALLY to minimize questions for typical applicants.

### Exercise 3: Assumption Audit
Create a function that reports all TYPICALLY defaults used in an evaluation (for compliance logging).

### Exercise 4: Override Patterns
Write tests demonstrating the precedence rules when DECLARE, GIVEN, and explicit values all provide values for the same field.

## Future Developments

Planned enhancements to TYPICALLY:

- **Runtime state tracking**: Distinguish "not asked", "I don't know", and "uses default"
- **Provenance tracking**: Record whether each value came from user input or TYPICALLY default
- **Interactive override**: Allow users to explicitly accept or reject defaults
- **Audit trails**: Log which defaults were applied in each evaluation
- **Complex defaults**: Support for computed defaults (not just literals)

## Next Steps

In **Module A6**, we'll explore JSON integration patterns for bridging L4 with external systems and databases.
