# Module 6 — Test Data & #EVAL Execution

## Overview

In software development, automated testing catches bugs before production. In legal computation, testing verifies that rules behave as intended across scenarios—including edge cases that might otherwise become courtroom disputes.

This module covers:

- Creating reusable test data (fixtures)
- Using `#EVAL` to execute tests
- Using `#ASSERT` for validation
- Building comprehensive test suites
- Testing edge cases and boundary conditions

## Why Testing Matters in Legal Tech

Consider an insurance policy that "leaks" millions due to an ambiguous payout formula (from the CLAUDE.md backstory). Comprehensive testing would catch this before deployment.

Testing legal rules provides:

1. **Verification**: Rules work as intended
2. **Documentation**: Test cases serve as examples
3. **Regression prevention**: Changes don't break existing behavior
4. **Edge case discovery**: Find ambiguities and contradictions

## Test Data as Fixtures

### Defining Reusable Test Cases

Create named constants for common scenarios:

```l4
§§ `Test Fixture - Sample Employees`

`Alice - Tech Professional` MEANS Employee WITH
    name               IS "Alice Chen"
    passportNumber     IS "P1234567"
    nationality        IS "Canada"
    dateOfBirth        IS DATE OF 15, 3, 1990
    educationLevel     IS Master
    yearsExperience    IS 8
    monthlySalary      IS 8500
    category           IS TechProfessional
    hasDisqualifyingRecord IS FALSE

`Bob - Entry Level Tech` MEANS Employee WITH
    name               IS "Bob Smith"
    passportNumber     IS "P2345678"
    nationality        IS "United States"
    dateOfBirth        IS DATE OF 22, 7, 1998
    educationLevel     IS Bachelor
    yearsExperience    IS 1
    monthlySalary      IS 4800
    category           IS TechProfessional
    hasDisqualifyingRecord IS FALSE

`Charlie - Underage` MEANS Employee WITH
    name               IS "Charlie Lee"
    passportNumber     IS "P3456789"
    nationality        IS "Australia"
    dateOfBirth        IS DATE OF 10, 11, 2008
    educationLevel     IS HighSchool
    yearsExperience    IS 0
    monthlySalary      IS 3000
    category           IS Other
    hasDisqualifyingRecord IS FALSE

`Diana - Healthcare` MEANS Employee WITH
    name               IS "Diana Patel"
    passportNumber     IS "P4567890"
    nationality        IS "India"
    dateOfBirth        IS DATE OF 5, 2, 1985
    educationLevel     IS Diploma
    yearsExperience    IS 12
    monthlySalary      IS 5200
    category           IS HealthcareWorker
    hasDisqualifyingRecord IS FALSE

`Eve - Criminal Record` MEANS Employee WITH
    name               IS "Eve Johnson"
    passportNumber     IS "P5678901"
    nationality        IS "United Kingdom"
    dateOfBirth        IS DATE OF 18, 9, 1992
    educationLevel     IS Bachelor
    yearsExperience    IS 6
    monthlySalary      IS 6000
    category           IS FinancialServices
    hasDisqualifyingRecord IS TRUE
```

### Sample Companies

```l4
§§ `Test Fixture - Sample Companies`

`TechCorp - Established` MEANS Company WITH
    registrationNumber IS "TC12345"
    businessName       IS "TechCorp Innovations Pte Ltd"
    industry           IS "Technology"
    foundedYear        IS 2015
    localEmployeeCount IS 45
    foreignWorkerCount IS 8
    foreignWorkerQuota IS 15
    paidUpCapital      IS 2000000

`StartupCo - New Company` MEANS Company WITH
    registrationNumber IS "SC67890"
    businessName       IS "StartupCo Labs Pte Ltd"
    industry           IS "Technology"
    foundedYear        IS 2023
    localEmployeeCount IS 5
    foreignWorkerCount IS 1
    foreignWorkerQuota IS 3
    paidUpCapital      IS 100000

`HospitalGroup - Large` MEANS Company WITH
    registrationNumber IS "HG11111"
    businessName       IS "National Hospital Group"
    industry           IS "Healthcare"
    foundedYear        IS 2005
    localEmployeeCount IS 350
    foreignWorkerCount IS 42
    foreignWorkerQuota IS 50
    paidUpCapital      IS 50000000

`OverQuotaCorp` MEANS Company WITH
    registrationNumber IS "OQ22222"
    businessName       IS "OverQuota Corp"
    industry           IS "Manufacturing"
    foundedYear        IS 2018
    localEmployeeCount IS 20
    foreignWorkerCount IS 10
    foreignWorkerQuota IS 10
    paidUpCapital      IS 500000
```

### Sample Applications

```l4
§§ `Test Fixture - Sample Applications`

`Application 1 - Strong Candidate` MEANS WorkPassApplication WITH
    applicationId     IS "WPA-2025-001"
    employee          IS `Alice - Tech Professional`
    employer          IS `TechCorp - Established`
    proposedStartDate IS DATE OF 1, 4, 2025
    contractDuration  IS 24
    submissionDate    IS DATE OF 1, 2, 2025

`Application 2 - Borderline Salary` MEANS WorkPassApplication WITH
    applicationId     IS "WPA-2025-002"
    employee          IS `Bob - Entry Level Tech`
    employer          IS `StartupCo - New Company`
    proposedStartDate IS DATE OF 15, 4, 2025
    contractDuration  IS 12
    submissionDate    IS DATE OF 5, 2, 2025

`Application 3 - Underage` MEANS WorkPassApplication WITH
    applicationId     IS "WPA-2025-003"
    employee          IS `Charlie - Underage`
    employer          IS `TechCorp - Established`
    proposedStartDate IS DATE OF 1, 5, 2025
    contractDuration  IS 6
    submissionDate    IS DATE OF 10, 2, 2025

`Application 4 - Healthcare` MEANS WorkPassApplication WITH
    applicationId     IS "WPA-2025-004"
    employee          IS `Diana - Healthcare`
    employer          IS `HospitalGroup - Large`
    proposedStartDate IS DATE OF 1, 3, 2025
    contractDuration  IS 36
    submissionDate    IS DATE OF 1, 1, 2025

`Application 5 - Criminal Record` MEANS WorkPassApplication WITH
    applicationId     IS "WPA-2025-005"
    employee          IS `Eve - Criminal Record`
    employer          IS `TechCorp - Established`
    proposedStartDate IS DATE OF 1, 6, 2025
    contractDuration  IS 24
    submissionDate    IS DATE OF 15, 2, 2025

`Application 6 - Quota Exceeded` MEANS WorkPassApplication WITH
    applicationId     IS "WPA-2025-006"
    employee          IS `Alice - Tech Professional`
    employer          IS `OverQuotaCorp`
    proposedStartDate IS DATE OF 1, 4, 2025
    contractDuration  IS 24
    submissionDate    IS DATE OF 20, 2, 2025
```

## Using #EVAL for Testing

### Basic #EVAL Syntax

```l4
#EVAL expression
```

The expression is evaluated and the result is displayed.

### Simple Tests

```l4
§§ `Basic Function Tests`

-- Test age calculation
#EVAL `age of` `Alice - Tech Professional` `as of` DATE OF 1, 1, 2025

-- Test salary checks
#EVAL `salary meets requirement` `Alice - Tech Professional`
#EVAL `salary meets requirement` `Bob - Entry Level Tech`

-- Test education requirements
#EVAL `education meets requirement` `Alice - Tech Professional`
#EVAL `education meets requirement` `Bob - Entry Level Tech`
```

### Testing with Lists

```l4
§§ `List Processing Tests`

`all test employees` MEANS LIST
    `Alice - Tech Professional`,
    `Bob - Entry Level Tech`,
    `Charlie - Underage`,
    `Diana - Healthcare`,
    `Eve - Criminal Record`

-- Filter tech professionals
#EVAL filter isTech `all test employees`
    WHERE
        isTech emp MEANS emp's category EQUALS TechProfessional

-- Count eligible employees
#EVAL `length of` eligibleEmployees
    WHERE
        eligibleEmployees MEANS
            filter isEligible `all test employees`
        isEligible emp MEANS
            `check personal eligibility` emp THEN passed
```

### Testing Complete Pipeline

```l4
§§ `End-to-End Pipeline Tests`

-- Should approve
#EVAL `process application` `Application 1 - Strong Candidate`

-- Should approve with warnings
#EVAL `process application` `Application 2 - Borderline Salary`

-- Should reject - underage
#EVAL `process application` `Application 3 - Underage`

-- Should approve - healthcare
#EVAL `process application` `Application 4 - Healthcare`

-- Should reject - criminal record
#EVAL `process application` `Application 5 - Criminal Record`

-- Should reject - quota exceeded
#EVAL `process application` `Application 6 - Quota Exceeded`
```

## Using #ASSERT for Validation

`#ASSERT` verifies that a Boolean expression is TRUE. If it's FALSE, the test fails.

```l4
§§ `Assertion-Based Tests`

-- Positive tests (should be TRUE)
#ASSERT `meets minimum age` `Alice - Tech Professional`
#ASSERT `meets minimum age` `Diana - Healthcare`
#ASSERT `salary meets requirement` `Alice - Tech Professional`

-- Negative tests (should be TRUE after NOT)
#ASSERT NOT `meets minimum age` `Charlie - Underage`
#ASSERT NOT `salary meets requirement` `Bob - Entry Level Tech`

-- Complex assertions
#ASSERT (`age of` `Alice - Tech Professional` `as of` `today`) AT LEAST 18

#ASSERT
    (`check personal eligibility` `Alice - Tech Professional`)'s passed
    EQUALS TRUE

#ASSERT
    (`check quota availability` `OverQuotaCorp`)'s passed
    EQUALS FALSE
```

### Batch Assertions

```l4
§§ `Batch Assertion Tests`

-- All tech professionals should meet education requirements
#ASSERT all meetsEdu techEmployees
    WHERE
        techEmployees MEANS
            filter isTech `all test employees`
        isTech emp MEANS
            emp's category EQUALS TechProfessional
        meetsEdu emp MEANS
            `education meets requirement` emp

-- No underage employees should pass age check
#ASSERT NOT any isUnderage eligibleEmployees
    WHERE
        eligibleEmployees MEANS
            filter passesAge `all test employees`
        passesAge emp MEANS
            `meets minimum age` emp
        isUnderage emp MEANS
            `age of` emp `as of` `today` LESS THAN 18
```

## Building a Comprehensive Test Suite

### Test Organization

```l4
§ `WPA Test Suite`

§§ `Unit Tests - Age Requirements`

#EVAL `age of` `Alice - Tech Professional` `as of` DATE OF 1, 1, 2025
#ASSERT `age of` `Alice - Tech Professional` `as of` DATE OF 1, 1, 2025 EQUALS 34

#ASSERT `meets minimum age` `Alice - Tech Professional`
#ASSERT `meets maximum age` `Alice - Tech Professional`
#ASSERT NOT `meets minimum age` `Charlie - Underage`

§§ `Unit Tests - Salary Requirements`

#EVAL `minimum monthly salary for` TechProfessional
#ASSERT `minimum monthly salary for` TechProfessional EQUALS 5000

#ASSERT `salary meets requirement` `Alice - Tech Professional`
#ASSERT NOT `salary meets requirement` `Bob - Entry Level Tech`

§§ `Unit Tests - Education Requirements`

#ASSERT `education meets requirement` `Alice - Tech Professional`
#ASSERT `education meets requirement` `Diana - Healthcare`

§§ `Integration Tests - Personal Eligibility`

#EVAL `check personal eligibility` `Alice - Tech Professional`
#ASSERT (`check personal eligibility` `Alice - Tech Professional`)'s passed

#EVAL `check personal eligibility` `Charlie - Underage`
#ASSERT NOT (`check personal eligibility` `Charlie - Underage`)'s passed

§§ `Integration Tests - Company Quota`

#EVAL `check quota availability` `TechCorp - Established`
#ASSERT (`check quota availability` `TechCorp - Established`)'s passed

#EVAL `check quota availability` `OverQuotaCorp`
#ASSERT NOT (`check quota availability` `OverQuotaCorp`)'s passed

§§ `End-to-End Tests - Complete Pipeline`

#EVAL `process application` `Application 1 - Strong Candidate`
#ASSERT (`process application` `Application 1 - Strong Candidate`)'s approved

#EVAL `process application` `Application 3 - Underage`
#ASSERT NOT (`process application` `Application 3 - Underage`)'s approved

#EVAL `process application` `Application 6 - Quota Exceeded`
#ASSERT NOT (`process application` `Application 6 - Quota Exceeded`)'s approved
```

## Edge Case Testing

### Boundary Value Tests

```l4
§§ `Boundary Value Tests`

`Employee - Age 18` MEANS Employee WITH
    name               IS "Just Turned 18"
    passportNumber     IS "P9999991"
    nationality        IS "Singapore"
    dateOfBirth        IS DATE OF 1, 1, 2007
    educationLevel     IS Bachelor
    yearsExperience    IS 0
    monthlySalary      IS 5000
    category           IS TechProfessional
    hasDisqualifyingRecord IS FALSE

`Employee - Age 65` MEANS Employee WITH
    name               IS "Retirement Age"
    passportNumber     IS "P9999992"
    nationality        IS "Singapore"
    dateOfBirth        IS DATE OF 1, 1, 1960
    educationLevel     IS Master
    yearsExperience    IS 40
    monthlySalary      IS 10000
    category           IS TechProfessional
    hasDisqualifyingRecord IS FALSE

`Employee - Age 17` MEANS Employee WITH
    name               IS "One Year Too Young"
    passportNumber     IS "P9999993"
    nationality        IS "Singapore"
    dateOfBirth        IS DATE OF 2, 1, 2008
    educationLevel     IS Diploma
    yearsExperience    IS 0
    monthlySalary      IS 4000
    category           IS Other
    hasDisqualifyingRecord IS FALSE

`Employee - Age 66` MEANS Employee WITH
    name               IS "One Year Over"
    passportNumber     IS "P9999994"
    nationality        IS "Singapore"
    dateOfBirth        IS DATE OF 31, 12, 1958
    educationLevel     IS Bachelor
    yearsExperience    IS 45
    monthlySalary      IS 8000
    category           IS Other
    hasDisqualifyingRecord IS FALSE

-- Test boundaries
#ASSERT `meets minimum age` `Employee - Age 18`
#ASSERT `meets maximum age` `Employee - Age 65`
#ASSERT NOT `meets minimum age` `Employee - Age 17`
#ASSERT NOT `meets maximum age` `Employee - Age 66`
```

### Salary Boundary Tests

```l4
`Employee - Exact Minimum Salary` MEANS Employee WITH
    name               IS "Exact Minimum"
    passportNumber     IS "P9999995"
    nationality        IS "Canada"
    dateOfBirth        IS DATE OF 15, 6, 1990
    educationLevel     IS Bachelor
    yearsExperience    IS 3
    monthlySalary      IS 5000  -- Exactly minimum for TechProfessional
    category           IS TechProfessional
    hasDisqualifyingRecord IS FALSE

`Employee - One Dollar Below` MEANS Employee WITH
    name               IS "One Dollar Short"
    passportNumber     IS "P9999996"
    nationality        IS "Canada"
    dateOfBirth        IS DATE OF 15, 6, 1990
    educationLevel     IS Bachelor
    yearsExperience    IS 3
    monthlySalary      IS 4999  -- One dollar below minimum
    category           IS TechProfessional
    hasDisqualifyingRecord IS FALSE

#ASSERT `salary meets requirement` `Employee - Exact Minimum Salary`
#ASSERT NOT `salary meets requirement` `Employee - One Dollar Below`
```

### Quota Boundary Tests

```l4
`Company - At Quota Limit` MEANS Company WITH
    registrationNumber IS "CQ99999"
    businessName       IS "At Limit Corp"
    industry           IS "Technology"
    foundedYear        IS 2015
    localEmployeeCount IS 50
    foreignWorkerCount IS 19
    foreignWorkerQuota IS 20  -- One slot remaining
    paidUpCapital      IS 1000000

#EVAL `check quota availability` `Company - At Quota Limit`
#ASSERT (`check quota availability` `Company - At Quota Limit`)'s passed

-- After hiring one more
#EVAL `remaining quota for` `Company - At Quota Limit` `given`
    (LIST `Alice - Tech Professional`)
```

## Test Data Generation Patterns

### Parameterized Test Cases

```l4
§§ `Parameterized Test Helpers`

GIVEN name IS A STRING
      salary IS A NUMBER
GIVETH AN Employee
`tech employee with` `name` `earning` `salary` MEANS
    Employee WITH
        name               IS name
        passportNumber     IS "P0000000"
        nationality        IS "Test Country"
        dateOfBirth        IS DATE OF 1, 1, 1990
        educationLevel     IS Bachelor
        yearsExperience    IS 5
        monthlySalary      IS salary
        category           IS TechProfessional
        hasDisqualifyingRecord IS FALSE

-- Generate multiple test cases
#EVAL map checkSalary salaryTestCases
    WHERE
        salaryTestCases MEANS LIST
            `tech employee with` "Test1" `earning` 4000,
            `tech employee with` "Test2" `earning` 5000,
            `tech employee with` "Test3" `earning` 6000,
            `tech employee with` "Test4" `earning` 10000
        checkSalary emp MEANS
            `salary meets requirement` emp
```

## Interpretation and Debugging

When a test fails:

1. **Read the result**: L4 shows the full evaluation trace
2. **Check assumptions**: Verify test data is correct
3. **Isolate the failure**: Test smaller components
4. **Check edge cases**: Boundary conditions often reveal bugs

Example debugging workflow:

```l4
-- Test fails
#ASSERT (`process application` `Application 2 - Borderline Salary`)'s approved

-- Break down the pipeline
#EVAL `check personal eligibility` (`Application 2 - Borderline Salary`)'s employee
#EVAL `check salary requirements` (`Application 2 - Borderline Salary`)'s employee
#EVAL `check quota availability` (`Application 2 - Borderline Salary`)'s employer

-- Found it! Salary check fails
#EVAL `minimum monthly salary for` TechProfessional
-- Returns 5000

#EVAL (`Application 2 - Borderline Salary`)'s employee's monthlySalary
-- Returns 4800

-- Bob's salary (4800) < minimum (5000)
```

## Key Takeaways

1. **Create reusable fixtures**—name your test data
2. **Test at multiple levels**: unit, integration, end-to-end
3. **Use #EVAL for exploration**, #ASSERT for validation
4. **Test edge cases and boundaries** explicitly
5. **Batch tests with lists** for comprehensive coverage
6. **Document expected behavior** through test names
7. **Debug by breaking down** complex tests into smaller pieces

## Exercises

### Exercise 1: Create Test Fixtures

Define 3 employees and 2 companies covering different scenarios (approved, rejected for various reasons).

### Exercise 2: Write Unit Tests

Write #ASSERT tests for at least 5 helper functions from Module 5.

### Exercise 3: Boundary Testing

Create test cases for all boundary conditions in age requirements (17, 18, 65, 66).

### Exercise 4: Integration Testing

Write end-to-end tests for at least 6 different application scenarios.

### Exercise 5: Debugging

Intentionally create a failing test, then use #EVAL to diagnose the root cause.

## Congratulations!

You've completed the **L4 Foundation Course**. You can now:

- Define types with DECLARE
- Write functions with GIVEN/GIVETH/MEANS
- Use pattern matching and control flow
- Work with lists and MAYBE
- Build complex multi-stage pipelines
- Create comprehensive test suites

## Next Steps: Advanced Course

The Advanced Course covers:

- Multi-file project structure and imports
- AI-assisted ingestion of legislation
- Temporal logic for dates and time windows
- Regression testing and change control
- JSON integration for external systems
- System-level architecture for large rulebases

Ready to level up? Continue to **Module A1**.
