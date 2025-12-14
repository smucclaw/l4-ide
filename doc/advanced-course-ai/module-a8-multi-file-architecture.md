# Module A8 — Multi-File Pipelines & System Architecture

## Overview

Real-world legal systems are complex. A comprehensive immigration system might involve:

- Eligibility rules across multiple visa categories
- Quota management and allocation
- Risk assessment frameworks
- Fee calculation logic
- Supporting documentation requirements
- Appeal processes

Trying to maintain all of this in a single file quickly becomes unwieldy. This module covers:

- Structuring large L4 projects across multiple files
- Import and module dependency management
- Orchestrating multi-stage workflows
- Team collaboration patterns
- System architecture for enterprise deployments

## Part 1: When to Split into Multiple Files

### Single-File Pain Points

```l4
§ `WorkPass Authority - Monolithic Implementation`

-- 500 lines of type declarations
DECLARE EmploymentCategory IS ONE OF ...
DECLARE Employee HAS ...
DECLARE Company HAS ...
-- ... 30 more type declarations

-- 800 lines of validation rules
`meets age requirement` MEANS ...
`meets education requirement` MEANS ...
-- ... 50 more validation functions

-- 600 lines of eligibility logic
`qualifies for tech professional pass` MEANS ...
`qualifies for healthcare worker pass` MEANS ...
-- ... 20 more eligibility functions

-- 400 lines of quota management
`calculate quota usage` MEANS ...
-- ... more quota logic

-- 300 lines of fee calculations
`calculate application fee` MEANS ...
-- ... more fee logic

-- 200 lines of test data
`Test Employee 1` MEANS ...
-- ... 40 more test cases
```

**Problems:**

- Hard to navigate (2,800 lines!)
- Merge conflicts when multiple team members edit
- Slow to compile/check
- Difficult to reuse components
- No clear separation of concerns

### Multi-File Benefits

```
wpa-system/
├── core/
│   ├── types.l4              # 150 lines - Type definitions
│   ├── constants.l4          # 50 lines - System constants
│   └── prelude-extensions.l4 # 100 lines - Utility helpers
├── validation/
│   ├── age-validation.l4     # 80 lines
│   ├── education-validation.l4 # 120 lines
│   ├── salary-validation.l4  # 100 lines
│   └── risk-validation.l4    # 150 lines
├── eligibility/
│   ├── tech-professional.l4  # 200 lines
│   ├── healthcare-worker.l4  # 180 lines
│   └── researcher.l4         # 160 lines
├── operations/
│   ├── quota-management.l4   # 250 lines
│   ├── fee-calculation.l4    # 150 lines
│   └── application-pipeline.l4 # 200 lines
├── integration/
│   ├── api-exports.l4        # 100 lines
│   └── json-schemas.l4       # 80 lines
└── tests/
    ├── fixtures.l4           # 200 lines
    ├── unit-tests.l4         # 300 lines
    └── integration-tests.l4  # 250 lines
```

**Benefits:**

- Each file has a clear purpose
- Team members can work independently
- Faster compilation (only changed files)
- Easy to reuse components
- Better organization and navigation

## Part 2: Import System

### Basic Imports

```l4
§ `Tech Professional Eligibility`

-- Import core type definitions
IMPORT core/types
IMPORT core/constants

-- Import validation modules
IMPORT validation/age-validation
IMPORT validation/education-validation
IMPORT validation/salary-validation

-- Now we can use types and functions from imported modules
GIVEN employee IS AN Employee
      company IS A Company
GIVETH A CheckResult
`qualifies for tech professional pass` employee company MEANS
    allPass LIST
        `meets age requirement` employee,
        `meets education requirement for` TechProfessional employee,
        `meets salary requirement for` TechProfessional employee,
        `company meets requirements` company
```

### Import Paths

```l4
-- Relative import (from current directory)
IMPORT ./types
IMPORT ../core/constants

-- Absolute import (from project root)
IMPORT core/types
IMPORT validation/age-validation

-- Import from standard library
IMPORT prelude
IMPORT daydate
```

### Selective Imports

```l4
-- Import specific functions/types (future feature)
IMPORT core/types (Employee, Company, EmploymentCategory)
IMPORT validation/age-validation (`meets age requirement`)

-- Import with aliasing (future feature)
IMPORT validation/age-validation AS AgeVal
-- Use as: AgeVal.`meets age requirement`
```

## Part 3: Module Design Patterns

### Pattern 1: Type-First Architecture

**core/types.l4** — Central type definitions

```l4
§ `WPA Core Types`

DECLARE EmploymentCategory IS ONE OF
    TechProfessional
    HealthcareWorker
    Researcher
    EducationProfessional
    ArtsProfessional

DECLARE EducationLevel IS ONE OF
    HighSchool
    Bachelor
    Master
    Doctorate

DECLARE Employee HAS
    name IS A STRING
    age IS A NUMBER
    category IS AN EmploymentCategory
    education IS AN EducationLevel
    monthlySalary IS A NUMBER
    yearsOfExperience IS A NUMBER

DECLARE Company HAS
    name IS A STRING
    registrationNumber IS A STRING
    isRegisteredLocally IS A BOOLEAN
    numberOfEmployees IS A NUMBER
    industry IS A STRING

DECLARE CheckResult HAS
    passed IS A BOOLEAN
    reason IS A STRING
```

**Why this works:**

- All modules depend on types, so put them in one place
- Single source of truth for data structures
- Easy to evolve types (change propagates automatically)

### Pattern 2: Validation Layer

Each validator is independent and focused:

**validation/age-validation.l4**

```l4
§ `Age Validation Rules`

IMPORT core/types

`minimum age` MEANS 18

GIVEN employee IS AN Employee
GIVETH A BOOLEAN
`meets age requirement` employee MEANS
    `age of` employee AT LEAST `minimum age`

GIVEN employee IS AN Employee
GIVETH A CheckResult
`check age` employee MEANS
    IF `meets age requirement` employee
    THEN CheckResult WITH
        passed IS TRUE
        reason IS "Age requirement met"
    ELSE CheckResult WITH
        passed IS FALSE
        reason IS "Must be at least 18 years old"
```

**validation/education-validation.l4**

```l4
§ `Education Validation Rules`

IMPORT core/types

GIVEN category IS AN EmploymentCategory
GIVETH AN EducationLevel
`minimum education for` category MEANS
    CONSIDER category
    WHEN TechProfessional THEN Bachelor
    WHEN HealthcareWorker THEN Bachelor
    WHEN Researcher THEN Master
    WHEN EducationProfessional THEN Bachelor
    WHEN ArtsProfessional THEN HighSchool

GIVEN employee IS AN Employee
      category IS AN EmploymentCategory
GIVETH A BOOLEAN
`meets education requirement for` category employee MEANS
    `education level of` employee AT LEAST `minimum education for` category

GIVEN employee IS AN Employee
      category IS AN EmploymentCategory
GIVETH A CheckResult
`check education for` category employee MEANS
    IF `meets education requirement for` category employee
    THEN CheckResult WITH
        passed IS TRUE
        reason IS "Education requirement met"
    ELSE CheckResult WITH
        passed IS FALSE
        reason IS "Insufficient education level for this category"
```

**Benefits:**

- Each validator is self-contained
- Easy to test independently
- Can be developed by different team members
- Clear responsibility boundaries

### Pattern 3: Orchestration Layer

**operations/application-pipeline.l4**

```l4
§ `Application Processing Pipeline`

IMPORT core/types
IMPORT validation/age-validation
IMPORT validation/education-validation
IMPORT validation/salary-validation
IMPORT validation/risk-validation
IMPORT operations/quota-management
IMPORT operations/fee-calculation

DECLARE ApplicationResult HAS
    approved IS A BOOLEAN
    failureReasons IS A LIST OF STRING
    feeAmount IS A NUMBER
    quotaConsumed IS A BOOLEAN

GIVEN employee IS AN Employee
      company IS A Company
      category IS AN EmploymentCategory
GIVETH AN ApplicationResult
`process application` employee company category MEANS
    LET
        ageCheck IS `check age` employee
        educationCheck IS `check education for` category employee
        salaryCheck IS `check salary for` category employee
        riskCheck IS `check risk profile` employee company

        allChecks IS LIST ageCheck, educationCheck, salaryCheck, riskCheck

        passed IS all isPassedCheck allChecks
            WHERE isPassedCheck result MEANS result's passed

        failureReasons IS map getReason (filter isFailedCheck allChecks)
            WHERE
                isFailedCheck result MEANS NOT result's passed
                getReason result MEANS result's reason

        feeAmount IS
            IF passed
            THEN `calculate application fee for` category employee
            ELSE 0

        quotaConsumed IS
            IF passed
            THEN `consume quota for` category
            ELSE FALSE
    IN
        ApplicationResult WITH
            approved IS passed
            failureReasons IS failureReasons
            feeAmount IS feeAmount
            quotaConsumed IS quotaConsumed
```

**Why this pattern works:**

- Orchestration logic is separate from business rules
- Pipeline is easy to visualize and understand
- Adding new checks doesn't require changing existing validators
- Can swap out implementation of any validator

### Pattern 4: API Export Layer

**integration/api-exports.l4**

```l4
§ `API Exports for Decision Service`

IMPORT core/types
IMPORT operations/application-pipeline
IMPORT operations/fee-calculation

@desc export Check if employee qualifies for work pass
GIVEN employee IS AN Employee @desc Employee details
      company IS A Company @desc Employer details
      category IS AN EmploymentCategory @desc Desired employment category
GIVETH AN ApplicationResult
checkEligibility employee company category MEANS
    `process application` employee company category

@desc export Calculate application fee
GIVEN category IS AN EmploymentCategory @desc Employment category
      employee IS AN Employee @desc Employee details
GIVETH A NUMBER
calculateFee category employee MEANS
    `calculate application fee for` category employee

@desc export Get minimum requirements for category
GIVEN category IS AN EmploymentCategory @desc Employment category
GIVETH A STRING
getRequirements category MEANS
    "Minimum age: 18\n" APPEND
    "Minimum education: " APPEND showEducation (`minimum education for` category) APPEND "\n" APPEND
    "Minimum salary: " APPEND showNumber (`minimum salary for` category)
    WHERE
        showEducation level MEANS
            CONSIDER level
            WHEN Bachelor THEN "Bachelor's degree"
            WHEN Master THEN "Master's degree"
            OTHERWISE "See documentation"
        showNumber n MEANS
            -- Convert number to string (simplified)
            "See documentation"
```

## Part 4: Dependency Management

### Dependency Graph

```
                    core/types.l4
                          |
         +----------------+----------------+
         |                |                |
  core/constants.l4  validation/*    eligibility/*
                          |                |
                          +-------+--------+
                                  |
                         operations/application-pipeline.l4
                                  |
                         integration/api-exports.l4
```

### Avoiding Circular Dependencies

**Bad: Circular dependency**

```
module-a.l4  →  imports  →  module-b.l4
     ↑                           ↓
     └───────  imports  ←────────┘
```

**Good: Layered architecture**

```
Layer 1: core/types.l4
         ↓
Layer 2: validation/*.l4, eligibility/*.l4
         ↓
Layer 3: operations/application-pipeline.l4
         ↓
Layer 4: integration/api-exports.l4
```

### Shared Utilities

**core/prelude-extensions.l4**

```l4
§ `WPA Utility Functions`

IMPORT prelude

-- Helper for CheckResult lists
GIVEN results IS A LIST OF CheckResult
GIVETH A BOOLEAN
allPass results MEANS
    all isPassed results
    WHERE isPassed result MEANS result's passed

-- Helper to collect failure reasons
GIVEN results IS A LIST OF CheckResult
GIVETH A LIST OF STRING
collectFailures results MEANS
    map getReason (filter isFailed results)
    WHERE
        isFailed result MEANS NOT result's passed
        getReason result MEANS result's reason

-- Safe division with default
GIVEN numerator IS A NUMBER
      denominator IS A NUMBER
      defaultValue IS A NUMBER
GIVETH A NUMBER
`divide` numerator `by` denominator `or` defaultValue MEANS
    IF denominator EQUALS 0
    THEN defaultValue
    ELSE numerator DIVIDED BY denominator
```

## Part 5: Team Collaboration Patterns

### Feature Branch Workflow

```bash
# Developer 1: Working on education validation
git checkout -b feature/education-validation-updates
vim validation/education-validation.l4
# Make changes, test, commit
git push origin feature/education-validation-updates

# Developer 2: Working on salary validation (in parallel)
git checkout -b feature/salary-validation-updates
vim validation/salary-validation.l4
# Make changes, test, commit
git push origin feature/salary-validation-updates

# Both can work independently!
# No merge conflicts because they're in different files
```

### Code Ownership

```
# CODEOWNERS file
core/types.l4                          @team-lead @senior-dev
core/constants.l4                      @team-lead
validation/*.l4                        @validation-team
eligibility/*.l4                       @eligibility-team
operations/application-pipeline.l4     @team-lead @senior-dev
integration/api-exports.l4             @api-team
tests/*.l4                             @qa-team
```

### Review Checklist

When reviewing multi-file changes:

1. **Type Changes** (core/types.l4):
   - Do all dependent modules still compile?
   - Are there breaking changes to API exports?
   - Do existing tests still pass?

2. **Validation Changes** (validation/\*.l4):
   - Are new validation rules properly tested?
   - Do they integrate correctly with the pipeline?
   - Are error messages clear and actionable?

3. **Pipeline Changes** (operations/\*.l4):
   - Does the orchestration logic make sense?
   - Are all validation steps included?
   - Is error handling comprehensive?

4. **API Changes** (integration/\*.l4):
   - Are @desc annotations clear and complete?
   - Are parameter types correctly mapped to JSON?
   - Is backward compatibility maintained?

## Part 6: Testing Strategy

### Test Organization

**tests/fixtures.l4**

```l4
§ `Test Fixtures`

IMPORT core/types

-- Reusable test employees
`Alice - Strong Candidate` MEANS Employee WITH
    name IS "Alice"
    age IS 28
    category IS TechProfessional
    education IS Bachelor
    monthlySalary IS 5500
    yearsOfExperience IS 5

`Bob - Underage` MEANS Employee WITH
    name IS "Bob"
    age IS 17
    category IS TechProfessional
    education IS Bachelor
    monthlySalary IS 5000
    yearsOfExperience IS 1

`Carol - Insufficient Education` MEANS Employee WITH
    name IS "Carol"
    age IS 25
    category IS Researcher
    education IS Bachelor  -- Needs Master
    monthlySalary IS 5500
    yearsOfExperience IS 3

-- Reusable test companies
`TechCorp - Valid Company` MEANS Company WITH
    name IS "TechCorp"
    registrationNumber IS "TC12345"
    isRegisteredLocally IS TRUE
    numberOfEmployees IS 150
    industry IS "Software Development"
```

**tests/unit-tests.l4**

```l4
§ `Unit Tests - Validation Modules`

IMPORT core/types
IMPORT validation/age-validation
IMPORT validation/education-validation
IMPORT validation/salary-validation
IMPORT tests/fixtures

-- Age validation tests
#ASSERT `meets age requirement` `Alice - Strong Candidate`
#ASSERT NOT `meets age requirement` `Bob - Underage`

-- Education validation tests
#ASSERT `meets education requirement for` TechProfessional `Alice - Strong Candidate`
#ASSERT NOT `meets education requirement for` Researcher `Carol - Insufficient Education`

-- Salary validation tests
#ASSERT `meets salary requirement for` TechProfessional `Alice - Strong Candidate`
```

**tests/integration-tests.l4**

```l4
§ `Integration Tests - Full Pipeline`

IMPORT core/types
IMPORT operations/application-pipeline
IMPORT tests/fixtures

-- Happy path
#ASSERT (`process application`
            `Alice - Strong Candidate`
            `TechCorp - Valid Company`
            TechProfessional)'s approved

-- Underage rejection
#ASSERT NOT (`process application`
                `Bob - Underage`
                `TechCorp - Valid Company`
                TechProfessional)'s approved

-- Education mismatch
#ASSERT NOT (`process application`
                `Carol - Insufficient Education`
                `TechCorp - Valid Company`
                Researcher)'s approved
```

### Test Execution

```bash
# Run all tests
cabal run jl4-cli -- tests/unit-tests.l4
cabal run jl4-cli -- tests/integration-tests.l4

# Run specific test module
cabal run jl4-cli -- tests/validation-tests.l4

# Run with verbose output
cabal run jl4-cli -- --verbose tests/unit-tests.l4
```

## Part 7: System Architecture Patterns

### Pattern 1: Microservices with Shared Core

```
┌─────────────────────────────────────┐
│         Core Types Library          │
│         (core/types.l4)             │
└─────────────────────────────────────┘
            ↓        ↓        ↓
  ┌─────────────┬─────────────┬─────────────┐
  │  Eligibility│   Quota     │    Fee      │
  │   Service   │  Service    │  Service    │
  │             │             │             │
  │  eligibility│ operations/ │ operations/ │
  │    /*.l4    │  quota-     │  fee-       │
  │             │  mgmt.l4    │  calc.l4    │
  └─────────────┴─────────────┴─────────────┘
            ↓        ↓        ↓
  ┌───────────────────────────────────┐
  │    Orchestration Service          │
  │    (application-pipeline.l4)      │
  └───────────────────────────────────┘
```

Each service can be deployed independently, but shares core types.

### Pattern 2: Rules Engine with Plugin Architecture

```
┌─────────────────────────────────────┐
│         Core Engine                 │
│    (application-pipeline.l4)        │
└─────────────────────────────────────┘
            ↓
  ┌─────────────────────────────┐
  │    Validation Plugins        │
  ├─────────────────────────────┤
  │  • age-validation.l4        │
  │  • education-validation.l4  │
  │  • salary-validation.l4     │
  │  • risk-validation.l4       │
  │  • custom-validation-1.l4   │ ← Easy to add!
  │  • custom-validation-2.l4   │ ← Easy to add!
  └─────────────────────────────┘
```

New validations can be added without modifying the core engine.

### Pattern 3: Multi-Tenant System

```
shared/
├── core/types.l4           # Shared types
└── core/prelude-ext.l4     # Shared utilities

tenants/
├── singapore/
│   ├── validation/*.l4     # Singapore-specific rules
│   ├── eligibility/*.l4
│   └── config.l4
├── malaysia/
│   ├── validation/*.l4     # Malaysia-specific rules
│   ├── eligibility/*.l4
│   └── config.l4
└── thailand/
    ├── validation/*.l4     # Thailand-specific rules
    ├── eligibility/*.l4
    └── config.l4
```

Each tenant has customized rules but shares common infrastructure.

## Part 8: Performance Considerations

### Lazy Evaluation Benefits

L4 uses lazy evaluation, which means:

```l4
GIVEN employee IS AN Employee
GIVETH AN ApplicationResult
`process with all checks` employee MEANS
    LET
        check1 IS `expensive check 1` employee  -- Not computed yet
        check2 IS `expensive check 2` employee  -- Not computed yet
        check3 IS `expensive check 3` employee  -- Not computed yet

        -- If check1 fails, check2 and check3 are never computed!
        result IS
            IF check1's passed AND check2's passed AND check3's passed
            THEN approved
            ELSE rejected
    IN result
```

### Module Compilation Caching

When using multiple files:

- Only changed files are recompiled
- Unchanged dependencies use cached results
- Speeds up development iteration

### Import Optimization

```l4
-- Good: Import only what you need
IMPORT core/types
IMPORT validation/age-validation

-- Less optimal: Import entire module family
IMPORT validation/*  -- Imports all validation modules even if unused
```

## Part 9: Documentation Strategy

### Module-Level Documentation

**validation/age-validation.l4**

```l4
§ `Age Validation Rules`

{-
Module: validation/age-validation
Purpose: Validate applicant age requirements
Dependencies: core/types
Exports:
  - `minimum age` : NUMBER
  - `meets age requirement` : Employee → BOOLEAN
  - `check age` : Employee → CheckResult

Business Rules:
  - All applicants must be at least 18 years old
  - Age is calculated based on birthdate, not current age field

Last Updated: 2024-12-01
Owner: @validation-team
-}

IMPORT core/types

-- Implementation follows...
```

### Architecture Decision Records

**docs/adr/001-split-validation-modules.md**

```markdown
# ADR 001: Split Validation into Separate Modules

## Status

Accepted

## Context

Our WPA eligibility system has grown to 2,000+ lines in a single file.
Multiple team members need to work on validation logic simultaneously.

## Decision

Split validation logic into separate modules:

- validation/age-validation.l4
- validation/education-validation.l4
- validation/salary-validation.l4
- validation/risk-validation.l4

## Consequences

Positive:

- Reduced merge conflicts
- Clearer code organization
- Easier to test individual validators
- Faster compilation

Negative:

- More files to manage
- Need to understand import system
- Slightly more boilerplate

## Implementation

- Completed: 2024-11-15
- Migration: All tests passing
```

## Part 10: Deployment Strategies

### Monolithic Deployment

```bash
# Build entire system
cabal build all

# Deploy all modules together
./deploy.sh production --all-modules

# All changes go live simultaneously
```

**Use when:**

- Small team
- Tightly coupled rules
- Infrequent updates
- Simple deployment pipeline

### Modular Deployment

```bash
# Build only changed modules
cabal build jl4-core
cabal build jl4-decision-service

# Deploy specific modules
./deploy.sh production --module validation/age-validation

# Only age validation rules update
# Other modules remain unchanged
```

**Use when:**

- Large team
- Independent rule domains
- Frequent updates
- Need for gradual rollout

### Blue-Green Deployment

```
Blue Environment (current):
  - validation/*.l4 (version 1.0)
  - eligibility/*.l4 (version 1.0)

Green Environment (new):
  - validation/*.l4 (version 1.1) ← Updated
  - eligibility/*.l4 (version 1.0) ← Unchanged

Test green environment
↓
Switch traffic to green
↓
Blue becomes new green for next deployment
```

## Key Takeaways

1. **Split files by responsibility**: types, validation, eligibility, operations, integration
2. **Avoid circular dependencies**: Use layered architecture
3. **Share common utilities**: Create dedicated utility modules
4. **Use consistent patterns**: Type-first, validation layer, orchestration
5. **Enable parallel work**: Clear module boundaries reduce conflicts
6. **Test at every level**: Unit tests per module, integration tests across modules
7. **Document architecture**: ADRs, module docs, dependency graphs
8. **Choose deployment strategy**: Monolithic vs modular based on team size
9. **Leverage lazy evaluation**: Expensive checks only run when needed
10. **Cache compilation results**: Only rebuild changed modules

## Exercises

### Exercise 1: Split Your Monolith

Take your WPA pipeline from Module 5 and split it into:

- core/types.l4
- validation/age-validation.l4
- validation/education-validation.l4
- validation/salary-validation.l4
- operations/application-pipeline.l4

### Exercise 2: Add a New Validator

Create a new validation module for criminal background checks without modifying existing validators. Integrate it into the pipeline.

### Exercise 3: Team Simulation

Simulate a team environment:

- Person A: Update age validation rules
- Person B: Update education validation rules
- Both work in parallel on separate branches
- Merge changes - should have no conflicts!

### Exercise 4: Design a Multi-Tenant System

Design a file structure for a multi-country immigration system where each country has different rules but shares common infrastructure.

## Next Steps

Congratulations! You've completed the Advanced Course. You now have the skills to:

- Build production-grade L4 systems
- Use AI assistance for legal text ingestion
- Implement temporal logic and decision services
- Handle rebuttable presumptions
- Integrate with external JSON systems
- Maintain regression test suites
- Architect large multi-file projects

**Continue learning:**

- Contribute to the L4 project on GitHub
- Join the L4 community discussions
- Build your own legal domain applications
- Share your experiences and use cases
