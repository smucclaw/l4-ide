# Module A7 — Regression Testing & Change Control

## Overview

Legal rules evolve: legislation is amended, case law develops, business policies change. When you update L4 code, you must ensure:

1. **Existing decisions remain correct** (unless intentionally changed)
2. **New features don't break old ones**
3. **Changes are traceable and auditable**
4. **Rollback is possible if needed**

This module covers:

- Building regression test suites
- Golden master testing
- Change impact analysis
- Version control strategies
- Continuous integration for L4

## Part 1: Why Regression Testing Matters for Legal Code

### The Problem

```l4
-- Original rule (2020)
GIVEN employee IS AN Employee
GIVETH A BOOLEAN
DECIDE `qualifies for leave` IF
    employee's yearsOfService AT LEAST 1
```

**Someone updates it:**

```l4
-- Updated rule (2023)
GIVEN employee IS AN Employee
GIVETH A BOOLEAN
DECIDE `qualifies for leave` IF
        employee's yearsOfService AT LEAST 2  -- Changed!
    AND employee's employmentType EQUALS "Permanent"  -- Added!
```

**Impact:**

- 1,000+ historical decisions need review
- Employees with 1 year of service suddenly ineligible
- Potential legal liability

**With regression tests:**

```bash
Running regression tests...
FAILED: test_1year_employee
  Expected: TRUE
  Got: FALSE
  Test case: Employee with 1 year of service

Change Impact: 127 existing cases affected
```

## Part 2: Building a Test Suite

### Test File Organization

```
tests/
├── unit/
│   ├── eligibility-rules.l4
│   ├── salary-calculations.l4
│   └── date-arithmetic.l4
├── integration/
│   ├── application-pipeline.l4
│   └── quota-management.l4
├── regression/
│   ├── 2020-baseline.l4
│   ├── 2023-amendment.l4
│   └── edge-cases.l4
├── fixtures/
│   ├── employees.l4
│   ├── companies.l4
│   └── applications.l4
└── golden/
    ├── baseline-outputs/
    └── current-outputs/
```

### Unit Test Example

```l4
§ `Unit Tests - Salary Validation`

IMPORT ../src/rules/salary-rules
IMPORT ../fixtures/employees

-- Test minimum salary for tech professional
#ASSERT `meets salary requirement`
    (`Alice - Tech Professional` WITH monthlySalary IS 5000)

-- Test below minimum
#ASSERT NOT `meets salary requirement`
    (`Bob - Entry Level` WITH monthlySalary IS 4999)

-- Test edge case: exactly at minimum
#ASSERT `meets salary requirement`
    (`Charlie` WITH
        category IS TechProfessional,
        monthlySalary IS 5000)

-- Test for each category
#ASSERT `minimum salary for` TechProfessional EQUALS 5000
#ASSERT `minimum salary for` HealthcareWorker EQUALS 4500
#ASSERT `minimum salary for` Researcher EQUALS 5500
```

### Integration Test Example

```l4
§ `Integration Tests - Complete Pipeline`

IMPORT ../src/main
IMPORT ../fixtures/applications

-- Happy path: All requirements met
#ASSERT (`process application` `Application 1 - Strong Candidate`)'s approved

-- Failure case: Underage
#ASSERT NOT (`process application` `Application 3 - Underage`)'s approved

-- Boundary case: Exactly minimum age
#ASSERT (`process application` `Application - Age 18`)'s approved

-- Complex case: Multiple conditions
#EVAL `process application` `Application - Complex Case`
```

### Regression Test Example

```l4
§ `Regression Tests - 2020 Baseline`

{-
These tests capture the behavior of the system as of 2020-12-31.
Any changes to these results indicate a breaking change that requires review.
-}

IMPORT ../src/eligibility
IMPORT ../fixtures/historical-cases

-- Historical case #12345 (2020-03-15)
#ASSERT `qualifies for leave`
    (Employee WITH
        name IS "John Doe",
        yearsOfService IS 1,
        employmentType IS "Contract")
    -- Result: TRUE in 2020 rules

-- Historical case #12346 (2020-06-20)
#ASSERT NOT `qualifies for leave`
    (Employee WITH
        name IS "Jane Smith",
        yearsOfService IS 0.5,
        employmentType IS "Permanent")
    -- Result: FALSE in 2020 rules (insufficient years)

-- Historical case #12347 (2020-09-10)
#ASSERT `qualifies for leave`
    (Employee WITH
        name IS "Bob Chen",
        yearsOfService IS 3,
        employmentType IS "Temporary")
    -- Result: TRUE in 2020 rules (no employment type check)
```

## Part 3: Golden Master Testing

### What is Golden Master Testing?

**Golden Master Testing** captures the output of your system at a known-good state, then compares future outputs against it.

### Creating Golden Files

```bash
# Run all tests and capture outputs
cabal run jl4-cli -- tests/all-tests.l4 > tests/golden/2020-baseline.txt

# Store as "golden master"
git add tests/golden/2020-baseline.txt
git commit -m "Golden master: 2020 baseline outputs"
```

### Golden File Format

```
=== Test: Employee #12345 ===
Input:
  Employee {
    name: "John Doe",
    yearsOfService: 1,
    employmentType: "Contract"
  }
Output:
  qualifies_for_leave: TRUE
Trace:
  - Checking yearsOfService: 1 >= 1 ✓
  - Result: TRUE

=== Test: Employee #12346 ===
Input:
  Employee {
    name: "Jane Smith",
    yearsOfService: 0.5,
    employmentType: "Permanent"
  }
Output:
  qualifies_for_leave: FALSE
Trace:
  - Checking yearsOfService: 0.5 >= 1 ✗
  - Result: FALSE
```

### Comparing Against Golden

```bash
# Run tests and compare
cabal run jl4-cli -- tests/all-tests.l4 > tests/golden/current.txt
diff tests/golden/2020-baseline.txt tests/golden/current.txt
```

**Output if changed:**

```diff
=== Test: Employee #12345 ===
Input:
  Employee {
    name: "John Doe",
    yearsOfService: 1,
    employmentType: "Contract"
  }
Output:
-  qualifies_for_leave: TRUE
+  qualifies_for_leave: FALSE
Trace:
-  - Checking yearsOfService: 1 >= 1 ✓
+  - Checking yearsOfService: 1 >= 2 ✗
+  - Checking employmentType: "Contract" == "Permanent" ✗
-  - Result: TRUE
+  - Result: FALSE
```

### Updating Golden Files

When a change is **intentional and verified**:

```bash
# Update golden master
cp tests/golden/current.txt tests/golden/2023-amendment.txt
git add tests/golden/2023-amendment.txt
git commit -m "Update golden master for 2023 amendment

Breaking changes:
- Minimum years of service increased from 1 to 2
- Added employment type requirement (Permanent only)

Impact: 127 historical cases now return different results
Reviewed by: Legal team
Approved: 2023-06-15"
```

## Part 4: Change Impact Analysis

### Automated Impact Reports

Create a test that reports which cases changed:

```l4
§ `Change Impact Analysis`

IMPORT prelude
IMPORT ../fixtures/historical-cases

DECLARE ChangeImpact HAS
    caseId IS A STRING
    oldResult IS A BOOLEAN
    newResult IS A BOOLEAN
    changed IS A BOOLEAN

GIVEN cases IS A LIST OF HistoricalCase
GIVETH A LIST OF ChangeImpact
analyzeImpact cases MEANS
    map checkCase cases
    WHERE
        checkCase case MEANS
            ChangeImpact WITH
                caseId IS case's id
                oldResult IS case's expectedResult
                newResult IS `qualifies for leave` case's employee
                changed IS (case's expectedResult NOT EQUALS newResult)

-- Run on all historical cases
`all impacts` MEANS analyzeImpact `all historical cases`

-- Count changed cases
`changed cases count` MEANS
    `length of` (filter isChanged `all impacts`)
    WHERE
        isChanged impact MEANS impact's changed

#EVAL `changed cases count`
#EVAL filter isChanged `all impacts`
    WHERE isChanged i MEANS i's changed
```

**Output:**

```
changed cases count: 127

Changed cases:
- Case #12345: TRUE → FALSE
- Case #12347: TRUE → FALSE
- Case #12389: TRUE → FALSE
...
```

### Impact Report Generation

```bash
#!/bin/bash
# scripts/impact-report.sh

echo "L4 Change Impact Report"
echo "======================="
echo ""

echo "Running baseline tests..."
git checkout baseline-2020
cabal run jl4-cli -- tests/regression/all-cases.l4 > /tmp/baseline.txt

echo "Running current tests..."
git checkout main
cabal run jl4-cli -- tests/regression/all-cases.l4 > /tmp/current.txt

echo "Comparing results..."
python3 scripts/compare-results.py /tmp/baseline.txt /tmp/current.txt

echo ""
echo "Summary:"
echo "  Total cases: $(grep -c "^=== Test:" /tmp/baseline.txt)"
echo "  Changed:     $(diff /tmp/baseline.txt /tmp/current.txt | grep -c "^<.*Output:")"
echo "  Unchanged:   $(comm -12 /tmp/baseline.txt /tmp/current.txt | grep -c "^=== Test:")"
```

## Part 5: Version Control Strategies

### Git Workflow for L4

```bash
# Feature branch for rule changes
git checkout -b feature/2023-leave-amendment

# Make changes to L4 files
vim src/rules/leave-eligibility.l4

# Run tests
./scripts/run-all-tests.sh

# If tests pass
git add src/rules/leave-eligibility.l4
git commit -m "feat: Update leave eligibility for 2023 amendment

Changes:
- Increase minimum service from 1 to 2 years
- Restrict to permanent employees only

Tests: All regression tests updated
Reviewed: #PR-123"

# Push and create PR
git push origin feature/2023-leave-amendment
```

### Semantic Versioning for L4 Modules

```l4
§ `Leave Eligibility Rules v2.0.0`

{-
Version: 2.0.0
Date: 2023-06-15
Breaking changes from v1.0.0:
- Minimum years of service: 1 → 2 years
- Added employment type restriction

Migration guide:
- Review all cases with yearsOfService between 1-2
- Review all cases with non-Permanent employment type
-}
```

### Tagging Releases

```bash
# Tag a release
git tag -a v2.0.0 -m "v2.0.0: 2023 Leave Amendment
Breaking changes:
- Minimum service years: 1 → 2
- Employment type: Added restriction

Approved: Legal-2023-06-15"

git push origin v2.0.0
```

## Part 6: Continuous Integration

### GitHub Actions for L4

```yaml
# .github/workflows/l4-tests.yml
name: L4 Tests

on:
  push:
    branches: [main, develop]
  pull_request:
    branches: [main]

jobs:
  test:
    runs-on: ubuntu-latest

    steps:
      - uses: actions/checkout@v3

      - name: Setup Haskell
        uses: haskell/actions/setup@v2
        with:
          ghc-version: "9.6"
          cabal-version: "3.10"

      - name: Cache dependencies
        uses: actions/cache@v3
        with:
          path: ~/.cabal
          key: ${{ runner.os }}-cabal-${{ hashFiles('**/*.cabal') }}

      - name: Build
        run: cabal build all

      - name: Run unit tests
        run: cabal run jl4-cli -- tests/unit/*.l4

      - name: Run integration tests
        run: cabal run jl4-cli -- tests/integration/*.l4

      - name: Run regression tests
        run: |
          cabal run jl4-cli -- tests/regression/*.l4 > current-output.txt
          diff golden-output.txt current-output.txt || {
            echo "Regression test differences detected!"
            echo "Review changes before merging."
            exit 1
          }

      - name: Generate impact report
        if: github.event_name == 'pull_request'
        run: ./scripts/impact-report.sh > impact-report.md

      - name: Comment PR with impact
        if: github.event_name == 'pull_request'
        uses: actions/github-script@v6
        with:
          script: |
            const fs = require('fs');
            const impact = fs.readFileSync('impact-report.md', 'utf8');
            github.rest.issues.createComment({
              issue_number: context.issue.number,
              owner: context.repo.owner,
              repo: context.repo.repo,
              body: impact
            });
```

### Pre-Commit Hooks

```bash
# .git/hooks/pre-commit
#!/bin/bash

echo "Running L4 pre-commit checks..."

# Type check all L4 files
echo "Type checking..."
cabal run jl4-cli -- --check-only src/**/*.l4
if [ $? -ne 0 ]; then
    echo "Type checking failed!"
    exit 1
fi

# Run fast unit tests
echo "Running unit tests..."
cabal run jl4-cli -- tests/unit/*.l4
if [ $? -ne 0 ]; then
    echo "Unit tests failed!"
    exit 1
fi

echo "Pre-commit checks passed ✓"
exit 0
```

## Part 7: Test Data Management

### Versioned Test Fixtures

```l4
§ `Test Fixtures - 2020 Baseline`

`historical_case_12345` MEANS Employee WITH
    name IS "John Doe"
    yearsOfService IS 1
    employmentType IS "Contract"
    -- Expected result under 2020 rules: TRUE

`historical_case_12346` MEANS Employee WITH
    name IS "Jane Smith"
    yearsOfService IS 0.5
    employmentType IS "Permanent"
    -- Expected result under 2020 rules: FALSE

-- Store expected results
`expected_results_2020` MEANS LIST
    PAIR "12345" TRUE,
    PAIR "12346" FALSE
```

### Test Case Generation

```l4
§ `Automated Test Case Generation`

IMPORT prelude

-- Generate test cases for all boundary conditions
`boundary_cases` MEANS LIST
    Employee WITH yearsOfService IS 0,   employmentType IS "Permanent",
    Employee WITH yearsOfService IS 0.9, employmentType IS "Permanent",
    Employee WITH yearsOfService IS 1,   employmentType IS "Permanent",
    Employee WITH yearsOfService IS 1.1, employmentType IS "Permanent",
    Employee WITH yearsOfService IS 2,   employmentType IS "Permanent",
    Employee WITH yearsOfService IS 1,   employmentType IS "Contract",
    Employee WITH yearsOfService IS 1,   employmentType IS "Temporary"

-- Test all boundary cases
#EVAL map checkEligibility `boundary_cases`
    WHERE
        checkEligibility emp MEANS
            PAIR emp `qualifies for leave` emp
```

## Part 8: Rollback Procedures

### Safe Rollback Strategy

```bash
# If a deployment causes issues, rollback to previous version

# Tag current state before deploying
git tag -a pre-deploy-$(date +%Y%m%d) -m "Pre-deployment snapshot"

# Deploy new version
./deploy.sh v2.0.0

# If issues detected
echo "Issues detected, rolling back..."

# Rollback to previous version
git checkout v1.9.0
./deploy.sh v1.9.0

# Update status
git tag -a rollback-$(date +%Y%m%d) -m "Rolled back from v2.0.0 due to issue #456"
```

### Database State Management

When L4 rules change, historical database records may become inconsistent:

```sql
-- Store evaluation metadata
CREATE TABLE evaluation_log (
    id SERIAL PRIMARY KEY,
    case_id VARCHAR(50),
    l4_version VARCHAR(20),
    evaluated_at TIMESTAMP,
    result BOOLEAN,
    input_json JSONB,
    trace_json JSONB
);

-- Query historical decisions under old rules
SELECT * FROM evaluation_log
WHERE l4_version = 'v1.9.0'
  AND result = TRUE;

-- Re-evaluate under new rules
-- (Run in application code, not SQL)
```

## Part 9: Documentation of Changes

### Change Log Template

```markdown
# Changelog

## [2.0.0] - 2023-06-15

### Breaking Changes

- **Minimum years of service**: Increased from 1 to 2 years
- **Employment type restriction**: Only "Permanent" employees qualify

### Impact

- 127 historical cases now return different results
- See `tests/regression/impact-2023-06-15.txt` for full list

### Migration

- Review all cases with 1-2 years of service
- Review all cases with non-Permanent employment

### References

- Legal amendment: Employment Act 2023 Amendment
- Approved by: Legal Department (2023-06-10)
- Implementation: PR#123

## [1.9.0] - 2022-12-01

### Added

- Support for part-time employees
- Prorated leave calculations

### Impact

- No breaking changes
- 15 new test cases added
```

## Key Takeaways

1. **Regression tests prevent unintended changes** to legal logic
2. **Golden master testing** captures known-good outputs
3. **Change impact analysis** quantifies effects of rule changes
4. **Version control strategies** enable safe rollback
5. **Continuous integration** automates testing on every change
6. **Document all breaking changes** with migration guides
7. **Test at multiple levels**: unit, integration, regression
8. **Maintain historical test cases** for long-term validation

## Exercises

### Exercise 1: Build a Test Suite

Create a comprehensive test suite for your WPA pipeline with unit, integration, and regression tests.

### Exercise 2: Golden Master

Generate a golden master file for your current system, then intentionally break something and see if the diff catches it.

### Exercise 3: Impact Analysis

Write a script that compares two git commits and reports how many test cases changed.

### Exercise 4: CI Pipeline

Set up a GitHub Actions workflow that runs your L4 tests on every pull request.

## Next Steps

In **Module A8**, we'll explore multi-file pipelines and system architecture—organizing large L4 projects across multiple modules and teams.
