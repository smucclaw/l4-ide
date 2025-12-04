# Module 5 — WPA Eligibility Pipeline

## Overview

In this module, we bring together everything from Modules 1-4 to build a **complete, production-grade eligibility assessment pipeline** for the WorkPass Authority.

You'll learn how to:
- Structure complex multi-step assessments
- Compose functions for maintainability
- Generate detailed explanations for decisions
- Handle edge cases gracefully

## The Pipeline Architecture

Our pipeline follows this flow:

```
Application Input
    ↓
Personal Eligibility Check (age, education, experience)
    ↓
Salary Verification
    ↓
Company Quota Check
    ↓
Document Verification
    ↓
Risk Assessment
    ↓
Final Decision + Explanation
```

Each stage can:
- **Pass** (proceed to next stage)
- **Fail** (return rejection with reason)
- **Flag** (note issue but continue)

## Complete Domain Model

First, let's establish our full type system:

```l4
IMPORT prelude
IMPORT daydate

§ `WorkPass Authority - Complete System`

§§ `Core Enums`

DECLARE EmploymentCategory IS ONE OF
    TechProfessional
    HealthcareWorker
    Researcher
    FinancialServices
    EntertainmentArts
    Other

DECLARE EducationLevel IS ONE OF
    HighSchool
    Diploma
    Bachelor
    Master
    Doctorate

DECLARE RiskLevel IS ONE OF
    VeryLow
    Low
    Medium
    High
    VeryHigh

DECLARE DocumentType IS ONE OF
    Passport
    Diploma
    EmploymentContract
    FinancialStatements
    ReferenceLetter
    BackgroundCheck
    TaxClearance

§§ `Core Records`

DECLARE Employee HAS
    name               IS A STRING
    passportNumber     IS A STRING
    nationality        IS A STRING
    dateOfBirth        IS A DATE
    educationLevel     IS AN EducationLevel
    yearsExperience    IS A NUMBER
    monthlySalary      IS A NUMBER
    category           IS AN EmploymentCategory
    hasDisqualifyingRecord IS A BOOLEAN

DECLARE Company HAS
    registrationNumber IS A STRING
    businessName       IS A STRING
    industry           IS A STRING
    foundedYear        IS A NUMBER
    localEmployeeCount IS A NUMBER
    foreignWorkerCount IS A NUMBER
    foreignWorkerQuota IS A NUMBER
    paidUpCapital      IS A NUMBER

DECLARE WorkPassApplication HAS
    applicationId      IS A STRING
    employee           IS AN Employee
    employer           IS A Company
    proposedStartDate  IS A DATE
    contractDuration   IS A NUMBER
    submissionDate     IS A DATE

§§ `Result Types`

DECLARE CheckResult HAS
    passed             IS A BOOLEAN
    reason             IS A STRING
    details            IS A LIST OF STRING

DECLARE AssessmentResult HAS
    applicationId      IS A STRING
    approved           IS A BOOLEAN
    riskLevel          IS A RiskLevel
    processingDays     IS A NUMBER
    requiredDocuments  IS A LIST OF DocumentType
    failureReasons     IS A LIST OF STRING
    warningMessages    IS A LIST OF STRING
    recommendedActions IS A LIST OF STRING
```

## Stage 1: Personal Eligibility

```l4
§§ `Stage 1 - Personal Eligibility Checks`

GIVEN employee IS AN Employee
GIVETH A CheckResult
`check personal eligibility` employee MEANS
    CheckResult WITH
        passed  IS allPassed
        reason  IS summary
        details IS allDetails
    WHERE
        ageCheck MEANS `check age requirements` employee
        eduCheck MEANS `check education requirements` employee
        expCheck MEANS `check experience requirements` employee
        recCheck MEANS `check criminal record` employee

        allPassed MEANS
                ageCheck's passed
            AND eduCheck's passed
            AND expCheck's passed
            AND recCheck's passed

        summary MEANS
            IF allPassed
            THEN "Personal eligibility: Passed"
            ELSE "Personal eligibility: Failed"

        allDetails MEANS
            ageCheck's details
            `append` eduCheck's details
            `append` expCheck's details
            `append` recCheck's details

-- Age requirements
GIVEN employee IS AN Employee
GIVETH A CheckResult
`check age requirements` employee MEANS
    CheckResult WITH
        passed  IS meetsAge
        reason  IS ageMessage
        details IS LIST ageMessage
    WHERE
        currentAge MEANS
            `age of` employee `as of` `today`

        meetsAge MEANS
            currentAge AT LEAST 18 AND currentAge AT MOST 65

        ageMessage MEANS
            IF currentAge LESS THAN 18
            THEN "Age requirement failed: Applicant is under 18"
            ELSE IF currentAge GREATER THAN 65
            THEN "Age requirement failed: Applicant is over 65"
            ELSE "Age requirement passed: " APPEND STRING currentAge

-- Education requirements
GIVEN employee IS AN Employee
GIVETH A CheckResult
`check education requirements` employee MEANS
    CheckResult WITH
        passed  IS meetsEducation
        reason  IS eduMessage
        details IS LIST eduMessage
    WHERE
        employeeRank MEANS
            `education level rank` employee's educationLevel

        requiredRank MEANS
            `education level rank` requiredLevel

        requiredLevel MEANS
            `minimum education for` employee's category

        meetsEducation MEANS
            employeeRank AT LEAST requiredRank

        eduMessage MEANS
            IF meetsEducation
            THEN "Education requirement passed"
            ELSE "Education requirement failed: Requires at least "
                APPEND `education level name` requiredLevel

-- Experience requirements
GIVEN employee IS AN Employee
GIVETH A CheckResult
`check experience requirements` employee MEANS
    CheckResult WITH
        passed  IS meetsExperience
        reason  IS expMessage
        details IS LIST expMessage
    WHERE
        yearsNeeded MEANS
            `minimum experience for` employee's category

        meetsExperience MEANS
            employee's yearsExperience AT LEAST yearsNeeded

        expMessage MEANS
            IF meetsExperience
            THEN "Experience requirement passed: "
                APPEND STRING employee's yearsExperience APPEND " years"
            ELSE "Experience requirement failed: Requires at least "
                APPEND STRING yearsNeeded APPEND " years"

-- Criminal record check
GIVEN employee IS AN Employee
GIVETH A CheckResult
`check criminal record` employee MEANS
    CheckResult WITH
        passed  IS NOT employee's hasDisqualifyingRecord
        reason  IS message
        details IS LIST message
    WHERE
        message MEANS
            IF employee's hasDisqualifyingRecord
            THEN "Has disqualifying criminal record"
            ELSE "No disqualifying records found"
```

## Stage 2: Salary Verification

```l4
§§ `Stage 2 - Salary Verification`

GIVEN employee IS AN Employee
GIVETH A CheckResult
`check salary requirements` employee MEANS
    CheckResult WITH
        passed  IS meetsSalary
        reason  IS message
        details IS detailsList
    WHERE
        minSalary MEANS
            `minimum monthly salary for` employee's category

        meetsSalary MEANS
            employee's monthlySalary AT LEAST minSalary

        percentageAbove MEANS
            ((employee's monthlySalary MINUS minSalary)
                DIVIDED BY minSalary) TIMES 100

        message MEANS
            IF meetsSalary
            THEN "Salary requirement passed"
            ELSE "Salary below minimum for category"

        detailsList MEANS
            LIST
                "Offered salary: $" APPEND STRING employee's monthlySalary,
                "Minimum required: $" APPEND STRING minSalary,
                IF meetsSalary
                THEN STRING percentageAbove APPEND "% above minimum"
                ELSE "Deficit: $" APPEND STRING (minSalary MINUS employee's monthlySalary)
```

## Stage 3: Company Quota Check

```l4
§§ `Stage 3 - Company Quota Verification`

GIVEN company IS A Company
GIVETH A CheckResult
`check quota availability` company MEANS
    CheckResult WITH
        passed  IS withinQuota
        reason  IS message
        details IS detailsList
    WHERE
        currentUsage MEANS company's foreignWorkerCount
        maxAllowed MEANS company's foreignWorkerQuota
        available MEANS maxAllowed MINUS currentUsage
        utilizationPct MEANS (currentUsage DIVIDED BY maxAllowed) TIMES 100

        withinQuota MEANS currentUsage LESS THAN maxAllowed

        message MEANS
            IF withinQuota
            THEN "Quota check passed"
            ELSE "Foreign worker quota exceeded"

        detailsList MEANS
            LIST
                "Current foreign workers: " APPEND STRING currentUsage,
                "Maximum allowed: " APPEND STRING maxAllowed,
                "Available slots: " APPEND STRING available,
                "Utilization: " APPEND STRING utilizationPct APPEND "%"
```

## Stage 4: Risk Assessment

```l4
§§ `Stage 4 - Risk Assessment`

GIVEN application IS A WorkPassApplication
GIVETH A RiskLevel
`assess application risk` application MEANS
    CONSIDER totalScore
    WHEN score THEN
        IF score AT LEAST 90 THEN VeryLow
        ELSE IF score AT LEAST 75 THEN Low
        ELSE IF score AT LEAST 50 THEN Medium
        ELSE IF score AT LEAST 25 THEN High
        ELSE VeryHigh
    WHERE
        totalScore MEANS
            companyScore PLUS employeeScore PLUS salaryScore

        employee MEANS application's employee
        employer MEANS application's employer

        companyScore MEANS
            `company reliability score` employer

        employeeScore MEANS
            `employee qualification score` employee

        salaryScore MEANS
            `salary competitiveness score` employee

GIVEN company IS A Company
GIVETH A NUMBER
`company reliability score` company MEANS
    ageScore PLUS sizeScore PLUS capitalScore
    WHERE
        yearsInBusiness MEANS `today`'s year MINUS company's foundedYear

        ageScore MEANS
            IF yearsInBusiness AT LEAST 10 THEN 15
            ELSE IF yearsInBusiness AT LEAST 5 THEN 10
            ELSE 5

        sizeScore MEANS
            IF company's localEmployeeCount AT LEAST 50 THEN 15
            ELSE IF company's localEmployeeCount AT LEAST 20 THEN 10
            ELSE 5

        capitalScore MEANS
            IF company's paidUpCapital AT LEAST 1000000 THEN 10
            ELSE IF company's paidUpCapital AT LEAST 500000 THEN 5
            ELSE 0

GIVEN employee IS AN Employee
GIVETH A NUMBER
`employee qualification score` employee MEANS
    eduScore PLUS expScore
    WHERE
        eduScore MEANS
            CONSIDER employee's educationLevel
            WHEN Doctorate THEN 30
            WHEN Master THEN 25
            WHEN Bachelor THEN 20
            WHEN Diploma THEN 10
            WHEN HighSchool THEN 5

        expScore MEANS
            IF employee's yearsExperience AT LEAST 10 THEN 30
            ELSE employee's yearsExperience TIMES 3

GIVEN employee IS AN Employee
GIVETH A NUMBER
`salary competitiveness score` employee MEANS
    percentageAbove DIVIDED BY 10
    WHERE
        minSalary MEANS
            `minimum monthly salary for` employee's category

        percentageAbove MEANS
            IF employee's monthlySalary AT LEAST minSalary
            THEN ((employee's monthlySalary MINUS minSalary)
                    DIVIDED BY minSalary) TIMES 100
            ELSE 0
```

## Stage 5: Document Requirements

```l4
§§ `Stage 5 - Document Requirements`

GIVEN application IS A WorkPassApplication
GIVETH A LIST OF DocumentType
`required documents for` application MEANS
    baseDocuments `append` categoryDocuments `append` riskDocuments
    WHERE
        employee MEANS application's employee
        employer MEANS application's employer

        baseDocuments MEANS
            LIST Passport, Diploma, EmploymentContract

        categoryDocuments MEANS
            CONSIDER employee's category
            WHEN FinancialServices THEN
                LIST FinancialStatements, BackgroundCheck
            WHEN Researcher THEN
                LIST ReferenceLetter
            WHEN TechProfessional THEN
                IF employer's foundedYear AT LEAST 2020
                THEN LIST ReferenceLetter
                ELSE EMPTY
            OTHERWISE EMPTY

        riskLevel MEANS
            `assess application risk` application

        riskDocuments MEANS
            CONSIDER riskLevel
            WHEN High THEN LIST BackgroundCheck, TaxClearance
            WHEN VeryHigh THEN LIST BackgroundCheck, TaxClearance, ReferenceLetter
            OTHERWISE EMPTY
```

## The Complete Pipeline

```l4
§§ `Complete Assessment Pipeline`

GIVEN application IS A WorkPassApplication
GIVETH AN AssessmentResult
`process application` application MEANS
    AssessmentResult WITH
        applicationId      IS application's applicationId
        approved           IS isApproved
        riskLevel          IS risk
        processingDays     IS estimatedDays
        requiredDocuments  IS documents
        failureReasons     IS failures
        warningMessages    IS warnings
        recommendedActions IS actions
    WHERE
        employee MEANS application's employee
        employer MEANS application's employer

        -- Run all checks
        personalCheck MEANS `check personal eligibility` employee
        salaryCheck MEANS `check salary requirements` employee
        quotaCheck MEANS `check quota availability` employer

        -- Determine approval
        isApproved MEANS
                personalCheck's passed
            AND salaryCheck's passed
            AND quotaCheck's passed

        -- Assess risk
        risk MEANS `assess application risk` application

        -- Estimate processing time
        estimatedDays MEANS
            baseProcessing PLUS riskAdjustment
            WHERE
                baseProcessing MEANS
                    `standard processing days for` employee's category

                riskAdjustment MEANS
                    CONSIDER risk
                    WHEN VeryLow THEN 0
                    WHEN Low THEN 0
                    WHEN Medium THEN 5
                    WHEN High THEN 10
                    WHEN VeryHigh THEN 20

        -- Collect documents needed
        documents MEANS
            `required documents for` application

        -- Collect failure reasons
        failures MEANS
            IF NOT personalCheck's passed THEN personalCheck's details
            ELSE EMPTY
            `append`
            IF NOT salaryCheck's passed THEN salaryCheck's details
            ELSE EMPTY
            `append`
            IF NOT quotaCheck's passed THEN quotaCheck's details
            ELSE EMPTY

        -- Generate warnings
        warnings MEANS
            IF employer's foundedYear AT LEAST 2022
            THEN LIST "Company is relatively new (< 3 years)"
            ELSE EMPTY
            `append`
            IF employee's yearsExperience LESS THAN 3
            THEN LIST "Applicant has limited work experience"
            ELSE EMPTY

        -- Recommended actions
        actions MEANS
            IF NOT isApproved
            THEN LIST "Review failure reasons and resubmit"
            ELSE IF risk EQUALS High OR risk EQUALS VeryHigh
            THEN LIST "Expedite document verification", "Schedule interview"
            ELSE LIST "Standard processing workflow"
```

## Helper Functions

```l4
§§ `Helper Functions`

GIVEN category IS AN EmploymentCategory
GIVETH A NUMBER
`minimum monthly salary for` category MEANS
    CONSIDER category
    WHEN TechProfessional  THEN 5000
    WHEN HealthcareWorker  THEN 4500
    WHEN Researcher        THEN 5500
    WHEN FinancialServices THEN 6000
    WHEN EntertainmentArts THEN 3500
    WHEN Other             THEN 3000

GIVEN category IS AN EmploymentCategory
GIVETH AN EducationLevel
`minimum education for` category MEANS
    CONSIDER category
    WHEN TechProfessional  THEN Bachelor
    WHEN HealthcareWorker  THEN Diploma
    WHEN Researcher        THEN Master
    WHEN FinancialServices THEN Bachelor
    WHEN EntertainmentArts THEN Diploma
    WHEN Other             THEN HighSchool

GIVEN category IS AN EmploymentCategory
GIVETH A NUMBER
`minimum experience for` category MEANS
    CONSIDER category
    WHEN TechProfessional  THEN 2
    WHEN HealthcareWorker  THEN 3
    WHEN Researcher        THEN 4
    WHEN FinancialServices THEN 3
    WHEN EntertainmentArts THEN 1
    WHEN Other             THEN 0

GIVEN category IS AN EmploymentCategory
GIVETH A NUMBER
`standard processing days for` category MEANS
    CONSIDER category
    WHEN TechProfessional  THEN 5
    WHEN HealthcareWorker  THEN 7
    WHEN Researcher        THEN 10
    WHEN FinancialServices THEN 14
    OTHERWISE 21

GIVEN level IS AN EducationLevel
GIVETH A NUMBER
`education level rank` level MEANS
    CONSIDER level
    WHEN Doctorate  THEN 5
    WHEN Master     THEN 4
    WHEN Bachelor   THEN 3
    WHEN Diploma    THEN 2
    WHEN HighSchool THEN 1

GIVEN level IS AN EducationLevel
GIVETH A STRING
`education level name` level MEANS
    CONSIDER level
    WHEN Doctorate  THEN "Doctorate"
    WHEN Master     THEN "Master's Degree"
    WHEN Bachelor   THEN "Bachelor's Degree"
    WHEN Diploma    THEN "Diploma"
    WHEN HighSchool THEN "High School"
```

## Key Takeaways

1. **Break complex logic into stages**—each stage is independently testable
2. **Use CheckResult type** for consistent return values across checks
3. **Compose functions** with WHERE clauses for readability
4. **Collect detailed explanations** in lists for transparency
5. **Risk assessment** uses scoring to handle gradations
6. **Helper functions** centralize business rules (salary minimums, etc.)
7. **Warnings vs failures**—not everything that's concerning is disqualifying

## Next Steps

In **Module 6**, we'll create comprehensive test data and use `#EVAL` to validate our pipeline against realistic scenarios.
