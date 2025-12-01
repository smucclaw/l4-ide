# Module A2 — AI-Assisted Ingestion & Model Refinement

## Overview

One of L4's most powerful applications is **encoding existing legislation and regulations** into executable form. However, legal documents are often:
- Written in dense, ambiguous language
- Full of cross-references and implicit dependencies
- Structured inconsistently across documents
- Thousands of pages long

**AI assistance** (particularly LLMs) can accelerate the ingestion process by:
1. Extracting structured information from legal text
2. Identifying definitions, rules, and conditions
3. Suggesting L4 type and function structures
4. Finding inconsistencies and ambiguities

This module teaches a workflow for using AI as a drafting assistant while maintaining formal rigor.

## The Ingestion Workflow

```
Legislative Text
    ↓
1. AI Initial Extraction (GPT/Claude)
    ↓
2. Human Review & Refinement
    ↓
3. L4 Type Definition
    ↓
4. AI Rule Translation
    ↓
5. Human Verification (Type Checking)
    ↓
6. Test Generation
    ↓
7. Regression Testing
    ↓
Formal L4 Model
```

## Phase 1: Initial Text Analysis

### Example: Extracting from Legislative Text

**Source Text** (fictional):
```
Employment (Foreign Workforce) Regulations 2025

Part II — Work Pass Applications

Section 5. Eligibility Criteria

(1) An employee shall be eligible for a work pass if:
    (a) the employee is at least 18 years of age and not more than 65 years of age;
    (b) the employee holds a recognized degree or diploma relevant to the employment;
    (c) the employee has at least 2 years of relevant work experience;
    (d) the employee's proposed monthly salary is not less than $5,000;
    (e) the employing company has not exceeded its foreign worker quota.

(2) For the purposes of subsection (1)(b), "recognized degree or diploma" means:
    (a) a degree from a university recognized by the Ministry;
    (b) a diploma from a polytechnic or equivalent institution; or
    (c) a professional qualification approved by the relevant regulatory body.

(3) The Director may waive the requirement in subsection (1)(c) if the employee possesses exceptional skills or qualifications.
```

### AI Prompt for Initial Extraction

```
You are assisting in converting legislation into the L4 programming language for law.

Given the following legal text, please:

1. Identify all DEFINED TERMS (words with specific legal meanings)
2. Extract ELIGIBILITY CONDITIONS as Boolean predicates
3. Identify NUMERIC THRESHOLDS (ages, amounts, durations)
4. Note EXCEPTIONS and DISCRETIONARY POWERS
5. Map relationships between definitions

Format your response as:

## Defined Terms
- term: definition (source: section reference)

## Eligibility Conditions
- condition: description (Boolean)

## Numeric Thresholds
- parameter: value (units)

## Exceptions
- exception: description (who can invoke, under what circumstances)

[LEGISLATIVE TEXT HERE]
```

### AI Response Example

```markdown
## Defined Terms
- employee: A person applying for a work pass (source: implied throughout)
- work pass: Authorization to work in the jurisdiction (source: section title)
- recognized degree or diploma: Defined in Section 5(2)
- foreign worker quota: Maximum number of foreign employees (source: 5(1)(e))
- Director: The authority with discretionary powers (source: 5(3))

## Eligibility Conditions
- Age requirement: employee.age >= 18 AND employee.age <= 65
- Education requirement: employee has recognized degree or diploma
- Experience requirement: employee.years_experience >= 2
- Salary requirement: employee.monthly_salary >= 5000
- Quota requirement: company.foreign_workers < company.foreign_worker_quota

## Numeric Thresholds
- Minimum age: 18 (years)
- Maximum age: 65 (years)
- Minimum experience: 2 (years)
- Minimum salary: 5000 (dollars per month)

## Exceptions
- Experience waiver: Director may waive 2-year requirement for exceptional skills
  (discretionary, case-by-case)
```

## Phase 2: Translating to L4 Types

### AI Prompt for Type Definition

```
Based on the extracted conditions, generate L4 type definitions.

Use this template:
- Enums for fixed sets of values
- Records for entities with multiple properties
- MAYBE for optional fields
- Lists for collections

Extracted data:
[PASTE AI RESPONSE FROM PHASE 1]
```

### AI-Generated L4 (First Draft)

```l4
DECLARE EducationLevel IS ONE OF
    UnrecognizedDegree
    RecognizedDiploma
    RecognizedDegree
    ProfessionalQualification

DECLARE Employee HAS
    name IS A STRING
    age IS A NUMBER
    educationLevel IS AN EducationLevel
    yearsExperience IS A NUMBER
    monthlySalary IS A NUMBER
    hasExceptionalSkills IS A BOOLEAN

DECLARE Company HAS
    name IS A STRING
    foreignWorkerCount IS A NUMBER
    foreignWorkerQuota IS A NUMBER

DECLARE WaiverRequest HAS
    employee IS AN Employee
    justification IS A STRING
    approvedByDirector IS A BOOLEAN
```

### Human Refinement

The human reviewer then refines:

```l4
-- Improved version with better naming and additional fields

DECLARE EducationType IS ONE OF
    UniversityDegree
    PolytechnicDiploma
    ProfessionalQualification
    NotRecognized

DECLARE Employee HAS
    fullName IS A STRING
    passportNumber IS A STRING
    dateOfBirth IS A DATE
    educationType IS AN EducationType
    yearsRelevantExperience IS A NUMBER
    proposedMonthlySalary IS A NUMBER
    nationality IS A STRING

DECLARE Company HAS
    registrationNumber IS A STRING
    businessName IS A STRING
    currentForeignWorkers IS A NUMBER
    foreignWorkerQuota IS A NUMBER
    industry IS A STRING

DECLARE ExperienceWaiver HAS
    employeeId IS A STRING
    justification IS A STRING
    directorApproved IS A BOOLEAN
    approvalDate IS A MAYBE DATE
```

## Phase 3: Rule Translation

### AI Prompt for Rule Generation

```
Convert the eligibility conditions to L4 functions.

Template:
GIVEN employee IS AN Employee
      company IS A Company
GIVETH A BOOLEAN
DECIDE `meets eligibility` IF
    [conditions combined with AND]

Conditions:
[LIST CONDITIONS FROM PHASE 1]
```

### AI-Generated Rules

```l4
IMPORT prelude
IMPORT daydate

GIVEN employee IS AN Employee
GIVETH A BOOLEAN
DECIDE `meets age requirement` IF
        `age of` employee AT LEAST 18
    AND `age of` employee AT MOST 65

GIVEN employee IS AN Employee
GIVETH A NUMBER
`age of` employee MEANS
    -- Calculate age from date of birth
    FLOOR ((today MINUS employee's dateOfBirth) DIVIDED BY 365.25)

GIVEN employee IS AN Employee
GIVETH A BOOLEAN
DECIDE `has recognized education` IF
    CONSIDER employee's educationType
    WHEN UniversityDegree         THEN TRUE
    WHEN PolytechnicDiploma       THEN TRUE
    WHEN ProfessionalQualification THEN TRUE
    WHEN NotRecognized            THEN FALSE

GIVEN employee IS AN Employee
GIVETH A BOOLEAN
DECIDE `meets experience requirement` IF
    employee's yearsRelevantExperience AT LEAST 2

GIVEN employee IS AN Employee
GIVETH A BOOLEAN
DECIDE `meets salary requirement` IF
    employee's proposedMonthlySalary AT LEAST 5000

GIVEN company IS A Company
GIVETH A BOOLEAN
DECIDE `company within quota` IF
    company's currentForeignWorkers LESS THAN company's foreignWorkerQuota

-- Main eligibility check
GIVEN employee IS AN Employee
      company IS A Company
GIVETH A BOOLEAN
DECIDE `eligible for work pass` IF
        `meets age requirement` employee
    AND `has recognized education` employee
    AND `meets experience requirement` employee
    AND `meets salary requirement` employee
    AND `company within quota` company
```

### Human Review Checklist

After AI generation, verify:

1. **Type correctness**: Do all types match?
2. **Logic correctness**: Does the Boolean logic match the legislation?
3. **Edge cases**: Are boundaries (18, 65, 2 years) handled correctly?
4. **Missing nuances**: Did AI miss implicit conditions?
5. **Ambiguities**: Are there unclear cases that need clarification?

## Phase 4: Handling Discretion and Exceptions

### Modeling Discretionary Powers

The original text says:
> "The Director may waive the requirement in subsection (1)(c)..."

This introduces **discretion**—a human decision that can't be fully automated.

**Approach 1: Model as Data**
```l4
DECLARE Application HAS
    employee IS AN Employee
    company IS A Company
    experienceWaiver IS A MAYBE ExperienceWaiver

GIVEN application IS AN Application
GIVETH A BOOLEAN
DECIDE `eligible for work pass` IF
    -- Base requirements
    baseRequirements
    -- Either meets experience OR has approved waiver
    AND (meetsExperience OR hasWaiver)
    WHERE
        baseRequirements MEANS
                `meets age requirement` employee
            AND `has recognized education` employee
            AND `meets salary requirement` employee
            AND `company within quota` company

        employee MEANS application's employee
        company MEANS application's employer

        meetsExperience MEANS
            `meets experience requirement` employee

        hasWaiver MEANS
            CONSIDER application's experienceWaiver
            WHEN NOTHING THEN FALSE
            WHEN JUST waiver THEN waiver's directorApproved
```

**Approach 2: External Decision Points**
```l4
-- Mark as requiring human review
DECLARE EligibilityResult IS ONE OF
    Eligible
    NotEligible HAS reason IS A STRING
    RequiresDirectorReview HAS reason IS A STRING

GIVEN application IS AN Application
GIVETH AN EligibilityResult
`assess eligibility` application MEANS
    IF NOT meetsExperience AND hasExceptionalSkills
    THEN RequiresDirectorReview WITH
        reason IS "Experience waiver: exceptional skills claimed"
    ELSE IF meetsAllRequirements
    THEN Eligible
    ELSE NotEligible WITH
        reason IS failureReason
```

## Phase 5: Iterative Refinement with AI

### Finding Ambiguities

**AI Prompt:**
```
Review this L4 code for potential ambiguities or edge cases:

[PASTE L4 CODE]

Identify:
1. Boundary conditions not explicitly handled
2. Ambiguous terms that need clarification
3. Potential conflicts between rules
4. Missing error cases
```

**AI Response:**
```markdown
Potential Issues:

1. Age calculation: What if employee born on Feb 29? (leap year)
   - Current: Uses 365.25 average
   - Risk: Off-by-one errors near boundaries

2. Salary: Currency not specified
   - Assumption: Local currency
   - Risk: Multi-currency companies

3. "Recognized degree": Who recognizes?
   - Text says "Ministry" but Ministry not modeled
   - Need: List of recognized institutions or approval process

4. Experience counting: Does part-time count?
   - Text says "relevant work experience"
   - Unclear: How to count overlapping positions, internships, etc.

5. Quota checking: Race condition
   - Multiple applications processed simultaneously
   - Quota could be exceeded between check and approval
```

### Addressing Ambiguities

Update code based on feedback:

```l4
-- Document assumptions
{-
ASSUMPTIONS:
1. Ages calculated using standard calendar (365.25 days/year)
2. Salaries in local currency (SGD)
3. Education recognition verified separately before this check
4. Experience is full-time equivalent (FTE) years
5. Quota is checked at application time, not approval time
-}

-- Add validation for edge cases
GIVEN employee IS AN Employee
GIVETH A BOOLEAN
`has valid date of birth` employee MEANS
        employee's dateOfBirth LESS THAN today
    AND employee's dateOfBirth GREATER THAN DATE OF 1, 1, 1900
```

## Phase 6: Test Generation with AI

### AI Prompt for Test Cases

```
Generate comprehensive test cases for this L4 code:

[PASTE L4 CODE]

Include:
- Happy path (all conditions met)
- Each failure condition individually
- Boundary values (18, 65, 2 years, $5000)
- Edge cases (just under/over boundaries)

Format as L4 test fixtures.
```

### AI-Generated Tests

```l4
§ `AI-Generated Test Fixtures`

-- Happy path
`Employee - All Requirements Met` MEANS Employee WITH
    fullName IS "John Doe"
    passportNumber IS "P1234567"
    dateOfBirth IS DATE OF 15, 6, 1990  -- Age 34
    educationType IS UniversityDegree
    yearsRelevantExperience IS 5
    proposedMonthlySalary IS 6000
    nationality IS "Canada"

-- Boundary: Exactly minimum age (18)
`Employee - Age 18` MEANS Employee WITH
    fullName IS "Just Eighteen"
    passportNumber IS "P2345678"
    dateOfBirth IS DATE OF 1, 1, 2007  -- Turns 18 in 2025
    educationType IS UniversityDegree
    yearsRelevantExperience IS 2
    proposedMonthlySalary IS 5000
    nationality IS "USA"

-- Boundary: One day under 18
`Employee - Age 17` MEANS Employee WITH
    fullName IS "Almost Eighteen"
    passportNumber IS "P3456789"
    dateOfBirth IS DATE OF 2, 1, 2008  -- Still 17 in early 2025
    educationType IS PolytechnicDiploma
    yearsRelevantExperience IS 0
    proposedMonthlySalary IS 5000
    nationality IS "UK"

-- [More test cases...]

§§ `Automated Assertions`

#ASSERT `eligible for work pass`
    `Employee - All Requirements Met`
    `Company - Within Quota`

#ASSERT NOT `eligible for work pass`
    `Employee - Age 17`
    `Company - Within Quota`

-- Test each condition individually
#ASSERT `meets age requirement` `Employee - Age 18`
#ASSERT NOT `meets age requirement` `Employee - Age 17`
```

## Tools and Techniques

### Claude/GPT Integration

**Recommended workflow:**
1. Use Claude/GPT-4 for initial extraction and drafting
2. Always review AI output for hallucinations
3. Run type checker after each AI suggestion
4. Use AI to generate test cases, then verify by hand
5. Iterate: AI suggests, human refines, AI refines further

### Prompt Engineering Tips

**Good prompts:**
- Provide examples of desired output format
- Be specific about what you want extracted
- Ask for structured output (markdown, JSON, or L4 directly)
- Request citations to source text

**Avoid:**
- Asking AI to "fully automate" complex legal reasoning
- Trusting AI on numeric calculations without verification
- Letting AI make policy decisions (stick to extraction)

### Version Control for AI-Assisted Work

```bash
# Create branch for AI-assisted ingestion
git checkout -b ingest/employment-regulations

# Commit AI first draft
git add src/employment-regs-draft.l4
git commit -m "WIP: AI first pass at employment regulations"

# Commit human refinements
git add src/employment-regs-refined.l4
git commit -m "refine: Fix type errors and add edge cases"

# Commit tests
git add tests/employment-regs-tests.l4
git commit -m "test: Add comprehensive test coverage"
```

## Case Study: Real Legislation

### Singapore's Personal Data Protection Act (Simplified)

**Original Text:**
```
Section 18. Consent for collection, use or disclosure of personal data

(1) An organisation shall not collect, use or disclose personal data about an individual unless —
    (a) the individual gives, or is deemed to have given, consent under this Act;
    (b) the collection, use or disclosure, as the case may be, without consent is required or authorised under this Act or any other written law.
```

**After AI Extraction and Human Refinement:**

```l4
IMPORT prelude

DECLARE Consent IS ONE OF
    ExplicitConsent HAS date IS A DATE
    DeemedConsent HAS basisSection IS A STRING
    NoConsent

DECLARE LegalBasis IS ONE OF
    ActRequirement HAS section IS A STRING
    WrittenLawAuthorization HAS statute IS A STRING
    NoLegalBasis

DECLARE DataAction IS ONE OF
    Collection
    Use
    Disclosure

DECLARE Individual HAS
    id IS A STRING
    name IS A STRING

DECLARE PersonalData HAS
    dataType IS A STRING
    belongsTo IS AN Individual

DECLARE Organisation HAS
    name IS A STRING
    registrationNumber IS A STRING

GIVEN org IS AN Organisation
      data IS A PersonalData
      action IS A DataAction
      consent IS A Consent
      legalBasis IS A LegalBasis
GIVETH A BOOLEAN
DECIDE `may perform action` IF
    hasConsent OR hasLegalBasis
    WHERE
        hasConsent MEANS
            CONSIDER consent
            WHEN ExplicitConsent WITH date THEN TRUE
            WHEN DeemedConsent WITH basisSection THEN TRUE
            WHEN NoConsent THEN FALSE

        hasLegalBasis MEANS
            CONSIDER legalBasis
            WHEN ActRequirement WITH section THEN TRUE
            WHEN WrittenLawAuthorization WITH statute THEN TRUE
            WHEN NoLegalBasis THEN FALSE
```

## Key Takeaways

1. **AI accelerates extraction** but requires human verification
2. **Start with definitions**—types before functions
3. **Iterate in phases**: extract → draft → refine → test
4. **Document assumptions** explicitly in comments
5. **Model discretion** as either data or external decision points
6. **Generate test cases with AI**, verify by hand
7. **Use version control** to track AI vs human contributions
8. **Always run type checker** after AI suggestions

## Exercises

### Exercise 1: Extract and Model
Take a 2-page excerpt from real legislation. Use AI to extract definitions and conditions, then write L4 types.

### Exercise 2: Ambiguity Detection
Review AI-generated L4 code. Identify 5 potential ambiguities or edge cases.

### Exercise 3: Test Generation
Use AI to generate 20 test cases for a complex eligibility function. Verify each one.

### Exercise 4: Discretion Modeling
Model a regulation with discretionary powers. Create both approaches (data-based and review-required).

## Next Steps

In **Module A3**, we'll explore temporal logic—handling effective dates, rolling windows, and time-based conditions that are ubiquitous in legislation.
