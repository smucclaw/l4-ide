# L4 Development Workflow Guide

## Overview

This guide walks through the complete process of encoding legal rules in L4, from analyzing source documents to deploying applications.

## Phase 1: Analysis & Domain Modeling

### Step 1.1: Gather Source Material

Input formats can include:

- PDF documents (contracts, policies, regulations)
- URLs to online legislation/regulations
- Natural language text descriptions
- Existing legal documentation

### Step 1.2: Identify the Domain Ontology

**Goal**: Extract the type system from the legal text.

**What to identify**:

- **Entities**: People, companies, documents, applications
- **Statuses/Categories**: Employment types, document statuses, risk levels
- **Relationships**: Who works for whom, what belongs to what
- **Attributes**: Properties of entities (names, dates, amounts)

**Key insight**: The ontology is often **unstated** in legal documents. Use domain knowledge and inference to identify implicit types.

**Example Analysis**:

```
Source text: "Foreign workers in technology roles earning
above $5,000 monthly may qualify..."

Identified types:
- EmploymentCategory (enum: Technology, Healthcare, etc.)
- Employee (record with salary field)
- Currency amounts (NUMBER type)
- Qualification rules (BOOLEAN-returning functions)
```

### Step 1.3: Model Sum and Product Types

**Sum types (enums)**: Use when there are fixed alternatives

```l4
DECLARE ApplicationStatus IS ONE OF
    Draft
    Submitted
    Approved
    Rejected
```

**Product types (records)**: Use when bundling related data

```l4
DECLARE Employee HAS
    name              IS A STRING
    salary            IS A NUMBER
    employmentCategory IS AN EmploymentCategory
```

**Maybe monad**: Use for optional/nullable data

```l4
DECLARE Document HAS
    title      IS A STRING
    expiryDate IS A MAYBE DATE  -- might not have expiry
```

## Phase 2: Encode Decision Logic

### Step 2.1: Break Down Complex Rules

**Source text structure**:

```
Section 3.2: Eligibility requires:
  (a) Age between 18 and 65, AND
  (b) Education level of Bachelor's or higher, AND
  (c) Salary meeting minimum for category
```

**L4 encoding (isomorphic structure)**:

```l4
DECIDE `meets eligibility requirements` IF
        `age in valid range` employee
    AND `has sufficient education` employee
    AND `salary meets minimum` employee
```

**Key principle**: Mirror the logical structure (ANDs/ORs) of the source text.

### Step 2.2: Define Helper Functions

Break complex logic into reusable pieces:

```l4
GIVEN employee IS AN Employee
GIVETH A BOOLEAN
`age in valid range` employee MEANS
        `age of` employee AT LEAST 18
    AND `age of` employee AT MOST 65

GIVEN employee IS AN Employee
      asOfDate IS A DATE
GIVETH A NUMBER
`age of` `employee` `as of` `asOfDate` MEANS
    FLOOR ((asOfDate MINUS employee's dateOfBirth) DIVIDED BY 365.2425)
```

### Step 2.3: Use Pattern Matching for Multi-Case Logic

**Decision tables** map naturally to CONSIDER/WHEN:

```l4
GIVEN category IS AN EmploymentCategory
GIVETH A NUMBER
`minimum salary for` category MEANS
    CONSIDER category
    WHEN TechProfessional  THEN 5000
    WHEN HealthcareWorker  THEN 4500
    WHEN Researcher        THEN 5500
    OTHERWISE 3000
```

## Phase 3: Handle Modal Logic (Advanced)

### Step 3.1: Identify Deontic Modalities

**Deontic logic** deals with obligations, permissions, prohibitions:

- MUST / SHALL: Obligations
- MAY: Permissions
- MUST NOT: Prohibitions

### Step 3.2: Identify Temporal Modalities

**Temporal logic** deals with time-dependent conditions:

- BEFORE: Something must happen before a deadline
- AFTER: Something can only happen after a condition
- WITHIN: Something must occur within a timeframe

### Step 3.3: Identify Epistemic Modalities

**Epistemic logic** deals with knowledge and belief:

- Party A knows that X
- Party B believes that Y
- It is common knowledge that Z

### Step 3.4: Model State Transitions

Multi-party contracts involve changing obligations:

```
State 1: Buyer submits order
  → Obligation: Seller must ship within 7 days

State 2: Seller ships
  → Obligation: Buyer must pay within 30 days
  → Right: Buyer may return within 14 days

State 3: Payment received
  → Obligation: Seller must provide warranty for 1 year
```

Model as state machines with transitions driven by events.

**Note**: Modal logic features are covered in the Advanced Course. Foundation features (types, functions, pattern matching) handle most common legal rules.

## Phase 4: Validation

### Step 4.1: Initial Type Check

```bash
jl4-cli your-file.l4
```

**Common errors**:

- Type mismatches (passing STRING where NUMBER expected)
- Undefined functions/types
- Pattern match not exhaustive
- Indentation/layout errors

**Fix iteratively** until you see "checking successful".

### Step 4.2: Review Compiler Warnings

Even if type-checking succeeds, warnings may indicate:

- Unused variables
- Incomplete pattern matches
- Potential logic errors

Address warnings before proceeding.

## Phase 5: Testing

### Step 5.1: Define Test Data

Create representative test cases:

```l4
`Alice` MEANS Employee WITH
    name              IS "Alice"
    dateOfBirth       IS DATE OF 15, 3, 1990
    educationLevel    IS Bachelor
    yearsExperience   IS 8
    monthlySalary     IS 8500
    category          IS TechProfessional

`Bob` MEANS Employee WITH
    name              IS "Bob"
    dateOfBirth       IS DATE OF 20, 7, 2005  -- Too young
    educationLevel    IS HighSchool
    yearsExperience   IS 1
    monthlySalary     IS 3000
    category          IS Other
```

### Step 5.2: Execute Tests

```l4
-- Positive tests (should pass)
#EVAL `meets eligibility requirements` `Alice`
#ASSERT `meets eligibility requirements` `Alice`

-- Negative tests (should fail)
#EVAL `meets eligibility requirements` `Bob`
#ASSERT NOT `meets eligibility requirements` `Bob`

-- Edge cases
#EVAL `age of` `Alice` `as of` (DATE OF 15, 3, 2008)  -- Exactly 18
```

### Step 5.3: Test Coverage

Ensure you test:

- **Happy path**: All conditions met
- **Boundary conditions**: Minimum/maximum ages, salaries, etc.
- **Invalid inputs**: Missing data, out-of-range values
- **Each branch**: Every WHEN clause, every IF/ELSE path

### Step 5.4: Regression Testing

When making changes:

1. Run all existing tests
2. Add new tests for new features
3. Verify no unexpected failures

## Phase 6: Operationalization (Generating Applications)

### Step 6.1: Identify Output Needs

What should the L4 code generate?

- **Web application**: For citizen/user interaction
- **API service**: For system integration
- **Decision tree**: For visualization
- **Documentation**: For human readers
- **Test reports**: For validation

### Step 6.2: Generate Web Apps

L4 can generate interactive web applications where users:

- Input their information
- Receive eligibility decisions
- See explanations of reasoning
- Navigate complex regulatory requirements

**Current status**: L4 ecosystem includes transpiler backends for web app generation. Consult Advanced Course Module A4 for API generation.

### Step 6.3: Create Visualizations

L4 IDE can generate:

- **Ladder diagrams**: Visual representation of decision logic
- **Decision trees**: Step-by-step reasoning paths
- **State transition diagrams**: For multi-party contract flows

### Step 6.4: Generate Documentation

From L4 source, generate:

- **Markdown**: For technical documentation
- **PDF**: For formal legal documents
- **Natural language**: Controlled English descriptions

## Phase 7: Iteration & Maintenance

### Step 7.1: Track Changes

Legal rules change frequently due to:

- New legislation
- Court decisions
- Policy updates
- Discovered edge cases

**Best practices**:

- Version control with Git
- Document changes in comments
- Maintain test suite
- Keep isomorphic mapping to source documents

### Step 7.2: Refactor for Clarity

As codebase grows:

- Extract common patterns into helper functions
- Split large files into modules
- Improve naming for readability
- Add comments for complex logic

### Step 7.3: Extend with New Requirements

When adding features:

1. Update type definitions first
2. Add function signatures
3. Implement function bodies
4. Add tests
5. Validate

## Complete Example Workflow

### Example: Encoding Insurance Policy

**Source**: Home insurance exclusion clause:

```
We do not cover loss or damage caused by rodents, insects,
vermin or birds. However, this exclusion does not apply to:
  (a) loss or damage to contents caused by birds, or
  (b) ensuing loss where an animal causes water escape from
      household appliance, swimming pool, or plumbing system
```

**Step 1: Model Domain**

```l4
DECLARE Damage HAS
    causedBy IS A STRING
    affectsContents IS A BOOLEAN
    involvesWaterEscape IS A BOOLEAN
    fromAppliance IS A BOOLEAN
```

**Step 2: Encode Logic (Isomorphic Structure)**

```l4
DECIDE `coverage applies to` `damage` IF
    NOT (   `caused by excluded pest` damage
        AND NOT `bird damage to contents` damage
        AND NOT `animal water escape from appliance` damage)
```

**Step 3: Define Helpers**

```l4
GIVEN damage IS A Damage
GIVETH A BOOLEAN
`caused by excluded pest` damage MEANS
        CONTAINS damage's causedBy "rodent"
    OR  CONTAINS damage's causedBy "insect"
    OR  CONTAINS damage's causedBy "vermin"
    OR  CONTAINS damage's causedBy "bird"

`bird damage to contents` damage MEANS
        CONTAINS damage's causedBy "bird"
    AND damage's affectsContents

`animal water escape from appliance` damage MEANS
        damage's involvesWaterEscape
    AND damage's fromAppliance
```

**Step 4: Create Test Cases**

```l4
`rodent damage not covered` MEANS Damage WITH
    causedBy           IS "rodent"
    affectsContents    IS FALSE
    involvesWaterEscape IS FALSE
    fromAppliance      IS FALSE

`bird damage to contents covered` MEANS Damage WITH
    causedBy           IS "bird"
    affectsContents    IS TRUE
    involvesWaterEscape IS FALSE
    fromAppliance      IS FALSE

#ASSERT NOT `coverage applies to` `rodent damage not covered`
#ASSERT `coverage applies to` `bird damage to contents covered`
```

**Step 5: Validate**

```bash
jl4-cli insurance-policy.l4
```

**Step 6: Generate Decision Tree**

Use L4 visualizer to create interactive decision tree for users to determine coverage.

## Tips for Success

### Start Small

Begin with simple rules before tackling complex multi-party contracts.

### Maintain Isomorphism

Keep code structure aligned with source text structure for traceability.

### Test Early and Often

Don't wait until everything is encoded—test each function as you write it.

### Use Where Clauses Liberally

Break down complex expressions into named helpers for readability.

### Leverage Functional Programming

Compose small, reusable functions rather than writing monolithic logic.

### Consult Examples

When stuck, look at working examples in `jl4/examples/ok/`.

### Read Error Messages Carefully

L4's type checker provides helpful error messages—read them completely.

## Common Pitfalls

### Pitfall 1: Ignoring the Type System

**Problem**: Trying to pass wrong types
**Solution**: Let the type checker guide you—fix type errors before testing logic

### Pitfall 2: Deeply Nested Conditionals

**Problem**: Hard to read and maintain
**Solution**: Use pattern matching (CONSIDER/WHEN) or helper functions with WHERE

### Pitfall 3: Missing Edge Cases

**Problem**: Rules work for typical cases but fail on boundaries
**Solution**: Explicitly test boundary conditions (minimum ages, maximum amounts, etc.)

### Pitfall 4: Incomplete Pattern Matches

**Problem**: Forgetting to handle all enum constructors
**Solution**: Always use OTHERWISE or handle all cases explicitly

### Pitfall 5: Not Using Isomorphic Structure

**Problem**: Code structure diverges from legal text, making maintenance hard
**Solution**: Mirror the AND/OR structure and section numbering of source documents

## Resources for Each Phase

### Phase 1 (Analysis):

- Module 1 (Enums & Records)
- Real-world legal documents

### Phase 2 (Decision Logic):

- Module 2 (Functions)
- Module 3 (Control Flow)

### Phase 3 (Modal Logic):

- Advanced Course Module A3

### Phase 4-5 (Validation & Testing):

- Module 6 (Test Data & EVAL)
- Advanced Course Module A7 (Regression Testing)

### Phase 6 (Operationalization):

- Advanced Course Module A4 (APIs)
- Advanced Course Module A6 (JSON Integration)

### Phase 7 (Maintenance):

- Advanced Course Module A8 (Multi-file Architecture)
- Git version control practices
