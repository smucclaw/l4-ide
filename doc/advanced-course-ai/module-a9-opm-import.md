# Module A9 — Importing OPM/OIA Files

## Overview

Many organizations have existing rule investments in **Oracle Policy Modeling** (OPM), part of **Oracle Intelligent Advisor** (OIA). These rules, often created in Word or Excel by subject matter experts, represent years of work encoding legislation, regulations, and business policies.

Rather than rewrite everything from scratch, you can **import existing OPM rules into L4** using the `opm2l4` translator. This module covers:

1. Understanding OPM/OIA file formats
2. Using the opm2l4 command-line tool
3. Understanding the translation mapping
4. Reviewing and refining imported code
5. Testing translated rules
6. Migration strategies

## Part 1: What is OPM/OIA?

### Oracle Policy Modeling Overview

OPM allows business analysts and legal experts to author rules in natural language using familiar tools:

**Word Documents** — Write rules in structured English:

```
The applicant is eligible if
  the applicant's age >= 18 and
  the applicant is a citizen
```

**Excel Spreadsheets** — Create decision tables:

```
| Group Code | Application Group |
|------------|------------------|
| COY0003    | AGF Group        |
| COY0004    | FM Group         |
| COY0005    | GO Group         |
```

These documents are compiled into **XGEN files** (XML format) that Oracle's Determinations Engine can execute.

### OPM Features

- **Natural language authoring** — Write rules without programming
- **Three-valued logic** — True, False, Uncertain (unknown)
- **Entity modeling** — Define data structures and relationships
- **Interactive interviews** — Auto-generated web forms
- **Temporal reasoning** — Date-based rules
- **Excel integration** — Import/export test data

### Why Migrate to L4?

| OPM/OIA               | L4                                             |
| --------------------- | ---------------------------------------------- |
| Vendor-locked         | Open source                                    |
| Proprietary runtime   | Multiple targets (Web, API, CLI)               |
| Natural language only | Code + natural language                        |
| Limited verification  | Formal verification with TLA+ style properties |
| No LLM integration    | AI-powered explanations                        |
| License costs         | Free                                           |

## Part 2: OPM File Formats

### Project Structure

An OPM project contains:

```
MyProject/
├── MyProject.xprj              # Project metadata
├── projectDataModel.xml        # Data model (entities & attributes)
├── bin/
│   ├── rules1.docx.xgen        # Compiled rules from Word
│   ├── rules2.xlsx.xgen        # Compiled rules from Excel
│   └── rulebase.xml            # Full compiled rulebase
├── rules/
│   ├── eligibility.docx        # Source Word document
│   └── calculations.xlsx       # Source Excel spreadsheet
└── interviews/
    └── application.xint        # Interview definition
```

### Key File Types

**projectDataModel.xml** — The data model

```xml
<model>
  <entity name="the applicant">
    <attributes>
      <attribute name="the applicant's age" data-type="number"/>
      <attribute name="the applicant is eligible" data-type="boolean"/>
    </attributes>
  </entity>
</model>
```

Maps to L4:

```l4
DECLARE Applicant HAS
    age IS A NUMBER
    isEligible IS A BOOLEAN
```

**XGEN files** — Compiled rules

```xml
<rule>
  <condition>
    <attribute name="the applicant's age"/>
    <operator type=">="/>
    <value>18</value>
  </condition>
  <conclusion>
    <attribute name="the applicant is eligible"/>
    <value>true</value>
  </conclusion>
</rule>
```

Maps to L4:

```l4
GIVEN applicant IS An Applicant
DECIDE `applicant is eligible`
  IF applicant's age AT LEAST 18
```

## Part 3: Installing and Using opm2l4

### Installation

The opm2l4 translator is a Node.js command-line tool:

```bash
# Clone the repository
git clone https://github.com/smucclaw/opm2l4
cd opm2l4

# Install dependencies
npm install

# Build
npm run build

# Verify installation
npx opm2l4 --help
```

### Basic Usage

**Translate an entire OPM project:**

```bash
npx opm2l4 translate "./MyProject" -o ./output
```

**Output:**

```
Translating project: /path/to/MyProject
Loaded data model: 5 entities
Found 3 XGEN files
  Parsed eligibility.docx.xgen: 8 rules
  Parsed calculations.xlsx.xgen: 12 rules
  Parsed validations.docx.xgen: 5 rules
Normalized 25 rules

Translation complete:
  Entities: 5
  Rules: 25

Output written to: ./output/MyProject.l4
```

**Validate project structure:**

```bash
npx opm2l4 validate "./MyProject"
```

**Inspect project contents:**

```bash
npx opm2l4 inspect "./MyProject" --show-entities --show-rules
```

**Translate a single XGEN file:**

```bash
npx opm2l4 translate-file "./MyProject/bin/eligibility.docx.xgen" -o eligibility.l4
```

## Part 4: Translation Mapping

### Data Types

| OPM Type    | L4 Type    | Example                 |
| ----------- | ---------- | ----------------------- |
| `boolean`   | `BOOLEAN`  | `TRUE`, `FALSE`         |
| `text`      | `STRING`   | `"Hello"`               |
| `number`    | `NUMBER`   | `42`, `3.14`            |
| `date`      | `DATE`     | `January 1 2024`        |
| `datetime`  | `DATETIME` | `"2024-01-01T14:30:00"` |
| `timeofday` | `TIME`     | `"14:30:00"`            |
| `currency`  | `NUMBER`   | `50000`                 |

### Entities and Attributes

**OPM XML:**

```xml
<entity name="the employee">
  <attributes>
    <attribute name="the employee's name" data-type="text"/>
    <attribute name="the employee's age" data-type="number"/>
    <attribute name="the employee's salary" data-type="currency"/>
  </attributes>
</entity>
```

**Generated L4:**

```l4
DECLARE Employee HAS
    name IS A STRING
    age IS A NUMBER
    salary IS A NUMBER
```

### String Values → Enumerations

OPM uses string comparisons. L4 uses type-safe enumerations.

**OPM:**

```
the application type = "EME" or
the application type = "ESE"
```

**L4:**

```l4
DECLARE ApplicationType IS ONE OF
    EME
    ESE

GIVEN applicationType IS An ApplicationType
GIVETH A BOOLEAN
`application type matches` applicationType MEANS
       applicationType EQUALS EME
    OR applicationType EQUALS ESE
```

### Boolean Rules

**OPM:**

```
the applicant is eligible if
  the applicant's age >= 18 and
  the applicant is a citizen
```

**L4:**

```l4
GIVEN applicant IS An Applicant
DECIDE `applicant is eligible`
  IF      applicant's age AT LEAST 18
     AND  applicant's isCitizen
```

### Value Rules

**OPM:**

```
the applicant's category =
  "senior" if the applicant's age >= 65
  "adult" if the applicant's age >= 18
  otherwise "minor"
```

**L4:**

```l4
GIVEN applicant IS An Applicant
GIVETH A STRING
`applicant category` applicant MEANS
    IF applicant's age AT LEAST 65
    THEN "senior"
    ELSE IF applicant's age AT LEAST 18
    THEN "adult"
    ELSE "minor"
```

### Decision Tables

**OPM Excel Table:**

```
| Salary    | Category           | Minimum Salary |
|-----------|-------------------|----------------|
| Tech      | TechProfessional  | 5000           |
| Health    | HealthcareWorker  | 4500           |
| Research  | Researcher        | 5500           |
```

**L4:**

```l4
GIVEN category IS An EmploymentCategory
GIVETH A NUMBER
`minimum salary for` category MEANS
    BRANCH IF category EQUALS TechProfessional THEN 5000
           IF ^        ^      HealthcareWorker  THEN 4500
           IF ^        ^      Researcher        THEN 5500
           OTHERWISE 3000
```

### Entity Collections

**OPM:**

```
for all the household's members
  the member's age >= 18
```

**L4:**

```l4
IMPORT prelude

GIVEN household IS A Household
GIVETH A BOOLEAN
`all members are adults` household MEANS
    all isAdult household's members
    WHERE
        isAdult member MEANS member's age AT LEAST 18
```

### Date Arithmetic

**OPM:**

```
the applicant's years of service =
  number of years between
    the applicant's start date and
    today
```

**L4:**

```l4
IMPORT daydate

GIVEN applicant IS An Applicant
      assessmentDate IS A DATE
GIVETH A NUMBER
`years of service` applicant assessmentDate MEANS
    FLOOR ((assessmentDate MINUS applicant's startDate) DIVIDED BY 365.2425)
```

## Part 5: Working with Translated Code

### Generated Code Structure

The translator generates well-organized L4 code:

```l4
-- Generated by OPM2L4 from: MyProject
-- Source files: eligibility.docx.xgen, calculations.xlsx.xgen

IMPORT prelude
IMPORT daydate

§ `Types`

DECLARE ApplicationType IS ONE OF
    EME
    ESE

DECLARE Employee HAS
    name IS A STRING
    age IS A NUMBER
    salary IS A NUMBER
    category IS An EmploymentCategory

§ `Input and Output`

-- Input: What comes from external systems
DECLARE Input HAS
    employee IS An Employee
    applicationDate IS A DATE

-- Output: What the rules compute
DECLARE Output HAS
    isEligible IS A BOOLEAN
    decisionCodes IS A LIST OF STRING

§ `Rules`

-- Rule from eligibility.docx (line 45)
GIVEN employee IS An Employee
DECIDE `employee is eligible`
  IF      employee's age AT LEAST 18
     AND  employee's salary AT LEAST 5000

-- Rule from calculations.xlsx (table "Minimum Salary")
GIVEN category IS An EmploymentCategory
GIVETH A NUMBER
`minimum salary for` category MEANS
    BRANCH IF category EQUALS TechProfessional THEN 5000
           IF ^        ^      HealthcareWorker  THEN 4500
           OTHERWISE 3000

§ `Tests`

#EVAL `employee is eligible` (Employee WITH
    name IS "Alice"
    age IS 25
    salary IS 6000
    category IS TechProfessional)
```

### Review Checklist

After translation, review the generated code for:

1. **Type correctness** — Are enums properly defined?
2. **Missing values** — OPM's "uncertain" state may need MAYBE types
3. **Name clarity** — OPM's verbose names might need shortening
4. **Logic accuracy** — Verify boolean logic is preserved
5. **Date handling** — Check date arithmetic conversions
6. **Test coverage** — Add more test cases

### Common Refinements

**Simplify verbose names:**

```l4
-- Generated (verbose)
`the application's existing group effective date`

-- Refined (concise)
`effectiveDate`
```

**Add MAYBE for optional values:**

```l4
-- Generated (assumes always present)
DECLARE Document HAS
    expiryDate IS A DATE

-- Refined (optional)
DECLARE Document HAS
    expiryDate IS A MAYBE DATE
```

**Improve enum names:**

```l4
-- Generated (string-based)
DECLARE StatusCode IS ONE OF
    RC216
    PR036

-- Refined (descriptive)
DECLARE StatusCode IS ONE OF
    RC216  -- Underage applicant
    PR036  -- RP group not eligible
```

## Part 6: Testing Translated Rules

### Generate Test Cases

OPM projects often include test cases in Excel:

```
| Employee Name | Age | Salary | Expected Result |
|---------------|-----|--------|----------------|
| Alice         | 25  | 6000   | TRUE           |
| Bob           | 17  | 5000   | FALSE          |
| Carol         | 30  | 4000   | FALSE          |
```

Convert these to L4 assertions:

```l4
§ `Test Cases`

`Alice - Adult with Good Salary` MEANS Employee WITH
    name IS "Alice"
    age IS 25
    salary IS 6000

`Bob - Underage` MEANS Employee WITH
    name IS "Bob"
    age IS 17
    salary IS 5000

`Carol - Low Salary` MEANS Employee WITH
    name IS "Carol"
    age IS 30
    salary IS 4000

#ASSERT `employee is eligible` `Alice - Adult with Good Salary`
#ASSERT NOT `employee is eligible` `Bob - Underage`
#ASSERT NOT `employee is eligible` `Carol - Low Salary`
```

### Compare OPM vs L4 Results

**Run OPM tests:**

```bash
# In Oracle Determinations Engine
> run_tests MyProject
Test 1: PASS
Test 2: PASS
Test 3: PASS
```

**Run L4 tests:**

```bash
cabal run jl4-cli -- MyProject.l4
```

If results differ, investigate:

- Three-valued logic differences (OPM's "uncertain")
- Date calculation differences
- Rounding differences in numeric calculations

### Golden Master Testing

Create a golden master file from OPM results:

```bash
# Export OPM test results
opm_export_test_results MyProject > opm-results.json

# Run L4 and compare
cabal run jl4-cli -- MyProject.l4 > l4-results.json
diff opm-results.json l4-results.json
```

## Part 7: Migration Strategies

### Strategy 1: Big Bang Migration

**When to use:**

- Small to medium projects (<100 rules)
- No ongoing OPM development
- Can afford downtime for testing

**Process:**

1. Translate entire project with opm2l4
2. Review and refine all generated code
3. Run comprehensive tests
4. Switch to L4 completely

**Timeline:** 1-2 weeks for small projects

### Strategy 2: Incremental Migration

**When to use:**

- Large projects (>100 rules)
- Active OPM development
- Need continuous operation

**Process:**

1. Identify self-contained modules
2. Translate one module at a time
3. Run both OPM and L4 in parallel
4. Gradually shift traffic to L4

**Timeline:** 2-6 months

### Strategy 3: Hybrid Operation

**When to use:**

- Very large projects
- Mission-critical systems
- Need for gradual validation

**Process:**

1. Translate to L4 but keep OPM
2. Run both engines side-by-side
3. Compare results for every transaction
4. Build confidence over time
5. Eventually retire OPM

**Timeline:** 6-12 months

### Strategy 4: New Development Only

**When to use:**

- Cannot migrate legacy rules
- Want L4 for new features only

**Process:**

1. Keep existing OPM rules
2. Develop new rules in L4
3. Integrate both systems via API
4. Gradually port high-value legacy rules

**Timeline:** Ongoing

## Part 8: Common Pitfalls and Solutions

### Pitfall 1: Three-Valued Logic

**Problem:**
OPM supports "uncertain" for unknown values. L4 uses MAYBE instead.

**OPM:**

```
the applicant is eligible = uncertain
```

**L4 Solution:**

```l4
DECLARE EligibilityResult IS ONE OF
    Eligible
    NotEligible
    Uncertain

-- Or use MAYBE:
GIVETH A MAYBE BOOLEAN
```

### Pitfall 2: Implicit Type Coercion

**Problem:**
OPM automatically converts types (e.g., number to text). L4 is strict.

**OPM:**

```
the label = "Age: " + the age  // Implicitly converts number to text
```

**L4 Solution:**

```l4
-- Explicit conversion needed
`label` MEANS "Age: " APPEND toString age
```

### Pitfall 3: Side Effects

**Problem:**
OPM rules can have side effects (set multiple values). L4 functions are pure.

**OPM:**

```
conclude the status = "approved"
conclude the approval date = today
```

**L4 Solution:**

```l4
-- Return a record with both values
DECLARE ApprovalResult HAS
    status IS A STRING
    approvalDate IS A DATE

GIVETH An ApprovalResult
approve MEANS ApprovalResult WITH
    status IS "approved"
    approvalDate IS `today`
```

### Pitfall 4: Circular Dependencies

**Problem:**
OPM allows circular rule dependencies. L4 does not (lazy evaluation prevents infinite loops, but circular value definitions are rejected).

**OPM:**

```
A depends on B
B depends on A
```

**L4 Solution:**

```l4
-- Refactor to eliminate circularity
-- Identify base cases or add explicit recursion guards
```

## Part 9: Advanced Features

### Custom Transformations

Extend the translator for organization-specific needs:

```typescript
// custom-transform.ts
import { Rule, normalizeRule } from "opm2l4";

function customNormalize(rule: Rule): L4Code {
  // Your custom logic
  // e.g., apply naming conventions, add annotations
  return normalizeRule(rule);
}
```

### Integration with Existing L4

Import OPM-translated modules into existing L4 projects:

```l4
-- main.l4
IMPORT opm-translated/eligibility
IMPORT opm-translated/calculations
IMPORT core/types  -- Your existing types

-- Mix OPM rules with hand-written L4
GIVEN employee IS An Employee
GIVETH A BOOLEAN
`comprehensive check` employee MEANS
        `employee is eligible` employee        -- From OPM
    AND `passes security clearance` employee  -- Hand-written L4
```

### Metadata Preservation

The translator preserves OPM metadata as comments:

```l4
-- @source eligibility.docx:45
-- @opm-rule-id rule-001
-- @author Jane Smith
-- @last-modified 2024-11-15
GIVEN applicant IS An Applicant
DECIDE `applicant is eligible`
  IF applicant's age AT LEAST 18
```

## Part 10: Case Study

### Singapore MOM Work Pass Rules

**Original:** 120-page Word document + 15 Excel spreadsheets in OPM

**Translation process:**

```bash
npx opm2l4 translate "./SG MOM UpdatedAPI Names" -o ./sg-mom.l4
```

**Generated:**

- 5 entity types
- 12 eligibility rules
- 4 decision tables
- 112 lines of L4 code

**Refinements:**

1. Simplified verbose attribute names
2. Added MAYBE types for optional fields
3. Consolidated redundant rules
4. Added comprehensive test cases
5. Added @desc annotations for API export

**Result:**

- Clean, maintainable L4 code
- Formal verification found 2 edge cases
- 30% reduction in total lines of code
- Full API exposure via decision service

**Before (OPM):**

```xml
<rule>
  <condition>
    <attribute name="the application's existing group code"/>
    <operator type="="/>
    <value>COY0003</value>
  </condition>
  <conclusion>
    <attribute name="the application group"/>
    <value>AGF Group</value>
  </conclusion>
</rule>
```

**After (L4):**

```l4
GIVEN groupCode IS A GroupCode
GIVETH A MAYBE ApplicationGroup
codeToGroup groupCode MEANS
    BRANCH IF groupCode EQUALS "COY0003" THEN JUST AGFGroup
           IF ^         ^      "COY0004" THEN JUST FMGroup
           OTHERWISE NOTHING
```

## Key Takeaways

1. **OPM rules can be automatically translated to L4** using the opm2l4 tool
2. **Review generated code** for type correctness and clarity
3. **Test extensively** — compare OPM and L4 results
4. **Choose the right migration strategy** based on project size and constraints
5. **Handle three-valued logic** with MAYBE types or explicit enums
6. **Preserve metadata** for traceability
7. **Refine generated code** to make it idiomatic L4
8. **Integrate with existing L4** for hybrid systems
9. **Use golden master testing** to validate translation correctness

## Exercises

### Exercise 1: Translate a Sample OPM Project

Download a sample OPM project and translate it to L4. Review the generated code and identify areas for improvement.

### Exercise 2: Test Comparison

Create a test suite in both OPM and L4. Run both and compare results. Investigate any differences.

### Exercise 3: Hybrid Integration

Create an L4 module that imports OPM-translated rules and extends them with new hand-written logic.

### Exercise 4: Migration Plan

Given a hypothetical OPM project with 500 rules across 20 modules, create a detailed migration plan with timeline and risk assessment.

## Resources

- **opm2l4 Repository:** https://github.com/smucclaw/opm2l4
- **OPM Documentation:** Oracle Policy Modeling User's Guide
- **L4 Mapping Guide:** opm2l4/docs/L4_MAPPING.md
- **Sample Projects:** opm2l4/examples/

## Next Steps

You've now completed all advanced modules! You can:

- Build production-grade L4 systems
- Integrate with external systems
- Migrate from existing rule engines
- Implement comprehensive testing
- Architect large multi-file projects

**Continue your journey:**

- Contribute to L4 open source
- Share your use cases with the community
- Explore formal verification capabilities
- Build custom domain-specific extensions
