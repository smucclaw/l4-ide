# Module 4: Decision Logic

In this module, you'll learn how to encode **constitutive rules** - legal rules that determine facts, classifications, and eligibility.

## Learning Objectives

By the end of this module, you will be able to:

- Distinguish between constitutive rules (decision logic) and regulative rules (obligations)
- Express eligibility determinations using DECIDE
- Define computations and definitions using MEANS
- Break down complex calculations with WHERE
- Write decision logic that reads like legal text

---

## What is Decision Logic?

### Constitutive vs Regulative Rules

Legal systems contain two types of rules:

**Regulative rules** (Module 1 and 5) specify what parties **must**, **may**, or **must not** do:
- "The applicant **must** submit documentation **within** 30 days"
- "The landlord **may** terminate the lease if rent is unpaid"
- "The seller **shall not** disclose confidential information"

**Constitutive rules** (this module) determine **facts**, **classifications**, and **eligibility**:
- "An applicant **is eligible** if they are a citizen and over 18"
- "The tax owed **is** 20% of gross income minus deductions"
- "A person **is a resident** if they have lived here for 183 days or more"

In L4, we express constitutive rules as **decision logic** using functions.

---

## Why Functional Purity Matters for Legal Rules

L4 follows the **functional programming** principle that **every input needed to make a decision must be explicitly declared**. This is a feature, not a bug.

### Explicit Dependencies

When you write decision logic in L4, the `GIVEN` clause declares **exactly what information is needed**:

```l4
GIVEN `the applicant` IS A Person
      `today` IS A DATE
GIVETH A BOOLEAN
DECIDE `the application is timely` IF ...
```

This signature tells you immediately: "To determine if an application is timely, I need to know **who the applicant is** and **what today's date is**."

### No Hidden Dependencies

In natural language, legal rules often have implicit dependencies:

> "The applicant is eligible if they are a resident."

**Questions immediately arise:**
- What makes someone a resident?
- How long must they have resided?
- Where must they reside?

In L4, you **must** make these dependencies explicit:

```l4
GIVEN `the applicant` IS A Person
      `the jurisdiction` IS A STRING
      `today` IS A DATE
GIVETH A BOOLEAN
DECIDE `the applicant is eligible` IF
    `the applicant is a resident of the jurisdiction`
    WHERE
        GIVEN person IS A Person
              jurisdiction IS A STRING
              currentDate IS A DATE
        GIVETH A BOOLEAN
        `the applicant is a resident of the jurisdiction` MEANS
            person's `country of residence` == jurisdiction
            AND person's `years of residence` >= 5
```

Notice how the helper decision `is a resident of` **also** declares its dependencies. You can't hide information - if a decision needs something, you must pass it explicitly.

### "It Depends" - On What, Exactly?

This explicit dependency tracking has a **huge benefit for legal reasoning**:

**When someone says "it depends"** - L4 forces you to specify **what "it" depends ON**.

```l4
-- ❌ Unclear: What does eligibility depend on?
"The person is eligible"

-- ✅ Clear: Eligibility depends on these specific factors
GIVEN `the person` IS A Person
      `the application date` IS A DATE
      `the jurisdiction rules` IS A RuleSet
GIVETH A BOOLEAN
DECIDE `the person is eligible` IF ...
```

The function signature **documents the complete set of factors** that affect the decision. This makes legal rules:
- **Auditable**: You can trace exactly what information influenced a decision
- **Testable**: You know exactly what inputs to vary in your test scenarios
- **Maintainable**: When requirements change, you know what dependencies need updating
- **Explainable**: You can show stakeholders exactly what factors matter

### Parameter Threading

If a sub-decision needs information, you must "thread" it through the caller:

```l4
GIVEN `the applicant` IS A Person
      `today` IS A DATE        -- We need this
GIVETH A BOOLEAN
DECIDE `qualifies for benefit` IF
    `is eligible` `the applicant` `today`  -- Pass it through!
    AND ...
    WHERE
        GIVEN person IS A Person
              currentDate IS A DATE  -- Sub-decision needs it too!
        GIVETH A BOOLEAN
        `is eligible` MEANS ...
```

This might seem verbose, but it's **transparency, not bureaucracy**. Every piece of information that influences the legal outcome is **visible and traceable**.

---

## Working Examples

All examples in this module are in: [module-4-examples.l4](module-4-examples.l4)

---

## Basic Decisions with DECIDE

### Eligibility Determinations

The most common legal decision: "Does X qualify?"

```l4
GIVEN `the applicant` IS A Person
GIVETH A BOOLEAN
DECIDE `the applicant is eligible for benefits` IF
    `the applicant is a citizen`
    AND `the applicant's age` >= 18
    AND NOT `the applicant is disqualified`
```

**Key points:**
- Use **DECIDE IF** for yes/no questions
- Use backticks for natural language identifiers
- Conditions follow IF, connected by AND/OR/NOT

### Classification Rules

"What category does X fall into?"

Use `BRANCH` for multi-way classification decisions:

```l4
GIVEN `the income` IS A NUMBER
GIVETH A STRING
DECIDE `the tax bracket` IS
    BRANCH
        IF `the income` < 10000 THEN "low"
        IF `the income` < 50000 THEN "medium"
        OTHERWISE "high"
```

**Note:** `BRANCH` is clearer than nested `IF/THEN/ELSE` for classification decisions. It avoids indentation problems and reads more like a legal test with multiple conditions. See [Module 3: Control Flow](module-3-control-flow.md#multi-way-decisions-with-branch) for details.

### DECIDE IS vs DECIDE IF

Use whichever reads more naturally:

```l4
-- These are equivalent:
DECIDE `the person is an adult` IF age >= 18
DECIDE `the person is an adult` IS age >= 18

-- Use IF when it reads like a legal condition:
DECIDE `qualifies for exemption` IF
    `is a first-time buyer`
    AND `purchase price` < 500000

-- Use IS with BRANCH for multi-way classification:
DECIDE `the applicable rate` IS
    BRANCH
        IF `customer type` = "premium" THEN 0.05
        IF `customer type` = "standard" THEN 0.10
        OTHERWISE 0.15
```

---

## Computations with MEANS

### Defining Calculations

Use **MEANS** to define computed values and formulas:

```l4
GIVEN `the gross income` IS A NUMBER
      `the deductions` IS A NUMBER
GIVETH A NUMBER
`the taxable income` MEANS
    `the gross income` - `the deductions`
```

### Simple Tax Calculation

```l4
GIVEN `the taxable income` IS A NUMBER
GIVETH A NUMBER
`the tax owed` MEANS
    `the taxable income` * `the tax rate`
```

### DECIDE vs MEANS

Both define things - use whichever reads better:

| Use DECIDE when...                | Use MEANS when...                 |
| --------------------------------- | --------------------------------- |
| Asking a yes/no question          | Defining a value or computation   |
| Determining classification/status | Stating what something equals     |
| Condition reads naturally with IF | Definition reads naturally with = |

**Examples:**

```l4
-- ✅ Good: DECIDE for questions
DECIDE `the person is eligible` IF age >= 18

-- ✅ Good: MEANS for definitions
`the person's status` MEANS
    IF age >= 18 THEN "adult" ELSE "minor"

-- Both work, but one reads better:
DECIDE `the person is an adult` IF age >= 18           -- More natural
`the person is an adult` MEANS age >= 18                -- Also valid

`the net income` MEANS `gross income` - `expenses`     -- More natural
DECIDE `the net income` IS `gross income` - `expenses` -- Also valid
```

---

## Breaking Down Complex Logic with WHERE

### Local Helper Calculations

Legal formulas often involve intermediate calculations. Use **WHERE** to break them down:

```l4
GIVEN `the principal` IS A NUMBER
      `the annual rate` IS A NUMBER
      `the years` IS A NUMBER
GIVETH A NUMBER
`the compound interest` MEANS
    `the principal` * (`the growth factor` ^ `the years`)
    WHERE
        `the growth factor` MEANS 1 + `the annual rate`
```

**Benefits:**
- Makes complex formulas readable
- Gives meaningful names to intermediate values
- Matches how legal documents explain calculations

### Multiple WHERE Definitions

```l4
GIVEN `the loan amount` IS A NUMBER
      `the annual rate` IS A NUMBER
      `the term in months` IS A NUMBER
GIVETH A NUMBER
`the monthly payment` MEANS
    `the loan amount` *
    (`the monthly rate` * `the compound factor`) /
    (`the compound factor` - 1)
    WHERE
        `the monthly rate` MEANS `the annual rate` / 12
        `the compound factor` MEANS (1 + `the monthly rate`) ^ `the term in months`
```

---

## Realistic Legal Scenarios

### Example 1: Benefit Eligibility

```l4
DECLARE Person HAS
    name IS A STRING
    age IS A NUMBER
    citizenship IS A STRING
    `criminal record` IS A BOOLEAN
    `years of residence` IS A NUMBER

GIVEN `the applicant` IS A Person
GIVETH A BOOLEAN
DECIDE `the applicant is eligible for housing benefit` IF
    `the applicant is a qualifying resident`
    AND `the applicant is of age`
    AND NOT `the applicant is disqualified`
    WHERE
        `the applicant is a qualifying resident` MEANS
            `the applicant`'s citizenship == "citizen"
            OR `the applicant`'s `years of residence` >= 5

        `the applicant is of age` MEANS
            `the applicant`'s age >= 21

        `the applicant is disqualified` MEANS
            `the applicant`'s `criminal record`
```

**Notice:**
- Natural language identifiers with backticks
- WHERE breaks down the eligibility logic
- Each condition has a clear name
- Logic mirrors how legislation is written

### Example 2: Progressive Tax Calculation

```l4
GIVEN `the income` IS A NUMBER
GIVETH A NUMBER
`the income tax owed` MEANS
    `tax on first bracket` + `tax on second bracket` + `tax on third bracket`
    WHERE
        `tax on first bracket` MEANS
            `the amount in first bracket` * 0.10

        `tax on second bracket` MEANS
            `the amount in second bracket` * 0.20

        `tax on third bracket` MEANS
            `the amount in third bracket` * 0.30

        `the amount in first bracket` MEANS
            IF `the income` <= 10000
            THEN `the income`
            ELSE 10000

        `the amount in second bracket` MEANS
            IF `the income` <= 10000
            THEN 0
            ELSE IF `the income` <= 50000
            THEN `the income` - 10000
            ELSE 40000

        `the amount in third bracket` MEANS
            IF `the income` <= 50000
            THEN 0
            ELSE `the income` - 50000
```

### Example 3: Contract Clause Interpretation

```l4
DECLARE Contract HAS
    `effective date` IS A DATE
    `termination date` IS A MAYBE DATE
    `notice period in days` IS A NUMBER

DECLARE Party HAS
    name IS A STRING
    `notice given on` IS A MAYBE DATE

GIVEN `the contract` IS A Contract
      `the party` IS A Party
      `today` IS A DATE
GIVETH A BOOLEAN
DECIDE `the party may terminate` IF
    `the contract is active`
    AND `the party has given sufficient notice`
    WHERE
        `the contract is active` MEANS
            `today` >= `the contract`'s `effective date`
            AND CONSIDER `the contract`'s `termination date`
                WHEN Nothing THEN True
                WHEN Just d THEN `today` < d

        `the party has given sufficient notice` MEANS
            CONSIDER `the party`'s `notice given on`
            WHEN Nothing THEN False
            WHEN Just noticeDate THEN
                `days since notice` >= `the contract`'s `notice period in days`
                WHERE
                    `days since notice` MEANS `today` - noticeDate
```

---

## Function Signatures

Every decision or computation needs a **type signature** that declares:
1. **Inputs** (GIVEN): What information is needed
2. **Output** (GIVETH): What type of result is produced

### Common Patterns

**Boolean decisions** (yes/no questions):
```l4
GIVEN `the person` IS A Person
GIVETH A BOOLEAN
DECIDE `the person is eligible` IF ...
```

**Classification** (categorizing into types):
```l4
GIVEN `the entity` IS AN Entity
GIVETH A STRING
DECIDE `the entity type` IS ...
```

**Computation** (calculating a value):
```l4
GIVEN `the income` IS A NUMBER
GIVETH A NUMBER
`the tax owed` MEANS ...
```

**Multiple inputs**:
```l4
GIVEN `the applicant` IS A Person
      `the application date` IS A DATE
      `today` IS A DATE
GIVETH A BOOLEAN
DECIDE `the application is timely` IF ...
```

---

## Exercises

### Exercise 1: Simple Eligibility

Write a decision that determines if a person qualifies for a senior discount (age 65 or older):

```l4
GIVEN `the person's age` IS A NUMBER
GIVETH A BOOLEAN
DECIDE `qualifies for senior discount` IF
    -- Your code here
```

### Exercise 2: Multi-Condition Eligibility

A person qualifies for a student loan if they:
- Are between 18 and 35 years old
- Are enrolled in an accredited institution
- Have no prior loan defaults

Write the decision logic.

### Exercise 3: Tax Bracket Classification

Write a decision that classifies income into tax brackets:
- Income < $10,000: "exempt"
- Income $10,000-$50,000: "standard"
- Income > $50,000: "higher rate"

### Exercise 4: Calculation with WHERE

Write a computation for net income that:
- Starts with gross income
- Subtracts standard deduction (calculated as 10% of gross, minimum $1000)
- Subtracts itemized deductions

Use WHERE to break down the calculation clearly.

---

## Common Mistakes

### 1. Missing Type Signature

```l4
-- ❌ Wrong: No GIVETH
GIVEN `the person's age` IS A NUMBER
DECIDE `is adult` IF `the person's age` >= 18

-- ✅ Right: Include GIVETH
GIVEN `the person's age` IS A NUMBER
GIVETH A BOOLEAN
DECIDE `is adult` IF `the person's age` >= 18
```

### 2. Using Programmer Names

```l4
-- ❌ Wrong: Programmer style
GIVEN p IS A Person
GIVETH A BOOLEAN
isEligible p MEANS p.age >= 18 && !p.disqualified

-- ✅ Right: Natural language
GIVEN `the person` IS A Person
GIVETH A BOOLEAN
DECIDE `the person is eligible` IF
    `the person`'s age >= 18
    AND NOT `the person`'s disqualified
```

### 3. Complex Logic Without WHERE

```l4
-- ❌ Wrong: Everything inline, hard to read
`the result` MEANS
    (x * 0.1 + y * 0.2) / (1 + r)^n

-- ✅ Right: Break down with WHERE
`the result` MEANS
    `the combined amount` / `the discount factor`
    WHERE
        `the combined amount` MEANS x * 0.1 + y * 0.2
        `the discount factor` MEANS (1 + r)^n
```

### 4. Wrong DECIDE vs MEANS Choice

```l4
-- ❌ Awkward: MEANS for a yes/no question
`is eligible` MEANS age >= 18 AND income < 50000

-- ✅ Better: DECIDE IF for questions
DECIDE `is eligible` IF
    age >= 18
    AND income < 50000

-- ❌ Awkward: DECIDE for a simple definition
DECIDE `the net amount` IS gross - deductions

-- ✅ Better: MEANS for definitions
`the net amount` MEANS gross - deductions
```

### 5. Nested IF/THEN/ELSE Instead of BRANCH

```l4
-- ❌ Fragile: Nested IF/THEN/ELSE (indentation-sensitive)
`the category` MEANS
    IF score >= 90 THEN "excellent"
    ELSE IF score >= 70 THEN "good"
         ELSE IF score >= 50 THEN "pass"
              ELSE "fail"

-- ✅ Better: BRANCH (clear, flat structure)
`the category` MEANS
    BRANCH
        IF score >= 90 THEN "excellent"
        IF score >= 70 THEN "good"
        IF score >= 50 THEN "pass"
        OTHERWISE "fail"
```

---

## Summary

| Concept              | Use for                           | Syntax                                   |
| -------------------- | --------------------------------- | ---------------------------------------- |
| DECIDE IF            | Yes/no questions, eligibility     | `DECIDE name IF condition`               |
| DECIDE IS            | Classification, value assignment  | `DECIDE name IS expression`              |
| MEANS                | Definitions, computations         | `name MEANS expression`                  |
| BRANCH               | Multi-way classification          | `BRANCH IF cond1 THEN val1 ... OTHERWISE` |
| WHERE                | Breaking down complex logic       | `expression WHERE helpers`               |
| GIVEN ... GIVETH ... | Type signature (always required!) | `GIVEN inputs GIVETH OutputType name...` |

**Key principles:**
- Write code that reads like legal text
- Use backticks liberally for natural language
- Use WHERE to make complex logic transparent
- Choose DECIDE vs MEANS based on readability

---

## What's Next?

In [Module 5: Regulative Rules](module-5-regulative.md), you'll learn how to combine constitutive rules (decision logic) with regulative rules (obligations, permissions, prohibitions) to model complete legal workflows.

You'll see how decision logic determines **when** obligations apply:

```l4
-- Decision logic determines eligibility
DECIDE `the person is eligible` IF ...

-- Regulative rule creates obligation based on decision
PARTY `the government`
MUST `pay the benefit`
TO `the person`
IF `the person is eligible`
```
