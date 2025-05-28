# **L4: A Learning Guide for Legal Professionals**

## **Introduction**

L4 is a programming language designed specifically for translating legal documents into precise, executable code. This guide will take you step-by-step from writing your first legal rule to modeling complete regulatory schemes like the Jersey Charities Law.

**What You'll Learn:**
- How to write legal rules that a computer can understand and execute
- How to model complex legal relationships and processes
- How to test and simulate legal scenarios
- How to organize and maintain large bodies of legislation

**Prerequisites:** 
- Basic familiarity with legal concepts
- No programming experience required (we'll teach you what you need)

---

# **Part 1: Your First Legal Rule**

Let's start with something every lawyer understands: a simple legal obligation.

## **1.1 A Single Legal Obligation**

**Example:** "A registered charity must file an annual return."

In L4, we write this as:

```l4
GIVEN charity IS A RegisteredCharity
PARTY charity
MUST `file annual return`
```

Let's break this down:
- `GIVEN charity IS A RegisteredCharity` - This rule applies to registered charities
- `PARTY charity` - The charity is the one with the obligation  
- `MUST` - This creates a legal obligation
- `file annual return` - This is what they must do

**Try it yourself:** Write a rule that says "A solicitor must maintain client confidentiality."

<details>
<summary>Answer</summary>

```l4
GIVEN solicitor IS A Solicitor
PARTY solicitor  
MUST `maintain client confidentiality`
```
</details>

## **1.2 Adding Conditions and Consequences**

Real legal rules have conditions and consequences. Let's make our charity rule more realistic:

```l4
GIVEN charity IS A RegisteredCharity
IF charity IS registered
PARTY charity
MUST `file annual return`
WITHIN 2 months OF `end of financial year`
HENCE `compliance maintained`
LEST `Commissioner may issue Required Steps Notice`
```

New elements:
- `IF charity IS registered` - Adds a condition
- `WITHIN 2 months OF` - Sets a deadline
- `HENCE` - What happens if they comply
- `LEST` - What happens if they don't comply

**The Legal Logic:** This captures the complete legal structure: who, what, when, and consequences.

## **1.3 Your First Simulation**

Let's test our rule with some data:

```l4
-- Create a test charity
testCharity MEANS RegisteredCharity WITH
    name IS "Animal Welfare Society"
    registrationDate IS Date OF 2020, 1, 15
    financialYearEnd IS Date OF 2023, 12, 31

-- Test the rule
#EVAL `must file annual return by` testCharity
```

**What This Does:** L4 can calculate that this charity must file by February 28, 2024 (2 months after December 31, 2023).

**Success Check:** You can now write a basic legal obligation, add conditions and deadlines, and test it with data.

---

# **Part 2: Legal Entities and Relationships**

## **2.1 From Strings to Structured Types**

**Problem:** Using simple text leads to errors and ambiguity.

**Bad approach:**
```l4
charity MEANS "Some Charity Name"  -- Just text, no structure
```

**Better approach - Structured Types:**
```l4
DECLARE RegisteredCharity
    HAS name IS A STRING
        registrationNumber IS A STRING  
        registrationDate IS A Date
        address IS A STRING
        purposes IS A LIST OF Purpose
```

**Why This Matters:**
- **Prevents errors:** Can't accidentally use a person where you need a charity
- **Captures relationships:** Links charities to their purposes, addresses, etc.
- **Enables validation:** L4 can check if all required information is present

**Example:**
```l4
animalCharity MEANS RegisteredCharity WITH
    name IS "Jersey Animal Welfare"
    registrationNumber IS "CH001"
    registrationDate IS Date OF 2020, 1, 15
    address IS "St. Helier, Jersey"
    purposes IS LIST `advancement of animal welfare`
```

## **2.2 Enumerating Legal Categories**

**Legal Insight:** Law often provides specific lists and categories.

Instead of treating charitable purposes as free text:
```l4
purposes IS A LIST OF STRING  -- Allows typos and inconsistencies
```

Use precise legal categories:
```l4
DECLARE Purpose IS ONE OF
    `prevention or relief of poverty`
    `advancement of education` 
    `advancement of religion`
    `advancement of health`
    `advancement of animal welfare`
    -- ... other statutory purposes
    otherPurpose HAS description IS A STRING  -- For edge cases
```

**Benefits:**
- **Matches legal structure:** Mirrors how legislation actually defines categories
- **Prevents typos:** Can't accidentally write "education advancement" instead of "advancement of education"
- **Enables legal tests:** Can easily check if all purposes are charitable

**Pattern matching with legal categories:**
```l4
GIVEN p IS A Purpose
GIVETH A BOOLEAN
DECIDE `is charitable purpose` IF
    CONSIDER p
    WHEN `advancement of education` THEN TRUE
    WHEN `advancement of animal welfare` THEN TRUE
    WHEN `prevention or relief of poverty` THEN TRUE
    WHEN otherPurpose s THEN `is analogous to charitable purpose` p
```

## **2.3 Connecting Multiple Entities**

Legal systems involve relationships between multiple entities:

```l4
DECLARE Person
    HAS name IS A STRING
        address IS A STRING
        isGovernor IS A BOOLEAN

DECLARE RegisteredCharity  
    HAS name IS A STRING
        registrationNumber IS A STRING
        governors IS A LIST OF Person  -- Relationship to people
        address IS A STRING
        purposes IS A LIST OF Purpose  -- Relationship to purposes
```

**Real legal rule using relationships:**
```l4
GIVEN governor IS A Person
      charity IS A RegisteredCharity
IF governor's isGovernor EQUALS TRUE
   AND elem governor charity's governors  -- Check the relationship
PARTY governor
MUST `act in best interests of charity and beneficiaries`
```

**Success Check:** You can now model structured legal entities, use proper legal categories, and express relationships between entities.

---

# **Part 3: Multi-Step Legal Processes**

## **3.1 Beyond Single Rules: The PROVISION Approach**

**Problem:** Real legal processes involve multiple connected steps, not isolated rules.

**Example Process:** Charity registration involves application → assessment → decision → registration → ongoing compliance.

**Traditional approach (limited):**
```l4
PARTY applicant MUST `submit application`
PARTY Commissioner MUST `assess application`  
PARTY Commissioner MUST `make decision`
-- These are disconnected rules
```

**PROVISION approach (powerful):**
```l4
§ `Charity Registration Process`
GIVEN applicant IS A CharityApplication
GIVETH A PROVISION Actor Action
`registration process` MEANS
  PARTY Applicant OF applicant
  MUST ProvideApplication OF applicationContents
  WITHIN reasonable time
  HENCE `Commissioner must assess` applicant
  LEST `application incomplete`
```

**Key insight:** The `HENCE` creates a connection to the next step in the process.

## **3.2 Actor/Action Patterns**

**Structure who can do what:**

```l4
DECLARE Actor IS ONE OF
   Charity HAS entry IS A RegisteredCharity
   Person HAS person IS A Person
   Commissioner  -- Regulatory authority
   Applicant HAS application IS A CharityApplication

DECLARE Action IS ONE OF
   ProvideApplication HAS contents IS A ApplicationContents
   AssessApplication HAS application IS A CharityApplication
   RegisterCharity HAS charity IS A RegisteredCharity
   IssueNotice HAS notice IS A Notice
```

**Why This Works:**
- **Legal precision:** Matches how law defines roles and powers
- **Prevents errors:** A charity can't issue regulatory notices
- **Clear authority:** Shows who has power to do what

**Example using structured actors/actions:**
```l4
GIVETH A PROVISION Actor Action
`registration assessment` MEANS
  PARTY Commissioner
  MUST AssessApplication OF application
  WITHIN 28 days
  HENCE RegisterCharity OF newCharity
  LEST `refuse registration with reasons`
```

## **3.3 State Changes and Register Events**

**Legal processes change official records.** We need to model how actions update the register:

```l4
DECLARE CharityRegister
    HAS activeCharities IS A LIST OF RegisteredCharity
        historicCharities IS A LIST OF RegisteredCharity
        lastUpdated IS A Date

-- Show how registration changes the register
GIVEN newCharity IS A RegisteredCharity
      register IS A CharityRegister
      date IS A Date
GIVETH A CharityRegister
DECIDE `add charity to register` IS
    CharityRegister WITH
        activeCharities IS register's activeCharities PLUS newCharity
        lastUpdated IS date
```

**Complete process with state changes:**
```l4
GIVETH A PROVISION Actor Action  
`complete registration` MEANS
  PARTY Commissioner
  MUST RegisterCharity OF newCharity
  HENCE `update register` newRegister newCharity
  WHERE newRegister MEANS `add charity to register` newCharity currentRegister today
```

**Success Check:** You can now model connected legal processes, structured authority relationships, and state changes to official records.

---

# **Part 4: Real Regulatory Scheme - Jersey Charities Law**

Now we'll build a complete model of real legislation step by step.

## **4.1 The Three Layers Architecture**

**Real legislation has three types of provisions:**

1. **Structural/Interpretive Layer:** Definitions, institutions, basic concepts
2. **Deontic Rules Layer:** Obligations, powers, prohibitions  
3. **Register/State-Transition Layer:** How actions change official records

**Jersey Charities Example:**

**Layer 1 - Structural:**
```l4
-- Basic definitions (Art 1-2)
DECLARE Purpose IS ONE OF
    `prevention or relief of poverty`
    `advancement of education`
    -- ... 13 statutory purposes from Art 6(2)

-- Institutions (Art 3-4)  
Commissioner  -- Created by the Law
CharityRegister  -- Official register maintained by Commissioner
```

**Layer 2 - Deontic Rules:**
```l4
-- Annual return obligation (Art 13)
§ `Annual Return Obligation`
GIVETH A PROVISION Actor Action
`annual return process` MEANS
  PARTY RegisteredCharity  
  MUST FileReturn OF financialData
  WITHIN 2 months OF `financial year end`
  HENCE `transparency maintained`
  LEST `Commissioner may issue Required Steps Notice`
```

**Layer 3 - Register Events:**
```l4
-- How filing updates the register
DECLARE RegisterAction IS ONE OF
    AnnualReturnLogged HAS charity IS A RegisteredCharity
                          financials IS A FinancialData
                          date IS A Date
```

## **4.2 Building Your Register**

**Start with an empty register:**
```l4
emptyRegister MEANS CharityRegister 
    WITH activeCharities IS EMPTY_LIST
         historicCharities IS EMPTY_LIST
         nextRegistrationNumber IS 1
         lastUpdated IS commencement
```

**Add registration process:**
```l4
§ `Charity Registration (Art 11)`
GIVETH A PROVISION Actor Action
`registration process` MEANS
  PARTY Applicant OF application
  MUST ProvideApplication OF requiredDocuments
  WITHIN reasonable time
  HENCE `Commissioner assessment process` application
```

**Show the complete flow:**
```l4
#PROVISION `Complete Registration` applicant Commissioner emptyRegister AT 1 WITH
  -- Step 1: Application
  PARTY applicant DOES ProvideApplication OF applicationDocs AT 10
  
  -- Step 2: Assessment  
  PARTY Commissioner DOES AssessApplication OF application AT 20
  
  -- Step 3: Registration
  PARTY Commissioner DOES RegisterCharity OF newCharity AT 30
```

## **4.3 Enforcement and Sanctions**

**Real law includes enforcement mechanisms:**

```l4
-- Governor misconduct (Art 19-20)
§ `Governor Misconduct Process`
GIVETH A PROVISION Actor Action
`misconduct response` MEANS
  IF `misconduct discovered`
  THEN PARTY Commissioner
       MAY SuspendGovernor OF governor, reason, period
       HENCE `disciplinary action recorded`
       LEST `misconduct unaddressed`

-- Required Steps Notice (Art 27)
§ `Required Steps Notice Power`  
GIVETH A PROVISION Actor Action
`enforcement escalation` MEANS
  IF `compliance failure detected`
  THEN PARTY Commissioner
       MAY IssueNotice OF requiredSteps, deadline
       HENCE `charity must comply or face deregistration`
```

## **4.4 Bringing It All Together**

**Complete charity lifecycle simulation:**

```l4
#PROVISION `Complete Charity Lifecycle` charity Commissioner AT 1 WITH
  -- Registration
  PARTY charity DOES ProvideApplication OF docs AT 10
  PARTY Commissioner DOES RegisterCharity OF charity AT 20
  
  -- Ongoing compliance
  PARTY charity DOES FileReturn OF year1Financials AT 365
  PARTY charity DOES FileReturn OF year2Financials AT 730
  
  -- Enforcement
  PARTY Commissioner DOES `discover non-compliance` AT 800
  PARTY Commissioner DOES IssueNotice OF requiredSteps AT 810
  PARTY charity DOES `comply with required steps` AT 830
  
  -- Voluntary deregistration
  PARTY charity DOES `request deregistration` AT 1000
  PARTY Commissioner DOES `move to historic register` AT 1010
```

**Success Check:** You have modeled a complete regulatory scheme with registration, ongoing compliance, enforcement, and deregistration.

---

# **Part 5: Understanding the Why**

## **5.1 Why Type Safety Matters for Law**

**The Problem:** Legal drafting often suffers from ambiguity and inconsistency.

**Example ambiguity:**
> "The charity must notify the person responsible within 30 days."

**Questions arise:**
- Which person? The governor? The secretary? The Commissioner?
- 30 days from what event?
- What form should the notification take?

**L4's type safety prevents these problems:**

```l4
DECLARE NotificationTarget IS ONE OF
    Governor HAS name IS A STRING
    Commissioner
    Secretary HAS name IS A STRING

DECLARE NotificationEvent IS ONE OF  
    GovernorAppointment HAS date IS A Date
    ConvictionReported HAS convictionDate IS A Date
    AddressChange HAS changeDate IS A Date

PARTY charity
MUST NotifyPerson OF Commissioner, notificationContent
WITHIN 30 days OF event's date
```

**Benefits:**
- **Eliminates ambiguity:** Exactly which person, exactly which event
- **Prevents inconsistency:** Can't accidentally use wrong notification type
- **Enables automation:** Computer can check compliance automatically

## **5.2 Why PROVISION Over Simple Rules**

**Traditional legal drafting:** Each section states an isolated obligation.

**Problem:** Real legal processes are interconnected workflows, not isolated duties.

**Example - Annual Return Process:**
- Art 13(7): Charity must file return
- Art 13(8): Commissioner must publish data  
- Art 27(1): Commissioner may issue Required Steps Notice for non-compliance
- Art 16(1): Commissioner may deregister for continued non-compliance

**These are connected steps in a process, not separate obligations.**

**PROVISION approach captures the connections:**

```l4
§ `Annual Return Workflow`
GIVETH A PROVISION Actor Action
`annual return process` MEANS
  PARTY RegisteredCharity
  MUST FileReturn OF financials
  WITHIN 2 months OF `year end`
  HENCE `Commissioner publishes data` AND `compliance maintained`
  LEST `Commissioner empowered to issue Required Steps Notice`
       WHICH MAY LEAD TO `deregistration proceedings`
```

**Benefits:**
- **Captures legal reality:** Shows how legal processes actually work
- **Enables simulation:** Can test complete workflows, not just isolated rules
- **Supports drafting:** Reveals gaps and inconsistencies in legal design

## **5.3 Why Modular Architecture Works**

**Legal reality:** Modern regulation involves primary law + multiple subsidiary instruments.

**Jersey Charities Example:**
- **Primary Law:** Charities (Jersey) Law 2014
- **Core Financial Info Regs 2018:** Defines financial reporting requirements
- **Additional Info Order 2018:** Adds governor payment reporting
- **Timing Order 2019:** Sets annual return deadlines
- **Reasons and Time Limits Order 2020:** Appeals procedures

**L4 modular approach:**

```l4
-- Main legislation file  
IMPORT prelude
IMPORT `charities-types`     -- Shared definitions
IMPORT `annual-returns`      -- Specific processes

-- Types file (charities-types.l4)
DECLARE RegisteredCharity HAS ...
DECLARE FinancialData HAS ...

-- Process file (annual-returns.l4)
IMPORT `charities-types`
§ `Annual Return Process (Art 13 + multiple orders)`
-- Uses shared types, implements specific process
```

**Benefits:**
- **Mirrors legal structure:** Matches how law is actually organized
- **Supports collaboration:** Different teams can work on different modules
- **Enables updates:** Can amend subsidiary regulations without changing primary law
- **Reduces complexity:** Each file focuses on its specific domain

## **5.4 Common Patterns in Regulatory Law**

**Recognition Patterns** (registration, licensing):
- Application → Assessment → Decision → Registration → Ongoing compliance

**Ongoing Compliance Patterns** (reporting, inspection):
- Periodic obligations → Monitoring → Non-compliance → Enforcement → Resolution

**Enforcement Patterns** (sanctions, appeals):
- Detection → Investigation → Decision → Sanction → Appeal → Final resolution

**These patterns appear across different regulatory domains** (charities, financial services, professional regulation, etc.)

**L4 captures these patterns consistently:**

```l4
-- Generic enforcement pattern
DECLARE EnforcementAction IS ONE OF
    Warning HAS issueDate IS A Date
    Fine HAS amount IS A Money
    Suspension HAS period IS A Duration  
    Revocation HAS effectiveDate IS A Date
```

---

# **Part 6: Advanced Techniques** 

## **6.1 Cross-Cutting Concerns**

**Problem:** Some legal concepts (like appeals) apply across multiple processes.

**Example:** Appeals can be made against:
- Registration refusal (Art 33)
- Required Steps Notice (Art 33)  
- Deregistration decision (Art 33)
- Name change refusal (Art 33)

**Solution - Generic appeal mechanism:**

```l4
DECLARE AppealableDecision IS ONE OF
    RegistrationRefusal HAS reasons IS A LIST OF STRING
    RequiredStepsNotice HAS steps IS A LIST OF STRING
    DeregistrationDecision HAS grounds IS A LIST OF STRING
    NameChangeRefusal HAS reasons IS A LIST OF STRING

§ `Universal Appeal Right (Art 33)`
GIVEN decision IS AN AppealableDecision
GIVETH A PROVISION Actor Action  
`appeal process` MEANS
  PARTY AffectedParty
  MAY AppealDecision OF decision
  WITHIN 28 days OF `decision notice served`
  HENCE `appeal proceeds to tribunal`
  LEST `appeal time expired`
```

## **6.2 Multi-Instrument Integration**

**Real example - Annual Return Requirements:**

Combines requirements from 4 different instruments:
- **Art 13(7)-(10) Law 2014:** Basic obligation
- **Core Info Regs 2018:** Financial data requirements  
- **Additional Info Order 2018:** Governor payment details
- **Timing Order 2019:** Deadline specifications

**L4 integration approach:**

**Step 1:** Define the complex data structure outside the PROVISION:
```l4
-- Define integrated financial data structure first
integratedFinancialData MEANS IntegratedFinancialData WITH
         coreFinancialInfo IS TRUE,      -- Core Info Regs 2018
         governorPayments IS TRUE,       -- Additional Info Order 2018
         publicBenefitNarrative IS TRUE  -- Additional Info Order 2018
```

**Step 2:** Reference it in the PROVISION rule:
```l4
§ `Integrated Annual Return (4 instruments)`
GIVETH A PROVISION Actor Action
`comprehensive annual return` MEANS
  PARTY RegisteredCharity
  MUST FileReturn OF integratedFinancialData
  WITHIN 2 months OF `financial year end`  -- Timing Order 2019
  HENCE `full transparency achieved`        -- Purpose of Art 13 Law 2014
  LEST `lateFlag set + Required Steps Notice enabled`
```

**Key insight:** Keep PROVISION actions simple by pre-defining complex objects rather than constructing them inline.

## **6.2a PROVISION Syntax Best Practices**

**Critical Rule:** Avoid complex object construction within PROVISION rules.

**❌ Problematic (causes syntax errors):**
```l4
PARTY applicant
MUST ProvideApplication OF ApplicationContents WITH
       constitution IS applicant's constitution,
       purposes IS applicant's purposes,
       coreFinancialInfo IS CoreFinancialInfo WITH
         income IS Money OF 0 "GBP",
         expenditure IS Money OF 0 "GBP"
```

**✅ Correct approach - Use helper functions:**
```l4
-- Define helper function outside PROVISION
GIVEN applicant IS A RegisterEntry
GIVETH AN ApplicationContents
DECIDE `build application contents` IS
  ApplicationContents WITH
    constitution IS applicant's constitution,
    purposes IS applicant's purposes,
    coreFinancialInfo IS CoreFinancialInfo WITH
      income IS Money OF 0 "GBP",
      expenditure IS Money OF 0 "GBP"

-- Use simple reference in PROVISION
PARTY applicant
MUST ProvideApplication OF `build application contents` applicant
```

**✅ Alternative - Pre-define examples:**
```l4
-- Define complete example outside PROVISION
exampleApplication MEANS ApplicationContents WITH
    constitution IS standardConstitution,
    purposes IS LIST `advancement of education`,
    coreFinancialInfo IS standardFinancials

-- Reference directly in PROVISION
PARTY applicant
MUST ProvideApplication OF exampleApplication
```

**Why this matters:**
- **Syntax requirements:** L4 parser expects simple expressions in PROVISION actions
- **Readability:** Complex object construction clutters the legal logic
- **Maintainability:** Helper functions can be reused across multiple PROVISION rules
- **Testing:** Pre-defined objects are easier to validate and test

**Pattern for actions with structured data:**
1. **Define the structure** using `DECLARE`
2. **Create helper functions** or examples using `DECIDE` or `MEANS`
3. **Use simple references** in PROVISION rules
4. **Keep legal logic clean** and focus on the process flow

## **6.3 Advanced Simulations**

**Multi-party scenario with error handling:**

```l4
#PROVISION `Complex Misconduct Scenario` charity Commissioner governor tribunal AT 1 WITH
  -- Discovery phase
  PARTY Commissioner DOES `discover potential misconduct` governor AT 10
  PARTY Commissioner DOES `investigate allegations` AT 20
  
  -- Decision phase  
  PARTY Commissioner DOES SuspendGovernor OF governor, "mismanagement", (Date OF 2024, 12, 31) AT 30
  
  -- Charity response (may fail)
  PARTY charity DOES `attempt to ignore suspension` AT 40  -- Error condition
  PARTY Commissioner DOES `detect non-compliance by charity` AT 50
  
  -- Escalation
  PARTY Commissioner DOES IssueNotice OF requiredSteps AT 60
  PARTY charity DOES `comply with required steps` AT 70
  
  -- Appeal
  PARTY governor DOES AppealDecision OF suspensionOrder AT 80
  PARTY tribunal DOES `hear appeal and vary order` AT 120
  
  -- Resolution
  PARTY Commissioner DOES VaryOrder OF originalSuspension, newConditions AT 130
```

**Performance considerations for large schemes:**

```l4
-- Efficient register lookups
ASSUME `charity by registration number` IS A FUNCTION FROM STRING TO MAYBE RegisteredCharity
ASSUME `active governors for charity` IS A FUNCTION FROM STRING TO LIST OF Person

-- Bulk operations for large datasets
DECLARE BulkAction IS ONE OF
    BulkAnnualReturnReminder HAS charities IS A LIST OF STRING
    BulkComplianceCheck HAS cutoffDate IS A Date
```

**Success Check:** You can now handle cross-cutting legal concerns, integrate multiple legal instruments, and create sophisticated simulations with error handling and performance considerations.

---

# **Part 7: L4 Syntax Mastery - Functional Programming for Lawyers**

## **7.1 The Functional Mindset: "Think More in a Haskell Way"**

**Critical insight:** L4 is fundamentally a functional programming language. Understanding this is essential for writing correct L4 code.

**What does "functional" mean for lawyers?**

1. **Functions over procedures:** Instead of step-by-step instructions, you define what something IS
2. **Immutable data:** Legal facts don't change—you create new states rather than modifying existing ones
3. **Composition:** Complex legal rules are built by combining simpler ones
4. **Type safety:** The computer prevents logical errors by checking that everything "fits together"

**Example transformation from procedural to functional thinking:**

**❌ Procedural mindset:**
```l4
-- "Step 1: Check if application complete"
-- "Step 2: Check if purposes charitable"
-- "Step 3: If both true, register charity"
```

**✅ Functional mindset:**
```l4
-- Define what completeness IS
GIVEN applicant IS A RegisterEntry
GIVETH A BOOLEAN
`application is complete` MEANS
    `constitution is written` applicant
    AND `has at least one purpose` applicant
    AND `has valid public benefit statement` applicant

-- Define what charitable purposes ARE
GIVEN charity IS A RegisterEntry
GIVETH A BOOLEAN
`all purposes are charitable` MEANS
    all (GIVEN p YIELD `is charitable purpose` p) (charity's purposes)

-- Define registration criteria using the above
GIVEN applicant IS A RegisterEntry
GIVETH A BOOLEAN
`should register` MEANS
    `application is complete` applicant
    AND `all purposes are charitable` applicant
```

**Why this matters:** Legal reasoning is naturally functional—we define criteria and apply them, rather than executing procedures.

## **7.2 Function Application Precedence - The Critical Pattern**

**The most important syntax rule in L4:** Function application has higher precedence than field access.

**This causes systematic errors that look like this:**
```
unexpected 's
expecting &&, (, *, +, -, /, ;, <, <=, =, >, >=, AND, OR...
```

**Problem examples and solutions:**

**❌ Wrong - Parser confusion:**
```l4
length applicant's purposes > 0           -- Parser sees: (length applicant)'s purposes > 0
all (GIVEN p YIELD...) charity's purposes -- Parser sees: (all (GIVEN p YIELD...) charity)'s purposes
elem governor charity's governors         -- Parser sees: (elem governor charity)'s governors
```

**✅ Correct - Use parentheses:**
```l4
length (applicant's purposes) > 0
all (GIVEN p YIELD...) (charity's purposes)
elem governor (charity's governors)
```

**The pattern:** When passing field access as function arguments, always wrap in parentheses.

**Why this happens:**
- L4 follows Haskell-like precedence: `function application > field access > comparison > boolean operators`
- `length applicant's purposes` is parsed as `(length applicant)'s purposes`
- The parser expects `length` to be applied to `applicant`, then tries to access `purposes` field of the result
- But `applicant` is not a list, so `length applicant` is a type error, and the parser gets confused

## **7.3 Field Access Patterns - Possessive vs Functional**

**L4 supports both possessive syntax and function application for field access.**

**When possessive syntax works:**
```l4
applicant's purposes                    -- ✅ Fine in simple contexts
charity's constitution's isWritten      -- ✅ Chained access works
governor's convictions                  -- ✅ Basic field access
```

**When you MUST use function application:**
```l4
-- In EXISTS expressions (possessive not supported)
❌ EXISTS c IN list SUCH THAT c's isSpent EQUALS FALSE
✅ EXISTS c IN list SUCH THAT isSpent c EQUALS FALSE

-- In nested function calls (precedence issues)
❌ length applicant's purposes
✅ length (applicant's purposes)

-- In quantifier arguments
❌ all (GIVEN p YIELD...) charity's purposes
✅ all (GIVEN p YIELD...) (charity's purposes)
```

**Convert possessive to functional:**
- `object's field` → `field object`
- `object's field1's field2` → `field2 (field1 object)`

**For deeply nested access:**
```l4
-- Possessive (can be problematic in complex expressions)
(lastFinancialRecord charity)'s financialYear's endDate

-- Functional (always works)
endDate (financialYear (lastFinancialRecord charity))
```

## **7.4 List Operations and Quantifiers**

**Key insight:** Use prelude functions instead of inventing syntax.

**❌ Non-existent syntax:**
```l4
EXISTS c IN list SUCH THAT condition    -- Not valid L4
EVERY x IS A Type WHERE condition       -- Not valid L4
FOR ALL x IN list CHECK condition       -- Not valid L4
```

**✅ Use prelude functions:**
```l4
-- For "exists" - use `any`
any (GIVEN c YIELD condition) list

-- For "all" - use `all`
all (GIVEN p YIELD condition) list

-- For "none" - use NOT and any
NOT any (GIVEN x YIELD condition) list

-- For filtering - use `filter`
filter (GIVEN x YIELD condition) list
```

**Pattern for quantified conditions:**
```l4
-- Template: quantifier (GIVEN variable YIELD condition) list

-- Example: "All governors have no unspent convictions"
all (GIVEN gov YIELD NOT any (GIVEN c YIELD isSpent c EQUALS FALSE) (gov's convictions)) governors

-- Example: "Some purpose is charitable"
any (GIVEN p YIELD `is charitable purpose` p) purposes

-- Example: "No governor is disqualified"
NOT any (GIVEN gov YIELD gov's isDisqualified EQUALS TRUE) governors
```

## **7.5 Type Safety and Missing Functions**

**Problem:** L4's prelude is minimal—common functions may be missing.

**Systematic approach:**
1. **Check if function exists** in prelude (like `length`, `elem`, `filter`)
2. **Define missing functions** at the top of your file
3. **Use proper types** (don't mix STRING and LIST operations)

**Essential missing functions to define:**
```l4
-- String length (often needed, not in prelude)
GIVEN str IS A STRING
GIVETH A NUMBER
stringLength str MEANS
    -- Implementation depends on L4 version
    0  -- Placeholder, or use string comparison patterns

-- List length (also missing from prelude)
GIVEN a IS A TYPE
      list IS A LIST OF a
GIVETH A NUMBER
length list MEANS
  CONSIDER list
  WHEN EMPTY THEN 0
  WHEN x FOLLOWED BY xs THEN 1 + length xs

-- Safe list access
GIVEN a IS A TYPE
      list IS A LIST OF a
      index IS A NUMBER
GIVETH A MAYBE a
safeAt list index MEANS
    IF index < 0 OR index >= length list
    THEN NOTHING
    ELSE JUST (at list index)
```

**Type checking patterns:**
```l4
-- ❌ Type error: mixing string and list operations
length someString > 0           -- length expects LIST, gets STRING

-- ✅ Correct: check string non-emptiness
NOT someString EQUALS ""        -- STRING comparison

-- ✅ Correct: check list non-emptiness
length someList > 0            -- LIST operation
```

## **7.6 Boolean Logic and Comparison Patterns**

**Prefer direct comparisons over negation:**

**❌ Unnecessarily complex:**
```l4
NOT c's isSpent EQUALS TRUE
NOT governor's isDisqualified EQUALS TRUE
```

**✅ Cleaner and clearer:**
```l4
c's isSpent EQUALS FALSE
governor's isDisqualified EQUALS FALSE
```

**Comparison precedence patterns:**
```l4
-- ✅ Simple comparisons work naturally
age >= 18
amount > 1000
date EQUALS today

-- ✅ Parentheses for complex expressions
(length purposes) > 0
(governor's age) >= minimumAge
(charity's income's amount) > threshold
```

**Boolean combination patterns:**
```l4
-- AND/OR precedence (AND binds tighter than OR)
condition1 AND condition2 OR condition3    -- Means: (condition1 AND condition2) OR condition3
condition1 OR condition2 AND condition3    -- Means: condition1 OR (condition2 AND condition3)

-- Use parentheses for clarity
(condition1 OR condition2) AND condition3
condition1 AND (condition2 OR condition3)
```

## **7.7 Systematic Debugging Approach**

**When you see syntax errors, follow this checklist:**

1. **Function precedence:** Are field accesses in function arguments wrapped in parentheses?
2. **Missing functions:** Is the function defined in prelude or your file?
3. **Type mismatches:** Are you using STRING operations on LISTs or vice versa?
4. **Possessive syntax:** Can you convert to function application form?
5. **Boolean logic:** Are comparisons and boolean operations correctly parenthesized?

**Error patterns and fixes:**
```
Error: "unexpected 's"
→ Fix: Add parentheses around field access in function arguments

Error: "could not find definition for LENGTH"
→ Fix: Define length function at top of file

Error: "expecting BOOLEAN, got STRING"
→ Fix: Use correct comparison (EQUALS "", not length > 0)

Error: "could not find definition for EXISTS"
→ Fix: Use any (GIVEN x YIELD condition) list
```

**Success Check:** You now understand L4's functional nature, can handle precedence correctly, use proper quantifiers, and systematically debug type and syntax errors.

---

# **Conclusion**

You've learned to:

1. **Write precise legal rules** that capture obligations, conditions, and consequences
2. **Model complex legal entities** with proper types and relationships  
3. **Handle multi-step legal processes** using the PROVISION approach
4. **Organize complete regulatory schemes** using the three-layer architecture
5. **Understand the design principles** that make L4 effective for legal modeling
6. **Apply advanced techniques** for real-world complexity

**What's Next:**
- Practice with your own legal domain (employment law, tax law, etc.)
- Experiment with the Jersey Charities model in the examples
- Explore integration with legal databases and case management systems
- Consider how L4 could improve legal drafting in your practice

**Key Insight:** L4 isn't just about automating existing legal processes—it's about thinking more clearly about how legal systems actually work, and designing better legal frameworks as a result.

---

# **Quick Reference**

## **Basic Syntax**
```l4
-- Comments start with --
GIVEN entity IS A Type        -- Input parameters
PARTY actor                   -- Who has the obligation
MUST action                   -- What they must do  
WITHIN timeframe             -- Deadline
HENCE consequence            -- If they comply
LEST alternative             -- If they don't comply
```

## **Type Declarations**
```l4
DECLARE TypeName              -- Simple type
DECLARE RecordType           -- Record with fields
    HAS field1 IS A Type1
        field2 IS A Type2

DECLARE EnumType IS ONE OF   -- Enumeration
    Option1
    Option2 HAS data IS A Type
```

## **PROVISION Syntax**
```l4
GIVETH A PROVISION Actor Action
`process name` MEANS
  PARTY Actor
  MUST Action OF simpleParameter  -- Use simple parameters only
  WITHIN timeframe
  HENCE nextStep
  LEST errorPath
```

**Important:** Avoid complex object construction within PROVISION rules. Use helper functions or pre-defined objects instead.

```l4
-- ❌ Don't do this:
MUST Action OF ComplexType WITH field1 IS ..., field2 IS ...

-- ✅ Do this instead:
MUST Action OF `build complex object` parameters
-- or
MUST Action OF predefinedExample
```

## **Simulation**
```l4
#EVAL expression                                    -- Evaluate expression
#PROVISION `name` actor1 actor2 state AT time WITH  -- Simulate process
  PARTY actor1 DOES action1 AT time1
  PARTY actor2 DOES action2 AT time2
```
