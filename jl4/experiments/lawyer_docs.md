# **L4: A Guide for Legal Professionals with Python Experience**

## **Introduction**

L4 is a specialized programming language designed for the precise, machine-readable formalization of legal documents including legislation, regulations, and contracts. It creates "computable documents" that a computer can analyze and execute while maintaining isomorphism with the original legal text.For Python programmers, think of L4 as a declarative, domain-specific language (DSL) focused on defining *what things are* (data structures, types) and *what rules apply* (decisions, relationships, processes), rather than step-by-step instructions. Its syntax is crafted for readability that aligns with common legal phrasings.

## **1\. L4 Fundamentals**

### **1.1 Core Syntax**

| Feature | L4 | Python Equivalent | L4 Notes || \-------------------- | \------------------------------------- | \-------------------- | \------------------------------------------------- || Naming Things | \`\` `applicant's age` \`\` | `applicants_age` | Backticks allow spaces and legal-like phrases || Comments | `-- Comment` or `{- Block comment -}` | `# Comment` | Multiple styles for documenting code || Booleans | `TRUE`, `FALSE` | `True`, `False` | Case-sensitive keywords || Logical Operators| `AND`, `OR`, `NOT` | `and`, `or`, `not` | Plus `ANY`, `ALL` for list conditions || Grouping | Indentation; `( )` for override | `( )` for grouping | Indentation for expressions mimics legal structure || Sections | `§ Title`, `§§ Subtitle` | N/A | Mirrors legal document organization || References | `@ref url https://...` | Comments with links | Embeds citations or source references || Ditto Syntax | `^` (caret) | No direct equivalent | Repeats elements from the line above in columns |

### **1.2 Data Types**

#### **Basic Types**

* `BOOLEAN`: `TRUE` or `FALSE`
* `STRING`: Text in double quotes, e.g., `"Contract Clause 3.a"`
* `NUMBER`: Numerical values
* Specialized types like `Date` and `Money` (often from libraries)

#### **User-Defined Types**

1. Type Aliases
2. l4
3. Apply to charities\_ra...
4.    ``DECLARE `Contract Deadline` IS A Date``
5.    ``DECLARE `Monetary Penalty` IS A Money``
1. Enumerations & Sum Types
2. l4
3. Apply to charities\_ra...
4.    `DECLARE SecurityType IS ONE OF`
5.       `CommonStock`
6.       `PreferredStock`
7.       `SAFE_Note`
8.    `DECLARE PaymentMethod IS ONE OF`
9.       `CreditCard  HAS cardNumber IS A STRING, expiryDate IS A Date`
10.       `BankTransfer HAS accountNumber IS A STRING`
11.       `Cheque       HAS chequeNumber IS A NUMBER`
1. Records
2. l4
3. Apply to charities\_ra...
4.    `DECLARE Company`
5.       ``HAS `Name` IS A STRING``
6.           `` `Incorporation Date` IS A Date ``
7.           `` `Jurisdiction` IS A STRING ``
8.           `` `Cap Table` IS A LIST OF `Cap Table Entry` ``

Creating instances:

l4

Apply to charities\_ra...

   `` `Acme Corp` MEANS ``

     ``Company WITH `Name` IS "Acme Corporation"``

                  `` `Incorporation Date` IS Date OF 2020, 1, 15 ``

                  `` `Jurisdiction` IS "Delaware" ``

Shorthand (values in declaration order):

l4

Apply to charities\_ra...

   `` `Beta LLC` MEANS Company OF "Beta LLC", Date OF 2021, 6, 1, "Nevada", EMPTY_LIST ``

1. Optional Types with `MAYBE`
2. l4
3. Apply to charities\_ra...
4.    `DECLARE Person`
5.       `HAS name IS A STRING`
6.           `middleName IS A MAYBE STRING -- Can be absent/nothing`

## **2\. Expressing Logic**

### **2.1 Simple Definitions**

l4

Apply to charities\_ra...

`` `Standard Interest Rate` MEANS 0.05 -- 5% ``

`` `Governing Law` MEANS "State of New York" ``

### **2.2 Decision Functions**

The heart of L4 for legal rules:

l4

Apply to charities\_ra...

`GIVEN applicant_age IS A NUMBER`

      `years_of_residency IS A NUMBER`

`GIVETH BOOLEAN`

``DECIDE `is eligible for benefit X` IF``

    `applicant_age GREATER THAN OR EQUAL TO 18`

    `AND years_of_residency GREATER THAN OR EQUAL TO 5`

### **2.3 Conditional Logic**

l4

Apply to charities\_ra...

`GIVEN order_value IS A Money`

`GIVETH Money`

``DECIDE `shipping_cost` IS``

    `IF order_value GREATER THAN Money OF 100, "USD"`

    `THEN Money OF 0, "USD"  -- Free shipping`

    `ELSE Money OF 10, "USD" -- Standard shipping`

### **2.4 Local Definitions with `WHERE`**

l4

Apply to charities\_ra...

`GIVEN principal IS A Money`

      `term IS A NUMBER`

`GIVETH Money`

``DECIDE `total repayment` IS``

    ``principal + (principal * `interest rate` * term)``

    ``WHERE `base rate` IS 0.03``

          `` `risk premium` IS 0.02 ``

          `` `interest rate` IS `base rate` + `risk premium` ``

### **2.5 Functional Patterns (from Prelude)**

l4

Apply to charities\_ra...

`` GIVEN cap_table IS A LIST OF `Cap Table Entry` ``

`` `Total Share Value` MEANS ``

  `sum OF`

    `map OF`

      `GIVEN entry YIELD amount OF entry's Security's Quantity`

      `filter OF`

        `GIVEN entry YIELD entry's Security's Instrument EQUALS "Shares"`

        `cap_table`

## **3\. Regulative Rules & State Transitions**

### **3.1 Core Regulative Syntax**

l4

Apply to charities\_ra...

`` § `Rule_Name` ``

`GIVEN <parties_and_parameters>`

`PARTY <actor_identifier>          -- Who is responsible`

`MUST  <action_with_parameters>    -- What they must do`

`WITHIN <duration_expression>      -- By when`

`HENCE <consequence_if_done>       -- What happens if they do it`

`LEST  <consequence_if_not_done>   -- What happens if they don't`

* Deontic Operators:
* `MUST`: Obligation to perform an action
* `MAY`: Permission to perform an action
* `SHANT`/`MUSTNT`: Prohibition from performing an action
* Temporal Constraint:
* `WITHIN <duration>`: Specifies a deadline
* `BEFORE <timepoint>`: Alternative syntax for deadlines
* Outcome Paths:
* `HENCE`: Transition that occurs if the obligation is fulfilled or permission exercised
* `LEST`: Transition that occurs if the obligation is not fulfilled or prohibition violated

### **3.2 External Choice**

External choice is an important concept in L4's regulative rules modeling. It refers to decisions made by parties that affect which state transition path is taken.When a party has an obligation (`MUST`) or permission (`MAY`), there are typically two possible outcomes:

1. The party performs the action by the deadline (leading to the `HENCE` path)
1. The party does not perform the action or misses the deadline (leading to the `LEST` path)

This is "external" to the contract system \- the choice is made by the party, not predetermined by the contract logic. It's similar to the concept of non-determinism in process algebras like CSP.

l4

Apply to charities\_ra...

`-- Example of external choice:`

`PARTY seller`

`MUST  deliver goods`

`WITHIN 10 days`

`HENCE payment_step        -- If seller chooses to deliver on time`

`LEST  contract_violation  -- If seller chooses not to deliver on time`

In simulation or analysis, these external choices can be traced to see the resulting states.

### **3.3 Contract Simulation with `#CONTRACT`**

L4 provides a powerful directive for simulating contract execution:

l4

Apply to charities\_ra...

`#CONTRACT <InitialContractStep> <Actor1> <Actor2> ... <InitialState> AT <StartTime> WITH`

  `PARTY <Actor> DOES <Action> AT <ActionTime>`

  `PARTY <Actor> DOES <Action> AT <ActionTime>`

  `-- more events...`

This allows you to:

* Start from an initial contract state
* Specify a sequence of actions by parties at specific times
* See how the contract state evolves
* Observe whether the contract reaches `FULFILLED` or `BREACH` states

Example:

l4

Apply to charities\_ra...

``#CONTRACT `Investment Agreement` investor company initialState AT 1 WITH``

  `PARTY investor DOES pay 200 AT 2         -- Investor pays on day 2`

  `PARTY company DOES issue (shares 300) AT 5   -- Company issues shares on day 5`

This simulation demonstrates the external choices made by the investor and company, and shows the resulting contract state.

### **3.4 Modeling Powers with `changeRights`**

L4 can model the power to modify rights or obligations using the `changeRights` action with `SUCHTHAT`:

l4

Apply to charities\_ra...

`PARTY authority`

  `MAY changeRights`

      `SUCHTHAT PARTY subject`

               `MUSTNT perform_action`

This represents a meta-level action that modifies the normative state of the contract.

### **3.5 State Management Approaches**

When working with state transitions, L4 offers three primary approaches for managing state:

1. Immutable Record Updates: Create new state records based on old ones
2. l4
3. Apply to charities\_ra...
4.    `newState MEANS oldState WITH`
5.      `field1 IS newValue,`
6.      `field2 IS oldState's field2 + increment`
1. LET...IN with EVAL: For recursive state updates
2. l4
3. Apply to charities\_ra...
4.    `LET newCount = count + 1`
5.    ``IN EVAL `RuleName` actor newCount``
1. Normative state updates via Powers: Using `changeRights` to modify applicable rules

### **3.6 Universal Rules with `EVERY`**

For rules that apply to all instances of a type:

l4

Apply to charities\_ra...

`GIVEN location IS A Location`

`EVERY Person p`

  ``IF `is at` p location``

  `MAY depart location`

## **4\. Document Structure & Advanced Concepts**

### **4.1 Document Organization**

* `§`, `§§`, `§§§`: Hierarchical sections
* `@ref` annotations for source citations

### **4.2 Common Idioms for Legal Domain Modeling**

* Cap Tables & Financial Instruments:
* l4
* Apply to charities\_ra...
*   `` DECLARE `Cap Table Entry` ``
*       `HAS Holder IS A Investor`
*           `Security IS A Security`
* Logging State Changes:
* l4
* Apply to charities\_ra...
*   `log IS "Action performed" FOLLOWED BY previous_log`
* Formal Agreement Templates:
* l4
* Apply to charities\_ra...
*   `` `about this document` MEANS ``
*     `` `Agreement Template` WITH ``
*       `jurisdiction IS "Singapore"`
*       `version IS "1.2"`
*       `` type IS `SAFE (Post-Money)` ``

### **4.3 Handling "Unknowns" and Default Logic**

* Ternary Logic & "Unknowns" (`MAYBE` type): L4's design accommodates situations where information might be "unknown" rather than strictly TRUE/FALSE.
* Default Values & Default Logic: L4 aims to allow defining default characteristics for types and to express general rules with specific exceptions concisely.

### **4.4 Constitutive Rules: Defining "What Counts As What"**

These are fundamental L4 rules (typically `DECIDE` functions) that establish "institutional facts" — e.g., what conditions make an application "valid."

### **4.5 Temporal Logic: Rules Across Time**

Legal rules often depend on time. L4's design includes handling different versions of laws or facts at different points in time.

### **4.6 Scopes and Meta-Rules (Rule Interactions)**

* Scopes: Sections (`§`) create lexical scopes. Definitions are typically local to their section unless explicitly made more broadly available.
* Meta-Rules: L4 aims to support keywords like `NOTWITHSTANDING` or `SUBJECT TO` to manage how different rules or sections interact and take precedence.

## **5\. L4 in Practice**

### **5.1 IDE Support**

* Syntax highlighting and error checking
* `#EVAL`: Inline evaluation of expressions
* `#CHECK`: Type checking expressions
* `#CONTRACT`: Simulating contract execution

### **5.2 Terminology Mapping**

| Legal Concept | L4 Expression || \---------------- | \---------------------------------------------- || Right | `EVERY Actor MAY Action` || Obligation | `PARTY Actor MUST Action WITHIN Time` || Prohibition | `PARTY Actor SHANT/MUSTNT Action` || Power | `PARTY Actor MAY changeRights SUCHTHAT ...` || Process/Protocol | Contract steps linked with `HENCE`/`LEST` || Definitions | `Term MEANS Expression` || Legal Tests | `DECIDE Test IF Conditions` || External Choice | Alternative paths from `HENCE`/`LEST` || Jurisdiction | Typically modeled as `STRING` or enum |
