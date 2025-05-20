
# L4: A Guide for Legal Professionals with Python Experience

## Introduction

L4 is a specialized programming language designed for the precise, machine-readable formalization of legal documents including legislation, regulations, and contracts. It creates "computable documents" that a computer can analyze and execute while maintaining isomorphism with the original legal text.

For Python programmers, think of L4 as a declarative, domain-specific language (DSL) focused on defining *what things are* (data structures, types) and *what rules apply* (decisions, relationships, processes), rather than step-by-step instructions. Its syntax is crafted for readability that aligns with common legal phrasings.

## 1. L4 Fundamentals

### 1.1 Core Syntax

| Feature              | L4                                    | Python Equivalent    | L4 Notes                                          |
| -------------------- | ------------------------------------- | -------------------- | ------------------------------------------------- |
| **Naming Things**    | `` `applicant's age` ``              | `applicants_age`     | Backticks allow spaces and legal-like phrases     |
| **Comments**         | `-- Comment` or `{- Block comment -}` | `# Comment`          | Multiple styles for documenting code               |
| **Booleans**         | `TRUE`, `FALSE`                       | `True`, `False`      | Case-sensitive keywords                            |
| **Logical Operators**| `AND`, `OR`, `NOT`                    | `and`, `or`, `not`   | Plus `ANY`, `ALL` for list conditions              |
| **Grouping**         | Indentation; `( )` for override       | `( )` for grouping   | Indentation for expressions mimics legal structure |
| **Sections**         | `§ Title`, `§§ Subtitle`              | N/A                  | Mirrors legal document organization                |
| **References**       | `@ref url https://...`                | Comments with links   | Embeds citations or source references              |
| **Ditto Syntax**     | `^` (caret)                           | No direct equivalent  | Repeats elements from the line above in columns    |

### 1.2 Data Types

#### Basic Types
* `BOOLEAN`: `TRUE` or `FALSE`
* `STRING`: Text in double quotes, e.g., `"Contract Clause 3.a"`
* `NUMBER`: Numerical values
* Specialized types like `Date` and `Money` (often from libraries)

#### User-Defined Types

1. **Type Aliases**
   ```l4
   DECLARE `Contract Deadline` IS A Date
   DECLARE `Monetary Penalty` IS A Money
   ```

2. **Enumerations & Sum Types**
   ```l4
   DECLARE SecurityType IS ONE OF
      CommonStock
      PreferredStock
      SAFE_Note

   DECLARE PaymentMethod IS ONE OF
      CreditCard  HAS cardNumber IS A STRING, expiryDate IS A Date
      BankTransfer HAS accountNumber IS A STRING
      Cheque       HAS chequeNumber IS A NUMBER
   ```

3. **Records**
   ```l4
   DECLARE Company
      HAS `Name` IS A STRING
          `Incorporation Date` IS A Date
          `Jurisdiction` IS A STRING
          `Cap Table` IS A LIST OF `Cap Table Entry`
   ```

   Creating instances:
   ```l4
   `Acme Corp` MEANS
     Company WITH `Name` IS "Acme Corporation"
                  `Incorporation Date` IS Date OF 2020, 1, 15
                  `Jurisdiction` IS "Delaware"
   ```
   
   Shorthand (values in declaration order):
   ```l4
   `Beta LLC` MEANS Company OF "Beta LLC", Date OF 2021, 6, 1, "Nevada", EMPTY_LIST
   ```

4. **Optional Types with `MAYBE`**
   ```l4
   DECLARE Person
      HAS name IS A STRING
          middleName IS A MAYBE STRING -- Can be absent/nothing
   ```

## 2. Expressing Logic

### 2.1 Simple Definitions

```l4
`Standard Interest Rate` MEANS 0.05 -- 5%
`Governing Law` MEANS "State of New York"
```

### 2.2 Decision Functions

The heart of L4 for legal rules:

```l4
GIVEN applicant_age IS A NUMBER
      years_of_residency IS A NUMBER
GIVETH BOOLEAN
DECIDE `is eligible for benefit X` IF
    applicant_age GREATER THAN OR EQUAL TO 18
    AND years_of_residency GREATER THAN OR EQUAL TO 5
```

### 2.3 Conditional Logic

```l4
GIVEN order_value IS A Money
GIVETH Money
DECIDE `shipping_cost` IS
    IF order_value GREATER THAN Money OF 100, "USD"
    THEN Money OF 0, "USD"  -- Free shipping
    ELSE Money OF 10, "USD" -- Standard shipping
```

### 2.4 Local Definitions with `WHERE`

```l4
GIVEN principal IS A Money
      term IS A NUMBER
GIVETH Money
DECIDE `total repayment` IS
    principal + (principal * `interest rate` * term)
    WHERE `base rate` IS 0.03
          `risk premium` IS 0.02
          `interest rate` IS `base rate` + `risk premium`
```

### 2.5 Functional Patterns (from Prelude)

```l4
GIVEN cap_table IS A LIST OF `Cap Table Entry`
`Total Share Value` MEANS
  sum OF
    map OF
      GIVEN entry YIELD amount OF entry's Security's Quantity
      filter OF
        GIVEN entry YIELD entry's Security's Instrument EQUALS "Shares"
        cap_table
```

## 3. Regulative Rules & State Transitions

### 3.1 Core Structure

```l4
§ `Contract Step Name`

GIVEN investor IS A Actor, company IS A Actor, state IS A `Symbol Table`
GIVETH CONTRACT Actor Action
`Investment Step` MEANS
  PARTY investor
  MUST pay 200
  WITHIN 10 days
  HENCE `Share Issuance Step` investor company (
    state WITH balance IS state's balance + 200
  )
  LEST `Default Procedure`
```

### 3.2 Key Regulative Elements

* **Universal Rules with `EVERY`**:
  ```l4
  GIVEN location IS A Location
  EVERY Person p
    IF `is at` p location
    MAY depart location
  ```

* **Deontic Operators**:
  * `MUST`: Obligation
  * `MAY`: Permission
  * `SHANT`/`MUSTNT`: Prohibition
  
* **Timeframes**:
  * `WITHIN <duration>`: Deadline for action

* **Outcome Paths**:
  * `HENCE <next_step>`: Consequence of successful action
  * `LEST <alternative_step>`: Consequence of failed/missed action

* **State Management**:
  * Immutable updates: `oldState WITH field IS newValue`
  * LET-IN binding: `LET newState = ... IN EVAL RuleName newState`

* **Terminal States**:
  * `FULFILLED`: Contract successfully completed
  * Implied `BREACH`: Contract violated/obligations missed

### 3.3 Modeling Powers and Meta-Rules

Powers allow one party to change the rights or obligations of another:

```l4
GIVEN officer IS A PoliceOfficer
      person IS A Person
      location IS A Location
PARTY officer
  MAY changeRights
      SUCHTHAT PARTY person
               MUSTNT depart location
```

Higher-order powers (power to grant powers):

```l4
GIVEN president IS A HeadOfState
      officer_candidate IS A Person
PARTY president
  MAY changeRights
      SUCHTHAT PARTY officer_candidate
               MAY changeRights
                   SUCHTHAT PARTY citizen
                            MUSTNT depart location
```

### 3.4 Simulating Contracts: `#CONTRACT`

```l4
#CONTRACT `Investment Agreement` investor company initialState AT 1 WITH
  PARTY investor DOES pay 200 AT 2
  PARTY company DOES issue (shares 300) AT 5
```

This directive simulates the execution of the contract with specific actors, actions, and timing, returning the resulting state or "residual contract" (remaining obligations).

## 4. Advanced L4 Concepts

### 4.1 Document Structure

* `§`, `§§`, `§§§`: Hierarchical sections
* `@ref` annotations for source citations

### 4.2 Common Idioms for Legal Domain Modeling

* **Cap Tables & Financial Instruments**:
  ```l4
  DECLARE `Cap Table Entry`
      HAS Holder IS A Investor
          Security IS A Security
  ```

* **Logging State Changes**:
  ```l4
  log IS "Action performed" FOLLOWED BY previous_log
  ```

* **Formal Agreement Templates**:
  ```l4
  `about this document` MEANS
    `Agreement Template` WITH
      jurisdiction IS "Singapore"
      version IS "1.2"
      type IS `SAFE (Post-Money)`
  ```

## 5. L4 in Practice

### 5.1 IDE Support

* Syntax highlighting and error checking
* `#EVAL`: Inline evaluation of expressions
* `#CHECK`: Type checking expressions
* `#CONTRACT`: Simulating contract execution

### 5.2 Terminology Mapping

| Legal Concept    | L4 Expression                                  |
| ---------------- | ---------------------------------------------- |
| Right            | `EVERY Actor MAY Action`                       |
| Obligation       | `PARTY Actor MUST Action WITHIN Time`          |
| Prohibition      | `PARTY Actor SHANT/MUSTNT Action`              |
| Power            | `PARTY Actor MAY changeRights SUCHTHAT ...`    |
| Process/Protocol | Contract steps linked with `HENCE`/`LEST`      |
| Definitions      | `Term MEANS Expression`                        |
| Legal Tests      | `DECIDE Test IF Conditions`                    |
| Jurisdiction     | Typically modeled as `STRING` or enum          |

---
