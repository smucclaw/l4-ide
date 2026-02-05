---
globs: "**/*.l4"
---

L4 is a typed, functional, DSL designed for encoding legal and regulatory logic with precision, clarity, and executability. Your goal is to produce syntactically correct, semantically accurate, and maintainable L4 code that faithfully represents the input natural language.

HOW TO THINK ABOUT L4:

1. Model the smallest set of explicitly and implicitly defined essential types referred to in the source text, with only the attributes and relationships required to implement the spirit of the rules or legislation and not more.
2. Use those type definitions to implement facts, rules, conditional logic and regulative functions the source material explicitly and implicitly describes. If something is not well enough defined in the source, make it an input to a regulative function or an attribute on the relevant type. Implement the code with variable and function definitions that mimic natural language. Consider Infix, Postfix and Mixfix notations where appropriate.
3. Implement and run tests (using #EVAL/#CHECK/#ASSERT) for the entire code and proceed to refine the code if necessary based on the diagnostic feedback. Implementing test-cases is best done in a separate files in the folder of the original file and using "IMPORT `filename_name-of-test-scenario.l4`" in the first line to reference the original L4 file.
4. Decorate the important rules that qualify for direct evaluation (relevant for an API consumer) as export using the @export notation and describe the parameters with @desc.

If given, DO NOT expand on the source policy, regulation or legislation. Avoid redundancies, reduce complexity and minimise type dependencies but do not compromise the language of the rule, it's permissions, obligations, logic and intent.
Always ensure your output is valid L4 code based on the language's syntax and semantics. Use the "search_l4_documentation" MCP tool frequently to retrieve examples and explaination of the code patterns you want to use and validate them with the "validate_l4_file" MCP tool before generating.

## Core Principles of L4

- **Declarative and Logical**: L4 focuses on declaring types, functions, and regulative rules using logical constructs.
- **Readability**: Use natural-language-like syntax with backticks for very descriptive names (e.g., `is eligible for benefits`). Best to use as much original content as possible within these variable/function names.
- **Scoping**: Organize code into sections that reflect the source material (§). Identifiers can be accessed in a scoped fashion.
- **Type-Safety**: Types and functions are defined explicitly. L4 supports type inference.
- **Legal Fidelity**: Preserve the intent, conditions, and exceptions from the source text without adding or omitting details.
- **Executability**: L4 code should be evaluable, supporting features like lazy evaluation, traces, and assertions.

## Syntax Overview

Based on the L4 lexer and documentation, L4 supports the following tokens and constructs:

### Operators and Symbols

- Arithmetic: PLUS, MINUS, TIMES, DIVIDED BY, MODULO
- Comparison: AT LEAST, AT MOST, EQUALS, GREATER THAN, LESS THAN, ABOVE, BELOW
- Negator: NOT (condition)
- Logical: AND, OR
- Symbols: (, ), [, ], §, ,, , 's (genitive)
- Annotations: @ref, @ref-src, @ref-map, @desc

### Literals

- Integers: e.g., 42, -5 (Type NUMBER)
- Rationals/Floats: e.g., 3.14, -2.5 (Type NUMBER)
- Percent: e.g. 5% (Type NUMBER)
- Strings: "hello" (Type STRING)
- Boolean: TRUE, FALSE (Type BOOLEAN)

#### Extra with IMPORT prelude/IMPORT daydate (Use documentation for more info)

- Lists : LIST "a", "b", "c" (Type LIST OF STRING)
- Nullables Types: MAYBE NUMBER
- Nullable Values: e.g. JUST 5
- Date and Days (Type DATE): February 2 2024, DATE OF (2, 10, 2025)

### Identifiers

- Alphanumeric with underscores, starting with a letter.
- Use backticks for multi-word or descriptive names: `age of person`
- Access properties of objects (field access) with "'s" (not a backtick!) behind object identifiers. E.g.

```l4
`the commission`'s `area of responsibility` EQUALS person's location
```

### Comments

- Line: -- or //
- Block: {- ... -}

### Descriptions and Referencing

- Often some text inside of legislation is not trying to regulate but merely a comment or describing the law itself or purpose of it. Use @desc "Description" in those cases.
- Sometimes legal language references external context. Use @ref behind an attribute or type definition with the given reference.

## Structure and Organization

- **Sections**: Use § followed by a backticked name for headers, e.g., § `Section (1) b`.
- **Indentation**: Use consistent indentation (4 spaces) to denote nesting/belonging to an above-level construct. Proper indentation is VERY important for the code to compile.
- **Scoping**: Group related declarations, functions, and rules logically.
- **Parenthesis**: Use () to wrap small code pieces to indicate execution priority where other unclear.

### Base Types and Custom Types

- Base Types: BOOLEAN, NUMBER, STRING, TYPE
- Declare a type with attributes unknown (makes it untestable):

```l4
ASSUME TypeWithUnknownAttributes IS A TYPE
```

- Declare a type with attributes known:

```l4
DECLARE `Known Type` HAS
    Attribute   IS A STRING
    Attribute2  IS A BOOLEAN
```

- Group/Compound types:

```l4
DECLARE Fruit IS ONE OF
    Apple
    Banana
```

- L4 supports type inference. DECLARE automatically assumes a TYPE. So there is no need to ASSUME it before using DECLARE.
- Important: Types implemented with ASSUME can't be tested later as they are intentionally not specific. It's best to never use ASSUME unless unavoidable!

### Object creation

```l4
`Variable name` MEANS `Known type` OF (AttributeValue, Attribute2Value)

`Other variable` MEANS `Known type` WITH
                            Attribute   IS "Hello"
                            Attribute2  IS TRUE
```

### Function Declarations vs Implementations

- **ASSUME vs DECIDE ... IS/MEANS**: Use ASSUME when the function implementation is external/unspecified. Use DECIDE ... IS/MEANS when the implementation can be completed.
- Avoid using ASSUME for functions critical for evaluation. Often unknown implementations need to be escalated as function parameters or become attributes of input types. In the same spirit, don't DECIDE things to be TRUE or FALSE unless explicitly stated. It usually means it needs to become an input to a function.
- Only use the keyword GIVEN once per function. No nead to repeat every line.
- Function calling: `function name` `parameter 1` `parameter 2` ...

## Function Definitions

- DECIDE ... IS/MEANS signatures:

```l4
GIVEN param1 IS A Type
      param2 IS AN `Other Type`
GIVETH A ReturnType
DECIDE `function name` IS
    ...expression

GIVEN   param1 IS A NUMBER
        param2 IS A NUMBER
GIVETH A BOOLEAN
`are both twos` MEANS
    param1 PLUS param2 EQUALS 4
    AND `multiplies to four` param1
    WHERE
        GIVEN param1    IS A NUMBER
        DECIDE `multiplies to four` IS
            param1 TIMES param1 EQUALS 4

`add one` p MEANS p + 1
`add two` MEANS GIVEN x YIELD x + 2
`is four` MEANS x PROVIDED x EQUALS 4

```

- Local function scope definitions possible at the end of a function after WHERE keyword.

## Conditional Statements

```l4
IF      condition
THEN    TRUE
ELSE    IF      othercondition
        THEN    TRUE
        ELSE    FALSE

CONSIDER variable
  WHEN value   THEN TRUE
  WHEN NOTHING THEN FALSE

BRANCH
  IF b1 THEN 1
  IF b2 THEN 2
  IF b3 THEN 3
  OTHERWISE 4
```

### Regulative Pattern (Obligations and contract state progression)

- Keywords: PARTY, DO, DOES, MUST, MAY, WITHIN, HENCE, LEST, EXACTLY, PROVIDED
- Regulative: PARTY Who MUST/MAY/DO Action (WITHIN Timeframe) HENCE Consequence LEST Penalty. The return type for this regulative PARTY pattern is a PROVISION OF Who, Action. To express a fulfilled PROVISION use FULFILLED.
- Consequence and Penalty must be expressed as another regulative pattern
- Regulative Action parameters take validation function as parameter
- The regulative temporal definition in this regulative pattern using WITHIN always takes NUMBER as a parameter (usually as an expression of days)
- Validation function for regulatory obligation actions ONLY: `x PROVIDED x > 5` or `EXACTLY "This string"`. Make sure to use locally specific identifier names for x that can't be confused with other identifiers. One obligation must have only one action with one validation function.
- If the same party has multiple obligations as part of the same regulative action, create a combined action instead of stacking.
- Note: Use MUST for obligations, MAY for permissions, DO for actions. There is no functional difference, just to reflect original spirit of the text.
- Important: There is NO "MUST NOT" construct. Express prohibitions as a negative action. E.g. PARTY Actor MUST `not act`
- Use the "search_l4_documentation" tool to better understand the syntax and retrieve examples for this PROVISION/PARTY pattern and how to validate it with #TRACE.

```l4
DECLARE `A party` HAS
        name IS A STRING

DECLARE `An Action` IS ONE OF
    `act` HAS
        `in this way`               IS A STRING
        `use more than this amount` IS A NUMBER
    `not act` HAS
        `in this way`               IS A STRING
        `use more than this amount` IS A NUMBER

`The party` MEANS `A party` WITH
                        name IS "The party"

GIVEN `some way` IS A STRING
GIVETH A PROVISION OF `A party`, `An Action`
`some way` `of doing something` MEANS
    PARTY   `The party`
    DO      `act`
                `in what way` `use more than what amount` PROVIDED
                    `in what way` EQUALS `some way`
                    AND `use more than what amount` GREATER THAN 5

GIVEN `some way` IS A STRING
GIVETH A PROVISION OF `A party`, `An Action`
`Doing something exactly` `some way` MEANS
    PARTY   `The party`
    MUST    `not act`
                EXACTLY `some way`
                EXACTLY 5

#TRACE `Doing something provided` "gently" AT 0 WITH
    PARTY `The party` DOES `act` "gently" 5 AT 1

#TRACE `Doing something exactly` "gently" AT 0 WITH
    PARTY `The party` DOES `not act` "gently" 5 AT 1
```

## Best Practices

1. **Descriptive Naming**: Prefer backticked phrases like `meets age requirement` over generic names.
2. **Modular Logic**: Break complex conditions into named local functions using WHERE.
3. **Consistency**: Follow type declarations before usage; ensure all variables are declared.
4. **Fidelity to Source**: Map natural language directly—e.g., "if X and Y" becomes X AND Y.
5. **Structural Organization**: Always organize code in the correct order: types, functions, actions, main logic.
6. **Avoid Ambiguity**: If input has multiple interpretations, generate variants or clarify. Explicit identifier and type names based on scope are always preferred.
7. **Conciseness**: Omit unnecessary details but ensure completeness.
8. **Action Declaration**: If using regulative statements, always declare action types before the main regulative logic.

## Testing Functions

- Validating L4 code using the tool call will return results of the following commands in `diagnostics` array.
- Types and functions declared with ASSUME can't be tested well due to lack of specific implementation. Best to use it sparsingly!

### Running code

- #EVAL: Evaluate code and return result value and type
- #ASSERT: Checks for function result to be TRUE
- #CHECK: Returns type of function result
- #TRACE: Evaluate state of function with PROVISION result after list of events

- Running functions: #EVAL `function name` param1 param2 param3 ...
- Running traces against PROVISION functions with integers representing temporal progression of events:

```l4
#EVAL foo 2 WHERE foo a MEANS a TIMES a
#ASSERT 2 + 2 EQUALS 4
#CHECK "Hello"
#TRACE `function name` AT 0 WITH
    PARTY Who DOES Action param1 param2 AT 1
```

## Full regulatory example with tests

```l4
IMPORT daydate

§ `Promissory Note`

`Note Date` MEANS September 2 2024
`Principal Amount` MEANS USD 25000
`Interest Rate Per Annum` MEANS 15%
`Monthly Installments` MEANS 12
`Default After Days Not Paid Beyond Due` MEANS 30
`Late Payment Penalty` MEANS Penalty WITH
                                `Interest Rate`     IS 5%
                                `Grace Period Days` IS 10

§§ `Parties`

`The Borrower` MEANS
  `Commercial Borrower` OF
    Company WITH
      `Name` IS "Jane Dough Pte Ltd"

`The Lender` MEANS
  `Individual Lender` OF
    `Natural Person as defined in Reg 4.3a` WITH
      `Name` IS "John Doe"

§§ `Repayment Terms`

`Monthly Interest Rate` MEANS
    `Interest Rate Per Annum` DIVIDED BY 12

`Monthly Installment Amount` MEANS
    Money WITH
        Currency  IS `Principal Amount`'s Currency
        Value     IS `Principal Amount`'s Value
                        TIMES (`Monthly Interest Rate` TIMES `Compound Factor`)
                            DIVIDED BY (`Compound Factor` MINUS 1)
    WHERE
        `Compound Factor` MEANS
            `Base to the power of`
                (1 PLUS `Monthly Interest Rate`)
                `Monthly Installments`

`Total Repayment Amount` MEANS
    Money WITH
        Currency  IS `Principal Amount`'s Currency
        Value     IS `Monthly Installment Amount`'s Value TIMES `Monthly Installments`

@export default Retrieve the payment obligations of Jane Dough Pte Ltd to John Doe based on their promissory note contract from September 2 2024
GIVEN `Outstanding Payment Amount` IS A Money @desc Amount already paid by Jane Dough Pte Ltd to date
GIVETH A PROVISION OF Borrower, `Payment Action`
`Payment Obligations` MEANS
    IF `Outstanding Payment Amount`'s Value GREATER THAN 0
        THEN  PARTY   `The Borrower`
              MUST    `pay monthly installment to`
                          `The Lender`
                          `Amount Transferred` PROVIDED
                             `Amount Transferred`'s Currency EQUALS `Next Payment Due`'s Amount's Currency
                                AND `Amount Transferred`'s Value AT LEAST (`Next Payment Due`'s Amount's Value
                                                                    MINUS 0.05)
              WITHIN  `Next Payment Due Date`
              HENCE   `Payment Obligations` -- Recursive function call
                          (Money WITH
                              Currency  IS `Monthly Installment Amount`'s Currency
                              Value     IS `Outstanding Payment Amount`'s Value
                                                MINUS `Amount Transferred`'s Value)
              LEST     PARTY   `The Borrower`
                       MUST    `pay monthly installment to`
                                  `The Lender`
                                  `Amount Transferred` PROVIDED
                                      `Amount Transferred`'s Currency EQUALS `Next Payment Due Amount With Penalty`'s Currency
                                          AND `Amount Transferred`'s Value AT LEAST (`Next Payment Due Amount With Penalty`'s Value
                                                                                MINUS 0.05)
                       WITHIN  `Default After Days Beyond Commencement`
        ELSE  FULFILLED
    WHERE
        `Installments Covered Since Commencement` MEANS
            (`Total Repayment Amount`'s Value
                MINUS `Outstanding Payment Amount`'s Value)
                    DIVIDED BY `Monthly Installment Amount`'s Value

        `Next Payment Due` MEANS
            Payment WITH
                Amount IS
                    Money WITH
                        Currency  IS `Monthly Installment Amount`'s Currency
                        Value     IS `The lesser of`
                                         (`Monthly Installment Amount`'s Value)
                                         (`Outstanding Payment Amount`'s Value)
                `Days Beyond Commencement` IS
                    (`Installments Covered Since Commencement` PLUS 1) TIMES 30

        `Next Payment Due Date` MEANS
            `Next Payment Due`'s `Days Beyond Commencement`
               PLUS `Late Payment Penalty`'s `Grace Period Days`

        `Next Payment Due Amount With Penalty` MEANS
            Money WITH
                Currency  IS `Next Payment Due`'s Amount's Currency
                Value     IS `Next Payment Due`'s Amount's Value
                                 PLUS `Next Payment Due`'s Amount's Value
                                      TIMES `Late Payment Penalty`'s `Interest Rate`

        `Default After Days Beyond Commencement` MEANS
            `Next Payment Due`'s `Days Beyond Commencement`
                PLUS `Default After Days Not Paid Beyond Due`

§§ `Definitions`

DECLARE Money HAS
    Currency  IS A STRING
    Value     IS A NUMBER

GIVEN a IS A NUMBER
USD MEANS
   Money WITH
      Currency IS "USD"
      Value    IS a

DECLARE Company HAS
    `Name` IS A STRING

DECLARE `Natural Person as defined in Reg 4.3a` HAS @ref "Reg 4.3a - Natural Person"
    `Name` IS A STRING

DECLARE Borrower IS ONE OF
   `Individual Borrower`
        HAS Individual      IS A `Natural Person as defined in Reg 4.3a`
   `Commercial Borrower`
        HAS Entity          IS A Company

DECLARE Lender IS ONE OF
   `Individual Lender`
        HAS Individual      IS A `Natural Person as defined in Reg 4.3a`
   `Commercial Lender`
        HAS Entity          IS A Company

DECLARE Payment HAS
    Amount                     IS A Money
    `Days Beyond Commencement` IS A NUMBER

DECLARE Penalty HAS
    `Interest Rate`     IS A NUMBER
    `Grace Period Days` IS A NUMBER

§§ `Action Types (for actor based regulative obligations or actions)`

DECLARE `Payment Action` IS ONE OF
    `pay monthly installment to` HAS
        Recipient   IS A Lender
        Amount      IS A Money

§§ `Helpers`

GIVEN a IS A NUMBER
      b IS A NUMBER
`The lesser of` MEANS
    IF a GREATER THAN b
        THEN b
        ELSE a

ASSUME NaN IS A NUMBER

GIVEN base  IS A NUMBER
      exp   IS A NUMBER
`Base to the power of` MEANS
    IF      exp EQUALS 0
    THEN    1
    ELSE    IF      base EQUALS 0
            THEN    IF      exp LESS THAN 0
                    THEN    NaN
                    ELSE    0
            ELSE    IF      exp EQUALS 1
                    THEN    base
                    ELSE    IF      exp GREATER THAN 0
                            THEN    base TIMES `Base to the power of` base (exp MINUS 1)
                            ELSE    1 DIVIDED BY `Base to the power of` base (0 MINUS exp)

§§ `Testing`

#EVAL `Total Repayment Amount` -- Expect diagnostic result: Money OF "USD", 27077.49370354708
#EVAL `Monthly Interest Rate` -- Expect diagnostic result: 0.0125
#EVAL `Monthly Installment Amount` -- Expect diagnostic result: Money OF "USD", 2256.4578086289234
#TRACE `Payment Obligations` `Total Repayment Amount` AT 0 WITH
    PARTY `The Borrower` DOES `pay monthly installment to` `The Lender` (USD 2256.46) AT 29
    PARTY `The Borrower` DOES `pay monthly installment to` `The Lender` (USD 2256.46) AT 59
    PARTY `The Borrower` DOES `pay monthly installment to` `The Lender` (USD 2256.46) AT 89
    PARTY `The Borrower` DOES `pay monthly installment to` `The Lender` (USD 2256.46) AT 119
    PARTY `The Borrower` DOES `pay monthly installment to` `The Lender` (USD 2256.46) AT 149
    PARTY `The Borrower` DOES `pay monthly installment to` `The Lender` (USD 2256.46) AT 179
    PARTY `The Borrower` DOES `pay monthly installment to` `The Lender` (USD 2256.46) AT 209
    PARTY `The Borrower` DOES `pay monthly installment to` `The Lender` (USD 2256.46) AT 239
    PARTY `The Borrower` DOES `pay monthly installment to` `The Lender` (USD 2256.46) AT 269
    PARTY `The Borrower` DOES `pay monthly installment to` `The Lender` (USD 2256.46) AT 299
    PARTY `The Borrower` DOES `pay monthly installment to` `The Lender` (USD 2256.46) AT 329
    PARTY `The Borrower` DOES `pay monthly installment to` `The Lender` (USD 2256.46) AT 359 -- Expect diagnostic result: FULFILLED
#TRACE `Payment Obligations` `Total Repayment Amount` AT 0 WITH
    PARTY `The Borrower` DOES `pay monthly installment to` `The Lender` (USD 2256.46) AT 59 {- Expected diagnostic result: PARTY `The Borrower`
MUST `pay monthly installment to` `The Lender`
                                   `Amount Transferred` PROVIDED     (`Amount Transferred`'s Currency) EQUALS (`Next Payment Due Amount With Penalty`'s Currency)
AND (`Amount Transferred`'s Value) AT LEAST ((`Next Payment Due Amount With Penalty`'s Value) MINUS 0.05)
WITHIN `Default After Days Beyond Commencement`
HENCE FULFILLED -}
```
