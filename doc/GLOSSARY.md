# L4 Language Glossary

## A

- [**Algebraic Types**](30-algebraic-types.md#algebraic-types) - Ways to combine and structure data types
- [**Annotations**](20-basic-syntax.md#textual-annotations) - Paratextual information enclosed in `[square brackets]`
- [**AND**](10-boolean-logic.md#boolean-logic) - Logical conjunction operator
- [**Asyndetic Conjunction (`...`)**](20-basic-syntax.md#asyndetic-conjunction-) - The _semantics_ of implicit AND where the conjunction keyword is omitted; expressed using three-dot ellipsis _syntax_ (see also: Ellipsis)
- [**Asyndetic Disjunction (`..`)**](20-basic-syntax.md#asyndetic-disjunction-) - The _semantics_ of implicit OR where the disjunction keyword is omitted; expressed using two-dot ellipsis _syntax_
- [**ASSUME**](guide-index.md#assume) - Declares a variable with a specific type
- [**AT**](regulative.md#temporal-operators) - Temporal operator for specifying time points
- [**AT LEAST**](default-logic.md#default-reasoning) - Operator for minimum quantity conditions `>=`
- [**AT MOST**](default-logic.md#default-reasoning) - Operator for maximum quantity conditions `<=`

## B

- [**Basic Data Types**](10-data-types.md#basic-data-types) - Core data types including booleans, strings, numbers, and records
- [**Booleans**](10-data-types.md#booleans) - Basic true/false values represented as `TRUE` and `FALSE`
- [**Boolean Logic**](10-boolean-logic.md#boolean-logic) - Logical operators and conditional logic
- [**BY**](regulative.md#regulative-rules) - Specifies the method or means of an action

## C

- [**CHARAT**](10-data-types.md#strings) - String function to get character at index (STRING → NUMBER → STRING)
- [**Comments**](20-basic-syntax.md#comments) - Single-line comments `--` or block comments `{- ... -}`
- [**Conditional Logic**](10-boolean-logic.md#boolean-logic) - IF/THEN/ELSE structures
- [**CONSIDER**](10-data-types.md#destructuring-with-consider) - Pattern matching operator for type matching
- [**CONTAINS**](10-data-types.md#strings) - String operator to check if string contains substring (STRING → STRING → BOOLEAN)
- [**DECIDE**](25-functions.md#functions) - Keyword for defining decision functions
- [**DIVIDED BY**](10-data-types.md#numbers) - Division operator for numbers `÷`

## D

- [**Days and Dates**](libraries/daydate.md) - Library with functions for day and date math
- [**Default Logic**](default-logic.md#default-logic) - Predefined values and their handling
- [**Default Reasoning**](default-logic.md#default-reasoning) - Logic for handling unspecified cases
- [**DECLARE**](10-data-types.md#user-defined-types) - Keyword for defining new types
- [**Deontic Logic**](regulative.md#deontic-logic) - Logic of obligation and permission
- [**Ditto Syntax**](20-basic-syntax.md#ditto-syntax) - Using `^` to reference the word above in the same column
- [**DO**](regulative.md#regulative-rules) - Action operator in regulative rules
- [**DOES**](regulative.md#regulative-rules) - Alternative form of DO for actions

## E

- [**EITHER**](30-algebraic-types.md#algebraic-types) - Algebraic data type for representing two possible types (LEFT/RIGHT)
- [**Ellipsis (`...` / `..`)**](20-basic-syntax.md#asyndetic-conjunction-) - The _syntax_ for asyndetic operations: three dots (`...`) for AND, two dots (`..`) for OR; see _Asyndetic Conjunction_ and _Asyndetic Disjunction_
- [**ELSE**](10-boolean-logic.md#boolean-logic) - Alternative branch in conditional logic
- [**ENDSWITH**](10-data-types.md#strings) - String operator to check if string ends with suffix (STRING → STRING → BOOLEAN)
- [**EQUALS**](10-boolean-logic.md#boolean-logic) - Equality comparison operator
- [**EXACTLY**](default-logic.md#default-reasoning) - Operator for exact matching conditions
- [**EXPONENT**](10-data-types.md#numbers) - Exponentiation operator for numbers (base to the power of exponent)
- [**#EVAL**](25-functions.md#functions) - Directive for evaluating expressions inline

## F

- [**FALSE**](10-data-types.md#booleans) - Boolean false value
- [**FOLLOWED**](regulative.md#temporal-operators) - Temporal operator for sequence
- [**FOR**](regulative.md#regulative-rules) - Specifies purpose or target
- [**FROM**](regulative.md#regulative-rules) - Specifies source or origin
- [**Functions**](25-functions.md#functions) - Core building blocks for reusable code
- [**FULFILLED**](regulative.md#regulative-rules) - State indicating successful completion

## G

- [**GIVEN**](25-functions.md#functions) - Keyword for declaring function parameters
- [**GIVETH**](25-functions.md#functions) - Keyword for specifying return type
- [**GREATER THAN**](10-boolean-logic.md#boolean-logic) - Greater than comparison operator

## H

- [**HAS**](10-data-types.md#record-definition) - Keyword for defining record fields
- [**HENCE**](regulative.md#regulative-rules) - Consequence operator in regulative rules

## I

- [**Identifiers**](20-basic-syntax.md#identifiers) - Space-separated words enclosed in backticks
- [**IF**](10-boolean-logic.md#boolean-logic) - Conditional operator
- [**IMPLIES**](10-boolean-logic.md#boolean-logic) - Logical implication operator
- [**Inert Elements**](10-boolean-logic.md#inert-elements-grammatical-scaffolding) - String literals in boolean context that serve as grammatical scaffolding (TRUE in AND, FALSE in OR)
- [**IMPORT**](guide-index.md#import) - Keyword for importing external definitions
- [**INDEXOF**](10-data-types.md#strings) - String operator to find index of substring (STRING → STRING → NUMBER)
- [**IS**](10-data-types.md#user-defined-types) - Type assertion operator

## L

- [**LESS THAN**](10-boolean-logic.md#boolean-logic) - Less than comparison operator `<`
- [**LIST**](30-algebraic-types.md#algebraic-types) - Collection type for ordered elements
- [**LEST**](regulative.md#regulative-rules) - Alternative consequence operator
- [**Logical Operators**](10-boolean-logic.md#boolean-logic) - AND, OR, NOT operations

## M

- [**MAYBE**](30-algebraic-types.md#algebraic-types) - Algebraic data type for representing optional values (JUST/NOTHING)
- [**MEANS**](25-functions.md#functions) - Assignment operator for definitions
- [**MINUS**](10-data-types.md#numbers) - Subtraction operator `-`
- [**MODULO**](10-data-types.md#numbers) - Modulo operator for numbers `%`
- [**Math Library**](libraries/math.md) - Optional helpers such as `EULER`, `exp`, `ln`, and `log10`
- [**MUST**](regulative.md#regulative-rules) - Obligation operator in regulative rules
- [**Multitemporal Logic**](multitemporals.md#multitemporal-logic) - Handling time-based logic and constraints

## N

- [**NOT**](10-boolean-logic.md#boolean-logic) - Logical negation operator
- [**NOTHING**](default-logic.md#default-reasoning) - Represents absence of value
- [**Numbers**](10-data-types.md#numbers) - Numeric values (no distinction between integers and floats)
- [**@nlg**](guide-index.md#nlg) - Natural Language Generation annotation

## O

- [**OF**](10-data-types.md#record-definition) - Type composition operator
- [**ONE**](10-data-types.md#enumerations-or-enums) - Enumeration operator
- [**OR**](10-boolean-logic.md#boolean-logic) - Logical disjunction operator
- [**OTHERWISE**](10-boolean-logic.md#boolean-logic) - Alternative branch in conditional logic

## P

- [**PARTY**](regulative.md#regulative-rules) - Specifies actor in regulative rules
- [**PLUS**](10-data-types.md#numbers) - Addition operator `+`
- [**PROVIDED**](regulative.md#regulative-rules) - Alternative to IF for preconditions

## R

- [**RAND**](10-boolean-logic.md#boolean-logic) - Random AND operator
- [**REPLACE**](10-data-types.md#strings) - String function to replace all occurrences (STRING → STRING → STRING → STRING)
- [**ROR**](10-boolean-logic.md#boolean-logic) - Random OR operator
- [**Records**](10-data-types.md#records) - Composite data types that group related data together
- [**References**](20-basic-syntax.md#references) - Citations using `@` symbol
- [**@ref**](20-basic-syntax.md#references) - Reference annotation
- [**@ref-map**](20-basic-syntax.md#references) - Reference mapping annotation
- [**@ref-src**](20-basic-syntax.md#references) - Reference source annotation
- [**Regulative Rules**](regulative.md#regulative-rules) - Rules governing behavior and constraints
- [**Return Values**](25-functions.md#functions) - Output values from functions

## S

- [**#SEVAL**](25-functions.md#functions) - Directive for evaluating expressions in sections
- [**Shall Statements**](regulative.md#shall-statements) - Mandatory requirements and obligations
- [**SPLIT**](10-data-types.md#strings) - String operator to split by delimiter (STRING → STRING → LIST OF STRING)
- [**SQRT**](10-data-types.md#numbers) - Square root function for numbers
- [**STARTSWITH**](10-data-types.md#strings) - String operator to check if string starts with prefix (STRING → STRING → BOOLEAN)
- [**STARTING**](regulative.md#temporal-operators) - Temporal operator for beginning
- [**STRING LENGTH**](10-data-types.md#strings) - String function to get length (STRING → NUMBER)
- [**Strings**](10-data-types.md#strings-and-terms) - Text values enclosed in double quotes
- [**SUBSTRING**](10-data-types.md#strings) - String function to extract substring (STRING → NUMBER → NUMBER → STRING)

## T

- [**TBD**](future-features.md#future-features) - Placeholder for future implementation
- [**THE**](guide-index.md#the) - Article in type definitions
- [**THEN**](10-boolean-logic.md#boolean-logic) - Consequence operator in conditional logic
- [**Time-based Rules**](multitemporals.md#time-based-rules) - Rules that change over time
- [**TIMES**](10-data-types.md#numbers) - Multiplication operator `*`
- [**TO**](regulative.md#regulative-rules) - Destination or target operator
- [**TO LOWER**](10-data-types.md#strings) - String function to convert to lowercase (STRING → STRING)
- [**TO UPPER**](10-data-types.md#strings) - String function to convert to uppercase (STRING → STRING)
- [**TRIM**](10-data-types.md#strings) - String function to remove leading and trailing whitespace (STRING → STRING)
- [**TRUE**](10-data-types.md#booleans) - Boolean true value
- [**Types**](30-algebraic-types.md#algebraic-types) - Ways to combine and structure data types
- [**#TRACE**](25-functions.md#functions) - Directive for tracing execution

## W

- [**WHEN**](10-data-types.md#destructuring-with-consider) - Pattern matching operator
- [**WHERE**](25-functions.md#functions) - Local definition scope
- [**WITH**](10-data-types.md#record-definition) - Record construction operator
- [**WITHIN**](regulative.md#temporal-operators) - Temporal operator for duration

## Y

- [**YIELD**](25-functions.md#functions) - Return value operator
