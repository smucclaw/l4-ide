# L4 Keywords Reference

Keywords are reserved words in L4 that have special meaning and cannot be used as identifiers. This section documents all L4 keywords organized by category.

## Overview

L4 has keywords for

- Declarations and definitions
- Functions and parameters  
- Control flow and pattern matching
- Logic and Boolean operations
- Type system constructs
- Regulative rules (legal obligations)
- Arithmetic and comparison
- External integrations

---

## Declaration Keywords

Used to declare types, variables, and functions.

- **[DECLARE](DECLARE.md)** - Declare a new type (record, enum, synonym)
- **[ASSUME](ASSUME.md)** - Declare a variable with optional type
- **[DECIDE](DECIDE.md)** - Define a decision function
- **[MEANS](MEANS.md)** - Define function body
- **[AKA](AKA.md)** - Provide alternate names
- **[IMPORT](IMPORT.md)** - Import from another file

--- 

## Function Keywords

Used in function definitions and lambda expressions.

- **[GIVEN](GIVEN.md)** - Introduce function parameters
- **[GIVETH](GIVETH.md)** - Specify return type
- **[GIVES](GIVETH.md)** - Synonym for GIVETH
- **[YIELD](TYPE-KEYWORDS.md)** - Return value in lambda
- **[WHERE](WHERE.md)** - Local declarations
- **[LET](LET.md)** - Local binding
- **[IN](LET.md)** - Scope of LET binding
- **[FUNCTION](TYPE-KEYWORDS.md)** - Function type constructor

---

## Control Flow Keywords

Used for conditional logic and pattern matching.

**See [Control Flow](CONTROL-FLOW.md) for overview and comparisons.**

- **[IF](IF.md)** - Conditional expression
- **[THEN](CONTROL-FLOW.md)** - Consequent branch
- **[ELSE](CONTROL-FLOW.md)** - Alternative branch
- **[OTHERWISE](CONTROL-FLOW.md)** - Default case
- **[CONSIDER](CONSIDER.md)** - Pattern matching
- **[WHEN](CONSIDER.md)** - Pattern match case
- **[BRANCH](CONTROL-FLOW.md)** - Multi-way IF alternative

---

## Logical Keywords

Boolean operators and logical operations.

- **[AND](AND.md)** - Conjunction (`&&`)
- **[OR](OR.md)** - Disjunction (`||`)
- **[NOT](NOT.md)** - Negation
- **[IMPLIES](IMPLIES.md)** - Implication (`=>`)
- **[RAND](REGULATIVE.md)** - Regulative AND (combines contracts)
- **[ROR](REGULATIVE.md)** - Regulative OR (combines contracts)

---

## Type Keywords

Used in type declarations and type expressions.

**See [Type Keywords](TYPE-KEYWORDS.md) for detailed documentation.**

- **[IS](TYPE-KEYWORDS.md)** - Type assertion/definition
- **[HAS](TYPE-KEYWORDS.md)** - Record fields
- **[ONE](TYPE-KEYWORDS.md)** OF - Enum variant declaration
- **[OF](TYPE-KEYWORDS.md)** - Type application/constructor
- **[WITH](TYPE-KEYWORDS.md)** - Named field construction
- **[A](ARTICLES.md)** / **[AN](ARTICLES.md)** / **[THE](ARTICLES.md)** - Type articles
- **[LIST](TYPE-KEYWORDS.md)** - List type/literal
- **[TYPE](TYPE-KEYWORDS.md)** - Kind of types
- **[FOR](TYPE-KEYWORDS.md)** **[ALL](TYPE-KEYWORDS.md)** - Universal quantification
- **[FUNCTION](TYPE-KEYWORDS.md)** **[FROM](TYPE-KEYWORDS.md)** **[TO](TYPE-KEYWORDS.md)** - Function types

---

## Regulative Keywords

For expressing legal rules, obligations, and permissions.

**See [Regulative Rules](REGULATIVE.md) for comprehensive documentation.**

### Deontic Modalities
- **[PARTY](PARTY.md)** - Legal party
- **[MUST](MUST.md)** - Obligation
- **[MAY](MAY.md)** - Permission
- **[SHANT](SHANT.md)** - Prohibition

### Rule Structure
- **[WITHIN](REGULATIVE.md)** - Temporal deadline
- **[HENCE](REGULATIVE.md)** - Consequence on fulfillment
- **[LEST](REGULATIVE.md)** - Consequence on breach
- **[PROVIDED](REGULATIVE.md)** - Guard condition
- **[BREACH](REGULATIVE.md)** - Violation marker
- **[BECAUSE](REGULATIVE.md)** - Justification for breach
- **[UNLESS](REGULATIVE.md)** - Negative condition

### Testing & Simulation
- **[DO](REGULATIVE.md)** / **[DOES](REGULATIVE.md)** - Action verb (in #TRACE)
- **[AT](REGULATIVE.md)** - Time specification
- **[STARTING](REGULATIVE.md)** - Start time for deadlines
- **[FOLLOWED](REGULATIVE.md)** - Sequence (with BY)

---

## Comparison Keywords

For comparing values.

**See [Comparisons](COMPARISONS.md) for detailed documentation and examples.**

- **[EQUALS](COMPARISONS.md)** - Equality (`=`)
- **[GREATER](COMPARISONS.md)** **[THAN](COMPARISONS.md)** - Greater than (`>`)
- **[LESS](COMPARISONS.md)** **[THAN](COMPARISONS.md)** - Less than (`<`)
- **[ABOVE](COMPARISONS.md)** - Synonym for GREATER THAN
- **[BELOW](COMPARISONS.md)** - Synonym for LESS THAN
- **[AT](COMPARISONS.md)** **[LEAST](COMPARISONS.md)** - Greater or equal (`>=`)
- **[AT](COMPARISONS.md)** **[MOST](COMPARISONS.md)** - Less or equal (`<=`)
- **[EXACTLY](COMPARISONS.md)** - Exact match in patterns

---

## Arithmetic Keywords

Mathematical operations.

**See [Arithmetic](ARITHMETIC.md) for detailed documentation and examples.**

- **[PLUS](ARITHMETIC.md)** - Addition (`+`)
- **[MINUS](ARITHMETIC.md)** - Subtraction (`-`)
- **[TIMES](ARITHMETIC.md)** - Multiplication (`*`)
- **[DIVIDED](ARITHMETIC.md)** **[BY](ARITHMETIC.md)** - Division (`/`)
- **[MODULO](ARITHMETIC.md)** - Remainder (`%`)

---

## Other Keywords

Miscellaneous functionality.

- **[FETCH](../libraries/README.md)** - HTTP GET request
- **[POST](../libraries/README.md)** - HTTP POST request
- **[ENV](../libraries/README.md)** - Environment variables
- **[CONCAT](../operators/README.md)** - String concatenation
- **[AS](TYPE-KEYWORDS.md)** - Type annotation/alias
- **[BE](TYPE-KEYWORDS.md)** / **[MEAN](TYPE-KEYWORDS.md)** - Alternative binding forms (reserved)

---

## Usage Patterns

### Multi-Word Keywords

Some keywords are used together:

- **ONE OF** - Enum declarations
- **GREATER THAN** / **LESS THAN** - Comparisons
- **DIVIDED BY** - Division
- **AT LEAST** / **AT MOST** - Inclusive comparisons
- **FOR ALL** - Universal quantification
- **FOLLOWED BY** - List cons operator

### Case Sensitivity

**All L4 keywords are case-sensitive and must be UPPERCASE.**

✅ Correct: `ASSUME age IS A NUMBER`  
❌ Wrong: `assume age is a number`

### Reserved vs. Fully Implemented

All 80 keywords are in the lexer. Some may have limited parser support:

- **[BE](TYPE-KEYWORDS.md)**, **[MEAN](TYPE-KEYWORDS.md)** - Alternative binding syntax (limited use)
- **[YIELD](TYPE-KEYWORDS.md)** - Lambda return (advanced)
- **[AS](TYPE-KEYWORDS.md)** - Type aliasing (context-dependent)

Check individual reference pages for implementation status and examples.

---

## Symbolic Alternatives

Many keywords have symbolic alternatives:

| Keyword | Symbol |
|---------|--------|
| AND | && |
| OR | \|\| |
| IMPLIES | => |
| PLUS | + |
| MINUS | - |
| TIMES | * |
| DIVIDED BY | / |
| EQUALS | = |
| GREATER THAN | > |
| LESS THAN | < |
| AT LEAST | >= |
| AT MOST | <= |

---

## Learning Path

### Beginners
Start with these essential keywords:
1. **[ASSUME](ASSUME.md)** - Declare variables
2. **[DECIDE](DECIDE.md)** / **[MEANS](MEANS.md)** - Define functions
3. **[IF](IF.md)** / **[THEN](CONTROL-FLOW.md)** / **[ELSE](CONTROL-FLOW.md)** - Conditionals
4. **[AND](AND.md)** / **[OR](OR.md)** / **[NOT](NOT.md)** - Logic
5. **[DECLARE](DECLARE.md)** / **[IS](TYPE-KEYWORDS.md)** / **[HAS](TYPE-KEYWORDS.md)** - Types

### Intermediate
Build on basics with:
- **[GIVEN](GIVEN.md)** / **[GIVETH](GIVETH.md)** - Function signatures
- **[CONSIDER](CONSIDER.md)** / **[WHEN](CONSIDER.md)** - Pattern matching
- **[WHERE](WHERE.md)** / **[LET](LET.md)** / **[IN](LET.md)** - Local scope
- **[LIST](TYPE-KEYWORDS.md)** - Polymorphic types

### Advanced
Master these for complex programs:
- **[PARTY](PARTY.md)** / **[MUST](MUST.md)** / **[MAY](MAY.md)** - Regulative rules
- **[WITHIN](REGULATIVE.md)** / **[AT](REGULATIVE.md)** / **[STARTING](REGULATIVE.md)** - Temporal logic
- **[FETCH](../libraries/README.md)** / **[POST](../libraries/README.md)** / **[ENV](../libraries/README.md)** - External I/O
- **[FOR](TYPE-KEYWORDS.md)** **[ALL](TYPE-KEYWORDS.md)** - Type quantification

---

## See Also

- **[GLOSSARY](../GLOSSARY.md)** - Complete language index
- **[Types Reference](../types/README.md)** - Type system documentation
- **[Operators Reference](../operators/README.md)** - Operator documentation
- **[Syntax Reference](../syntax/README.md)** - Syntax patterns

---

## Contributing

Found an error or want to add a keyword reference page?

1. Check the [Lexer.hs](https://github.com/smucclaw/l4-ide/blob/main/jl4-core/src/L4/Lexer.hs) for authoritative keyword list
2. Create examples as `.l4` files in this directory
3. Submit a pull request

All keyword reference pages should include:
- Clear explanation
- Role in the language
- 2-3 working examples
- Cross-links to related features
