# L4 Language Glossary

Complete reference index of all L4 language features. Links point to the consolidated reference pages which contain detailed documentation.

---

## Keywords

Keywords are reserved words that form the structure of L4 programs.

For complete documentation, see **[Keywords Reference](keywords/README.md)**.

### Declaration Keywords

| Keyword | Purpose | Reference |
|---------|---------|-----------|
| **DECLARE** | Declares a new type (record, enum, or synonym) | [DECLARE](keywords/DECLARE.md) |
| **ASSUME** | Declares a variable, optionally with a type | [ASSUME](keywords/ASSUME.md) |
| **DECIDE** | Defines a decision function | [DECIDE](keywords/DECIDE.md) |
| **MEANS** | Defines the body of a function or decision | [MEANS](keywords/MEANS.md) |
| **AKA** | Provides alternate names (aliases) | [AKA](keywords/AKA.md) |
| **IMPORT** | Imports definitions from another file | [IMPORT](keywords/IMPORT.md) |

### Function Keywords

| Keyword | Purpose | Reference |
|---------|---------|-----------|
| **GIVEN** | Introduces function parameters | [GIVEN](keywords/GIVEN.md) |
| **GIVETH** | Specifies function return type | [GIVETH](keywords/GIVETH.md) |
| **GIVES** | Synonym for GIVETH | [GIVETH](keywords/GIVETH.md) |
| **YIELD** | Returns a value in lambda expressions | [TYPE-KEYWORDS](keywords/TYPE-KEYWORDS.md) |
| **WHERE** | Introduces local declarations | [WHERE](keywords/WHERE.md) |
| **LET** | Introduces a local binding | [LET](keywords/LET.md) |
| **IN** | Used with LET for scoped bindings | [LET](keywords/LET.md) |
| **FUNCTION** | Declares a function type | [TYPE-KEYWORDS](keywords/TYPE-KEYWORDS.md) |

### Control Flow Keywords

| Keyword | Purpose | Reference |
|---------|---------|-----------|
| **IF** | Conditional expression | [IF](keywords/IF.md) |
| **THEN** | Consequent branch of IF | [CONTROL-FLOW](keywords/CONTROL-FLOW.md) |
| **ELSE** | Alternative branch of IF | [CONTROL-FLOW](keywords/CONTROL-FLOW.md) |
| **OTHERWISE** | Default case in CONSIDER | [CONTROL-FLOW](keywords/CONTROL-FLOW.md) |
| **CONSIDER** | Pattern matching on values | [CONSIDER](keywords/CONSIDER.md) |
| **WHEN** | Introduces a pattern match case | [CONSIDER](keywords/CONSIDER.md) |
| **BRANCH** | Alternative pattern matching keyword | [CONTROL-FLOW](keywords/CONTROL-FLOW.md) |

### Logical Keywords

| Keyword | Purpose | Reference |
|---------|---------|-----------|
| **AND** | Logical conjunction | [AND](keywords/AND.md) |
| **OR** | Logical disjunction | [OR](keywords/OR.md) |
| **NOT** | Logical negation | [NOT](keywords/NOT.md) |
| **IMPLIES** | Logical implication | [IMPLIES](keywords/IMPLIES.md) |
| **RAND** | Regulative AND | [REGULATIVE](keywords/REGULATIVE.md) |
| **ROR** | Regulative OR | [REGULATIVE](keywords/REGULATIVE.md) |

### Type Keywords

| Keyword | Purpose | Reference |
|---------|---------|-----------|
| **IS** | Type assertion or definition | [TYPE-KEYWORDS](keywords/TYPE-KEYWORDS.md) |
| **HAS** | Record field declaration | [TYPE-KEYWORDS](keywords/TYPE-KEYWORDS.md) |
| **ONE** | Used in "ONE OF" for enum types | [TYPE-KEYWORDS](keywords/TYPE-KEYWORDS.md) |
| **OF** | Type application or constructor pattern | [TYPE-KEYWORDS](keywords/TYPE-KEYWORDS.md) |
| **WITH** | Record construction with named fields | [TYPE-KEYWORDS](keywords/TYPE-KEYWORDS.md) |
| **A** / **AN** | Type articles | [ARTICLES](keywords/ARTICLES.md) |
| **THE** | Definite article for field access | [ARTICLES](keywords/ARTICLES.md) |
| **LIST** | List type or list literal | [TYPE-KEYWORDS](keywords/TYPE-KEYWORDS.md) |
| **TYPE** | The kind of types | [TYPE-KEYWORDS](keywords/TYPE-KEYWORDS.md) |
| **FOR ALL** | Universal type quantification | [TYPE-KEYWORDS](keywords/TYPE-KEYWORDS.md) |

### Regulative Keywords

For expressing legal obligations, permissions, and prohibitions.

| Keyword | Purpose | Reference |
|---------|---------|-----------|
| **PARTY** | Declares a legal party | [PARTY](keywords/PARTY.md) |
| **MUST** | Obligation (deontic necessity) | [MUST](keywords/MUST.md) |
| **MAY** | Permission (deontic possibility) | [MAY](keywords/MAY.md) |
| **SHANT** | Prohibition | [SHANT](keywords/SHANT.md) |
| **DO** / **DOES** | Action verb in regulative rules | [REGULATIVE](keywords/REGULATIVE.md) |
| **WITHIN** | Temporal deadline | [REGULATIVE](keywords/REGULATIVE.md) |
| **HENCE** | Consequence (then) | [REGULATIVE](keywords/REGULATIVE.md) |
| **LEST** | Negative consequence (else) | [REGULATIVE](keywords/REGULATIVE.md) |
| **BREACH** | Violation of obligation | [REGULATIVE](keywords/REGULATIVE.md) |
| **BECAUSE** | Justification or reason | [REGULATIVE](keywords/REGULATIVE.md) |
| **PROVIDED** | Condition or proviso | [REGULATIVE](keywords/REGULATIVE.md) |
| **AT** | Temporal specification | [REGULATIVE](keywords/REGULATIVE.md) |
| **STARTING** | Temporal start point | [REGULATIVE](keywords/REGULATIVE.md) |
| **FOLLOWED** | Temporal sequence or list cons | [REGULATIVE](keywords/REGULATIVE.md) |
| **FOR** | Duration or iteration | [REGULATIVE](keywords/REGULATIVE.md) |
| **UNLESS** | Negative condition | [REGULATIVE](keywords/REGULATIVE.md) |

### Comparison Keywords

| Keyword | Purpose | Reference |
|---------|---------|-----------|
| **EQUALS** | Equality test | [COMPARISONS](keywords/COMPARISONS.md) |
| **GREATER** | Greater than comparison | [COMPARISONS](keywords/COMPARISONS.md) |
| **LESS** | Less than comparison | [COMPARISONS](keywords/COMPARISONS.md) |
| **THAN** | Comparison conjunction word | [COMPARISONS](keywords/COMPARISONS.md) |
| **ABOVE** | Synonym for GREATER THAN | [COMPARISONS](keywords/COMPARISONS.md) |
| **BELOW** | Synonym for LESS THAN | [COMPARISONS](keywords/COMPARISONS.md) |
| **LEAST** | Used in "AT LEAST" (≥) | [COMPARISONS](keywords/COMPARISONS.md) |
| **MOST** | Used in "AT MOST" (≤) | [COMPARISONS](keywords/COMPARISONS.md) |
| **EXACTLY** | Exact match/precision modifier | [COMPARISONS](keywords/COMPARISONS.md) |

### Arithmetic Keywords

| Keyword | Purpose | Reference |
|---------|---------|-----------|
| **PLUS** | Addition | [ARITHMETIC](keywords/ARITHMETIC.md) |
| **MINUS** | Subtraction | [ARITHMETIC](keywords/ARITHMETIC.md) |
| **TIMES** | Multiplication | [ARITHMETIC](keywords/ARITHMETIC.md) |
| **DIVIDED** | Division (use with BY) | [ARITHMETIC](keywords/ARITHMETIC.md) |
| **BY** | Division conjunction word | [ARITHMETIC](keywords/ARITHMETIC.md) |
| **MODULO** | Modulus (remainder) | [ARITHMETIC](keywords/ARITHMETIC.md) |

### Other Keywords

| Keyword | Purpose | Reference |
|---------|---------|-----------|
| **ALL** | Universal quantification | [TYPE-KEYWORDS](keywords/TYPE-KEYWORDS.md) |
| **FETCH** | HTTP GET request | [Libraries](libraries/README.md) |
| **POST** | HTTP POST request | [Libraries](libraries/README.md) |
| **ENV** | Environment variable access | [Libraries](libraries/README.md) |
| **CONCAT** | String concatenation | [Operators](operators/README.md) |
| **AS** | Type annotation or alias | [TYPE-KEYWORDS](keywords/TYPE-KEYWORDS.md) |
| **BE** / **MEAN** | Alternative forms (reserved) | [TYPE-KEYWORDS](keywords/TYPE-KEYWORDS.md) |
| **FROM** / **TO** | Used in FUNCTION types and ranges | [TYPE-KEYWORDS](keywords/TYPE-KEYWORDS.md) |

---

## Types

L4's type system includes primitive types, algebraic types, and polymorphic types.

For complete documentation, see **[Types Reference](types/README.md)**.

### Primitive Types

| Type | Description |
|------|-------------|
| **NUMBER** | Numeric values (integers and rationals) |
| **STRING** | Text strings |
| **BOOLEAN** | Truth values (TRUE, FALSE) |
| **DATE** | Calendar dates |

### Algebraic Types

| Type | Description |
|------|-------------|
| **Records** | Product types with named fields |
| **Enums** | Sum types with named constructors |
| **PAIR** | Two-element tuple |

### Polymorphic Types

| Type | Description |
|------|-------------|
| **LIST** | Ordered collection of elements |
| **MAYBE** | Optional values (JUST x or NOTHING) |
| **EITHER** | Choice between two values (LEFT or RIGHT) |
| **Dictionary** | Key-value associative map |

### Special Types

| Type | Description |
|------|-------------|
| **TYPE** | The kind of types |
| **FUNCTION** | Function types |

---

## Operators

For complete documentation, see **[Operators Reference](operators/README.md)**.

### Symbolic Operators

| Operator | Textual Form | Description |
|----------|--------------|-------------|
| `*` | TIMES | Multiplication |
| `+` | PLUS | Addition |
| `-` | MINUS | Subtraction |
| `/` | DIVIDED BY | Division |
| `>=` | AT LEAST | Greater than or equal |
| `<=` | AT MOST | Less than or equal |
| `>` | GREATER THAN / ABOVE | Greater than |
| `<` | LESS THAN / BELOW | Less than |
| `=` | EQUALS | Equality |
| `&&` | AND | Logical conjunction |
| `\|\|` | OR | Logical disjunction |
| `=>` | IMPLIES | Logical implication |

### List Operators

| Operator | Description |
|----------|-------------|
| **FOLLOWED BY** | List cons (prepend element) |
| **EMPTY** | Empty list |

### String Operators

| Operator | Description |
|----------|-------------|
| **CONCAT** | String concatenation |
| **APPEND** | String concatenation (infix) |

### Temporal Operators

| Operator | Description |
|----------|-------------|
| **AT** | Point in time |
| **WITHIN** | Time duration constraint |
| **STARTING** | Starting time |
| **FOLLOWED** | Temporal sequence |

---

## Syntax Patterns

Special syntax features and patterns in L4.

For complete documentation, see **[Syntax Reference](syntax/README.md)**.

| Feature | Description |
|---------|-------------|
| **Layout Rules** | Indentation-based grouping |
| **Comments** | `--` line comments and `{- -}` block comments |
| **Identifiers** | Backtick-quoted identifiers |
| **Annotations** | `@desc`, `@nlg`, `@ref`, `@export` |
| **Directives** | `#EVAL`, `#TRACE`, `#CHECK`, `#ASSERT` |
| **Ditto** | `^` copy from previous line |
| **Asyndetic** | `...` (AND) and `..` (OR) implicit operators |
| **Genitive** | `'s` for record field access |
| **Section Markers** | `§` for document sections |

---

## Symbols

| Symbol | Name | Purpose |
|--------|------|---------|
| `()` | Parentheses | Grouping, tuples |
| `{}` | Braces | Block comments |
| `[]` | Square brackets | NLG inline annotations |
| `<<>>` | Double angles | Reference annotations |
| `§` | Section symbol | Document sections |
| `^` | Caret | Ditto (copy above) |
| `,` | Comma | Separator |
| `;` | Semicolon | Statement separator |
| `.` | Dot | Decimal point, punctuation |
| `...` | Ellipsis | Asyndetic AND |
| `..` | Double dot | Asyndetic OR |
| `:` | Colon | Type signature separator |
| `%` | Percent | Percentage, NLG delimiter |
| `'s` | Genitive | Possession/field access |

---

## Literals

| Literal Type | Syntax | Example |
|--------------|--------|---------|  
| **Integer** | Digits | `42`, `-17` |
| **Rational** | Digits with decimal point | `3.14`, `-0.5` |
| **String** | Double quotes | `"hello world"` |
| **Boolean** | TRUE or FALSE | `TRUE`, `FALSE` |
| **List** | LIST or FOLLOWED BY | `LIST 1, 2, 3` |

---

## Core Libraries

Libraries shipped with L4.

For complete documentation, see **[Libraries Reference](libraries/README.md)**.

| Library | Purpose |
|---------|---------|  
| **prelude** | Standard functions (always imported) |
| **daydate** | Date calculations and temporal logic |
| **excel-date** | Excel date compatibility |
| **math** | Mathematical functions |
| **currency** | ISO 4217 currency handling |
| **legal-persons** | Legal entity types |
| **jurisdiction** | Jurisdiction definitions |
| **llm** | LLM API integration |

### Built-in Coercion Functions

These are built into the compiler (not a library):

| Function | Purpose |
|----------|---------|  
| **TOSTRING** | Convert to STRING |
| **TONUMBER** | Convert to NUMBER |
| **TODATE** | Convert to DATE |
| **TRUNC** | Truncate number |

See [coercions documentation](libraries/coercions.md) for details.

---

## Directives

Compiler directives for testing and evaluation.

| Directive | Purpose |
|-----------|---------|  
| `#EVAL` | Evaluate and print expression |
| `#EVALTRACE` | Evaluate with execution trace |
| `#TRACE` | Contract/state graph tracing |
| `#CHECK` | Type check expression |
| `#ASSERT` | Assert truth value |

---

## Annotations

Metadata annotations for documentation and generation.

| Annotation | Purpose |
|------------|---------|  
| `@desc` | Human-readable description |
| `@nlg` | Natural language generation hint |
| `@ref` | Cross-reference to legal source |
| `@ref-src` | Source reference |
| `@ref-map` | Reference mapping |
| `@export` | Mark for export |

---

## Built-in Constants

| Constant | Type | Description |
|----------|------|-------------|
| **TRUE** | BOOLEAN | Boolean true value |
| **FALSE** | BOOLEAN | Boolean false value |
| **NOTHING** | MAYBE a | Absence of value |
| **JUST** | a → MAYBE a | Present value constructor |
| **LEFT** | a → EITHER a b | Left alternative |
| **RIGHT** | b → EITHER a b | Right alternative |
| **EMPTY** | LIST a | Empty list |

---

## Navigation

- **[Reference Home](README.md)** - Reference documentation overview
- **[Main Documentation](../README.md)** - Return to docs home
- **[Courses](../courses/README.md)** - Learning paths
- **[Tutorials](../tutorials/README.md)** - Task-oriented guides
- **[Concepts](../concepts/README.md)** - Understanding L4's design
- **[Specifications](https://github.com/smucclaw/l4-ide/tree/main/specs)** - Technical specifications

---

## Notes

This glossary reflects the current state of L4 as implemented in the jl4 compiler. Features marked as "reserved" exist in the lexer but may not be fully implemented.

For detailed information about any feature, follow the links to the consolidated reference pages.
