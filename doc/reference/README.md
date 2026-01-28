# L4 Language Reference

The L4 language reference provides precise technical documentation for all language features. Use this section to look up specific keywords, types, operators, and syntax rules.

## Contents

### [📚 GLOSSARY](GLOSSARY.md)

Master index linking to all language features - start here to find what you need.

### Language Components

#### [Functions](functions/README.md)

Keywords for defining functions and computations:

- **Parameters:** GIVEN, GIVETH
- **Definitions:** DECIDE, MEANS
- **Local bindings:** LET, WHERE
- **Aliases:** AKA

#### [Types](types/README.md)

L4's type system for representing data:

- **Primitives:** BOOLEAN, NUMBER, STRING, DATE
- **Algebraic Types:** Records, Enums
- **Polymorphic Types:** LIST, MAYBE, EITHER
- **Type Constructors:** IS, HAS, ONE OF

#### [Operators](operators/README.md)

Operations for computing and comparing values:

- **Arithmetic:** PLUS, MINUS, TIMES, DIVIDED BY, MODULO
- **Comparison:** EQUALS, GREATER THAN, LESS THAN
- **Logical:** AND, OR, NOT, IMPLIES
- **String:** CONCAT
- **Temporal:** AT, WITHIN

#### [Syntax](syntax/README.md)

Structural rules and special syntax patterns:

- **Layout Rules:** Indentation-based grouping
- **Comments:** `--` and `{- -}`
- **Identifiers:** Backtick-quoted names
- **Annotations:** `@desc`, `@nlg`, `@ref`, `@export`
- **Directives:** `#EVAL`, `#TRACE`, `#CHECK`, `#ASSERT`
- **Special Syntax:** Ditto (`^`), Asyndetic (`...`, `..`)

### [Libraries](libraries/README.md)

Core libraries shipped with L4:

- **prelude:** Standard functions (auto-imported)
- **daydate:** Date calculations and temporal logic
- **excel-date:** Excel date compatibility
- **math:** Mathematical functions
- **currency:** Currency handling
- **legal-persons:** Legal entity types
- **jurisdiction:** Jurisdiction definitions
