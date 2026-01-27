# L4 Language Reference

The L4 language reference provides precise technical documentation for all language features. Use this section to look up specific keywords, types, operators, and syntax rules.

## Contents

### [📚 GLOSSARY](GLOSSARY.md)

Master index linking to all language features - start here to find what you need.

### Language Components

#### [Keywords](keywords/README.md)

Reserved words that form the structure of L4 programs:

- **Declarations:** ASSUME, DECIDE, DECLARE, MEANS
- **Functions:** GIVEN, GIVETH, WHERE, LET
- **Control Flow:** IF, THEN, ELSE, CONSIDER, WHEN
- **Logic:** AND, OR, NOT, IMPLIES
- **Regulative:** PARTY, MUST, MAY, SHANT, WITHIN, HENCE, LEST
- **And more...**

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
- **Temporal:** AT, WITHIN, STARTING, FOLLOWED

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

## How to Use This Reference

### Looking Up a Keyword

1. Check the [GLOSSARY](GLOSSARY.md) for an alphabetical index
2. Navigate to the specific keyword page for details and examples

### Understanding a Concept

- Each reference page includes links to related [Concepts](../concepts/README.md) for deeper understanding

### Finding Examples

- Working `.l4` example files are included alongside reference pages (e.g., in `types/`, `syntax/`, `operators/`)
- Examples are validated using the LSP server

### Learning More

- **New to L4?** Start with the [Foundation Course](../courses/foundation/README.md)
- **Need to accomplish a task?** Check the [Tutorials](../tutorials/README.md)
- **Want to understand why?** Explore the [Concepts](../concepts/README.md)

## Reference Standards

All reference documentation follows these principles:

- **Precision:** Accurate, unambiguous descriptions
- **Completeness:** Every language feature documented
- **Examples:** Multiple working code examples for each feature
- **Verification:** All examples are tested and working
- **Cross-linking:** Related features and concepts are linked

## Contributing

Found an error or want to improve the reference? Contributions are welcome via GitHub pull requests.
