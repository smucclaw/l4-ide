# JSON Schema CLI Specification

## Overview

This document specifies the `jl4-schema` CLI tool that generates JSON Schema documents from L4 programs based on `@export` annotations and type information.

## Motivation

L4 programs define typed functions with parameters. When exposing these functions via APIs (like the decision service), it's valuable to generate JSON Schema documents that describe:

- The input parameters expected by exported functions
- The types of those parameters
- Human-readable descriptions from `@desc` annotations

This enables:

- Automatic form generation for web UIs
- API documentation
- Input validation
- Integration with tools that consume JSON Schema

## Usage

```bash
jl4-schema <file.l4> [--function <name>] [--output <file.json>]
```

### Arguments

| Argument               | Required | Description                                                                                                                                                                 |
| ---------------------- | -------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `<file.l4>`            | Yes      | The L4 source file to process                                                                                                                                               |
| `--function <name>`    | No       | Name of a specific exported function to generate schema for. If omitted, generates schema for the default export (marked with `@export default`) or the first export found. |
| `--output <file.json>` | No       | Output file path. If omitted, writes to stdout.                                                                                                                             |

### Exit Codes

- `0`: Success
- `1`: Parse/typecheck error in L4 file
- `2`: No exports found in file

## L4 to JSON Schema Type Mapping

### Primitive Types

| L4 Type   | JSON Schema             |
| --------- | ----------------------- |
| `NUMBER`  | `{ "type": "number" }`  |
| `STRING`  | `{ "type": "string" }`  |
| `BOOLEAN` | `{ "type": "boolean" }` |
| `INTEGER` | `{ "type": "integer" }` |

### Date/Time Types

| L4 Type    | JSON Schema                                   |
| ---------- | --------------------------------------------- |
| `DATE`     | `{ "type": "string", "format": "date" }`      |
| `DATETIME` | `{ "type": "string", "format": "date-time" }` |

### Collection Types

| L4 Type                  | JSON Schema                                         |
| ------------------------ | --------------------------------------------------- |
| `LIST X`                 | `{ "type": "array", "items": <schema for X> }`      |
| `OPTIONAL X` / `MAYBE X` | `{ "oneOf": [<schema for X>, { "type": "null" }] }` |

### Record Types

L4 records map to JSON Schema objects:

```l4
DECLARE Person
    HAS name IS A STRING @desc The person's full name
        age  IS A NUMBER @desc The person's age in years
```

Becomes:

```json
{
  "type": "object",
  "properties": {
    "name": {
      "type": "string",
      "description": "The person's full name"
    },
    "age": {
      "type": "number",
      "description": "The person's age in years"
    }
  },
  "required": ["name", "age"]
}
```

### Enum Types (Sum Types)

L4 enums with no payload map to JSON Schema enum:

```l4
DECLARE Color IS ONE OF
    Red
    Green
    Blue
```

Becomes:

```json
{
  "type": "string",
  "enum": ["Red", "Green", "Blue"]
}
```

L4 enums with payloads map to discriminated unions:

```l4
DECLARE Shape IS ONE OF
    Circle HAS radius IS A NUMBER
    Rectangle HAS width IS A NUMBER
              HAS height IS A NUMBER
```

Becomes:

```json
{
  "oneOf": [
    {
      "type": "object",
      "properties": {
        "tag": { "const": "Circle" },
        "radius": { "type": "number" }
      },
      "required": ["tag", "radius"]
    },
    {
      "type": "object",
      "properties": {
        "tag": { "const": "Rectangle" },
        "width": { "type": "number" },
        "height": { "type": "number" }
      },
      "required": ["tag", "width", "height"]
    }
  ]
}
```

### Type Synonyms

Type synonyms are expanded to their underlying types.

## Schema Structure

The generated schema for an exported function follows this structure:

```json
{
  "$schema": "https://json-schema.org/draft/2020-12/schema",
  "title": "<function name>",
  "description": "<@export/@desc annotation text>",
  "type": "object",
  "properties": {
    "<param1>": { ... },
    "<param2>": { ... }
  },
  "required": ["<param1>", "<param2>"],
  "$defs": {
    "<TypeName1>": { ... },
    "<TypeName2>": { ... }
  }
}
```

## Example

Given the L4 file:

```l4
@desc A person's information
DECLARE Person
    HAS name    IS A STRING @desc The person's full name
        age     IS A NUMBER @desc Age in years
        email   IS A STRING @desc Contact email

@export default Calculate eligibility for a person
GIVEN person IS A Person @desc The applicant's information
      threshold IS A NUMBER @desc Minimum age threshold
DECIDE isEligible IF
    person's age >= threshold
```

Running `jl4-schema example.l4` produces:

```json
{
  "$schema": "https://json-schema.org/draft/2020-12/schema",
  "title": "isEligible",
  "description": "Calculate eligibility for a person",
  "type": "object",
  "properties": {
    "person": {
      "$ref": "#/$defs/Person",
      "description": "The applicant's information"
    },
    "threshold": {
      "type": "number",
      "description": "Minimum age threshold"
    }
  },
  "required": ["person", "threshold"],
  "$defs": {
    "Person": {
      "type": "object",
      "description": "A person's information",
      "properties": {
        "name": {
          "type": "string",
          "description": "The person's full name"
        },
        "age": {
          "type": "number",
          "description": "Age in years"
        },
        "email": {
          "type": "string",
          "description": "Contact email"
        }
      },
      "required": ["name", "age", "email"]
    }
  }
}
```

## Implementation Notes

### Dependencies

The implementation uses:

- `aeson` for JSON encoding
- `optparse-applicative` for CLI argument parsing
- Existing `jl4-core` and `jl4-lsp` libraries for parsing and typechecking
- `L4.Export` module for extracting exported functions

### Type Environment

To resolve custom types (records, enums) referenced in function parameters, the implementation needs access to the type environment after typechecking. This is available via the `TypeCheckResult` which contains all declared types.

### Recursive Types

For recursive types, we use JSON Schema `$ref` to avoid infinite expansion:

```l4
DECLARE Tree
    HAS value IS A NUMBER
        left  IS A OPTIONAL Tree
        right IS A OPTIONAL Tree
```

### Schema Validation

The generated schema should be valid according to JSON Schema Draft 2020-12.

## Future Enhancements

1. **Multiple exports**: Generate schemas for all exports in one file
2. **OpenAPI integration**: Generate OpenAPI 3.0 specification wrapping the schemas
3. **Custom annotations**: Support `@schema` annotation for JSON Schema-specific metadata (e.g., `@schema minimum 0`, `@schema pattern "^[a-z]+$"`)
4. **Output formats**: Support YAML output in addition to JSON

## Related Work

- `jl4-decision-service` already does some type-to-JSON conversion in `Server.hs`
- The `L4.Export` module provides the foundation for identifying exported functions
