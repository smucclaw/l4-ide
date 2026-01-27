# IMPORT

Imports definitions from another L4 file or library.

## Syntax

```l4
IMPORT filename
IMPORT `hyphenated-name`
```

## Purpose

IMPORT brings type definitions, functions, and values from another L4 source file into the current file's scope.

## Examples

**Example file:** [import-example.l4](import-example.l4)

### Importing Core Libraries

```l4
IMPORT math
IMPORT daydate
IMPORT currency
```

### Importing Hyphenated Names

Use backticks for names containing hyphens:

```l4
IMPORT `legal-persons`
IMPORT `excel-date`
```

### Importing Local Files

```l4
IMPORT myhelpers
IMPORT utils
```

## Available Libraries

L4 includes several standard libraries:

| Library         | Description                    |
| --------------- | ------------------------------ |
| `prelude`       | Core functions (auto-imported) |
| `math`          | Mathematical functions         |
| `daydate`       | Date calculations              |
| `excel-date`    | Excel date compatibility       |
| `currency`      | Currency handling              |
| `legal-persons` | Legal entity types             |
| `coercion`      | Type conversions               |

## Import Resolution

1. Prelude is automatically imported in all files
2. Library names resolve to `jl4-core/libraries/`
3. Relative names resolve to the current directory

## Related Keywords

- **[DECLARE](DECLARE.md)** - Define types to export
- **[DECIDE](DECIDE.md)** - Define functions to export

## See Also

- **[Libraries Reference](../libraries/README.md)** - Available libraries
