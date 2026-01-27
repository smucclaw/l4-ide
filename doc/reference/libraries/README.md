# L4 Core Libraries

L4 ships with a set of core libraries that provide essential functions for common tasks. These libraries are written in L4 itself and serve as both utilities and examples of idiomatic L4 code.

## Overview

Core libraries are located in the [jl4-core/libraries/](https://github.com/smucclaw/l4-ide/tree/main/jl4-core/libraries) directory:

- **[prelude](prelude.md)** - Standard functions (automatically imported)
- **[daydate](daydate.md)** - Date calculations and temporal logic
- **[excel-date](excel-date.md)** - Excel date compatibility
- **[math](math.md)** - Mathematical functions
- **[currency](currency.md)** - Currency handling (ISO 4217)
- **[legal-persons](legal-persons.md)** - Legal entity types and capacity
- **[jurisdiction](jurisdiction.md)** - Jurisdiction definitions
- **[holdings](holdings.md)** - Holdings and ownership
- **[date-compat](date-compat.md)** - Legacy DATE syntax compatibility
- **[llm](llm.md)** - LLM API integration

---

## Built-in Functions (Not a Library)

L4 includes several **built-in functions** that are always available without importing any library. These are implemented in the compiler core:

### Type Coercion Builtins

| Function    | Signature                      | Description                      |
| ----------- | ------------------------------ | -------------------------------- |
| `TOSTRING`  | `NUMBER/BOOLEAN/DATE → STRING` | Convert to string representation |
| `TONUMBER`  | `STRING → MAYBE NUMBER`        | Parse string to number           |
| `TODATE`    | `STRING → MAYBE DATE`          | Parse string to date             |
| `TRUNC`     | `NUMBER, NUMBER → NUMBER`      | Truncate decimal places          |
| `AS STRING` | `value AS STRING`              | Inline string conversion         |

### Numeric Builtins

| Function               | Description                       |
| ---------------------- | --------------------------------- |
| `FLOOR`                | Round down to integer             |
| `CEILING`              | Round up to integer               |
| `ROUND`                | Round to nearest integer          |
| `EXPONENT`             | Exponentiation (base, power)      |
| `LN`                   | Natural logarithm                 |
| `LOG10`                | Base-10 logarithm                 |
| `SQRT`                 | Square root                       |
| `SIN`, `COS`, `TAN`    | Trigonometric functions (radians) |
| `ASIN`, `ACOS`, `ATAN` | Inverse trigonometric functions   |

### String Builtins

| Function       | Description          |
| -------------- | -------------------- |
| `STRINGLENGTH` | Length of string     |
| `SUBSTRING`    | Extract substring    |
| `TOUPPER`      | Convert to uppercase |
| `TOLOWER`      | Convert to lowercase |

### Date Builtins

| Function           | Description                          |
| ------------------ | ------------------------------------ |
| `DATE_FROM_DMY`    | Construct DATE from day, month, year |
| `DATE_FROM_SERIAL` | Construct DATE from serial number    |
| `DATE_SERIAL`      | Get serial number from DATE          |
| `DATE_DAY`         | Extract day from DATE                |
| `DATE_MONTH`       | Extract month from DATE              |
| `DATE_YEAR`        | Extract year from DATE               |
| `TODAY`            | Current date                         |

For detailed coercion documentation, see [coercions.md](coercions.md).

---

## Using Libraries

Libraries require explicit import:

```l4
IMPORT daydate
IMPORT math
IMPORT currency
```

### Import Paths

Import by library name or file path:

```l4
IMPORT daydate                   -- Standard/core library
IMPORT "my-custom-lib.l4"        -- Custom library
```

---

## See Also

- **[GLOSSARY](../GLOSSARY.md)** - Language feature index
- **[Types](../types/README.md)** - Type system documentation
- **[Keywords](../keywords/README.md)** - Language keywords
- **[Specs](https://github.com/smucclaw/l4-ide/tree/main/specs)** - Technical specifications

---

## Contributing

To add or improve library documentation:

1. Check implementation in [jl4-core/libraries/](https://github.com/smucclaw/l4-ide/tree/main/jl4-core/libraries)
2. Add working examples
3. Document all functions with type signatures
4. Test thoroughly
5. Submit pull request

See [contributing guidelines](https://github.com/smucclaw/l4-ide/blob/main/CONTRIBUTING.md).
