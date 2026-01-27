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
