# AKA

Provides alternate names (aliases) for a definition. Allows the same value or type to be referenced by multiple names.

## Syntax

```l4
name1 AKA name2, name3 MEANS expression
DECLARE TypeName AKA AliasName IS ...
```

## Purpose

AKA ("also known as") enables:

1. Creating synonyms for legal terminology
2. Supporting multiple naming conventions
3. Providing human-readable alternatives

## Examples

**Example file:** [aka-example.l4](aka-example.l4)

### Value Aliases

```l4
x AKA y, z MEANS TRUE

#EVAL x   -- TRUE
#EVAL y   -- TRUE
#EVAL z   -- TRUE
```

### Type Aliases

```l4
DECLARE B AKA YesNo IS BOOLEAN

-- Both names refer to the same type
GIVETH B
example1 MEANS TRUE

GIVETH YesNo
example2 MEANS FALSE
```

### Complex Type with Aliases

```l4
DECLARE Pair OF a, b AKA `2-tuple`
  HAS
    px IS AN a
    py IS A b

-- Use either name
GIVETH Pair OF NUMBER, NUMBER
test1 MEANS Pair WITH px IS 3, py IS 5

GIVETH `2-tuple` OF NUMBER, NUMBER
test2 MEANS Pair WITH px IS 10, py IS 20
```

## Use Cases

### Legal Terminology

```l4
-- The regulation uses "Authorized Representative"
-- but practitioners say "Agent"
DECLARE `Authorized Representative` AKA Agent
  HAS
    name IS A STRING
    licenseNumber IS A STRING
```

### Internationalization

```l4
-- Support English and abbreviated forms
DECLARE Value Added Tax AKA VAT, `sales tax`
  IS NUMBER
```

## Related Keywords

- **[DECLARE](DECLARE.md)** - Type declarations
- **[DECIDE](DECIDE.md)** - Value definitions
- **[MEANS](MEANS.md)** - Definition body

## See Also

- **[Types Reference](../types/README.md)** - Type system
