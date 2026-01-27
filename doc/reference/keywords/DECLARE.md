# DECLARE

Declares a new type in L4. Types can be records (product types), enums (sum types), or type synonyms.

## Syntax

```l4
DECLARE TypeName IS ...
DECLARE TypeName HAS ...
DECLARE TypeName IS ONE OF ...
```

## Forms

### Record Types (Product Types)

Records are types with named fields:

```l4
DECLARE Person
  HAS
    name IS A STRING
    age IS A NUMBER
```

### Enum Types (Sum Types)

Enums define a type with multiple alternatives:

```l4
DECLARE Colour IS ONE OF red, green, blue
```

Enum constructors can have fields:

```l4
DECLARE Shape IS ONE OF
  Circle HAS radius IS A NUMBER
  Rectangle HAS width IS A NUMBER, height IS A NUMBER
```

### Type Synonyms

Create an alias for an existing type:

```l4
DECLARE Age IS NUMBER
DECLARE PersonName IS STRING
```

## Examples

**Example file:** [declare-example.l4](declare-example.l4)

### Basic Record

```l4
DECLARE Customer
  HAS
    name IS A STRING
    email IS A STRING
    balance IS A NUMBER
```

### Parameterized Types

Types can have type parameters:

```l4
GIVEN a IS A TYPE
DECLARE Box
  HAS
    contents IS AN a
```

### Field Syntax Variations

L4 supports multiple syntaxes for fields:

```l4
-- Using IS A
DECLARE Person1 HAS name IS A STRING

-- Using colon
DECLARE Person2 HAS name: STRING

-- Using colon with article
DECLARE Person3 HAS name: A STRING
```

## Related Keywords

- **[HAS](HAS.md)** - Introduces record fields
- **[IS](IS.md)** - Type assertion
- **[ONE](ONE.md)** - Used in enum declarations
- **[OF](OF.md)** - Type application

## See Also

- **[Types Reference](../types/README.md)** - Complete type system documentation
