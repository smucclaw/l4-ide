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

### Computed Fields (Methods)

Record fields can have a `MEANS` clause that defines a derived value — computed automatically from the record's other fields. These are analogous to **methods**, **calculated properties**, or **derived attributes** in other languages.

```l4
DECLARE Employee HAS
    -- stored fields (primary attributes)
    `name`             IS A STRING
    `date of birth`    IS A NUMBER
    `current year`     IS A NUMBER
    -- computed fields (derived attributes)
    `age`              IS A NUMBER
        MEANS `current year` - `date of birth`
    `adult`            IS A BOOLEAN
        MEANS `age` >= 18
```

**Key points:**

- Computed fields are accessed with `'s` just like stored fields: `employee's `age``
- Computed fields may depend on other computed fields (chaining)
- Computed fields may call external functions using OF syntax: `MEANS `f` OF `x`, `y``
- Computed fields may use WHERE and LET/IN for local bindings
- When constructing a record with WITH, only stored fields are supplied — computed fields are derived automatically

**Style guide:** Group stored fields first, then computed fields, so readers see the primary data before the derived logic. Depart from this convention when expository clarity calls for a different ordering — for instance, placing a computed field immediately after the stored fields it depends on.

**Example file:** [computed-fields-example.l4](computed-fields-example.l4)

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

- **[TYPE-KEYWORDS](keywords.md)** - Type-related keywords (IS, HAS, ONE OF)

## See Also

- **[Types Reference](../types/README.md)** - Complete type system documentation
