# FOR ALL

Universal quantifier for introducing type variables in polymorphic type signatures. Used to declare that a value or function works for _any_ type.

## Syntax

```l4
FOR ALL typeVar
FOR ALL typeVar1 AND typeVar2
FOR ALL typeVar1 AND typeVar2 AND typeVar3 ...
```

## Purpose

`FOR ALL` introduces type variables (type parameters) in type signatures, enabling polymorphism. A polymorphic function can operate on values of any type without knowing the specific type in advance.

## Examples

**Example file:** [for-all-example.l4](for-all-example.l4)

### Single Type Variable

```l4
-- A function that works for any type 'a'
ASSUME identity IS
  FOR ALL a
  A FUNCTION FROM a TO a
```

This declares `identity` as a function that takes a value of any type and returns a value of the same type.

### Multiple Type Variables

```l4
-- The classic 'map' function for lists
ASSUME map IS
  FOR ALL a AND b
  A FUNCTION
    FROM      FUNCTION FROM a TO b
          AND LIST OF a
    TO    LIST OF b
```

This says: "For all types `a` and `b`, `map` is a function that takes:

- a function from `a` to `b`
- a list of `a` values

and returns a list of `b` values."

### With Custom Types

```l4
DECLARE Choice OF a, b
  IS ONE OF
    Left  HAS payload IS AN a
    Right HAS payload IS A  b

-- A function that handles either alternative
ASSUME choose IS
  FOR ALL a AND b AND c
  A FUNCTION
    FROM      FUNCTION FROM a TO c
          AND FUNCTION FROM b TO c
          AND Choice OF a, b
    TO    c
```

## Usage Context

`FOR ALL` is used with `ASSUME` to declare the type of external or primitive functions. It's the declaration-style way to express polymorphism.

### ASSUME vs GIVEN

There are two ways to write polymorphic functions:

**Declaration style (FOR ALL):** Declares a type signature without implementation

```l4
ASSUME map IS
  FOR ALL a AND b
  A FUNCTION
    FROM FUNCTION FROM a TO b AND LIST OF a
    TO LIST OF b
```

**Definition style (GIVEN ... IS A TYPE):** Defines a function with implementation

```l4
GIVEN a IS A TYPE
      b IS A TYPE
      f IS A FUNCTION FROM a TO b
      list IS A LIST OF a
GIVETH A LIST OF b
map f list MEANS
  CONSIDER list
  WHEN EMPTY THEN EMPTY
  WHEN x FOLLOWED BY xs THEN f x FOLLOWED BY map f xs
```

Both express the same polymorphic type, but:

- `FOR ALL` is for **type declarations** (no implementation)
- `GIVEN ... IS A TYPE` is for **function definitions** (with implementation)

## Type Variables

Type variables introduced by `FOR ALL`:

- Are conventionally single lowercase letters: `a`, `b`, `c`
- Can be any valid identifier
- Are scoped to the type signature
- Must be distinct within the same `FOR ALL`

```l4
-- Valid:
FOR ALL a AND b AND c

-- Invalid (duplicate):
-- FOR ALL a AND a  -- Error: duplicate type variable
```

## Common Patterns

### Identity Function

```l4
ASSUME identity IS
  FOR ALL a
  A FUNCTION FROM a TO a
```

### Constant Function

```l4
ASSUME const IS
  FOR ALL a AND b
  A FUNCTION FROM a AND b TO a
```

### Function Composition

```l4
ASSUME compose IS
  FOR ALL a AND b AND c
  A FUNCTION
    FROM      FUNCTION FROM b TO c
          AND FUNCTION FROM a TO b
    TO    FUNCTION FROM a TO c
```

### List Operations

```l4
ASSUME head IS
  FOR ALL a
  A FUNCTION FROM LIST OF a TO MAYBE a

ASSUME tail IS
  FOR ALL a
  A FUNCTION FROM LIST OF a TO LIST OF a

ASSUME filter IS
  FOR ALL a
  A FUNCTION
    FROM      FUNCTION FROM a TO BOOLEAN
          AND LIST OF a
    TO    LIST OF a
```

## NOT a Runtime Quantifier

**Important:** `FOR ALL` is a _type-level_ construct, not a runtime quantifier over lists or collections.

```l4
-- WRONG - FOR ALL is not for iterating over lists:
-- FOR ALL x IN myList CHECK x > 0  -- NOT VALID L4!

-- CORRECT - use prelude functions for list quantification:
all (GIVEN x YIELD x > 0) myList    -- Check all elements
any (GIVEN x YIELD x > 0) myList    -- Check if any element
```

## Comparison with Other Languages

| L4                | Haskell       | TypeScript | Java     |
| ----------------- | ------------- | ---------- | -------- |
| `FOR ALL a`       | `forall a.`   | `<T>`      | `<T>`    |
| `FOR ALL a AND b` | `forall a b.` | `<T, U>`   | `<T, U>` |

## Related Keywords

- **[GIVEN](../functions/GIVEN.md)** - Function parameters (including type parameters)
- **[TYPE](keywords.md)** - The kind of types
- **[ASSUME](ASSUME.md)** - Declare types without implementation
- **[FUNCTION](keywords.md)** - Function type constructor

## See Also

- **[Types Reference](README.md)** - Type system overview
- **[Polymorphic Types](README.md#polymorphic-types)** - LIST, MAYBE, EITHER
- **[Prelude Library](../libraries/prelude.md)** - Polymorphic standard functions
