# GIVEN

Introduces function parameters with their types. Used to declare what inputs a function accepts.

## Syntax

```l4
GIVEN name IS A Type

GIVEN name IS A Type, name2 IS A Type2

GIVEN name1 IS A Type1
      name2 IS A Type2
```

## Purpose

GIVEN declares the parameters of a function before the function definition. Each parameter has a name and a type.

## Examples

**Example file:** [given-example.l4](given-example.l4)

### Single Parameter

```l4
GIVEN x IS A NUMBER
double x MEANS x TIMES 2
```

### Multiple Parameters (Same Line)

```l4
GIVEN a IS A NUMBER, b IS A NUMBER
add a b MEANS a PLUS b
```

### Multiple Parameters (Continuation)

Use indentation to continue the parameter list:

```l4
GIVEN x IS A NUMBER
      y IS A NUMBER
      z IS A NUMBER
sum3 x y z MEANS x PLUS y PLUS z
```

### With Type Parameters

```l4
GIVEN a IS A TYPE
GIVEN xs IS A LIST OF a
length xs MEANS
  CONSIDER xs
  WHEN EMPTY THEN 0
  WHEN _ FOLLOWED BY tail THEN 1 PLUS length tail
```

### With Complex Types

```l4
DECLARE Person HAS name IS A STRING, age IS A NUMBER

GIVEN p IS A Person
getName p MEANS p's name
```

## Annotations

Parameters can have NLG annotations:

```l4
GIVEN customer IS A Person @nlg
GIVEN amount IS A NUMBER @nlg
processPayment customer amount MEANS ...
```

## Related Keywords

- **[GIVETH](GIVETH.md)** - Specifies return type
- **[GIVES](GIVES.md)** - Synonym for GIVETH
- **[DECIDE](DECIDE.md)** - Function definition
- **[MEANS](MEANS.md)** - Function body
- **[IS](IS.md)** - Type assertion

## See Also
- **[Types Reference](../types/README.md)** - Available types
