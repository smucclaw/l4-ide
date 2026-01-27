# ASSUME

Declares a variable or function with a specified type, without providing a definition. Used for external values or assumptions about inputs.

## Syntax

```l4
ASSUME name IS A Type
ASSUME name IS A FUNCTION FROM Type1 TO Type2
```

## Purpose

ASSUME is used to:
1. Declare input variables for decision logic
2. Declare external functions whose implementation is provided elsewhere
3. State assumptions about values that will be provided at runtime

## Examples

**Example file:** [assume-example.l4](assume-example.l4)

### Basic Variable Assumptions

```l4
-- Assume a boolean input
ASSUME isEmployed IS A BOOLEAN

-- Assume a numeric value
ASSUME income IS A NUMBER

-- Assume a string
ASSUME applicantName IS A STRING
```

### Function Assumptions

```l4
-- Assume an external function
ASSUME calculateTax IS A FUNCTION FROM NUMBER TO NUMBER

-- Multi-parameter function (using AND)
ASSUME addNumbers IS A FUNCTION FROM NUMBER AND NUMBER TO NUMBER
```

### Using Assumed Values

```l4
ASSUME age IS A NUMBER
ASSUME income IS A NUMBER

DECIDE isEligible IS
  age >= 18 AND income > 50000
```

## Behavior

- Assumed values can be used in expressions but have no defined value
- When evaluated directly, assumed values remain symbolic
- Assumed values are typically bound via `#CHECK ... WITH` or `#TRACE ... WITH`

## Binding Assumed Values

```l4
ASSUME age IS A NUMBER

DECIDE isAdult IS age >= 18

-- Bind the assumed value for checking
#CHECK isAdult WITH age IS 25
```

## Related Keywords

- **[DECIDE](DECIDE.md)** - Define a value or function with a body
- **[GIVEN](GIVEN.md)** - Introduce function parameters
- **[IS](IS.md)** - Type assertion
- **[FUNCTION](FUNCTION.md)** - Function types

## See Also

- **[Types Reference](../types/README.md)** - Type syntax
