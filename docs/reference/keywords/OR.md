# OR

Logical disjunction operator. Returns TRUE if either or both operands are TRUE.

## Syntax

```l4
expression1 OR expression2
expression1 || expression2
```

## Truth Table

| A | B | A OR B |
|---|---|--------|
| TRUE | TRUE | TRUE |
| TRUE | FALSE | TRUE |
| FALSE | TRUE | TRUE |
| FALSE | FALSE | FALSE |

## Examples

**Example file:** [or-example.l4](or-example.l4)

## Short-Circuit Evaluation

OR evaluates lazily - if the first operand is TRUE, the second is not evaluated.

## Related Keywords

- **[AND](AND.md)** - Logical conjunction
- **[NOT](NOT.md)** - Logical negation
- **[IMPLIES](IMPLIES.md)** - Logical implication

## See Also

- **[Logical Operators](../operators/README.md#logical-operators)** - All logical operators
