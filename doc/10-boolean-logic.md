# Boolean Logic

`TRUE` and `FALSE` values are combined using the operators `AND`, `OR`,
`NOT`, `ANY`, `ALL`.

Logical implication as a special case of syntax.

## Example: XOR Function

```l4
§ xor

GIVEN x IS A BOOLEAN, y IS A BOOLEAN
GIVETH A BOOLEAN
DECIDE xor x y IS
     x AND NOT y
  OR NOT x AND y
```
