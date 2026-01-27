# CONSIDER

Pattern matching construct that examines a value and executes different branches based on its structure.

## Syntax

```l4
CONSIDER expression
WHEN pattern1 THEN result1
WHEN pattern2 THEN result2
...
```

## Purpose

CONSIDER enables pattern matching on algebraic types (enums and records), lists, and other structured data. It's similar to `case` or `match` in other functional languages.

## Examples

**Example file:** [consider-example.l4](consider-example.l4)

### Matching Enums

```l4
DECLARE Colour IS ONE OF red, green, blue

GIVEN c IS A Colour
colourName c MEANS
  CONSIDER c
  WHEN red THEN "Red"
  WHEN green THEN "Green"
  WHEN blue THEN "Blue"
```

### Matching Lists

```l4
GIVEN xs IS A LIST OF NUMBER
GIVETH A NUMBER
sumList xs MEANS
  CONSIDER xs
  WHEN EMPTY THEN 0
  WHEN head FOLLOWED BY tail THEN head PLUS sumList tail
```

### Matching MAYBE Values

```l4
GIVEN mx IS A MAYBE NUMBER
getOrDefault mx MEANS
  CONSIDER mx
  WHEN NOTHING THEN 0
  WHEN JUST x THEN x
```

### Matching Constructors with Fields

```l4
DECLARE Shape IS ONE OF
  Circle HAS radius IS A NUMBER
  Rectangle HAS width IS A NUMBER, height IS A NUMBER

GIVEN s IS A Shape
area s MEANS
  CONSIDER s
  WHEN Circle r THEN 3.14159 TIMES r TIMES r
  WHEN Rectangle w h THEN w TIMES h
```

### Using Wildcards

Use `_` to ignore values:

```l4
GIVEN xs IS A LIST OF NUMBER
firstElement xs MEANS
  CONSIDER xs
  WHEN EMPTY THEN 0
  WHEN head FOLLOWED BY _ THEN head
```

## OTHERWISE

Use OTHERWISE for a catch-all pattern:

```l4
GIVEN n IS A NUMBER
describe n MEANS
  CONSIDER n
  WHEN 0 THEN "zero"
  WHEN 1 THEN "one"
  OTHERWISE "many"
```

## Related Keywords

- **[IF](IF.md)** - Simple conditional alternative
- **[CONTROL-FLOW](CONTROL-FLOW.md)** - All control flow keywords

> Note: WHEN, OTHERWISE, and BRANCH are part of the CONSIDER syntax.
