# Mixfix Operators in L4

## Motivation

L4 aims to be a CNL (Controlled Natural Language) DSL for legal specifications. To achieve natural language readability, we need to support function application patterns that go beyond traditional prefix notation. This spec describes **mixfix operators** - user-defined functions that can be called with infix, postfix, or more complex patterns.

Key goals:
- **Natural readability**: `alice is eligible for healthcare` reads better than `isEligibleFor alice healthcare`
- **Type safety**: Types disambiguate application patterns
- **Low-code friendly**: No need to declare precedence or associativity explicitly
- **Compositional**: Complex patterns emerge from simple building blocks

## Design Overview

### Core Idea

At **definition site**, use explicit underscores (`_`) to show where arguments go:
```l4
GIVEN person IS A Person, program IS A Program
GIVETH Bool
_ `is eligible for` _ MEANS
  person's age >= 18 AND ...
```

At **call site**, use the same pattern:
```l4
`alice` `is eligible for` `healthcare`
```

The parser/type-checker recognizes this as a mixfix application by matching the pattern against known function signatures.

### Key Design Decisions

1. **Explicit underscores** mark parameter positions (no inference from GIVEN)
2. **No underscores = traditional prefix** (backward compatible)
3. **No implicit precedence** - require parentheses or indentation for disambiguation
4. **Type-directed matching** - types help select the right function
5. **Leverages existing scanning phase** - all signatures collected before type checking

## Syntax

### Definition Site

#### Mixfix Function (with underscores)
```l4
GIVEN param1 IS A Type1, param2 IS A Type2, ...
GIVETH ResultType
pattern with _ and _ MEANS body
```

Where:
- Underscores (`_`) mark parameter positions
- Keywords (backticked identifiers) are between/around underscores
- Parameters are matched to underscores by **position** (first `_` → first GIVEN param, etc.)

#### Traditional Prefix Function (no underscores)
```l4
GIVEN x IS A Number, y IS A Number
GIVETH Number
`add` MEANS x + y
```
Called as: `add` 5 3

### Pattern Forms

#### Infix (binary)
```l4
GIVEN a IS A Number, b IS A Number
GIVETH Number
_ `plus` _ MEANS a + b
```
Call: `3 `plus` 5`

#### Postfix (unary)
```l4
GIVEN amount IS A Number
GIVETH Number
_ `percent` MEANS amount / 100
```
Call: `50 `percent``

#### Prefix (unary)
```l4
GIVEN x IS A Number
GIVETH Number
`negate` _ MEANS -x
```
Call: `negate` `5`

#### Ternary Mixfix
```l4
GIVEN condition IS A Bool, thenBranch IS A Text, elseBranch IS A Text
GIVETH Text
`if` _ `then` _ `else` _ MEANS
  -- body
```
Call: `if` eligible `then` "approved" `else` "denied"

#### N-ary Mixfix
```l4
GIVEN mummy IS A Person, daddy IS A Person, baby IS A Person
GIVETH Person
_ `copulated with` _ `to make` _ MEANS
  -- body
```
Call: `alice` `copulated with` `bob` `to make` `charlie`

### Call Site Syntax

At call sites, use the same pattern as the definition:
```l4
argument `keyword` argument `keyword` argument
```

All identifiers use backticks (standard L4 syntax for identifiers with whitespace).

## Precedence and Disambiguation

**No implicit precedence rules.** When multiple mixfix patterns could apply, require explicit grouping.

### Use Parentheses
```l4
(`alice` `married to` `bob`) `and has child` `charlie`
```

### Use Indentation
```l4
result MEANS
    `alice` `married to` `bob`
  `and has child` `charlie`
```

If indentation clearly groups subexpressions, that provides disambiguation.

## Type Checking

### Pattern Matching Algorithm

When type-checking a sequence of tokens `[t1, t2, ..., tn]`:

1. **Collect candidate functions**: Find all mixfix functions whose keywords appear in the sequence
2. **Try pattern matching**: For each candidate, try to match the pattern:
   - Extract keywords at their positions
   - Extract arguments between/around keywords
   - Check if the number of arguments matches the function's arity
3. **Type check arguments**: For each successful pattern match, type-check the arguments
4. **Unique solution**: Should find exactly one valid interpretation
   - Zero matches → error: no function found
   - Multiple matches → error: ambiguous, need parentheses

### Pattern Matching Example

Pattern: `_ copulated with _ to make _`
Keywords: `["copulated with", "to make"]`
Arity: 3

Token sequence: `[alice, copulated with, bob, to make, charlie]`

1. Find keywords at indices [1, 3]
2. Extract arguments:
   - Before first keyword (index 0-1): `alice`
   - Between keywords (index 2-3): `bob`
   - After last keyword (index 4-5): `charlie`
3. Type check: `alice : Person`, `bob : Person`, `charlie : Person` ✓
4. Apply: `copulated_with_to_make alice bob charlie`

### Integration with Existing Type Checker

L4 already has a multi-phase type checking process:

**Phase 1 (Scanning)**: `scanFunSigModule`
- Collect all function signatures
- **NEW**: Extract mixfix patterns from function definitions
- Store pattern info in `FunTypeSig`

**Phase 2 (Type Checking)**: `inferProgram`
- When encountering `expr = name+` (sequence of names)
- **NEW**: Try mixfix pattern matching
- Use types to verify correctness

## Implementation Plan

### Phase 1: Extend AST and Parser

1. **Extend `AppForm` syntax** to include underscore positions
2. **Update parser** to handle `_` tokens in patterns
3. **Parse pattern structure**: `_ keyword _ keyword _` → structured representation

### Phase 2: Scanning Phase Enhancement

1. **Extract mixfix info** from `AppForm` during `scanFunSigDecide`
2. **Store in `FunTypeSig`**:
   ```haskell
   data MixfixInfo = MixfixInfo
     { keywords :: [Name]
     , arity :: Int
     , positions :: [KeywordPosition]
     }
   ```
3. **Build global registry** of mixfix functions

### Phase 3: Type Checker Enhancement

1. **Pattern matching function**: `tryMixfixApplication :: [Name] -> Check [(Expr Resolved, Type)]`
2. **Integrate with `inferExpr`**: When seeing sequence of names, try mixfix interpretation
3. **Error handling**: Ambiguity, type mismatches, no matches

### Phase 4: Testing

1. **Unit tests** for pattern extraction
2. **Golden tests** for various mixfix patterns
3. **Error message tests**
4. **Integration with existing operators**

## Examples

### Example 1: Simple Infix
```l4
GIVEN a IS A Number, b IS A Number
GIVETH Number
_ `plus` _ MEANS a + b

GIVEN a IS A Text, b IS A Text
GIVETH Text
_ `plus` _ MEANS a ++ b

#EVAL 3 `plus` 5 = 8
#EVAL "hello" `plus` "world" = "helloworld"
```

Type-directed name resolution picks the right version.

### Example 2: Eligibility Check
```l4
DECLARE Person HAS name IS A String, age IS A Number
DECLARE Program HAS minAge IS A Number

GIVEN person IS A Person, program IS A Program
GIVETH Bool
_ `is eligible for` _ MEANS
  person's age >= program's minAge

alice MEANS Person WITH name IS "Alice", age IS 25
healthcare MEANS Program WITH minAge IS 18

#EVAL alice `is eligible for` healthcare = TRUE
```

### Example 3: Range Check
```l4
GIVEN lower IS A Number, value IS A Number, upper IS A Number
GIVETH Bool
_ `<=` _ `<=` _ MEANS
  lower <= value AND value <= upper

#EVAL 0 `<=` 5 `<=` 10 = TRUE
#EVAL 0 `<=` 15 `<=` 10 = FALSE
```

### Example 4: Complex Pattern
```l4
GIVEN list IS A List OF A, start IS A Number, end IS A Number
GIVETH List OF A
_ `from` _ `to` _ MEANS
  -- implementation

myList MEANS LIST 1, 2, 3, 4, 5, 6, 7, 8, 9, 10

#EVAL myList `from` 3 `to` 7 = LIST 3, 4, 5, 6, 7
```

## Backward Compatibility

- **Existing code unaffected**: Functions without underscores work as before (prefix)
- **Built-in operators unchanged**: `AND`, `OR`, `+`, etc. continue to work
- **Gradual adoption**: Can mix prefix and mixfix styles

## Limitations and Future Work

### Current Limitations

1. **No automatic precedence**: Must use parentheses to disambiguate
2. **No operator overloading on arity**: Can't have both `_ plus _` and `plus _ _ _`
3. **Pattern must be contiguous**: Can't skip tokens

### Future Enhancements

1. **Precedence hints**: Optional annotations for common cases
2. **Section syntax**: Partial application like Haskell's `(+ 5)`
3. **Operator composition**: Define new operators in terms of existing ones
4. **Smart disambiguation**: Use heuristics when types alone don't suffice

## Related Work

- **Agda**: Full mixfix with explicit precedence declarations
- **Haskell**: Infix operators with backticks, explicit fixity
- **Smalltalk**: Keyword messages (similar to mixfix)
- **Raku**: Comprehensive operator definition syntax

L4's approach is unique in:
- **Automatic inference** of patterns from definition
- **Type-directed resolution** without explicit precedence
- **CNL-oriented** design for legal domain

## References

- GRAMMAR.md - L4 grammar specification
- TypeCheck.hs - L4 type checking implementation
- british-citizen-act.l4 - Example usage patterns
