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

At **definition site**, use parameter names in the pattern:

```l4
GIVEN person IS A Person, program IS A Program
GIVETH Bool
person `is eligible for` program MEANS
  person's age >= 18 AND ...
```

At **call site**, use the same pattern structure:

```l4
`alice` `is eligible for` `healthcare`
```

The parser recognizes this as a mixfix pattern by identifying which names in the pattern are GIVEN parameters (those become argument positions) versus backticked keywords (those become the function name parts).

**Note**: Underscores (`_`) only appear in the **internal representation** of the function pattern (e.g., `_ copulated with _ to make _`). Users never write underscores - they write natural patterns using parameter names and keywords.

### Backticks Are Lexical, Not Semantic

Backticks simply allow identifiers to contain whitespace or punctuation; they do **not** tell the parser that something is an operator. When a parameter or keyword is a single word you may omit the backticks entirely:

```l4
salary exceedsMinimumFor category
n squared
```

Reserve backticks for identifiers whose spelling would otherwise be invalid:

```l4
`salary` `exceeds minimum for` `category`
interestRate `percent`
alice `copulated with` bob `to make` charlie
```

Multi-word keywords still require backticks because each keyword is scanned as a single identifier token.

### Key Design Decisions

1. **Parameters in pattern** - Use actual parameter names from GIVEN clause in the pattern
2. **Keyword identification** - Backticked names NOT in GIVEN are function name parts
3. **No implicit precedence** - require parentheses or indentation for disambiguation
4. **Type-directed matching** - types help select the right function at call sites
5. **Leverages existing scanning phase** - all signatures collected before type checking

## Syntax

### Definition Site

#### Mixfix Function

```l4
GIVEN param1 IS A Type1, param2 IS A Type2, ...
GIVETH ResultType
param1 `keyword` param2 MEANS body
```

Where:

- Parameter names from GIVEN appear in the pattern
- Backticked names NOT in GIVEN are keywords (function name parts)
- The pattern shows exactly how the function should be called

#### Traditional Prefix Function

```l4
GIVEN x IS A Number, y IS A Number
GIVETH Number
`add` MEANS x + y
```

If the pattern contains no GIVEN parameters, it's treated as prefix application.
Called as: `add` 5 3

### Pattern Forms

#### Infix (binary)

```l4
GIVEN a IS A Number, b IS A Number
GIVETH Number
a `plus` b MEANS a + b
```

Call: `3 `plus` 5`

#### Postfix (unary)

```l4
GIVEN amount IS A Number
GIVETH Number
amount `percent` MEANS amount / 100
```

Call: `50 `percent``

Since December 2025 the type checker reinterprets misparsed `App operand [keyword]` fragments, so `x percent` and `LET y BE 5 IN y percent` type-check without any extra parentheses or backticks. See `jl4/examples/ok/postfix-with-variables.l4` and `jl4/examples/ok/mixfix-with-variables.l4` for regression coverage.

#### Prefix (unary)

```l4
GIVEN x IS A Number
GIVETH Number
`negate` x MEANS -x
```

Call: `negate` `5`

#### Ternary Mixfix

```l4
GIVEN condition IS A Bool, thenBranch IS A Text, elseBranch IS A Text
GIVETH Text
`if` condition `then` thenBranch `else` elseBranch MEANS
  -- body
```

Call: `if` eligible `then` "approved" `else` "denied"

#### N-ary Mixfix

```l4
GIVEN mummy IS A Person, daddy IS A Person, baby IS A Person
GIVETH Person
mummy `copulated with` daddy `to make` baby MEANS
  -- body
```

Call: `alice` `copulated with` `bob` `to make` `charlie`

### Call Site Syntax

At call sites, use the same pattern as the definition:

```l4
argument `keyword` argument `keyword` argument
```

All identifiers may use backticks (standard L4 syntax for identifiers with whitespace).

Operands can be bare variables, literals, or expressions. Parentheses are only needed for grouping, not to convince the parser that a postfix interpretation is allowed.

## Precedence and Disambiguation

**No implicit precedence rules.** When multiple mixfix patterns could apply, require explicit grouping. This is made easy by L4's indentation conventions.

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

Given that either t1 or t2 will always be (the first part of) a function name, parsing lookahead is limited to +1.

### Pattern Matching Example

**Definition**:

```l4
GIVEN mummy IS A Person, daddy IS A Person, baby IS A Person
mummy `copulated with` daddy `to make` baby MEANS ...
```

**Internal representation**:

- Pattern: `_ copulated with _ to make _`
- Keywords: `["copulated with", "to make"]`
- Arity: 3

**Call site**: `[alice, copulated with, bob, to make, charlie]`

1. Find keywords at indices [1, 3]
2. Extract arguments:
   - Before first keyword (index 0-1): `alice`
   - Between keywords (index 2-3): `bob`
   - After last keyword (index 4-5): `charlie`
3. Type check: `alice : Person`, `bob : Person`, `charlie : Person` ✓
4. Apply: `copulated_with_to_make alice bob charlie`

### Postfix Reinterpretation Safety Net

The surface parser still prefers prefix application, so `x squared` initially arrives as `App x [squared]`. Before running the general mixfix matcher, the type checker (`jl4-core/src/L4/TypeCheck.hs`, `reinterpretPostfixAppIfNeeded`) detects this shape whenever `squared` (or any registered postfix keyword) expects a single operand. It then rewrites the AST to `App squared [x]`, preserving annotations so IDE tooling highlights the operator rather than the operand as the callee. This guarantees that bare variables, LET-bound names, and other expressions can participate in postfix mixfix calls without extra syntax.

Perhaps the internal representations will be some combination of the following, for lookup and matching purposes:

- `"copulated with"` -- first function name index
- `["copulated with", "to make"]` -- full function name index
- `[P1, "copulated with", P2, "to make", P3]` -- with parameters
- and then something even more comprehensive with type information.

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
GIVEN a IS A NUMBER
      b IS A NUMBER
GIVETH A NUMBER
a `plus` b MEANS a + b

// this may not actually be a thing just yet, but is here to illustrate type-directed name resolution / overloading
GIVEN a IS A Text, b IS A Text
GIVETH Text
a `plus` b MEANS a ++ b

#EVAL 3 `plus` 5 EQUALS 8
#EVAL "hello" `plus` "world" EQUALS "hello world"
```

Type-directed name resolution picks the right version.

### Example 2: Eligibility Check

```l4
DECLARE Person HAS
   name IS A STRING
   age IS A Number
DECLARE Program HAS
   minAge IS A Number

GIVEN person IS A Person
      program IS A Program
GIVETH A BOOLEAN
person `is eligible for` program MEANS
  person's age >= program's minAge

alice MEANS Person WITH name IS "Alice", age IS 25
healthcare MEANS Program WITH minAge IS 18

#EVAL alice `is eligible for` healthcare EQUALS TRUE
```

### Example 3: Range Check

```l4
GIVEN lower IS A Number, value IS A Number, upper IS A Number
GIVETH Bool
lower `<=` value `<=` upper MEANS
       lower <= value
   AND          value <= upper

#EVAL 0 `<=` 5 `<=` 10 = TRUE
#EVAL 0 `<=` 15 `<=` 10 = FALSE
```

### Example 4: Complex Pattern

```l4
GIVEN mylist IS A LIST OF A
      start IS A Number
      end IS A Number
GIVETH LIST OF A
mylist `from` start `to` end MEANS
  -- implementation

myList MEANS LIST 1, 2, 3, 4, 5, 6, 7, 8, 9, 10

#EVAL myList `from` 3 `to` 7 EQUALS LIST 3, 4, 5, 6, 7
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

- l4-ide/jl4/examples - sample code from which you may infer the grammar; this forms the test suite and is canonical
- l4-ide/jl4/experiments - sample code from which you may infer the grammar; some of the syntax here is not 100% supported
- TypeCheck.hs - L4 type checking implementation
