# Directives Reference

Directives are special instructions that begin with `#` and tell the L4 compiler to perform actions like evaluation, assertion checking, or trace generation.

## Available Directives

| Directive    | Purpose                                            |
| ------------ | -------------------------------------------------- |
| `#EVAL`      | Evaluate an expression and display the result      |
| `#EVALTRACE` | Evaluate with full execution trace (for debugging) |
| `#ASSERT`    | Verify a boolean expression is TRUE                |

## Basic Syntax

```l4
#EVAL expression
#EVALTRACE expression
#ASSERT boolean_expression
```

## Multiline Directive Syntax

Directives support two different multiline syntaxes, each suited to different situations.

### Method 1: Directive on its own line (recommended for complex expressions)

Put the directive keyword on its own line, then write the expression on subsequent **indented** lines using standard L4 layout rules:

```l4
#EVAL
  CONSIDER decodeArgs inputJson
    WHEN RIGHT args THEN JUST args
    WHEN LEFT error THEN NOTHING
```

This method works naturally with L4's indentation-sensitive syntax and is ideal for:

- `CONSIDER`/`WHEN` blocks
- `IF`/`THEN`/`ELSE` expressions
- Complex nested expressions
- Expressions with `WHERE` clauses

### Method 2: Continuation marker (for operator chains)

Start the expression on the same line as the directive, then prefix continuation lines with `# ` (hash followed by space):

```l4
#EVAL 1
# + 2
# + 3
```

This evaluates to `6`. The `# ` prefix signals "continue the expression from the previous line."

**Important:** The continuation marker expects an **operator** to follow. This syntax is designed for chaining infix operators, not for arbitrary expression continuation.

### Lists: Trailing Commas Enable Continuation

When writing list literals, trailing commas allow the expression to continue to the next line:

```l4
#EVAL LIST
       `generate quote` `Alice`   25000,
       `generate quote` `Bob`     30000,
       `generate quote` `Charlie` 20000,
       `generate quote` `Diana`   40000
```

The comma at the end of each line (except the last) is both:

1. A list element separator (semantic meaning)
2. A line continuation signal (parser behavior)

**Without trailing commas**, the parser stops at the end of the first complete expression:

```l4
-- This does NOT work as expected!
#EVAL LIST
       `generate quote` `Alice`   25000
       `generate quote` `Bob`     30000   -- orphaned, causes error
```

### Summary: When to Use Each Method

| Situation                                    | Recommended Syntax                          |
| -------------------------------------------- | ------------------------------------------- |
| Complex expressions (CONSIDER, IF/THEN/ELSE) | Method 1: `#EVAL` on own line + indentation |
| Operator chains (arithmetic, boolean)        | Method 2: `# ` continuation marker          |
| List literals                                | Method 1 with trailing commas               |
| Simple single-line expressions               | No continuation needed                      |

## Examples

### #EVAL Examples

```l4
-- Simple evaluation
#EVAL `meets minimum age` `Alice`

-- With WHERE clause (Method 1)
#EVAL
  filter isEligible employees
  WHERE
    isEligible emp MEANS emp's age AT LEAST 18

-- List evaluation with trailing commas
#EVAL LIST
    `check eligibility` applicant1,
    `check eligibility` applicant2,
    `check eligibility` applicant3

-- Operator chain (Method 2)
#EVAL baseAmount
# + taxAmount
# + serviceFee
```

### #EVALTRACE Examples

```l4
-- Get full evaluation trace for debugging
#EVALTRACE `process application` `Application 1`

-- Trace a complex calculation
#EVALTRACE
  CONSIDER riskLevel
    WHEN High THEN premium TIMES 2
    WHEN Medium THEN premium TIMES 1.5
    OTHERWISE premium
```

### #ASSERT Examples

```l4
-- Simple assertion
#ASSERT `is eligible` `Alice`

-- Negated assertion
#ASSERT NOT `is eligible` `Charlie`

-- Complex assertion (Method 1)
#ASSERT
    (`check personal eligibility` `Alice`)'s passed
    EQUALS TRUE

-- Comparison assertion
#ASSERT (`age of` employee) AT LEAST 18
```

## Parser Behavior Details

The directive parser (`singleLineExpr` in `L4/Parser.hs`) follows these rules:

1. **Same-line continuation**: Operators on the same line as the directive are always included
2. **Next-line continuation**: Only continues to the next line if:
   - The next line starts with `# ` (continuation marker), OR
   - An operator (like `,`) at the end of the current line signals more to come
3. **Indented block**: When the directive keyword is alone on its line, the indented expression below is parsed using standard L4 layout rules

This design makes directives "line-oriented" (like C preprocessor directives) while still supporting complex multiline expressions through explicit continuation.

## See Also

- [Module 6: Test Data & #EVAL Execution](../foundation-course-ai/module-6-test-data-eval.md) - Comprehensive testing guide
- [Module A7: Regression Testing](../advanced-course-ai/module-a7-regression-testing.md) - CI/CD and change control
- [Quickstart](../foundation-course-ai/quickstart.md) - Basic usage examples
