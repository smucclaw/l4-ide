# Postfix Operators with Variables Bug

**Status**: Open
**Date Discovered**: 2025-12-18
**Branch**: `mengwong/let-in-mixfix`
**Severity**: Medium - Workarounds exist

## Summary

Postfix operators (mixfix operators without parameters after the keyword) don't work with bare variables, only with literals or parenthesized expressions.

## Example

```l4
IMPORT prelude

GIVEN n IS A NUMBER
n squared MEANS n * n

-- ✓ Works: literal
test1 MEANS 5 squared
#EVAL test1  -- Result: 25

-- ✗ Fails: bare variable
GIVEN x IS A NUMBER
test2 MEANS x squared
#EVAL test2 OF 5  -- TYPE ERROR: "x is not a function"

-- ✗ Fails: LET...IN with variable
test3 MEANS
  LET y BE 5
  IN y squared
#EVAL test3  -- TYPE ERROR: "y is not a function"
```

## Root Cause

The `app` parser (jl4-core/src/L4/Parser.hs:1404-1412) consumes same-line bare identifiers as function arguments before the postfix operator parser gets a chance to run.

**Parser execution order:**
1. `indentedExpr` → `mixfixChainExpr` → `baseExpr`
2. `baseExpr = postfixPWithLine mixfixPostfixOp regularPostfixOperator baseExpr'`
3. `baseExpr'` tries alternatives including `app` (line 1289)
4. `app` matches `x` as function name, then uses `lmany (const (indented atomicExpr' current))` to parse arguments
5. `atomicExpr'` includes `nameAsApp App` which parses `squared` as a bare identifier
6. Result: `App x [Var squared]` instead of `App squared [x]`
7. Post fix parser never runs because `baseExpr'` has already consumed all tokens

**Why literals work:**
- `app` requires the function name to be parsed by `name` (an identifier)
- `5 squared` fails at step 3 because `5` is not a name
- Falls through to `lit` which parses `5`
- Then `postfixPWithLine` runs and finds `squared`
- Result: `App squared [5]` ✓

## Investigation History

### Initial Hypothesis: LET...IN Specific Issue
- **Status**: ❌ Incorrect
- **Finding**: Bug exists in top-level MEANS clauses too, not specific to LET...IN

### Attempt 1: Use `atomicExpr` Instead of `atomicExpr'`
- **Change**: Modified line 1411 to use `atomicExpr` (which includes postfix support) instead of `atomicExpr'`
- **Result**: ❌ Failed
- **Error**: Broke prelude.l4:994 - `insertValue` function parsing failed
- **Reason**: `atomicExpr` includes postfix parsing which interfered with multi-argument function calls
- **Example failure**: `go (insertValue key value acc) xs` was mis-parsed

### Attempt 2: Create `atomicExprWithPostfix` Variant
- **Change**: Created a separate parser combining `atomicExpr'` with postfix support
- **Result**: ❌ Same failure as Attempt 1
- **Reason**: Identical behavior to `atomicExpr`

### Attempt 3: Parser-Level Lookahead
- **Approach**: Create `nameAsAppForArg` that checks if an identifier is followed by another expression on the same line
- **Logic**: If NOT followed by anything, reject it (might be postfix); if followed, consume as argument
- **Implementation attempts**:
  1. Used `optional $ lookAhead $ atomicExpr'` - **Failed**: Type mismatch between Parser and AnnoParser contexts
  2. Simplified to `lookAhead anySingle` to check next token - **Failed**: Type ambiguity errors
  3. Added type annotations `(nextTok :: PosToken)` - **Failed**: Still type mismatches
  4. Restructured to do lookahead before `attachAnno` - **Failed**: Complex type errors with `Epa Name` vs `Name`

- **Blocker**: The parser's type system makes it difficult to mix `lookAhead` (which works at the Parser/token level) with `attachAnno` (which works at the AnnoParser level). The existing `mixfixPostfixOp` works because it operates entirely at the Parser level before entering AnnoParser context.

### Key Insight: Mixfix Registry Timing
- **User suggestion**: Use a lookup table of registered mixfix operators to check during parsing
- **Reality**: The mixfix registry is built during the **scanning phase** of type checking, which happens AFTER parsing
- **Implication**: Parser cannot know which identifiers are postfix operators at parse time
- **However**: The current postfix parser doesn't need the registry - it permissively accepts any identifier that LOOKS LIKE a postfix operator (not followed by expression), then type checker validates it

### Discovery: Incomplete Test Coverage
- Commit 8269055a "Allow bare identifiers for postfix mixfix operators" added support for `5 cubed` without backticks
- **All tests use literals or parenthesized expressions:**
  - `50 percent` ✓
  - `75 percent` ✓
  - `(33 plus 22) percent` ✓
  - `(10 + 5) percent` ✓
- **No tests with bare variables:** `x percent` ✗
- **Bug was never caught by test suite**

## Workarounds

### 1. Use Prefix Notation with Backticks
```l4
GIVEN x IS A NUMBER
test MEANS `squared` x

#EVAL test OF 5  -- ✓ Works: 25
```

### 2. Use Parentheses
```l4
test MEANS (x) squared
```
**Status**: Untested - may or may not work

### 3. For LET...IN: Create Intermediate Binding
```l4
test MEANS
  LET y BE 5
      ySquared BE `squared` y
  IN ySquared

#EVAL test  -- ✓ Works: 25
```

### 4. Keep Operand as Literal
```l4
test MEANS 5 squared  -- ✓ Works
```

## Proposed Solutions

### Solution A: Fix `app` Parser with Proper Lookahead (Complex)
**Approach**: Make `app` check if a bare identifier could be a postfix operator before consuming it

**Requirements**:
1. Parse identifier
2. Look ahead to next token
3. If next token is on different line OR is not an expression-starting token, reject identifier (might be postfix)
4. Handle AnnoParser/Parser type system correctly

**Challenges**:
- Type system complexity with `attachAnno` and `lookAhead`
- Need to avoid breaking existing multi-argument function calls
- Parser context makes lookahead difficult

**Estimated effort**: 1-2 days of expert parser work

### Solution B: Require Backticks for Ambiguous Cases (Simple)
**Approach**: Document that postfix operators require backticks when applied to variables

**Changes**:
1. Update documentation to clarify backtick requirements
2. Add warning/hint in error message when type checker detects potential postfix usage
3. Add test cases demonstrating the limitation

**Pros**:
- No parser changes needed
- Clear, predictable behavior
- Aligns with original mixfix design (backticks at call sites)

**Cons**:
- Less ergonomic than `x squared`
- Inconsistent (literals work without backticks)

### Solution C: Two-Pass Parsing (Major Refactor)
**Approach**: Parse twice - first pass collects signatures, second pass uses registry

**Changes**:
1. Initial parse with permissive grammar
2. Build mixfix registry from parsed signatures
3. Re-parse with registry-aware postfix handling

**Pros**:
- Could enable registry-based disambiguation
- More principled solution

**Cons**:
- Major architectural change
- Performance impact
- Complex to implement correctly

## Recommendation

**Short term**: Solution B - Document the limitation and require backticks for variables

**Long term**: Solution A - Fix the parser once we have bandwidth for careful parser work

## Test Case

Created: `jl4/examples/not-ok/tc/postfix-with-variables.l4`
Demonstrates:
- Working cases (literals)
- Failing cases (variables)
- All three workarounds

## Related Files

- Parser: `jl4-core/src/L4/Parser.hs:1404-1412` (`app` function)
- Postfix parser: `jl4-core/src/L4/Parser.hs:1143-1166` (`mixfixPostfixOp`)
- Mixfix docs: `doc/mixfix-operators.md`
- Status doc: `MIXFIX-STATUS.md`

## Discussion

The fundamental tension is between:
1. **Ergonomics**: Want `x squared` to work naturally
2. **Ambiguity**: `f x y` could be function application OR `f (y x)` with postfix
3. **Parser limitations**: Can't easily look ahead within current type system

The existing implementation chose to prioritize function application (`f x y`) over postfix, which makes sense for the common case but creates this edge case with variables.

The fact that literals work is actually somewhat accidental - it's because the `app` parser fails on non-identifiers, not because of intentional design.

## Next Steps

1. ✓ Document bug in this file
2. ✓ Create test case in `jl4/examples/not-ok/tc/`
3. Add better error message when this pattern is detected
4. Update user documentation with backtick guidelines
5. Consider Solution A when we have parser expertise available
