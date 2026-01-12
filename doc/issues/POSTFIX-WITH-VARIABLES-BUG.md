# Postfix Operators with Variables Bug

**Status**: Fixed
**Date Discovered**: 2025-12-18
**Date Fixed**: 2025-12-18
**Branch**: `mengwong/let-in-mixfix`
**Severity**: Medium - Workarounds exist

## Summary

Before the fix, postfix operators (mixfix operators without parameters after the keyword) failed for bare variables, only working with literals or parenthesized expressions.

## Resolution (2025-12-18)

**Approach**: Instead of teaching the parser to look ahead, we now rewrite mis-parsed applications inside the type checker.

1. Added `reinterpretPostfixAppIfNeeded` in `jl4-core/src/L4/TypeCheck.hs`.
   - Detects AST shards of the form `App operand [keyword]` where:
     - `keyword` is a registered postfix mixfix operator (pattern `[Param, Keyword]`)
     - `operand` is not known to be callable
   - Rewrites them to `App keyword [operand]` before the usual mixfix matcher runs.
2. Ensured annotations are rebuilt whenever this rewrite happens so IDE tooling shows the keyword as the function name.
3. Promoted the regression file to `jl4/examples/ok/postfix-with-variables.l4` and enabled all `#EVAL` directives so the golden suite locks in the behavior.
4. Added `jl4/examples/ok/mixfix-with-variables.l4` to confirm longer mixfix chains with bare variables already pass (the parser’s `mixfixChainExpr` logic never mis-parsed these, but we now have coverage).

This keeps regular function application (`f x`) untouched while making `x squared` and LET/IN variants behave the same as literal operands.

## Example (Now Passing)

```l4
IMPORT prelude

GIVEN n IS A NUMBER
n squared MEANS n * n

-- Formerly only literals worked:
testLiteral MEANS 5 squared
#EVAL testLiteral  -- Result: 25

-- After the Dec 2025 fix, bare variables now pass type checking:
GIVEN x IS A NUMBER
testVariable MEANS x squared
#EVAL testVariable OF 5  -- Result: 25

-- LET/IN bindings also behave:
testLetIn MEANS
  LET y BE 5
  IN y squared
#EVAL testLetIn  -- Result: 25
```

These cases are locked in by `jl4/examples/ok/postfix-with-variables.l4`.

### Follow-up: Multiline Layout (Jan 2026)

While re-validating the regression file we noticed that splitting the call across multiple lines still failed:

```l4
demo_multiline_postfix MEANS
  radius
  `squared`
```

`mixfixPostfixOp` and `mixfixChainExpr` only accepted keywords that lived on the **same line** as the preceding operand. A newline forced `` `squared` `` (or any mixfix keyword) to be parsed as the start of a new expression, yielding “radius is not a function”.

The parser now tracks each operand’s end line and starting column. Mixfix keywords/postfix operators are accepted either (a) on the same line, or (b) on the immediately following line **and** aligned with the operand’s indentation (mirroring `IF/THEN/ELSE`). Hint checks still gate which identifiers count as keywords, so accidental captures remain impossible. See `jl4/examples/ok/mixfix-multiline.l4` plus the `MixfixParserSpec` additions for coverage of postfix, binary, and ternary vertical layouts.

### Local Helpers Regression (2025-12-18)

Moving the postfix helper into a `WHERE` block exposed another failure mode:

```l4
IMPORT prelude

GIVEN radius IS A NUMBER
GIVETH A NUMBER
area radius MEANS
  LET pi BE 3
  IN radius squared TIMES pi
  WHERE
    n squared MEANS n TIMES n

#EVAL area OF 2
```

This still raised “radius … is not a function” plus “could not find a definition for `squared`”. The parser produced `App radius [squared]`, but when the type checker tried to reinterpret it, the mixfix registry only contained top-level operators.

**Root cause**:

1. `withScanTypeAndSigEnvironment` scanned `WHERE` declarations, then exited before type-checking the block body. The registry therefore reverted to its outer state and forgot the local postfix definitions.
2. Even inside the scanning scope, `withDecides` replaced the registry instead of extending it, so local scopes could not see both global and local mixfix operators simultaneously.

**Fix (Dec 19, 2025)**:

- Type-check both the local declarations _and_ the enclosing body while still inside `withScanTypeAndSigEnvironment`, keeping the local mixfix registry alive for `reinterpretPostfixAppIfNeeded`.
- Change `withDecides` so it merges (`Map.unionWith (<>)`) new entries into the outer registry rather than replacing it.
- Add the `circle area using where` regression plus `#EVAL` directives to `jl4/examples/ok/postfix-with-variables.l4` (and regenerate the goldens).

Now postfix/mixfix helpers defined inside `WHERE` behave exactly like their top-level counterparts.

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

## Historical Workarounds (Pre-Fix)

The following mitigations were necessary before the type-checker rewrite and are documented here only for archeology. Modern code does **not** need them:

1. Rewrite postfix calls using prefix notation with backticks.
2. Wrap operands in parentheses: `(x) squared`.
3. Create intermediate LET bindings (`ySquared BE `squared` y`).
4. Stick to literal operands (`5 squared`).

They remain valid syntax but should no longer be required in day-to-day L4.

## Solutions Considered (Historical)

### Solution A: Fix `app` Parser with Proper Lookahead (Complex)

**Approach**: Make `app` check if a bare identifier could be a postfix (or mixfix) operator before consuming it

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

### Solution B: Require Backticks for Ambiguous Cases (Simple, Superseded)

**Approach**: Document that postfix operators require backticks when applied to variables. Ultimately rejected once the type-checker rewrite landed—we prefer not to encode semantic meaning into backticks.

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

- **Shipped**: Type-checker reinterpretation (`reinterpretPostfixAppIfNeeded`) fixes postfix-with-variables without changing the parser or requiring backticks.
- **2025-12**: Local scopes extend (rather than replace) the mixfix registry, so `WHERE` helpers remain visible to the postfix rewriter.
- **Future**: Pursue Solution A (parser lookahead/registry) when we can afford a deeper parser refactor so the safety net becomes unnecessary.

### Implemented Fix (2025-12-18)

- Added a targeted rewrite in the type checker (`reinterpretPostfixAppIfNeeded`) that flips mis-parsed `App operand [keyword]` nodes into `App keyword [operand]` whenever:
  - the trailing identifier is a registered postfix mixfix keyword (`pattern = [Param, Keyword]`), and
  - the operand isn't known to be a callable function
- Keeps parser untouched for now, but guarantees correct ASTs before inference, so mixfix call-site matching, exact printing, and downstream tooling see the intended structure.
- Future parser work (Solution A) can make this rewrite unnecessary, but the rewrite is robust enough to ship immediately.

## Test Case

Regression files:

- `jl4/examples/ok/postfix-with-variables.l4` (pure postfix coverage, including the `WHERE` helper regression)
- `jl4/examples/ok/mixfix-with-variables.l4` (binary/ternary mixfix coverage)

Both demonstrate:

- Literal operands still work
- Bare variables (top-level and LET...IN) typecheck the same way
- Historical workarounds remain valid, so backticks/parentheses usage stays tested

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

1. ✓ Document the bug and resolution here (ongoing updates welcome)
2. ✓ Promote regression coverage to `jl4/examples/ok/postfix-with-variables.l4` and `jl4/examples/ok/mixfix-with-variables.l4`
3. ◻ Consider parser-level lookahead (Solution A) so the safety net can eventually be removed
4. ◻ Keep IDE and user-facing docs synced whenever mixfix semantics evolve
