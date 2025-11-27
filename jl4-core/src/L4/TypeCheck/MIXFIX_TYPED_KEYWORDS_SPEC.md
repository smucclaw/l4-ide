# Typed Keyword Placeholders for Mixfix Expressions

## Overview

This document specifies enhancements to the mixfix expression handling in L4's type checker. The foundation has been laid with the `MixfixArgMatch` type and `Keyword` built-in type. This spec describes how to complete the implementation for better error messages and richer AST representation.

## Current State (as of commit 4d70785e)

### What Exists

1. **`MixfixArgMatch` type** in `L4/TypeCheck/Types.hs`:

   ```haskell
   data MixfixArgMatch a
     = MixfixKeywordArg RawName   -- A validated keyword placeholder
     | MixfixParamArg a           -- A real parameter argument
   ```

2. **`matchLinearAfterHeadKeyword`** returns `MixfixMatchResult [MixfixArgMatch (Expr Name)]`, preserving info about which positions are keywords vs parameters.

3. **`extractParamArgs`** helper extracts only parameter args for type checking.

4. **`Keyword` built-in type** registered in Environment.hs (but not yet used).

### Current Behavior

- Keywords are validated during pattern matching
- Keyword info is immediately discarded via `extractParamArgs`
- Only parameter arguments reach the type checker
- ~~Error messages don't distinguish keyword mismatches from other errors~~ **IMPLEMENTED**

## Enhancement 1: Better Error Messages for Keyword Mismatches ✅ IMPLEMENTED

### Goal

When a user writes `a `plus` b` but `plus` isn't defined, or writes `a `puls` b` (typo), provide a helpful error message like:

```
Error: Unknown mixfix keyword `puls`
Did you mean `plus`?
```

Or when keywords are in wrong order:

```
Error: Expected keyword `then` but found `else`
In expression: `myif` cond `else` x `then` y
```

### Implementation (Completed)

The following has been implemented:

1. **Created `MixfixMatchError` type** in `L4/TypeCheck/Types.hs`:

   ```haskell
   data MixfixMatchError
     = UnknownMixfixKeyword RawName [RawName]  -- unknown kw, suggestions
     | WrongKeyword RawName RawName            -- expected, actual
     | MissingKeyword RawName                  -- expected keyword not found
     | ExtraKeyword RawName                    -- unexpected keyword in args
     | ArityMismatch Int Int                   -- expected, actual arg count
   ```

2. **Created `MixfixMatchResult` type** for three-way matching results:

   ```haskell
   data MixfixMatchResult a
     = MixfixNoMatch        -- Pattern doesn't match; try other patterns
     | MixfixError MixfixMatchError  -- Partial match with error
     | MixfixSuccess a      -- Pattern matches successfully
   ```

3. **Modified `matchLinearAfterHeadKeyword`** to return `MixfixMatchResult`:

   - Returns `MixfixSuccess` on successful match
   - Returns `MixfixError (WrongKeyword expected actual)` when wrong keyword found
   - Returns `MixfixNoMatch` when pattern doesn't match at all

4. **Propagated errors through**:

   - `matchMixfixPattern`
   - `matchParamFirstPattern`
   - `tryMatchAnyPattern`
   - `tryMatchMixfixCall`

5. **Added error reporting in `inferExpr` App case** (line ~1180):

   - Reports `MixfixMatchErrorCheck` when mixfix pattern matching finds an error

6. **Added `prettyMixfixMatchError`** for user-friendly error messages:

   - "Expected keyword `mythen` but found `myelse`"
   - "Check that keywords are in the correct order."

7. **Added fuzzy matching utilities** (prepared for future use):
   - `_suggestKeywords` - finds similar keywords using edit distance
   - `_editDistance` - Levenshtein edit distance implementation
   - `_rawNameText` - extracts text from RawName for comparison

### Test Case

Added `jl4/examples/not-ok/tc/mixfix-wrong-keyword.l4` which tests:

- Defining a ternary mixfix function `myif cond mythen thenVal myelse elseVal`
- Calling it with wrong keyword order: `myif TRUE myelse 1 mythen 2`
- Produces error: "Expected keyword `mythen` but found `myelse`"

### Files Modified

- `jl4-core/src/L4/TypeCheck/Types.hs` - Added `MixfixMatchError`, `MixfixMatchResult`, `mixfixResultToMaybe`
- `jl4-core/src/L4/TypeCheck.hs` - Modified matching functions, added error handling and fuzzy matching utilities

## Enhancement 2: Keep Keywords in the AST

### Goal

Instead of discarding keyword placeholders after validation, keep them in the resolved AST with type `Keyword`. This enables:

- Better debugging/inspection of AST
- Accurate source mapping for IDE features
- Foundation for keyword-aware transformations

### Implementation Steps

1. **Modify `tryMatchMixfixCall`** (line ~2071) to return the full `[MixfixArgMatch (Expr Name)]` instead of extracting params:

   ```haskell
   tryMatchMixfixCall :: Name -> [Expr Name]
     -> Check (Maybe (RawName, [MixfixArgMatch (Expr Name)]))
   ```

2. **In the `App` case of `inferExpr`**, handle `MixfixArgMatch` specially:

   ```haskell
   App ann n es -> do
     mMixfixMatch <- tryMatchMixfixCall n es
     case mMixfixMatch of
       Nothing -> ... -- normal handling
       Just (funcRawName, matchedArgs) -> do
         -- Separate keywords from params
         let paramArgs = extractParamArgs matchedArgs

         -- Type check param args against function type
         (rn, pt) <- resolveTerm actualFuncName
         t <- instantiate pt
         (checkedParams, rt) <- matchFunTy False rn t paramArgs

         -- Rebuild full arg list with typed keywords
         checkedArgs <- zipWithM typeCheckMatchedArg matchedArgs checkedParams

         pure (App finalAnn rn checkedArgs, rt)
   ```

3. **Add helper to type-check matched args**:

   ```haskell
   typeCheckMatchedArg
     :: MixfixArgMatch (Expr Name)  -- Original matched arg
     -> Expr Resolved               -- Type-checked param (if param)
     -> Check (Expr Resolved)
   typeCheckMatchedArg (MixfixKeywordArg kw) _ = do
     -- Create a resolved keyword expression with type Keyword
     let kwExpr = Lit emptyAnno (StringLit kw)  -- or special Keyword literal
     pure kwExpr  -- typed as Keyword
   typeCheckMatchedArg (MixfixParamArg _) checkedExpr =
     pure checkedExpr
   ```

4. **Consider adding a `KeywordLit` constructor** to the `Lit` type in `L4/Syntax.hs`:
   ```haskell
   data Lit
     = IntLit Integer
     | DoubleLit Double
     | StringLit Text
     | KeywordLit RawName  -- NEW: for mixfix keyword placeholders
   ```

### Files to Modify

- `jl4-core/src/L4/Syntax.hs` - Optionally add `KeywordLit`
- `jl4-core/src/L4/TypeCheck.hs` - Modify App handling, add helpers
- Potentially evaluation and printing modules if `KeywordLit` is added

## Enhancement 3: Partial Application with Keywords

### Goal

Support partial application of mixfix functions where some arguments are provided:

```l4
-- Full application
6 `is divisible by` 3

-- Partial application (returns a function)
`is divisible by` 3  -- :: Number -> Boolean
6 `is divisible by`  -- :: Number -> Boolean (curried from right)
```

### Implementation Steps

1. **Detect partial application** in `tryMatchMixfixCall`:

   - If pattern expects N params but only M < N are provided
   - Keywords must still all be present and in correct positions

2. **Generate curried function type**:

   ```haskell
   -- For `is divisible by` 3 where full type is Number -> Number -> Boolean
   -- Result type is Number -> Boolean (first param missing)
   ```

3. **Create lambda wrapper** in the AST for partial applications:

   ```haskell
   -- `is divisible by` 3 becomes:
   -- \d -> d `is divisible by` 3
   ```

4. **Handle both left and right partial application**:
   - Left partial: `f a _ c` -> `\b -> f a b c`
   - Right partial: `f _ b c` -> `\a -> f a b c`

### Complexity Note

This enhancement is more complex and may require:

- Changes to how mixfix patterns are matched
- New AST representation for partial mixfix applications
- Careful handling of keyword positions in partial applications

Consider implementing Enhancements 1 and 2 first before tackling this.

## Testing Strategy

### For Enhancement 1 (Error Messages)

Add test cases in `jl4/examples/not-ok/tc/`:

```l4
-- mixfix-unknown-keyword.l4
`plus` a b MEANS a + b
test MEANS 1 `puls` 2  -- Should error: unknown keyword `puls`, did you mean `plus`?

-- mixfix-wrong-keyword.l4
`myif` cond `then` t `else` e MEANS ...
test MEANS `myif` True `else` 1 `then` 2  -- Should error: expected `then` but found `else`
```

### For Enhancement 2 (AST Keywords)

Add tests that inspect the resolved AST:

- Verify keyword nodes are present with type `Keyword`
- Verify source ranges are preserved correctly
- Test IDE features (hover, go-to-definition) work correctly near keywords

### For Enhancement 3 (Partial Application)

```l4
`is divisible by` d n MEANS n MODULO d EQUALS 0

-- Test partial applications
divBy3 MEANS `is divisible by` 3
test1 MEANS divBy3 6  -- Should be True

sixDivBy MEANS 6 `is divisible by`
test2 MEANS sixDivBy 3  -- Should be True
```

## Priority Order

1. **Enhancement 1** (Error Messages) - High value, moderate complexity
2. **Enhancement 2** (AST Keywords) - Moderate value, moderate complexity
3. **Enhancement 3** (Partial Application) - Lower priority, high complexity

## Related Code Locations

| Component                   | File                        | Line (approx)      |
| --------------------------- | --------------------------- | ------------------ |
| MixfixMatchError type       | L4/TypeCheck/Types.hs       | 190-204            |
| MixfixMatchResult type      | L4/TypeCheck/Types.hs       | 206-226            |
| MixfixArgMatch type         | L4/TypeCheck/Types.hs       | 228-234            |
| matchLinearAfterHeadKeyword | L4/TypeCheck.hs             | 2236               |
| extractParamArgs            | L4/TypeCheck.hs             | 2291               |
| tryMatchMixfixCall          | L4/TypeCheck.hs             | 2074               |
| App case in inferExpr       | L4/TypeCheck.hs             | 1158-1211          |
| prettyMixfixMatchError      | L4/TypeCheck.hs             | 2813-2845          |
| Fuzzy matching utilities    | L4/TypeCheck.hs             | 2312-2347          |
| Keyword built-in            | L4/TypeCheck/Environment.hs | 25, 96-98, 282-285 |
| MixfixInfo type             | L4/TypeCheck/Types.hs       | 240-250            |
| MixfixPatternToken          | L4/TypeCheck/Types.hs       | 178-186            |

## Notes for Implementer

- The `Keyword` type is already registered but unused - it's ready for Enhancement 2
- The `_hasMatchedKeywords` helper exists but is prefixed with `_` to suppress unused warnings
- The fuzzy matching utilities (`_suggestKeywords`, `_editDistance`) are prefixed with `_` as they're prepared for future use
- All 402 tests now pass (399 original + 3 for mixfix error messages)
- Consider adding a feature flag to enable/disable new behavior during development
