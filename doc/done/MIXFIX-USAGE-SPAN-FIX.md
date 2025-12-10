# Mixfix Usage Span Fix - COMPLETED

## Summary

**Status**: ✅ FIXED (2024-12)

**Problem**: Bare identifiers like `alice` failed to parse as mixfix operands, requiring parentheses `(alice)`.

**Solution**: Implemented Option 3 (post-parsing resolution) in the typechecker to detect and rewrite mis-parsed mixfix expressions.

## The Bug

```l4
-- This worked:
#EVAL (alice) `copulated with` (bob) `to make` charlie

-- This failed:
#EVAL alice `copulated with` bob `to make` charlie
```

**Root cause**: The `app` parser was greedy and consumed backticked names as function arguments before `mixfixChainExpr` could process them.

Parser produced: `App alice [`copulated with`, bob, `to make`, charlie]`

Should have been: Mixfix expression with `alice`, `bob`, `charlie` as operands.

## The Fix

Modified `tryMatchMixfixCall` in `jl4-core/src/L4/TypeCheck.hs` (~line 2095) to handle the multi-argument case:

1. **Detection**: When `funcName` is NOT callable and the first arg is a registered mixfix keyword
2. **Reinterpretation**: Transform `App funcName [kw1, arg1, kw2, arg2, ...]` to proper mixfix call
3. **Matching**: Use existing `tryMatchAnyPattern` to match against registered mixfix patterns

### Code Change

Added a new case in `tryMatchMixfixCall` after the existing `[l, r]` case:

```haskell
-- Multi-arg case: App funcName [kw1, arg1, kw2, arg2, ...]
(firstArg:restArgs) | isSimpleName funcName && length args >= 2 -> do
  funcNameInScope <- lookupRawNameInEnvironment (rawName funcName)
  let isCallable = any isCallableEntity funcNameInScope
  case (not isCallable, getExprName firstArg) of
    (True, Just potentialOpName) | Map.member (rawName potentialOpName) registry -> do
      let opRawName = rawName potentialOpName
      case Map.lookup opRawName registry of
        Just sigs -> do
          let funcAsExpr = App (getAnno funcName) funcName []
              newArgs = funcAsExpr : restArgs
          result <- tryMatchAnyPattern opRawName newArgs sigs
          case result of
            Just (restructuredArgs, mErr) -> pure $ Just (opRawName, restructuredArgs, mErr)
            Nothing -> tryRegularMatch
        Nothing -> tryRegularMatch
    _ -> tryRegularMatch
```

## Testing

- ✅ `alice \`copulated with\` bob \`to make\` charlie` now works
- ✅ Simple binary infix `3 \`plus\` 5` still works
- ✅ Parenthesized versions still work
- ✅ All 441 existing tests pass (no regressions)

## Why This Approach Works

1. **No parser changes needed**: The parser continues to parse greedily
2. **Type information used**: We check if `funcName` is callable to decide whether to reinterpret
3. **No ambiguity at typecheck time**: By the time we reach typechecking, we know if the name is a function
4. **Leverages existing infrastructure**: Reuses `tryMatchAnyPattern` and `matchMixfixPattern`

## Files Changed

- `jl4-core/src/L4/TypeCheck.hs` (~line 2095): Added multi-arg mixfix reinterpretation case

## Test File

- `jl4/experiments/mixfix.l4`: Both parenthesized and bare versions now work
