# TYPICALLY Defaults - Current Status & Next Steps

**Last Updated:** 2025-12-05
**Current Blocker:** Ambiguity errors when directive rewriting is enabled
**Quick Status:** 70% complete - core works, runtime integration blocked

---

## Executive Summary

The TYPICALLY feature is mostly implemented but **gated behind a feature flag** due to test regressions. The core language features (syntax, parsing, type checking) work perfectly. The blocker is in the presumptive directive rewriting which causes ambiguity errors in test files like `foo.l4` when enabled.

**What works today:**

- ✅ Syntax: `GIVEN age IS A NUMBER TYPICALLY 18`
- ✅ Type checking validates TYPICALLY values
- ✅ Wrapper generation creates `'presumptive <fn>'` functions
- ✅ Runtime auto-apply fallback (safety net)
- ✅ CLI can use `#PEVAL` with auto-defaults

**What's blocked:**

- ❌ Directive rewriting (causes ambiguity errors)
- ❌ Explicit `JUST`/`NOTHING` wrapper calls
- ❌ Transitive propagation (inner calls)
- ❌ Decision Service integration
- ❌ Four-state InputState model

---

## Code Status: The Feature Flag

**Location:** `jl4-core/src/L4/TypeCheck.hs:3404-3405`

```haskell
enablePresumptiveDirectiveRewriting :: Bool
enablePresumptiveDirectiveRewriting = False
```

**When enabled:** Wrappers are generated, `#PEVAL` directives are rewritten to call them
**When disabled:** Runtime fallback `maybeApplyDefaults` handles TYPICALLY defaults

---

## Recent Work Timeline

### Phase 1: Foundation (Nov 2024 - Early Jan 2025)

**Commits:** `6fea6b8e` → `a249d8b4`

- ✅ Lexer: TYPICALLY keyword
- ✅ Parser: TYPICALLY clauses
- ✅ AST: Extended TypedName with tnTypically field
- ✅ Type checking: Validate TYPICALLY values match types
- ✅ Directives: `#PEVAL`, `#PEVALTRACE`, `#PASSERT` syntax
- ✅ ASSUME deprecation warnings

### Phase 2: Wrapper Generation Attempts (Jan 2025)

**Commits:** `1c06b550` → `352924a3`

**First attempt - Runtime wrapper generation:**

- Tried generating wrappers during evaluation
- Problem: Wrappers not available during type checking
- **Abandoned** - moved to compile-time approach

**Second attempt - Compile-time wrapper generation:**

- Generate wrappers during type checking (commit `25f1bd8d`)
- Store in environment with proper CheckInfo
- Success: Wrappers appear in AST

### Phase 3: Directive Rewriting (Recent)

**Commits:** `31811a4a` → `afba20a0`

- ✅ Implemented `rewritePresumptiveDirective` to call wrappers
- ✅ Pad missing args with `NOTHING`, wrap provided args with `JUST`
- ✅ `#PASSERT` unwraps result, treats `NOTHING` as failure
- ❌ **Blocked:** Causes ambiguity errors in test suite

### Phase 4: Fallback & Stabilization (Current)

**Commit:** `82211141`

- Added `maybeApplyDefaults` runtime fallback
- Disabled directive rewriting via feature flag
- System works, but not as intended architecture

---

## Technical Deep Dive: What's Implemented

### 1. Wrapper Generation (Line 3456-3489)

```haskell
generateWrapper :: Anno -> Decide Resolved -> Check (TopDecl Resolved)
```

**What it does:**

- Creates `'presumptive <name>'` function with fresh unique
- Wraps parameters: `NUMBER` → `MAYBE NUMBER`
- Wraps return type: `BOOLEAN` → `MAYBE BOOLEAN`
- Generates CONSIDER chains to unwrap parameters
- Applies TYPICALLY defaults when parameter is `NOTHING`

**Example transformation:**

```l4
-- Original:
GIVEN age IS A NUMBER TYPICALLY 18
GIVETH A BOOLEAN
canVote age MEANS age >= 18

-- Generated wrapper:
GIVEN age IS A MAYBE NUMBER
GIVETH A MAYBE BOOLEAN
'presumptive canVote' age MEANS
  CONSIDER age
    WHEN NOTHING  -> JUST (canVote 18)   -- Apply TYPICALLY
    WHEN (JUST a) -> JUST (canVote a)    -- Use provided value
```

### 2. Directive Rewriting (Line 3359-3400)

```haskell
rewritePresumptiveDirective :: Map Text Int -> Directive Name -> Maybe (Directive Name)
```

**What it does:**

- Finds `#PEVAL myFunc arg1 arg2`
- Looks up wrapper `'presumptive myFunc'` in map
- Rewrites to: `#EVAL 'presumptive myFunc' (JUST arg1) (JUST arg2)`
- Pads missing args: if myFunc has 3 params but only 2 provided, adds `NOTHING`

**Example transformation:**

```l4
-- User writes:
#PEVAL canVote 25

-- Rewritten to:
#EVAL 'presumptive canVote' (JUST 25)

-- User writes (missing argument):
#PEVAL canVote

-- Rewritten to:
#EVAL 'presumptive canVote' NOTHING
```

### 3. Runtime Fallback (Machine.hs)

```haskell
maybeApplyDefaults :: Val -> IO Val
```

**What it does:**

- When `#PEVAL` evaluates to a closure (unapplied function)
- Extracts TYPICALLY defaults from the closure's GivenSig
- Automatically applies them
- Returns final evaluated result

**This is the safety net that works today.**

---

## The Blocker: Ambiguity Errors

### What's Happening

When `enablePresumptiveDirectiveRewriting = True`, the test suite fails with ambiguity errors.

**From HELP.md:**

> "This proved out the approach but exposed regressions (e.g. ok/foo.l4 ambiguity errors) so the code currently needs further hardening before it can stay enabled."

### Example Error Pattern

```
AmbiguousTermError: 'canVote' could refer to:
  1. canVote (original function)
  2. 'presumptive canVote' (generated wrapper)
```

### Root Cause Hypotheses

1. **Duplicate registration**: Original function and wrapper both registered with similar names
2. **Name resolution conflict**: Lookup finds both original + wrapper
3. **Mixfix interaction**: Mixfix resolver confused by `'presumptive <name>'` pattern
4. **Unique collision**: Fresh unique generation may clash

### Evidence

**From commit `afba20a0`:**

> "Refactor PEVAL rewriting to avoid re-registering original functions"

This suggests the issue is related to duplicate registration.

---

## Architecture Choices Made

### Choice 1: Compile-Time vs Runtime Wrapper Generation

**Decision:** Compile-time (Option B from spec)

**Rationale:**

- Wrappers need to be available during type checking
- Allows downstream code to reference them by name
- Enables Decision Service to call wrappers explicitly

**Implementation:** `inferSection` generates wrappers in Pass 2, adds to environment in Pass 3

### Choice 2: Wrapper Naming Convention

**Decision:** Prefix with `'presumptive '`

**Example:** `canVote` → `'presumptive canVote'`

**Rationale:**

- Clear distinction from original
- Matches legal terminology ("presumptive" = assumed unless proven otherwise)
- Backticks allow spaces in identifier

### Choice 3: Transitive Propagation (Deferred)

**Decision:** Not implemented yet

**What it means:**

```l4
-- Original:
GIVEN age IS A NUMBER TYPICALLY 18
canVote age MEANS age >= 18

GIVEN age IS A NUMBER TYPICALLY 18
canDrive age MEANS canVote age AND age >= 16
```

**Current behavior:** `canDrive` calls original `canVote` (no defaults)
**Desired behavior:** `canDrive` should call `'presumptive canVote'` (with defaults)

**Why deferred:** Need to rewrite DECIDE bodies to call presumptive versions

---

## Next Steps: Unblocking the Feature

### Step 1: Debug Ambiguity Errors (High Priority)

**Goal:** Understand why enabling the flag causes ambiguity

**Actions:**

1. Create minimal reproduction case:

   ```l4
   GIVEN x IS A NUMBER TYPICALLY 5
   GIVETH A NUMBER
   foo x MEANS x * 2

   #PEVAL foo
   ```

2. Enable flag temporarily:

   ```haskell
   enablePresumptiveDirectiveRewriting = True
   ```

3. Run with verbose logging:

   ```bash
   cabal test --test-show-details=streaming 2>&1 | tee test-output.log
   grep -A 10 "Ambiguous\|foo.l4" test-output.log
   ```

4. Examine CheckInfo registration:
   - Add trace statements in `makeWrapperCheckInfo` (line 3407)
   - Check if original function is being re-registered
   - Verify wrapper has distinct Unique

### Step 2: Fix Ambiguity (High Priority)

**Hypothesis 1: Duplicate registration**

**Fix:** Ensure wrappers are registered ONLY, originals are NOT re-registered

**Code location:** `inferSection` line 458-477

Currently:

```haskell
-- Third pass: Add wrappers to environment, then rewrite and recheck ONLY directives
dirRewritten <-
  extendKnownMany wrapperExtends' $
  forM (zip topdecls (zip rtopdecls topDeclExtends)) $ ...
```

**Check:** Are originals in `topDeclExtends` being re-added?

**Hypothesis 2: Name lookup finds both**

**Fix:** Modify name resolution to prefer original over wrapper (or vice versa)

**Code location:** Type checker name resolution

**Hypothesis 3: Fresh unique collision**

**Fix:** Ensure `makePresumptiveName` generates truly fresh uniques

**Code location:** Line 3493-3502

### Step 3: Add Regression Tests (Medium Priority)

Once fixed, add tests to prevent regression:

1. **Test wrapper generation:**

   ```l4
   GIVEN x IS A NUMBER TYPICALLY 10
   foo x MEANS x + 1

   -- Should exist: 'presumptive foo'
   -- Should work: #PEVAL foo → 11
   ```

2. **Test directive rewriting:**

   ```l4
   GIVEN a IS A NUMBER TYPICALLY 5
   GIVEN b IS A NUMBER TYPICALLY 7
   bar a b MEANS a * b

   #PEVAL bar        → 35 (both defaults)
   #PEVAL bar 10     → 70 (one default)
   #PEVAL bar 10 20  → 200 (no defaults)
   ```

3. **Test `#PASSERT` failure:**

   ```l4
   GIVEN x IS A NUMBER  -- No TYPICALLY!
   baz x MEANS x > 0

   #PASSERT baz  -- Should FAIL (missing required arg, no default)
   ```

### Step 4: Implement Transitive Propagation (Low Priority)

**Goal:** Inner calls use presumptive versions

**Approach:** Rewrite Pass 2

1. After generating wrappers, build map: `original → wrapper`
2. For each wrapper body, traverse Expr tree
3. Rewrite `App originalFunc args` → `App wrapperFunc (map JUST args)`

**Code location:** New function in TypeCheck.hs around line 3550

**Complication:** Need to handle shadowing, local definitions

### Step 5: Decision Service Integration (Medium Priority)

**Goal:** Decision Service can call wrappers with `defaultMode` parameter

**Spec:** `doc/todo/RUNTIME-INPUT-STATE-SPEC.md` Phase 6-8

**Actions:**

1. Add `defaultMode: "honor-defaults" | "ignore-defaults"` to FnArguments
2. Modify `createFunction` to use wrapper when `honor-defaults`
3. Return audit trail showing which defaults were used

### Step 6: Four-State InputState Model (Low Priority)

**Goal:** Distinguish NotProvided vs ExplicitUnknown vs Explicit vs NotApplicable

**Spec:** `doc/todo/RUNTIME-INPUT-STATE-SPEC.md`

**Why deferred:** Requires API changes, larger migration

---

## Testing Strategy

### Current Test Files

1. **`jl4/examples/ok/typically-basic.l4`**

   - Tests TYPICALLY syntax
   - Type checking validation
   - ✅ Passes

2. **`jl4/examples/ok/peval-test.l4`**

   - Tests runtime auto-apply fallback
   - ✅ Works with flag disabled

3. **`jl4/examples/ok/evalstrict.l4`**
   - Tests strict directive variants
   - ✅ Parses correctly

### Needed Test Files

1. **`wrapper-generation.l4`**

   - Verify wrappers are created
   - Check wrapper signatures
   - Test calling wrappers explicitly

2. **`directive-rewriting.l4`**

   - Test `#PEVAL` with various arg counts
   - Test `#PASSERT` failure cases
   - Test `#PEVALTRACE` output

3. **`transitive-defaults.l4`**
   - Test nested function calls
   - Verify defaults propagate through call stack

### Manual Testing Checklist

When unblocking:

- [ ] Enable `enablePresumptiveDirectiveRewriting = True`
- [ ] Run full test suite: `cabal test all`
- [ ] Check for ambiguity errors
- [ ] Verify `peval-test.l4` still works
- [ ] Check CLI output shows `JUST`/`NOTHING`
- [ ] Test with nested functions
- [ ] Verify Decision Service can load wrappers

---

## Alternative Approaches (If Unblocking Fails)

### Alternative 1: Keep Runtime Fallback Permanently

**Pros:**

- Already works
- Simpler architecture
- No ambiguity issues

**Cons:**

- Not explicit in types
- Can't control per-call
- Harder for Decision Service integration

**Decision:** Use as safety net, but not long-term solution

### Alternative 2: Explicit Wrapper Suffix

Instead of `'presumptive <name>'`, use `<name>_presumptive`:

```l4
canVote → canVote_presumptive
```

**Pros:**

- Might avoid name conflicts
- Simpler lexer handling (no spaces)

**Cons:**

- Less readable
- Breaks convention

### Alternative 3: Annotation-Based Opt-In

Only generate wrappers for functions marked with `@presumptive`:

```l4
@presumptive
GIVEN age IS A NUMBER TYPICALLY 18
canVote age MEANS age >= 18
```

**Pros:**

- Explicit control
- Reduces generated code

**Cons:**

- Extra syntax burden
- Breaks composability

---

## Communication with ChatGPT Codex / Previous Work

### What ChatGPT Codex Attempted

Based on commit history:

1. Tried runtime wrapper generation (`1c06b550`)
2. Switched to compile-time generation (`25f1bd8d`)
3. Implemented Pass 1 wrapper generation (`352924a3`)
4. Fixed variable scoping (`c1f3a6b4`)
5. Added PEVAL integration (`31811a4a`)
6. Hit ambiguity errors, added fallback (`82211141`)
7. Disabled flag, added comments

### Where It Got Stuck

**Main blocker:** Ambiguity errors when directive rewriting enabled
**Last attempted fix:** Refactor to avoid re-registering originals (`afba20a0`)
**Status:** Still blocked, flag remains disabled

### Key Insight from the Work

The architecture is sound. The implementation is mostly correct. The blocker is a narrow technical issue with name resolution/registration.

---

## Recommendations

### For Immediate Progress (This Week)

1. **Focus on ambiguity debugging**

   - Create minimal test case
   - Add verbose logging
   - Understand exact cause

2. **Document findings**

   - Update HELP.md with details
   - Add notes to TypeCheck.hs

3. **Consider asking for help**
   - Post issue on GitHub with reproduction
   - Explain what's been tried
   - Ask for architecture review

### For Medium Term (This Month)

1. **Fix ambiguity issue**

   - Implement solution based on debugging
   - Test thoroughly

2. **Enable directive rewriting**

   - Flip flag to True
   - Ensure all tests pass

3. **Implement transitive propagation**
   - Rewrite inner calls
   - Test nested scenarios

### For Long Term (Next Quarter)

1. **Decision Service integration**

   - Add defaultMode parameter
   - Implement API changes

2. **Four-state InputState model**

   - Design migration path
   - Implement incrementally

3. **Production deployment**
   - Monitor performance
   - Gather user feedback

---

## Success Criteria

### Phase 1: Unblocked (Near Term)

- ✅ `enablePresumptiveDirectiveRewriting = True` doesn't cause test failures
- ✅ All existing tests pass
- ✅ Wrappers are generated correctly
- ✅ `#PEVAL` calls wrappers properly

### Phase 2: Complete (Medium Term)

- ✅ Transitive propagation works
- ✅ Decision Service can use wrappers
- ✅ Documentation updated
- ✅ Tutorial/examples added

### Phase 3: Production Ready (Long Term)

- ✅ Four-state model implemented
- ✅ Performance benchmarked
- ✅ User feedback incorporated
- ✅ Migration guide for existing code

---

## Resources

### Code Locations

| Feature              | File                    | Lines     |
| -------------------- | ----------------------- | --------- |
| Feature flag         | TypeCheck.hs            | 3404-3405 |
| Wrapper generation   | TypeCheck.hs            | 3456-3489 |
| Directive rewriting  | TypeCheck.hs            | 3359-3400 |
| Runtime fallback     | EvaluateLazy/Machine.hs | 1618-1634 |
| TYPICALLY extraction | EvaluateLazy/Machine.hs | 797-802   |

### Documentation

- Spec: `doc/todo/TYPICALLY-DEFAULTS-SPEC.md`
- Runtime: `doc/todo/RUNTIME-INPUT-STATE-SPEC.md`
- Status: `HELP.md` (this file will be updated there)
- Examples: `jl4/examples/ok/typically-basic.l4`

### Commits to Review

- `6fea6b8e` - Initial TYPICALLY implementation
- `352924a3` - Wrapper generation Pass 1
- `31811a4a` - PEVAL integration
- `82211141` - Fallback mechanism
- `afba20a0` - Latest refactoring attempt

---

## Questions for Review

1. **Architecture:** Is compile-time wrapper generation the right approach?
2. **Naming:** Should wrappers use `'presumptive <name>'` or different pattern?
3. **Priority:** Should we fix ambiguity or switch to alternative approach?
4. **Testing:** What additional test coverage is needed?
5. **Decision Service:** Should we implement before or after transitive propagation?

---

**Next Action:** Debug ambiguity errors with minimal reproduction case
**Assigned To:** Next developer picking up this work
**Timeline:** Aim to unblock within 1-2 weeks
**Risk:** Medium - architecture is sound, issue is localized

---

_This document should be updated as progress is made. Mark sections complete, add new findings, and track decisions._
