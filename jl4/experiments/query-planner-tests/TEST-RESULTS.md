# Query Planner Test Results

**Test Date:** 2026-01-05
**Decision Service:** Running on localhost:8001
**Test Files:** jl4/experiments/query-planner-tests/*.l4

## Summary

✅ **Working Features:**
1. Basic short-circuiting (AND/OR)
2. Don't-care detection (simple cases)
3. Impact analysis
4. Stable atom IDs
5. Query plan API endpoint

⚠️ **Issues Found:**
1. Complex nested logic may not fully optimize don't-care variables
2. Possible issues with binding format or label matching

---

## Test 1: Simple AND - Short Circuit

**File:** `01-simple-and.l4`
**Formula:** `a AND b`

### Test 1a: No bindings
```bash
curl -X POST 'http://localhost:8001/functions/simple%20and%20result/query-plan' \
  -H 'Content-Type: application/json' -d '{"fnArguments": {}}'
```

**Result:** ✅ **PASS**
- `determined`: null (undetermined)
- `stillNeeded`: ["a", "b"]
- `asks`: Both variables listed with equal scores (4.5)

**Interpretation:** Correctly identifies that both variables are needed.

### Test 1b: a=false (short-circuit)
```bash
curl -X POST 'http://localhost:8001/functions/simple%20and%20result/query-plan' \
  -H 'Content-Type: application/json' -d '{"fnArguments": {"a": false}}'
```

**Result:** ✅ **PASS**
- `determined`: false
- `stillNeeded`: []
- `asks`: []

**Interpretation:** Correctly short-circuits! When `a=false`, knows that `b` is don't-care and result is False.

### Test 1c: a=true (still need b)
```bash
curl -X POST 'http://localhost:8001/functions/simple%20and%20result/query-plan' \
  -H 'Content-Type: application/json' -d '{"fnArguments": {"a": true}}'
```

**Result:** ✅ **PASS**
- `determined`: null
- `stillNeeded`: ["b"]
- `asks`: ["b"]

**Interpretation:** Correctly identifies that when `a=true`, we still need `b` to determine the result.

---

## Test 2: Simple OR - Short Circuit

**File:** `01-simple-or.l4`
**Formula:** `a OR b`

### Test 2a: a=true (short-circuit)
```bash
curl -X POST 'http://localhost:8001/functions/simple%20or%20result/query-plan' \
  -H 'Content-Type: application/json' -d '{"fnArguments": {"a": true}}'
```

**Result:** ✅ **PASS**
- `determined`: true
- `stillNeeded`: []

**Interpretation:** Correctly short-circuits! When `a=true`, knows that `b` is don't-care and result is True.

---

## Test 3: Don't-Care Detection - Branch Success

**File:** `03-dont-care-simple.l4`
**Formula:** `(a AND b) OR (c AND d)`

### Test 3a: Left branch succeeds
```bash
curl -X POST 'http://localhost:8001/functions/either%20branch%20succeeds/query-plan' \
  -H 'Content-Type: application/json' -d '{"fnArguments": {"a": true, "b": true}}'
```

**Result:** ✅ **PASS**
- `determined`: true
- `stillNeeded`: []
- `impact`: {} (empty - no variables have impact)

**Interpretation:** When left branch succeeds, correctly identifies that `c` and `d` are don't-care.

### Test 3b: Left branch fails (a=false)
```bash
curl -X POST 'http://localhost:8001/functions/either%20branch%20succeeds/query-plan' \
  -H 'Content-Type: application/json' -d '{"fnArguments": {"a": false}}'
```

**Result:** ✅ **PASS**
- `determined`: null
- `stillNeeded`: ["c", "d"]

**Interpretation:** When `a=false`, correctly identifies that:
- `b` is don't-care (left branch already failed)
- `c` and `d` are needed (must check right branch)

---

## Test 4: Complex Nested Logic - Alcohol Purchase

**File:** `04-alcohol-purchase.l4`
**Formula:** Complex nested AND/OR with 6 variables

### Test 4a: Age 21+
```bash
curl -X POST 'http://localhost:8001/functions/may%20purchase%20alcohol/query-plan' \
  -H 'Content-Type: application/json' -d '{"fnArguments": {"age_21_plus": true}}'
```

**Result:** ⚠️ **NEEDS INVESTIGATION**
- `determined`: null
- `stillNeeded`: All 6 variables still listed
- `asks`: Top 3 asks include age_21_plus, beer_only, spousal_approval

**Expected Behavior:**
- `parental_approval` and `legally_emancipated` should be don't-care (under-21 path irrelevant)
- Only `married`, `spousal_approval`, and `beer_only` should be relevant

**Possible Issues:**
1. Binding may not be applied correctly (check if variable name matches)
2. Complex nested formula may not be fully optimized by BDD
3. Need to verify the L4 source parses correctly

**Action Items:**
- [ ] Check actual L4 source structure
- [ ] Verify binding format (labels, backticks, etc.)
- [ ] Test with simpler nesting to isolate issue
- [ ] Check BDD construction for this formula

---

## Key Findings

### ✅ What Works Well

1. **Basic Short-Circuiting**: AND and OR operations correctly short-circuit
   - `False AND Unknown → False`
   - `True OR Unknown → True`

2. **Don't-Care Detection**: Simple branch-based don't-care works
   - When one branch of OR succeeds, other branch variables are don't-care
   - When one term of AND fails, other terms are don't-care

3. **Query Plan Structure**: API returns useful information
   - `determined`: Whether outcome is known
   - `stillNeeded`: What variables are still needed
   - `asks`: Prioritized questions
   - `impact`: What happens if each variable is true/false
   - `atomId`: Stable UUIDs for each atom

4. **Bindings Work**: Can provide partial bindings and get updated query plan

### ⚠️ Issues to Investigate

1. **Complex Nested Logic**: May not fully optimize when nesting is deep
   - Simple cases (2 levels) work fine
   - Complex cases (3+ levels with multiple branches) may not fully detect don't-care

2. **Possible Causes:**
   - BDD variable ordering may not be optimal for complex formulas
   - Cofactoring may not propagate through all levels
   - Impact calculation may be conservative (marking things as relevant when they're not)

3. **Variable Naming:** Need to clarify:
   - Do backticks in L4 source affect binding keys?
   - Should bindings use label, unique ID, or atomId?
   - What's the precedence when multiple match methods are available?

---

## Recommendations

### Short Term
1. **Document Binding Format**: Clarify exactly how to bind variables
   - By label (with or without backticks?)
   - By unique ID
   - By atomId

2. **Add Debugging Tools**:
   - Endpoint to show BDD structure
   - Endpoint to show cofactoring steps
   - More verbose impact explanation

3. **Test Intermediate Complexity**:
   - Create test cases between simple (working) and complex (not fully working)
   - Find the threshold where optimization breaks down

### Medium Term
1. **BDD Optimization**: Review variable ordering strategy
2. **Impact Calculation**: Make impact scores more informative (not just binary relevant/don't-care)
3. **Parent Node Bindings**: Test whether users can bind intermediate expressions

### Long Term
1. **Formal Verification**: Prove that don't-care detection is sound
2. **Performance**: Optimize for larger formulas (10+ variables)
3. **UI Integration**: Build actual form that demonstrates reactive graying

---

## Testing Commands Reference

### Start Decision Service
```bash
cd jl4-decision-service
cabal run jl4-decision-service-exe -- --port 8001 \
  --sourcePaths ../jl4/experiments/query-planner-tests
```

### List All Functions
```bash
curl -s 'http://localhost:8001/functions' | jq '.[].function | {name, description}'
```

### Get Function Metadata
```bash
curl -s 'http://localhost:8001/functions/<FUNCTION_NAME>' | jq '.'
```

### Query Plan (No Bindings)
```bash
curl -s -X POST 'http://localhost:8001/functions/<FUNCTION_NAME>/query-plan' \
  -H 'Content-Type: application/json' \
  -d '{"fnArguments": {}}' | jq '.'
```

### Query Plan (With Bindings)
```bash
curl -s -X POST 'http://localhost:8001/functions/<FUNCTION_NAME>/query-plan' \
  -H 'Content-Type: application/json' \
  -d '{"fnArguments": {"var_name": true}}' | jq '.'
```

### Useful jq Filters
```bash
# Just the essentials
| jq '{determined, stillNeeded: [.stillNeeded[] | .label], asks: [.asks[] | .label]}'

# With impact
| jq '{determined, stillNeeded: [.stillNeeded[] | .label], impact}'

# Top 3 asks
| jq '{asks: [.asks[0:3][] | {label, score}]}'
```

---

## Next Steps

1. Investigate why `parental_approval` and `legally_emancipated` aren't detected as don't-care in alcohol purchase test
2. Create intermediate complexity test cases to find the threshold
3. Review BDD construction and cofactoring code
4. Test parent node assertions (binding WHERE clause expressions)
5. Document expected vs actual behavior for all test files
6. Consider adding trace/debug endpoints to decision service
