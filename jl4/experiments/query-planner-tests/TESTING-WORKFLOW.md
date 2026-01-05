# Testing Workflow Guide

## Quick Start

```bash
# 1. Start the decision service with test files
cd jl4/experiments/query-planner-tests
./start-decision-service.sh

# 2. In another terminal, run a simple test
./test-api.sh 01-simple-and.l4 '{"label": {"a": false}}'

# 3. Or run all tests
./run-all-tests.sh
```

## Interactive Testing Workflow

### Step 1: Start with Simplest Case

Test the most basic boolean operations first to verify the fundamentals:

```bash
# Simple AND - verify short-circuiting
./test-api.sh 01-simple-and.l4 '{"label": {"a": false}}'
# Expect: outcome=False, b is don't-care

./test-api.sh 01-simple-and.l4 '{"label": {"a": true}}'
# Expect: outcome=Unknown, still need b

# Simple OR - verify short-circuiting
./test-api.sh 01-simple-or.l4 '{"label": {"a": true}}'
# Expect: outcome=True, b is don't-care
```

**If these fail**: Problem with basic BDD construction or cofactoring

### Step 2: Test Don't-Care Detection

Verify that irrelevant variables are correctly identified:

```bash
# Test that right branch variables become don't-care when left succeeds
./test-api.sh 03-dont-care-simple.l4 '{"label": {"a": true, "b": true}}'
# Expect: outcome=True, c and d are don't-care

# Test that left branch variables become don't-care when it fails
./test-api.sh 03-dont-care-simple.l4 '{"label": {"a": false}}'
# Expect: outcome=Unknown, b is don't-care, still need c and d
```

**If these fail**: Problem with don't-care detection or impact scoring

### Step 3: Test Prioritization

Verify that the query planner orders questions sensibly:

```bash
# Check that "asks" array is ordered by impact
./test-api.sh 04-alcohol-purchase.l4 '{}'
# Expect: asks[0] should be highest-impact variable

# After partial input, verify re-prioritization
./test-api.sh 04-alcohol-purchase.l4 '{"label": {"age_21_plus": true}}'
# Expect: asks[0] should be highest-impact among remaining relevant vars
```

**If these fail**: Problem with impact calculation or ask ordering

### Step 4: Test Reactive Relevance

Verify that relevance changes as user provides input:

```bash
# Start: nothing known
./test-api.sh 06-reactive-relevance.l4 '{}'
# Note the impact scores

# Step 1: Set switch=true
./test-api.sh 06-reactive-relevance.l4 '{"label": {"switch": true}}'
# Expect: option_a relevant, option_b don't-care

# Step 2: Set switch=false
./test-api.sh 06-reactive-relevance.l4 '{"label": {"switch": false}}'
# Expect: option_b relevant, option_a don't-care

# Verify relevance flip-flops correctly!
```

**If these fail**: Problem with reactive query planning or BDD cofactoring

### Step 5: Test Parent Node Assertions (Advanced)

This tests whether users can bind at intermediate expression levels:

```bash
# Test if we can assert the disjunction without specifying which disjunct
./test-api.sh 05-parent-node-assertion.l4 '{"label": {"a": true, "b_or_c": true}}'
# Expect: outcome=True, b and c don't-care

# Test nested parent assertion
./test-api.sh 05-nested-parent-assertion.l4 '{"label": {"left_branch": true}}'
# Expect: outcome=True, all leaf variables don't-care
```

**If these fail**: Parent node bindings may not be supported yet - this is a known gap

### Step 6: Test with Real-World Examples

```bash
# British citizenship (complex nested logic from default files)
# First, modify start-decision-service.sh to load britishcitizen5.l4

# Parking cost (nested conditionals)
# Modify start-decision-service.sh to load parking.l4
```

## Using the REPL for Debugging

The REPL is better for iterative exploration:

```bash
cabal run jl4-repl -- jl4/experiments/query-planner-tests/01-simple-and.l4

> :decides
# Shows all DECIDE functions

> :qp result
# Shows query plan with no bindings

> :qp result a=true
# Shows query plan with a=true

> :qp result a=false
# Shows query plan with a=false (should short-circuit)

> :type a
# Check the type of variable a

> :info result
# Get information about the result function
```

## Debugging Checklist

When a test fails, work through this checklist:

### 1. Is the function loading correctly?
```bash
curl http://localhost:8001/functions | jq '.'
# Should see your test function in the list
```

### 2. What's the function metadata?
```bash
FUNC_ID=$(curl -s http://localhost:8001/functions | jq -r '.[0].id')
curl http://localhost:8001/functions/$FUNC_ID | jq '.'
# Check parameters, types, description
```

### 3. What does evaluation give?
```bash
curl -X POST "http://localhost:8001/functions/$FUNC_ID/evaluation" \
  -H "Content-Type: application/json" \
  -d '{"fnArguments": {"a": true, "b": false}}'
# Verify the actual evaluation result
```

### 4. What does query-plan give?
```bash
curl -X POST "http://localhost:8001/functions/$FUNC_ID/query-plan" \
  -H "Content-Type: application/json" \
  -d '{"label": {"a": true}}' | jq '.'
# Check outcome, stillNeeded, impact, asks
```

### 5. Is the BDD being built correctly?
- Check REPL with `:trace` to see evaluation steps
- Look for BDD construction in Haskell code
- Verify variable ordering in BDD

### 6. Are impact scores reasonable?
- Variables that affect outcome: impact > 0
- Don't-care variables: impact = 0
- Check if scores make sense relative to each other

### 7. Is the asks array useful?
- Are questions ordered by priority?
- Do schemas provide enough info for UI?
- Are labels clear and actionable?

## Expected Issues to Investigate

Based on the test suite, we expect to find:

1. **Parent node bindings**: Likely not fully implemented
   - Test files: `05-*.l4`
   - Impact: Medium (nice-to-have for UX)

2. **Impact score calculation**: May need tuning
   - All test files
   - Impact: High (affects UX quality)

3. **Schema exposure for nested types**: May be incomplete
   - Test files: `05-*.l4`
   - Impact: Medium (affects form generation)

4. **Provenance tracking**: May not be fully populated
   - All test files
   - Impact: Medium (affects explanation quality)

5. **BDD variable ordering**: May not be optimal
   - Complex test files: `04-*.l4`
   - Impact: Low (affects performance, not correctness)

## Next Steps After Testing

Once you've run through the tests:

1. Document which tests pass/fail
2. Identify patterns in failures
3. File issues for specific problems
4. Prioritize fixes based on UX impact
5. Enhance test cases based on findings

## Example Test Session

```bash
# Terminal 1
./start-decision-service.sh

# Terminal 2
./test-api.sh 01-simple-and.l4 '{}'
# Note the initial state

./test-api.sh 01-simple-and.l4 '{"label": {"a": true}}'
# Note: b should be needed, outcome Unknown

./test-api.sh 01-simple-and.l4 '{"label": {"a": false}}'
# Note: b should be don't-care, outcome False

./test-api.sh 01-simple-and.l4 '{"label": {"a": true, "b": true}}'
# Note: outcome True

./test-api.sh 01-simple-and.l4 '{"label": {"a": true, "b": false}}'
# Note: outcome False

# Compare impact scores across these scenarios
# Verify the "asks" array makes sense
```
