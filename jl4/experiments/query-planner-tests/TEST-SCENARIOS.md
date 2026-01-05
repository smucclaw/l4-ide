# Interactive Test Scenarios

These scenarios can be tested using the `test-api.sh` script or by calling the decision service API directly.

## Setup

```bash
# Terminal 1: Start decision service with test files
./start-decision-service.sh

# Terminal 2: Run tests
./test-api.sh 01-simple-and.l4
```

## Level 1: Basic Boolean Operations

### Test 1.1: Simple AND - Short Circuit with False
```bash
./test-api.sh 01-simple-and.l4 '{"label": {"a": false}}'
```
**Expected:**
- Outcome: `False`
- Still needed: `[]` (determined)
- Don't care: `b` (can't change outcome)
- Impact: `{"a": N/A (already bound), "b": 0}`

### Test 1.2: Simple AND - Still Need Both
```bash
./test-api.sh 01-simple-and.l4 '{"label": {"a": true}}'
```
**Expected:**
- Outcome: `Unknown`
- Still needed: `["b"]`
- Don't care: `[]`
- Impact: `{"b": >0}` (b is relevant)

### Test 1.3: Simple OR - Short Circuit with True
```bash
./test-api.sh 01-simple-or.l4 '{"label": {"a": true}}'
```
**Expected:**
- Outcome: `True`
- Still needed: `[]`
- Don't care: `b`
- Impact: `{"b": 0}`

### Test 1.4: Three Variables - One Short Circuits
```bash
./test-api.sh 01-three-vars.l4 '{"label": {"c": true}}'
```
**Expected:**
- Outcome: `True` (c=true makes entire (a AND b) OR c true)
- Still needed: `[]`
- Don't care: `a, b` (the left branch is irrelevant)

### Test 1.5: Three Variables - Right Branch Fails
```bash
./test-api.sh 01-three-vars.l4 '{"label": {"c": false}}'
```
**Expected:**
- Outcome: `Unknown`
- Still needed: `a, b`
- Don't care: `c` (already bound)
- Impact: `{"a": >0, "b": >0}` (both needed for left branch)

## Level 2: Short-Circuiting Chains

### Test 2.1: AND Chain - First False
```bash
./test-api.sh 02-and-short-circuit.l4 '{"label": {"a": false}}'
```
**Expected:**
- Outcome: `False`
- Don't care: `b, c`

### Test 2.2: AND Chain - Middle False
```bash
./test-api.sh 02-and-short-circuit.l4 '{"label": {"a": true, "b": false}}'
```
**Expected:**
- Outcome: `False`
- Don't care: `c`

### Test 2.3: OR Chain - First True
```bash
./test-api.sh 02-or-short-circuit.l4 '{"label": {"a": true}}'
```
**Expected:**
- Outcome: `True`
- Don't care: `b, c`

## Level 3: Don't-Care Detection

### Test 3.1: Left Branch Succeeds
```bash
./test-api.sh 03-dont-care-simple.l4 '{"label": {"a": true, "b": true}}'
```
**Expected:**
- Outcome: `True` ((a AND b) succeeds)
- Don't care: `c, d` (right branch irrelevant)

### Test 3.2: Left Branch Partially Fails
```bash
./test-api.sh 03-dont-care-simple.l4 '{"label": {"a": false}}'
```
**Expected:**
- Outcome: `Unknown`
- Still needed: `c, d` (need right branch)
- Don't care: `b` (left branch already failed)

### Test 3.3: Both Branches Fail
```bash
./test-api.sh 03-dont-care-simple.l4 '{"label": {"a": false, "c": false}}'
```
**Expected:**
- Outcome: `False`
- Don't care: `b, d` (both branches failed)

### Test 3.4: If-Then-Else - Condition True
```bash
./test-api.sh 03-dont-care-branch.l4 '{"label": {"condition": true}}'
```
**Expected:**
- Outcome: `Unknown`
- Still needed: `consequence`
- Don't care: `alternative` (condition=true means we take then-branch)

### Test 3.5: If-Then-Else - Condition False
```bash
./test-api.sh 03-dont-care-branch.l4 '{"label": {"condition": false}}'
```
**Expected:**
- Outcome: `Unknown`
- Still needed: `alternative`
- Don't care: `consequence` (condition=false means we take else-branch)

## Level 4: Complex Nested Logic

### Test 4.1: Alcohol Purchase - Age 21+
```bash
./test-api.sh 04-alcohol-purchase.l4 '{"label": {"age_21_plus": true}}'
```
**Expected:**
- Outcome: `Unknown`
- Still needed: `married, spousal_approval, beer_only` (or some subset)
- Don't care: `parental_approval, legally_emancipated` (under-21 path irrelevant)

### Test 4.2: Alcohol Purchase - Age 21+ and Unmarried
```bash
./test-api.sh 04-alcohol-purchase.l4 '{"label": {"age_21_plus": true, "married": false}}'
```
**Expected:**
- Outcome: `True` (21+ and unmarried can purchase)
- Don't care: `spousal_approval, parental_approval, legally_emancipated`

### Test 4.3: Alcohol Purchase - Under 21
```bash
./test-api.sh 04-alcohol-purchase.l4 '{"label": {"age_21_plus": false}}'
```
**Expected:**
- Outcome: `Unknown`
- Still needed: `parental_approval, legally_emancipated` (or one of them)
- Don't care: `married, spousal_approval, beer_only` (21+ path irrelevant)

### Test 4.4: Alcohol Purchase - Under 21 with Parental Approval
```bash
./test-api.sh 04-alcohol-purchase.l4 '{"label": {"age_21_plus": false, "parental_approval": true}}'
```
**Expected:**
- Outcome: `True`
- Don't care: `married, spousal_approval, beer_only, legally_emancipated`

## Level 5: Parent Node Assertions

### Test 5.1: Assert Parent Disjunction
```bash
# User knows (b OR c) is true but doesn't want to specify which
# This requires the WHERE clause to create a label for the subexpression
./test-api.sh 05-parent-node-assertion.l4 '{"label": {"a": true, "b_or_c": true}}'
```
**Expected:**
- Outcome: `True`
- Don't care: `b, c` (parent node assertion makes details unnecessary)

**Note:** This test will reveal whether the current implementation supports parent-level bindings!

### Test 5.2: Nested Parent Assertion - Left Branch
```bash
./test-api.sh 05-nested-parent-assertion.l4 '{"label": {"left_branch": true}}'
```
**Expected:**
- Outcome: `True`
- Don't care: `a, b, c, d, e` (parent assertion satisfies entire formula)

### Test 5.3: Nested Parent Assertion - Right Branch Partial
```bash
./test-api.sh 05-nested-parent-assertion.l4 '{"label": {"c": true, "d_or_e": true}}'
```
**Expected:**
- Outcome: `True`
- Don't care: `a, b, d, e`

## Testing with REPL

Alternative to API testing - use the REPL for interactive exploration:

```bash
cabal run jl4-repl -- jl4/experiments/query-planner-tests/01-simple-and.l4

> :decides
> :qp result
> :qp result a=true
> :qp result a=false
> :qp result a=true b=true
```

The `:qp` command shows query planning results in a text format.

## Expected Failures / Areas to Investigate

Based on the test cases, we should verify:

1. **BDD Construction**: Does the BDD compiler handle all boolean operators correctly?
2. **Don't-care detection**: Are variables correctly identified as don't-care after short-circuits?
3. **Impact scoring**: Are impact scores meaningful and useful for prioritization?
4. **Parent node bindings**: Can users bind at intermediate expression levels? ← **KEY QUESTION**
5. **Provenance tracking**: Do `inputRefs` correctly identify which parameters each atom depends on?
6. **Schema exposure**: Are nested schemas correctly surfaced in the API response?

## Debugging Checklist

If a test fails:
- [ ] Check the BDD construction (is the boolean formula correct?)
- [ ] Verify cofactoring (are variable substitutions working?)
- [ ] Inspect the impact scores (are they sensible?)
- [ ] Look at stillNeeded vs dontCare (is the partition correct?)
- [ ] Check the asks ordering (is prioritization reasonable?)
- [ ] Review parent node support (can we bind intermediate expressions?)
