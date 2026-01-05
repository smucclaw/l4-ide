# Backtick Binding Issue in Query Planning

## Summary

Query planning works correctly, but there's a **usability issue** with binding parameter names that have backticks in their internal representation.

## The Problem

### Expected Behavior (User Perspective)
User sees parameter name `age_21_plus` in JSON schema:
```json
{
  "parameters": {
    "properties": {
      "age_21_plus": { "type": "boolean" }
    }
  }
}
```

User sends binding:
```json
{"fnArguments": {"age_21_plus": true}}
```

### Actual Behavior (System Perspective)
Internal label representation uses backticks: `` `age_21_plus` ``

Binding matcher requires **exact label match**, so `age_21_plus` doesn't match `` `age_21_plus` ``.

### Workaround
Send bindings with backticks matching the internal label:
```json
{"fnArguments": {"`age_21_plus`": true}}
```

## Test Results

When using correct backtick format, all scenarios work perfectly:

| Scenario | Bindings | Determined | Still Needed | Status |
|----------|----------|------------|--------------|--------|
| Age 21+ | `` {"`age_21_plus`": true} `` | null | married, spousal_approval, beer_only | ✅ PASS |
| Under 21 | `` {"`age_21_plus`": false} `` | null | parental_approval, legally_emancipated | ✅ PASS |
| 21+, unmarried | `` {"`age_21_plus`": true, "married": false} `` | **true** | [] | ✅ PASS |
| 21+, beer only | `` {"`age_21_plus`": true, "`beer_only`": true} `` | **true** | [] | ✅ PASS |

## Root Cause

The binding matcher in `jl4-query-plan/src/L4/Decision/QueryPlan.hs` (lines 320-341) attempts to match bindings by:

1. **Exact label match** in `labelToUniques` (line 332)
2. Unique ID as decimal string (line 333)
3. AtomId UUID (line 334)
4. Leaf uniques from InputRef (line 337)

The label match is exact string comparison - no normalization for backticks.

There is a `stripBackticks` function (line 234-237), but it's only used for parsing projection labels like `person's name`, not for general binding key matching.

## Why Some Variables Have Backticks

In L4, backticks create quoted identifiers (similar to Haskell). Variables declared without spaces don't need backticks:

```l4
age_21_plus IS A BOOLEAN  -- Declaration: no backticks needed
```

But when used in expressions or DECIDE names, they may get backticks in the internal representation:

```l4
DECIDE `may purchase alcohol` IF ...  -- Multi-word identifier requires backticks
```

The inconsistency arises because:
- JSON schema parameter names strip backticks for API usability
- Internal ladder representation preserves backticks for exact roundtripping
- Binding matcher uses internal representation

## Potential Solutions

### Option 1: Normalize binding keys (Recommended)
Extend `labelToUniques` matching to try both with and without backticks:
- `age_21_plus` matches `` `age_21_plus` ``
- `` `age_21_plus` `` matches `age_21_plus`

### Option 2: Strip backticks from internal labels
Change how labels are stored in `cached.varLabelByUnique` to always strip backticks.

**Trade-off:** May break exact roundtripping for display purposes.

### Option 3: Document and expect backticks
Update API documentation to specify that binding keys must match internal labels exactly, including backticks.

**Trade-off:** Poor UX - users must inspect `stillNeeded` labels to know which format to use.

## Related Files

- `jl4-query-plan/src/L4/Decision/QueryPlan.hs` (lines 234-237, 293-341)
- `jl4-decision-service/src/Backend/DecisionQueryPlan.hs` (lines 309-333)
- Test file: `jl4/experiments/query-planner-tests/04-alcohol-purchase.l4`

## Testing Commands

```bash
# Test with correct backtick format
curl -s -X POST "http://localhost:8001/functions/may%20purchase%20alcohol/query-plan" \
  -H "Content-Type: application/json" \
  -d '{"fnArguments": {"`age_21_plus`": true}}' \
  | jq '{determined, stillNeeded: [.stillNeeded[] | .label]}'

# Compare: without backticks (doesn't work)
curl -s -X POST "http://localhost:8001/functions/may%20purchase%20alcohol/query-plan" \
  -H "Content-Type: application/json" \
  -d '{"fnArguments": {"age_21_plus": true}}' \
  | jq '{determined, stillNeeded: [.stillNeeded[] | .label]}'
```

## Impact

**Severity:** Medium
- Query planning logic works correctly
- Workaround exists (use backticks in binding keys)
- Mainly affects API usability and developer experience

**Recommendation:** Implement Option 1 (normalize binding keys) to match both variants.
