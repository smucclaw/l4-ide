# Query Planner Test Suite

This directory contains test cases for the query planner and symbolic evaluator, organized by complexity level.

## Test Levels

### Level 1: Simple Boolean Logic
- `01-simple-and.l4` - Basic AND operation
- `01-simple-or.l4` - Basic OR operation
- `01-simple-not.l4` - Basic NOT operation
- `01-three-vars.l4` - Three variables with AND/OR

### Level 2: Short-Circuiting
- `02-and-short-circuit.l4` - Test AND short-circuit (False AND Unknown = False)
- `02-or-short-circuit.l4` - Test OR short-circuit (True OR Unknown = True)

### Level 3: Don't-Care Detection
- `03-dont-care-simple.l4` - Simple don't-care variable
- `03-dont-care-branch.l4` - Don't-care in one branch

### Level 4: Nested Logic
- `04-nested-and-or.l4` - Nested AND/OR combinations
- `04-alcohol-purchase.l4` - Realistic example (age-based rules)

## Running Tests

### Start the decision service with test files:
```bash
cd jl4-decision-service
cabal run jl4-decision-service-exe -- --port 8001 \
  --sourcePaths ../jl4/experiments/query-planner-tests
```

### Or use the test runner script:
```bash
./jl4/experiments/query-planner-tests/run-tests.sh
```

## Testing via REPL

```bash
cabal run jl4-repl -- jl4/experiments/query-planner-tests/01-simple-and.l4
> :decides
> :qp function_name a=true
> :qp function_name a=false
```

## Testing via Decision Service API

```bash
# Get function metadata
curl "http://localhost:8001/functions"

# Call query-plan endpoint
curl -X POST "http://localhost:8001/functions/uuid:function/query-plan" \
  -H "Content-Type: application/json" \
  -d '{"label": {"a": true}}'
```

## Expected Behaviors

### Three-Valued Logic (Kleene)
- `False AND Unknown = False` (short-circuit)
- `True OR Unknown = True` (short-circuit)
- `True AND Unknown = Unknown` (need more info)
- `False OR Unknown = Unknown` (need more info)

### Don't-Care Detection
When a variable can't affect the outcome regardless of its value, it should:
- Not appear in `stillNeeded`
- Appear in `dontCare`
- Have `impact = 0`

### Query Planning
The `asks` array should:
- Be ordered by relevance/impact
- Include schema information for each question
- Provide provenance (which inputs does each atom depend on?)
