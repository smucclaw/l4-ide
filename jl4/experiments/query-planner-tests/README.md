# Query Planner Test Cases

This directory contains L4 test files exercising the query planner and symbolic
evaluator. They cover boolean logic, short-circuiting, don't-care detection,
nested logic, parent-node assertions, and reactive relevance.

## Test Levels

### Level 1: Simple Boolean Logic
- `01-simple-and.l4` — AND
- `01-simple-or.l4` — OR
- `01-simple-not.l4` — NOT
- `01-three-vars.l4` — three variables with AND/OR

### Level 2: Short-Circuiting
- `02-and-short-circuit.l4` — `False AND Unknown = False`
- `02-or-short-circuit.l4` — `True OR Unknown = True`

### Level 3: Don't-Care Detection
- `03-dont-care-simple.l4`
- `03-dont-care-branch.l4`

### Level 4: Nested Logic
- `04-nested-and-or.l4`
- `04-alcohol-purchase.l4` — realistic age-based rules

### Level 5: Parent Assertions
- `05-parent-node-assertion.l4`
- `05-nested-parent-assertion.l4`

### Level 6: Reactive Relevance
- `06-reactive-relevance.l4`

## How they're used

These files are reference inputs. The unit test suite in
`jl4-service/test/QueryPlanSpec.hs` exercises analogous logic via the
LSP and jl4-service query plan code paths. You can also load any of
these files in the IDE / web playground and toggle the ladder to see
the elicitation order in action.
