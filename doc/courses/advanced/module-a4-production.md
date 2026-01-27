# Module A4: Production Patterns

In this module, you'll learn patterns for robust, maintainable L4 code in production environments.

## Learning Objectives

By the end of this module, you will be able to:

- Organize large L4 codebases
- Implement comprehensive testing strategies
- Debug common issues effectively
- Integrate L4 with other systems

---

## Organizing Large Codebases

### File Structure

For substantial legal models, organize files by domain:

```
my-legal-system/
├── types/
│   ├── parties.l4        -- Actor types
│   ├── actions.l4        -- Action types
│   ├── records.l4        -- Data records
│   └── enums.l4          -- Enumerations
├── rules/
│   ├── eligibility.l4    -- Eligibility rules
│   ├── obligations.l4    -- MUST/MAY/SHANT
│   ├── procedures.l4     -- Multi-step procedures
│   └── calculations.l4   -- Computational rules
├── tests/
│   ├── unit/
│   │   ├── eligibility-tests.l4
│   │   └── calculation-tests.l4
│   └── integration/
│       ├── happy-paths.l4
│       └── edge-cases.l4
└── main.l4               -- Main entry point with imports
```

### Using IMPORT

Split code across files and import:

```l4
-- main.l4
IMPORT "types/parties.l4"
IMPORT "types/actions.l4"
IMPORT "rules/eligibility.l4"
IMPORT "rules/obligations.l4"

-- Your main definitions here
```

### Sections for Organization

Within files, use sections:

```l4
§ `Charity Registration System`

§§ `Type Definitions`
-- All DECLARE statements

§§ `Eligibility Rules`
-- All eligibility-related DECIDE statements

§§ `Filing Obligations`
-- All DEONTIC-related rules

§§ `Helper Functions`
-- Utility functions

§§ `Tests`
-- #EVAL and #TRACE statements
```

---

## Testing Strategies

### Unit Tests with #EVAL

Test individual functions:

```l4
§ `Unit Tests`

-- Test helper functions
#EVAL `is adult` (Person "Test" 18)    -- TRUE (boundary)
#EVAL `is adult` (Person "Test" 17)    -- FALSE (below boundary)
#EVAL `is adult` (Person "Test" 100)   -- TRUE (well above)

-- Test calculations
#EVAL `monthly payment` (LoanTerms (USD 10000) 0.12 12)
-- Expected: approximately $888.49

-- Test with edge values
#EVAL `monthly payment` (LoanTerms (USD 0) 0.12 12)
-- Expected: $0 (no principal)
```

### Integration Tests with #TRACE

Test complete workflows:

```l4
§ `Integration Tests`

§§ `Happy Paths`

-- Complete successful workflow
#TRACE `complete procedure` testInput AT 0 WITH
    PARTY Actor1 DOES Action1 AT 5
    PARTY Actor2 DOES Action2 AT 15
    PARTY Actor1 DOES Action3 AT 25
-- Expected: FULFILLED

§§ `Edge Cases`

-- Boundary timing
#TRACE `time-sensitive rule` testInput AT 0 WITH
    PARTY Actor1 DOES Action1 AT 14  -- Exactly at deadline
-- Expected: FULFILLED (just in time)

#TRACE `time-sensitive rule` testInput AT 0 WITH
    PARTY Actor1 DOES Action1 AT 15  -- One day late
-- Expected: BREACH or enters late path

§§ `Failure Scenarios`

-- Missing required action
#TRACE `required action rule` testInput AT 0 WITH
    -- No actions performed
-- Expected: BREACH (timeout)
```

### Test Data Factory

Create reusable test data:

```l4
§ `Test Data Factory`

-- Valid test entities
validPerson MEANS Person "Valid Person" 30 FALSE (LIST)
validCharity MEANS RegisteredCharity "Test Charity" "CH999" Active ...

-- Invalid test entities
underagePerson MEANS Person "Minor" 16 FALSE (LIST)
bankruptPerson MEANS Person "Bankrupt" 40 TRUE (LIST)

-- Parameterized test data
GIVEN age IS A NUMBER
GIVETH A Person
`person with age` MEANS Person "Test" age FALSE (LIST)

-- Test: boundary conditions
#EVAL `is adult` (`person with age` 17)   -- FALSE
#EVAL `is adult` (`person with age` 18)   -- TRUE
#EVAL `is adult` (`person with age` 19)   -- TRUE
```

---

## Debugging Patterns

### Common Errors and Fixes

| Error                     | Likely Cause              | Fix                                           |
| ------------------------- | ------------------------- | --------------------------------------------- |
| "Type not in scope"       | Using type before DECLARE | Move DECLARE before use                       |
| "Expected BOOLEAN, got X" | Wrong type in IF          | Check condition returns BOOLEAN               |
| "Not enough arguments"    | Missing function args     | Count parameters in GIVEN                     |
| "Ambiguous parse"         | Missing parentheses       | Add parentheses around expressions            |
| "Unexpected indent"       | Inconsistent spacing      | Use consistent indentation (spaces, not tabs) |

### The Parentheses Rule

When in doubt, add parentheses:

```l4
-- ❌ Ambiguous
length charity's governors > 0

-- ✅ Clear
length (charity's governors) > 0

-- ❌ Ambiguous
all (GIVEN g YIELD g's age >= 18) charity's governors

-- ✅ Clear
all (GIVEN g YIELD g's age >= 18) (charity's governors)
```

### Debug with #EVAL

Isolate problems by evaluating sub-expressions:

```l4
-- Full expression fails
#EVAL `complex function` complexInput

-- Debug by breaking apart
#EVAL complexInput                           -- Check input is valid
#EVAL complexInput's field1                  -- Check field access
#EVAL `helper function` (complexInput's field1)  -- Check helper
```

### Trace Intermediate Values

Use WHERE to name and inspect intermediate values:

```l4
GIVEN x IS A SomeType
GIVETH A Result
`debug me` MEANS
    finalResult
    WHERE
        step1 MEANS `first operation` x
        step2 MEANS `second operation` step1
        step3 MEANS `third operation` step2
        finalResult MEANS `final operation` step3

-- Debug by evaluating each step
#EVAL step1 WHERE step1 MEANS `first operation` testInput
#EVAL step2 WHERE ... -- and so on
```

---

## Integration Patterns

### JSON Input/Output

L4 supports JSON encoding and decoding for integration:

```l4
-- Encode a record to JSON
#EVAL JSONENCODE myCharity

-- Decode JSON to a record
GIVEN jsonString IS A STRING
GIVETH A MAYBE RegisteredCharity
`parse charity` MEANS JSONDECODE jsonString
```

### REST API Integration

The Decision Service provides a REST API:

```bash
# Start the decision service
cabal run jl4-decision-service -- myfile.l4

# Call via HTTP
curl -X POST http://localhost:8080/evaluate \
  -H "Content-Type: application/json" \
  -d '{"function": "is eligible", "args": {"person": {"name": "Test", "age": 25}}}'
```

### Web Form Generation

L4 can generate web forms from type definitions:

```l4
-- Types become form fields
DECLARE Application
    HAS name IS A STRING          -- Text input
        age IS A NUMBER           -- Number input
        status IS A Status        -- Dropdown
        purposes IS A LIST OF Purpose  -- Multi-select
```

---

## Performance Considerations

### Avoid Deep Recursion

L4 uses lazy evaluation but deep recursion can be slow:

```l4
-- ❌ Potentially slow for large n
GIVEN n IS A NUMBER
GIVETH A NUMBER
`sum to n` MEANS
    IF n = 0 THEN 0
    ELSE n + `sum to n` (n - 1)

-- ✅ Better: use formula
`sum to n formula` MEANS n * (n + 1) / 2
```

### Use Prelude Functions

Prefer built-in functions over manual recursion:

```l4
-- ❌ Manual recursion
GIVEN xs IS A LIST OF NUMBER
GIVETH A NUMBER
`my sum` MEANS
    CONSIDER xs
    WHEN EMPTY THEN 0
    WHEN x FOLLOWED BY rest THEN x + `my sum` rest

-- ✅ Use prelude
`my sum` MEANS fold (GIVEN a b YIELD a + b) 0 xs
```

### Limit Trace Depth

When testing with #TRACE, limit scenario complexity:

```l4
-- ❌ Very long trace (slow)
#TRACE `loan contract` AT 0 WITH
    -- 360 monthly payments...

-- ✅ Test with shorter period
#TRACE `loan contract with` 3 AT 0 WITH  -- 3 payments
    PARTY Borrower DOES `pay` AT 30
    PARTY Borrower DOES `pay` AT 60
    PARTY Borrower DOES `pay` AT 90
```

---

## Deployment Checklist

Before deploying L4 code:

### Code Quality

- [ ] All #EVAL tests pass
- [ ] All #TRACE scenarios work correctly
- [ ] No ambiguous parses or type errors
- [ ] Code is properly sectioned and commented

### Test Coverage

- [ ] Happy path tested
- [ ] Boundary conditions tested
- [ ] Error cases tested
- [ ] All CONSIDER branches have tests

### Documentation

- [ ] Types documented with comments
- [ ] Complex rules explained
- [ ] External references (legislation) cited
- [ ] Examples provided

### Integration

- [ ] JSON schema generated if needed
- [ ] API endpoints documented
- [ ] Input validation confirmed
- [ ] Error responses defined

---

## Summary

| Area             | Best Practice                                  |
| ---------------- | ---------------------------------------------- |
| **Organization** | Split into files by domain, use sections       |
| **Testing**      | Unit tests (#EVAL), integration tests (#TRACE) |
| **Debugging**    | Isolate with #EVAL, use parentheses            |
| **Integration**  | JSON for data exchange, REST API for services  |
| **Performance**  | Prefer formulas over recursion, use prelude    |

---

## Course Complete!

You've finished the Advanced Course. You now know how to:

- Model complete regulatory schemes using the three-layer approach
- Implement cross-cutting concerns (timing, notices, appeals)
- Build complex contracts with recursive obligations
- Organize and test production L4 code

### Continue Learning

- **[Tutorials](../../tutorials/README.md)** - Task-focused guides
- **[Concepts](../../concepts/README.md)** - Theoretical foundations
- **[Reference](../../reference/README.md)** - Complete syntax reference

### Get Help

- **GitHub Issues**: Report bugs or ask questions
- **Example Code**: Study `jl4/examples/` and `jl4/experiments/`
