# Advanced Topics to Add

These topics were removed from Foundation Module 4 (Decision Logic) as they are too advanced for beginners. They should be incorporated into the Advanced course.

## Recursion

### Concepts to Cover
- Recursive function definitions
- Base cases and recursive cases
- List recursion patterns
- Tail recursion optimization

### Examples to Include

**Factorial** (classic recursion):
```l4
GIVEN n IS A NUMBER
GIVETH A NUMBER
`the factorial of` MEANS
    IF n <= 1
    THEN 1
    ELSE n * (`the factorial of` (n - 1))
```

**List recursion**:
```l4
GIVEN numbers IS A LIST OF NUMBER
GIVETH A NUMBER
`the sum of the list` MEANS
    CONSIDER numbers
    WHEN EMPTY THEN 0
    WHEN x FOLLOWED BY rest THEN x + `the sum of the list` rest
```

### Legal Use Cases for Recursion
- Processing hierarchical organizational structures
- Calculating cumulative effects over time periods
- Traversing chains of contracts or amendments
- Computing transitive relationships (e.g., beneficial ownership chains)

---

## Higher-Order Functions

### Concepts to Cover
- Functions as first-class values
- Functions that take functions as parameters
- Anonymous functions (lambdas) with GIVEN...YIELD
- Common higher-order patterns: map, filter, fold

### Examples to Include

**Lambda functions**:
```l4
-- Double each number
map (GIVEN n YIELD n * 2) (LIST 1, 2, 3)
-- Result: LIST 2, 4, 6

-- Filter positive numbers
filter (GIVEN n YIELD n > 0) (LIST -1, 2, -3, 4)
-- Result: LIST 2, 4
```

**Using prelude functions**:
```l4
IMPORT prelude

-- Check if all satisfy condition
all (GIVEN n YIELD n > 0) `positive numbers`

-- Check if any satisfy condition
any (GIVEN n YIELD n > 100) `incomes`

-- Find first match
find (GIVEN p YIELD p's age >= 18) `applicants`
```

### Legal Use Cases for Higher-Order Functions
- Bulk compliance checking (all parties meet requirement)
- Finding exceptions (any party violates condition)
- Batch processing applications
- Transforming data for reporting
- Applying rules uniformly across lists of entities

---

## Function Composition

### Concepts to Cover
- Composing multiple decisions/computations
- Pipeline style processing
- Building complex decisions from simple ones

### Examples
```l4
-- Compose eligibility checks
`fully qualified` MEANS
    `is citizen` AND `is adult` AND `has no disqualifications`
```

---

## Advanced Pattern Matching

### Concepts to Cover
- Nested CONSIDER patterns
- Pattern matching on complex types
- Guards in pattern matching

---

## Integration Strategy

These topics should be introduced:

1. **Module A5: Recursive Logic** (if needed)
   - Focus on legal scenarios where recursion is natural
   - Emphasize when to use recursion vs iteration

2. **Module A6: Higher-Order Functions and Bulk Processing**
   - Introduce map/filter/fold
   - Show how to process lists of entities uniformly
   - Connect to real-world compliance and reporting needs

3. Throughout modules:
   - Reference the foundation course's focus on single-entity decisions
   - Show how these advanced techniques scale to multiple entities
   - Maintain legal-domain focus rather than pure CS theory

---

## Notes

- Keep examples legal-domain focused
- Always explain **why** these patterns are useful for law
- Don't teach FP for FP's sake - teach it as a tool for legal modeling
- Consider prerequisites: students should be comfortable with Module 4 first
