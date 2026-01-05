# Non-Monotonicity in Query Planning

## The Phenomenon

The query planner exhibits **non-monotonic** behavior with respect to variable relevance:

- Adding knowledge can make variables **irrelevant** (relevance decreases)
- Adding *different* knowledge can make them **relevant again** (relevance increases)
- The set of "relevant variables" is **not monotonic** in the size of our knowledge base

## Classical Monotonicity vs. Query Planning

### Classical Monotonic Logic
```
Knowledge: ∅
Conclusions: {A, B, C}

Add knowledge: {X}
Conclusions: {A, B, C, D}  ← Can only grow

Add more knowledge: {X, Y}
Conclusions: {A, B, C, D, E}  ← Still only grows
```

**Property**: More knowledge → More conclusions (or same)
**Never**: More knowledge → Fewer conclusions

### Query Planning (Non-Monotonic)
```
Knowledge: ∅
Relevant vars: {switch, option_a, option_b}

Add knowledge: {switch = true}
Relevant vars: {option_a}  ← Shrinks! option_b became don't-care

Add different knowledge: {switch = false}
Relevant vars: {option_b}  ← Different set! option_a became don't-care

Change knowledge: {switch = true}
Relevant vars: {option_a}  ← Changed again!
```

**Property**: Same-sized knowledge → Different relevant sets
**Key**: Relevance is **context-dependent**, not monotonic

## Why This Matters

### 1. UI State Management
The UI must handle **reactive relevance changes**:
```
User action: Set switch = true
Effect: option_b grayed out (don't-care)

User action: Set switch = false
Effect: option_b un-grayed (now relevant!)
       option_a grayed out (now don't-care)
```

**Implication**: Can't just accumulate "disabled" fields - must re-evaluate on every change

### 2. Caching Strategies
Simple caching breaks:
```
Cache key: "switch"
Cache value: {relevant: [option_a, option_b]}  ← Generic answer

But actual relevance depends on the VALUE:
  switch=true  → {relevant: [option_a]}
  switch=false → {relevant: [option_b]}
```

**Implication**: Must cache with full binding context, not just variable names

### 3. User Mental Model
Users need to understand that relevance is **dynamic**:
- "This question is grayed out" ← Now
- "But it might become relevant again" ← Later
- "So I can still answer it" ← User affordance

**Implication**: UI should indicate "currently not needed" rather than "never needed"

## Theoretical Foundations

### Default Logic (Reiter 1980)
Non-monotonic reasoning allows **defeasible** conclusions:
```
normally(bird(X) → flies(X))
bird(tweety)
penguin(tweety)
---
flies(tweety) :- bird(tweety), not ab(tweety)  ← Default assumption
ab(tweety) :- penguin(tweety)                  ← Exception
---
Conclusion: ¬flies(tweety)  ← Default was defeated
```

### Query Planning Analogy
```
normally(relevant(option_b))  ← By default, might be needed
switch = true                 ← New information
---
irrelevant(option_b) :- switch = true  ← Defeasible conclusion
```

Adding `switch = true` **defeats** the default that `option_b` is relevant.

### Circumscription (McCarthy 1980)
Minimize assumptions about what's relevant:
```
Relevant(X) ← affects_outcome(X)
affects_outcome(option_a) :- switch = true
affects_outcome(option_b) :- switch = false
---
If switch = true:
  Minimize Relevant → {option_a}  ← option_b doesn't affect outcome

If switch = false:
  Minimize Relevant → {option_b}  ← option_a doesn't affect outcome
```

## Implementation Consequences

### 1. Query Planner Must Be Stateless (Functional)
```haskell
-- Good: Pure function of full bindings
queryPlan :: Bindings -> QueryPlanResponse
queryPlan bindings = ...

-- Bad: Maintains state about what was previously relevant
queryPlanStateful :: Query -> IO QueryPlanResponse
queryPlanStateful query = do
  prevRelevant <- get  -- ← Broken! Relevance is non-monotonic
  ...
```

### 2. UI Must Re-Query on Every Change
```typescript
// Good: Always re-query with full current bindings
const onInputChange = (var: string, value: boolean) => {
  const newBindings = {...bindings, [var]: value}
  const newPlan = await queryPlan(functionId, newBindings)
  updateUI(newPlan)  // ← Might un-gray previously grayed fields!
}

// Bad: Cache relevance across binding changes
const relevanceCache = new Map()  // ← Broken!
```

### 3. Bindings Are the Sole Source of Truth
```
Not: "option_b was irrelevant last time" ← Temporal state
But: "option_b is irrelevant given current bindings" ← Functional dependency
```

## Examples of Non-Monotonic Relevance Changes

### Example 1: If-Then-Else
```
Formula: (switch AND option_a) OR (NOT switch AND option_b)

Bindings: {}
Relevant: {switch, option_a, option_b}

Bindings: {switch: true}
Relevant: {option_a}  ← option_b irrelevant

Bindings: {switch: false}
Relevant: {option_b}  ← option_a irrelevant
```

### Example 2: Disjunctive Paths
```
Formula: (a AND b) OR (c AND d)

Bindings: {}
Relevant: {a, b, c, d}

Bindings: {a: true, b: true}
Relevant: {}  ← All irrelevant! (left branch succeeded)

Bindings: {a: true, b: false}
Relevant: {c, d}  ← c, d relevant again! (left branch failed)
```

### Example 3: Cascade Effects
```
Formula: a AND (b OR (c AND d))

Bindings: {}
Relevant: {a, b, c, d}

Bindings: {b: true}
Relevant: {a}  ← c, d irrelevant (inner OR satisfied)

Bindings: {b: false}
Relevant: {a, c, d}  ← c, d relevant again! (inner OR needs them)

Bindings: {a: false}
Relevant: {}  ← All irrelevant (top-level AND failed)
```

## Testing Non-Monotonicity

See `06-reactive-relevance.l4` for a test case that exercises this:

```bash
# Start: All relevant
./test-api.sh 06-reactive-relevance.l4 '{}'
# relevant: {switch, option_a, option_b}

# Add knowledge: switch=true
./test-api.sh 06-reactive-relevance.l4 '{"label": {"switch": true}}'
# relevant: {option_a}  ← Shrunk!

# Change knowledge: switch=false
./test-api.sh 06-reactive-relevance.l4 '{"label": {"switch": false}}'
# relevant: {option_b}  ← Different!
```

## Related Concepts

- **Defeasible reasoning**: Conclusions can be defeated by new info
- **Belief revision**: Changing beliefs in light of new evidence
- **Closed-world assumption**: What's not known to be true is false
- **Frame problem**: Which things change and which stay the same?

## Further Reading

- Reiter, R. (1980). "A Logic for Default Reasoning"
- McCarthy, J. (1980). "Circumscription—A Form of Non-Monotonic Reasoning"
- Ginsberg, M. (1987). "Readings in Nonmonotonic Reasoning"

## The Bottom Line

**Relevance is not monotonic in the knowledge base.**

This has profound implications for:
- UI design (must handle dynamic graying/un-graying)
- Caching (must include full binding context)
- User experience (allow input on currently-irrelevant fields)
- Implementation (pure functions, stateless query planning)

Welcome to non-monotonic reasoning! 🎉
