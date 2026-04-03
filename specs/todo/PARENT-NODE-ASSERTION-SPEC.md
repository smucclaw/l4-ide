# Specification: Parent Node Assertions in Wizard

**Status:** Draft
**Priority:** Medium
**Related:** `WEB-APP-GENERATOR-SPEC.md` (Section 3)
**Branch:** TBD

## Summary

Allow users to assert truth values at **intermediate nodes** (AND/OR) in the decision logic, not just at leaf parameters. This supports privacy-preserving answers like "I satisfy this requirement but don't want to specify how."

## Motivation

### Current Limitation

The wizard currently only allows users to answer questions at the **leaf level** (individual parameters). For a rule like:

```l4
DECIDE `eligible` IF
    `is adult`
    AND (`has income` OR `has assets`)
```

Users must answer:

1. "Are you an adult?" → Yes/No
2. "Do you have income?" → Yes/No
3. "Do you have assets?" → Yes/No

### User Need

A user might **know** they satisfy the income-or-assets requirement without knowing or wanting to disclose which one. They should be able to click on the `(has income OR has assets)` group and assert "Yes, this is satisfied" directly.

**Use cases:**

1. **Privacy preservation**: User qualifies via one path but doesn't want to reveal which
2. **Delegated knowledge**: "My accountant says I qualify for this, but I don't know the details"
3. **Hypothetical exploration**: "If this whole section were true, what would happen?"
4. **Simplification**: Skip detailed questions when the high-level answer is known

## Design

### UI Changes

#### 1. Clickable Parent Nodes

Currently, only `Var` and `App` nodes are clickable in the ladder diagram. Extend to make `And` and `Or` nodes clickable:

```svelte
<!-- Current: only Var/App clickable -->
{#if (node.type === 'Var' || node.type === 'App') && onNodeClick}
  <button onclick={() => handleNodeClick(node)}>...</button>
{/if}

<!-- Proposed: And/Or also clickable -->
{#if onNodeClick && (node.type === 'Var' || node.type === 'App' || node.type === 'And' || node.type === 'Or')}
  <button onclick={() => handleNodeClick(node)}>...</button>
{/if}
```

#### 2. Visual Indication of Parent Assertion

When a parent node is directly asserted:

- Show a distinct style (e.g., double border, badge icon)
- Indicate "Asserted directly" vs "Derived from children"
- Child nodes should show "inherited" or "locked" state

**Visual states for parent nodes:**

| State              | Appearance           | Meaning                              |
| ------------------ | -------------------- | ------------------------------------ |
| Unknown            | Gray border          | No assertion, children also unknown  |
| Derived True       | Green fill           | All children satisfied the condition |
| Derived False      | Red fill             | Children failed to satisfy           |
| **Asserted True**  | Green fill + ✓ badge | User directly asserted True          |
| **Asserted False** | Red fill + ✗ badge   | User directly asserted False         |

#### 3. Child Node Locking

When a parent is asserted:

- Child nodes become visually "locked" (grayed out, non-interactive)
- Tooltip: "This value is determined by the parent assertion"
- User can still see what the children are, but can't modify them

**Unlock flow:**

- Clicking on a locked child shows "Clear parent assertion to modify this"
- Or: clicking parent again cycles: True → False → **Unlocked** → True

### Data Model Changes

#### 1. New Binding Type: Parent Assertions

Current bindings are keyed by leaf parameter paths:

```typescript
type Bindings = Record<string, unknown>;
// e.g., { "age": 30, "has income": true }
```

Extend to support parent node assertions:

```typescript
type ParentAssertion = {
  nodeId: number; // The IRId of the asserted node
  value: boolean; // The asserted value
};

type ExtendedBindings = {
  parameters: Record<string, unknown>; // Leaf bindings
  parentAssertions: ParentAssertion[]; // Parent node assertions
};
```

#### 2. Backend API Changes

The `/query-plan` and `/evaluation` endpoints need to accept parent assertions:

```typescript
// POST /functions/{name}/query-plan
{
  "bindings": {
    "parameters": { "age": 30 },
    "parentAssertions": [
      { "nodeId": 42, "value": true }
    ]
  }
}
```

The evaluator must respect parent assertions:

- If a node has a direct assertion, use that value
- Skip evaluation of children (they're "short-circuited by assertion")
- Return appropriate trace showing "asserted" vs "evaluated"

### Semantic Considerations

#### 1. Assertion Propagation

When user asserts `Or(A, B) = True`:

- The OR is satisfied
- A and B become "don't care" for the overall result
- BUT: we don't know which one is true

This is different from asserting `A = True`:

- The OR is satisfied
- We know A is true
- B becomes don't care

#### 2. Conflict Detection

What if user asserts `Or(A, B) = True` but also `A = False` and `B = False`?

Options:

1. **Prevent**: Don't allow child assertions when parent is asserted (current design: lock children)
2. **Warn**: Show conflict warning, let user resolve
3. **Override**: Parent assertion takes precedence

Recommended: **Option 1** (lock children when parent asserted)

#### 3. Partial Assertions

For `And(A, B, C)`:

- User asserts `And = True` → all children locked as "satisfied"
- User asserts `A = True`, `B = True`, but not `C` → AND still unknown (not all children known)

For `Or(A, B, C)`:

- User asserts `Or = True` → all children locked as "one is satisfied"
- User asserts `A = True` → OR satisfied, B and C become don't care

### Query Planning Impact

When parent is asserted True:

- Children should be removed from `stillNeeded` and `asks`
- Children should appear in `dontCare` (due to parent assertion)

The query planner needs to:

1. Accept parent assertions as input
2. Treat asserted nodes as known values
3. Propagate appropriately through the decision tree

## Implementation Plan

### Phase 1: UI Only (Client-Side Simulation)

1. Make And/Or nodes clickable in LadderDiagram
2. Store parent assertions in local state (not sent to backend)
3. Lock child nodes when parent asserted
4. Visual feedback for assertion states

**Limitation:** Backend still evaluates based on leaf bindings only. Parent assertion is just UI convenience that "pretends" children are answered.

### Phase 2: Backend Support

1. Extend query-plan API to accept parent assertions
2. Modify evaluator to respect parent assertions
3. Update trace generation to show "asserted" vs "evaluated"
4. Extend query planner to handle assertions at any level

### Phase 3: Persistence & Sharing

1. Include parent assertions in session state
2. Allow sharing wizard state with assertions preserved
3. Export/import of partial answers including assertions

## Open Questions

1. **Should parent assertions persist across page reload?**

   - Currently bindings are in local state
   - If we add session persistence, include assertions?

2. **How to handle assertions on deeply nested nodes?**

   - Allow assertion at any level?
   - Or only on "meaningful" groupings (named sub-expressions)?

3. **Should we distinguish "I know this is true" vs "Assume this is true"?**

   - Former: user has actual knowledge
   - Latter: hypothetical exploration
   - Different UI treatment?

4. **What about negation?**

   - `Not(A)` where A is a complex expression
   - Asserting `Not = True` means the inner expression is false
   - UI implications?

5. **Integration with explanation panel?**
   - How to explain decisions that rely on parent assertions?
   - "Result is True because you asserted (X OR Y) = True"

## Success Criteria

1. User can click on AND/OR nodes in ladder diagram
2. Clicking cycles through: Unknown → True → False → Unknown
3. When parent is asserted, child nodes are visually locked
4. Result updates correctly based on parent assertion
5. Clear visual distinction between "derived" and "asserted" values
6. No conflict states possible (children locked when parent asserted)

## References

- `WEB-APP-GENERATOR-SPEC.md` Section 3: "Answer at Any Level of Granularity"
- Three-valued logic (Kleene): handling of unknown values
- Privacy-preserving computation: conceptual parallel
