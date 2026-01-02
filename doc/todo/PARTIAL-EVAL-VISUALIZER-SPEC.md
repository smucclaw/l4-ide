# Specification: Interactive Partial Evaluation Visualizer (Revised)

**Status:** 📋 Draft (rewrite)
**Related Issue:** #638
**Parent Spec:** [BOOLEAN-MINIMIZATION-SPEC.md](./BOOLEAN-MINIMIZATION-SPEC.md)

## Summary

Provide an interactive UI for exploring how an L4 boolean decision reduces as the user supplies inputs.

The UI:

1. Groups parameters into **Not Asked**, **Still Needed**, and **Don't Care**.
2. Shows which parts of the decision structure are **live**, **short‑circuited**, or **irrelevant**.
3. Displays a **restricted** (partially simplified) expression plus a clear **result** when determined.

This spec is UI/UX + local analysis only. It intentionally does **not** define the backend API for partial evaluation (that belongs in the parent spec).

## Critique Of Current Approach (Why Rewrite)

The earlier draft implicitly required _many_ round trips to an evaluator (or an LSP) by doing per‑variable “don’t‑care tests”:

- For every unassigned variable `x`, evaluate `f[x:=T]` and `f[x:=F]` (often via `L4Connection`), then compare results.

Problems:

- **O(n) evaluations per edit**: interactive typing/clicking becomes `2 * (#unassigned vars)` evaluations, which is latency‑sensitive and can thrash the LSP.
- **UI coupled to backend semantics**: the visualizer becomes unusable offline and harder to test; “visualization” shouldn’t depend on RPC for basic shading/bucketing.
- **Incorrect/misleading under unknowns**: comparing three‑valued results (`Unknown`) can label variables as irrelevant when they actually influence whether the result becomes determined later.
- **Hard to extend**: once non‑boolean predicates appear (`age >= 21`), the cofactor approach needs a consistent “atomic predicate” layer anyway.

The better approach: **compile once**, then update cheaply.

## Better Approach: One‑Time Compilation To A Reduced OBDD

Treat the boolean decision as a boolean function over _atomic_ boolean inputs (UBool variables / predicates).

1. Compile the expression once to a **Reduced Ordered Binary Decision Diagram (ROBDD)**.
2. On each assignment change, **restrict** the ROBDD with known bindings and read off:
   - Whether the result is determined (terminal).
   - Which variables still appear (the remaining **support**), i.e. which are still relevant.

Key property: in a reduced OBDD, after restriction, “variable is relevant” ≡ “variable appears in the restricted diagram”. No per‑variable equivalence tests required.

This yields stable, low‑latency UI updates and a clean separation between backend evaluation and frontend visualization.

## Terminology

- **Assignment**: map from variable unique id → `{True, False, Unknown}` (Unknown means “unassigned / not provided yet”).
- **Restrict**: apply all known (True/False) bindings to a diagram, producing a smaller diagram representing `f|assignment`.
- **Support**: the set of variables that the restricted function still depends on.
- **Short‑circuited**: a subtree not evaluated due to left‑to‑right short‑circuiting in the visualizer’s evaluator.

## Goals

- Updates after each click/keystroke feel instantaneous for typical decision sizes.
- “Still Needed” matches the formal definition: inputs that can still change the final result, given what’s known.
- “Don’t Care” is not a heuristic: it means _provably irrelevant_ under current assignments.
- Visual cues (gray/strike/dim) match these buckets and short‑circuit semantics.

## Non‑Goals

- Full boolean minimization / CNF/DNF pretty‑printing.
- Dynamic variable reordering (initially); we choose a deterministic order and optimize later if needed.
- Handling arbitrary non‑boolean arithmetic constraints in the visualizer; those are represented as boolean _atomic predicates_ supplied by earlier compilation stages.

## Data Inputs (What The Visualizer Already Has)

From `@repo/viz-expr` / existing ladder visualizer pipeline:

- Expression tree with stable `IRId`s
- Boolean variables as `UBoolVar` with `name.unique`
- Existing three‑valued evaluator (Kleene) and the ladder graph UI

## Analysis Outputs

```ts
export type RelevanceStatus =
  | "unknown" // before analysis
  | "determined" // assigned or constant node
  | "relevant" // in support of restricted function
  | "irrelevant" // not in support (don’t care)
  | "short-circuited"; // eliminated by eval order

export interface RelevanceAnalysis {
  overallResult: "True" | "False" | "Unknown";
  isDetermined: boolean;

  // Parameter bucketing
  notAsked: string[]; // unassigned vars
  stillNeeded: string[]; // unassigned but relevant
  dontCare: string[]; // unassigned and irrelevant

  // Visualization
  nodeRelevance: Map<IRId, RelevanceStatus>;
  consultedVars: Set<string>;
  shortCircuitedNodes: Set<IRId>;

  // Optional display
  restrictedExpr?: Expr; // syntactic restriction + constant fold (best-effort)
  bddStats?: { nodes: number; support: number };
}
```

## Core Algorithms

### A. Compile Expr → ROBDD (Once Per Expr Version)

Implement a small ROBDD engine in TS:

- `mk(var, low, high)` with a unique table
- `apply(op, a, b)` (memoized) for `AND`, `OR`
- `neg(a)` (memoized)
- `restrict(bdd, bindings)` (memoized per update)
- `support(bdd)` (variables appearing in the restricted diagram)

Variable order for MVP:

1. `GIVEN` declaration order (or stable pre‑order in expression if GIVEN not available)
2. Fallback: lexical order of `name.unique`

### B. Update On Assignment Change (Fast Path)

Given `assignment`:

1. Evaluate expression with existing evaluator to get:
   - `overallResult` under Kleene logic (for immediate UI display)
   - `shortCircuitedNodes`, `consultedVars` (for shading that matches user mental model)
2. Restrict the compiled ROBDD using only known True/False bindings.
3. Compute `support` from the restricted ROBDD.
4. Bucket variables:
   - `notAsked` = variables not in assignment (or set to Unknown)
   - `stillNeeded` = `notAsked ∩ support`
   - `dontCare` = `notAsked \ support`

### C. Node Relevance (For Graph Shading)

Precompute `subtreeVars : Map<IRId, Set<string>>` once per expression by walking the tree.

For each node `n`:

1. If `n.id ∈ shortCircuitedNodes` → `"short-circuited"`
2. Else if evaluator produced a definite value at `n` → `"determined"`
3. Else if `subtreeVars[n.id] ∩ support` is empty → `"irrelevant"`
4. Else → `"relevant"`

This produces stable, explainable shading without trying to “prove” equivalence at every IR node.

### D. Restricted Expression View (Best‑Effort)

To show “simplified to”, do **syntactic restriction**:

- Substitute assigned vars with `TrueE`/`FalseE`
- Apply identity folds (`x ∧ True = x`, `x ∨ False = x`, etc.)
- Preserve original `IRId`s where possible to keep highlighting stable

This view is for human comprehension, not canonical minimization; “Don’t Care” semantics come from ROBDD support, not from this pretty‑printer.

## UI/UX

### Layout

Three panels:

1. **Parameter Buckets** (left)
2. **Graph Visualizer** (center)
3. **Restricted Expression + Result** (right)

### Parameter Buckets

- Each parameter row supports quick assignment `T/F/?`.
- Clicking a parameter focuses/highlights corresponding node(s) in the graph.
- “Still Needed” is ordered by:
  1. earliest variable in ROBDD order (proxy for decision power)
  2. tie‑break: lexical

### Graph Visualizer

- `relevant`: normal/emphasized
- `irrelevant`: dimmed/gray
- `short-circuited`: more strongly dimmed + strike marker
- `determined`: show value indicator as today

### Restricted Expression View

- Shows:
  - original expression (compact)
  - current bindings
  - restricted expression (best‑effort)
  - result banner when determined
  - a compact “Remaining inputs: …” list derived from ROBDD support

## Performance Targets

- After first compilation, updates per assignment change should be:
  - `restrict` + `support` + bucketing: fast enough to feel instant (< ~50ms typical)
- Cache compiled ROBDD by `(docVersionId, exprRootId)`; rebuild only when expression changes.

## Implementation Plan

1. Add ROBDD engine under `ts-shared/l4-ladder-visualizer/src/lib/bdd/`.
2. Add `PartialEvalAnalyzer` that combines:
   - existing evaluator trace (for short‑circuit + intermediates)
   - ROBDD restriction + support (for relevance bucketing)
3. Add `ParameterBuckets.svelte` (UI only) consuming `RelevanceAnalysis`.
4. Extend node/edge styling to use `nodeRelevance`.
5. Add restricted expression view (best‑effort) and result banner.

## Acceptance Criteria

- With the alcohol example, setting `age=30` moves `parental_approval` and `emancipated` to **Don’t Care** immediately.
- If the restricted ROBDD is terminal, UI shows “Result determined” and all unassigned vars are **Don’t Care**.
- Node shading matches:
  - short‑circuit behavior (strike/dim)
  - don’t‑care irrelevance (gray)
  - determined nodes (value indicator)

## Open Questions

- What is the canonical variable order when the UI expression includes derived predicates (e.g. `age>=21`)?
- Should we expose a “why relevant?” explanation using ROBDD path examples (stretch goal)?
- How should we represent “explicitly unknown” vs “not asked” (if/when the parent spec adds that distinction)?
