# Specification: Interactive Partial Evaluation Visualizer

**Status:** 📋 Draft
**Related Issue:** #638
**Parent Spec:** [BOOLEAN-MINIMIZATION-SPEC.md](./BOOLEAN-MINIMIZATION-SPEC.md)
**Branch:** `mengwong/symbeval`

## Executive Summary

This spec defines a UI/UX layer for visualizing **progressive partial evaluation** as users incrementally provide input data to boolean decision functions. As the user provides more information, the system will:

1. **Classify parameters** into three buckets:

   - **Not Asked Yet** - parameters that haven't been assigned values
   - **Still Needed** - parameters that could affect the outcome (relevant)
   - **Don't Care** - parameters that can't affect the outcome (irrelevant)

2. **Visually gray out** subtrees that have been minimized away through:

   - Short-circuit evaluation (e.g., `False AND x` makes `x` irrelevant)
   - Cofactoring (substituting known values and simplifying)
   - Don't-care detection (where `f|_{x=T} = f|_{x=F}`)

3. **Display simplified expressions** showing how the formula reduces as values are provided

## Motivation

The parent spec [BOOLEAN-MINIMIZATION-SPEC.md](./BOOLEAN-MINIMIZATION-SPEC.md) defines the theory and backend API for partial evaluation. This spec focuses on making that capability **interactive and visual** for end users and developers.

### The User Experience Goal

```
User sees alcohol purchase rule with 6 parameters
┌─────────────────────────────────────────────┐
│ Not Asked Yet                               │
│ • age                                       │
│ • married                                   │
│ • spousal_approval                          │
│ • beer_only                                 │
│ • parental_approval                         │
│ • emancipated                               │
└─────────────────────────────────────────────┘

User sets: age = 30
→ Graph: Minor branch grays out
→ Buckets update:
  Not Asked Yet: married, spousal_approval, beer_only
  Don't Care: parental_approval, emancipated ← moved here!

User sets: married = false
→ Graph: Result determined! Whole expression simplifies to True
→ Buckets update:
  Don't Care: spousal_approval, beer_only, parental_approval, emancipated
  Result: ✓ You may purchase alcohol
```

## Current System Analysis

### Existing Infrastructure (l4-ladder-visualizer)

✅ **Already Have:**

1. **Three-valued logic** (`ts-shared/l4-ladder-visualizer/src/lib/eval/type.ts`)

   - `TrueVal`, `FalseVal`, `UnknownVal`
   - Proper Kleene logic semantics

2. **Evaluator with short-circuit logic** (`src/lib/eval/eval.ts`)

   - `evalAndChain`: short-circuits on False
   - `evalOrChain`: short-circuits on True
   - Tracks intermediate results in `Map<IRId, UBoolVal>`

3. **Interactive value assignment** (`ubool-var.svelte:68-73`)

   - Click to cycle through True/False/Unknown
   - Updates flow through `submitNewBinding`

4. **Svelte Flow visualization** (`flow.svelte`, `flow-base.svelte`)

   - Graph rendering with custom nodes
   - Custom edge styling
   - Node selection and context menus

5. **Value indicators** (`value-indicator.svelte`)

   - Visual checkmarks (✓) and crosses (✗) on nodes
   - Color-coded backgrounds (green/red/gray)

6. **Expression IR** (`@repo/viz-expr`)
   - Full expression tree representation
   - Unique IDs for all nodes
   - Type information preserved

### What We Need to Add

❌ **Need to Build:**

1. **Relevance Analyzer**

   - Cofactor operation (substitute variable with constant)
   - Don't-care detection (test if `f|_{x=T} = f|_{x=F}`)
   - Short-circuit tracking (which subtrees were eliminated)
   - Expression simplification (constant folding)

2. **Parameter Buckets Component**

   - Three-column view: Not Asked / Still Needed / Don't Care
   - Click to focus parameter in graph
   - Drag-and-drop to assign values (stretch goal)

3. **Simplified Expression View**

   - Show original expression
   - Show cofactored/simplified expression
   - Highlight eliminated terms

4. **Enhanced Node Styling**

   - Gray out irrelevant nodes
   - Strike-through short-circuited nodes
   - Dim/fade irrelevant edges

5. **Integration Layer**
   - Hook relevance analysis into evaluation pipeline
   - Update UI reactively as assignments change
   - Sync with backend symbolic evaluation (future)

## UI Design

### Layout: Three-Panel Interface

```
┌────────────────────────────────────────────────────────────────────┐
│ Top Bar: Example Selector | Controls | Export                      │
├──────────────┬─────────────────────────────────────┬───────────────┤
│              │                                     │               │
│  Parameter   │      Graph Visualizer               │  Simplified   │
│   Buckets    │                                     │  Expression   │
│              │   ┌─────┐      ┌──────┐            │               │
│ Not Asked:   │   │ age │─────▶│ ≥21? │            │ Original:     │
│  • married   │   └─────┘      └──────┘            │ (age≥21 ∧...) │
│  • spousal   │        │            │               │               │
│              │        ▼            ▼               │ With age=30:  │
│ Still Needed:│   ┌─────────┐  ┌────────┐          │ ¬married ∨... │
│  (none yet)  │   │married? │  │parental│ (grayed) │               │
│              │   └─────────┘  └────────┘          │ Result:       │
│ Don't Care:  │                                     │ (pending...)  │
│  • parental  │                                     │               │
│  • emancip.  │                                     │               │
└──────────────┴─────────────────────────────────────┴───────────────┘
```

### Panel 1: Parameter Buckets (Left Sidebar, ~300px)

**Component:** `ParameterBuckets.svelte`

```typescript
interface ParameterBucketsProps {
  analysis: RelevanceAnalysis;
  assignment: Assignment;
  onParamClick: (unique: string) => void;
  onParamAssign?: (unique: string, value: UBoolVal) => void;
}
```

**Visual Structure:**

```html
<div class="parameter-buckets">
  <!-- Bucket 1: Not Asked Yet -->
  <div class="bucket neutral">
    <h3>Not Asked Yet</h3>
    <p class="bucket-description">
      Parameters you haven't assigned values to yet
    </p>
    <ul>
      <li><button>age</button></li>
      <li><button>married</button></li>
      ...
    </ul>
  </div>

  <!-- Bucket 2: Still Needed (Relevant) -->
  <div class="bucket relevant">
    <h3>Still Needed</h3>
    <p class="bucket-description">These parameters could affect the outcome</p>
    <ul>
      <li class="relevant-param">
        <button>spousal_approval</button>
        <span class="quick-assign">
          <button>T</button>
          <button>F</button>
          <button>?</button>
        </span>
      </li>
    </ul>
  </div>

  <!-- Bucket 3: Don't Care (Irrelevant) -->
  <div class="bucket irrelevant">
    <h3>Don't Care</h3>
    <p class="bucket-description">
      These parameters can't affect the outcome anymore
    </p>
    <ul>
      <li class="irrelevant-param">
        <button><s>parental_approval</s></button>
      </li>
      <li class="irrelevant-param">
        <button><s>emancipated</s></button>
      </li>
    </ul>
  </div>
</div>
```

**Interactions:**

- Click parameter name → focus/highlight in graph, scroll into view
- Click T/F/? buttons → assign value, trigger re-evaluation
- Drag parameter to graph node → assign value (stretch goal)

**Styling:**

```css
.bucket {
  padding: 1rem;
  margin-bottom: 1rem;
  border-radius: 8px;
  border: 2px solid var(--border);
}

.bucket.neutral {
  background: var(--muted);
}

.bucket.relevant {
  background: #fef3c7; /* yellow-100 */
  border-color: #f59e0b; /* yellow-500 */
}

.bucket.irrelevant {
  background: #f3f4f6; /* gray-100 */
  opacity: 0.7;
}

.irrelevant-param {
  text-decoration: line-through;
  color: var(--muted-foreground);
}
```

### Panel 2: Enhanced Graph Visualizer (Center, Flex-Grow)

**Extends:** Existing `flow.svelte` / `flow-base.svelte`

**New Node States:**

```typescript
export type NodeRelevanceStatus =
  | "unknown" // Not yet analyzed
  | "relevant" // Could affect outcome
  | "irrelevant" // Don't care (cofactor test showed no impact)
  | "short-circuited" // Eliminated by short-circuit eval
  | "determined"; // Value is known (True/False)
```

**New CSS Classes:**

```typescript
// Add to node-styles.ts
export const RelevantNodeCSSClass = "relevant-node";
export const IrrelevantNodeCSSClass = "irrelevant-node";
export const ShortCircuitedNodeCSSClass = "short-circuited-node";
```

```css
/* Add to app.css or component styles */

/* Relevant nodes - normal or slightly emphasized */
.relevant-node {
  border-width: 2px;
  border-color: var(--primary);
  box-shadow: 0 0 0 2px rgba(59, 130, 246, 0.1);
}

/* Irrelevant nodes - grayed out */
.irrelevant-node {
  opacity: 0.35;
  filter: grayscale(80%);
  background: var(--muted) !important;
}

.irrelevant-node .label-wrapper-for-content-bearing-sf-node {
  color: var(--muted-foreground) !important;
}

/* Short-circuited nodes - struck through */
.short-circuited-node {
  opacity: 0.25;
  position: relative;
}

.short-circuited-node::after {
  content: "";
  position: absolute;
  top: 50%;
  left: 0;
  right: 0;
  height: 2px;
  background: var(--destructive);
  transform: translateY(-50%);
}

/* Edge styling */
.irrelevant-edge {
  stroke: var(--muted-foreground) !important;
  stroke-opacity: 0.2 !important;
  stroke-dasharray: 5, 5;
}

.relevant-edge {
  stroke: var(--primary) !important;
  stroke-width: 2.5px !important;
}

.short-circuited-edge {
  stroke: var(--destructive) !important;
  stroke-opacity: 0.15 !important;
  stroke-dasharray: 3, 3;
}
```

**Modified Components:**

```svelte
<!-- ubool-var.svelte - add relevance styling -->
<script lang="ts">
  // ... existing code ...

  // NEW: Get relevance status from ladder graph
  const relevanceStatus = $derived(
    ladderGraph.getRelevanceStatus?.(data.context, node) ?? 'unknown'
  )

  const relevanceClass = $derived(
    match(relevanceStatus)
      .with('irrelevant', () => IrrelevantNodeCSSClass)
      .with('short-circuited', () => ShortCircuitedNodeCSSClass)
      .with('relevant', () => RelevantNodeCSSClass)
      .otherwise(() => '')
  )
</script>

<ValueIndicator
  value={node.getValue(data.context, ladderGraph)}
  additionalClasses={[
    'ubool-var-node-border',
    maybeHighlightedStyle,
    relevanceClass  // NEW
  ]}
>
  <!-- ... existing content ... -->
</ValueIndicator>
```

```svelte
<!-- ladder-edge.svelte - add relevance styling -->
<script lang="ts">
  // ... existing code ...

  // NEW: Determine edge relevance
  const sourceRelevance = $derived(
    ladderGraph.getRelevanceStatus?.(context, sourceNode)
  )
  const targetRelevance = $derived(
    ladderGraph.getRelevanceStatus?.(context, targetNode)
  )

  const edgeRelevanceClass = $derived(
    // Edge is irrelevant if either endpoint is
    sourceRelevance === 'irrelevant' || targetRelevance === 'irrelevant'
      ? 'irrelevant-edge'
      : sourceRelevance === 'short-circuited' || targetRelevance === 'short-circuited'
      ? 'short-circuited-edge'
      : 'relevant-edge'
  )
</script>

<BaseEdge
  path={edgePath}
  markerEnd={markerEnd}
  class={edgeRelevanceClass}  <!-- NEW -->
  style={edgeStyle}
/>
```

### Panel 3: Simplified Expression View (Right Sidebar or Bottom, ~300px)

**Component:** `SimplifiedExpressionView.svelte`

```typescript
interface SimplifiedExpressionViewProps {
  originalExpr: Expr;
  analysis: RelevanceAnalysis;
  assignment: Assignment;
}
```

**Visual Structure:**

```html
<div class="simplified-expr-view">
  <!-- Original Expression -->
  <div class="expr-section">
    <h3>Original Expression</h3>
    <pre class="expr-display">
(age ≥ 21 ∧ (¬married ∨ spousal_approval ∨ beer_only))
∨ (age < 21 ∧ (parental_approval ∨ emancipated))</pre
    >
  </div>

  <!-- Current Assignments -->
  <div class="expr-section">
    <h3>Given</h3>
    <ul class="assignment-list">
      <li>age = 30 <span class="badge">✓</span></li>
      <li>married = false <span class="badge">✓</span></li>
    </ul>
  </div>

  <!-- Cofactored Expression -->
  <div class="expr-section">
    <h3>Simplified To</h3>
    <pre class="expr-display simplified">
¬married ∨ spousal_approval ∨ beer_only

Evaluates to: <strong class="result-true">TRUE</strong>
<span class="explanation">(since ¬false = true)</span>
    </pre>

    <!-- Grayed out eliminated branches -->
    <details class="eliminated-branches">
      <summary>Eliminated branches</summary>
      <pre class="expr-display eliminated">
<s>age < 21 ∧ (parental_approval ∨ emancipated)</s>
  ↳ Reason: age = 30, so age < 21 is false
      </pre>
    </details>
  </div>

  <!-- Result (if determined) -->
  <div class="expr-section result">
    <h3>Result</h3>
    <div class="result-display true">
      <span class="result-icon">✓</span>
      <span class="result-text">You may purchase alcohol</span>
    </div>
  </div>
</div>
```

**Styling:**

```css
.simplified-expr-view {
  padding: 1rem;
  font-size: 0.9rem;
  overflow-y: auto;
}

.expr-section {
  margin-bottom: 1.5rem;
  padding: 1rem;
  background: var(--card);
  border-radius: 8px;
  border: 1px solid var(--border);
}

.expr-display {
  font-family: "JetBrains Mono", "Fira Code", monospace;
  font-size: 0.85rem;
  line-height: 1.6;
  padding: 0.75rem;
  background: var(--muted);
  border-radius: 4px;
  overflow-x: auto;
}

.expr-display.simplified {
  background: #fef3c7; /* yellow-100 */
  border-left: 4px solid #f59e0b; /* yellow-500 */
}

.expr-display.eliminated {
  opacity: 0.5;
  color: var(--muted-foreground);
}

.result-display {
  display: flex;
  align-items: center;
  gap: 1rem;
  padding: 1rem;
  border-radius: 8px;
  font-size: 1.1rem;
  font-weight: 600;
}

.result-display.true {
  background: #dcfce7; /* green-100 */
  color: #166534; /* green-800 */
}

.result-display.false {
  background: #fee2e2; /* red-100 */
  color: #991b1b; /* red-800 */
}

.result-icon {
  font-size: 2rem;
}
```

## Technical Architecture

### 1. Relevance Analyzer

**File:** `ts-shared/l4-ladder-visualizer/src/lib/eval/relevance.ts`

```typescript
import type { Expr, UBoolVal } from "./type.js";
import type { Assignment } from "./assignment.js";
import type { IRId } from "@repo/viz-expr";
import { Evaluator, type EvalResult } from "./eval.js";
import { TrueVal, FalseVal, UnknownVal } from "./type.js";
import { match } from "ts-pattern";

/**
 * Relevance status of a variable or expression node
 */
export type RelevanceStatus =
  | "unknown" // Not yet analyzed
  | "relevant" // Could affect the outcome
  | "irrelevant" // Don't care - can't affect outcome
  | "short-circuited" // Eliminated by short-circuit evaluation
  | "determined"; // Has a known value

/**
 * Result of relevance analysis
 */
export interface RelevanceAnalysis {
  // Maps variable unique ID to its relevance status
  variableRelevance: Map<string, RelevanceStatus>;

  // Maps expression IR ID to its relevance status
  nodeRelevance: Map<IRId, RelevanceStatus>;

  // The cofactored (simplified) expression given current assignments
  // Null if expression is fully determined
  simplifiedExpr: Expr | null;

  // Overall result if determinable
  overallResult: UBoolVal;

  // Was the result fully determined?
  isDetermined: boolean;

  // Which variables were consulted during evaluation?
  consultedVars: Set<string>;

  // Which subtrees were short-circuited?
  shortCircuitedNodes: Set<IRId>;
}

/**
 * Analyzes which variables are relevant given current assignments.
 *
 * Algorithm:
 * 1. Perform three-valued evaluation with current assignments
 * 2. If fully determined (True/False), all unassigned vars are irrelevant
 * 3. For each unassigned variable:
 *    a. Cofactor expression with var=True, simplify, evaluate
 *    b. Cofactor expression with var=False, simplify, evaluate
 *    c. If results differ → variable is RELEVANT
 *    d. If results same → variable is IRRELEVANT (don't care)
 * 4. Identify short-circuited subtrees from eval trace
 */
export class RelevanceAnalyzer {
  /**
   * Analyze relevance of variables and expressions
   */
  async analyze(
    expr: Expr,
    assignment: Assignment,
    l4connection: L4Connection,
    verDocId: VersionedDocId,
  ): Promise<RelevanceAnalysis> {
    // 1. Evaluate expression with current assignments
    const evalResult = await Evaluator.eval(
      l4connection,
      verDocId,
      expr,
      assignment,
    );

    const isDetermined = this.isFullyDetermined(evalResult.result);

    // 2. If fully determined, all unassigned variables are irrelevant
    if (isDetermined) {
      const unassignedVars = this.getUnassignedVars(expr, assignment);
      const variableRelevance = new Map<string, RelevanceStatus>();

      for (const varUnique of unassignedVars) {
        variableRelevance.set(varUnique, "irrelevant");
      }

      // Mark all assigned vars as determined
      for (const varUnique of assignment.keys()) {
        variableRelevance.set(varUnique, "determined");
      }

      return {
        variableRelevance,
        nodeRelevance: new Map(), // All nodes effectively irrelevant
        simplifiedExpr: null, // Fully determined
        overallResult: evalResult.result,
        isDetermined: true,
        consultedVars: evalResult.consultedVars ?? new Set(),
        shortCircuitedNodes: evalResult.shortCircuited ?? new Set(),
      };
    }

    // 3. For each unassigned variable, perform don't-care test
    const unassignedVars = this.getUnassignedVars(expr, assignment);
    const variableRelevance = new Map<string, RelevanceStatus>();

    for (const varUnique of unassignedVars) {
      const isRelevant = await this.isDontCare(
        expr,
        varUnique,
        assignment,
        l4connection,
        verDocId,
      );

      variableRelevance.set(varUnique, isRelevant ? "irrelevant" : "relevant");
    }

    // Mark all assigned vars as determined
    for (const varUnique of assignment.keys()) {
      variableRelevance.set(varUnique, "determined");
    }

    // 4. Simplify expression by substituting known values
    const simplifiedExpr = this.simplify(
      this.cofactorMultiple(expr, assignment),
    );

    // 5. Build node relevance map
    const nodeRelevance = this.buildNodeRelevanceMap(
      expr,
      variableRelevance,
      evalResult.shortCircuited ?? new Set(),
    );

    return {
      variableRelevance,
      nodeRelevance,
      simplifiedExpr,
      overallResult: evalResult.result,
      isDetermined: false,
      consultedVars: evalResult.consultedVars ?? new Set(),
      shortCircuitedNodes: evalResult.shortCircuited ?? new Set(),
    };
  }

  /**
   * Test if a variable is a "don't care" given current assignments.
   *
   * A variable x is don't-care if: f|_{x=T} = f|_{x=F}
   * (i.e., the result is the same regardless of x's value)
   */
  private async isDontCare(
    expr: Expr,
    varUnique: string,
    assignment: Assignment,
    l4connection: L4Connection,
    verDocId: VersionedDocId,
  ): Promise<boolean> {
    // Cofactor with x=True
    const exprWithTrue = this.simplify(
      this.cofactorMultiple(this.cofactor(expr, varUnique, true), assignment),
    );

    // Cofactor with x=False
    const exprWithFalse = this.simplify(
      this.cofactorMultiple(this.cofactor(expr, varUnique, false), assignment),
    );

    // Evaluate both
    const resultTrue = await Evaluator.eval(
      l4connection,
      verDocId,
      exprWithTrue,
      assignment,
    );

    const resultFalse = await Evaluator.eval(
      l4connection,
      verDocId,
      exprWithFalse,
      assignment,
    );

    // If results are the same, variable is don't-care
    return this.equalValues(resultTrue.result, resultFalse.result);
  }

  /**
   * Cofactor: substitute a variable with a constant value.
   *
   * This is the key operation from Shannon expansion:
   * f = (x ∧ f|_{x=T}) ∨ (¬x ∧ f|_{x=F})
   *
   * See BOOLEAN-MINIMIZATION-SPEC.md lines 180-210
   */
  private cofactor(expr: Expr, varUnique: string, value: boolean): Expr {
    return match(expr)
      .with({ $type: "UBoolVar" }, (v): Expr => {
        if (v.name.unique === varUnique) {
          // Replace variable with constant
          return value
            ? { $type: "TrueE", id: v.id }
            : { $type: "FalseE", id: v.id };
        }
        return v;
      })
      .with({ $type: "TrueE" }, (e) => e)
      .with({ $type: "FalseE" }, (e) => e)
      .with(
        { $type: "Not" },
        (not): Expr => ({
          ...not,
          negand: this.cofactor(not.negand, varUnique, value),
        }),
      )
      .with(
        { $type: "And" },
        (and): Expr => ({
          ...and,
          args: and.args.map((arg) => this.cofactor(arg, varUnique, value)),
        }),
      )
      .with(
        { $type: "Or" },
        (or): Expr => ({
          ...or,
          args: or.args.map((arg) => this.cofactor(arg, varUnique, value)),
        }),
      )
      .with(
        { $type: "App" },
        (app): Expr => ({
          ...app,
          args: app.args.map((arg) => this.cofactor(arg, varUnique, value)),
        }),
      )
      .exhaustive();
  }

  /**
   * Cofactor multiple variables at once using an Assignment
   */
  private cofactorMultiple(expr: Expr, assignment: Assignment): Expr {
    let result = expr;

    for (const [varUnique, value] of assignment.entries()) {
      const boolValue = match(value)
        .with({ $type: "TrueV" }, () => true)
        .with({ $type: "FalseV" }, () => false)
        .with({ $type: "UnknownV" }, () => null)
        .exhaustive();

      if (boolValue !== null) {
        result = this.cofactor(result, varUnique, boolValue);
      }
    }

    return result;
  }

  /**
   * Simplify expression by evaluating constants.
   *
   * Rules:
   * - True ∧ x → x
   * - False ∧ x → False
   * - True ∨ x → True
   * - False ∨ x → x
   * - ¬True → False
   * - ¬False → True
   * - ¬¬x → x
   */
  private simplify(expr: Expr): Expr {
    return match(expr)
      .with({ $type: "TrueE" }, (e) => e)
      .with({ $type: "FalseE" }, (e) => e)
      .with({ $type: "UBoolVar" }, (e) => e)
      .with({ $type: "Not" }, (not): Expr => {
        const simplified = this.simplify(not.negand);

        return match(simplified)
          .with(
            { $type: "TrueE" },
            () => ({ $type: "FalseE", id: not.id }) as Expr,
          )
          .with(
            { $type: "FalseE" },
            () => ({ $type: "TrueE", id: not.id }) as Expr,
          )
          .with({ $type: "Not" }, (inner) => this.simplify(inner.negand)) // ¬¬x → x
          .otherwise(() => ({ ...not, negand: simplified }));
      })
      .with({ $type: "And" }, (and): Expr => {
        const simplifiedArgs = and.args.map((arg) => this.simplify(arg));

        // If any arg is False, whole AND is False
        if (simplifiedArgs.some((arg) => arg.$type === "FalseE")) {
          return { $type: "FalseE", id: and.id };
        }

        // Filter out True args (True ∧ x = x)
        const nonTrueArgs = simplifiedArgs.filter(
          (arg) => arg.$type !== "TrueE",
        );

        if (nonTrueArgs.length === 0) {
          return { $type: "TrueE", id: and.id };
        }
        if (nonTrueArgs.length === 1) {
          return nonTrueArgs[0];
        }

        return { ...and, args: nonTrueArgs };
      })
      .with({ $type: "Or" }, (or): Expr => {
        const simplifiedArgs = or.args.map((arg) => this.simplify(arg));

        // If any arg is True, whole OR is True
        if (simplifiedArgs.some((arg) => arg.$type === "TrueE")) {
          return { $type: "TrueE", id: or.id };
        }

        // Filter out False args (False ∨ x = x)
        const nonFalseArgs = simplifiedArgs.filter(
          (arg) => arg.$type !== "FalseE",
        );

        if (nonFalseArgs.length === 0) {
          return { $type: "FalseE", id: or.id };
        }
        if (nonFalseArgs.length === 1) {
          return nonFalseArgs[0];
        }

        return { ...or, args: nonFalseArgs };
      })
      .with({ $type: "App" }, (app) => app) // Can't simplify App without evaluation
      .exhaustive();
  }

  /**
   * Build map of expression node IDs to relevance status
   */
  private buildNodeRelevanceMap(
    expr: Expr,
    variableRelevance: Map<string, RelevanceStatus>,
    shortCircuited: Set<IRId>,
  ): Map<IRId, RelevanceStatus> {
    const nodeRelevance = new Map<IRId, RelevanceStatus>();

    const traverse = (e: Expr): RelevanceStatus => {
      // Check if this node was short-circuited
      if (shortCircuited.has(e.id)) {
        nodeRelevance.set(e.id, "short-circuited");
        return "short-circuited";
      }

      const status = match(e)
        .with({ $type: "UBoolVar" }, (v) => {
          return variableRelevance.get(v.name.unique) ?? "unknown";
        })
        .with({ $type: "TrueE" }, () => "determined" as RelevanceStatus)
        .with({ $type: "FalseE" }, () => "determined" as RelevanceStatus)
        .with({ $type: "Not" }, (not) => traverse(not.negand))
        .with({ $type: "And" }, (and) => {
          const childStatuses = and.args.map(traverse);

          // If any child is relevant, the AND is relevant
          if (childStatuses.some((s) => s === "relevant")) {
            return "relevant";
          }
          // If all children are irrelevant/short-circuited, AND is irrelevant
          if (
            childStatuses.every(
              (s) => s === "irrelevant" || s === "short-circuited",
            )
          ) {
            return "irrelevant";
          }
          return "unknown";
        })
        .with({ $type: "Or" }, (or) => {
          const childStatuses = or.args.map(traverse);

          if (childStatuses.some((s) => s === "relevant")) {
            return "relevant";
          }
          if (
            childStatuses.every(
              (s) => s === "irrelevant" || s === "short-circuited",
            )
          ) {
            return "irrelevant";
          }
          return "unknown";
        })
        .with({ $type: "App" }, (app) => {
          const childStatuses = app.args.map(traverse);

          if (childStatuses.some((s) => s === "relevant")) {
            return "relevant";
          }
          if (
            childStatuses.every((s) => s === "irrelevant" || s === "determined")
          ) {
            return "irrelevant";
          }
          return "unknown";
        })
        .exhaustive();

      nodeRelevance.set(e.id, status);
      return status;
    };

    traverse(expr);
    return nodeRelevance;
  }

  /**
   * Get all unique variable IDs that aren't assigned
   */
  private getUnassignedVars(expr: Expr, assignment: Assignment): Set<string> {
    const allVars = this.getAllVars(expr);
    const unassigned = new Set<string>();

    for (const varUnique of allVars) {
      if (!assignment.has(varUnique)) {
        unassigned.add(varUnique);
      }
    }

    return unassigned;
  }

  /**
   * Get all variable unique IDs in an expression
   */
  private getAllVars(expr: Expr): Set<string> {
    const vars = new Set<string>();

    const traverse = (e: Expr) => {
      match(e)
        .with({ $type: "UBoolVar" }, (v) => {
          vars.add(v.name.unique);
        })
        .with({ $type: "Not" }, (not) => traverse(not.negand))
        .with({ $type: "And" }, (and) => and.args.forEach(traverse))
        .with({ $type: "Or" }, (or) => or.args.forEach(traverse))
        .with({ $type: "App" }, (app) => app.args.forEach(traverse))
        .otherwise(() => {});
    };

    traverse(expr);
    return vars;
  }

  /**
   * Check if a value is fully determined (not Unknown)
   */
  private isFullyDetermined(value: UBoolVal): boolean {
    return value.$type === "TrueV" || value.$type === "FalseV";
  }

  /**
   * Check if two UBoolVals are equal
   */
  private equalValues(v1: UBoolVal, v2: UBoolVal): boolean {
    return v1.$type === v2.$type;
  }
}
```

### 2. Enhanced Evaluator with Dependency Tracking

**File:** `ts-shared/l4-ladder-visualizer/src/lib/eval/eval.ts` (modifications)

```typescript
// EXTEND existing EvalResult interface
export interface EvalResult {
  result: UBoolVal
  intermediate: Map<IRId, UBoolVal>

  // NEW: Track which variables were consulted during evaluation
  consultedVars?: Set<string>

  // NEW: Track which expression nodes were short-circuited
  shortCircuited?: Set<IRId>
}

// In evalAndChain function (line 198)
function evalAndChain(
  bools: UBoolVal[],
  exprIds: IRId[]
): { result: UBoolVal; shortCircuited: Set<IRId> } {
  // Find first False value
  const falseIndex = bools.findIndex(isFalseVal)

  if (falseIndex !== -1) {
    // Everything after the first False is short-circuited
    const shortCircuited = new Set(exprIds.slice(falseIndex + 1))
    return {
      result: new FalseVal(),
      shortCircuited
    }
  }

  if (bools.every(isTrueVal)) {
    return {
      result: new TrueVal(),
      shortCircuited: new Set()
    }
  }

  return {
    result: new UnknownVal(),
    shortCircuited: new Set()
  }
}

// Similar for evalOrChain (line 208)
function evalOrChain(
  bools: UBoolVal[],
  exprIds: IRId[]
): { result: UBoolVal; shortCircuited: Set<IRId> } {
  // Find first True value
  const trueIndex = bools.findIndex(isTrueVal)

  if (trueIndex !== -1) {
    // Everything after the first True is short-circuited
    const shortCircuited = new Set(exprIds.slice(trueIndex + 1))
    return {
      result: new TrueVal(),
      shortCircuited
    }
  }

  if (bools.every(isFalseVal)) {
    return {
      result: new FalseVal(),
      shortCircuited: new Set()
    }
  }

  return {
    result: new UnknownVal(),
    shortCircuited: new Set()
  }
}

// UPDATE And case in eval_ (line 115)
.with({ $type: 'And' }, async (expr: And) => {
  const andResults = await Promise.all(
    expr.args.map((arg) => eval_(arg, intermediate))
  )

  const { result, shortCircuited } = evalAndChain(
    andResults.map((res) => res.result),
    expr.args.map((arg) => arg.id)
  )

  // Combine short-circuited sets from children
  const allShortCircuited = new Set([
    ...shortCircuited,
    ...andResults.flatMap(res => [...(res.shortCircuited ?? [])])
  ])

  const finalIntermediate = combineIntermediates(
    andResults.map((res) => res.intermediate)
  ).set(expr.id, result)

  return {
    result,
    intermediate: finalIntermediate,
    shortCircuited: allShortCircuited,
    consultedVars: new Set([
      ...andResults.flatMap(res => [...(res.consultedVars ?? [])])
    ]),
  }
})

// Similar updates for Or case
```

### 3. Integration into LadderGraph

**File:** `ts-shared/l4-ladder-visualizer/src/lib/layout-ir/ladder-graph/ladder.svelte.ts`

```typescript
import {
  RelevanceAnalyzer,
  type RelevanceAnalysis,
} from "$lib/eval/relevance.js";

export class NNFLadderGraphLirNode extends LadderGraphLirNode {
  // ... existing code ...

  // NEW: Store current relevance analysis
  private relevanceAnalysis: RelevanceAnalysis | null = $state(null);

  // NEW: Recompute relevance analysis when assignment changes
  private async updateRelevanceAnalysis(context: Context) {
    const analyzer = new RelevanceAnalyzer();
    const expr = this.getExpr(context);

    this.relevanceAnalysis = await analyzer.analyze(
      expr,
      this.assignment,
      this.ladderEnv.getL4Connection(),
      this.ladderEnv.getVersionedTextDocIdentifier(),
    );

    console.log("Relevance analysis updated:", this.relevanceAnalysis);
  }

  // NEW: Public getter for relevance analysis
  getRelevanceAnalysis(): RelevanceAnalysis | null {
    return this.relevanceAnalysis;
  }

  // NEW: Get relevance status for a specific variable
  getRelevanceStatus(context: Context, node: UBoolVarLirNode): RelevanceStatus {
    const unique = node.getUnique(context);
    return this.relevanceAnalysis?.variableRelevance.get(unique) ?? "unknown";
  }

  // NEW: Get relevance status for an expression node by ID
  getNodeRelevanceStatus(nodeId: IRId): RelevanceStatus {
    return this.relevanceAnalysis?.nodeRelevance.get(nodeId) ?? "unknown";
  }

  // OVERRIDE: submitNewBinding to trigger relevance update
  override submitNewBinding(context: Context, binding: Binding) {
    super.submitNewBinding(context, binding);

    // Trigger relevance analysis update (async)
    this.updateRelevanceAnalysis(context).catch((err) => {
      console.error("Error updating relevance analysis:", err);
    });
  }

  // ... rest of existing code ...
}
```

### 4. New Components

#### ParameterBuckets.svelte

**File:** `ts-shared/l4-ladder-visualizer/src/lib/displayers/parameter-buckets.svelte`

```svelte
<script lang="ts">
  import type { RelevanceAnalysis } from '$lib/eval/relevance.js'
  import type { Assignment } from '$lib/eval/assignment.js'
  import { TrueVal, FalseVal, UnknownVal } from '$lib/eval/type.js'
  import { match } from 'ts-pattern'

  interface Props {
    analysis: RelevanceAnalysis | null
    assignment: Assignment
    onParamClick?: (unique: string) => void
    onParamAssign?: (unique: string, value: 'True' | 'False' | 'Unknown') => void
  }

  let {
    analysis,
    assignment,
    onParamClick = () => {},
    onParamAssign = () => {}
  }: Props = $props()

  // Group variables by relevance status
  const notAsked = $derived(
    analysis
      ? [...analysis.variableRelevance.entries()]
          .filter(([unique, status]) =>
            status === 'unknown' && !assignment.has(unique)
          )
          .map(([unique]) => unique)
      : []
  )

  const relevant = $derived(
    analysis
      ? [...analysis.variableRelevance.entries()]
          .filter(([_, status]) => status === 'relevant')
          .map(([unique]) => unique)
      : []
  )

  const irrelevant = $derived(
    analysis
      ? [...analysis.variableRelevance.entries()]
          .filter(([_, status]) =>
            status === 'irrelevant' || status === 'short-circuited'
          )
          .map(([unique]) => unique)
      : []
  )

  function handleAssign(unique: string, value: 'True' | 'False' | 'Unknown') {
    onParamAssign?.(unique, value)
  }
</script>

<div class="parameter-buckets">
  <!-- Bucket 1: Not Asked Yet -->
  <div class="bucket neutral">
    <h3 class="bucket-title">Not Asked Yet</h3>
    <p class="bucket-description">
      Parameters you haven't assigned values to yet
    </p>
    {#if notAsked.length === 0}
      <p class="empty-message">All parameters have been assigned</p>
    {:else}
      <ul class="param-list">
        {#each notAsked as unique}
          <li class="param-item">
            <button
              class="param-name"
              onclick={() => onParamClick(unique)}
            >
              {unique}
            </button>
            <div class="quick-assign">
              <button
                class="assign-btn true"
                onclick={() => handleAssign(unique, 'True')}
                title="Set to True"
              >
                T
              </button>
              <button
                class="assign-btn false"
                onclick={() => handleAssign(unique, 'False')}
                title="Set to False"
              >
                F
              </button>
              <button
                class="assign-btn unknown"
                onclick={() => handleAssign(unique, 'Unknown')}
                title="Set to Unknown"
              >
                ?
              </button>
            </div>
          </li>
        {/each}
      </ul>
    {/if}
  </div>

  <!-- Bucket 2: Still Needed (Relevant) -->
  <div class="bucket relevant">
    <h3 class="bucket-title">Still Needed</h3>
    <p class="bucket-description">
      These parameters could affect the outcome
    </p>
    {#if relevant.length === 0}
      <p class="empty-message">
        {#if analysis?.isDetermined}
          Result is determined - no more parameters needed
        {:else}
          All unassigned parameters are irrelevant
        {/if}
      </p>
    {:else}
      <ul class="param-list">
        {#each relevant as unique}
          <li class="param-item relevant-param">
            <button
              class="param-name"
              onclick={() => onParamClick(unique)}
            >
              {unique}
            </button>
            <div class="quick-assign">
              <button
                class="assign-btn true"
                onclick={() => handleAssign(unique, 'True')}
              >
                T
              </button>
              <button
                class="assign-btn false"
                onclick={() => handleAssign(unique, 'False')}
              >
                F
              </button>
              <button
                class="assign-btn unknown"
                onclick={() => handleAssign(unique, 'Unknown')}
              >
                ?
              </button>
            </div>
          </li>
        {/each}
      </ul>
    {/if}
  </div>

  <!-- Bucket 3: Don't Care (Irrelevant) -->
  <div class="bucket irrelevant">
    <h3 class="bucket-title">Don't Care</h3>
    <p class="bucket-description">
      These parameters can't affect the outcome
    </p>
    {#if irrelevant.length === 0}
      <p class="empty-message">No irrelevant parameters yet</p>
    {:else}
      <ul class="param-list">
        {#each irrelevant as unique}
          <li class="param-item irrelevant-param">
            <button
              class="param-name"
              onclick={() => onParamClick(unique)}
            >
              <s>{unique}</s>
            </button>
          </li>
        {/each}
      </ul>
    {/if}
  </div>
</div>

<style>
  .parameter-buckets {
    display: flex;
    flex-direction: column;
    gap: 1rem;
    padding: 1rem;
    height: 100%;
    overflow-y: auto;
  }

  .bucket {
    padding: 1rem;
    border-radius: 8px;
    border: 2px solid var(--border);
    background: var(--card);
  }

  .bucket.neutral {
    background: var(--muted);
  }

  .bucket.relevant {
    background: #fef3c7; /* yellow-100 */
    border-color: #f59e0b; /* yellow-500 */
  }

  .bucket.irrelevant {
    background: #f3f4f6; /* gray-100 */
    opacity: 0.7;
  }

  .bucket-title {
    font-size: 0.9rem;
    font-weight: 600;
    margin: 0 0 0.5rem 0;
    color: var(--foreground);
  }

  .bucket-description {
    font-size: 0.75rem;
    color: var(--muted-foreground);
    margin: 0 0 0.75rem 0;
  }

  .param-list {
    list-style: none;
    padding: 0;
    margin: 0;
  }

  .param-item {
    display: flex;
    justify-content: space-between;
    align-items: center;
    padding: 0.5rem;
    margin-bottom: 0.5rem;
    border-radius: 4px;
    background: var(--background);
    border: 1px solid var(--border);
  }

  .param-item.relevant-param {
    border-color: #f59e0b;
    background: #fffbeb;
  }

  .param-item.irrelevant-param {
    opacity: 0.6;
  }

  .param-name {
    font-family: 'JetBrains Mono', monospace;
    font-size: 0.85rem;
    background: none;
    border: none;
    padding: 0;
    cursor: pointer;
    color: var(--foreground);
    text-align: left;
  }

  .param-name:hover {
    color: var(--primary);
    text-decoration: underline;
  }

  .quick-assign {
    display: flex;
    gap: 0.25rem;
  }

  .assign-btn {
    width: 1.5rem;
    height: 1.5rem;
    padding: 0;
    font-size: 0.7rem;
    font-weight: 600;
    border-radius: 3px;
    border: 1px solid var(--border);
    background: var(--background);
    cursor: pointer;
    transition: all 150ms;
  }

  .assign-btn:hover {
    transform: scale(1.1);
  }

  .assign-btn.true {
    color: #166534;
    border-color: #86efac;
  }

  .assign-btn.true:hover {
    background: #dcfce7;
  }

  .assign-btn.false {
    color: #991b1b;
    border-color: #fca5a5;
  }

  .assign-btn.false:hover {
    background: #fee2e2;
  }

  .assign-btn.unknown {
    color: var(--muted-foreground);
  }

  .assign-btn.unknown:hover {
    background: var(--muted);
  }

  .empty-message {
    font-size: 0.8rem;
    font-style: italic;
    color: var(--muted-foreground);
    margin: 0.5rem 0;
  }
</style>
```

#### SimplifiedExpressionView.svelte

**File:** `ts-shared/l4-ladder-visualizer/src/lib/displayers/simplified-expression-view.svelte`

```svelte
<script lang="ts">
  import type { Expr } from '$lib/eval/type.js'
  import type { RelevanceAnalysis } from '$lib/eval/relevance.js'
  import type { Assignment } from '$lib/eval/assignment.js'
  import { match } from 'ts-pattern'

  interface Props {
    originalExpr: Expr
    analysis: RelevanceAnalysis | null
    assignment: Assignment
  }

  let { originalExpr, analysis, assignment }: Props = $props()

  /**
   * Pretty-print an expression to readable string
   */
  function prettyPrint(expr: Expr, indent = 0): string {
    const pad = '  '.repeat(indent)

    return match(expr)
      .with({ $type: 'TrueE' }, () => 'True')
      .with({ $type: 'FalseE' }, () => 'False')
      .with({ $type: 'UBoolVar' }, (v) => v.name.unique)
      .with({ $type: 'Not' }, (not) =>
        `¬${prettyPrint(not.negand, indent)}`
      )
      .with({ $type: 'And' }, (and) => {
        if (and.args.length === 2) {
          return `(${prettyPrint(and.args[0], indent)} ∧ ${prettyPrint(and.args[1], indent)})`
        }
        const args = and.args.map(arg => `${pad}  ${prettyPrint(arg, indent + 1)}`).join(' ∧\n')
        return `(\n${args}\n${pad})`
      })
      .with({ $type: 'Or' }, (or) => {
        if (or.args.length === 2) {
          return `(${prettyPrint(or.args[0], indent)} ∨ ${prettyPrint(or.args[1], indent)})`
        }
        const args = or.args.map(arg => `${pad}  ${prettyPrint(arg, indent + 1)}`).join(' ∨\n')
        return `(\n${args}\n${pad})`
      })
      .with({ $type: 'App' }, (app) =>
        `${app.func}(${app.args.map(a => prettyPrint(a, indent)).join(', ')})`
      )
      .exhaustive()
  }

  const assignmentList = $derived(
    [...assignment.entries()].map(([unique, value]) => ({
      unique,
      value: value.toPretty(),
      icon: match(value.$type)
        .with('TrueV', () => '✓')
        .with('FalseV', () => '✗')
        .with('UnknownV', () => '?')
        .exhaustive()
    }))
  )
</script>

<div class="simplified-expr-view">
  <!-- Original Expression -->
  <div class="expr-section">
    <h3>Original Expression</h3>
    <pre class="expr-display">{prettyPrint(originalExpr)}</pre>
  </div>

  <!-- Current Assignments -->
  {#if assignmentList.length > 0}
    <div class="expr-section">
      <h3>Given</h3>
      <ul class="assignment-list">
        {#each assignmentList as { unique, value, icon }}
          <li>
            <span class="var-name">{unique}</span> =
            <span class="var-value">{value}</span>
            <span class="var-icon">{icon}</span>
          </li>
        {/each}
      </ul>
    </div>
  {/if}

  <!-- Simplified Expression or Result -->
  {#if analysis}
    <div class="expr-section">
      <h3>Simplified To</h3>

      {#if analysis.isDetermined}
        <!-- Fully determined -->
        <div class="result-display {analysis.overallResult.$type === 'TrueV' ? 'true' : 'false'}">
          <span class="result-icon">
            {analysis.overallResult.$type === 'TrueV' ? '✓' : '✗'}
          </span>
          <span class="result-text">
            {analysis.overallResult.toPretty()}
          </span>
        </div>
      {:else if analysis.simplifiedExpr}
        <!-- Partially simplified -->
        <pre class="expr-display simplified">{prettyPrint(analysis.simplifiedExpr)}</pre>

        <p class="explanation">
          Evaluates to: <strong>{analysis.overallResult.toPretty()}</strong>
        </p>
      {:else}
        <p class="explanation">
          Expression evaluates to: <strong>{analysis.overallResult.toPretty()}</strong>
        </p>
      {/if}
    </div>

    <!-- Short-circuited branches -->
    {#if analysis.shortCircuitedNodes.size > 0}
      <details class="eliminated-branches">
        <summary>
          Eliminated branches ({analysis.shortCircuitedNodes.size} nodes short-circuited)
        </summary>
        <p class="explanation">
          These subexpressions were eliminated by short-circuit evaluation
        </p>
      </details>
    {/if}
  {/if}
</div>

<style>
  .simplified-expr-view {
    padding: 1rem;
    font-size: 0.9rem;
    overflow-y: auto;
    height: 100%;
  }

  .expr-section {
    margin-bottom: 1.5rem;
    padding: 1rem;
    background: var(--card);
    border-radius: 8px;
    border: 1px solid var(--border);
  }

  .expr-section h3 {
    font-size: 0.9rem;
    font-weight: 600;
    margin: 0 0 0.75rem 0;
    color: var(--foreground);
  }

  .expr-display {
    font-family: 'JetBrains Mono', 'Fira Code', monospace;
    font-size: 0.85rem;
    line-height: 1.6;
    padding: 0.75rem;
    background: var(--muted);
    border-radius: 4px;
    overflow-x: auto;
    margin: 0;
    white-space: pre-wrap;
  }

  .expr-display.simplified {
    background: #fef3c7; /* yellow-100 */
    border-left: 4px solid #f59e0b; /* yellow-500 */
  }

  .assignment-list {
    list-style: none;
    padding: 0;
    margin: 0;
    font-family: 'JetBrains Mono', monospace;
    font-size: 0.85rem;
  }

  .assignment-list li {
    padding: 0.5rem;
    margin-bottom: 0.25rem;
    background: var(--muted);
    border-radius: 4px;
  }

  .var-name {
    color: var(--primary);
    font-weight: 600;
  }

  .var-value {
    color: var(--foreground);
  }

  .var-icon {
    margin-left: 0.5rem;
    font-size: 1rem;
  }

  .result-display {
    display: flex;
    align-items: center;
    gap: 1rem;
    padding: 1rem;
    border-radius: 8px;
    font-size: 1.1rem;
    font-weight: 600;
  }

  .result-display.true {
    background: #dcfce7; /* green-100 */
    color: #166534; /* green-800 */
  }

  .result-display.false {
    background: #fee2e2; /* red-100 */
    color: #991b1b; /* red-800 */
  }

  .result-icon {
    font-size: 2rem;
  }

  .explanation {
    font-size: 0.85rem;
    color: var(--muted-foreground);
    margin: 0.5rem 0 0 0;
  }

  .eliminated-branches {
    margin-top: 1rem;
    padding: 0.75rem;
    background: var(--muted);
    border-radius: 4px;
    font-size: 0.85rem;
  }

  .eliminated-branches summary {
    cursor: pointer;
    font-weight: 600;
    color: var(--muted-foreground);
  }

  .eliminated-branches summary:hover {
    color: var(--foreground);
  }
</style>
```

### 5. Main Layout Integration

**File:** `ts-apps/jl4-web/src/routes/+page.svelte` (or wherever the visualizer is used)

```svelte
<script lang="ts">
  import Flow from '@repo/l4-ladder-visualizer/displayers/flow/flow.svelte'
  import ParameterBuckets from '@repo/l4-ladder-visualizer/displayers/parameter-buckets.svelte'
  import SimplifiedExpressionView from '@repo/l4-ladder-visualizer/displayers/simplified-expression-view.svelte'

  // ... existing imports and setup ...

  let showParameterBuckets = $state(true)
  let showSimplifiedExpr = $state(true)

  // Get relevance analysis from ladder graph
  const relevanceAnalysis = $derived(
    ladderGraph?.getRelevanceAnalysis() ?? null
  )

  function handleParamClick(unique: string) {
    // Focus the parameter in the graph
    // Scroll to node, highlight it
    console.log('Focus param:', unique)
  }

  function handleParamAssign(unique: string, value: 'True' | 'False' | 'Unknown') {
    const uboolValue = match(value)
      .with('True', () => new TrueVal())
      .with('False', () => new FalseVal())
      .with('Unknown', () => new UnknownVal())
      .exhaustive()

    ladderGraph?.submitNewBinding(context, {
      unique,
      value: uboolValue
    })
  }
</script>

<div class="app-layout">
  <!-- Top bar -->
  <div class="top-bar">
    <ExampleSelector />
    <div class="controls">
      <button onclick={() => showParameterBuckets = !showParameterBuckets}>
        {showParameterBuckets ? 'Hide' : 'Show'} Parameters
      </button>
      <button onclick={() => showSimplifiedExpr = !showSimplifiedExpr}>
        {showSimplifiedExpr ? 'Hide' : 'Show'} Simplified
      </button>
    </div>
  </div>

  <!-- Main content -->
  <div class="main-content">
    <!-- Left sidebar: Parameter Buckets -->
    {#if showParameterBuckets}
      <div class="sidebar left">
        <ParameterBuckets
          analysis={relevanceAnalysis}
          assignment={ladderGraph?.assignment ?? new Map()}
          onParamClick={handleParamClick}
          onParamAssign={handleParamAssign}
        />
      </div>
    {/if}

    <!-- Center: Graph Visualizer -->
    <div class="center-panel">
      <Flow {context} {node} {env} />
    </div>

    <!-- Right sidebar: Simplified Expression -->
    {#if showSimplifiedExpr}
      <div class="sidebar right">
        <SimplifiedExpressionView
          originalExpr={originalExpression}
          analysis={relevanceAnalysis}
          assignment={ladderGraph?.assignment ?? new Map()}
        />
      </div>
    {/if}
  </div>
</div>

<style>
  .app-layout {
    display: flex;
    flex-direction: column;
    height: 100vh;
  }

  .top-bar {
    display: flex;
    justify-content: space-between;
    align-items: center;
    padding: 1rem;
    border-bottom: 1px solid var(--border);
    background: var(--background);
  }

  .main-content {
    display: flex;
    flex: 1;
    overflow: hidden;
  }

  .sidebar {
    width: 300px;
    border-right: 1px solid var(--border);
    background: var(--background);
    overflow-y: auto;
  }

  .sidebar.left {
    border-right: 1px solid var(--border);
  }

  .sidebar.right {
    border-left: 1px solid var(--border);
    border-right: none;
  }

  .center-panel {
    flex: 1;
    overflow: hidden;
  }
</style>
```

## Implementation Roadmap

### Phase 1: Core Relevance Analysis (Week 1)

**Goal:** Get basic don't-care detection working

- [ ] Implement `RelevanceAnalyzer` class with cofactor operation
- [ ] Add `isDontCare` method (test if `f|_{x=T} = f|_{x=F}`)
- [ ] Add expression simplification (constant folding)
- [ ] Write unit tests for cofactor and simplification
- [ ] Integration test with alcohol example

**Deliverable:** Console output showing which variables are irrelevant

### Phase 2: Evaluator Enhancement (Week 1-2)

**Goal:** Track short-circuits and consulted variables

- [ ] Extend `EvalResult` with `consultedVars` and `shortCircuited`
- [ ] Modify `evalAndChain` to track short-circuits
- [ ] Modify `evalOrChain` to track short-circuits
- [ ] Update all eval cases to propagate tracking info
- [ ] Write tests for short-circuit detection

**Deliverable:** Evaluator returns which nodes were short-circuited

### Phase 3: Visual Layer (Week 2)

**Goal:** Show relevance in the graph

- [ ] Add relevance CSS classes to `node-styles.ts`
- [ ] Modify `ubool-var.svelte` to use relevance classes
- [ ] Modify `ladder-edge.svelte` to gray out irrelevant edges
- [ ] Integrate relevance analysis into `NNFLadderGraphLirNode`
- [ ] Test visual appearance with alcohol example

**Deliverable:** Graph grays out irrelevant nodes/edges as user assigns values

### Phase 4: Parameter Buckets UI (Week 3)

**Goal:** Interactive three-bucket interface

- [ ] Implement `ParameterBuckets.svelte` component
- [ ] Add quick-assign buttons (T/F/?)
- [ ] Wire up parameter focus/highlight in graph
- [ ] Add drag-and-drop (stretch)
- [ ] Responsive styling

**Deliverable:** Sidebar showing Not Asked / Still Needed / Don't Care buckets

### Phase 5: Simplified Expression View (Week 3-4)

**Goal:** Show how expression simplifies

- [ ] Implement `SimplifiedExpressionView.svelte`
- [ ] Add pretty-print function for expressions
- [ ] Show original vs simplified expressions
- [ ] Display eliminated branches
- [ ] Add result display

**Deliverable:** Panel showing cofactored expressions and final result

### Phase 6: Polish & Integration (Week 4)

**Goal:** Complete end-to-end experience

- [ ] Integrate all components into main layout
- [ ] Add toggle buttons for show/hide panels
- [ ] Implement parameter focus/scroll in graph
- [ ] Add keyboard shortcuts
- [ ] Performance optimization (memoization)
- [ ] Documentation and examples

**Deliverable:** Full interactive partial evaluation visualizer

## Testing Strategy

### Unit Tests

1. **Cofactor operation** (`relevance.test.ts`)

   ```typescript
   test("cofactor substitutes variable with constant", () => {
     const expr = And([Var("x"), Var("y")]);
     const cofactored = cofactor(expr, "x", true);
     expect(cofactored).toEqual(Var("y"));
   });
   ```

2. **Simplification** (`relevance.test.ts`)

   ```typescript
   test("simplify: True AND x = x", () => {
     const expr = And([TrueE(), Var("x")]);
     const simplified = simplify(expr);
     expect(simplified).toEqual(Var("x"));
   });
   ```

3. **Don't-care detection** (`relevance.test.ts`)

   ```typescript
   test('detects don't-care variable', async () => {
     // (x AND y) OR (NOT x AND y) → y is relevant, x is don't-care
     const expr = Or([
       And([Var('x'), Var('y')]),
       And([Not(Var('x')), Var('y')])
     ])
     const assignment = new Map([['y', new TrueVal()]])

     const isDC = await analyzer.isDontCare(expr, 'x', assignment, ...)
     expect(isDC).toBe(true) // x is don't-care when y=True
   })
   ```

### Integration Tests

1. **Alcohol example scenarios** (`alcohol.integration.test.ts`)

   - No inputs → all parameters relevant
   - age=30 → parental/emancipated irrelevant
   - age=30, married=False → all other params irrelevant, result=True

2. **Visual regression tests**
   - Screenshot comparison of grayed-out nodes
   - Check CSS classes applied correctly

### End-to-End Tests

1. **User interaction flow** (`e2e.spec.ts`)

   ```typescript
   test("parameter buckets update as values assigned", async () => {
     await page.click('[data-testid="age-param"]');
     await page.click('[data-testid="assign-true"]');

     // Check that parental_approval moved to "Don't Care" bucket
     const dontCare = await page.locator(".bucket.irrelevant .param-item");
     await expect(dontCare).toContainText("parental_approval");
   });
   ```

## Future Enhancements

### Phase 7: Backend Integration with Grisette

**Consideration: Grisette Library for Symbolic Evaluation**

[Grisette](https://hackage.haskell.org/package/grisette) is a Haskell library for symbolic evaluation that could significantly enhance our backend implementation:

**What Grisette Provides:**

- Symbolic evaluation with union types (handling multiple execution paths)
- SMT solver integration (Z3, Yices, Boolector, etc.)
- Symbolic reasoning about program behavior
- Merge strategies for handling branching
- Built-in support for symbolic booleans and constraints

**Potential Benefits:**

1. **More Rigorous Symbolic Evaluation**

   - Instead of manual cofactoring in TypeScript, use Grisette's symbolic evaluation
   - Let SMT solver determine satisfiability and relevance
   - Handle complex expressions more reliably

2. **Better Don't-Care Detection**

   ```haskell
   import Grisette

   -- Check if variable x is don't-care
   isDontCare :: SymBool -> String -> IO Bool
   isDontCare expr varName = do
     -- Evaluate with x=True
     let exprTrue = substitute expr varName (con True)
     -- Evaluate with x=False
     let exprFalse = substitute expr varName (con False)

     -- Check if results are equivalent using SMT solver
     result <- solve (exprTrue ./= exprFalse)
     return $ case result of
       Unsatisfiable -> True  -- Always equal → don't care
       _             -> False -- Could differ → relevant
   ```

3. **Automatic Simplification**

   - Grisette's merge strategies automatically simplify symbolic expressions
   - No need to manually implement constant folding

4. **Handling Quantifiers and Constraints**
   - If we extend L4 to support more complex logic (exists, forall)
   - Grisette can handle these naturally

**Integration Approach:**

```haskell
-- Backend endpoint: POST /partial-evaluation
partialEvaluation :: ExprResolved -> Assignment -> IO RelevanceAnalysis
partialEvaluation expr assignment = do
  -- Convert L4 expression to Grisette symbolic expression
  let symExpr = toGrisetteExpr expr

  -- Apply known assignments
  let simplified = applyAssignments symExpr assignment

  -- For each unassigned variable, check if it's don't-care
  unassigned <- getUnassignedVars expr assignment
  relevance <- forM unassigned $ \var -> do
    isDC <- checkDontCare simplified var
    return (var, if isDC then Irrelevant else Relevant)

  return RelevanceAnalysis {
    variableRelevance = Map.fromList relevance,
    simplifiedExpr = fromGrisetteExpr simplified,
    ...
  }

checkDontCare :: SymBool -> Var -> IO Bool
checkDontCare expr var = do
  -- Cofactor with var=True and var=False
  let withTrue = substitute expr var (con True)
  let withFalse = substitute expr var (con False)

  -- Ask SMT solver: can these ever differ?
  result <- solve (withTrue ./= withFalse)

  return $ case result of
    Unsatisfiable -> True   -- They're always equal → don't care
    Satisfiable _ -> False  -- They can differ → relevant
    Unknown       -> False  -- Conservative: assume relevant
```

**Trade-offs:**

| Aspect      | Manual Implementation (TypeScript)    | Grisette (Haskell Backend)       |
| ----------- | ------------------------------------- | -------------------------------- |
| Complexity  | Higher (implement cofactor, simplify) | Lower (library handles it)       |
| Performance | Good for simple cases                 | Excellent (SMT solver optimized) |
| Correctness | Risk of bugs in manual logic          | More reliable (proven solver)    |
| Latency     | Very low (client-side)                | Network round-trip to backend    |
| Scalability | Limited by browser                    | Handles complex expressions      |

**Recommendation:**

Start with **hybrid approach**:

1. **Phase 1-6:** Implement client-side cofactoring for MVP (fast, no latency)
2. **Phase 7:** Add Grisette backend as optional enhancement
3. Use client-side for simple expressions, backend for complex ones
4. Compare results to validate client-side implementation

**Backend Tasks:**

- [ ] Add Grisette dependency to `jl4.cabal`
- [ ] Implement L4 → Grisette expression conversion
- [ ] Create `/partial-evaluation` endpoint using Grisette
- [ ] Add SMT solver integration (Z3)
- [ ] Return prioritized list of parameters to ask
- [ ] Impact analysis: what happens if param=True vs param=False
- [ ] Benchmark Grisette vs manual implementation
- [ ] Add caching layer for repeated queries

### Phase 8: Advanced Features

- [ ] **Sensitivity analysis** - which parameters have biggest impact?
- [ ] **Explanation generation** - natural language why a param matters
- [ ] **Interactive scenarios** - save/load different assignment scenarios
- [ ] **Comparison view** - compare two different assignments side-by-side
- [ ] **Undo/redo** - step backward through assignments
- [ ] **Animation** - show nodes graying out smoothly as analysis runs

### Phase 9: Performance Optimization

- [ ] Memoize cofactor/simplify operations
- [ ] Incremental relevance analysis (don't recompute entire tree)
- [ ] Web Worker for heavy computation
- [ ] Virtual scrolling for large parameter lists

## Metrics & Success Criteria

**Must Have:**

- ✅ Relevance analysis completes in <500ms for typical rules
- ✅ Visual updates in <100ms after assignment
- ✅ Correctly identifies irrelevant params in alcohol example
- ✅ No false positives (marking relevant params as irrelevant)

**Should Have:**

- ✅ Works with expressions up to 20 parameters
- ✅ Handles nested boolean expressions 5+ levels deep
- ✅ Mobile-responsive layout

**Nice to Have:**

- ✅ Animations smooth (60fps)
- ✅ Keyboard navigation support
- ✅ Export/share feature

## References

- **Parent Spec:** [BOOLEAN-MINIMIZATION-SPEC.md](./BOOLEAN-MINIMIZATION-SPEC.md) - Theory and backend API
- **Issue:** #638 - Boolean minimization for query relevance
- **Issue:** #635 - Critical L4 Decision Service improvements
- **Existing Code:**
  - `ts-shared/l4-ladder-visualizer/src/lib/eval/` - Three-valued evaluator
  - `ts-shared/l4-ladder-visualizer/src/lib/displayers/flow/` - Svelte Flow visualizer
  - `ts-shared/l4-ladder-visualizer/src/lib/layout-ir/ladder-graph/` - LIR graph representation

## Appendix: Example Walkthrough

### Alcohol Purchase - Progressive Partial Evaluation

**Initial State:**

```
Expression:
(age≥21 ∧ (¬married ∨ spousal ∨ beer)) ∨ (age<21 ∧ (parental ∨ emancipated))

Assignment: {}

Relevance Analysis:
  All variables: unknown (not analyzed yet)

Buckets:
  Not Asked Yet: [age, married, spousal, beer, parental, emancipated]
  Still Needed: []
  Don't Care: []

Graph: All nodes normal opacity
```

**Step 1: User sets age = 30**

```
Assignment: {age: True} (where age≥21)

Relevance Analysis:
  - Cofactor with adult=True: ¬married ∨ spousal ∨ beer
  - Cofactor with adult=False: parental ∨ emancipated
  - Adult branch is active, minor branch eliminated

  Variable Relevance:
    age: determined
    married: relevant (could make first branch True if False)
    spousal: relevant (could make first branch True if True)
    beer: relevant (could make first branch True if True)
    parental: irrelevant (minor branch eliminated)
    emancipated: irrelevant (minor branch eliminated)

Buckets:
  Not Asked Yet: [married, spousal, beer]
  Still Needed: [] (all not-asked are implicitly needed)
  Don't Care: [parental, emancipated] ← MOVED HERE

Graph:
  - Minor branch (parental ∨ emancipated) grayed out
  - Adult branch normal/highlighted
  - parental, emancipated nodes grayed

Simplified Expression:
  ¬married ∨ spousal ∨ beer
  Evaluates to: UNKNOWN
```

**Step 2: User sets married = False**

```
Assignment: {age: True, married: False}

Relevance Analysis:
  - Cofactor: ¬False ∨ spousal ∨ beer = True ∨ spousal ∨ beer = True
  - Expression fully determined!

  Variable Relevance:
    age: determined
    married: determined
    spousal: irrelevant (result already True)
    beer: irrelevant (result already True)
    parental: irrelevant
    emancipated: irrelevant

Buckets:
  Not Asked Yet: []
  Still Needed: []
  Don't Care: [spousal, beer, parental, emancipated] ← ALL MOVED HERE

Graph:
  - All nodes grayed except age and married
  - Result node shows ✓ (True)

Simplified Expression:
  True

Result:
  ✓ You may purchase alcohol
  (because you're over 21 and unmarried)
```

---

**End of Specification**
