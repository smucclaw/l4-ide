# Specification: Consumer-Facing Web App Generator

**Status:** Draft
**Branch:** `mengwong/gen-web-app`
**Related:** `SYMBEVAL-QUERY-PLANNING-STATUS.md`, `PARTIAL-EVAL-VISUALIZER-SPEC.md`
**Predecessor:** `smucclaw/vue-pure-pdpa` (Dolores / Orwell)

## Summary

Generate consumer-facing web applications from L4 decision logic. End-users (citizens, customers, SME founders) can answer questions about their situation and receive:

- A determination (yes/no, amount, category)
- An explanation of why (audit-grade trace)
- Optionally, generated documents

This is distinct from the developer-facing ladder visualizer. The ladder is for authoring and debugging; the web app is for end-users answering questions.

## Design Principles

### 1. All Parameters Visible (Not a Forced March)

**Critical design decision from vue-pure-pdpa:**

Traditional wizards force users through questions one at a time in a predetermined order. This is suboptimal because:

- Users may already know several answers and want to enter them immediately
- Users may want to skip around based on what information they have at hand
- Users may want to see the "shape" of what's being asked before committing to answer
- A linear wizard feels bureaucratic and opaque

**Our approach:** Expose all input parameters on a single page. Users can click on any parameter to provide an answer, in any order they choose.

The query planner's ordering is used for:
- Visual emphasis (highlighting "recommended next" questions)
- Keyboard navigation defaults
- Accessibility announcements
- But NOT for restricting what users can answer

### 2. Progressive Disclosure of Relevance with Real-Time Visual Feedback

As users provide answers, the UI provides **immediate, animated visual feedback** showing how their answers affect other questions:

**Short-Circuit Visualization:**
- When a user answers a question that short-circuits a logical condition (e.g., satisfying one branch of an OR), all other alternatives **immediately fade to gray**
- The transition is smooth and animated (not instant) to help users understand the cause-and-effect relationship
- Questions are **not hidden**, just visually de-emphasized - users can still see what became irrelevant and why
- This makes the logical reasoning transparent: "I answered Yes to X, so Y and Z are no longer needed"

**Example (Disjunction):**
```
Did a data breach occur?
  - unauthorised access [Yes ✓]
  - unauthorised use     [grayed out - no longer needed]
  - unauthorised disclosure  [grayed out - no longer needed]
  - unauthorised copying     [grayed out - no longer needed]
```

**Dynamic Relevance:**
- Parameters that become irrelevant transition smoothly to grayed-out state
- Parameters that become more relevant are emphasized (highlighted, moved to top)
- Parameters maintain their spatial position during transitions (spatial stability)
- The overall result updates live as soon as it becomes determined

**Visual Principle:** Make the decision logic **visible** through UI state changes, not just functional.

### 3. Answer at Any Level of Granularity (Parent Node Assertions)

Users should be able to assert truth values at **intermediate nodes**, not just leaf parameters.

**Example:** Given a rule like:

```
DECIDE `eligible` IF
    `is adult`
    AND (`has income` OR `has assets`)
```

A traditional wizard would force the user to answer:
1. "Are you an adult?" → Yes
2. "Do you have income?" → ???
3. "Do you have assets?" → ???

But the user might *know* they satisfy the income-or-assets requirement without knowing (or wanting to disclose) which one. They should be able to click on the `(has income OR has assets)` node and assert "Yes, this is true" directly.

**Benefits:**
- Respects user privacy (no need to disclose *which* sub-condition is satisfied)
- Matches how people actually reason ("I know I qualify for this part")
- Reduces cognitive load for complex nested conditions
- Enables "I don't know the details, but I know the answer" scenarios

**UI Implication:** The parameter grid should show not just leaf inputs but also composite conditions that users can directly answer. These could be rendered as:
- Collapsible groups with a "I know this is true/false" toggle at the group level
- Inline assertion buttons next to OR/AND groups
- A "shortcut" mode that shows only high-level conditions

**Backend Implication:** The query-plan API already tracks atoms at various levels. The web app needs to map user assertions on parent nodes to the appropriate bindings, and the evaluator must respect these higher-level assertions.

### 4. Explanation First

Every determination comes with an explanation:
- "You qualify because X and Y"
- "You do not qualify because Z was false"
- With citations back to the source rules

This is the "explainable AI" promise: the formal evaluation trace rendered in natural language.

### 5. Schema-Driven Input Controls

The decision-service provides JSON Schema for each parameter. The web app renders appropriate input controls:

| Schema Type | Control |
|-------------|---------|
| `boolean` | Toggle / Yes-No buttons |
| `string` with `enum` | Radio buttons or select |
| `number` | Number input with validation |
| `string` (date format) | Date picker |
| `object` | Expandable fieldset |
| `array` | Repeatable section |

### 6. Embeddable and Standalone

The web app should work as:
- A standalone page (deployable to static hosting)
- An embeddable component (iframe or web component)
- A route within jl4-web for quick prototyping

## Data Model

### Parameter State Model

Each parameter in the wizard maintains the following state:

```typescript
type ParameterState = {
  key: string                    // Parameter identifier
  label: string                  // Display label (from schema.alias or key)
  schema: Parameter              // JSON schema for the parameter
  value: unknown                 // Current user answer
  status: ParameterStatus        // Current relevance status
  rank: number                   // Priority ranking from query-plan
  asks: QueryAsk[]              // Related query atoms from query-plan
  error?: string                 // Validation error message
}

type ParameterStatus =
  | 'unanswered-next'      // Should be answered next (highest priority)
  | 'unanswered-relevant'  // Still needed but not next
  | 'answered'             // User has provided an answer
  | 'irrelevant'           // No longer needed based on other answers
```

### Component Structure

```
Wizard.svelte                    # Main orchestrator component
├── OutcomeBanner.svelte        # Shows determination status/result
└── ParameterGrid.svelte        # Organizes parameters into active/irrelevant groups
    └── ParameterCard.svelte    # Individual parameter input with visual state transitions
        ├── BooleanInput.svelte      # Yes/No toggle buttons
        ├── TextInput.svelte         # Text field
        ├── NumberInput.svelte       # Number field
        └── EnumInput.svelte         # Radio buttons/select for enums
```

### Visual Design Principles

1. **Transparency**: Make logical reasoning visible through UI state changes
2. **Immediate Feedback**: Update UI instantly when answers change relevance
3. **Spatial Stability**: Keep questions in consistent positions during transitions
4. **Clear Hierarchy**: Use visual weight to indicate priority (next > relevant > irrelevant)
5. **Graceful Degradation**: Irrelevant questions fade but remain visible and accessible

## Architecture

```
┌─────────────────────────────────────────────────────────┐
│                    L4 Source File                       │
│                   (e.g., pdpa.l4)                       │
└─────────────────────┬───────────────────────────────────┘
                      │ @export
                      ▼
┌─────────────────────────────────────────────────────────┐
│                  Decision Service                        │
│  ┌─────────────┐  ┌──────────────┐  ┌────────────────┐  │
│  │ /functions  │  │ /evaluation  │  │ /query-plan    │  │
│  │ (schema)    │  │ (compute)    │  │ (what to ask)  │  │
│  └─────────────┘  └──────────────┘  └────────────────┘  │
└─────────────────────┬───────────────────────────────────┘
                      │ REST API
                      ▼
┌─────────────────────────────────────────────────────────┐
│                 Web App (Browser)                        │
│  ┌─────────────┐  ┌──────────────┐  ┌────────────────┐  │
│  │ Parameter   │  │ Result       │  │ Explanation    │  │
│  │ Grid        │  │ Banner       │  │ Panel          │  │
│  └─────────────┘  └──────────────┘  └────────────────┘  │
└─────────────────────────────────────────────────────────┘
```

## User Interface

### Layout: Single Page, Three Zones

```
┌─────────────────────────────────────────────────────────┐
│  [Logo]  Function Name / Title           [Reset] [Help] │
├─────────────────────────────────────────────────────────┤
│                                                         │
│  ┌─────────────────────────────────────────────────┐   │
│  │              RESULT BANNER                       │   │
│  │  (appears when determination is made)            │   │
│  │  "You qualify for the exemption"                 │   │
│  │  [Show explanation ▼]                            │   │
│  └─────────────────────────────────────────────────┘   │
│                                                         │
│  ┌─────────────────────────────────────────────────┐   │
│  │           PARAMETER GRID                         │   │
│  │                                                  │   │
│  │  ┌──────────────┐  ┌──────────────┐             │   │
│  │  │ Age          │  │ Resident?    │  ← emphasized│   │
│  │  │ [    30    ] │  │ [Yes] [No]   │             │   │
│  │  └──────────────┘  └──────────────┘             │   │
│  │                                                  │   │
│  │  ┌──────────────┐  ┌──────────────┐             │   │
│  │  │ Income       │  │ Has spouse?  │             │   │
│  │  │ [  50000   ] │  │ [Yes] [No]   │             │   │
│  │  └──────────────┘  └──────────────┘             │   │
│  │                                                  │   │
│  │  ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─  │   │
│  │  (grayed out: don't-care parameters)             │   │
│  │  ┌──────────────┐                                │   │
│  │  │ Parental OK  │  ← irrelevant given age=30    │   │
│  │  │ [   ?    ]   │                                │   │
│  │  └──────────────┘                                │   │
│  └─────────────────────────────────────────────────┘   │
│                                                         │
│  ┌─────────────────────────────────────────────────┐   │
│  │           EXPLANATION PANEL (expandable)         │   │
│  │                                                  │   │
│  │  The result was determined because:              │   │
│  │  • Age (30) >= 21 ✓                              │   │
│  │  • Therefore, parental approval not required     │   │
│  │  • Resident = Yes ✓                              │   │
│  │                                                  │   │
│  │  Source: PDPA Section 4(1)(a)                    │   │
│  └─────────────────────────────────────────────────┘   │
│                                                         │
└─────────────────────────────────────────────────────────┘
```

### Parameter Card States

Each parameter is rendered as a card with visual states. **All state transitions are animated smoothly** to provide visual feedback about logical relationships:

| State | Appearance | Meaning | Transition |
|-------|------------|---------|------------|
| `unanswered-next` | Emphasized border, highlighted | Query planner's top recommendation | Fade in emphasis when prioritized |
| `unanswered-relevant` | Normal, clean appearance | Should answer this | Standard state |
| `answered` | Filled, checkmark, accent color | User has provided value | Smooth fill animation |
| `irrelevant` | Grayed out (50% opacity), subtle | No longer needed based on other answers | **300ms fade to gray** - most important transition |
| `error` | Red border, shake animation | Validation failed | Shake effect on error |

**Key Transition: Relevant → Irrelevant**
- Duration: 300ms ease-out
- Effect: Opacity fades from 100% to 50%, subtle scale down
- Purpose: Make short-circuit logic visible ("your answer made this irrelevant")
- Spatial: Card stays in place, doesn't jump or reflow

### Interaction Flow

1. **Initial load**: All parameters visible, query-plan's top recommendations highlighted
2. **User clicks parameter**: Input control becomes active
3. **User provides value**:
   - Card shows "answered" state
   - API call to `/evaluation` with current bindings
   - API call to `/query-plan` to update relevance
   - UI updates: result banner, parameter relevance, explanation
4. **Result determined**:
   - Result banner appears prominently
   - Remaining unanswered parameters move to "irrelevant" section
   - Explanation panel auto-expands

### Keyboard Navigation

- `Tab` / `Shift+Tab`: Move between parameters (in query-plan order by default)
- `Enter` / `Space`: Activate current parameter
- `Escape`: Deactivate current parameter
- `?`: Show help overlay

## API Integration

### Initialization

```typescript
// 1. Get function metadata and schema
const fn = await fetch(`${baseUrl}/functions/${fnName}`)
const { parameters, description } = await fn.json()

// 2. Get initial query-plan (no bindings)
const plan = await fetch(`${baseUrl}/functions/${fnName}/query-plan`, {
  method: 'POST',
  body: JSON.stringify({ bindings: {} })
})
const { asks, atoms, stillNeeded, dontCare } = await plan.json()
```

### On Value Change

```typescript
async function onValueChange(paramPath: string, value: unknown) {
  bindings[paramPath] = value

  // Parallel: evaluate and get new query-plan
  const [evalResult, newPlan] = await Promise.all([
    fetch(`${baseUrl}/functions/${fnName}/evaluation`, {
      method: 'POST',
      body: JSON.stringify({ arguments: bindings, trace: 'full' })
    }),
    fetch(`${baseUrl}/functions/${fnName}/query-plan`, {
      method: 'POST',
      body: JSON.stringify({ bindings })
    })
  ])

  updateUI(await evalResult.json(), await newPlan.json())
}
```

## Deployment Modes

### Mode 1: Hosted (jl4.legalese.com/app/*)

- L4 source uploaded via jl4-web or websessions
- Web app served at predictable URL
- Good for demos and quick sharing

### Mode 2: Embedded (iframe)

```html
<iframe
  src="https://jl4.legalese.com/app/embed/my-function"
  width="100%"
  height="600"
></iframe>
```

### Mode 3: Static Export

Generate a self-contained HTML/JS bundle that:
- Includes the compiled decision logic (via decision-service snapshot)
- Can be deployed to any static hosting (GitHub Pages, S3, etc.)
- Works offline after initial load

### Mode 4: Web Component

```html
<script src="https://jl4.legalese.com/l4-wizard.js"></script>
<l4-wizard
  function="my-function"
  service-url="https://api.example.com">
</l4-wizard>
```

## Implementation Plan

### Phase 1: Core Component ✅ COMPLETED

1. ✅ Create `ts-apps/l4-wizard/` with Svelte + Vite
2. ✅ Implement `ParameterGrid.svelte` with card-based layout
3. ✅ Implement schema-to-control mapping (boolean, text, number, enum)
4. ✅ Wire up decision-service client
5. ✅ Add query-plan integration for relevance highlighting
6. ✅ Fixed infinite loop bug in Svelte 5 reactivity
7. ✅ Fixed API response structure mismatch (`data.function` not `data.declaration`)
8. ✅ Fixed backtick normalization for L4 identifiers

### Phase 2: Visual Feedback ✅ COMPLETED

1. ✅ Add smooth CSS transitions (300ms ease-out) to ParameterCard
2. ✅ Enhance visual styling for irrelevant state (opacity 50%, scale 98%)
3. ✅ Add "(no longer needed)" indicator for irrelevant parameters
4. ✅ Text color transitions for fading effect
5. ✅ Maintain spatial stability during state changes

### Phase 3: Explanation & Trace

1. Render evaluation trace as human-readable explanation
2. Add collapsible explanation panel
3. Support `@desc` annotations for parameter labels
4. Add source citations (rule name, line number)

### Phase 4: Deployment

1. Add route to jl4-web for embedded mode
2. Create standalone build configuration
3. Document embedding and static export options

### Phase 5: Polish

1. Accessibility audit (WCAG 2.1 AA)
2. Mobile responsive design
3. Internationalization hooks
4. Theming / white-label support

## Comparison: vue-pure-pdpa vs. l4-wizard

| Aspect | vue-pure-pdpa (old) | l4-wizard (new) |
|--------|---------------------|-----------------|
| Input format | PureScript (.purs) | L4 source directly |
| Backend | Flask + natural4 | Decision service API |
| Query planning | Ad-hoc | Formal (ROBDD-based) |
| Explanation | Limited | Full trace with citations |
| Deployment | Custom server pool (v8k) | Static or embedded |
| Framework | Vue 2 + PureScript | Svelte + TypeScript |

## Open Questions

1. **Nested objects**: How should we render complex record types? Accordion? Inline fieldset?

2. **Arrays/lists**: How should repeatable items work? Add/remove buttons? Minimum/maximum counts?

3. **Conditional visibility**: Should we hide irrelevant parameters entirely, or just de-emphasize them? (Current spec: de-emphasize, don't hide)

4. **Partial saves**: Should we support saving progress and resuming later? (Implies persistence layer)

5. **Multi-function apps**: Should one web app support multiple related decision functions? (e.g., "Check eligibility" then "Calculate amount")

6. **Parent node assertion UI**: How exactly should we render intermediate/composite nodes for user assertion?
   - Option A: Show the decision tree structure with toggles at each level
   - Option B: Flat list with indentation showing hierarchy
   - Option C: "Expert mode" toggle that reveals intermediate nodes
   - Need to balance power-user flexibility with simplicity for casual users

7. **Parent node assertion semantics**: When a user asserts a parent node, should we:
   - Lock the child nodes as "answered by parent"?
   - Allow overriding with specific child values later?
   - Show a warning if child values would contradict the parent assertion?

## Success Criteria

1. A user with no technical background can answer questions and understand the result
2. The explanation is clear enough that users can self-diagnose why they got a particular result
3. The query-plan ordering feels helpful, not restrictive
4. The app works on mobile devices
5. Load time under 2 seconds for typical decision functions

## References

- `smucclaw/vue-pure-pdpa` — predecessor implementation
- `SYMBEVAL-QUERY-PLANNING-STATUS.md` — query-plan infrastructure
- `PARTIAL-EVAL-VISUALIZER-SPEC.md` — relevance analysis algorithms
- PDPA pilot — government regulatory compliance web wizard
