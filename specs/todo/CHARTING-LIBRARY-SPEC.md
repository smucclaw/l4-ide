# L4 Results Visualizer

## Summary

Overhaul the IDE visualization experience with two changes:

1. **Rename & refactor the existing ladder visualizer**: "Visualize" → "Show decision graph". Move "Simplify and visualize" inside the ladder panel as a toggle instead of a separate CodeLens.

2. **New Results Panel**: A scrollable, composable results viewer. Any `#EVAL`, `#TRACE`, `#EVALTRACE`, `#ASSERT`, `#CHECK` directive gets a "Track result" CodeLens. Clicking it adds that result as a section to the Results Panel. Sections are ordered to match the L4 source, each individually dismissible. Results are rendered with type-aware formatting: charts for `LIST OF PAIR`, formatted dates/times, syntax-highlighted L4 for complex types.

**No L4 library needed. No Haskell backend changes.**

---

## Part 1: Ladder Visualizer Cleanup

### Rename CodeLens

| Current                  | New                     |
| ------------------------ | ----------------------- |
| "Visualize"              | "Show decision graph"   |
| "Simplify and visualize" | _(removed as CodeLens)_ |

### Move simplification into the panel

The ladder panel gets a toolbar toggle: **"Simplified"** (on/off). When toggled, re-requests visualization with `simplify=true`. This replaces the separate CodeLens.

#### Files to change

| File                                                                                      | Change                                                                               |
| ----------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------ |
| [Handlers.hs](jl4-lsp/app/LSP/L4/Handlers.hs) L301-341                                    | Change CodeLens title to "Show decision graph", remove the `simplify=True` CodeLens  |
| [+page.svelte](ts-apps/webview/src/routes/+page.svelte)                                   | Add "Simplified" toggle button to toolbar, send re-render request with simplify flag |
| [vscode-and-webview-messages.ts](ts-shared/jl4-client-rpc/vscode-and-webview-messages.ts) | Add `simplify` field to `RenderAsLadderInfo` or add a `ResimplifyRequest` message    |
| [extension.mts](ts-apps/vscode/src/extension.mts)                                         | Handle resimplify requests from webview                                              |

---

## Part 2: Results Panel

### Overview

A new webview panel (or a new view within the existing webview) that displays evaluation results from directives. Each result is a **section** — a card-like region with a header and type-appropriate rendering.

### How it works

1. User writes `#EVAL some expression` in their L4 file
2. LSP evaluates it (already happens today — results become diagnostics)
3. A **"Track result"** CodeLens appears above the directive
4. User clicks → that result is added as a section in the Results Panel
5. The panel shows all added results, ordered by source position
6. Each section has an **✕ dismiss** button to remove it

### Section anatomy

```
┌─────────────────────────────────────────────┐
│ ✕  #EVAL `monthly premium schedule`    L42  │  ← header (always visible, clickable to collapse)
├─────────────────────────────────────────────┤
│                                             │
│   [type-appropriate rendering here]         │  ← body (max-height ~40vh, scrollable if taller)
│                                             │
│  ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─  │
│   ▼ Show full result                        │  ← expand button (only if content overflows)
└─────────────────────────────────────────────┘
```

The header shows:

- **Dismiss button** (✕) to remove this section
- **Directive type** (`#EVAL`, `#ASSERT`, etc.)
- **Expression name** (the backtick identifier or expression text)
- **Source line** (clickable, jumps to source)
- **Collapse/expand** — clicking the header toggles the body

### Section sizing and overflow

- **Max height:** Each section body is capped at **~40vh** by default
- **Overflow:** If content exceeds 40vh, the body becomes scrollable with a subtle gradient fade at the bottom and an **"Expand"** button
- **Expanded state:** Clicking "Expand" removes the max-height cap, showing the full result. An "Collapse" button appears to restore the cap.
- **Collapsed state:** Clicking the header collapses the section to just the header bar (zero body height). Useful when many results are open and the user wants to focus on specific ones.
- **Charts** always render at a fixed aspect ratio (~16:9) within the section, never taller than 40vh

### VSCode theming

The Results Panel must look native to VSCode — not like a custom web app embedded in a panel.

**Approach:** Use VSCode's CSS custom properties throughout. These are automatically injected into all webview panels and respond to light/dark/high-contrast themes:

```css
.result-section {
  background: var(--vscode-editor-background);
  color: var(--vscode-editor-foreground);
  border: 1px solid var(--vscode-panel-border);
  font-family: var(--vscode-editor-font-family);
  font-size: var(--vscode-editor-font-size);
}

.result-section-header {
  background: var(--vscode-sideBarSectionHeader-background);
  color: var(--vscode-sideBarSectionHeader-foreground);
  border-bottom: 1px solid var(--vscode-panel-border);
  padding: 4px 8px;
  font-size: 11px;
  text-transform: uppercase;
  letter-spacing: 0.04em;
}

.result-section-body {
  max-height: 40vh;
  overflow-y: auto;
  padding: 8px 12px;
}

.result-section-body.expanded {
  max-height: none;
}

.dismiss-button {
  color: var(--vscode-icon-foreground);
}

.dismiss-button:hover {
  color: var(--vscode-errorForeground);
}

.badge-true {
  background: var(--vscode-testing-iconPassed);
  color: var(--vscode-editor-background);
}

.badge-false {
  background: var(--vscode-testing-iconFailed);
  color: var(--vscode-editor-background);
}

.assertion-passed {
  color: var(--vscode-testing-iconPassed);
}

.assertion-failed {
  color: var(--vscode-testing-iconFailed);
}
```

**Key principles:**

- **Zero hardcoded colors** — every color comes from `var(--vscode-*)` tokens
- **Light/dark/high-contrast** all work automatically
- **Section headers** styled like VSCode's native sidebar section headers (uppercase, small, muted)
- **Scrollbars** inherit VSCode's scrollbar styling
- **Typography** matches the editor font family and size for code blocks; system font for labels
- **Spacing** follows VSCode's 4px/8px grid
- **ECharts** theme colors derived from VSCode CSS variables at runtime (read `getComputedStyle` for chart palette generation)

### Type-aware result rendering

The body renderer inspects the evaluated JSON value and picks the best display:

#### Primitive types

| L4 Type    | JSON Shape                    | Rendering                                                                                    |
| ---------- | ----------------------------- | -------------------------------------------------------------------------------------------- |
| `BOOLEAN`  | `true` / `false`              | Large styled badge: green "TRUE" / red "FALSE"                                               |
| `NUMBER`   | `42`, `3.14`                  | Large formatted number with locale-appropriate separators                                    |
| `STRING`   | `"hello"`                     | Styled text block, monospace                                                                 |
| `DATE`     | `"2025-01-15"`                | Formatted date with relative hint (e.g. "15 January 2025 — 341 days ago"). Timezone-unaware. |
| `TIME`     | `"10:30:00"`                  | Formatted wall-clock time (e.g. "10:30 AM"). No timezone.                                    |
| `DATETIME` | `"2025-01-15T10:30:00+08:00"` | Formatted absolute timestamp with timezone (e.g. "15 January 2025, 10:30 AM SGT")            |

#### Charts (LIST OF PAIR)

When the result is a `LIST OF PAIR OF a, b` with chart-compatible types, render as a chart using Apache ECharts:

| `LIST OF PAIR OF ...` | Default chart  | X-axis format |
| --------------------- | -------------- | ------------- |
| `DATE, NUMBER`        | Line chart     | Date          |
| `DATETIME, NUMBER`    | Line chart     | DateTime      |
| `TIME, NUMBER`        | Line chart     | Time-of-day   |
| `NUMBER, NUMBER`      | Scatter plot   | Numeric       |
| `STRING, NUMBER`      | Bar chart      | Categories    |
| `NUMBER, STRING`      | Horizontal bar | Numeric       |

Chart type toggle in section toolbar (line ↔ area, bar ↔ pie, etc.).

For **multi-series** (record with multiple `LIST OF PAIR` fields): field names become series labels, rendered as multi-line chart.

#### Lists (non-PAIR)

| Shape            | Rendering                                |
| ---------------- | ---------------------------------------- |
| `LIST OF NUMBER` | Compact inline list or sparkline         |
| `LIST OF STRING` | Bulleted list                            |
| `LIST OF DATE`   | Timeline or formatted list               |
| `LIST OF record` | Table with field names as column headers |

#### Complex types (records, constructors, nested)

Render as **syntax-highlighted L4 code** using the WITH format:

```l4
Person WITH
    `full name` IS "Jane Smith"
    age IS 34
    `is eligible` IS TRUE
```

**Not** the OF format:

```l4
Person OF "Jane Smith", 34, TRUE   -- ✗ don't use this
```

#### Assertions (#ASSERT, #CHECK)

| Result           | Rendering                                                             |
| ---------------- | --------------------------------------------------------------------- |
| `#ASSERT` passes | Green checkmark badge: "✓ Assertion passed"                           |
| `#ASSERT` fails  | Red cross badge: "✗ Assertion failed" with the expression that failed |
| `#CHECK`         | Type information display (formatted L4 type signature)                |

#### Traces (#TRACE, #EVALTRACE)

Show the evaluated result (using the renderers above) **plus** a collapsible "Evaluation trace" section below it. The trace shows the step-by-step reduction, rendered as syntax-highlighted L4.

### Syntax highlighting for L4 code in the Results Panel

Use **`monaco.editor.colorize()`** — Monaco's static colorization API. It tokenizes a string and returns highlighted HTML without creating an editor instance. This is the right choice over highlight.js because:

- **Guaranteed identical highlighting** to the main editor — same tokenizer, same theme, same colors
- **Zero extra grammar to maintain** — the highlight.js grammar ([`legalese/highlightjs-l4`](https://github.com/legalese/highlightjs-l4)) is a regex approximation that will drift from the LSP's semantic tokenizer over time
- **Automatic theme response** — `colorize()` uses the active VSCode/Monaco theme, so light/dark mode works for free
- **Already loaded** — Monaco is available in both the VSCode webview and jl4-web, no additional dependency
- **Lightweight** — no editor instantiation, no DOM overhead

```typescript
// Usage in a Svelte component
// 1. Set the content and language on the DOM element
codeElement.dataset.lang = "jl4";
codeElement.textContent = l4CodeString;

// 2. Colorize in-place — applies token spans with theme-aware colors
await monaco.editor.colorizeElement(codeElement, { theme: "jl4Theme" });
```

`colorizeElement()` is preferred over `colorize()` because it operates directly on a DOM element — no intermediate HTML string to sanitize or inject. It applies `<span>` wrappers with inline styles matching the active theme, so light/dark mode works automatically.

For the VSCode extension webview (which doesn't load full Monaco), use the same approach as jl4-web's WASM bridge: call the semantic tokenizer directly and apply token spans as CSS classes using VSCode's theme CSS variables (`--vscode-editor-foreground`, etc.).

### L4 value pretty-printing

The Results Panel needs to convert JSON evaluation results back to readable L4 syntax. This is a TypeScript formatter that:

1. Detects constructor wrappers: `{"Person": {"full name": "Jane", "age": 30}}` → `Person WITH \n    \`full name\` IS "Jane"\n age IS 30`
2. Handles nested records recursively with indentation
3. Renders lists as `LIST ...` with items
4. Renders PAIRs as `PAIR OF fst, snd`
5. Renders primitives directly: `TRUE`, `42`, `"hello"`, dates as date literals

This formatted string is then passed through highlight.js for coloring.

---

## Architecture

### New package: `ts-shared/l4-result-visualizer/`

```
ts-shared/l4-result-visualizer/
├── package.json
├── src/
│   └── lib/
│       ├── index.ts                # Public API
│       ├── detect-type.ts          # Detect L4 type from JSON shape
│       ├── detect-chart.ts         # Detect chartable PAIR shapes
│       ├── format-l4-value.ts      # JSON → L4 WITH-format string
│       ├── chart-defaults.ts       # ECharts colors, axes, responsive config
│       ├── ResultsPanel.svelte     # Main scrollable panel with sections
│       ├── ResultSection.svelte    # Single result section (header + body)
│       ├── renderers/
│       │   ├── BooleanResult.svelte
│       │   ├── NumberResult.svelte
│       │   ├── StringResult.svelte
│       │   ├── DateResult.svelte
│       │   ├── TimeResult.svelte
│       │   ├── DateTimeResult.svelte
│       │   ├── ChartResult.svelte      # ECharts wrapper
│       │   ├── ListResult.svelte       # Lists (table for records, bullets for simple)
│       │   ├── CodeResult.svelte       # Syntax-highlighted L4 code (complex types)
│       │   └── AssertionResult.svelte  # Pass/fail badges
│       └── ResultRouter.svelte     # Picks the right renderer based on detected type
├── vite.config.ts
└── svelte.config.js
```

### Webview architecture

**Option A: Second webview panel** (recommended)

- Separate `PanelManager` instance for the Results Panel
- Opens beside the editor (like the ladder panel)
- Can coexist with the ladder panel (they show different things)
- Results Panel opens on the right; ladder panel can open right or below

**Option B: Tabs within single webview**

- Single webview with "Decision Graph" and "Results" tabs
- Simpler panel management but can't show both simultaneously

**Recommendation: Option A** — they serve different purposes and users may want both visible.

### Message protocol

**New messages in [vscode-and-webview-messages.ts](ts-shared/jl4-client-rpc/vscode-and-webview-messages.ts):**

```typescript
/** Add a result section to the Results Panel */
export interface AddResultSection {
  /** Unique ID for this section (based on directive source location) */
  id: string;
  /** Source line number for navigation */
  sourceLine: number;
  /** Document URI for navigation */
  sourceUri: string;
  /** The directive type */
  directive: "EVAL" | "TRACE" | "EVALTRACE" | "ASSERT" | "CHECK";
  /** The expression text (for the header) */
  expressionText: string;
  /** The evaluated JSON result */
  result: unknown;
  /** Whether the assertion passed (for ASSERT/CHECK) */
  assertionPassed?: boolean;
  /** Optional trace text (for TRACE/EVALTRACE) */
  trace?: string;
}

export const AddResult: RequestType<
  AddResultSection,
  { $type: "ok" | "error" }
> = {
  method: "addResult",
};

/** Remove a result section */
export const RemoveResult: NotificationType<{ id: string }> = {
  method: "removeResult",
};
```

### CodeLens for directives

**File: [Handlers.hs](jl4-lsp/app/LSP/L4/Handlers.hs)**

Extend the CodeLens handler to also emit "Track result" CodeLens for directives:

```haskell
-- For each #EVAL, #TRACE, #EVALTRACE, #ASSERT, #CHECK directive:
mkResultCodeLens srcPos directiveType exprText = CodeLens
  { _command = Just Command
    { _title = "Track result"
    , _command = "l4.renderResult"
    , _arguments = Just [toJSON verTextDocId, toJSON srcPos, toJSON directiveType, toJSON exprText]
    }
  , _range = pointRange $ srcPosToPosition srcPos
  }
```

**File: [extension.mts](ts-apps/vscode/src/extension.mts)**

Handle the new command:

1. Look up the directive's evaluation result (already computed as a diagnostic)
2. Open/reveal the Results Panel
3. Send `AddResult` message to the Results Panel webview

### Getting directive results to the extension

Currently directive results are LSP diagnostics (text strings). For rich rendering we need the **structured JSON result**, not the pretty-printed text.

**Option A: Custom LSP request** (cleaner)

- Add `l4/evalDirectiveResult` request that returns the structured result for a directive at a given position
- The LSP already evaluates these — just need to expose the `EvalDirectiveResult` as JSON instead of just text

**Option B: Piggyback on diagnostics** (simpler, less rich)

- Parse the diagnostic message text back into structured data
- Fragile, loses type information

**Recommendation: Option A** — add a custom LSP request.

**New LSP request in [CustomProtocol.hs](jl4-lsp/src/LSP/L4/Viz/CustomProtocol.hs):**

```haskell
-- Request the structured result of a directive at a given source position
data EvalDirectiveResultParams = EvalDirectiveResultParams
  { verDocId :: VersionedTextDocumentIdentifier
  , srcPos :: SrcPos
  }

data EvalDirectiveResultResponse = EvalDirectiveResultResponse
  { directive :: Text           -- "EVAL", "TRACE", etc.
  , expressionText :: Text      -- the expression as written
  , result :: Value             -- JSON-encoded evaluation result
  , assertionPassed :: Maybe Bool
  , trace :: Maybe Text         -- evaluation trace (if TRACE/EVALTRACE)
  }
```

This reuses the existing `nfToFnLiteral` pipeline from jl4-service to convert L4 values to JSON. The conversion code already exists in `Backend/Jl4.hs` — it needs to be extracted to a shared module or duplicated in the LSP.

---

## Implementation Order

| Step   | What                                                                                              | Where                                                 | Effort |
| ------ | ------------------------------------------------------------------------------------------------- | ----------------------------------------------------- | ------ |
| **1**  | Rename "Visualize" → "Show decision graph", remove "Simplify and visualize" CodeLens              | `jl4-lsp/app/LSP/L4/Handlers.hs`                      | Small  |
| **2**  | Add "Simplified" toggle inside ladder webview panel                                               | `ts-apps/webview/`, `ts-shared/l4-ladder-visualizer/` | Small  |
| **3**  | Add `l4/evalDirectiveResult` custom LSP request — return structured JSON for directive results    | `jl4-lsp/`, `jl4-core/`                               | Medium |
| **4**  | Add "Track result" CodeLens for `#EVAL` / `#TRACE` / `#EVALTRACE` / `#ASSERT` / `#CHECK`          | `jl4-lsp/app/LSP/L4/Handlers.hs`                      | Small  |
| **5**  | Create `ts-shared/l4-result-visualizer/` — type detection, L4 value formatter, highlight.js theme | `ts-shared/l4-result-visualizer/`                     | Medium |
| **6**  | Build primitive renderers (Boolean, Number, String, Date, Time, DateTime)                         | `ts-shared/l4-result-visualizer/src/lib/renderers/`   | Medium |
| **7**  | Build CodeResult renderer (syntax-highlighted L4 for complex types)                               | `ts-shared/l4-result-visualizer/src/lib/renderers/`   | Medium |
| **8**  | Build ChartResult renderer (ECharts for LIST OF PAIR)                                             | `ts-shared/l4-result-visualizer/src/lib/renderers/`   | Medium |
| **9**  | Build ResultsPanel + ResultSection (scrollable, ordered, dismissible)                             | `ts-shared/l4-result-visualizer/src/lib/`             | Medium |
| **10** | Create Results Panel webview in VSCode extension                                                  | `ts-apps/vscode/`, new webview app                    | Medium |
| **11** | Wire `l4.renderResult` command: LSP request → Results Panel                                       | `ts-apps/vscode/src/extension.mts`                    | Medium |
| **12** | Integrate into jl4-web                                                                            | `ts-apps/jl4-web/`                                    | Small  |
| **13** | Documentation + examples                                                                          | `doc/tutorials/`                                      | Small  |

Steps 1-2 (ladder cleanup) are independent of steps 3-12 (results panel).

Steps 5-9 (shared components) are independent of steps 10-11 (VSCode wiring).

---

## Dependencies

### npm (new)

- `echarts` — Apache ECharts (Apache 2.0)

### Not needed

- ~~`highlight.js`~~ — using `monaco.editor.colorizeElement()` instead (same tokenizer, same theme, no extra dependency)
- ~~`highlightjs-l4`~~ — Monaco's L4 tokenizer is the source of truth

### Haskell changes

- **New custom LSP request** `l4/evalDirectiveResult` in jl4-lsp
- **Shared value-to-JSON conversion** — extract/reuse `nfToFnLiteral` from jl4-service for LSP use
- No changes to jl4-core evaluation logic
- No changes to jl4-service

---

## Chart Details (LIST OF PAIR)

### Chart type inference from pair types

| `LIST OF PAIR OF ...` | Default chart  | X-axis format      |
| --------------------- | -------------- | ------------------ |
| `DATE, NUMBER`        | Line chart     | Date-formatted     |
| `DATETIME, NUMBER`    | Line chart     | DateTime-formatted |
| `TIME, NUMBER`        | Line chart     | Time-of-day        |
| `NUMBER, NUMBER`      | Scatter plot   | Numeric            |
| `STRING, NUMBER`      | Bar chart      | Categories         |
| `NUMBER, STRING`      | Horizontal bar | Numeric            |

### Label inference

| Label           | Source                                |
| --------------- | ------------------------------------- |
| Chart title     | Expression text from directive header |
| Series names    | Record field names (for multi-series) |
| Category labels | String values in `fst` position       |

### Multi-series

A record with multiple `LIST OF PAIR` fields → each field becomes a named series. Field names = series labels.

### JSON shape detection

PAIR serializes as `{"PAIR": {"fst": ..., "snd": ...}}`. Frontend detects:

- Array of PAIR objects → single series
- Record containing arrays of PAIR objects → multi-series
- Value type from first non-null `fst` value (ISO date regex → DATE, ISO datetime regex → DATETIME, time regex → TIME, number → NUMBER, other string → STRING)

### Smart defaults

- 8 accessible, distinct colors
- Auto-scale axes with padding
- Legend when > 1 series
- Tooltip on hover
- Responsive sizing
- Date axis auto-formats based on range
- Chart type toggle in section toolbar

---

## Open Questions

1. **Panel position**: Results Panel on right side (beside editor)? Or bottom (below editor)? Right side matches the ladder panel pattern.

2. **Auto-add all results**: Should there be an "Add all results" button that adds every directive result at once? Or always one-by-one via CodeLens?

3. **Live updates**: When the L4 source changes and is re-evaluated, should existing result sections auto-update? Or require manual re-click?

4. **Export**: Offer copy-to-clipboard for rendered results? PNG export for charts?

5. **Theming**: Dark mode support? The highlight.js theme should follow VSCode's current theme.
