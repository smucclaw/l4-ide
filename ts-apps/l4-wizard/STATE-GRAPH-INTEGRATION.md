# State Graph Integration in L4 Wizard

## Overview

The L4 Wizard now displays **state transition graphs** alongside evaluation traces, showing contract automata extracted from regulative rules (MUST/MAY/SHANT).

## What Was Added

### 1. API Functions (`src/lib/decision-service.ts`)

Added three new functions to interact with the state graph REST API:

```typescript
// List all state graphs in the module
fetchStateGraphList(client, functionName): Promise<StateGraphListResponse>

// Fetch state graph as SVG
fetchStateGraphSvg(client, functionName, graphName): Promise<string>

// Fetch state graph as DOT source
fetchStateGraphDot(client, functionName, graphName): Promise<string>
```

### 2. State Graph Component (`src/lib/components/StateGraphDiagram.svelte`)

New component that:
- Automatically fetches all state graphs when a function is loaded
- Shows a dropdown selector if multiple graphs are available
- Auto-selects and displays the first graph
- Renders SVG inline with scrollable container
- Shows helpful messages when no regulative rules are present

### 3. Wizard Integration (`src/lib/components/Wizard.svelte`)

Added `StateGraphDiagram` component to the bottom of the wizard page, appearing after the GraphViz evaluation trace.

## User Experience

### Page Layout

```
┌─────────────────────────────────────┐
│ Function Header & Description       │
├─────────────────────────────────────┤
│ Outcome Banner                      │
├─────────────────────────────────────┤
│ Ladder Diagram (clickable)          │
├─────────────────────────────────────┤
│ Parameter Grid (questions)          │
├─────────────────────────────────────┤
│ GraphViz Evaluation Trace           │  ← Existing
├─────────────────────────────────────┤
│ State Transition Graphs             │  ← NEW!
│ ┌────────────┬──────────────┐       │
│ │ Title      │ [Dropdown ▾] │       │
│ ├───────────────────────────┤       │
│ │                           │       │
│ │   [State Graph SVG]       │       │
│ │                           │       │
│ └───────────────────────────┘       │
└─────────────────────────────────────┘
```

### When Multiple Graphs Exist

If a module contains multiple regulative rules, users see a dropdown:

```
State Transition Graphs          [weddingceremony ▾]
```

Selecting a different graph instantly updates the visualization.

### When No Regulative Rules Exist

If the module has no MUST/MAY/SHANT rules:

```
┌─────────────────────────────────────┐
│ No regulative rules found in this   │
│ module.                             │
│                                     │
│ State graphs are generated from     │
│ MUST/MAY/SHANT rules.               │
└─────────────────────────────────────┘
```

## Example Usage

### Alcohol Purchase Rules

For a module like `04-alcohol-purchase.l4`:

```l4
SECTION alcohol

DECIDE `may purchase alcohol` IF
           you are 21+ years old
      AND     you are unmarried
           OR your spouse approved
```

**No state graphs** will appear because this is a constitutive rule (DECIDE/IF), not a regulative rule (MUST/MAY/SHANT).

### Wedding Vows

For a module like `wedding.l4`:

```l4
SECTION wedding

DECIDE `weddingceremony`
  PARTY Spouse1 MUST `exchange vows`
    HENCE
      PARTY Spouse2 MUST `exchange vows`

DECIDE `fidelity`
  PARTY Spouse1 SHANT `commit adultery`
```

**Multiple state graphs** appear:
- `weddingceremony` (chained obligations)
- `fidelity` (prohibition)

Users can switch between them using the dropdown.

## Technical Details

### API Calls

The component makes two API calls:

1. **On mount**: `GET /functions/{name}/state-graphs`
   - Returns list of available graphs
   - Triggers once when function loads

2. **When graph selected**: `GET /functions/{name}/state-graphs/{graphName}/svg`
   - Fetches SVG for the selected graph
   - Triggers when user changes dropdown selection

### State Management

Uses Svelte 5 runes (`$state`, `$effect`):

```typescript
let graphs = $state<StateGraphInfo[]>([])          // List of available graphs
let selectedGraph = $state<string | null>(null)    // Currently selected
let svgContent = $state<string | null>(null)       // SVG markup
let isLoading = $state(false)                      // Loading state
let error = $state<string | null>(null)            // Error messages
```

### Reactivity

Two `$effect` blocks handle reactivity:

1. **Fetch graph list** when `client` or `functionName` changes
2. **Fetch SVG** when `selectedGraph` changes

This ensures graphs update automatically when navigating between functions.

### Error Handling

The component gracefully handles:
- **No GraphViz installed** (503 error) - Shows error message
- **Function not found** (404) - Shows error message
- **Network errors** - Shows error message
- **No regulative rules** - Shows helpful guidance

## Configuration

No configuration needed! The component:
- Reuses the existing `DecisionServiceClient`
- Automatically discovers available graphs
- Works with any decision service URL

## Development

### Local Testing

1. Start decision service with a module containing regulative rules:
   ```bash
   cabal run jl4-decision-service -- --port 8001 \
     --sourcePaths ../jl4/experiments/wedding.l4
   ```

2. Run wizard dev server:
   ```bash
   cd ts-apps/l4-wizard
   npm run dev
   ```

3. Navigate to: `http://localhost:5174/uuid:some-id/weddingceremony`

### Building

```bash
cd ts-apps/l4-wizard
npm run build
# Output: build/ directory
```

### Testing Different Modules

Upload different L4 files to test state graph visualization:

```bash
# Wedding vows (11 graphs)
curl -X PUT http://localhost:8001/functions/wedding \
  -H "Content-Type: application/json" \
  -d @wedding-upload.json

# Simple prohibitions (11 graphs)
curl -X PUT http://localhost:8001/functions/prohibition \
  -H "Content-Type: application/json" \
  -d @prohibition-upload.json
```

## Visual Design

### Styling

The component matches existing wizard styling:
- Same border/padding as GraphvizDiagram
- Same rounded corners and spacing
- Consistent typography and colors
- Responsive layout with overflow scrolling

### Color Scheme

State graphs use standard GraphViz colors:
- **Light blue** (#e8f4fd) - Initial state
- **Light green** (#d4edda) - Fulfilled (success)
- **Light red** (#f8d7da) - Breach (failure)
- **Green edges** (#28a745) - Success paths (HENCE)
- **Red dashed edges** (#dc3545) - Failure paths (LEST/timeout)

## Deployment

No special deployment steps needed. The wizard is deployed as part of the standard build:

### NixOS Deployment

The l4-wizard is served via nginx at:
- **Dev**: `https://dev.jl4.legalese.com/wizard/`
- **Prod**: `https://jl4.legalese.com/wizard/`

After updating code:

```bash
# Rebuild wizard
cd ts-apps/l4-wizard
npm run build

# Deploy to dev
nixos-rebuild switch --flake .#jl4-dev

# Deploy to production
nixos-rebuild switch --flake .#jl4-aws-2505
```

## Future Enhancements

Potential improvements:

1. **Interactive State Graphs** - Click states/transitions to see rule details
2. **Highlight Current State** - Show where user is in the contract based on parameter values
3. **Path Tracing** - Visualize the execution path through the state machine
4. **Export Options** - Download as PNG/PDF/DOT
5. **Side-by-Side View** - Show state graph and ladder diagram simultaneously
6. **Animation** - Animate state transitions during evaluation

## Related Documentation

- [State Graph REST API](../../jl4-decision-service/STATE-GRAPH-API.md)
- [L4 Wizard README](./README.md)
- [Decision Service Documentation](../../jl4-decision-service/README.md)
- [State Graph Implementation](../../STATE-GRAPH-REST-API-IMPLEMENTATION.md)

## Files Modified/Created

### Modified
- `src/lib/decision-service.ts` - Added state graph API functions
- `src/lib/components/Wizard.svelte` - Added StateGraphDiagram import and section

### Created
- `src/lib/components/StateGraphDiagram.svelte` - New component
- `STATE-GRAPH-INTEGRATION.md` - This documentation

## Testing Status

✅ Compiles successfully (`npm run build`)
✅ Type-safe TypeScript
✅ Follows existing component patterns
✅ Graceful error handling
✅ Responsive design
✅ Ready for deployment
