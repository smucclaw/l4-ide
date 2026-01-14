# State Graph REST API Implementation Summary

## What Was Implemented

Added dynamic state transition graph visualization to the `jl4-decision-service` REST API, enabling real-time extraction and rendering of contract automata from L4 regulative rules.

## Changes Made

### 1. Backend Types (`jl4-decision-service/src/Backend/Api.hs`)

Added new types for state graph API:

```haskell
-- | Format for state graph output
data StateGraphFormat = StateGraphDot | StateGraphSvg | StateGraphPng

-- | Basic information about a state graph
data StateGraphInfo = StateGraphInfo
  { graphName :: Text
  , graphDescription :: Maybe Text
  }

-- | Response for listing all state graphs in a module
newtype StateGraphListResponse = StateGraphListResponse
  { graphs :: [StateGraphInfo]
  }
```

### 2. API Routes (`jl4-decision-service/src/Server.hs`)

Added four new endpoints under `/functions/{name}/state-graphs`:

- **`GET /state-graphs`** - List all state graphs in module
- **`GET /state-graphs/{graphName}`** - Get graph as DOT text
- **`GET /state-graphs/{graphName}/svg`** - Render as SVG
- **`GET /state-graphs/{graphName}/png`** - Render as PNG

### 3. Handler Functions (`jl4-decision-service/src/Server.hs`)

Implemented handlers that:
- Extract state graphs from compiled modules using `L4.StateGraph.extractStateGraphs`
- Reuse existing `Backend.GraphVizRender` for SVG/PNG conversion
- Return proper HTTP error codes (404 for not found, 503 for missing GraphViz)

### 4. Documentation

Created comprehensive documentation:
- **`STATE-GRAPH-API.md`** - API reference with examples
- **`test-state-graph-api.sh`** - Automated test script
- **This file** - Implementation summary

## API Examples

### List Graphs
```bash
curl http://localhost:8001/functions/mycontract/state-graphs
```

Returns:
```json
{
  "graphs": [
    {"graphName": "weddingceremony", "graphDescription": null},
    {"graphName": "noabandonment", "graphDescription": null},
    {"graphName": "fidelity", "graphDescription": null}
  ]
}
```

### Get DOT Source
```bash
curl http://localhost:8001/functions/mycontract/state-graphs/weddingceremony
```

### Get SVG
```bash
curl http://localhost:8001/functions/mycontract/state-graphs/weddingceremony/svg > graph.svg
```

### Get PNG
```bash
curl http://localhost:8001/functions/mycontract/state-graphs/weddingceremony/png > graph.png
```

## How It Works

1. **Function Upload**: User uploads L4 source with regulative rules (MUST/MAY/SHANT)
2. **Compilation**: Decision service parses and typechecks the module
3. **Storage**: Compiled module stored in `ValidatedFunction.fnCompiled`
4. **Extraction**: State graph endpoints call `StateGraph.extractStateGraphs` on the compiled module
5. **Rendering**:
   - DOT endpoint returns text directly
   - SVG/PNG endpoints pipe DOT through GraphViz `dot` command

## Testing

Run the test script:
```bash
# Start decision service first
cabal run jl4-decision-service -- --port 8001

# In another terminal
./jl4-decision-service/test-state-graph-api.sh
```

The script will:
- Upload wedding.l4 with multiple regulative rules
- List all extracted state graphs
- Download DOT/SVG/PNG for each graph
- Validate error handling
- Save outputs to `./test-output/`

## Integration Points

### With Existing Infrastructure

- **Reuses `Backend.GraphVizRender`** - Same PNG/SVG rendering as evaluation traces
- **Same error handling patterns** - Consistent 404/503 responses
- **Leverages compiled modules** - No separate compilation needed
- **Swagger auto-documentation** - Endpoints appear in OpenAPI spec

### With jl4-web Frontend (Future)

The frontend can:
1. Call `GET /state-graphs` after file changes to detect available graphs
2. Show dropdown if multiple graphs exist
3. Fetch SVG and embed with pan/zoom
4. Auto-refresh when user edits code

```typescript
// Example frontend integration
const response = await fetch('/functions/mycontract/state-graphs');
const { graphs } = await response.json();

// Show graph selector
const select = document.createElement('select');
graphs.forEach(g => {
  const option = new Option(g.graphName, g.graphName);
  select.add(option);
});

// Display selected graph
const svg = await fetch(`/functions/mycontract/state-graphs/${selected}/svg`)
  .then(r => r.text());
document.getElementById('graph-container').innerHTML = svg;
```

## Deployment

### No Infrastructure Changes Needed

The new endpoints are part of the existing decision service - no new systemd service or nginx config required.

### Deployment Steps

1. **Build updated decision service:**
   ```bash
   cabal build exe:jl4-decision-service
   ```

2. **Deploy to dev server:**
   ```bash
   # On dev.jl4.legalese.com
   nixos-rebuild switch --flake .#jl4-dev
   ```

3. **Verify endpoints:**
   ```bash
   curl https://dev.jl4.legalese.com/decision/functions/FUNCTION_NAME/state-graphs
   ```

4. **Deploy to production:**
   ```bash
   # On jl4.legalese.com
   nixos-rebuild switch --flake .#jl4-aws-2505
   ```

### URLs After Deployment

- **Dev**: `https://dev.jl4.legalese.com/decision/functions/{name}/state-graphs`
- **Prod**: `https://jl4.legalese.com/decision/functions/{name}/state-graphs`

## Example L4 Files with Regulative Rules

These files contain regulative rules that generate state graphs:

- `jl4/experiments/wedding.l4` - Wedding vows (11 graphs)
- `jl4/examples/ok/prohibition.l4` - SHANT rules (11 graphs)
- `jl4/examples/ok/contracts.l4` - Chained obligations (7 graphs)
- `jl4/experiments/patterns_and_idioms.l4` - Business contracts (4 graphs)
- `jl4/experiments/safe-post.l4` - Investment agreements (1 graph)

Total: **37 state graphs** available from existing L4 files in the repository.

## Benefits

1. **Dynamic Generation** - No static pre-compilation needed, works with user's custom L4 code
2. **REST API** - Standard HTTP endpoints, easy to integrate from any client
3. **Multiple Formats** - DOT (text), SVG (web), PNG (download)
4. **Automatic Discovery** - List endpoint reveals all graphs in a module
5. **Consistent Patterns** - Follows same conventions as existing trace endpoints

## Next Steps

### Immediate (Optional)
- Add caching for rendered SVG/PNG to avoid regenerating on every request
- Add query parameters to `StateGraphOptions` (e.g., `?showDeadlines=false`)

### Frontend Integration (Next Sprint)
- Add "State Graph" button/tab in jl4-web
- Show dropdown if multiple graphs available
- Embed SVG with pan/zoom controls
- Auto-refresh on code changes

### Advanced Features (Future)
- Interactive graph exploration (click transitions to see rule details)
- Highlight path through graph based on evaluation trace
- Export to other formats (PDF, Mermaid, PlantUML)
- Animation of state transitions during execution

## Files Modified/Created

### Modified
- `jl4-decision-service/src/Backend/Api.hs` - Added state graph types
- `jl4-decision-service/src/Server.hs` - Added routes and handlers

### Created
- `jl4-decision-service/STATE-GRAPH-API.md` - API documentation
- `jl4-decision-service/test-state-graph-api.sh` - Test script
- `STATE-GRAPH-REST-API-IMPLEMENTATION.md` - This file

### No Changes Needed
- `nix/configuration.nix` - Uses existing decision service
- `nix/jl4-decision-service/configuration.nix` - No config changes
- nginx configuration - Endpoints already proxied via `/decision/`

## Testing Status

✅ Code compiles successfully
✅ Test script created
✅ Documentation complete
✅ Ready for deployment

## OpenAPI/Swagger

The new endpoints automatically appear in the Swagger documentation:

```bash
# View OpenAPI spec
curl http://localhost:8001/swagger.json | jq '.paths' | grep state-graphs
```

Operation IDs:
- `listStateGraphs`
- `getStateGraphDot`
- `getStateGraphSvg`
- `getStateGraphPng`
