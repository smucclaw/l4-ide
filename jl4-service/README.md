# jl4-service

Multi-tenant decision service for L4 rule bundles. Deploy L4 programs as persistent, namespaced REST APIs with zip-upload bundles, async compilation, and filesystem persistence.

## How It Differs from jl4-decision-service

| | jl4-decision-service | jl4-service |
|---|---|---|
| Tenancy | Single-tenant, all functions in one flat namespace | Multi-tenant, each deployment is isolated under `/deployments/{id}` |
| Loading | CLI flags (`--sourcePaths`) or hardcoded examples | Zip upload via REST API, persisted to disk |
| Lifecycle | Ephemeral, functions lost on restart | Persistent, auto-reloaded on startup |
| Function CRUD | PUT/POST/DELETE individual functions | Deploy/replace/delete entire bundles |
| PNG/SVG rendering | Server-side GraphViz rendering | DOT text only (render client-side) |

## Quick Start

```bash
# Build
cabal build jl4-service

# Start with default settings (port 8080, store at /tmp/jl4-store)
cabal run jl4-service

# Start with custom settings
cabal run jl4-service -- --port 9000 --store-path ~/.local/share/jl4-service
```

## Deploying a Bundle

Create a zip archive containing `.l4` files, then upload it:

```bash
# Create a bundle
cd jl4/experiments && zip -r /tmp/bundle.zip *.l4

# Deploy with a chosen ID
curl -X POST http://localhost:8080/deployments \
  -F "id=my-rules" \
  -F "sources=@/tmp/bundle.zip"
# Returns 202 with status "compiling"

# Poll until ready
curl http://localhost:8080/deployments/my-rules
# Returns {"dsId":"my-rules","dsStatus":"ready",...}
```

If you omit the `id` field, a UUID is generated automatically.

L4 functions are discovered via `@export` annotations in the source:

```l4
@export default Check whether a person qualifies
GIVEN walks IS A BOOLEAN
      eats  IS A BOOLEAN
      drinks IS A BOOLEAN
GIVETH A BOOLEAN
DECIDE compute_qualifies IF walks AND eats AND drinks
```

## API Reference

### Control Plane

Manage deployment lifecycle.

| Method | Endpoint | Description |
|--------|----------|-------------|
| `POST` | `/deployments` | Deploy a new bundle (multipart: `id` + `sources` zip) |
| `GET` | `/deployments` | List all deployments with status |
| `GET` | `/deployments/{id}` | Get deployment status |
| `PUT` | `/deployments/{id}` | Replace a deployment's bundle (old stays active until new compiles) |
| `DELETE` | `/deployments/{id}` | Remove a deployment |

Deployment states: `compiling` (202), `ready` (200), `failed` (200 with error).

### Data Plane

Evaluate functions within a deployment.

| Method | Endpoint | Description |
|--------|----------|-------------|
| `GET` | `/deployments/{id}/functions` | List available functions |
| `GET` | `/deployments/{id}/functions/{fn}` | Get function schema |
| `POST` | `/deployments/{id}/functions/{fn}/evaluation` | Evaluate a function |
| `POST` | `/deployments/{id}/functions/{fn}/evaluation/batch` | Batch evaluate across many input cases |
| `POST` | `/deployments/{id}/functions/{fn}/query-plan` | Get next-question suggestions for interactive elicitation |
| `GET` | `/deployments/{id}/functions/{fn}/state-graphs` | List state graphs from regulative rules |
| `GET` | `/deployments/{id}/functions/{fn}/state-graphs/{name}` | Get DOT source for a state graph |
| `GET` | `/deployments/{id}/openapi.json` | Deployment metadata |

### Evaluation

```bash
curl -X POST http://localhost:8080/deployments/my-rules/functions/compute_qualifies/evaluation \
  -H "Content-Type: application/json" \
  -d '{"fnArguments":{"walks": true, "drinks": true, "eats": true}}'
```

#### Trace Output

Include execution traces with `?trace=full` or the `X-L4-Trace: full` header. Add `?graphviz=true` to include DOT source in the response (requires `trace=full`).

```bash
curl -X POST 'http://localhost:8080/deployments/my-rules/functions/compute_qualifies/evaluation?trace=full&graphviz=true' \
  -H "Content-Type: application/json" \
  -d '{"fnArguments":{"walks": true, "drinks": true, "eats": true}}'
```

### Batch Evaluation

Evaluate a function across many input cases in parallel:

```bash
curl -X POST http://localhost:8080/deployments/my-rules/functions/compute_qualifies/evaluation/batch \
  -H "Content-Type: application/json" \
  -d '{
    "outcomes": ["result"],
    "cases": [
      {"@id": 1, "walks": true, "eats": true, "drinks": true},
      {"@id": 2, "walks": false, "eats": true, "drinks": true},
      {"@id": 3, "walks": true, "eats": false, "drinks": false}
    ]
  }'
```

### Query Planning

Build interactive questionnaires by asking only the questions that still matter:

```bash
curl -X POST http://localhost:8080/deployments/my-rules/functions/compute_qualifies/query-plan \
  -H "Content-Type: application/json" \
  -d '{"fnArguments":{"walks": true}}'
```

Returns which inputs are still needed, ranked by impact on the outcome.

## CLI Options

| Option | Description | Default |
|--------|-------------|---------|
| `--port`, `-p` | HTTP port | `8080` |
| `--store-path` | Directory for persisting deployment bundles | `/tmp/jl4-store` |
| `--server-name`, `-s` | Server URL for OpenAPI metadata | - |
| `--lazy-load` | Compile deployments on first request instead of at startup | `false` |

## Persistence

Deployments are stored on disk at `{store-path}/{deployment-id}/`:

```
{store-path}/
  my-rules/
    sources/
      main.l4
      helper.l4
    metadata.json
  other-deploy/
    sources/
      rules.l4
    metadata.json
```

On startup, the service scans the store directory and recompiles all deployments (or marks them pending with `--lazy-load`).

## Testing

```bash
# Run jl4-service tests (40 tests)
cabal test jl4-service-test

# Run with pattern filter
cabal test jl4-service-test --test-options='--match "BundleStore"'
```

Test coverage:

- **ApiSpec** -- FnLiteral JSON parsing, query parameter coercion
- **BooleanDecisionQuerySpec** -- Boolean formula support/restriction
- **BundleStoreSpec** -- Filesystem persistence round-trips
- **CodeGenSpec** -- Input field name collision avoidance
- **DecisionQueryCacheKeySpec** -- Cache key determinism
- **IntegrationSpec** -- Full deployment lifecycle, evaluation, batch, control plane HTTP
- **SchemaSpec** -- QuickCheck property tests for API type serialization

## Architecture

```
jl4-service/
  app/Main.hs              -- Entry point
  src/
    Application.hs          -- WAI app wiring, startup, CORS
    Options.hs              -- CLI argument parsing
    Types.hs                -- Core domain types (DeploymentId, AppEnv, batch types)
    BundleStore.hs          -- Filesystem persistence (save/load/list/delete)
    Compiler.hs             -- Bundle compilation (typecheck + export discovery)
    ControlPlane.hs         -- POST/GET/PUT/DELETE /deployments
    DataPlane.hs            -- /deployments/{id}/functions/... evaluation handlers
    Schema.hs               -- OpenAPI spec generation
    Backend/
      Api.hs                -- FnLiteral, ResponseWithReason, RunFunction
      Jl4.hs                -- L4 typechecking and evaluation via Shake rules
      FunctionSchema.hs     -- Parameter schema extraction from L4 types
      DecisionQueryPlan.hs  -- Query planning for interactive elicitation
      CodeGen.hs            -- Evaluation wrapper code generation
      BooleanDecisionQuery.hs  -- Boolean formula analysis (BDD-based)
      MaybeLift.hs          -- Deep Maybe lifting for partial inputs
      DirectiveFilter.hs    -- Directive-based function filtering
  test/
    Spec.hs                 -- hspec-discover entry point
    TestData.hs             -- Shared L4 source fixtures
    ...Spec.hs              -- Test modules
```
