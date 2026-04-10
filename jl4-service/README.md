# jl4-service

Multi-tenant decision service for L4 rule bundles. Deploy L4 programs as persistent, namespaced REST APIs with zip-upload bundles, async compilation, and filesystem persistence.

## Features

- **Multi-tenant** — isolated deployments under `/deployments/{id}`
- **Zip-bundle deployment** — upload multiple `.l4` files at once, persisted to disk
- **Filesystem-backed** — deployments auto-reload on startup
- **Staging** — old version serves traffic while new bundle compiles
- **Deduplication** — SHA-256 content hash skips recompilation of identical sources
- **Health check** — `GET /health` with deployment counts
- **Concurrency limits** — configurable, returns 503 when exceeded
- **Per-evaluation memory limit** — GHC allocation limit (default 256 MB)
- **Configurable timeouts** — separate eval and compile timeouts
- **Upload validation** — zip size, file count, path traversal, deployment ID format
- **Structured logging** — JSON lines to stdout
- **OpenAPI** — metadata at `/deployments/{id}/openapi.json`
- **GraphViz DOT output** — raw DOT only, clients render themselves

## Quick Start

```bash
# Build
cabal build jl4-service

# Start with default settings (port 8080, store at /tmp/jl4-store)
cabal run jl4-service

# Start with debug logging and custom port
cabal run jl4-service -- --debug --port 9000 --store-path ~/.local/share/jl4-service

# Or configure via environment variables
JL4_PORT=9000 JL4_DEBUG=true cabal run jl4-service
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
# Returns {"id":"my-rules","status":"ready",...}
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

### Health

| Method | Endpoint  | Description                               |
| ------ | --------- | ----------------------------------------- |
| `GET`  | `/health` | Health check with deployment state counts |

Returns `{"status":"healthy","deployments":{"total":N,"ready":N,"pending":N,"compiling":N,"failed":N}}`. Exempt from the concurrency limiter so orchestrator probes always succeed.

### Control Plane

Manage deployment lifecycle.

| Method   | Endpoint            | Description                                                         |
| -------- | ------------------- | ------------------------------------------------------------------- |
| `POST`   | `/deployments`      | Deploy a new bundle (multipart: `id` + `sources` zip)               |
| `GET`    | `/deployments`      | List all deployments (`?functions=simple\|full\|none`, `?scope=id`) |
| `GET`    | `/deployments/{id}` | Get deployment status (triggers compilation if pending)             |
| `PUT`    | `/deployments/{id}` | Replace a deployment's bundle (old stays active until new compiles) |
| `DELETE` | `/deployments/{id}` | Remove a deployment                                                 |

Deployment states: `pending` (lazy-load, not yet compiled), `compiling` (compilation in progress), `ready` (compiled and serving), `failed` (compilation error stored).

**Optimistic compilation:** Evaluation and function listing on pending deployments trigger compilation with a 2-second optimistic timeout. If compilation finishes within 2 seconds, the result is returned inline (200). If not, the response is HTTP 202 with `{"status":"compiling","retryAfterMs":2000}` and a `Retry-After: 2` header — the client should retry after the delay. File browsing endpoints never require compilation.

**Validation rules:**

- Deployment IDs: max 36 characters, `[a-zA-Z0-9_-]` only, no `..` sequences
- Zip uploads: max 2 MB (configurable), max 5096 files (configurable), no path traversal
- If the `id` field is omitted, a UUID is generated automatically
- Duplicate detection: if the uploaded sources match an existing deployment (by content hash), the existing deployment is returned instead of recompiling

### Data Plane

Evaluate functions within a deployment. All routes are available in both short form (`/{id}/{fn}/...`) and long form (`/deployments/{id}/functions/{fn}/...`).

| Method | Endpoint                                               | Short Route                      |
| ------ | ------------------------------------------------------ | -------------------------------- |
| `GET`  | `/deployments/{id}/functions`                          | `/{id}/functions`                |
| `GET`  | `/deployments/{id}/functions/{fn}`                     | `/{id}/{fn}`                     |
| `POST` | `/deployments/{id}/functions/{fn}/evaluation`          | `/{id}/{fn}/evaluation`          |
| `POST` | `/deployments/{id}/functions/{fn}/evaluation/batch`    | `/{id}/{fn}/evaluation/batch`    |
| `POST` | `/deployments/{id}/functions/{fn}/query-plan`          | `/{id}/{fn}/query-plan`          |
| `GET`  | `/deployments/{id}/functions/{fn}/state-graphs`        | `/{id}/{fn}/state-graphs`        |
| `GET`  | `/deployments/{id}/functions/{fn}/state-graphs/{name}` | `/{id}/{fn}/state-graphs/{name}` |
| `GET`  | `/deployments/{id}/openapi.json`                       | `/{id}/openapi.json`             |
| `GET`  | `/deployments/{id}/files`                              | `/{id}/files`                    |
| `GET`  | `/deployments/{id}/files/{path}.l4`                    | `/{id}/{path}.l4`                |

Function names with spaces can use hyphens or URL-encoding in the path (e.g., `check-person` or `check%20person` for `check person`).

### File Browsing

Browse L4 source files within a deployment. **File browsing works immediately after upload — no compilation required.** This includes all deployment states: pending, compiling, ready, and failed.

| Method | Endpoint                            | Description                                                    |
| ------ | ----------------------------------- | -------------------------------------------------------------- |
| `GET`  | `/deployments/{id}/files`           | List files with content (`?identifier=`, `?search=`, `?file=`) |
| `GET`  | `/deployments/{id}/files/{path}.l4` | Raw file content (`?lines=start:end` for line range)           |

The `/files` endpoint supports three query parameters (combinable):

- `?identifier=name` — find definitions and references of an L4 identifier (text-based, works pre-compilation)
- `?search=text` — grep source files (case-insensitive, works pre-compilation)
- `?file=path.l4` — scope to a specific file

Export information (which functions a file exports) is available only after compilation. Pre-compilation responses include file content but empty export lists.

### Evaluation

```bash
curl -X POST http://localhost:8080/deployments/my-rules/functions/compute_qualifies/evaluation \
  -H "Content-Type: application/json" \
  -d '{"arguments":{"walks": true, "drinks": true, "eats": true}}'
```

#### Trace Output

Include execution traces with `?trace=full` or the `X-L4-Trace: full` header. Add `?graphviz=true` to include DOT source in the response (requires `trace=full`).

```bash
curl -X POST 'http://localhost:8080/deployments/my-rules/functions/compute_qualifies/evaluation?trace=full&graphviz=true' \
  -H "Content-Type: application/json" \
  -d '{"arguments":{"walks": true, "drinks": true, "eats": true}}'
```

#### Deontic (Contract) Evaluation

Functions returning `DEONTIC` model contract obligations and require additional parameters for simulation:

```bash
curl -X POST http://localhost:8080/deployments/my-contract/functions/service-requirement/evaluation \
  -H "Content-Type: application/json" \
  -d '{
    "arguments": {"state": {"status": "Active", "metrics": {"revenue": 1000000}}},
    "startTime": 0,
    "events": [
      {"party": {"Name": "Alice"}, "action": "maintain eligible service", "at": 1}
    ]
  }'
```

- `arguments` — the function's GIVEN parameters (same as non-deontic)
- `startTime` — start time for contract simulation (required for DEONTIC)
- `events` — list of trace events, each with `party`, `action`, and `at` timestamp (required for DEONTIC)

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
  -d '{"arguments":{"walks": true}}'
```

Returns which inputs are still needed, ranked by impact on the outcome.

### MCP (Model Context Protocol)

The service exposes an [MCP](https://modelcontextprotocol.io/) JSON-RPC 2.0 endpoint that AI agents and LLM tool-use clients can call directly. MCP provides structured tool discovery and invocation without requiring browser integration.

| Method | Endpoint                    | Description                                                   |
| ------ | --------------------------- | ------------------------------------------------------------- |
| `GET`  | `/.well-known/mcp`          | MCP discovery endpoint (server info, capabilities, endpoints) |
| `GET`  | `/.well-known/mcp/manifest` | MCP manifest (legacy/alternative discovery)                   |
| `POST` | `/.mcp`                     | Org-wide MCP JSON-RPC endpoint (all deployments)              |
| `POST` | `/{id}/.mcp`                | Deployment-scoped MCP endpoint (short route)                  |
| `POST` | `/deployments/{id}/.mcp`    | Deployment-scoped MCP endpoint (canonical route)              |

The org-wide endpoint (`/.mcp`) exposes tools from all deployments. The scoped endpoints (`/{id}/.mcp`) restrict tool visibility to a single deployment.

#### Field Name Sanitization

L4 uses backtick identifiers with spaces (e.g., `` `function or purpose` ``), but JSON schema property names and URL path segments work better with hyphens. The service automatically sanitizes field names:

- **MCP and WebMCP schemas**: spaces and special characters are replaced with hyphens (e.g., `function or purpose` → `function-or-purpose`)
- **OpenAPI** (`/openapi.json`): uses sanitized names in URL paths; parameter schemas preserve original L4 names
- **Incoming arguments** (MCP tool calls, REST API evaluation, batch): both hyphenated and original spaced names are accepted and mapped back to the L4 originals
- **Function names in URLs**: both hyphenated (`/functions/check-person/evaluation`) and URL-encoded (`/functions/check%20person/evaluation`) forms are accepted

**Collision detection:** If two L4 field names would sanitize to the same hyphenated form (e.g., `` `foo bar` `` and `` `foo-bar` ``), compilation fails with a clear error message explaining the collision.

#### MCP Discovery

```bash
# Fetch the MCP discovery document (primary endpoint)
curl http://localhost:8080/.well-known/mcp
# {"name":"L4 Rules Engine","version":"1.0.0","protocol_version":"2025-03-26","capabilities":{"tools":{}},...}

# Fetch the MCP manifest (legacy/alternative)
curl http://localhost:8080/.well-known/mcp/manifest
# {"version":"2025-03-26","capabilities":{"tools":true},"endpoints":{"mcp":"/.mcp"}}
```

#### MCP JSON-RPC

Send standard JSON-RPC 2.0 requests to the `/.mcp` endpoint. The service supports `tools/list` (discover available tools) and `tools/call` (invoke a tool):

```bash
# List available tools
curl -X POST http://localhost:8080/.mcp \
  -H "Content-Type: application/json" \
  -d '{"jsonrpc":"2.0","id":1,"method":"tools/list"}'

# Call a tool
curl -X POST http://localhost:8080/.mcp \
  -H "Content-Type: application/json" \
  -d '{"jsonrpc":"2.0","id":2,"method":"tools/call","params":{"name":"my-rules/compute_qualifies","arguments":{"walks":true,"eats":true,"drinks":true}}}'

# Scoped to a single deployment
curl -X POST http://localhost:8080/my-rules/.mcp \
  -H "Content-Type: application/json" \
  -d '{"jsonrpc":"2.0","id":1,"method":"tools/list"}'
```

#### MCP Schema Design

MCP tool schemas are optimized to minimize tokens in AI context windows:

- **Non-deontic functions**: parameters are listed directly at the top level of the tool's `inputSchema` (no wrapper). The AI calls the tool with `{"product": {...}}`.
- **Deontic functions**: parameters are wrapped in `arguments` alongside `startTime` and `events`. The AI calls with `{"arguments": {...}, "startTime": 0, "events": [...]}`.

This differs from the REST API which always uses `{"arguments": {...}}` for consistency.

### WebMCP (Browser AI Agent Integration)

Deployments are automatically [WebMCP](https://webmachinelearning.github.io/webmcp/)-compatible. Browser AI agents can discover and call deployed L4 rules as structured tools via a JavaScript snippet.

| Method | Endpoint                         | Description                                           |
| ------ | -------------------------------- | ----------------------------------------------------- |
| `GET`  | `/` (browser)                    | Deployment explorer — lists all deployments/functions |
| `GET`  | `/deployments?functions=full`    | All deployments with full function schemas            |
| `GET`  | `/deployments?scope=deploy-id`   | Filtered by deployment                                |
| `GET`  | `/openapi.json`                  | Org-wide OpenAPI 3.0 spec                             |
| `GET`  | `/deployments/{id}/openapi.json` | Per-deployment OpenAPI 3.0 spec                       |
| `GET`  | `/.webmcp/embed.js`              | Org-wide JS that registers WebMCP tools               |
| `GET`  | `/.well-known/webmcp`            | Discovery manifest listing all deployments            |

The `/deployments` endpoint serves cached metadata even for pending (lazy-loaded) deployments, so it works immediately after a restart without triggering compilation.

Query parameters for `GET /deployments`:

- `?functions=simple` — name, description, returnType per function (default)
- `?functions=full` — include full parameter schemas in function details
- `?functions=none` — omit functions from metadata
- `?scope=id1,id2` — filter to specific deployments

The script registers tools based on the `data-tools` attribute. By default (`auto`), it registers per-rule tools if ≤ 20 rules, otherwise discovery tools (`search_rules`, `get_rule_schema`, `evaluate_rule`). File browsing tools (`list_files`, `read_file`, `search_identifier`, `search_text`) must be opted into explicitly via `file-tools` or `all`. Use comma-separated values to combine categories (e.g., `data-tools="rules,file-tools"`).

#### Visibility Headers

The proxy injects these headers to control what jl4-service includes in responses. All default to `true` when absent (local dev, direct access).

| Header                | Controls                                                               | Proxy permission |
| --------------------- | ---------------------------------------------------------------------- | ---------------- |
| `X-Include-Functions` | Functions in deployment metadata, function listing tools in MCP/WebMCP | `l4:rules`       |
| `X-Include-Files`     | Files in deployment metadata, file browsing tools in MCP/WebMCP        | `l4:read`        |
| `X-Include-Evaluate`  | Evaluation/batch/query-plan paths in OpenAPI, evaluation tools in MCP  | `l4:evaluate`    |

> **Note:** The legacy path `/webmcp.js` is redirected to `/.webmcp/embed.js` with a 301. Update existing embeds when convenient.

#### Embedding on Third-Party Websites

```html
<!-- All deployments, all functions -->
<script src="https://your-host/.webmcp/embed.js"></script>

<!-- Scoped to specific deployments -->
<script
  src="https://your-host/.webmcp/embed.js"
  data-scope="sell-scenario,safe-valuation"
></script>

<!-- With API key for cross-origin auth -->
<script
  src="https://your-host/.webmcp/embed.js"
  data-api-key="sk_live_xxx"
></script>
```

**Configuration attributes:**

- `data-scope` — Filter by deployment and/or function: `deploy-id` (one deployment), `id1,id2` (multiple), `id/function-name` (specific function), `*/function-name` (function across all deployments). Default: all.
- `data-tools` — Comma-separated list of tool categories: `rules` (one tool per rule), `rule-tools` (search/schema/evaluate), `file-tools` (list/read/search files), `auto` (default: `rules` if ≤20, otherwise `rule-tools`), `all` (everything). Example: `rules,file-tools`.
- `data-api-key` — API key for cloud-hosted deployments on [Legalese Cloud](https://legalese.cloud). Not needed for self-hosted instances.

## CLI Options

All options can also be set via environment variables. CLI arguments take precedence over environment variables.

| Option                      | Env Var                       | Description                                                                                                                                                                              | Default          |
| --------------------------- | ----------------------------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ---------------- |
| `--port`, `-p`              | `JL4_PORT`                    | HTTP port                                                                                                                                                                                | `8080`           |
| `--store-path`              | `JL4_STORE_PATH`              | Directory for persisting deployment bundles                                                                                                                                              | `/tmp/jl4-store` |
| `--server-name`, `-s`       | `JL4_SERVER_NAME`             | Server URL for OpenAPI metadata                                                                                                                                                          | -                |
| `--lazy-load`               | `JL4_LAZY_LOAD`               | Register deployments as pending on startup; compile on first evaluation/function access (optimistic 2s timeout, returns 202 if still compiling). File browsing always works immediately. | `false`          |
| `--debug`                   | `JL4_DEBUG`                   | Enable debug mode (verbose errors, debug-level logs)                                                                                                                                     | `false`          |
| `--max-zip-size`            | `JL4_MAX_ZIP_SIZE`            | Maximum zip upload size in bytes                                                                                                                                                         | `2097152` (2 MB) |
| `--max-file-count`          | `JL4_MAX_FILE_COUNT`          | Maximum number of files per zip upload                                                                                                                                                   | `5096`           |
| `--max-deployments`         | `JL4_MAX_DEPLOYMENTS`         | Maximum number of concurrent deployments                                                                                                                                                 | `1024`           |
| `--max-concurrent-requests` | `JL4_MAX_CONCURRENT_REQUESTS` | Maximum concurrent requests (503 when exceeded)                                                                                                                                          | `20`             |
| `--max-eval-memory-mb`      | `JL4_MAX_EVAL_MEMORY_MB`      | Per-evaluation allocation limit in MB                                                                                                                                                    | `256`            |
| `--eval-timeout`            | `JL4_EVAL_TIMEOUT`            | Evaluation timeout in seconds                                                                                                                                                            | `60`             |
| `--compile-timeout`         | `JL4_COMPILE_TIMEOUT`         | Compilation timeout in seconds                                                                                                                                                           | `60`             |

Boolean env vars accept `1`, `true`, or `yes` (case-insensitive).

## Logging

All output is structured JSON (one object per line) to stdout, suitable for log aggregators:

```json
{"time":"2026-02-21 19:25:34 UTC","level":"info","msg":"Starting jl4-service","port":8080,"debug":true,...}
{"time":"2026-02-21 19:25:35 UTC","level":"info","msg":"http_request","method":"GET","path":"/health","status":200,"duration_ms":0.42}
```

Log levels: `debug`, `info`, `warn`, `error`. Debug-level messages are suppressed unless `--debug` is set.

## Error Sanitization

By default, error responses return generic messages (e.g., `"Deployment compilation failed"`, `"Evaluation resource limit exceeded"`). When `--debug` is enabled, full error details are included in API responses and logs.

## Resource Limits

The service enforces several resource limits to protect against abuse:

- **Concurrency**: Returns `503 Service at capacity` when `--max-concurrent-requests` is exceeded. The `/health` endpoint is exempt.
- **Evaluation memory**: Each evaluation is limited to `--max-eval-memory-mb` of GHC heap allocations via `setAllocationCounter`. Returns `500` on limit exceeded.
- **Evaluation timeout**: Each evaluation is limited to `--eval-timeout` seconds. Returns `500` on timeout.
- **Compilation timeout**: Bundle compilation is limited to `--compile-timeout` seconds.
- **Zip size**: Upload rejected with `400` if larger than `--max-zip-size`.
- **File count**: Upload rejected with `400` if zip contains more than `--max-file-count` entries.
- **Deployment count**: New deployment rejected with `400` if `--max-deployments` is reached.

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

On startup, the service scans the store directory and recompiles all deployments. With `--lazy-load`, deployments are registered as pending and compiled on first evaluation or function access (with a 2-second optimistic timeout). File browsing (`list_files`, `read_file`, `search_identifier`, `search_text`) reads directly from disk and works immediately regardless of compilation state.

## Testing

```bash
# Run jl4-service tests
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
- **IntegrationSpec** -- Full deployment lifecycle, evaluation, batch, control plane HTTP, field name remapping
- **SanitizationSpec** -- Property name sanitization, reverse mapping, collision detection
- **SerialisationSpec** -- CBOR serialisation round-trips and cache rebuild
- **SchemaSpec** -- QuickCheck property tests for API type serialization

## Architecture

```
jl4-service/
  app/Main.hs              -- Entry point
  src/
    Application.hs          -- WAI app wiring, startup, middleware (CORS, concurrency, logging)
    Logging.hs              -- Structured JSON logger
    Options.hs              -- CLI argument parsing with env var defaults
    Types.hs                -- Core domain types (DeploymentId, AppEnv, health, batch types)
    BundleStore.hs          -- Filesystem persistence (save/load/list/delete)
    Compiler.hs             -- Bundle compilation (typecheck + export discovery)
    ControlPlane.hs         -- POST/GET/PUT/DELETE /deployments
    DataPlane.hs            -- /deployments/{id}/functions/... evaluation handlers + short routes
    DeploymentLoader.hs     -- Shared compilation logic (eager startup, lazy compile-on-access)
    ExplorerPage.hs         -- Landing page HTML (deployment explorer, API docs)
    McpServer.hs            -- MCP JSON-RPC 2.0 handler (tools/list, tools/call)
    Schema.hs               -- OpenAPI spec generation
    Shared.hs               -- Shared utilities (scope matching, metadata, sanitization, JSON errors)
    WebMCPPage.hs           -- Org-wide WebMCP JavaScript (tool registration, sanitization)
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
