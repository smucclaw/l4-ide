# jl4-service — New Multi-Tenant Decision Service

**Status:** Proposal
**Supersedes:** `jl4-decision-service` (kept as-is; this is a parallel new package)

---

## Plan 1 — New `jl4-service` package

### Context

The current `jl4-decision-service` is a single-tenant, ephemeral-state service: it loads L4 files
from disk at startup, holds compiled functions in an in-memory TVar, and loses all state on restart.
It has no notion of isolated deployments and no persistence.

`jl4-service` replaces this with a persistent, multi-tenant service that has:

- A **control plane** to deploy/replace/delete self-contained L4 bundles at runtime
- A **data plane** (same evaluation surface) namespaced per deployment
- A **bundle store** (filesystem) that survives restarts
- Startup that recompiles from stored sources (Option A serialisation — no CBOR yet)

The static binary goal means no runtime system dependencies beyond a mount point for `--store-path`.

---

### Architecture

```
jl4-service (new standalone package)
├── Types.hs              — core domain types (DeploymentId, DeploymentState, AppEnv)
├── BundleStore.hs        — filesystem persistence (save/load/list/delete)
├── Compiler.hs           — compile in-memory Map FilePath Text → Map FunctionName ValidatedFunction
├── ControlPlane.hs       — Servant routes: POST/GET/PUT/DELETE /deployments/{id}
├── DataPlane.hs          — Servant routes: /deployments/{id}/functions/{fn}/...
├── Schema.hs             — per-deployment OpenAPI generation
├── Application.hs        — startup: scan store, compile in background, wire Warp
├── Options.hs            — CLI: --store-path, --port, --server-name, --lazy-load
└── app/Main.hs           — entrypoint
```

---

### New files to create

#### `jl4-service/jl4-service.cabal`

Model after `jl4-decision-service.cabal`. Key differences:

- Add deps: `zip-archive`, `cryptohash-sha256` or `SHA`, `servant-multipart`
- Depend directly on `jl4-core` and `jl4-query-plan` — **not** on `jl4-decision-service`
- Drop: `jl4-websessions`, `jl4-lsp`, `servant-swagger-ui`
- GHC options: document `-static -optl-static` for production builds (not default)

Register in `cabal.project` by adding `./jl4-service`.

---

#### `jl4-service/src/Types.hs`

```haskell
newtype DeploymentId = DeploymentId Text
  deriving (Eq, Ord, Show, FromJSON, ToJSON, FromHttpApiData, ToHttpApiData)

data DeploymentState
  = DeploymentPending                                     -- being compiled
  | DeploymentReady (Map Text ValidatedFunction) DeploymentMetadata
  | DeploymentFailed Text                                 -- compilation error

data DeploymentMetadata = DeploymentMetadata
  { metaFunctions :: [FunctionSummary]    -- name, description, param schema
  , metaVersion   :: Text                 -- SHA256 of all source contents
  , metaCreatedAt :: UTCTime
  }

data AppEnv = MkAppEnv
  { deploymentRegistry :: TVar (Map DeploymentId DeploymentState)
  , bundleStore        :: BundleStore
  , serverName         :: Maybe Text
  }
```

---

#### `jl4-service/src/BundleStore.hs`

Filesystem store. Layout:

```
{store-path}/
  {deploymentId}/
    sources/           -- all .l4 files (preserving relative paths from zip)
    metadata.json      -- DeploymentMetadata (JSON)
    api-config.yaml    -- optional, uploaded alongside sources
```

Key functions:

```haskell
data BundleStore = BundleStore { storePath :: FilePath }

initStore      :: FilePath -> IO BundleStore

saveBundle     :: BundleStore -> DeploymentId
               -> Map FilePath Text          -- sources
               -> Maybe ByteString           -- api-config.yaml bytes
               -> DeploymentMetadata
               -> IO ()

loadBundle     :: BundleStore -> DeploymentId
               -> IO (Map FilePath Text, Maybe ByteString, DeploymentMetadata)

listDeployments :: BundleStore -> IO [DeploymentId]

deleteBundle   :: BundleStore -> DeploymentId -> IO ()
```

Writes use `directory` + `filepath` (already in scope). Atomic writes via temp file + rename.

---

#### `jl4-service/src/Compiler.hs`

Wraps the existing pipeline. Key reuse:

- `Import.Resolution.typecheckWithDependencies` — resolves full import graph from an in-memory
  `ModuleLookup` built from the uploaded source map
- `Backend.Jl4.precompileModule` and `createFunction` — produce `ValidatedFunction`
- `L4.Export.getExportedFunctions` — discover `@export`-annotated functions

```haskell
-- Compile an in-memory source tree to a ready-to-evaluate function registry.
-- Entry point for both deploy-time and restart-time recompilation.
compileBundle
  :: Map FilePath Text           -- all .l4 sources (relative paths as keys)
  -> IO (Either Text (Map Text ValidatedFunction, DeploymentMetadata))
```

Internally builds a `ModuleLookup` from the source map, feeds it to
`typecheckWithDependencies`, then calls `Backend.Jl4.createFunction` per exported `Decide`.

SHA256 of all source contents (sorted by path) becomes `metaVersion`.

---

#### `jl4-service/src/ControlPlane.hs` — Servant API

```
POST   /deployments
  body: multipart/form-data
    - sources: <zip file>
    - id: optional DeploymentId (auto-generated UUID if absent)
    - config: optional api-config.yaml
  → 202 Accepted { id, status: "compiling" }
    (compilation runs in background thread; poll GET /deployments/{id} for status)

GET    /deployments
  → 200 [{ id, status, metadata? }]

GET    /deployments/{id}
  → 200 { id, status, metadata } | 404

PUT    /deployments/{id}
  body: same multipart as POST
  → 202 Accepted (atomically replaces when compiled; old version serves until then)

DELETE /deployments/{id}
  → 204 No Content | 404
```

On `POST`/`PUT`:

1. Extract zip in-memory (`zip-archive`)
2. Build `Map FilePath Text` from zip entries
3. Save to bundle store atomically
4. Launch `async` compilation thread
5. Register `DeploymentPending` in TVar
6. Return 202 immediately
7. Thread: on success → atomic TVar swap to `DeploymentReady`; on failure → `DeploymentFailed`

`PUT` respects in-flight requests: the old `DeploymentReady` entry stays in TVar until the new
compilation completes, then an STM swap replaces it. In-flight evaluations hold a reference to
the old `Map Text ValidatedFunction` snapshot and complete normally.

---

#### `jl4-service/src/DataPlane.hs` — Servant API

Same evaluation surface as current `jl4-decision-service`, prefixed with `/deployments/{id}`:

```
GET  /deployments/{id}/functions
POST /deployments/{id}/functions/{fn}/evaluation
POST /deployments/{id}/functions/{fn}/evaluation/batch
POST /deployments/{id}/functions/{fn}/query-plan
GET  /deployments/{id}/functions/{fn}/state-graphs
GET  /deployments/{id}/functions/{fn}/state-graphs/{graph}
GET  /deployments/{id}/openapi.json
```

Handlers: look up `DeploymentId` in TVar → require `DeploymentReady` (404 if missing,
503 if still `Pending`, 500 with message if `Failed`) → delegate to existing handler logic
from `jl4-decision-service/src/Server.hs`.

**GraphViz**: drop PNG/SVG rendering endpoints (no `dot` subprocess). DOT source is still
returned in JSON responses via the existing `graphviz.dot` field.

---

#### `jl4-service/src/Application.hs` — Startup

```haskell
defaultMain :: IO ()
defaultMain = do
  Options{port, storePath, serverName, lazyLoad} <- execParser opts
  store  <- initStore storePath
  tvar   <- newTVarIO Map.empty
  let env = MkAppEnv tvar store serverName

  -- Scan existing bundles and register
  ids <- listDeployments store
  if lazyLoad
    then forM_ ids $ \did ->
      atomically $ modifyTVar tvar $ Map.insert did DeploymentPending
    else do
      -- Compile all in parallel, up to N threads
      void $ mapConcurrently (loadAndRegister env store) ids

  withStdoutLogger $ \logger ->
    runSettings (setPort port $ setLogger logger defaultSettings)
      (corsMiddleware $ app env)
```

`loadAndRegister` = `loadBundle store did >>= compileBundle >>= atomically . updateTVar`.

Lazy loading (`--lazy-load`): mark all as `Pending` on startup, compile on first request.
Default (eager): compile all in parallel on startup.

---

#### `jl4-service/src/Options.hs`

```haskell
data Options = Options
  { port       :: Int       -- default 8080
  , storePath  :: FilePath  -- default ~/.local/share/jl4-service
  , serverName :: Maybe Text
  , lazyLoad   :: Bool      -- default False
  }
```

---

### Modules copied from `jl4-decision-service` (then evolved independently)

These are copied verbatim into `jl4-service/src/` as a starting point. They are **not** imported
from `jl4-decision-service` — `jl4-service` has no dependency on that package. This lets the two
services evolve independently.

| Module to copy                 | Source file                                                | What it provides                                                               |
| ------------------------------ | ---------------------------------------------------------- | ------------------------------------------------------------------------------ |
| `Backend.Api`                  | `jl4-decision-service/src/Backend/Api.hs`                  | `FnLiteral`, `ResponseWithReason`, `RunFunction`, `TraceLevel`, `EvalBackend`  |
| `Backend.Jl4`                  | `jl4-decision-service/src/Backend/Jl4.hs`                  | `CompiledModule`, `precompileModule`, `evaluateWithCompiled`, `createFunction` |
| `Backend.DecisionQueryPlan`    | `jl4-decision-service/src/Backend/DecisionQueryPlan.hs`    | `getOrBuildDecisionQueryCache`, query plan logic                               |
| `Backend.FunctionSchema`       | `jl4-decision-service/src/Backend/FunctionSchema.hs`       | JSON schema generation per function                                            |
| `Backend.CodeGen`              | `jl4-decision-service/src/Backend/CodeGen.hs`              | Wrapper-based evaluation fallback                                              |
| `Backend.MaybeLift`            | `jl4-decision-service/src/Backend/MaybeLift.hs`            | Optional type handling                                                         |
| `Backend.DirectiveFilter`      | `jl4-decision-service/src/Backend/DirectiveFilter.hs`      | Directive filtering                                                            |
| `Backend.BooleanDecisionQuery` | `jl4-decision-service/src/Backend/BooleanDecisionQuery.hs` | Boolean decision compilation                                                   |

After copying, apply the following adaptations before treating these modules as the baseline for
`jl4-service`:

**`Backend.Jl4`** — accept an in-memory `Map FilePath Text` source map instead of `FilePath`
disk paths (the key change enabling bundle-based compilation).

**`Backend.Api`** — fix the input/output JSON asymmetry at copy time:

```haskell
-- jl4-decision-service (original)
data ResponseWithReason = ResponseWithReason
  { values    :: [(Text, FnLiteral)]      -- serializes as array of 2-element arrays
  , reasoning :: Reasoning
  , graphviz  :: Maybe GraphVizResponse
  }

-- jl4-service (corrected)
data ResponseWithReason = ResponseWithReason
  { fnResult  :: Map Text FnLiteral       -- serializes as JSON object, mirrors fnArguments
  , reasoning :: Reasoning
  , graphviz  :: Maybe GraphVizResponse
  }
```

The field is renamed `values` → `fnResult` to match the `fn`-prefixed input convention
(`fnArguments`, `fnEvalBackend`). The type changes from `[(Text, FnLiteral)]` to
`Map Text FnLiteral` so the output serializes as a JSON object (same style as input).

Construction sites in `Backend.Jl4` (two locations):

```haskell
-- before
{ values = [("result", r)] }

-- after
{ fnResult = Map.singleton "result" r }
```

The wire format becomes symmetric:

```json
// request
{ "fnArguments": { "income": 60000, "is_citizen": true } }

// response
{ "fnResult": { "result": true }, "reasoning": { ... }, "graphviz": null }
```

---

### What is NOT carried over from `jl4-decision-service`

- `jl4-websessions` integration (session CRUD backend, fetching L4 by UUID)
- `Examples.hs` (hardcoded example functions)
- `--sourcePaths` flag (replaced by zip upload)
- `Schema.hs` / Swagger UI (replaced by per-deployment `/openapi.json`)
- `Backend.GraphVizRender` — PNG/SVG rendering via `dot` subprocess dropped entirely
- `process` and `graphviz` dependencies removed

---

### Verification

```bash
# Build
cabal build jl4-service

# Start with an empty store
cabal run jl4-service -- --port 8080 --store-path /tmp/jl4-store

# Deploy a bundle (zip of .l4 files)
zip -r bundle.zip *.l4
curl -X POST http://localhost:8080/deployments \
  -F "sources=@bundle.zip" \
  -F "id=my-rules"

# Poll until ready
curl http://localhost:8080/deployments/my-rules

# Evaluate
curl -X POST http://localhost:8080/deployments/my-rules/functions/is-eligible/evaluation \
  -H "Content-Type: application/json" \
  -d '{"fnArguments": {"income": 60000}}'

# Replace bundle
curl -X PUT http://localhost:8080/deployments/my-rules \
  -F "sources=@bundle-v2.zip"

# Test restart persistence
pkill jl4-service
cabal run jl4-service -- --port 8080 --store-path /tmp/jl4-store
# Should reload 'my-rules' automatically
curl http://localhost:8080/deployments/my-rules

# Run existing decision service tests for Backend.* regression
cabal test jl4-decision-service-test
```
