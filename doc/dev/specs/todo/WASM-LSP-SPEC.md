# WebAssembly LSP Architecture Specification

## Status: Active Development

## Overview

This document outlines the architecture for running L4 language server features in the browser via WebAssembly, eliminating the need for WebSocket connections to a backend server.

## Language Simplification for WASM Compatibility

To enable clean WASM compilation, the following changes will be made to jl4-core:

### 1. Remove TemporalGit Feature (BREAKING)

**Decision**: Delete `L4.TemporalGit` entirely from the codebase.

**Rationale**: Git-based temporal versioning (`AT DATE`, `EVAL UNDER COMMIT`) is a **platform feature**, not a core language feature. 

**Removal scope**:
- [✅] Delete `jl4-core/src/L4/TemporalGit.hs`
- [✅] Remove imports from `L4.EvaluateLazy.Machine`
- [✅] Remove temporal context frames (`EvalUnderCommit1`, `EvalUnderCommit2`, etc.)
- [✅] Remove `process` dependency from `jl4-core.cabal`
- [✅] Update/remove related tests
- [✅] Update documentation (remove `AT DATE`, `EVAL UNDER COMMIT` references)

### 2. Simplify Citations (Remove CSV, Replace Regex)

**Decision**: Remove CSV file loading and replace pcre2 with simple string matching.

**Changes**:
- [✅] Remove `@ref-src` annotation support (no file IO in language)
- [✅] Remove `file-io` and `pcre2` dependencies from `jl4-core.cabal`
- [✅] Replace `Text.Regex.Pcre2` with simple pattern matching:
  - The only regex pattern used is: `sg-c-(\d{4})-([a-z]+)-(\d+)` → URL substitution
  - This can be replaced with a simple parser for known patterns
- [✅] Keep `@ref <filename>` syntax as a **clickable link** that opens the file if present in IMPORT search paths
- [✅] Keep `@ref-map` for inline reference definitions (no file IO needed)

**New `@ref` behavior**:
```l4
-- @ref with a filename becomes a clickable link (Ctrl+click to open)
DECIDE foo IS TRUE @ref some-legal-doc.pdf

-- @ref-map still works for inline URL mappings
@ref-map "1981/61 sec. 2" https://www.legislation.gov.uk/ukpga/1981/61/section/2
```

### 3. Safe Mode for HTTP Operations

**Decision**: Add a `--safe-mode` flag that disables `FETCH` and `POST` builtins.

**Implementation**:
- [ ] Add `SafeMode` flag to evaluation config
- [ ] When enabled, `FETCH` and `POST` return errors instead of making HTTP requests
- [ ] WASM builds always compile with safe mode (no `req` dependency)
- [ ] Decision service can run in unsafe mode for full functionality

```haskell
-- In L4.EvaluateLazy.Machine
data EvalConfig = EvalConfig
  { safeMode :: Bool  -- When True, FETCH/POST are disabled
  , ...  
  }

runBuiltin val UnaryFetch _ = do
  config <- getConfig
  if config.safeMode
    then InternalException $ RuntimeTypeError 
      "FETCH is disabled in safe mode (WASM/sandboxed environment)"
    else ... -- existing implementation
```

## Completed Work

### Phase 0: Connection Abstraction Layer (Done)

Created infrastructure to support future WASM integration:

1. **`ts-apps/jl4-web/src/lib/lsp-connection-factory.ts`**
   - Factory for creating LSP connections (WebSocket or WASM)
   - Versioned WASM binary caching using Cache API
   - Automatic fallback between connection types
   - Environment variable configuration

2. **Updated `ts-apps/jl4-web/src/routes/+page.svelte`**
   - Uses connection factory instead of direct WebSocket code
   - Proper cleanup on component destruction
   - Configuration-driven connection type selection

3. **Updated `ts-apps/jl4-web/src/app.d.ts`**
   - Added Vite environment variable types for WASM config

### Environment Variables

```bash
# WebSocket (current default)
VITE_SOCKET_URL=ws://localhost:5007

# WASM (future)
VITE_WASM_URL=/wasm/l4-core.wasm
VITE_WASM_VERSION=0.1.0
VITE_PREFER_WASM=true
```

## Current Architecture

```
┌─────────────────────┐      WebSocket       ┌─────────────────────┐
│   jl4-web (Monaco)  │ ◄──────────────────► │   jl4-lsp (Haskell) │
│   - Monaco Editor   │     LSP Protocol     │   - Shake/hls-graph │
│   - Language Client │                      │   - Full LSP        │
└─────────────────────┘                      └─────────────────────┘
```

## Target Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│                        Browser (jl4-web)                        │
│  ┌─────────────────┐     ┌─────────────────────────────────┐    │
│  │  Monaco Editor  │     │    WASM LSP Bridge              │    │
│  │                 │◄───►│  ┌───────────────────────────┐  │    │
│  │  Language       │     │  │  l4-wasm-core.wasm        │  │    │
│  │  Client         │ LSP │  │  - Parser                 │  │    │
│  │                 │Msgs │  │  - Type Checker           │  │    │
│  └─────────────────┘     │  │  - Evaluator              │  │    │
│                          │  │  - Diagnostics            │  │    │
│                          │  └───────────────────────────┘  │    │
│                          └─────────────────────────────────┘    │
└─────────────────────────────────────────────────────────────────┘
```

## Technical Challenges

### 1. GHC WASM Backend Limitations

The current `jl4-lsp` cannot be compiled to WASM because:

- **Threading**: Uses `-threaded` and `async` extensively
- **System Dependencies**: `unix`, `websockets`
- **Shake/hls-graph**: Multi-threaded build system

### 2. Dependency Analysis (After Simplification)

#### Dependencies to Remove from jl4-core

| Dependency | Current Usage | Action |
|------------|---------------|--------|
| `pcre2` | Regex in `@ref` patterns | Replace with simple string parser |
| `file-io` | CSV loading for `@ref-src` | Remove feature entirely |
| `process` | Git in `L4.TemporalGit` | Remove module entirely |
| `req` | HTTP in `FETCH`/`POST` | Conditional compile with safe mode |

#### After Cleanup: All WASM-Compatible

- `L4.Lexer` - Tokenization
- `L4.Parser` - Parsing
- `L4.Parser.SrcSpan` - Source positions
- `L4.Parser.MixfixRegistry` - Mixfix operators
- `L4.Syntax` - AST definitions
- `L4.TypeCheck` - Type checking
- `L4.TypeCheck.Types` - Type system
- `L4.TypeCheck.Unify` - Unification
- `L4.TypeCheck.Environment` - Built-in types/functions
- `L4.Desugar` - Desugaring passes
- `L4.Nlg` - Natural language generation
- `L4.JsonSchema` - JSON schema generation
- `L4.Print` - Pretty printing
- `L4.ExactPrint` - Exact printing
- `L4.FindDefinition` - Go-to-definition
- `L4.Citations` - Simplified (no file IO, no regex)
- `L4.EvaluateLazy.Machine` - With safe mode (no HTTP)

## Proposed Solution: l4-wasm-core

Create a new, minimal Haskell package that:
1. Depends only on WASM-compatible libraries
2. Exposes core L4 functionality via a simple C FFI
3. Can be compiled with GHC's WASM backend

### Package Structure

```
l4-wasm-core/
├── l4-wasm-core.cabal
├── src/
│   ├── L4Wasm/
│   │   ├── Api.hs           -- Exported FFI functions
│   │   ├── Diagnostics.hs   -- Generate diagnostics
│   │   ├── Hover.hs         -- Hover information
│   │   ├── Completion.hs    -- Completions
│   │   └── SemanticTokens.hs
│   └── cbits/
│       └── exports.c        -- Wrapper for WASM exports
└── wasm-build.sh
```

### Minimal Dependencies

```cabal
library
  build-depends:
    base,
    text,
    containers,
    mtl,
    megaparsec,
    -- Shared with jl4-core (core parsing/typing modules)
    jl4-core-pure  -- New package with no IO dependencies
```

### FFI Interface

```haskell
-- L4Wasm/Api.hs
module L4Wasm.Api where

import Foreign.C.String
import Foreign.C.Types

-- Parse L4 source and return JSON-encoded diagnostics
foreign export ccall l4_check :: CString -> CInt -> IO CString

-- Get hover information at position
foreign export ccall l4_hover :: CString -> CInt -> CInt -> CInt -> IO CString

-- Get completions at position  
foreign export ccall l4_complete :: CString -> CInt -> CInt -> CInt -> IO CString

-- Get semantic tokens
foreign export ccall l4_semantic_tokens :: CString -> CInt -> IO CString

-- Free allocated string
foreign export ccall l4_free :: CString -> IO ()
```

### TypeScript/JavaScript Bridge

```typescript
// ts-apps/jl4-web/src/lib/wasm-lsp-bridge.ts

export interface WasmLspBridge {
  check(source: string): Promise<Diagnostic[]>;
  hover(source: string, line: number, column: number): Promise<Hover | null>;
  complete(source: string, line: number, column: number): Promise<CompletionItem[]>;
  semanticTokens(source: string): Promise<SemanticTokens>;
}

export class L4WasmLsp implements WasmLspBridge {
  private wasm: WebAssembly.Instance | null = null;
  private memory: WebAssembly.Memory | null = null;
  
  constructor(private wasmUrl: string, private version: string) {}
  
  async initialize(): Promise<void> {
    // Check cache first
    const cacheKey = `l4-wasm-${this.version}`;
    const cache = await caches.open('l4-wasm-cache');
    
    let wasmModule: WebAssembly.Module;
    const cached = await cache.match(cacheKey);
    
    if (cached) {
      const buffer = await cached.arrayBuffer();
      wasmModule = await WebAssembly.compile(buffer);
    } else {
      const response = await fetch(this.wasmUrl);
      const buffer = await response.arrayBuffer();
      wasmModule = await WebAssembly.compile(buffer);
      await cache.put(cacheKey, new Response(buffer));
    }
    
    this.wasm = await WebAssembly.instantiate(wasmModule, {
      // Import specs...
    });
    this.memory = this.wasm.exports.memory as WebAssembly.Memory;
  }
  
  async check(source: string): Promise<Diagnostic[]> {
    // ... implementation
  }
}
```

### Monaco Integration

```typescript
// ts-apps/jl4-web/src/lib/wasm-language-client.ts

import type { L4LanguageClient, L4RpcRequestType } from 'jl4-client-rpc';
import { L4WasmLsp } from './wasm-lsp-bridge';

/**
 * A language client that runs L4 analysis in-browser via WASM
 * instead of connecting to a remote LSP server.
 */
export class WasmL4LanguageClient implements L4LanguageClient {
  private bridge: L4WasmLsp;
  private documentVersions: Map<string, number> = new Map();
  
  constructor(wasmUrl: string, version: string) {
    this.bridge = new L4WasmLsp(wasmUrl, version);
  }
  
  async start(): Promise<void> {
    await this.bridge.initialize();
    // Set up document change listeners, etc.
  }
  
  async sendRequest<P extends object, R>(
    type: L4RpcRequestType<P, R>,
    params: P
  ): Promise<R | null> {
    // Map LSP requests to WASM bridge calls
    // ...
  }
  
  async dispose(): Promise<void> {
    // Cleanup
  }
}
```

## Implementation Phases

### Phase 1: Language Cleanup (1-2 weeks)

Remove WASM-incompatible features from jl4-core:

#### 1.1 Delete TemporalGit

```bash
# Files to delete
rm jl4-core/src/L4/TemporalGit.hs

# Remove from cabal
# Edit jl4-core/jl4-core.cabal: remove L4.TemporalGit from exposed-modules
# Edit jl4-core/jl4-core.cabal: remove 'process' from build-depends
```

Update `L4.EvaluateLazy.Machine`:
- Remove imports of `L4.TemporalGit`
- Remove `resolveRulesEffectiveContext`, `resolveCommitContext` calls
- Remove `EvalUnderCommit1`, `EvalUnderCommit2` frames
- Simplify temporal context to only support explicit date overrides

#### 1.2 Simplify Citations

Replace `L4.Citations` with a simpler implementation:

```haskell
-- L4/Citations.hs (simplified)
module L4.Citations
  ( withRefMap
  , mkReferences
  , normalizeRef
  ) where

import Base (Text, NormalizedUri)
import qualified Base.Text as Text
import Data.Char (isSpace)
import Data.Vector (Vector)
import qualified Data.Vector as Vector

import L4.Parser.SrcSpan as Lexer
import qualified L4.Utils.IntervalMap as IVMap
import qualified L4.Lexer as Lexer

-- | Parse @ref-map annotations (inline, no file IO)
withRefMap :: Lexer.PosToken -> Vector (Text, Text)
withRefMap = \case
  Lexer.MkPosToken {payload = Lexer.TAnnotations (Lexer.TRefMap refmp)}
    | (ref, url) <- Text.breakOnEnd " " $ Text.strip refmp
    , not $ Text.all isSpace ref
    , not $ Text.all isSpace url
    -> Vector.singleton (normalizeRef ref, normalizeRef url)
  _ -> mempty

normalizeRef :: Text -> Text
normalizeRef = Text.toLower . Text.strip

-- | Build reference map from tokens
-- Now only supports verbatim matching (no regex)
mkReferences
  :: [Lexer.PosToken]
  -> Vector (Text, Text)
  -> IVMap.IntervalMap Lexer.SrcPos (NormalizedUri, Int, Maybe Text)
mkReferences tokens decoded = foldMap getReferences tokens
  where
    getReferences = \case
      Lexer.MkPosToken {payload = Lexer.TAnnotations (Lexer.TRef reference _), range} ->
        let mk v = IVMap.singleton (IVMap.srcRangeToInterval range) 
                     (range.moduleUri, range.length, v)
            ref = normalizeRef reference
            -- Simple verbatim lookup (case-insensitive)
            matchedUrl = lookup ref [(normalizeRef k, v) | (k, v) <- Vector.toList decoded]
        in mk matchedUrl
      _ -> IVMap.empty

-- NOTE: @ref-src removed - no file IO in language
-- NOTE: regex patterns removed - use verbatim matching only
```

#### 1.3 Add Safe Mode Flag

```haskell
-- L4/EvaluateLazy/Config.hs (new file)
module L4.EvaluateLazy.Config where

data EvalConfig = EvalConfig
  { safeMode :: !Bool
    -- ^ When True, FETCH and POST builtins are disabled.
    --   Always True for WASM builds.
  } deriving (Eq, Show)

defaultConfig :: EvalConfig
defaultConfig = EvalConfig { safeMode = False }

wasmConfig :: EvalConfig  
wasmConfig = EvalConfig { safeMode = True }
```

Update `L4.EvaluateLazy.Machine`:

```haskell
-- Conditional HTTP operations
runBuiltin val UnaryFetch _ = do
  config <- GetConfig
  if safeMode config
    then InternalException $ RuntimeTypeError
      "FETCH is disabled in safe mode. This L4 program requires network access."
    else do
      url <- expectString val
      -- ... existing HTTPS implementation ...

runPost urlVal headersVal bodyVal = do
  config <- GetConfig  
  if safeMode config
    then InternalException $ RuntimeTypeError
      "POST is disabled in safe mode. This L4 program requires network access."
    else do
      -- ... existing implementation ...
```

### Phase 2: Update Cabal Dependencies (1 day)

After the cleanup, update `jl4-core.cabal`:

```cabal
-- Remove these dependencies:
--   process      (was used by TemporalGit)
--   file-io      (was used by Citations CSV)
--   pcre2        (was used by Citations regex)

-- Make 'req' conditional:
flag safe-mode
  description: Disable FETCH/POST builtins (for WASM builds)
  default: False
  manual: True

library
  build-depends:
    base,
    text,
    containers,
    -- ... other pure deps ...
    
  if !flag(safe-mode)
    build-depends: req >= 3.0
    cpp-options: -DHTTP_ENABLED
```

### Phase 3: WASM Build Infrastructure (1 week)

1. Set up GHC WASM cross-compiler (via Nix or Docker)
2. Create build script for WASM target:

```bash
#!/bin/bash
# l4-wasm-core/wasm-build.sh
set -e

WASM_GHC=/path/to/wasm32-wasi-ghc

# Build with safe-mode flag (no HTTP)
cabal build jl4-core -f safe-mode --with-ghc=$WASM_GHC

$WASM_GHC \
  -O2 \
  -o l4-core.wasm \
  src/L4Wasm/Api.hs

# Optimize with wasm-opt
wasm-opt -O3 l4-core.wasm -o l4-core.opt.wasm

echo "WASM size: $(wc -c < l4-core.opt.wasm | numfmt --to=iec)"
```

3. Add to CI for automatic WASM builds

### Phase 4: FFI Layer & TypeScript Bridge (1-2 weeks)

Implement the FFI exports and complete the TypeScript bridge in `lsp-connection-factory.ts`:

```haskell
-- L4Wasm/Api.hs
module L4Wasm.Api where

import Foreign.C
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Aeson as Aeson

import L4.Parser (parseFile)
import L4.TypeCheck (checkModule)

-- | Parse and typecheck, return JSON-encoded diagnostics
foreign export ccall l4_check :: CString -> CInt -> IO CString
l4_check srcPtr srcLen = do
  src <- peekCStringLen (srcPtr, fromIntegral srcLen)
  let result = case parseFile "input.l4" (T.pack src) of
        Left err -> [parseDiagnostic err]
        Right parsed -> case checkModule parsed of
          (checked, errs) -> map typeDiagnostic errs
  newCString $ show $ Aeson.encode result
```

Complete the stub in `createWasmConnection()`:

```typescript
// ts-apps/jl4-web/src/lib/lsp-connection-factory.ts
export async function createWasmConnection(
  wasmUrl: string,
  version: string
): Promise<LspConnectionResult> {
  const wasmModule = await loadWasmCached(wasmUrl, version)
  const instance = await WebAssembly.instantiate(wasmModule, {
    wasi_snapshot_preview1: wasiPolyfill,
  })
  
  const bridge = new L4WasmBridge(instance)
  const transports = createWasmTransports(bridge)
  
  return {
    type: 'wasm',
    transports,
    dispose: async () => bridge.dispose(),
  }
}
```

### Phase 5: Monaco Integration (1 week)

1. Create `WasmL4LanguageClient` implementing `L4LanguageClient`
2. Map LSP methods to WASM FFI calls
3. Update `+page.svelte` to select connection type
4. Add fallback for features requiring full LSP

### Phase 6: Testing & Polish (1 week)

1. Cross-browser testing (Chrome, Firefox, Safari)
2. Performance benchmarking vs WebSocket
3. WASM binary size optimization
4. Error handling and graceful degradation
5. Documentation and examples

## Caching Strategy

### Versioned WASM Binary

```typescript
const WASM_VERSION = '0.1.0'; // Tied to jl4-core version
const WASM_CACHE_NAME = 'l4-wasm-v1';

async function loadWasmCached(): Promise<WebAssembly.Module> {
  const cache = await caches.open(WASM_CACHE_NAME);
  const cacheKey = `/wasm/l4-core-${WASM_VERSION}.wasm`;
  
  // Try cache first
  const cached = await cache.match(cacheKey);
  if (cached) {
    console.log(`Loading WASM from cache: ${cacheKey}`);
    return WebAssembly.compileStreaming(cached);
  }
  
  // Fetch and cache
  console.log(`Fetching WASM: ${cacheKey}`);
  const response = await fetch(cacheKey);
  
  if (!response.ok) {
    throw new Error(`Failed to fetch WASM: ${response.status}`);
  }
  
  // Clone for cache (response can only be consumed once)
  await cache.put(cacheKey, response.clone());
  
  return WebAssembly.compileStreaming(response);
}
```

### Service Worker for Offline Support

```typescript
// static/sw.js
const WASM_CACHE = 'l4-wasm-v1';
const WASM_URLS = [
  '/wasm/l4-core-0.1.0.wasm',
];

self.addEventListener('install', (event) => {
  event.waitUntil(
    caches.open(WASM_CACHE).then((cache) => {
      return cache.addAll(WASM_URLS);
    })
  );
});

self.addEventListener('fetch', (event) => {
  if (event.request.url.includes('/wasm/')) {
    event.respondWith(
      caches.match(event.request).then((response) => {
        return response || fetch(event.request);
      })
    );
  }
});
```

## Feature Support Matrix (After Simplification)

| Feature | WASM Mode | Server Mode | Notes |
|---------|-----------|-------------|-------|
| Syntax highlighting | ✅ Full | ✅ Full | Pure lexer |
| Parse errors | ✅ Full | ✅ Full | Pure parser |
| Type errors | ✅ Full | ✅ Full | Pure type checker |
| Hover information | ✅ Full | ✅ Full | Pure AST traversal |
| Go-to-definition | ✅ Full | ✅ Full | Pure AST traversal |
| Completions | ✅ Full | ✅ Full | Pure scope analysis |
| Semantic tokens | ✅ Full | ✅ Full | Pure lexer + type info |
| Code actions | ✅ Full | ✅ Full | Pure transformations |
| JSON Schema output | ✅ Full | ✅ Full | Pure generation |
| `@ref` annotations | ✅ Full | ✅ Full | Verbatim matching only |
| `@ref` file links | ✅ Full | ✅ Full | Ctrl+click opens file |
| `@ref-map` inline | ✅ Full | ✅ Full | No file IO needed |
| `IMPORT` statements | ⚠️ VFS | ✅ Full | Browser: virtual file system |
| `FETCH`/`POST` | ❌ Disabled | ✅ Full | Safe mode in WASM |
| Ladder visualization | ✅ Full | ✅ Full | Pure IR generation |
| Evaluation (pure) | ✅ Full | ✅ Full | No HTTP calls |
| Evaluation (network) | ❌ Disabled | ✅ Full | Requires safe-mode=false |

### Legend
- ✅ Full: Complete support
- ⚠️ VFS: Requires virtual file system in browser
- ❌ Disabled: Intentionally disabled in safe mode

### Features Removed from Language

| Former Feature | Reason for Removal |
|----------------|--------------------|
| `@ref-src` CSV loading | No file IO in language core |
| `@ref` regex patterns | Replaced with verbatim matching |
| `AT DATE` / git temporals | Platform feature, not language feature |
| `EVAL UNDER COMMIT` | Platform feature, not language feature |

## Limitations

Features that require special handling in WASM:

1. **File imports** (`IMPORT "other-file.l4"`) - Use virtual file system in browser
2. **HTTP calls** (`FETCH`, `POST`) - Disabled in safe mode; use decision service for network access

## Deployment Modes

### 1. WASM-Only Mode (Offline/Embedded)

For fully offline editing with no server dependency:

```typescript
const client = new WasmL4LanguageClient(wasmUrl, version);
await client.start();
// All LSP features work except FETCH/POST evaluation
```

**Use cases:**
- Offline documentation/tutorials
- Embedded editors in static sites
- Privacy-sensitive environments
- Quick prototyping without server setup

### 2. Server Mode (Full Features)

For production with network access:

```typescript
const client = new WebSocketL4LanguageClient(wsUrl);
await client.start();
// All features including FETCH/POST
```

**Use cases:**
- Decision service integration
- Production rule evaluation
- Multi-file projects with real file system

### 3. Hybrid Mode (Progressive Enhancement)

Start with WASM for instant feedback, upgrade to server if available:

```typescript
class HybridL4LanguageClient implements L4LanguageClient {
  private wasmClient: WasmL4LanguageClient;
  private wsClient: WebSocketL4LanguageClient | null = null;
  
  async start(): Promise<void> {
    // Instant startup with WASM
    await this.wasmClient.start();
    
    // Try to upgrade to server for network features
    try {
      this.wsClient = await connectWebSocket();
    } catch (e) {
      console.info('Running in offline mode (WASM-only)');
    }
  }
  
  async evaluate(expr: string): Promise<Result> {
    // Use server if available and expression uses FETCH/POST
    if (this.wsClient && this.usesNetworkFeatures(expr)) {
      return this.wsClient.evaluate(expr);
    }
    return this.wasmClient.evaluate(expr);
  }
}
```

## Resources

- [GHC WASM Backend](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/wasm.html)
- [WebAssembly System Interface (WASI)](https://wasi.dev/)
- [Monaco Language Client](https://github.com/TypeFox/monaco-languageclient)

## Immediate Next Steps

### Step 1: Language Cleanup (Priority: HIGH)

Before any WASM work, clean up jl4-core:

1. **Delete TemporalGit** (1 day)
   ```bash
   # Create a branch for this work
   git checkout -b remove-temporal-git
   
   # Delete the module
   rm jl4-core/src/L4/TemporalGit.hs
   
   # Find and update all references
   rg -l "TemporalGit" --type haskell
   rg -l "AT DATE|EVAL UNDER COMMIT" --type markdown
   ```

2. **Simplify Citations** (1 day)
   - Remove `@ref-src` support
   - Replace pcre2 with verbatim matching
   - Update `@ref` to be clickable file links

3. **Add Safe Mode Flag** (1 day)
   - Add `EvalConfig` with `safeMode` field
   - Gate `FETCH`/`POST` behind the flag
   - Update CLI to accept `--safe-mode`

4. **Update Dependencies** (0.5 day)
   - Remove `process`, `file-io`, `pcre2` from cabal
   - Make `req` conditional with a flag

5. **Update Tests** (1 day)
   - Remove/update tests for deleted features
   - Add tests for safe mode behavior

6. **Update Documentation** (0.5 day)
   - Remove references to `@ref-src`, temporal git features
   - Document safe mode flag
   - Update feature matrix

### Step 2: Verify WASM Compatibility (1 day)

After cleanup, test compilation:

```bash
# Install GHC WASM
ghcup install ghc wasm32-wasi-9.10

# Try compiling with safe-mode flag
cabal build jl4-core -f safe-mode --with-ghc=wasm32-wasi-ghc
```

### Step 3: Create WASM Package (1 week)

Once jl4-core compiles, create the FFI wrapper.

## Open Questions

1. How large will the WASM binary be? (Target: <10MB compressed)
2. Should `@ref` file links work with IMPORT search paths or current directory only?
3. Do we need a migration guide for users of removed features?
4. Should safe mode be the default, with an `--unsafe` flag to enable network?

## Related Issues

- Track TemporalGit removal in a GitHub issue
- Track Citations simplification in a GitHub issue
- Track safe mode implementation in a GitHub issue
