# L4 WASM Integration

This directory contains TypeScript infrastructure for running L4 language features
in the browser via WebAssembly, without requiring a server connection.

## Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                     Monaco Editor                           │
│                  (MonacoLanguageClient)                     │
└───────────────────────────┬─────────────────────────────────┘
                            │ MessageTransports
                            │ (reader/writer)
                            ▼
┌─────────────────────────────────────────────────────────────┐
│            WasmMessageReader / WasmMessageWriter            │
│  wasm-message-transports.ts                                  │
│  - Implements vscode-languageclient MessageTransports        │
│  - Provides seamless integration with MonacoLanguageClient   │
└───────────────────────────┬─────────────────────────────────┘
                            │
                            ▼
┌─────────────────────────────────────────────────────────────┐
│                WasmLspHandler                                │
│  wasm-message-transports.ts                                  │
│  - Processes LSP requests/notifications                      │
│  - Tracks document state                                     │
│  - Publishes diagnostics                                     │
└───────────────────────────┬─────────────────────────────────┘
                            │ Method calls
                            ▼
┌─────────────────────────────────────────────────────────────┐
│                   L4WasmBridge                               │
│  wasm-bridge.ts                                              │
│  - Loads and caches WASM module                              │
│  - Typed interface to WASM exports                           │
│  - JSON parsing of results                                   │
└───────────────────────────┬─────────────────────────────────┘
                            │ GHC JS FFI
                            ▼
┌─────────────────────────────────────────────────────────────┐
│              jl4-core.wasm (from Haskell)                   │
│  - Parsing (L4.Parser)                                       │
│  - Type checking (L4.TypeCheck)                              │
│  - Lexing for semantic tokens (L4.Lexer)                     │
└─────────────────────────────────────────────────────────────┘
```

The key insight is that `WasmMessageReader` and `WasmMessageWriter` implement the
standard `vscode-languageclient` `MessageTransports` interface. This means the
WASM mode works identically to WebSocket mode from Monaco's perspective - no
special handling needed in the application code.

## Files

- **index.ts** - Module exports
- **wasm-bridge.ts** - Low-level WASM interface with typed exports
- **wasm-message-transports.ts** - LSP message handling:
  - `WasmLspHandler` - Routes LSP methods to WASM bridge
  - `WasmMessageReader` - Receives LSP responses/notifications
  - `WasmMessageWriter` - Sends LSP requests/notifications
  - `createWasmMessageTransports()` - Creates `MessageTransports` for Monaco

## Usage

The WASM connection is created via `lsp-connection-factory.ts` and returns
standard `MessageTransports` that work with `MonacoLanguageClient`:

```typescript
import { createLspConnection, getDefaultConfig } from './lsp-connection-factory'

// Get config from environment variables
const config = getDefaultConfig()

// Create connection (works for both WebSocket and WASM)
const connection = await createLspConnection(config)

console.log(`Connected via ${connection.type}`) // 'websocket' or 'wasm'

// Use transports with MonacoLanguageClient - same API for both modes!
const client = new MonacoLanguageClient({
  name: 'JL4 Language Client',
  clientOptions: { documentSelector: ['jl4'] },
  messageTransports: connection.transports, // Works for both WebSocket and WASM
})

await client.start()

// Cleanup
await connection.dispose()
```

To enable WASM mode, set `VITE_PREFER_WASM=true` in your environment.

## Building the WASM Module

The WASM module is built from `jl4-wasm` using GHC's WASM backend with JS FFI.

### Prerequisites

1. Install the GHC WASM toolchain (~30 min):

   ```bash
   curl https://gitlab.haskell.org/ghc/ghc-wasm-meta/-/raw/master/bootstrap.sh | FLAVOUR=9.10 bash
   ```

2. Source the environment:
   ```bash
   source ~/.ghc-wasm/env
   ```

### Build Steps

```bash
# From project root
source ~/.ghc-wasm/env

# Build the WASM module
wasm32-wasi-cabal build jl4-wasm --project-file=cabal-wasm.project

# Generate JS FFI glue code
node ~/.ghc-wasm/wasm32-wasi-ghc/lib/post-link.mjs \
  -i dist-newstyle/build/wasm32-wasi/ghc-*/jl4-wasm-*/x/jl4-wasm/opt/build/jl4-wasm/jl4-wasm.wasm \
  -o dist-newstyle/jl4-wasm.mjs

# Copy to static assets
mkdir -p ts-apps/jl4-web/static/wasm
cp dist-newstyle/build/wasm32-wasi/ghc-*/jl4-wasm-*/x/jl4-wasm/opt/build/jl4-wasm/jl4-wasm.wasm \
   ts-apps/jl4-web/static/wasm/jl4-core.wasm
cp dist-newstyle/jl4-wasm.mjs ts-apps/jl4-web/static/wasm/jl4-core.mjs
```

### Architecture

The Haskell side (`jl4-core/src/L4/Wasm.hs`) exports functions using GHC's JS FFI:

```haskell
{-# LANGUAGE GHC2021 #-}
#if defined(wasm32_HOST_ARCH)

import GHC.Wasm.Prim

-- Export to JavaScript
foreign export javascript "l4_check"
  l4CheckJS :: JSString -> IO JSString

l4CheckJS :: JSString -> IO JSString
l4CheckJS source = do
  let result = l4Check (Text.pack $ fromJSString source)
  pure $ toJSString $ Text.unpack result

#endif
```

The WASI reactor pattern is used (no main function), requiring:

1. Call `_initialize()` once after instantiation to set up the RTS
2. Then call exported functions as needed

### Testing

Test directly with Node.js:

```bash
source ~/.ghc-wasm/env
cd ts-apps/jl4-web/static/wasm
~/.ghc-wasm/nodejs/bin/node -e "
  // See wasm-test.html for full example
  const instance = await WebAssembly.instantiate(wasmModule, imports);
  instance.exports._initialize();
  const result = await instance.exports.l4_check('DECIDE x IS 42');
  console.log(result); // []
"
```

See https://ghc.gitlab.haskell.org/ghc/doc/users_guide/wasm.html for GHC WASM docs.

## Supported Features

| Feature           | Status | Notes                         |
| ----------------- | ------ | ----------------------------- |
| Diagnostics       | ✅     | Full parse + typecheck errors |
| Hover             | ✅     | Type info at cursor position  |
| Completions       | ✅     | Basic L4 keywords             |
| Semantic Tokens   | ✅     | Lexer-based highlighting      |
| Evaluation        | ✅     | #EVAL directive execution     |
| Visualization     | 🔄     | Ladder diagram (l4.visualize) |
| Go-to-definition  | ❌     | Requires multi-file support   |
| IMPORT resolution | ❌     | Requires virtual file system  |

✅ = Implemented (TypeScript + Haskell)
🔄 = TypeScript ready, awaiting Haskell export (l4_visualize)
❌ = Not planned for initial release

## Next Steps

1. **Build pipeline**: Set up CI to build jl4-core for wasm32-wasi target
2. **Context-aware completions**: Use scope information for smarter completions
3. **Binary size optimization**: Use `wasm-opt` and compression

## Environment Variables

- `VITE_WASM_URL` - URL to fetch the WASM module (default: `/wasm/jl4-core.wasm`)
- `VITE_WASM_JS_URL` - URL to fetch the JS FFI glue (default: `/wasm/jl4-core.mjs`)
- `VITE_WASM_VERSION` - Version string for cache busting (default: `dev`)
- `VITE_PREFER_WASM` - Set to "true" to prefer WASM over WebSocket

## File Sizes

| File          | Size   | Notes                          |
| ------------- | ------ | ------------------------------ |
| jl4-core.wasm | ~42 MB | Unoptimized, includes full RTS |
| jl4-core.mjs  | ~5 KB  | JS FFI glue code               |

The WASM file is large because it includes the entire Haskell runtime.
Future optimizations could include:

- `wasm-opt` optimization passes
- Dead code elimination
- Compression (gzip/brotli)
