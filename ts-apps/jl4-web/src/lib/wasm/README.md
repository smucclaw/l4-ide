# L4 WASM Integration

This directory contains TypeScript infrastructure for running L4 language features
in the browser via WebAssembly, without requiring a server connection.

## Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                     Monaco Editor                           │
└───────────────────────────┬─────────────────────────────────┘
                            │ LSP Messages
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

## Files

- **index.ts** - Module exports
- **wasm-bridge.ts** - Low-level WASM interface with typed exports
- **wasm-message-transports.ts** - `WasmLspHandler` for processing LSP messages

## Usage

The WASM connection is created via `lsp-connection-factory.ts`:

```typescript
import { createLspConnection } from './lsp-connection-factory'

const connection = await createLspConnection({
  preferredType: 'wasm',
  wasmUrl: '/l4-core.wasm',
  wasmVersion: '1.0.0',
  websocketUrl: 'ws://localhost:5007', // fallback
  enableFallback: true,
})

// For WASM mode, use the handler directly
if (connection.wasmHandler) {
  connection.wasmHandler.onMessage((msg) => {
    // Handle outgoing messages (responses, notifications)
  })
  // Send incoming messages
  connection.wasmHandler.handleMessage(incomingLspMessage)
}
```

## Building the WASM Module

The WASM module is built from `jl4-core` using GHC's WASM backend with JS FFI:

```bash
# Set up GHC WASM toolchain (9.10+)
source ~/.ghc-wasm/env

# Build for WASM
wasm32-wasi-cabal build jl4-core --project-file=cabal-wasm.project
```

The Haskell side (`jl4-core/src/L4/Wasm.hs`) exports functions using GHC's JS FFI:

```haskell
{-# LANGUAGE CPP #-}

-- Only compiled for JavaScript/WASM backend
#ifdef ghcjs_HOST_OS
foreign export javascript "l4_check"
  js_l4_check :: JSVal -> IO JSVal

js_l4_check :: JSVal -> IO JSVal  
js_l4_check sourceVal = do
  let source = Text.pack $ fromJSString sourceVal
  pure $ toJSString $ Text.unpack $ l4Check source
#endif
```

The core functions (`l4Check`, `l4Hover`, etc.) are available on all platforms
for testing, but the JS FFI wrappers only compile on the WASM/JS backend.

See https://ghc.gitlab.haskell.org/ghc/doc/users_guide/wasm.html for GHC WASM docs.

## Supported Features

| Feature | Status | Notes |
|---------|--------|-------|
| Diagnostics | ✅ | Full parse + typecheck errors |
| Hover | ✅ | Type info at cursor position |
| Completions | ✅ | Basic L4 keywords |
| Semantic Tokens | ✅ | Lexer-based highlighting |
| Evaluation | ✅ | #EVAL directive execution |
| Go-to-definition | ❌ | Requires multi-file support |
| IMPORT resolution | ❌ | Requires virtual file system |

✅ = Implemented (TypeScript + Haskell)
❌ = Not planned for initial release

## Next Steps

1. **Build pipeline**: Set up CI to build jl4-core for wasm32-wasi target
2. **Integration**: Wire WasmLspHandler into Monaco's language client
3. **Context-aware completions**: Use scope information for smarter completions

## Environment Variables

- `VITE_WASM_URL` - URL to fetch the WASM module
- `VITE_WASM_VERSION` - Version string for cache busting
- `VITE_PREFER_WASM` - Set to "true" to prefer WASM over WebSocket
