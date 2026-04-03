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

### Automatic Build

When `VITE_PREFER_WASM=true` is set in your environment, the WASM module is
automatically built when you run `npm run dev` or `npm run build`. The build
script checks if the WASM files exist and only rebuilds if necessary.

```bash
# Enable WASM mode in .env.local
echo "VITE_PREFER_WASM=true" >> .env.local

# Run dev server - WASM will be built automatically if needed
npm run dev
```

To force a rebuild, delete the `static/wasm/` directory and run again.

### Manual Build

You can also build manually using the build script:

```bash
# From project root
cd jl4-wasm
./scripts/build-wasm.sh --all   # Build, optimize, and test
```

### Prerequisites

1. Install the GHC WASM toolchain (~30 min):

   ```bash
   curl https://gitlab.haskell.org/ghc/ghc-wasm-meta/-/raw/master/bootstrap.sh | FLAVOUR=9.10 bash
   ```

2. The build script automatically sources the environment from `~/.ghc-wasm/env`.

### Build Script Options

```bash
./scripts/build-wasm.sh              # Build only
./scripts/build-wasm.sh --optimize   # Build and optimize (-Oz)
./scripts/build-wasm.sh --test       # Build and run tests
./scripts/build-wasm.sh --all        # Build, optimize, and test
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

| Feature           | Status | Notes                                           |
| ----------------- | ------ | ----------------------------------------------- |
| Diagnostics       | ✅     | Full parse + typecheck errors                   |
| Hover             | ✅     | Type info at cursor position                    |
| Completions       | ✅     | Basic L4 keywords                               |
| Semantic Tokens   | ✅     | Lexer-based highlighting                        |
| Evaluation        | ✅     | #EVAL results shown as blue squiggly with hover |
| Visualization     | ✅     | Ladder diagram generation with code lenses      |
| Code Lenses       | ✅     | "Visualize" / "Simplify and visualize" actions  |
| IMPORT resolution | ✅     | Core libraries embedded; VFS for user files     |
| Go-to-definition  | ✅     | Single-file only (same as find references)      |
| Find references   | ✅     | Single-file only                                |

### Visualization Features

The WASM module supports ladder diagram visualization with the following capabilities:

- **Code Lenses**: "Visualize" and "Simplify and visualize" actions appear above
  each DECIDE rule that returns a BOOLEAN type.
- **User Selection**: Clicking a code lens visualizes that specific function and
  sets it as the current selection.
- **Auto-Refresh**: When the document changes, the currently selected function is
  automatically re-visualized. If the function no longer exists (e.g., was deleted
  or renamed), the visualization is cleared.

This behavior matches the native LSP - users must explicitly click a code lens to
start visualization.

### Import Resolution

The WASM module supports L4's `IMPORT` statement with the following capabilities:

- **Embedded Core Libraries**: The standard library (`prelude`), `math`, `jurisdiction`,
  `currency`, and other core modules are bundled into the WASM binary at compile time.
  No network requests are needed to use these.
- **Virtual File System (VFS)**: User files can be provided via VFS for multi-file
  projects. The TypeScript bridge supports adding files to the VFS before type-checking.
- **Resolution Order**: Embedded libraries are checked first, then VFS.

Example usage in L4:

```l4
IMPORT prelude     -- Uses embedded library
IMPORT math        -- Uses embedded library
IMPORT helper      -- Would look in VFS
```

✅ = Implemented
❌ = Not available in WASM mode

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
