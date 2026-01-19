# jl4-wasm

WASM module for running L4 language features in the browser.

This package produces a standalone WebAssembly binary that can be loaded in any
browser or Node.js environment, enabling L4 language features without a server.

## Prerequisites

Install the GHC WASM toolchain (takes ~30 minutes):

```bash
curl https://gitlab.haskell.org/ghc/ghc-wasm-meta/-/raw/master/bootstrap.sh | FLAVOUR=9.10 bash
```

Then source the environment before any WASM commands:

```bash
source ~/.ghc-wasm/env
```

## Building

### Quick Build (Recommended)

```bash
# From project root
source ~/.ghc-wasm/env

# Build jl4-wasm (this also builds jl4-core as a dependency)
wasm32-wasi-cabal build jl4-wasm --project-file=cabal-wasm.project

# Generate JS FFI glue code
$(wasm32-wasi-ghc --print-libdir)/post-link.mjs \
  -i dist-newstyle/build/wasm32-wasi/ghc-*/jl4-wasm-*/x/jl4-wasm/opt/build/jl4-wasm/jl4-wasm.wasm \
  -o dist-newstyle/jl4-wasm.mjs

# Copy to web app static assets
mkdir -p ts-apps/jl4-web/static/wasm
cp dist-newstyle/build/wasm32-wasi/ghc-*/jl4-wasm-*/x/jl4-wasm/opt/build/jl4-wasm/jl4-wasm.wasm \
   ts-apps/jl4-web/static/wasm/jl4-core.wasm
cp dist-newstyle/jl4-wasm.mjs ts-apps/jl4-web/static/wasm/jl4-core.mjs
```

The build produces:
- `jl4-wasm.wasm` (~42 MB unoptimized) - The WebAssembly binary
- `jl4-wasm.mjs` (~5 KB) - JavaScript FFI glue code

### Optimizing the Binary

The raw WASM binary is large (~42 MB) but can be significantly reduced using `wasm-opt`:

```bash
# Install binaryen (provides wasm-opt)
# macOS: brew install binaryen
# Ubuntu: sudo apt install binaryen

# Run the optimization script (from jl4-wasm directory)
./scripts/optimize-wasm.sh -Oz --test
```

Optimization results:

| Level | Size | Reduction | Notes |
|-------|------|-----------|-------|
| Unoptimized | 42 MB | - | Raw GHC output |
| `-Os` | 22 MB | 47% | Size-optimized |
| `-Oz` | 10 MB | 76% | Aggressive size optimization (recommended) |

The `-Oz` level reduces the binary from 42 MB to ~10 MB. With HTTP compression (gzip/brotli), the transfer size is typically 3-4 MB.

## Exported Functions

The following functions are exported via GHC's JavaScript FFI:

| Function | Signature | Description |
|----------|-----------|-------------|
| `l4_check` | `(source: string) => Promise<string>` | Parse and type-check, returns JSON diagnostics |
| `l4_hover` | `(source: string, line: number, col: number) => Promise<string>` | Get hover info at position |
| `l4_completions` | `(source: string, line: number, col: number) => Promise<string>` | Get completion suggestions |
| `l4_semantic_tokens` | `(source: string) => Promise<string>` | Get semantic tokens for highlighting |
| `l4_eval` | `(source: string) => Promise<string>` | Evaluate #EVAL directives |

All functions return JSON-encoded results as strings.

## Usage from JavaScript

GHC WASM uses a two-phase loading process:

```javascript
// 1. Load the JS FFI glue code
const jsModule = await import('./jl4-core.mjs');
const generateImports = jsModule.default;

// 2. Load and compile the WASM module
const wasmBuffer = await fetch('./jl4-core.wasm').then(r => r.arrayBuffer());
const wasmModule = await WebAssembly.compile(wasmBuffer);

// 3. Create exports proxy (needed for FFI callbacks)
const exportsProxy = {};
const jsFFIImports = generateImports(exportsProxy);

// 4. WASI imports (minimal stubs for reactor mode)
const wasiImports = {
  clock_time_get: (_clockId, _precision, out) => {
    const view = new DataView(exportsProxy.memory.buffer);
    view.setBigUint64(out, BigInt(Date.now()) * BigInt(1_000_000), true);
    return 0;
  },
  fd_write: () => 0,
  fd_read: () => 0,
  fd_close: () => 0,
  fd_seek: () => 0,
  fd_fdstat_get: () => 0,
  fd_fdstat_set_flags: () => 0,
  fd_filestat_get: () => 0,
  fd_filestat_set_size: () => 0,
  fd_prestat_get: () => 8, // EBADF
  fd_prestat_dir_name: () => 8,
  path_create_directory: () => 63, // ENOSYS
  path_filestat_get: () => 63,
  path_open: () => 63,
  poll_oneoff: () => 0,
  proc_exit: (code) => console.warn('proc_exit:', code),
  args_sizes_get: (argc, argvBufSize) => {
    const view = new DataView(exportsProxy.memory.buffer);
    view.setUint32(argc, 0, true);
    view.setUint32(argvBufSize, 0, true);
    return 0;
  },
  args_get: () => 0,
  environ_sizes_get: (environc, environBufSize) => {
    const view = new DataView(exportsProxy.memory.buffer);
    view.setUint32(environc, 0, true);
    view.setUint32(environBufSize, 0, true);
    return 0;
  },
  environ_get: () => 0,
};

// 5. Instantiate with all imports
const instance = await WebAssembly.instantiate(wasmModule, {
  wasi_snapshot_preview1: wasiImports,
  ghc_wasm_jsffi: jsFFIImports,
});

// 6. Copy exports to proxy (required for FFI callbacks to work)
Object.assign(exportsProxy, instance.exports);

// 7. Initialize the WASI reactor (Haskell RTS)
instance.exports._initialize();

// 8. Now you can call L4 functions!
const diagnostics = await instance.exports.l4_check('DECIDE x IS 1 + 2');
console.log(JSON.parse(diagnostics)); // []

const hover = await instance.exports.l4_hover('DECIDE x IS 42', 0, 7);
console.log(JSON.parse(hover)); // { contents: { kind: "markdown", value: "```\nNUMBER\n```" }, ... }
```

## Testing

### Browser Test Page

A test page is available when running the web app:

```bash
cd ts-apps/jl4-web && npm run dev
# Open http://localhost:5173/wasm-test.html
```

### Node.js Test

```bash
source ~/.ghc-wasm/env
cd ts-apps/jl4-web/static/wasm

# Use the GHC WASM Node.js (has all required features)
~/.ghc-wasm/nodejs/bin/node -e "
const fs = require('fs');

async function main() {
  const jsModule = await import('./jl4-core.mjs');
  const generateImports = jsModule.default;
  
  const wasmBuffer = fs.readFileSync('./jl4-core.wasm');
  const wasmModule = await WebAssembly.compile(wasmBuffer);
  
  const exportsProxy = {};
  const jsFFIImports = generateImports(exportsProxy);
  
  // ... (WASI imports as above) ...
  
  const instance = await WebAssembly.instantiate(wasmModule, {
    wasi_snapshot_preview1: wasiImports,
    ghc_wasm_jsffi: jsFFIImports,
  });
  
  Object.assign(exportsProxy, instance.exports);
  instance.exports._initialize();
  
  const result = await instance.exports.l4_check('DECIDE x IS 42');
  console.log('Diagnostics:', result); // []
}

main();
"
```

## Architecture

This package is a thin wrapper that:
1. Re-exports `L4.Wasm` from `jl4-core` 
2. Provides a `Main.hs` that's empty (reactor mode has no main)
3. Configures the build for WASI reactor mode

The actual implementation is in `jl4-core/src/L4/Wasm.hs`, which:
- Provides pure Haskell functions for all L4 features
- Exports JavaScript FFI wrappers when compiled for `wasm32` architecture
- Uses `foreign export javascript` syntax from GHC's JS FFI

## File Sizes

| File | Unoptimized | Optimized (-Oz) | Compressed |
|------|-------------|-----------------|------------|
| jl4-core.wasm | ~42 MB | ~10 MB | ~3-4 MB (gzip) |
| jl4-core.mjs | ~5 KB | ~5 KB | ~2 KB (gzip) |

The CI pipeline automatically runs `wasm-opt -Oz` on all builds. For local development,
run `./scripts/optimize-wasm.sh -Oz` after building.

## Related Documentation

- [GHC WASM User Guide](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/wasm.html)
- [GHC WASM Meta](https://gitlab.haskell.org/ghc/ghc-wasm-meta) - Toolchain bootstrap
- [TypeScript Bridge](../ts-apps/jl4-web/src/lib/wasm/README.md) - Browser integration
