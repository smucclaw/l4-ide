# jl4-mlir

MLIR / WebAssembly compiler backend for L4.

Takes a typechecked L4 program (`Module Resolved` straight out of `jl4-core`) and lowers it through MLIR's standard dialects — `func`, `arith`, `scf`, `cf`, `memref`, `llvm` — down to a `.wasm` binary that runs in Node, browsers, or any wasm runtime. Alongside the binary it emits a `.schema.json` sidecar that is wire-compatible with [`jl4-service`](../jl4-service/)'s `FunctionSchema`, so a compiled `.wasm` can slot in anywhere the HTTP service already speaks.

```
test.l4  ──►  MLIR (textual IR)
                │  mlir-opt      (convert-scf-to-cf, *-to-llvm, ...)
                ▼
             LLVM dialect
                │  mlir-translate --mlir-to-llvmir
                ▼
             LLVM IR
                │  llc -march=wasm32 -filetype=obj
                ▼
             wasm32 object
                │  wasm-ld --no-entry --export-dynamic
                ▼
             test.wasm  +  test.schema.json
```

## Why

Running rules via the HTTP evaluator is convenient but slow; on the auth-proxy `test.l4` fixture each call costs ~160 ms of Haskell CPU. Compiling the same rules to wasm drops per-call cost to tens of microseconds and lets the rules execute anywhere — edge workers, in-browser agents, embedded in other services — without Haskell on the host. See [Performance](#performance).

## Build

```bash
# one-time toolchain setup (macOS)
brew install llvm
export PATH="/opt/homebrew/opt/llvm/bin:$PATH"

cabal build jl4-mlir
```

The runtime toolchain (`mlir-opt`, `mlir-translate`, `llc`, `wasm-ld`) must be on `PATH`. Any recent LLVM/MLIR works; the package has been used with the Homebrew `llvm` and `llvm@22` formulas.

## CLI

```bash
jl4-mlir wasm <file.l4> [-o out.wasm] [-O 0..3] [-v]  # full pipeline
jl4-mlir wasm <file.l4> --mlir-only                   # stop after .mlir
jl4-mlir wasm <file.l4> --llvm-only                   # stop after .ll

jl4-mlir list  <file.wasm>                            # show exports + signatures
jl4-mlir run   <file.wasm> -f <fn-name>               # invoke; JSON in on stdin
```

Example:

```bash
jl4-mlir wasm validation/test.l4 -o /tmp/test.wasm
jl4-mlir list /tmp/test.wasm
echo '{"arguments":{"years of service":5,"performance rating":4}}' \
  | jl4-mlir run /tmp/test.wasm --function is-eligible
# {"tag":"SimpleResponse","contents":{"result":{"value":true}}}
```

The input and output wire format is byte-identical to [`jl4-service`](../jl4-service/)'s `POST /deployments/<id>/functions/<fn>/evaluation` endpoint, so the same client code works against either backend.

## Architecture

| Module                                                        | Role                                                                                                                                                                                      |
| ------------------------------------------------------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| [`L4.MLIR.Lower`](src/L4/MLIR/Lower.hs)                       | `Module Resolved → MLIRModule`. Lambda-lifts WHERE / LET IN bindings with proper closure conversion, mangles overloaded names by arity, synthesizes externs for unresolved prelude calls. |
| [`L4.MLIR.Emit`](src/L4/MLIR/Emit.hs)                         | MLIR textual IR pretty-printer.                                                                                                                                                           |
| [`L4.MLIR.Dialect.*`](src/L4/MLIR/Dialect/)                   | Op constructors for `func`, `arith`, `scf`, `cf`, `memref`, `llvm`.                                                                                                                       |
| [`L4.MLIR.Runtime.Builtins`](src/L4/MLIR/Runtime/Builtins.hs) | Runtime imports (`__l4_min`, `__l4_list_count`, `__l4_alloc`, ...) declared as externs; resolved at wasm load time.                                                                       |
| [`L4.MLIR.Schema`](src/L4/MLIR/Schema.hs)                     | Emits the `.schema.json` sidecar — same shape as `jl4-service`'s `FunctionSchema.hs`.                                                                                                     |
| [`L4.MLIR.Marshal`](src/L4/MLIR/Marshal.hs)                   | JSON request / response types (`FnArguments`, `SimpleResponse`, etc.) that mirror `Backend.Api` verbatim.                                                                                 |
| [`L4.MLIR.Pipeline`](src/L4/MLIR/Pipeline.hs)                 | Drives `mlir-opt` → `mlir-translate` → `llc` → `wasm-ld`.                                                                                                                                 |

### ABI: uniform f64

Every L4 value crosses function boundaries as a single f64. Numbers and booleans use the value numerically (`0.0` / `1.0` for bool). Pointers (strings, records, lists, MAYBE) are bit-cast through an i64, so the f64 payload _is_ the pointer. In linear memory a slot is always 8 bytes — read it as f64, and `bitcast f64 → i64` recovers the original integer unchanged.

One consequence: when calling wasm from JavaScript, pointer-typed arguments must be passed as the f64 _bit pattern_ of the u64 pointer (not as a plain JS number, which would be reinterpreted as the IEEE 754 encoding of that integer). The Node runtime generated by `jl4-mlir run` handles this with an 8-byte scratch `DataView`; see [`app/Main.hs`](app/Main.hs) `generateNodeRunner`.

### Memory layout

| L4 type                       | Representation                                                              |
| ----------------------------- | --------------------------------------------------------------------------- |
| `NUMBER`, `BOOLEAN`           | Plain f64                                                                   |
| `STRING`                      | Pointer to NUL-terminated UTF-8 bytes                                       |
| record `R HAS a IS T, b IS U` | Pointer to contiguous 8-byte slots, one per field in declaration order      |
| `LIST OF T`                   | Null-terminated linked list of 16-byte nodes (slot 0 = head, slot 1 = tail) |
| `MAYBE T`                     | Pointer to a 2-slot record (slot 0 = tag `0.0`/`1.0`, slot 1 = payload)     |
| `DECLARE D IS ONE OF A, B, C` | Numeric enum tag as f64 (`0.0`, `1.0`, ...)                                 |

Optional record fields (those absent from the schema's `required` list) are always emitted as `MAYBE` records.

## Scope & limitations

This is a working compiler, not a finished one. Out of 431 `.l4` files in this repository, the batch script compiles 379 to either a `.wasm` binary or a valid MLIR module; the remaining 52 fail at jl4-core's typecheck stage, not in the MLIR backend.

Known gaps:

- String operations (`__l4_str_concat`, `__l4_str_len`, `__l4_to_string`) are stubbed at runtime — good for passing strings through, not for heavy string work.
- No DEONTIC evaluator — trace evaluation still lives in `jl4-service`.
- No state graphs, no GraphViz, no reasoning traces in the response — the envelope is `SimpleResponse` only.
- Only the Node.js wasm runtime is wired end-to-end for marshaling. `--wasmtime` / `--wasmer` flags exist but are not yet connected to the schema-driven marshaler.

## Performance

Benchmark on the [auth-proxy](https://github.com/legalese/jl4-auth-proxy) `validation/test.l4` fixture — 12 exported functions, 100 calls each = 1,200 calls total. Both backends serve HTTP on loopback, accept the same `{arguments: ...}` body, and return the same `SimpleResponse` envelope.

| Backend                                       |  Wall time | Per-call avg | Server RSS |
| --------------------------------------------- | ---------: | -----------: | ---------: |
| `jl4-service` (Haskell evaluator)             |  **159 s** |       132 ms |     127 MB |
| `jl4-mlir` (Node HTTP wrapper around `.wasm`) | **173 ms** |      0.14 ms |     ~75 MB |

Across non-trivial functions (anything taking a record input), the wasm path runs between 1,300× and 2,000× faster per call and uses ~40% less server memory. Tiny scalar-only functions (`is-eligible`, `calculate-bonus`) are a wash because per-request overhead dominates on both sides.

Output correctness was verified on all 12 exports: 11 return JSON byte-identical to the service, 1 (`order-total`) differs by a single ULP due to floating-point accumulation order.

## Scripts

```bash
jl4-mlir/scripts/batch-compile.sh     # scan every .l4 in the repo and report compile status
```

## See also

- [jl4-core](../jl4-core/) — parser, typechecker, evaluator
- [jl4-service](../jl4-service/) — HTTP/MCP decision service (the reference wire format)
- [jl4-wasm](../jl4-wasm/) — unrelated: a WebAssembly build of the _L4 compiler itself_ for in-browser editing
