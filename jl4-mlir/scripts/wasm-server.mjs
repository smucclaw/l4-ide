// HTTP wrapper around a jl4-mlir-compiled .wasm bundle. Routes and
// response envelope mirror jl4-service's /deployments/<id>/functions/
// <fn>/evaluation endpoint so the two backends are drop-in swappable.
//
// Usage: node wasm-server.mjs <schema.json> <wasm> [port]
//
// M7 hardening — a faulty rule (wasm trap, memory blow-up, exception
// from the runtime) returns a 500 *and* re-instantiates the module so
// subsequent requests start from a clean state. WebAssembly traps
// leave the instance unusable per spec, and the JS-side bump-pointer
// heap is per-runtime; without re-instantiation the next request
// would either reuse the leaked state or trap again on the same
// memory views.

import fs from "node:fs";
import http from "node:http";
import {
  createRuntime,
  aesonStringify,
  wrapEvaluationEnvelope,
  DEFAULT_MAX_HEAP_BYTES,
  MemoryLimitError,
} from "../runtime/jl4-runtime.mjs";

const [, , schemaPath, wasmPath, portArg] = process.argv;
if (!schemaPath || !wasmPath) {
  console.error("usage: node wasm-server.mjs <schema.json> <wasm> [port]");
  process.exit(1);
}
const port = parseInt(portArg || "8766", 10);
const maxHeapBytes = process.env.JL4_MAX_HEAP_BYTES
  ? parseInt(process.env.JL4_MAX_HEAP_BYTES, 10)
  : DEFAULT_MAX_HEAP_BYTES;

const schema = JSON.parse(fs.readFileSync(schemaPath, "utf8"));
const wasmBuf = fs.readFileSync(wasmPath);

// Single warm instantiation per worker. 'reinstantiate' rebuilds it
// after a failed eval — same module, fresh memory, fresh runtime
// state. The wasm bytes get cached in 'wasmModule' so we only pay
// the compile cost once.
const wasmModule = await WebAssembly.compile(wasmBuf);
let rt, instance;

async function reinstantiate() {
  rt = createRuntime({ maxHeapBytes });
  instance = await WebAssembly.instantiate(wasmModule, rt.makeImports());
  rt.attachMemory(instance.exports.memory);
  // M5 slice 4A — give the runtime the full schema so nested `<fn>$trace`
  // calls can resolve node IDs against the called function's nodes table.
  rt.setBundleFunctions(schema.functions || {});
}

await reinstantiate();

// Build name → schema lookup using both sanitized (hyphen) and spaced
// function names so either routing convention works.
const fnByName = {};
for (const [sanitized, meta] of Object.entries(schema.functions)) {
  fnByName[sanitized] = meta;
  fnByName[sanitized.replace(/-/g, " ")] = meta;
}

const server = http.createServer((req, res) => {
  if (req.method !== "POST") {
    res.writeHead(405);
    res.end();
    return;
  }
  const m = req.url.match(
    /\/deployments\/[^/]+\/functions\/([^/]+)\/evaluation(?:\?(.*))?$/,
  );
  if (!m) {
    res.writeHead(404);
    res.end("not found");
    return;
  }
  const fnName = decodeURIComponent(m[1]);
  const query = new URLSearchParams(m[2] || "");
  // jl4-service accepts both the X-L4-Trace header and the ?trace= query
  // param; mirror just the query for slice 1 (the harness uses the query).
  const traceMode = query.get("trace") || "none";
  const meta = fnByName[fnName] || fnByName[fnName.replace(/ /g, "-")];
  if (!meta) {
    res.writeHead(404, { "content-type": "application/json" });
    res.end(JSON.stringify({ error: "Unknown function: " + fnName }));
    return;
  }
  let body = "";
  req.on("data", (c) => (body += c));
  req.on("end", async () => {
    let trapped = false;
    let status = 200;
    let respBody;
    try {
      const parsed = body ? JSON.parse(body) : {};
      const args_ = parsed.arguments || {};
      const payload =
        traceMode === "full"
          ? rt.invokeFunctionWithReasoning(instance, meta, args_)
          : { value: rt.invokeFunction(instance, meta, args_) };
      // aesonStringify matches jl4-service's Aeson-encoded output byte-for-
      // byte: bytestring's `doubleDec` for fractional NUMBER results, and
      // alphabetically-sorted object keys for nested Reasoning trees.
      respBody = aesonStringify(wrapEvaluationEnvelope(payload));
    } catch (err) {
      // 413 for OOM (caller's payload-too-large guess is closer than
      // 500), 500 for everything else. A 'WebAssembly.RuntimeError'
      // (trap) leaves the instance unusable per spec — re-instantiate
      // after the response so the next request starts clean.
      status = err instanceof MemoryLimitError ? 413 : 500;
      trapped = err instanceof WebAssembly.RuntimeError || status === 413;
      respBody = JSON.stringify({ error: String(err.message || err) });
    }
    const peak = rt.getPeakHeapBytes();
    res.writeHead(status, {
      "content-type": "application/json",
      "x-jl4-peak-heap-bytes": String(peak),
      "x-jl4-max-heap-bytes": String(rt.getMaxHeapBytes()),
    });
    res.end(respBody);
    if (trapped) {
      try {
        await reinstantiate();
      } catch (e) {
        console.error("re-instantiation failed:", e);
        process.exit(1);
      }
    }
  });
});
server.listen(port, "127.0.0.1", () => {
  console.error("wasm HTTP wrapper listening on http://127.0.0.1:" + port);
});
