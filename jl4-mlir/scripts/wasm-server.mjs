// HTTP wrapper around a jl4-mlir-compiled .wasm bundle. Routes and
// response envelope mirror jl4-service's /deployments/<id>/functions/
// <fn>/evaluation endpoint so the two backends are drop-in swappable.
//
// Usage: node wasm-server.mjs <schema.json> <wasm> [port]

import fs from "node:fs";
import http from "node:http";
import {
  createRuntime,
  aesonStringify,
  wrapEvaluationEnvelope,
} from "../runtime/jl4-runtime.mjs";

const [, , schemaPath, wasmPath, portArg] = process.argv;
if (!schemaPath || !wasmPath) {
  console.error("usage: node wasm-server.mjs <schema.json> <wasm> [port]");
  process.exit(1);
}
const port = parseInt(portArg || "8766", 10);

const schema = JSON.parse(fs.readFileSync(schemaPath, "utf8"));
const wasmBuf = fs.readFileSync(wasmPath);

const rt = createRuntime();
const { instance } = await WebAssembly.instantiate(wasmBuf, rt.makeImports());
rt.attachMemory(instance.exports.memory);
// M5 slice 4A — give the runtime the full schema so nested `<fn>$trace`
// calls can resolve node IDs against the called function's nodes table.
rt.setBundleFunctions(schema.functions || {});

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
  req.on("end", () => {
    try {
      const parsed = body ? JSON.parse(body) : {};
      const args_ = parsed.arguments || {};
      const payload =
        traceMode === "full"
          ? rt.invokeFunctionWithReasoning(instance, meta, args_)
          : { value: rt.invokeFunction(instance, meta, args_) };
      res.writeHead(200, { "content-type": "application/json" });
      // aesonStringify matches jl4-service's Aeson-encoded output byte-for-
      // byte: bytestring's `doubleDec` for fractional NUMBER results, and
      // alphabetically-sorted object keys for nested Reasoning trees.
      res.end(aesonStringify(wrapEvaluationEnvelope(payload)));
    } catch (err) {
      res.writeHead(500, { "content-type": "application/json" });
      res.end(JSON.stringify({ error: String(err.message || err) }));
    }
  });
});
server.listen(port, "127.0.0.1", () => {
  console.error("wasm HTTP wrapper listening on http://127.0.0.1:" + port);
});
