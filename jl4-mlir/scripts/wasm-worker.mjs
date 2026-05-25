// Single-eval worker — the wasm runtime + instance live here, the
// parent thread serializes requests over 'parentPort'. Letting one
// worker fall on its sword (trap, timeout, OOM) lets the parent
// terminate + respawn it without taking the HTTP listener down.
//
// Protocol:
//   ⇢ { type: "eval", id, fnName, args, traceMode }
//   ⇡ { type: "result", id, status, body, peakHeap, maxHeap, backend }
//
// The parent owns timeout enforcement (the worker has no clean way
// to interrupt itself mid-call); a timed-out request just leaves
// this worker terminated and the parent spawns a fresh one.
//
// Worker arguments (passed via 'workerData'):
//   schemaPath   — absolute path to the bundle's .schema.json
//   wasmPath     — absolute path to the .wasm
//   maxHeapBytes — per-eval memory ceiling
//   backend      — wasm runtime name ('v8' default; arbitrary string
//                  resolves to an ES-module specifier — see
//                  'runtime/wasm-backend.mjs')

import fs from "node:fs";
import { parentPort, workerData } from "node:worker_threads";
import {
  createRuntime,
  aesonStringify,
  wrapEvaluationEnvelope,
  MemoryLimitError,
} from "../runtime/jl4-runtime.mjs";
import { createBackend } from "../runtime/wasm-backend.mjs";

if (!parentPort) {
  throw new Error("wasm-worker must be spawned via Worker, not run directly");
}

const { schemaPath, wasmPath, maxHeapBytes, backend: backendName } = workerData;
const schema = JSON.parse(fs.readFileSync(schemaPath, "utf8"));
const wasmBuf = fs.readFileSync(wasmPath);

// Single warm instance for this worker's lifetime. A trap or any
// other failure terminates the worker, so we never need to
// re-instantiate inside it.
const rt = createRuntime({ maxHeapBytes });
const backend = await createBackend(backendName);
const wasmModule = await backend.compile(wasmBuf);
const { instance } = await backend.instantiate(wasmModule, rt.makeImports());
rt.attachMemory(instance.exports.memory);
rt.setBundleFunctions(schema.functions || {});

// Build name → schema lookup using both sanitized (hyphen) and spaced
// function names so either routing convention works.
const fnByName = {};
for (const [sanitized, meta] of Object.entries(schema.functions)) {
  fnByName[sanitized] = meta;
  fnByName[sanitized.replace(/-/g, " ")] = meta;
}

parentPort.postMessage({ type: "ready", backend: backend.name });

parentPort.on("message", (msg) => {
  if (msg.type !== "eval") return;
  const { id, fnName, args, traceMode } = msg;
  const meta = fnByName[fnName] || fnByName[fnName.replace(/ /g, "-")];
  if (!meta) {
    parentPort.postMessage({
      type: "result",
      id,
      status: 404,
      body: JSON.stringify({ error: "Unknown function: " + fnName }),
      peakHeap: 0,
      maxHeap: rt.getMaxHeapBytes(),
    });
    return;
  }
  let status = 200;
  let body;
  try {
    const payload =
      traceMode === "full"
        ? rt.invokeFunctionWithReasoning(instance, meta, args)
        : { value: rt.invokeFunction(instance, meta, args) };
    body = aesonStringify(wrapEvaluationEnvelope(payload));
  } catch (err) {
    status = err instanceof MemoryLimitError ? 413 : 500;
    body = JSON.stringify({ error: String(err.message || err) });
    // Tell the parent we're toast — a WebAssembly.RuntimeError leaves
    // the instance unusable, and MemoryLimitError leaves heap-state
    // inconsistent enough that the next call would either re-trip the
    // limit or leak. Either way, parent should kill+respawn.
    parentPort.postMessage({
      type: "result",
      id,
      status,
      body,
      peakHeap: rt.getPeakHeapBytes(),
      maxHeap: rt.getMaxHeapBytes(),
      backend: backend.name,
      fatal: true,
    });
    return;
  }
  parentPort.postMessage({
    type: "result",
    id,
    status,
    body,
    peakHeap: rt.getPeakHeapBytes(),
    maxHeap: rt.getMaxHeapBytes(),
    backend: backend.name,
  });
});
