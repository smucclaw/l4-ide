// Single-eval worker — the wasm runtime + instance live here, the
// parent thread serializes requests over 'parentPort'. Letting one
// worker fall on its sword (trap, timeout, OOM) lets the parent
// terminate + respawn it without taking the HTTP listener down.
//
// Protocol:
//   ⇢ { type: "eval", id, fnName, args, traceMode, startTime?, events? }
//   ⇡ { type: "result", id, status, body, peakHeap, maxHeap, backend }
//
// 'startTime' and 'events' are only sent for deontic invocations
// (M6); the worker merges them into 'args' for the JS interpreter.
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
  DeonticInputError,
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
  const { id, fnName, args, traceMode, startTime, events } = msg;
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
  // M6 — refuse @startTime@ / @events@ payloads against a
  // non-deontic function so a typo / wrong-endpoint call doesn't
  // silently succeed. Match jl4-service's exact error wording so
  // proxy clients can parse it identically.
  if (!meta.isDeontic && (events !== undefined || startTime !== undefined)) {
    parentPort.postMessage({
      type: "result",
      id,
      status: 400,
      body: JSON.stringify({
        error:
          "startTime and events are only valid for functions returning DEONTIC",
      }),
      peakHeap: 0,
      maxHeap: rt.getMaxHeapBytes(),
      backend: backend.name,
    });
    return;
  }
  // For deontic functions the JS interpreter looks 'startTime' /
  // 'events' off the same args bag — merge them in here so the
  // runtime stays oblivious to the envelope shape.
  const effectiveArgs = meta.isDeontic ? { ...args, startTime, events } : args;
  let status = 200;
  let body;
  try {
    const payload =
      traceMode === "full"
        ? rt.invokeFunctionWithReasoning(instance, meta, effectiveArgs)
        : { value: rt.invokeFunction(instance, meta, effectiveArgs) };
    body = aesonStringify(wrapEvaluationEnvelope(payload));
  } catch (err) {
    // Map known errors to HTTP statuses; everything else is 500.
    // 'DeonticInputError' is a client-side input bug — the worker's
    // wasm state is untouched, so don't trigger a respawn.
    // 'MemoryLimitError' / wasm traps leave the bump-pointer or the
    // instance in inconsistent state and ARE fatal.
    const isFatal = !(err instanceof DeonticInputError);
    if (err instanceof DeonticInputError) {
      status = 400;
    } else if (err instanceof MemoryLimitError) {
      status = 413;
    } else {
      status = 500;
    }
    body = JSON.stringify({ error: String(err.message || err) });
    parentPort.postMessage({
      type: "result",
      id,
      status,
      body,
      peakHeap: rt.getPeakHeapBytes(),
      maxHeap: rt.getMaxHeapBytes(),
      backend: backend.name,
      fatal: isFatal,
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
