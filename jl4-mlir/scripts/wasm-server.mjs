// HTTP wrapper around a jl4-mlir-compiled .wasm bundle. Routes and
// response envelope mirror jl4-service's /deployments/<id>/functions/
// <fn>/evaluation endpoint so the two backends are drop-in swappable.
//
// Usage: node wasm-server.mjs <schema.json> <wasm> [port]
//
// M7 hardening — the wasm eval runs in a 'worker_thread'. The parent
// owns request routing, the timeout budget, and the
// terminate-and-respawn lifecycle; the worker owns the warm
// runtime + instance. A trap, OOM, or wall-clock timeout terminates
// the worker; the parent spawns a fresh one. The HTTP listener
// keeps serving regardless.
//
// Env knobs:
//   JL4_MAX_HEAP_BYTES   — per-eval memory ceiling (default 64 MiB)
//   JL4_EVAL_TIMEOUT_MS  — per-eval wall-clock budget (default 5000)

import fs from "node:fs";
import http from "node:http";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { Worker } from "node:worker_threads";
import { DEFAULT_MAX_HEAP_BYTES } from "../runtime/jl4-runtime.mjs";

const __dirname = path.dirname(fileURLToPath(import.meta.url));

const [, , schemaPath, wasmPath, portArg] = process.argv;
if (!schemaPath || !wasmPath) {
  console.error("usage: node wasm-server.mjs <schema.json> <wasm> [port]");
  process.exit(1);
}
const port = parseInt(portArg || "8766", 10);
const maxHeapBytes = process.env.JL4_MAX_HEAP_BYTES
  ? parseInt(process.env.JL4_MAX_HEAP_BYTES, 10)
  : DEFAULT_MAX_HEAP_BYTES;
const evalTimeoutMs = process.env.JL4_EVAL_TIMEOUT_MS
  ? parseInt(process.env.JL4_EVAL_TIMEOUT_MS, 10)
  : 5000;

// Resolve the schema/wasm paths now so the worker doesn't have to
// resolve relative to its own __dirname.
const schemaAbs = path.resolve(schemaPath);
const wasmAbs = path.resolve(wasmPath);
const workerPath = path.join(__dirname, "wasm-worker.mjs");

// ---- worker lifecycle -----------------------------------------------------
// Single warm worker, replaced on death. 'pending' tracks the in-flight
// eval so a 'message' event can resolve / a 'terminate' can reject.
let worker;
let pending = null; // { id, resolve, reject, timer }
let nextEvalId = 1;

async function spawnWorker() {
  return await new Promise((resolve, reject) => {
    const w = new Worker(workerPath, {
      workerData: { schemaPath: schemaAbs, wasmPath: wasmAbs, maxHeapBytes },
    });
    w.once("error", reject);
    w.once("exit", (code) => {
      // If a request was in flight, surface as a 500. The HTTP handler
      // already swapped 'worker' in 'evalInWorker'; clear 'pending' so
      // a stale resolver doesn't leak into the next request.
      if (pending) {
        const p = pending;
        pending = null;
        clearTimeout(p.timer);
        p.reject(new Error("worker exited (code=" + code + ") mid-eval"));
      }
    });
    w.on("message", (msg) => {
      if (msg.type === "ready") {
        worker = w;
        worker.on("message", onWorkerMessage);
        resolve();
      }
    });
  });
}

function onWorkerMessage(msg) {
  if (msg.type !== "result") return;
  if (!pending || pending.id !== msg.id) return;
  const p = pending;
  pending = null;
  clearTimeout(p.timer);
  p.resolve(msg);
  // A fatal failure (trap, OOM) leaves the worker's state poisoned.
  // Tear it down and spawn a fresh one before the next request
  // arrives — keeps the next caller from inheriting half-allocated
  // memory or an unusable instance.
  if (msg.fatal) {
    void terminateAndRespawn();
  }
}

async function terminateAndRespawn() {
  if (worker) {
    try {
      await worker.terminate();
    } catch {
      /* worker already gone */
    }
    worker = null;
  }
  await spawnWorker();
}

function evalInWorker(fnName, args, traceMode) {
  return new Promise((resolve, reject) => {
    if (!worker) {
      reject(new Error("worker not ready"));
      return;
    }
    if (pending) {
      reject(new Error("worker busy"));
      return;
    }
    const id = nextEvalId++;
    const timer = setTimeout(() => {
      if (pending && pending.id === id) {
        const p = pending;
        pending = null;
        // Terminate first so the next call doesn't try to use a
        // worker that's still grinding away on the timed-out eval.
        // The respawn races the reject; that's fine — the
        // 'worker = null' below blocks new evals until 'spawnWorker'
        // resolves.
        const dying = worker;
        worker = null;
        if (dying) dying.terminate().catch(() => {});
        spawnWorker().catch((e) =>
          console.error("respawn after timeout failed:", e),
        );
        p.reject(
          Object.assign(new Error("eval exceeded " + evalTimeoutMs + "ms"), {
            status: 504,
          }),
        );
      }
    }, evalTimeoutMs);
    pending = { id, resolve, reject, timer };
    worker.postMessage({ type: "eval", id, fnName, args, traceMode });
  });
}

await spawnWorker();

// ---- HTTP listener --------------------------------------------------------
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

  let body = "";
  req.on("data", (c) => (body += c));
  req.on("end", async () => {
    let parsed;
    try {
      parsed = body ? JSON.parse(body) : {};
    } catch (e) {
      res.writeHead(400, { "content-type": "application/json" });
      res.end(JSON.stringify({ error: "invalid JSON: " + e.message }));
      return;
    }
    const args = parsed.arguments || {};
    try {
      const result = await evalInWorker(fnName, args, traceMode);
      res.writeHead(result.status, {
        "content-type": "application/json",
        "x-jl4-peak-heap-bytes": String(result.peakHeap),
        "x-jl4-max-heap-bytes": String(result.maxHeap),
        "x-jl4-eval-timeout-ms": String(evalTimeoutMs),
      });
      res.end(result.body);
    } catch (err) {
      const status = err.status || 500;
      res.writeHead(status, {
        "content-type": "application/json",
        "x-jl4-eval-timeout-ms": String(evalTimeoutMs),
      });
      res.end(JSON.stringify({ error: String(err.message || err) }));
    }
  });
});
server.listen(port, "127.0.0.1", () => {
  console.error("wasm HTTP wrapper listening on http://127.0.0.1:" + port);
});
