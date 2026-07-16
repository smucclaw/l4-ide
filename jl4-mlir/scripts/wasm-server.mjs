// HTTP wrapper around a jl4-mlir-compiled .wasm bundle. Routes and
// response envelope mirror jl4-service's /deployments/<id>/functions/
// <fn>/evaluation endpoint so the two backends are drop-in swappable.
//
// Usage: node wasm-server.mjs <schema.json> <wasm> [port]
//
// M7 hardening — the wasm eval runs in a pool of 'worker_threads'. The
// parent owns request routing, the timeout budget, and the
// terminate-and-respawn lifecycle; each worker owns one warm
// runtime + instance pair. A trap, OOM, or wall-clock timeout
// terminates the offending worker; the parent spawns a fresh one.
// Other in-flight requests on other workers are unaffected.
//
// Env knobs:
//   JL4_MAX_HEAP_BYTES    — per-eval memory ceiling (default 64 MiB)
//   JL4_EVAL_TIMEOUT_MS   — per-eval wall-clock budget (default 5000)
//   JL4_WORKER_POOL_SIZE  — number of warm workers (default 4)
//   JL4_BACKEND           — wasm runtime backend ('v8' default; any
//                            other string resolves to an ES-module
//                            specifier exporting a {compile,
//                            instantiate} record — see
//                            'runtime/wasm-backend.mjs')

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
const poolSize = process.env.JL4_WORKER_POOL_SIZE
  ? Math.max(1, parseInt(process.env.JL4_WORKER_POOL_SIZE, 10))
  : 4;
const backendName = process.env.JL4_BACKEND || "v8";

// Resolve the schema/wasm paths now so the worker doesn't have to
// resolve relative to its own __dirname.
const schemaAbs = path.resolve(schemaPath);
const wasmAbs = path.resolve(wasmPath);
const workerPath = path.join(__dirname, "wasm-worker.mjs");

// ---- WorkerPool -----------------------------------------------------------
// Each slot is { worker, busy, pending }. 'busy' is true while a request
// is in flight; 'pending' is its { id, resolve, reject, timer } record.
// 'queue' is a FIFO of payloads waiting for an idle slot.
function createWorkerPool(size) {
  const slots = new Array(size).fill(null).map(() => ({
    worker: null,
    busy: false,
    pending: null,
  }));
  const queue = [];
  let nextEvalId = 1;
  let shuttingDown = false;

  function dispatchFromQueue() {
    if (shuttingDown) return;
    while (queue.length > 0) {
      const slot = slots.find((s) => s.worker && !s.busy);
      if (!slot) return;
      const job = queue.shift();
      runOnSlot(slot, job);
    }
  }

  function runOnSlot(slot, job) {
    const id = nextEvalId++;
    slot.busy = true;
    const timer = setTimeout(() => {
      // Timeout: the worker may still be busy on the timed-out eval, so
      // terminate it. The slot's lifecycle handler will spawn a fresh
      // worker; we just have to settle the pending promise and free up
      // the queue for the next request.
      if (!slot.pending || slot.pending.id !== id) return;
      const p = slot.pending;
      slot.pending = null;
      const dying = slot.worker;
      slot.worker = null;
      slot.busy = false;
      if (dying) dying.terminate().catch(() => {});
      spawnIntoSlot(slot).catch((e) =>
        console.error("respawn after timeout failed:", e),
      );
      p.reject(
        Object.assign(new Error("eval exceeded " + evalTimeoutMs + "ms"), {
          status: 504,
        }),
      );
    }, evalTimeoutMs);
    slot.pending = { id, resolve: job.resolve, reject: job.reject, timer };
    slot.worker.postMessage({ type: "eval", id, ...job.payload });
  }

  async function spawnIntoSlot(slot) {
    return await new Promise((resolve, reject) => {
      const w = new Worker(workerPath, {
        workerData: {
          schemaPath: schemaAbs,
          wasmPath: wasmAbs,
          maxHeapBytes,
          backend: backendName,
        },
      });
      w.once("error", reject);
      w.once("exit", (code) => {
        // Surface as a 500 to any in-flight request the parent didn't
        // already settle (e.g. an unclean exit during a non-timed-out
        // eval). The slot frees up either way.
        if (slot.pending) {
          const p = slot.pending;
          slot.pending = null;
          clearTimeout(p.timer);
          p.reject(new Error("worker exited (code=" + code + ") mid-eval"));
        }
        if (slot.worker === w) {
          slot.worker = null;
          slot.busy = false;
          // Auto-respawn so the pool stays at full size. Unless the
          // server is shutting down, in which case let it drain.
          if (!shuttingDown) {
            spawnIntoSlot(slot)
              .then(dispatchFromQueue)
              .catch((e) => console.error("auto-respawn failed:", e));
          }
        }
      });
      w.on("message", (msg) => {
        if (msg.type === "ready") {
          slot.worker = w;
          slot.busy = false;
          resolve();
          return;
        }
        if (msg.type !== "result") return;
        if (!slot.pending || slot.pending.id !== msg.id) return;
        const p = slot.pending;
        slot.pending = null;
        slot.busy = false;
        clearTimeout(p.timer);
        p.resolve(msg);
        // Fatal result (trap / OOM) — terminate the worker; the 'exit'
        // handler above will auto-respawn into the same slot.
        if (msg.fatal) {
          slot.worker = null;
          w.terminate().catch(() => {});
        } else {
          // Slot's free again — drain anything that was queued.
          dispatchFromQueue();
        }
      });
    });
  }

  async function start() {
    await Promise.all(slots.map((s) => spawnIntoSlot(s)));
  }

  function exec(payload) {
    return new Promise((resolve, reject) => {
      const slot = slots.find((s) => s.worker && !s.busy);
      if (slot) {
        runOnSlot(slot, { payload, resolve, reject });
      } else {
        queue.push({ payload, resolve, reject });
      }
    });
  }

  async function shutdown() {
    shuttingDown = true;
    await Promise.all(
      slots.map(async (s) => {
        if (s.worker) {
          try {
            await s.worker.terminate();
          } catch {
            /* worker already gone */
          }
        }
      }),
    );
  }

  function stats() {
    return {
      size,
      idle: slots.filter((s) => s.worker && !s.busy).length,
      busy: slots.filter((s) => s.busy).length,
      queued: queue.length,
    };
  }

  return { start, exec, shutdown, stats };
}

const pool = createWorkerPool(poolSize);
await pool.start();

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
    // M6 — deontic functions take @startTime@ + @events@ at the top
    // level of the request envelope (mirrors jl4-service's API). The
    // worker decides whether to thread them through based on the
    // looked-up function's 'isDeontic' flag.
    const startTime = parsed.startTime;
    const events = parsed.events;
    try {
      const result = await pool.exec({
        fnName,
        args,
        traceMode,
        startTime,
        events,
      });
      const stats = pool.stats();
      res.writeHead(result.status, {
        "content-type": "application/json",
        "x-jl4-peak-heap-bytes": String(result.peakHeap),
        "x-jl4-max-heap-bytes": String(result.maxHeap),
        "x-jl4-eval-timeout-ms": String(evalTimeoutMs),
        "x-jl4-pool-size": String(stats.size),
        "x-jl4-pool-queued": String(stats.queued),
        "x-jl4-backend": String(result.backend || backendName),
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
  console.error(
    "wasm HTTP wrapper listening on http://127.0.0.1:" +
      port +
      " (backend " +
      backendName +
      ", pool size " +
      poolSize +
      ", timeout " +
      evalTimeoutMs +
      "ms)",
  );
});

for (const sig of ["SIGTERM", "SIGINT"]) {
  process.once(sig, async () => {
    server.close();
    await pool.shutdown();
    process.exit(0);
  });
}
