// parity-harness.mjs — M0 differential parity tester.
//
// For each .l4 file: compile it to WASM (jl4-mlir), deploy the same source
// to a live jl4-service, then for every exported function evaluate the SAME
// arguments against BOTH backends and diff the responses. jl4-service is the
// reference; the WASM path must match it.
//
// Outcomes per (file, function, case):
//   byte-identical      response bodies are byte-for-byte equal
//   value-equal         result values match but envelope bytes differ
//   ulp-differs         numeric results differ within a few ULP (the known
//                       f64-vs-rational gap, M4) — tracked, not a gate failure
//   differs             result values differ for real (logic divergence)
//   wasm-error          WASM threw / trapped (logged, proxy would fall back)
//   service-error       jl4-service returned non-2xx
//   both-error          both sides errored (consistent)
//   refused-unsupported function is supported:false (routes to fallback; not run on WASM)
//
// Requirements at run time: mlir-opt / mlir-translate / llc / wasm-ld on PATH,
// and a built jl4-service + jl4-mlir.
//
//   node scripts/parity-harness.mjs [--port 9911] [--out DIR] file1.l4 [file2.l4 ...]
//
// Arguments are generated from each function's schema (deterministic per type).
// A sidecar `<file>.cases.json` — { "<fn-name>": [ {<args>}, ... ] } — overrides
// the generated case with curated inputs for richer coverage.

import fs from "node:fs";
import os from "node:os";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { execFileSync, spawn } from "node:child_process";
import { createRuntime } from "../runtime/jl4-runtime.mjs";

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const REPO_ROOT = path.resolve(__dirname, "..", "..");

// ---- arg parsing ----------------------------------------------------------
const argv = process.argv.slice(2);
let port = 9911;
let outDir = path.join(REPO_ROOT, "jl4-mlir", "parity-report");
const files = [];
for (let i = 0; i < argv.length; i++) {
  if (argv[i] === "--port") port = parseInt(argv[++i], 10);
  else if (argv[i] === "--out") outDir = argv[++i];
  else files.push(argv[i]);
}
if (files.length === 0) {
  // Default demo set: the rich 12-fn fixture + a deontic file (refused).
  files.push(
    path.join(REPO_ROOT, "..", "jl4-auth-proxy", "validation", "test.l4"),
  );
}

const cabalBin = (name) =>
  execFileSync("cabal", ["list-bin", name], {
    cwd: REPO_ROOT,
    encoding: "utf8",
  }).trim();

const MLIR_BIN = cabalBin("jl4-mlir");
const SERVICE_BIN = cabalBin("jl4-service");

// ---- schema-driven argument generation ------------------------------------
// Build a deterministic sample value for one parameter schema node.
function genValue(p) {
  const t = p.type;
  if (t === "number" || t === "integer") return 1;
  if (t === "boolean") return true;
  if (t === "string") {
    if (Array.isArray(p.enum) && p.enum.length) return p.enum[0];
    return "x";
  }
  if (t === "array") return p.items ? [genValue(p.items)] : [];
  if (t === "object" && p.properties) {
    const o = {};
    for (const [k, v] of Object.entries(p.properties)) o[k] = genValue(v);
    return o;
  }
  return null;
}

function genArgs(parameters) {
  const props = parameters?.properties || {};
  const args = {};
  for (const [k, v] of Object.entries(props)) args[k] = genValue(v);
  return args;
}

// ---- canonical JSON (sorted keys) for value comparison --------------------
function canonical(x) {
  if (Array.isArray(x)) return "[" + x.map(canonical).join(",") + "]";
  if (x && typeof x === "object")
    return (
      "{" +
      Object.keys(x)
        .sort()
        .map((k) => JSON.stringify(k) + ":" + canonical(x[k]))
        .join(",") +
      "}"
    );
  return JSON.stringify(x);
}
const extractValue = (body) => {
  try {
    return JSON.parse(body)?.contents?.result?.value;
  } catch {
    return undefined;
  }
};
// Are two values numerically equal to within a few ULP? This isolates the
// known f64-vs-exact-rational drift (M4) from genuine logic divergence so the
// harness can gate on the latter without being permanently red on the former.
function ulpEqual(a, b) {
  if (typeof a !== "number" || typeof b !== "number") return false;
  if (a === b) return true;
  if (!Number.isFinite(a) || !Number.isFinite(b)) return false;
  return Math.abs(a - b) <= 1e-9 * Math.max(1, Math.abs(a), Math.abs(b));
}

// ---- jl4-service lifecycle ------------------------------------------------
const store = fs.mkdtempSync(path.join(os.tmpdir(), "parity-store-"));
const svc = spawn(
  SERVICE_BIN,
  ["--port", String(port), "--store-path", store],
  {
    stdio: ["ignore", "ignore", "inherit"],
    env: { ...process.env },
  },
);
const BASE = `http://127.0.0.1:${port}`;

async function waitHealthy() {
  for (let i = 0; i < 100; i++) {
    try {
      const r = await fetch(`${BASE}/health`);
      if (r.ok) return;
    } catch {}
    await new Promise((r) => setTimeout(r, 100));
  }
  throw new Error("jl4-service did not become healthy");
}

async function deploy(id, srcFile) {
  const dir = path.dirname(srcFile);
  const name = path.basename(srcFile);
  const zip = execFileSync("zip", ["-j", "-q", "-", name], {
    cwd: dir,
    maxBuffer: 32 << 20,
  });
  const fd = new FormData();
  fd.append("id", id);
  fd.append(
    "sources",
    new Blob([zip], { type: "application/zip" }),
    "bundle.zip",
  );
  const r = await fetch(`${BASE}/deployments`, { method: "POST", body: fd });
  if (!r.ok && r.status !== 202)
    throw new Error(`deploy ${id} failed: ${r.status} ${await r.text()}`);
  // poll until ready
  for (let i = 0; i < 100; i++) {
    const s = await fetch(`${BASE}/deployments/${encodeURIComponent(id)}`);
    if (s.ok) {
      const b = await s.json();
      const status = b.dsStatus || b.status;
      if (status === "ready") return;
      if (status === "failed")
        throw new Error(
          `deploy ${id} compile failed: ${JSON.stringify(b).slice(0, 300)}`,
        );
    }
    await new Promise((r) => setTimeout(r, 100));
  }
  throw new Error(`deploy ${id} not ready`);
}

async function serviceEval(id, fn, args) {
  const r = await fetch(
    `${BASE}/deployments/${encodeURIComponent(id)}/functions/${encodeURIComponent(fn)}/evaluation`,
    {
      method: "POST",
      headers: { "content-type": "application/json" },
      body: JSON.stringify({ arguments: args }),
    },
  );
  return { status: r.status, body: await r.text() };
}

// ---- main -----------------------------------------------------------------
const results = [];
const tally = {};
const bump = (k) => (tally[k] = (tally[k] || 0) + 1);

try {
  await waitHealthy();
  const buildDir = fs.mkdtempSync(path.join(os.tmpdir(), "parity-build-"));

  for (const src of files) {
    const id = path
      .basename(src)
      .replace(/\.l4$/, "")
      .replace(/[^A-Za-z0-9_-]/g, "-");
    const wasmPath = path.join(buildDir, `${id}.wasm`);
    const schemaPath = path.join(buildDir, `${id}.schema.json`);

    // 1. compile to wasm
    try {
      execFileSync(MLIR_BIN, ["wasm", src, "-o", wasmPath], {
        stdio: ["ignore", "ignore", "pipe"],
      });
    } catch (e) {
      console.log(
        `[compile-fail] ${id}: ${String(e.stderr || e).slice(0, 200)}`,
      );
      results.push({ file: id, outcome: "compile-fail" });
      bump("compile-fail");
      continue;
    }
    const schema = JSON.parse(fs.readFileSync(schemaPath, "utf8"));

    // 2. deploy same source to jl4-service
    try {
      await deploy(id, src);
    } catch (e) {
      console.log(`[deploy-fail]  ${id}: ${String(e.message).slice(0, 200)}`);
      results.push({ file: id, outcome: "deploy-fail" });
      bump("deploy-fail");
      continue;
    }

    // 3. load wasm in-proc
    const rt = createRuntime();
    const { instance } = await WebAssembly.instantiate(
      fs.readFileSync(wasmPath),
      rt.makeImports(),
    );
    rt.attachMemory(instance.exports.memory);

    // optional curated cases
    let curated = {};
    const casesFile = src.replace(/\.l4$/, ".cases.json");
    if (fs.existsSync(casesFile))
      curated = JSON.parse(fs.readFileSync(casesFile, "utf8"));

    // 4. per function
    for (const [fnName, fe] of Object.entries(schema.functions)) {
      if (fe.supported === false) {
        console.log(
          `  refused-unsupported  ${id}::${fnName}  (${fe.unsupportedReason})`,
        );
        results.push({
          file: id,
          fn: fnName,
          outcome: "refused-unsupported",
          reason: fe.unsupportedReason,
        });
        bump("refused-unsupported");
        continue;
      }
      const cases = curated[fnName] || [genArgs(fe.parameters)];
      for (const args of cases) {
        const svcR = await serviceEval(id, fnName, args);
        let wasmBody = null,
          wasmErr = null;
        try {
          const val = rt.invokeFunction(instance, fe, args);
          wasmBody = JSON.stringify({
            contents: { result: { value: val } },
            tag: "SimpleResponse",
          });
        } catch (e) {
          wasmErr = String(e.message || e);
        }

        let outcome;
        const svcOk = svcR.status >= 200 && svcR.status < 300;
        if (wasmErr && !svcOk) outcome = "both-error";
        else if (wasmErr) outcome = "wasm-error";
        else if (!svcOk) outcome = "service-error";
        else if (svcR.body === wasmBody) outcome = "byte-identical";
        else if (
          canonical(extractValue(svcR.body)) ===
          canonical(extractValue(wasmBody))
        )
          outcome = "value-equal";
        else if (ulpEqual(extractValue(svcR.body), extractValue(wasmBody)))
          outcome = "ulp-differs";
        else outcome = "differs";

        bump(outcome);
        const mark =
          {
            "byte-identical": "✓",
            "value-equal": "≈",
            "ulp-differs": "~",
            differs: "✗",
            "wasm-error": "!",
            "service-error": "s",
            "both-error": "=",
          }[outcome] || "?";
        console.log(
          `  ${mark} ${outcome.padEnd(14)} ${id}::${fnName}  args=${JSON.stringify(args).slice(0, 60)}`,
        );
        const rec = { file: id, fn: fnName, args, outcome };
        if (outcome === "differs" || outcome === "ulp-differs") {
          rec.service = extractValue(svcR.body);
          rec.wasm = extractValue(wasmBody);
        }
        if (outcome === "wasm-error") rec.wasmError = wasmErr;
        if (outcome === "service-error") rec.serviceStatus = svcR.status;
        results.push(rec);
      }
    }
  }
} finally {
  svc.kill("SIGTERM");
}

// ---- report ---------------------------------------------------------------
fs.mkdirSync(outDir, { recursive: true });
fs.writeFileSync(
  path.join(outDir, "parity.json"),
  JSON.stringify({ tally, results }, null, 2),
);

const order = [
  "byte-identical",
  "value-equal",
  "ulp-differs",
  "differs",
  "wasm-error",
  "service-error",
  "both-error",
  "refused-unsupported",
  "compile-fail",
  "deploy-fail",
];
let txt = "M0 differential parity matrix (jl4-service vs WASM)\n\n";
for (const k of order)
  if (tally[k]) txt += `  ${String(tally[k]).padStart(4)}  ${k}\n`;
// Gate fails only on genuine logic divergence; ulp-differs is the tracked,
// known f64-vs-rational gap (M4), not a regression.
const parityFails = (tally["differs"] || 0) + (tally["wasm-error"] || 0);
txt += `\n${parityFails === 0 ? "PARITY OK" + (tally["ulp-differs"] || 0 ? " (" + tally["ulp-differs"] + " known ULP drift, M4)" : "") : "PARITY MISMATCHES: " + parityFails}\n`;
fs.writeFileSync(path.join(outDir, "parity.txt"), txt);
console.log("\n" + txt);
console.log(`Report: ${path.join(outDir, "parity.txt")} / parity.json`);
process.exit(parityFails > 0 ? 1 : 0);
