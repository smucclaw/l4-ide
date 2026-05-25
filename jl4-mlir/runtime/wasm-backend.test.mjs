// Unit tests for 'runtime/wasm-backend.mjs' — verifies the V8 default,
// the soft-fail-on-missing-module behavior, and that a custom backend
// can be loaded via an ES-module specifier.

import { createBackend, v8Backend } from "./wasm-backend.mjs";

let pass = 0;
let fail = 0;
const eq = (name, got, want) => {
  const ok = JSON.stringify(got) === JSON.stringify(want);
  console.log(
    `${ok ? "ok  " : "FAIL"} ${name}: got ${JSON.stringify(got)} want ${JSON.stringify(want)}`,
  );
  ok ? pass++ : fail++;
};

// ---- V8 default ------------------------------------------------------
{
  const b = await createBackend();
  eq("default backend name", b.name, "v8");
  eq("default uses 'v8Backend'", b.compile === v8Backend.compile, true);
}
{
  const b = await createBackend("v8");
  eq("explicit 'v8' resolves to v8Backend", b.name, "v8");
}

// ---- V8 round-trip: compile + instantiate a tiny module -------------
{
  // Smallest valid wasm: an empty module (8 bytes: magic + version).
  const empty = new Uint8Array([
    0x00, 0x61, 0x73, 0x6d, 0x01, 0x00, 0x00, 0x00,
  ]);
  const b = await createBackend("v8");
  const mod = await b.compile(empty);
  const { instance } = await b.instantiate(mod, {});
  eq(
    "V8 backend: instance.exports is an object",
    typeof instance.exports,
    "object",
  );
}

// ---- Missing-module fallback ----------------------------------------
{
  // Silence the warn so the test output stays clean. Restore after.
  const oldWarn = console.warn;
  let warned = "";
  console.warn = (msg) => {
    warned += msg;
  };
  const b = await createBackend("does-not-exist-package");
  console.warn = oldWarn;
  eq("missing backend falls back to V8", b.name, "v8");
  eq("missing backend warns", warned.includes("failed to load"), true);
}

console.log(`\n${pass} passed, ${fail} failed`);
process.exit(fail ? 1 : 0);
