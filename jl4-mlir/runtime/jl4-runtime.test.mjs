// Standalone tests for the JS side of the WASM runtime
// (runtime/jl4-runtime.mjs). The Haskell test suite covers lowering and
// schema; this covers the runtime imports the compiled .wasm calls into.
//
// No WASM toolchain required — we drive the import functions directly
// against a real WebAssembly.Memory, exactly as a compiled module would.
//
//   node runtime/jl4-runtime.test.mjs

import { createRuntime } from "./jl4-runtime.mjs";

const rt = createRuntime();
rt.attachMemory(new WebAssembly.Memory({ initial: 4 }));
const { env } = rt.makeImports();

// Box a JS string into the f64-bit-pattern-of-a-pointer the ABI uses;
// unbox the reverse.
const box = (s) => rt.u64ToF64(rt.writeString(s));
const unbox = (f) => rt.readCString(Number(rt.f64ToU64(f)));

let pass = 0;
let fail = 0;
const eq = (name, got, want) => {
  const ok = got === want;
  console.log(
    `${ok ? "ok  " : "FAIL"} ${name}: got ${JSON.stringify(got)} want ${JSON.stringify(want)}`,
  );
  ok ? pass++ : fail++;
};

// --- __l4_str_concat (M1b: was an identity stub) ---
eq("concat", unbox(env.__l4_str_concat(box("foo"), box("bar"))), "foobar");
eq("concat empty lhs", unbox(env.__l4_str_concat(box(""), box("x"))), "x");
eq("concat empty rhs", unbox(env.__l4_str_concat(box("x"), box(""))), "x");
eq(
  "concat unicode",
  unbox(env.__l4_str_concat(box("café "), box("☕"))),
  "café ☕",
);

// --- __l4_str_len (M1b: was a `() => 0` stub) ---
eq("len ascii", env.__l4_str_len(box("hello")), 5);
eq("len empty", env.__l4_str_len(box("")), 0);

// --- __l4_str_eq (M1b: was comparing pointers, not contents) ---
eq(
  "eq same content, different pointers",
  env.__l4_str_eq(box("abc"), box("abc")),
  1,
);
eq("eq different", env.__l4_str_eq(box("abc"), box("abd")), 0);

// --- __l4_to_string (M1b: was a `() => 0` stub) ---
eq("to_string int", unbox(env.__l4_to_string(5)), "5");
eq("to_string negative int", unbox(env.__l4_to_string(-3)), "-3");
eq("to_string fractional", unbox(env.__l4_to_string(3.5)), "3.5");
eq("to_string zero", unbox(env.__l4_to_string(0)), "0");

// --- composition: a concat result must be a valid string downstream ---
const c = env.__l4_str_concat(box("a"), box("b"));
eq("concat then len", env.__l4_str_len(c), 2);
eq("concat then eq", env.__l4_str_eq(c, box("ab")), 1);

console.log(`\n${pass} passed, ${fail} failed`);
process.exit(fail ? 1 : 0);
