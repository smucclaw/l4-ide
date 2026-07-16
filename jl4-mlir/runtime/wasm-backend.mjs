// Backend abstraction for WebAssembly compile + instantiate. Lets the
// worker swap V8's built-in 'WebAssembly.{compile,instantiate}' for an
// alternative runtime (wasmtime, wasmer, ...) without restructuring
// the marshaling code.
//
// A backend is just two async functions plus a name:
//
//   {
//     name,                        // "v8" | "wasmtime" | ...
//     compile(bytes) -> Module,
//     instantiate(Module, imports) -> { instance: { exports: …, memory } }
//   }
//
// 'instance.exports' must contain everything the lowering emits
// ('memory' + each function symbol). 'memory.buffer' must be an
// 'ArrayBuffer' the JS-side marshaler can wrap in 'DataView' /
// 'Uint8Array' — this is what V8's spec already gives us, and what
// any reasonable backend should expose.
//
// 'createBackend(name)' resolves a named backend, falling back to V8
// with a warning when the requested one isn't loadable. Keeps deploys
// soft-failing when an operator points 'JL4_BACKEND' at a runtime
// they haven't installed yet.

const v8Backend = {
  name: "v8",
  async compile(bytes) {
    return await WebAssembly.compile(bytes);
  },
  async instantiate(module, imports) {
    const instance = await WebAssembly.instantiate(module, imports);
    return { instance };
  },
};

/**
 * Resolve a named backend. Built-in: 'v8'. Anything else is treated
 * as an ES-module specifier whose default export is a backend record;
 * a 'load' failure (missing native dep, etc.) falls back to V8 with
 * a stderr warning so a misconfigured deploy still serves traffic.
 */
export async function createBackend(name) {
  const want = (name || "v8").toLowerCase();
  if (want === "v8") return v8Backend;
  try {
    const mod = await import(name);
    const backend = mod.default || mod.backend;
    if (
      !backend ||
      typeof backend.compile !== "function" ||
      typeof backend.instantiate !== "function"
    ) {
      throw new Error(
        "module '" +
          name +
          "' does not export a {compile, instantiate} backend",
      );
    }
    return { name: want, ...backend };
  } catch (err) {
    console.warn(
      "[jl4-runtime] backend '" +
        name +
        "' failed to load (" +
        (err.message || err) +
        "); falling back to V8",
    );
    return v8Backend;
  }
}

export { v8Backend };
