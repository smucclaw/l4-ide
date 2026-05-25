// jl4-mlir runtime — shared between the `jl4-mlir run` CLI and the
// scripts/wasm-server.mjs HTTP wrapper.
//
// The CLI embeds the contents of this file at compile time via Template
// Haskell (app/Main.hs) and concatenates a per-invocation launcher; the
// HTTP wrapper just `import`s `createRuntime` like any other ES module.
//
// Everything below is deliberately a single factory function so nothing
// stateful lives on the module top level — state (heap pointer, memory
// views, rational pool, etc.) is scoped per instantiation. The exception
// is the stateless exact-rational core (M4), kept at module level so the
// `jl4-mlir run` CLI — which embeds THIS file verbatim and concatenates a
// launcher — stays a single self-contained source with no extra imports.

// ===========================================================================
// Exact rational core (M4) — matches jl4-core's arbitrary-precision Rational.
// Pure, stateless, BigInt-backed. Unit-tested in rational.test.mjs.
// ===========================================================================

function bgcd(a, b) {
  if (a < 0n) a = -a;
  if (b < 0n) b = -b;
  while (b) {
    [a, b] = [b, a % b];
  }
  return a;
}

// Build a normalised rational {num, den}, den > 0, gcd(num,den) == 1.
export function makeRat(num, den) {
  if (den === 0n) throw new Error("rational: zero denominator");
  if (den < 0n) {
    num = -num;
    den = -den;
  }
  if (num === 0n) return { num: 0n, den: 1n };
  const g = bgcd(num, den);
  return { num: num / g, den: den / g };
}

export const fromInt = (i) => ({ num: BigInt(i), den: 1n });

// Parse a numeric literal exactly into a rational. Accepts:
//   * decimal text ("123", "1.5", "-0.001", "2.5e-3")
//   * arbitrary-precision "<num>/<den>" form (used by the MLIR lowering for
//     numeric literals, which preserves jl4-core's full Rational precision).
// No Double round-trip — `ratFromDecimalString("0.1") === {num:1n, den:10n}`.
export function ratFromDecimalString(s) {
  const str = String(s).trim();
  const slash = str.indexOf("/");
  if (slash >= 0) {
    const numTxt = str.slice(0, slash);
    const denTxt = str.slice(slash + 1);
    if (!/^[+-]?\d+$/.test(numTxt) || !/^[+-]?\d+$/.test(denTxt)) {
      throw new Error("rational: bad rational literal " + JSON.stringify(s));
    }
    return makeRat(BigInt(numTxt), BigInt(denTxt));
  }
  const m = /^([+-]?)(\d*)(?:\.(\d*))?(?:[eE]([+-]?\d+))?$/.exec(str);
  if (!m || (m[2] === "" && (m[3] || "") === "")) {
    throw new Error("rational: bad decimal literal " + JSON.stringify(s));
  }
  const sign = m[1] === "-" ? -1n : 1n;
  const intPart = m[2] || "0";
  const frac = m[3] || "";
  const exp = m[4] ? parseInt(m[4], 10) : 0;
  let num = BigInt(intPart + frac);
  let den = 10n ** BigInt(frac.length);
  if (exp >= 0) num *= 10n ** BigInt(exp);
  else den *= 10n ** BigInt(-exp);
  return makeRat(sign * num, den);
}

export const ratAdd = (a, b) =>
  makeRat(a.num * b.den + b.num * a.den, a.den * b.den);
export const ratSub = (a, b) =>
  makeRat(a.num * b.den - b.num * a.den, a.den * b.den);
export const ratMul = (a, b) => makeRat(a.num * b.num, a.den * b.den);
export const ratDiv = (a, b) => {
  if (b.num === 0n) throw new Error("rational: division by zero");
  return makeRat(a.num * b.den, a.den * b.num);
};
export const ratNeg = (a) => ({ num: -a.num, den: a.den });
export const ratAbs = (a) => (a.num < 0n ? { num: -a.num, den: a.den } : a);

// Floor / ceiling / round-half-to-even, exact (no f64 detour). Result is an
// integer rational. `ratFloor` and `ratCeil` match jl4-core's FLOOR/CEILING
// (Haskell's `floor` / `ceiling` for Rational); `ratRound` matches Haskell's
// `round` which is banker's rounding (ties to even), the same rule
// `ratToDouble` uses for the mantissa.
export function ratFloor(a) {
  let q = a.num / a.den;
  if (a.num < 0n && a.num % a.den !== 0n) q -= 1n;
  return fromInt(q);
}
export function ratCeil(a) {
  let q = a.num / a.den;
  if (a.num > 0n && a.num % a.den !== 0n) q += 1n;
  return fromInt(q);
}
export function ratRound(a) {
  // banker's rounding: round half-to-even
  const neg = a.num < 0n;
  const num = neg ? -a.num : a.num;
  const den = a.den;
  let q = num / den;
  const rem2 = (num % den) * 2n;
  if (rem2 > den || (rem2 === den && (q & 1n) === 1n)) q += 1n;
  return fromInt(neg ? -q : q);
}
// Truncate toward zero (Haskell `truncate`). NOT the same as floor for
// negative rationals.
export function ratTrunc(a) {
  return fromInt(a.num / a.den);
}

// MODULO: Haskell-style `mod` on Integer operands (Euclidean — sign follows
// divisor). jl4-core's `runBinOp BinOpModulo` raises `NotAnInteger` if either
// operand is non-integer, and `DivisionByZero` if the divisor is zero. Mirror
// both errors with messages that match the jl4-core text byte-for-byte so the
// parity harness sees identical output. Caller (the JS adapter) re-throws as
// a runtime trap to match jl4-service's HTTP error behaviour.
function prettyRatio(r) {
  // jl4-core's prettyRatio renders as "n / d" for non-integers, "n" for ints.
  if (r.den === 1n) return r.num.toString();
  return r.num.toString() + " / " + r.den.toString();
}
export function ratMod(a, b) {
  if (a.den !== 1n) {
    throw new Error(
      "Expected an Integer but got the fractional number: \n" +
        prettyRatio(a) +
        "\nDuring the evaluation of the operation:\nMODULO",
    );
  }
  if (b.den !== 1n) {
    throw new Error(
      "Expected an Integer but got the fractional number: \n" +
        prettyRatio(b) +
        "\nDuring the evaluation of the operation:\nMODULO",
    );
  }
  if (b.num === 0n) {
    throw new Error("Division by zero in the operation:\nMODULO");
  }
  // Haskell `mod`: result has the sign of the divisor.
  let r = a.num % b.num;
  if (r !== 0n && r < 0n !== b.num < 0n) r += b.num;
  return fromInt(r);
}

export const ratMin = (a, b) => (ratCmp(a, b) <= 0 ? a : b);
export const ratMax = (a, b) => (ratCmp(a, b) >= 0 ? a : b);

// -1 | 0 | 1
export function ratCmp(a, b) {
  const l = a.num * b.den;
  const r = b.num * a.den;
  return l < r ? -1 : l > r ? 1 : 0;
}

export const isInteger = (a) => a.den === 1n;

// Exact rational of an IEEE-754 double (every finite double is m·2^e). Used for
// the f64→rational shim around transcendental builtins (sqrt/ln/sin/…) that are
// inherently inexact and computed in f64. `ratToDouble(ratFromNumber(x)) === x`.
export function ratFromNumber(x) {
  if (!Number.isFinite(x)) throw new Error("rational: non-finite " + x);
  if (Number.isInteger(x)) return fromInt(BigInt(x));
  const dv = new DataView(new ArrayBuffer(8));
  dv.setFloat64(0, x);
  const bits = dv.getBigUint64(0);
  const sign = (bits >> 63n) & 1n ? -1n : 1n;
  const exp = Number((bits >> 52n) & 0x7ffn);
  const frac = bits & 0xfffffffffffffn;
  const mant = exp === 0 ? frac : frac | (1n << 52n);
  const e = exp === 0 ? -1074 : exp - 1075;
  return e >= 0
    ? makeRat(sign * mant * (1n << BigInt(e)), 1n)
    : makeRat(sign * mant, 1n << BigInt(-e));
}

// Correctly-rounded (round-half-to-even) rational -> IEEE 754 double.
// Scale so the quotient mantissa has 53 significant bits, keep the EXACT
// remainder (which subsumes the guard + sticky bits), then round half-to-even
// using `2*rem vs den`. Provably correctly-rounded; validated against
// Number(n)/Number(d) (itself correctly-rounded RNE) for operands < 2^53.
const _bitLen = (x) => (x === 0n ? 0 : x.toString(2).length);
const _TWO52 = 1n << 52n;
const _TWO53 = 1n << 53n;

export function ratToDouble(r) {
  let { num: n, den: d } = r;
  if (n === 0n) return 0;
  let sign = 1;
  if (n < 0n) {
    sign = -1;
    n = -n;
  }
  let e = _bitLen(n) - _bitLen(d) - 53;
  const scale = () => (e >= 0 ? [n, d << BigInt(e)] : [n << BigInt(-e), d]);
  let [num, den] = scale();
  let mant = num / den;
  let rem = num % den;
  if (mant >= _TWO53) {
    e += 1;
    [num, den] = scale();
    mant = num / den;
    rem = num % den;
  } else if (mant < _TWO52) {
    e -= 1;
    [num, den] = scale();
    mant = num / den;
    rem = num % den;
  }
  const twice = rem * 2n;
  if (twice > den || (twice === den && (mant & 1n) === 1n)) {
    mant += 1n;
    if (mant === _TWO53) {
      mant = _TWO52;
      e += 1;
    }
  }
  return sign * Number(mant) * Math.pow(2, e);
}

// The JSON value jl4-core would emit: integers exactly (number, or decimal
// string if outside JS safe-integer range); non-integers as the correctly-
// rounded Double (which JS serialises shortest-round-trip, matching Aeson Ryu).
const _MAX_SAFE = BigInt(Number.MAX_SAFE_INTEGER);
const _MIN_SAFE = BigInt(Number.MIN_SAFE_INTEGER);
export function ratToJSONValue(r) {
  if (r.den === 1n) {
    if (r.num <= _MAX_SAFE && r.num >= _MIN_SAFE) return Number(r.num);
    return r.num.toString();
  }
  return ratToDouble(r);
}

// Format a Double the way Aeson's `doubleDec` (bytestring's `formatDouble
// FStandard Nothing`) does — the encoding `jl4-service` emits on the wire.
// Differs from JS `Number.toString` in two ways:
//   * scientific notation kicks in once |x| < 0.1 or |x| >= 10^8 (the
//     Haskell `show :: Double` rule: `e < 0 || e > 7` on floatToDigits' e),
//     not at JS's wider 1e-7..1e21 range;
//   * integer-valued Doubles always carry an explicit ".0" suffix.
// `k` is the floatToDigits-style exponent (value = 0.d1d2…dn × 10^k); we
// derive it from JS's `toExponential` output, which exposes Ryu's shortest
// mantissa and the leading digit's decimal exponent.
// Render a Double in jl4-core's "fixed" decimal form — used by every
// trace-line Result text on a non-integer NUMBER. The encoder is
// 'Data.Scientific.formatScientific Fixed Nothing' in jl4-core's
// 'L4.Utils.Ratio.prettyRatio'; it never switches to scientific. The
// value-level JSON encoder still uses 'formatAesonDouble' below
// (Aeson's Ryu-shortest), which DOES switch to scientific at @k=-1@ —
// hence the two separate formatters: the trace text on the wire is
// '0.016583333333333332' while the value-level wire is
// '1.6583333333333332e-2', and both come from the same rational.
export function formatFixedDouble(x) {
  if (Number.isNaN(x)) return "NaN";
  if (x === Infinity) return "Infinity";
  if (x === -Infinity) return "-Infinity";
  if (x === 0) return Object.is(x, -0) ? "-0.0" : "0.0";
  const neg = x < 0;
  const abs = Math.abs(x);
  const sExp = abs.toExponential();
  const mm = /^(\d)(?:\.(\d+))?e([+-]?\d+)$/.exec(sExp);
  const d1 = mm[1];
  const rest = mm[2] || "";
  const expLead = parseInt(mm[3], 10);
  const digits = d1 + rest;
  const dl = digits.length;
  const k = expLead + 1;
  const sign = neg ? "-" : "";
  if (k >= dl) return sign + digits + "0".repeat(k - dl) + ".0";
  if (k > 0) return sign + digits.slice(0, k) + "." + digits.slice(k);
  return sign + "0." + "0".repeat(-k) + digits;
}

export function formatAesonDouble(x) {
  if (Number.isNaN(x)) return "NaN";
  if (x === Infinity) return "Infinity";
  if (x === -Infinity) return "-Infinity";
  if (x === 0) return Object.is(x, -0) ? "-0.0" : "0.0";
  const neg = x < 0;
  const abs = Math.abs(x);
  // JS toExponential gives the shortest mantissa + decimal exponent of its
  // leading digit: "1.6583333333333332e-2" / "5e-1" / "1e+7".
  const sExp = abs.toExponential();
  const mm = /^(\d)(?:\.(\d+))?e([+-]?\d+)$/.exec(sExp);
  const d1 = mm[1];
  const rest = mm[2] || "";
  const expLead = parseInt(mm[3], 10);
  const digits = d1 + rest;
  const dl = digits.length;
  // k = floatToDigits' exponent (`0.<digits> × 10^k == abs`).
  const k = expLead + 1;
  const sign = neg ? "-" : "";
  if (k < 0 || k > 7) {
    return dl === 1
      ? sign + d1 + ".0e" + expLead
      : sign + d1 + "." + rest + "e" + expLead;
  }
  if (k >= dl) return sign + digits + "0".repeat(k - dl) + ".0";
  if (k > 0) return sign + digits.slice(0, k) + "." + digits.slice(k);
  return sign + "0." + "0".repeat(-k) + digits;
}

// Tagged Double — a marker that says "render with Aeson Double rules, not JS
// Number.toString". `aesonStringify` recognises it; everything else treats it
// as a plain JS value (the wrapped Number coerces in arithmetic / comparisons,
// and the tag is invisible to anyone not asking for it).
const AESON_DOUBLE_TAG = "__l4_aeson_double";
export const aesonDouble = (x) => ({ [AESON_DOUBLE_TAG]: x });
export const isAesonDouble = (v) =>
  v !== null &&
  typeof v === "object" &&
  Object.prototype.hasOwnProperty.call(v, AESON_DOUBLE_TAG);

// Same integer-or-Double split as `ratToJSONValue`, but wraps non-integer
// rationals in an AesonDouble tag so the JSON serialiser can render them
// the way Aeson would.
export function ratToAesonValue(r) {
  if (r.den === 1n) {
    if (r.num <= _MAX_SAFE && r.num >= _MIN_SAFE) return Number(r.num);
    return { __l4_raw_json: r.num.toString() };
  }
  return aesonDouble(ratToDouble(r));
}

// Aeson-compatible JSON stringifier. Walks `value` and emits the same bytes
// `jl4-service` would for an equivalent Aeson value:
//   * AesonDouble (`{__l4_aeson_double: x}`) → `formatAesonDouble(x)`
//   * raw-JSON marker (`{__l4_raw_json: s}`) → `s` verbatim (used for big
//     integers that don't fit JS Number safely)
//   * integer Numbers → bare digits; non-integer Numbers fall back to Aeson
//     Double rules (defensive — non-integer values normally arrive tagged)
//   * arrays / objects → recursive
// Key order is insertion order, matching JS object iteration and the keys
// jl4-service writes alphabetically (covered by callers passing them in the
// same order).
export function aesonStringify(value) {
  if (value === null || value === undefined) return "null";
  if (typeof value === "boolean") return value ? "true" : "false";
  if (typeof value === "number") {
    if (Number.isInteger(value) && Number.isFinite(value)) return String(value);
    return formatAesonDouble(value);
  }
  if (typeof value === "string") return JSON.stringify(value);
  if (typeof value === "bigint") return value.toString();
  if (Array.isArray(value)) {
    return "[" + value.map(aesonStringify).join(",") + "]";
  }
  if (typeof value === "object") {
    if (AESON_DOUBLE_TAG in value)
      return formatAesonDouble(value[AESON_DOUBLE_TAG]);
    if ("__l4_raw_json" in value) return value.__l4_raw_json;
    // Aeson sorts Object keys alphabetically — match that so nested values
    // like Reasoning (children/exampleCode/explanation) stay byte-identical
    // regardless of insertion order in the construction site.
    const entries = Object.entries(value).sort(([a], [b]) =>
      a < b ? -1 : a > b ? 1 : 0,
    );
    return (
      "{" +
      entries
        .map(([k, v]) => JSON.stringify(k) + ":" + aesonStringify(v))
        .join(",") +
      "}"
    );
  }
  throw new Error("aesonStringify: unsupported value " + typeof value);
}

// Per-call handle pool. NUMBER is represented across the ABI as a handle
// (index) boxed into the f64 bit-pattern, like a pointer; the pool resets
// each call alongside the heap.
export function makeRationalPool() {
  const table = [];
  return {
    alloc(r) {
      table.push(r);
      return table.length - 1;
    },
    get(h) {
      return table[h];
    },
    reset() {
      table.length = 0;
    },
  };
}

// M5 slice 2B — per-call trace tree builder.
//
// `<fn>$trace` codegen variants bracket each instrumented subexpression with
// `__l4_trace_enter(nodeId)` / `__l4_trace_exit(resultBox, kindByte)`. The
// runtime maintains a stack: enter pushes a frame, exit pops it and appends
// it to the new top's `children` (or saves it as `root` if the stack is
// empty). When the wasm call returns, `pool.root` is the body's trace tree.
//
// nodeId is the compile-time-assigned index into `traceMeta.nodes`. The
// kindByte tells the runtime how to interpret the result box at trace
// serialisation time (rational handle, raw bool, pointer-to-string, …).
//
// The pool is per-call (resets alongside the heap and rational pool) so
// instrumented and uninstrumented calls into the same instance never
// share frames.
export function makeTracePool() {
  const frames = [];
  const roots = [];
  // M5 slice 4A — currentFn stack: every `<fn>$trace` calls
  // `__l4_trace_enter_fn(fnSymbol)` at entry and `__l4_trace_exit_fn()`
  // at return, so the trace pool always knows whose metadata to look
  // up for the trace_enter calls in flight. Without this, nested calls
  // to a different function would use the OUTER function's node table
  // and resolve every nodeId to the wrong (or out-of-range) entry.
  const fnStack = [];
  return {
    enter(nodeId) {
      const fn = fnStack.length > 0 ? fnStack[fnStack.length - 1] : null;
      frames.push({ nodeId, fn, children: [], result: null });
    },
    exit(rawResult, resultKind) {
      const frame = frames.pop();
      if (!frame) return; // defensive: stray exit
      frame.result = { raw: rawResult, kind: resultKind };
      if (frames.length === 0) roots.push(frame);
      else frames[frames.length - 1].children.push(frame);
    },
    pushFn(fnSymbol) {
      fnStack.push(fnSymbol);
    },
    popFn() {
      fnStack.pop();
    },
    // M5 slice 4A — a `<fn>$trace` invocation pushes TWO top-level
    // frames: the synthetic fn-value frame and the body root. Callers
    // need both. `root` (singular) is kept for back-compat with tests
    // that only push one enter/exit pair.
    get roots() {
      return roots;
    },
    get root() {
      return roots.length > 0 ? roots[roots.length - 1] : null;
    },
    get stackDepth() {
      return frames.length;
    },
    reset() {
      frames.length = 0;
      roots.length = 0;
      fnStack.length = 0;
    },
  };
}

/**
 * Sentinel error thrown when a single evaluation tries to allocate
 * past 'maxHeapBytes'. Caught by 'wasm-server.mjs' to surface a 413
 * (or similar) without taking the host down. The shipping default of
 * 64 MiB is enough for the M0 corpus's worst-case allocations
 * (calculate-tax / order-total each peak under 1 MiB) while staying
 * a comfortable factor under most cloud function memory budgets.
 */
export class MemoryLimitError extends Error {
  constructor(requested, used, limit) {
    super(
      "wasm evaluation tried to allocate " +
        requested +
        " bytes (used " +
        used +
        " of " +
        limit +
        " allowed)",
    );
    this.name = "MemoryLimitError";
    this.requested = requested;
    this.used = used;
    this.limit = limit;
  }
}

export const DEFAULT_MAX_HEAP_BYTES = 64 * 1024 * 1024;

/**
 * M6 — sentinel for missing / malformed deontic request payloads.
 * The HTTP wrapper catches this and surfaces a 400 with the
 * 'message' as the error body, matching jl4-service's
 * "startTime is required for functions returning DEONTIC" format.
 */
export class DeonticInputError extends Error {
  constructor(message) {
    super(message);
    this.name = "DeonticInputError";
  }
}

// ===========================================================================
// M6 — deontic interpreter. Walks 'schema.deonticContract' against the
// request's startTime + events stream, returning a wire-shaped value
// that mirrors jl4-service byte-for-byte. Inlined here (no separate
// module) so the CLI's @embedStringFile "runtime/jl4-runtime.mjs"@
// path stays a single self-contained source.
//
// Wire shapes:
//   "FULFILLED"
//   { OBLIGATION: { action, deadline, modal, party } }
//   "BREACH" | { BREACH: { by, because } }   -- when by/because set
// ===========================================================================

/**
 * @param contract  - parsed 'deonticContract' tree from the schema.
 * @param startTime - simulation start time (number).
 * @param events    - array of { party, action, at } from the request.
 * @param args      - request's @arguments@ bag — used to resolve
 *                    'DEParam' references inside contract nodes
 *                    (e.g. when the contract says @PARTY driver@ and
 *                    we need to look up the actual driver value).
 * @returns the JSON-shaped value to drop under @{value: …}@.
 */
export function runDeontic(contract, startTime, events, args, meta) {
  const t0 = Number(startTime);
  const sortedEvents = (events || [])
    .slice()
    .sort((a, b) => Number(a.at) - Number(b.at));
  const ctx = { args: args || {}, meta: meta || null };

  let current = contract;
  let now = t0;
  let obligationStart = t0; // when the *current* obligation became active
  let cursor = 0;

  while (current) {
    if (current.kind === "IF") {
      const v = evalDeonticGuard(current.cond, ctx);
      current = v ? current.then : current.else;
      continue;
    }
    if (current.kind !== "OBLIGATION") break;

    const deadlineLit =
      current.deadline == null ? null : Number(current.deadline);
    const absoluteDeadline =
      deadlineLit == null ? null : obligationStart + deadlineLit;

    while (
      cursor < sortedEvents.length &&
      Number(sortedEvents[cursor].at) < now
    ) {
      cursor++;
    }
    let matchingIdx = -1;
    for (let i = cursor; i < sortedEvents.length; i++) {
      const ev = sortedEvents[i];
      if (deonticEventMatchesObligation(ev, current, ctx)) {
        matchingIdx = i;
        break;
      }
    }

    const modal = current.modal || "MUST";
    if (matchingIdx === -1) {
      // No matching event. Find the latest event time the simulator
      // has 'observed' — anything from 'cursor' onward, capped at
      // the obligation's deadline (events past the deadline lapse
      // it before they're observed for this obligation).
      let lastObserved = now;
      for (let i = cursor; i < sortedEvents.length; i++) {
        const et = Number(sortedEvents[i].at);
        if (absoluteDeadline != null && et > absoluteDeadline) break;
        if (et > lastObserved) lastObserved = et;
      }

      // If any event sits strictly past the deadline → obligation
      // lapsed, take LEST.
      if (
        absoluteDeadline != null &&
        sortedEvents.some((e) => Number(e.at) > absoluteDeadline)
      ) {
        current = current.lest || { kind: "BREACH" };
        continue;
      }

      if (modal === "MAY") {
        // MAY is a permission — no event = terminal FULFILLED.
        current = { kind: "FULFILLED" };
        continue;
      }

      // Residual OBLIGATION. If 'now' has advanced past the
      // obligation's start (real events tested against it), surface
      // the *remaining* time as a JSON number; otherwise hand back
      // the original WITHIN literal as a string (matches
      // jl4-service's lazy display of un-instantiated obligations).
      const advanced = lastObserved > obligationStart;
      const remainingNum =
        advanced && absoluteDeadline != null
          ? absoluteDeadline - lastObserved
          : null;
      return deonticObligationToWire(current, ctx, remainingNum);
    }

    const ev = sortedEvents[matchingIdx];
    const evTime = Number(ev.at);
    if (absoluteDeadline != null && evTime > absoluteDeadline) {
      current = current.lest || { kind: "BREACH" };
      continue;
    }
    now = evTime;
    cursor = matchingIdx + 1;
    obligationStart = now; // the next obligation in the cascade starts here
    current = current.hence || { kind: "FULFILLED" };
  }

  if (!current) return "FULFILLED";
  switch (current.kind) {
    case "FULFILLED":
      return "FULFILLED";
    case "BREACH":
      return deonticBreachToWire(current);
    default:
      throw new Error(
        "deontic interpreter: unexpected node kind: " + current.kind,
      );
  }
}

// Evaluate a 'DeonticGuard' AST against args. Returns the JS value
// (boolean, number, string, object); the 'IF' walker treats the
// result via JS truthiness.
function evalDeonticGuard(g, ctx) {
  if (!g) return null;
  switch (g.kind) {
    case "lit":
      return g.value;
    case "var":
      return ctx.args ? ctx.args[g.name] : undefined;
    case "proj": {
      const obj = evalDeonticGuard(g.expr, ctx);
      return obj == null ? undefined : obj[g.field];
    }
    case "eq":
      return deonticDeepEqual(
        evalDeonticGuard(g.lhs, ctx),
        evalDeonticGuard(g.rhs, ctx),
      );
    case "and":
      return !!(evalDeonticGuard(g.lhs, ctx) && evalDeonticGuard(g.rhs, ctx));
    case "or":
      return !!(evalDeonticGuard(g.lhs, ctx) || evalDeonticGuard(g.rhs, ctx));
    case "not":
      return !evalDeonticGuard(g.expr, ctx);
    case "lt":
      return (
        Number(evalDeonticGuard(g.lhs, ctx)) <
        Number(evalDeonticGuard(g.rhs, ctx))
      );
    case "gt":
      return (
        Number(evalDeonticGuard(g.lhs, ctx)) >
        Number(evalDeonticGuard(g.rhs, ctx))
      );
    case "leq":
      return (
        Number(evalDeonticGuard(g.lhs, ctx)) <=
        Number(evalDeonticGuard(g.rhs, ctx))
      );
    case "geq":
      return (
        Number(evalDeonticGuard(g.lhs, ctx)) >=
        Number(evalDeonticGuard(g.rhs, ctx))
      );
    default:
      throw new Error("deontic guard: unknown kind: " + g.kind);
  }
}

// Resolve a 'DeonticExpr' (literal string or {param: "name"} ref)
// against args. Returns the underlying value (object / string).
function resolveDeonticExpr(expr, ctx) {
  if (expr == null) return null;
  if (typeof expr === "string") return expr;
  if (expr.param != null) {
    return ctx.args ? ctx.args[expr.param] : undefined;
  }
  return expr;
}

// Match an incoming event against a (possibly parameterized)
// obligation. Party / action are 'DeonticExpr' — literal text uses
// backtick-stripped string equality, param refs deep-equal the
// resolved value.
function deonticEventMatchesObligation(ev, obligation, ctx) {
  const obligationParty = resolveDeonticExpr(obligation.party, ctx);
  const obligationAction = resolveDeonticExpr(obligation.action, ctx);
  return (
    deonticValueMatches(obligationParty, ev.party) &&
    deonticValueMatches(obligationAction, ev.action)
  );
}

function deonticValueMatches(obligationVal, eventVal) {
  // Both string: compare with backticks stripped from contract side.
  if (typeof obligationVal === "string" && typeof eventVal === "string") {
    return deonticStripBackticks(obligationVal) === eventVal;
  }
  return deonticDeepEqual(obligationVal, eventVal);
}

// Deep equality for primitives + plain objects + arrays — enough to
// match record-typed parties (e.g. {name: "Alice"}) against event
// payloads of the same shape.
function deonticDeepEqual(a, b) {
  if (a === b) return true;
  if (a == null || b == null) return false;
  if (typeof a !== typeof b) return false;
  if (typeof a !== "object") return false;
  if (Array.isArray(a) !== Array.isArray(b)) return false;
  if (Array.isArray(a)) {
    if (a.length !== b.length) return false;
    for (let i = 0; i < a.length; i++) {
      if (!deonticDeepEqual(a[i], b[i])) return false;
    }
    return true;
  }
  const ka = Object.keys(a);
  const kb = Object.keys(b);
  if (ka.length !== kb.length) return false;
  for (const k of ka) {
    if (!deonticDeepEqual(a[k], b[k])) return false;
  }
  return true;
}

function deonticStripBackticks(s) {
  if (typeof s !== "string") return s;
  if (s.startsWith("`") && s.endsWith("`")) return s.slice(1, -1);
  return s;
}

function deonticObligationToWire(o, ctx, remainingNum) {
  const resolvedParty = resolveDeonticExpr(o.party, ctx);
  const resolvedAction = resolveDeonticExpr(o.action, ctx);
  // Record-typed parties wrap as @{<TypeName>: <fields>}@ — pull
  // the type name from the request's parameter schema (the wasm
  // bundle's @x-l4-type@ stamp). Literal-string parties stay as-is.
  const taggedParty =
    o.party && o.party.param != null
      ? deonticTagRecord(resolvedParty, ctx, o.party.param)
      : resolvedParty;
  // 'deadline' on the wire: number = "remaining time after observed
  // events", string = "original WITHIN literal, untouched". The
  // walker picks which one to pass.
  const deadline =
    remainingNum != null
      ? remainingNum
      : o.deadline == null
        ? null
        : String(o.deadline);
  return {
    OBLIGATION: {
      action: resolvedAction,
      deadline,
      modal: o.modal,
      party: taggedParty,
    },
  };
}

// Wrap a resolved parameter value in its L4 type name when the
// schema records one (matches jl4-service's @{"Driver": …}@
// formatting). Strings / numbers / un-typed bags pass through
// untouched so literal-party contracts don't accidentally gain
// an outer key.
function deonticTagRecord(value, ctx, paramName) {
  if (value == null || typeof value !== "object" || Array.isArray(value)) {
    return value;
  }
  const meta = ctx && ctx.meta;
  const props = meta && meta.parameters && meta.parameters.properties;
  const spec = props && props[paramName];
  const typeName = spec && spec["x-l4-type"];
  if (!typeName) return value;
  return { [typeName]: value };
}

function deonticBreachToWire(b) {
  if (b.by != null || b.because != null) {
    return { BREACH: { by: b.by, because: b.because } };
  }
  return "BREACH";
}

export function createRuntime(opts) {
  const maxHeapBytes =
    opts && typeof opts.maxHeapBytes === "number"
      ? opts.maxHeapBytes
      : DEFAULT_MAX_HEAP_BYTES;

  // ---- u64 <-> f64 bit reinterpret (uniform f64 ABI) ----
  const scratch = new ArrayBuffer(8);
  const sView = new DataView(scratch);
  const u64ToF64 = (u) => {
    sView.setBigUint64(0, BigInt(u), true);
    return sView.getFloat64(0, true);
  };
  const f64ToU64 = (f) => {
    sView.setFloat64(0, Number(f), true);
    return sView.getBigUint64(0, true);
  };

  // ---- Linear-memory allocator + views (attached once the wasm
  //      module has been instantiated) ----
  let memView, memU8;
  let heapPtr = 1024;
  // Tracks the max @heapPtr@ value seen since the last 'resetHeap' so
  // hosts can monitor per-eval allocation pressure even when individual
  // calls stay under the cap.
  let peakHeapPtr = 1024;
  const allocBytes = (n) => {
    const nn = Number(n);
    // Bump-pointer allocator: 8-byte align both the allocation size
    // and the resulting next-free pointer. Reject when honoring the
    // request would push 'heapPtr' past 'maxHeapBytes' so a runaway
    // wasm body (infinite cons chain, unbounded list grow) can't
    // exhaust the JS heap.
    const aligned = (nn + 7) & ~7;
    if (heapPtr + aligned > maxHeapBytes) {
      throw new MemoryLimitError(aligned, heapPtr - 1024, maxHeapBytes);
    }
    const p = heapPtr;
    heapPtr += aligned;
    if (heapPtr > peakHeapPtr) peakHeapPtr = heapPtr;
    return p;
  };
  // Per-call exact-rational pool (M4). NUMBER values are handles into this.
  const ratPool = makeRationalPool();
  // Per-call trace pool (M5 slice 2B). Built up by __l4_trace_enter /
  // __l4_trace_exit calls inside an instrumented `<fn>$trace` wasm function;
  // when the wasm call returns, `tracePool.root` holds the body's trace tree
  // (a {nodeId, children, result} record) — see `makeTracePool` below.
  const tracePool = makeTracePool();
  // M5 slice 4E — per-call wasm-pointer ↔ JS-object map. The marshaler
  // registers every compound JSON arg (record / list / JUST) with its
  // wasm linear-memory address; when the body fires
  // `__l4_mark_forced(wasmPtr, field)`, the runtime looks the JS
  // object up here and attaches `.__forced.add(field)` directly. The
  // renderer reads `__forced` on each object — no path strings, no
  // helper-call translation needed (a helper's param shares the
  // caller's pointer, so it shares the JS object too).
  const valueByPtr = new Map();
  // Kept for back-compat tests but no longer the primary mechanism.
  const forcedFields = new Set();
  const resetHeap = () => {
    heapPtr = 1024;
    peakHeapPtr = 1024;
    ratPool.reset();
    tracePool.reset();
    forcedFields.clear();
    valueByPtr.clear();
  };
  // Surface peak-allocation telemetry so the HTTP wrapper can log it
  // or expose it as a header for monitoring per-eval memory pressure.
  const getPeakHeapBytes = () => peakHeapPtr - 1024;
  const getHeapBytes = () => heapPtr - 1024;
  const getMaxHeapBytes = () => maxHeapBytes;
  const attachMemory = (memory) => {
    memView = new DataView(memory.buffer);
    memU8 = new Uint8Array(memory.buffer);
  };

  // Box/unbox a rational handle across the f64 ABI (handle index → f64 bit
  // pattern, like a pointer). The handle is a small non-negative integer.
  const ratBox = (r) => u64ToF64(BigInt(ratPool.alloc(r)));
  const ratUnbox = (f) => ratPool.get(Number(f64ToU64(f)));

  function readCString(p) {
    const ptr = Number(p);
    if (!ptr) return "";
    let end = ptr;
    while (end < memU8.length && memU8[end] !== 0) end++;
    return Buffer.from(memU8.subarray(ptr, end)).toString("utf8");
  }

  function writeString(s) {
    const bytes = Buffer.from(String(s), "utf8");
    const p = allocBytes(bytes.length + 1);
    memU8.set(bytes, p);
    memU8[p + bytes.length] = 0;
    return p;
  }

  // ---- DATE / TIME / DATETIME helpers ----
  // DATE: f64 days with a Sunday-zero offset. The raw number is
  //   days-since-1969-12-28 (the last Sunday before the Unix epoch),
  //   so `date % 7` gives the day-of-week with Sunday=0, matching the
  //   library's weekday constants.
  // TIME: f64 seconds since midnight.
  // DATETIME: pointer (bit-cast to f64) to a 2-slot record
  //   [utcSeconds, tzNamePtr].
  const DATE_EPOCH_SHIFT = 4; // 1969-12-28 (Sun) to 1970-01-01 (Thu)
  const dateFromDMY = (d, m, y) =>
    Math.floor(Date.UTC(y, m - 1, d) / 86400000) + DATE_EPOCH_SHIFT;
  const dateToUnixDays = (x) => Number(x) - DATE_EPOCH_SHIFT;
  function parseDateStr(s) {
    const m = /^(\d{4})-(\d{2})-(\d{2})/.exec(String(s));
    if (!m) return NaN;
    return dateFromDMY(+m[3], +m[2], +m[1]);
  }
  function formatDate(days) {
    const d = new Date(dateToUnixDays(days) * 86400000);
    const Y = d.getUTCFullYear(),
      M = d.getUTCMonth() + 1,
      D = d.getUTCDate();
    return (
      String(Y).padStart(4, "0") +
      "-" +
      String(M).padStart(2, "0") +
      "-" +
      String(D).padStart(2, "0")
    );
  }
  const timeFromHMS = (h, m, s) => h * 3600 + m * 60 + s;
  function parseTimeStr(s) {
    const m = /^(\d{1,2}):(\d{2})(?::(\d{2}(?:\.\d+)?))?/.exec(String(s));
    if (!m) return NaN;
    return +m[1] * 3600 + +m[2] * 60 + +(m[3] || 0);
  }
  function formatTime(sec) {
    const t = Number(sec);
    const h = Math.floor(t / 3600);
    const mi = Math.floor((t % 3600) / 60);
    const s = Math.floor(t % 60);
    return (
      String(h).padStart(2, "0") +
      ":" +
      String(mi).padStart(2, "0") +
      ":" +
      String(s).padStart(2, "0")
    );
  }
  function tzOffsetMs(tz, utcMs) {
    if (!tz || tz === "UTC" || tz === "Etc/UTC") return 0;
    const dtf = new Intl.DateTimeFormat("en-US", {
      timeZone: tz,
      hourCycle: "h23",
      year: "numeric",
      month: "numeric",
      day: "numeric",
      hour: "numeric",
      minute: "numeric",
      second: "numeric",
    });
    const parts = dtf.formatToParts(new Date(utcMs));
    const map = {};
    for (const p of parts)
      if (p.type !== "literal") map[p.type] = parseInt(p.value, 10);
    const asUTC = Date.UTC(
      map.year,
      map.month - 1,
      map.day,
      map.hour,
      map.minute,
      map.second,
    );
    return asUTC - utcMs;
  }
  function datetimeFromDTZ(daysF, secsF, tzPtrF) {
    const tzStr = readCString(Number(f64ToU64(tzPtrF))) || "UTC";
    const localMs = dateToUnixDays(daysF) * 86400000 + Number(secsF) * 1000;
    const utcMs = localMs - tzOffsetMs(tzStr, localMs);
    const p = allocBytes(16);
    memView.setFloat64(p, utcMs / 1000, true);
    memView.setBigUint64(p + 8, BigInt(writeString(tzStr)), true);
    return u64ToF64(p);
  }
  function formatDatetime(dtPtrF) {
    const p = Number(f64ToU64(dtPtrF));
    if (!p) return "";
    const utcSec = memView.getFloat64(p, true);
    const d = new Date(utcSec * 1000);
    return (
      String(d.getUTCFullYear()).padStart(4, "0") +
      "-" +
      String(d.getUTCMonth() + 1).padStart(2, "0") +
      "-" +
      String(d.getUTCDate()).padStart(2, "0") +
      " " +
      String(d.getUTCHours()).padStart(2, "0") +
      ":" +
      String(d.getUTCMinutes()).padStart(2, "0") +
      ":" +
      String(d.getUTCSeconds()).padStart(2, "0") +
      " UTC"
    );
  }

  // MAYBE helpers (tag 0 = NOTHING, 1 = JUST).
  function mbJustF64(f64Val) {
    const p = allocBytes(16);
    memView.setFloat64(p, 1.0, true);
    memView.setFloat64(p + 8, Number(f64Val), true);
    return u64ToF64(p);
  }
  function mbNothing() {
    const p = allocBytes(16);
    memView.setFloat64(p, 0.0, true);
    memView.setFloat64(p + 8, 0.0, true);
    return u64ToF64(p);
  }

  // ---- Argument marshaling ----
  function isPointerSchema(schema) {
    if (!schema) return false;
    if (schema.enum && schema.enum.length > 0) return false;
    // DATE / TIME wire as strings but are numeric f64 internally.
    if (schema.format === "date" || schema.format === "time") return false;
    const t = schema.type;
    return t === "string" || t === "array" || t === "object";
  }

  // NUMBER inputs become rational handles, parsed from the source decimal
  // text where available. JSON.parse already converted the literal to a
  // JS Number by the time we get here, so for fractional inputs we still
  // suffer the Number-of-decimal loss (slice 3 will fix this by intercepting
  // numeric literals before JSON.parse runs). Integer inputs are exact.
  function ratHandleFromInput(value) {
    if (typeof value === "bigint") return ratBox(fromInt(value));
    if (typeof value === "number" && Number.isInteger(value)) {
      return ratBox(fromInt(BigInt(value)));
    }
    // Render as decimal text. For finite non-integer doubles, JS's `String`
    // gives the shortest-round-trip decimal — which `ratFromDecimalString`
    // parses exactly into a rational. Same digits jl4-service sees if it
    // round-trips through JSON.
    return ratBox(ratFromDecimalString(String(value)));
  }

  function marshalArg(value, schema) {
    if (value === null || value === undefined) return 0;
    if (schema && schema.enum && schema.enum.length > 0) {
      const idx = schema.enum.indexOf(String(value));
      return idx >= 0 ? idx : 0;
    }
    if (schema && schema.format === "date" && schema.type === "string")
      return parseDateStr(value);
    if (schema && schema.format === "time" && schema.type === "string")
      return parseTimeStr(value);
    switch (schema && schema.type) {
      case "number":
        return ratHandleFromInput(value);
      case "boolean":
        return value ? 1 : 0;
      case "string":
        return writeString(value);
      case "array":
        return marshalList(value, (schema && schema.items) || {});
      case "object":
        return marshalStruct(value, schema);
      default:
        return Number(value) || 0;
    }
  }

  function marshalMaybe(value, innerSchema) {
    const p = allocBytes(16);
    if (value === null || value === undefined) {
      memView.setFloat64(p, 0.0, true);
      memView.setFloat64(p + 8, 0.0, true);
      return p;
    }
    memView.setFloat64(p, 1.0, true);
    const inner = marshalArg(value, innerSchema);
    // NUMBER handles are f64 bit-patterns; store directly. Other f64
    // values (DATE/TIME serials, enums, BOOL) likewise. Pointers (string /
    // array / object) come in as integer addresses — encode as u64.
    if (
      innerSchema &&
      (innerSchema.type === "number" || innerSchema.type === "boolean")
    )
      memView.setFloat64(p + 8, Number(inner), true);
    else if (
      innerSchema &&
      (innerSchema.format === "date" || innerSchema.format === "time")
    )
      memView.setFloat64(p + 8, Number(inner), true);
    else if (innerSchema && innerSchema.enum && innerSchema.enum.length > 0)
      memView.setFloat64(p + 8, Number(inner), true);
    else memView.setBigUint64(p + 8, BigInt(inner), true);
    return p;
  }

  function marshalStruct(value, schema) {
    if (!schema || !schema.properties) return 0;
    const order = schema.propertyOrder || Object.keys(schema.properties);
    const required = new Set(schema.required || order);
    const p = allocBytes(order.length * 8);
    // M5 slice 4E — register this record's JS object + schema under
    // its wasm address so `__l4_mark_forced` finds it (via .value) and
    // `renderTraceResult`'s compound-result fallback can walk it via
    // `prettyL4Value(value, schema)`. Initialise `__forced` as an
    // empty Set so the renderer can distinguish "wasm ran but didn't
    // force anything" (Set with size 0) from "synthetic context, no
    // wasm" (no `__forced` property) — the latter defaults to "treat
    // all as forced". Define it non-enumerable so it doesn't leak into
    // the JSON when the caller re-serialises the args (the harness
    // re-sends the same args object to jl4-service for differential
    // comparison, and a visible @__forced@ field pollutes svc's
    // trace).
    if (value && typeof value === "object") {
      valueByPtr.set(p, { value, schema });
      if (!Object.prototype.hasOwnProperty.call(value, "__forced")) {
        Object.defineProperty(value, "__forced", {
          value: new Set(),
          writable: true,
          enumerable: false,
          configurable: true,
        });
      }
    }
    order.forEach((name, idx) => {
      const fs_ = schema.properties[name] || {};
      const v = (value || {})[name];
      const isMaybe = !required.has(name);
      const isEnum = !!(fs_.enum && fs_.enum.length > 0);
      if (isMaybe) {
        memView.setBigUint64(p + idx * 8, BigInt(marshalMaybe(v, fs_)), true);
      } else if (fs_.type === "number" || isEnum) {
        // NUMBER: marshalArg returns the f64 bit-pattern of the rational
        // handle. Enum: a small integer encoded as f64.
        memView.setFloat64(p + idx * 8, Number(marshalArg(v, fs_)), true);
      } else {
        memView.setBigUint64(p + idx * 8, BigInt(marshalArg(v, fs_)), true);
      }
    });
    return p;
  }

  function marshalList(values, itemSchema) {
    if (!Array.isArray(values) || values.length === 0) return 0;
    let head = 0;
    for (let i = values.length - 1; i >= 0; i--) {
      const p = allocBytes(16);
      const item = marshalArg(values[i], itemSchema);
      if (itemSchema && itemSchema.type === "number")
        memView.setFloat64(p, Number(item), true);
      else memView.setBigUint64(p, BigInt(item), true);
      memView.setBigUint64(p + 8, BigInt(head), true);
      head = p;
    }
    // M5 final-cleanup — register the cons-list's head pointer under
    // the original JS array + an array schema synthesised from the
    // item schema. When the body's Proj returns this list, the trace
    // renderer looks it up and walks via prettyL4Value's array branch.
    if (head !== 0) {
      valueByPtr.set(head, {
        value: values,
        schema: { type: "array", items: itemSchema },
      });
    }
    return head;
  }

  function unmarshalScalar(raw, type) {
    if (type === "BOOLEAN") return Number(raw) !== 0;
    // NUMBER: raw is the f64 bit-pattern of a rational handle. Render
    // through ratToAesonValue so integers stay integer and fractions are
    // tagged for Aeson-compatible Double formatting downstream.
    if (type === "NUMBER") return ratToAesonValue(ratUnbox(raw));
    if (type === "STRING") return readCString(Number(f64ToU64(raw)));
    if (type === "DATE") return formatDate(raw);
    if (type === "TIME") return formatTime(raw);
    if (type === "DATETIME") return formatDatetime(raw);
    return Number(raw);
  }
  function unmarshalMaybe(raw, innerType) {
    const ptr = Number(f64ToU64(raw));
    if (!ptr) return "NOTHING";
    const tag = memView.getFloat64(ptr, true);
    if (tag === 0.0) return "NOTHING";
    let payload;
    if (innerType === "NUMBER") {
      // The slot holds the f64 bit-pattern of a rational handle.
      const handleF = memView.getFloat64(ptr + 8, true);
      payload = ratToAesonValue(ratUnbox(handleF));
    } else if (innerType === "BOOLEAN")
      payload = memView.getFloat64(ptr + 8, true) !== 0;
    else if (innerType === "STRING")
      payload = readCString(Number(memView.getBigUint64(ptr + 8, true)));
    else payload = memView.getFloat64(ptr + 8, true);
    return { JUST: [payload] };
  }
  function unmarshalResult(raw, returnType) {
    if (returnType && returnType.indexOf("MAYBE ") === 0)
      return unmarshalMaybe(raw, returnType.slice(6));
    return unmarshalScalar(raw, returnType);
  }

  // ---- WASM `env.*` import table. Wrapped in a Proxy that returns a
  //      throwing stub for any symbol we didn't implement, so modules
  //      instantiate even when they declare imports they never call.
  function makeImports() {
    // M4 slice 2b: NUMBER is represented as a rational handle (an f64-boxed
    // index into ratPool). Every builtin that *consumes* a NUMBER unboxes;
    // every builtin that *produces* a NUMBER allocates a handle. BOOLEAN,
    // DATE, TIME, DATETIME, STRING, enum tags, and pointers keep their raw
    // f64 encoding — only NUMBER changed.
    const ratFromInt = (i) => ratBox(fromInt(BigInt(i)));
    // Take a NUMBER handle, drop to Double (inherently inexact path) — for
    // transcendentals where jl4-core itself does `fromRational :: Double`.
    const numToF64 = (handleF) => ratToDouble(ratUnbox(handleF));
    // Take a Double, encode as a handle. Match jl4-core's `toRational ::
    // Double` (the exact m·2^e rational of the double, not a decimal).
    const f64ToNum = (x) => ratBox(ratFromNumber(Number(x)));
    // NUMBER that is required to be an Integer (per jl4-core's `expectInteger`
    // / `expectWhole`). Throws with the jl4-core message + operation label so
    // the parity harness sees identical output.
    const numToInt = (handleF, opLabel) => {
      const r = ratUnbox(handleF);
      if (r.den !== 1n) {
        throw new Error(
          "Expected an Integer but got the fractional number: \n" +
            (r.num.toString() + " / " + r.den.toString()) +
            "\nDuring the evaluation of the operation:\n" +
            opLabel,
        );
      }
      return r.num;
    };

    const env = {
      __l4_pow: (aF, bF) =>
        ratBox(ratFromNumber(Math.pow(numToF64(aF), numToF64(bF)))),
      __l4_min: (aF, bF) => ratBox(ratMin(ratUnbox(aF), ratUnbox(bF))),
      __l4_max: (aF, bF) => ratBox(ratMax(ratUnbox(aF), ratUnbox(bF))),
      __l4_abs: (aF) => ratBox(ratAbs(ratUnbox(aF))),
      __l4_floor: (aF) => ratBox(ratFloor(ratUnbox(aF))),
      __l4_ceil: (aF) => ratBox(ratCeil(ratUnbox(aF))),
      __l4_round: (aF) => ratBox(ratRound(ratUnbox(aF))),
      // Strings are NUL-terminated UTF-8 in linear memory, boxed as the
      // f64 bit-pattern of their pointer. concat allocates a fresh buffer;
      // the per-call heap reset means we never need to free it.
      __l4_str_concat: (aF, bF) =>
        u64ToF64(
          writeString(
            readCString(Number(f64ToU64(aF))) +
              readCString(Number(f64ToU64(bF))),
          ),
        ),
      // Content equality — NOT pointer equality. Two equal strings at
      // different addresses (e.g. concat results) must compare equal.
      __l4_str_eq: (aF, bF) =>
        readCString(Number(f64ToU64(aF))) === readCString(Number(f64ToU64(bF)))
          ? 1
          : 0,
      __l4_str_len: (sF) => readCString(Number(f64ToU64(sF))).length,
      // NUMBER → STRING. Matches jl4-core: integers print with no decimal
      // point, non-integers print as their correctly-rounded shortest Double.
      __l4_to_string: (vF) =>
        u64ToF64(writeString(String(ratToJSONValue(ratUnbox(vF))))),
      // ---- Exact-rational NUMBER ABI (M4). Args/results are f64-boxed
      //      handles into ratPool; resets each call. ----
      __l4_rat_parse: (sPtrF) =>
        ratBox(ratFromDecimalString(readCString(Number(f64ToU64(sPtrF))))),
      __l4_rat_from_int: (f) => ratBox(fromInt(BigInt(Math.trunc(Number(f))))),
      __l4_rat_add: (a, b) => ratBox(ratAdd(ratUnbox(a), ratUnbox(b))),
      __l4_rat_sub: (a, b) => ratBox(ratSub(ratUnbox(a), ratUnbox(b))),
      __l4_rat_mul: (a, b) => ratBox(ratMul(ratUnbox(a), ratUnbox(b))),
      __l4_rat_div: (a, b) => {
        const r = ratUnbox(b);
        if (r.num === 0n) {
          throw new Error("Division by zero in the operation:\nDIVIDED_BY");
        }
        return ratBox(ratDiv(ratUnbox(a), r));
      },
      // jl4-core's MODULO: integer-only, Haskell `mod` (sign follows divisor),
      // DivisionByZero on b==0. Errors match the jl4-core text byte-for-byte.
      __l4_rat_mod: (a, b) => ratBox(ratMod(ratUnbox(a), ratUnbox(b))),
      __l4_rat_neg: (a) => ratBox(ratNeg(ratUnbox(a))),
      // comparison returns a plain numeric -1/0/1 (for branching), not a handle
      __l4_rat_cmp: (a, b) => ratCmp(ratUnbox(a), ratUnbox(b)),
      // shims for inherently-inexact builtins (sqrt/ln/sin/pow…): drop to f64
      __l4_rat_to_f64: (a) => ratToDouble(ratUnbox(a)),
      __l4_f64_to_rat: (f) => ratBox(ratFromNumber(Number(f))),
      // count returns a NUMBER (count of list cells) — must be a handle.
      __l4_list_count: (ptrF) => {
        let n = 0,
          p = Number(f64ToU64(ptrF));
        while (p !== 0 && n < 100000) {
          p = Number(memView.getBigUint64(p + 8, true));
          n++;
        }
        return ratFromInt(n);
      },
      __l4_list_empty: () => 0,
      __l4_alloc: (n) => allocBytes(n),
      __l4_free: () => {},
      // libm functions llc emits from MLIR arith that wasm32 lacks
      fmod: (a, b) => Number(a) % Number(b),
      fmodf: (a, b) => Number(a) % Number(b),
      remainder: (a, b) =>
        Number(a) - Math.round(Number(a) / Number(b)) * Number(b),
      remainderf: (a, b) =>
        Number(a) - Math.round(Number(a) / Number(b)) * Number(b),
      trunc: Math.trunc,
      // DATE — DATE values are raw f64 day-serials (DATE type, not NUMBER).
      // NUMBER args (d, m, y, n) and NUMBER results (day/month/year/serial)
      // are rational handles.
      __l4_date_from_dmy: (dH, mH, yH) =>
        dateFromDMY(
          Number(numToInt(dH, "DATE_FROM_DMY")),
          Number(numToInt(mH, "DATE_FROM_DMY")),
          Number(numToInt(yH, "DATE_FROM_DMY")),
        ),
      __l4_date_from_serial: (nH) => Number(numToInt(nH, "DATE_FROM_SERIAL")),
      __l4_date_serial: (d) => ratFromInt(Number(d)),
      __l4_date_day: (d) =>
        ratFromInt(new Date(dateToUnixDays(d) * 86400000).getUTCDate()),
      __l4_date_month: (d) =>
        ratFromInt(new Date(dateToUnixDays(d) * 86400000).getUTCMonth() + 1),
      __l4_date_year: (d) =>
        ratFromInt(new Date(dateToUnixDays(d) * 86400000).getUTCFullYear()),
      __l4_datevalue: (sPtrF) => {
        const v = parseDateStr(readCString(Number(f64ToU64(sPtrF))));
        return Number.isFinite(v) ? mbJustF64(v) : mbNothing();
      },
      __l4_to_date: (sPtrF) => {
        const v = parseDateStr(readCString(Number(f64ToU64(sPtrF))));
        return Number.isFinite(v) ? mbJustF64(v) : mbNothing();
      },
      // TIME — TIME values are raw f64 seconds; NUMBER args/results are handles.
      __l4_time_from_hms: (hH, mH, sH) =>
        timeFromHMS(
          Number(numToInt(hH, "TIME_FROM_HMS")),
          Number(numToInt(mH, "TIME_FROM_HMS")),
          // seconds may be fractional in jl4-core; drop to Double.
          numToF64(sH),
        ),
      __l4_time_from_serial: (nH) => numToF64(nH),
      __l4_time_serial: (t) => f64ToNum(Number(t)),
      __l4_time_hour: (t) => ratFromInt(Math.floor(Number(t) / 3600)),
      __l4_time_minute: (t) => ratFromInt(Math.floor((Number(t) % 3600) / 60)),
      __l4_time_second: (t) => f64ToNum(Number(t) % 60),
      __l4_to_time: (sPtrF) => {
        const v = parseTimeStr(readCString(Number(f64ToU64(sPtrF))));
        return Number.isFinite(v) ? mbJustF64(v) : mbNothing();
      },
      // DATETIME — DATETIME is a pointer (bit-cast to f64); the NUMBER
      // serial it can produce is a handle.
      __l4_datetime_from_dtz: (d, t, tz) => datetimeFromDTZ(d, t, tz),
      __l4_datetime_date: (dtF) => {
        const p = Number(f64ToU64(dtF));
        if (!p) return 0;
        return (
          Math.floor(memView.getFloat64(p, true) / 86400) + DATE_EPOCH_SHIFT
        );
      },
      __l4_datetime_time: (dtF) => {
        const p = Number(f64ToU64(dtF));
        if (!p) return 0;
        const sec = memView.getFloat64(p, true);
        return ((sec % 86400) + 86400) % 86400;
      },
      __l4_datetime_tz: (dtF) => {
        const p = Number(f64ToU64(dtF));
        if (!p) return u64ToF64(writeString(""));
        return u64ToF64(Number(memView.getBigUint64(p + 8, true)));
      },
      __l4_datetime_serial: (dtF) => {
        const p = Number(f64ToU64(dtF));
        if (!p) return ratFromInt(0);
        return f64ToNum(memView.getFloat64(p, true));
      },
      __l4_to_datetime: (sPtrF) => {
        const s = readCString(Number(f64ToU64(sPtrF)));
        const t = Date.parse(s);
        if (Number.isNaN(t)) return mbNothing();
        const p = allocBytes(16);
        memView.setFloat64(p, t / 1000, true);
        memView.setBigUint64(p + 8, BigInt(writeString("UTC")), true);
        const recPtr = u64ToF64(p);
        const jp = allocBytes(16);
        memView.setFloat64(jp, 1.0, true);
        memView.setFloat64(jp + 8, recPtr, true);
        return u64ToF64(jp);
      },
      // Extended numeric intrinsics — inherently inexact: drop to f64 via the
      // ratToDouble shim (matches jl4-core, which itself does `fromRational ::
      // Double` for these), then re-box the exact rational of the result.
      __l4_sqrt: (xF) => f64ToNum(Math.sqrt(numToF64(xF))),
      __l4_ln: (xF) => f64ToNum(Math.log(numToF64(xF))),
      __l4_log10: (xF) => f64ToNum(Math.log10(numToF64(xF))),
      __l4_sin: (xF) => f64ToNum(Math.sin(numToF64(xF))),
      __l4_cos: (xF) => f64ToNum(Math.cos(numToF64(xF))),
      __l4_tan: (xF) => f64ToNum(Math.tan(numToF64(xF))),
      __l4_asin: (xF) => f64ToNum(Math.asin(numToF64(xF))),
      __l4_acos: (xF) => f64ToNum(Math.acos(numToF64(xF))),
      __l4_atan: (xF) => f64ToNum(Math.atan(numToF64(xF))),
      // TRUNC(value, digits): jl4-core does `truncate (value * 10^d) / 10^d`
      // for d >= 0 (and the inverse for negative d). `truncate` is
      // toward-zero, NOT floor. `digits` is banker-rounded to an Integer.
      __l4_trunc: (xF, dH) => {
        const digitsInt = ratRound(ratUnbox(dH)).num; // BigInt, possibly negative
        const v = ratUnbox(xF);
        if (digitsInt >= 0n) {
          const factor = makeRat(10n ** digitsInt, 1n);
          return ratBox(ratDiv(ratTrunc(ratMul(v, factor)), factor));
        }
        const factor = makeRat(10n ** -digitsInt, 1n);
        return ratBox(ratMul(ratTrunc(ratDiv(v, factor)), factor));
      },
      __l4_is_integer: (xF) => (isInteger(ratUnbox(xF)) ? 1 : 0),
      // String intrinsics (all string args arrive as f64-bitcast pointers).
      // STRINGLENGTH / INDEXOF return NUMBER → handle.
      __l4_string_length: (sF) =>
        ratFromInt(readCString(Number(f64ToU64(sF))).length),
      __l4_to_upper: (sF) =>
        u64ToF64(writeString(readCString(Number(f64ToU64(sF))).toUpperCase())),
      __l4_to_lower: (sF) =>
        u64ToF64(writeString(readCString(Number(f64ToU64(sF))).toLowerCase())),
      __l4_trim: (sF) =>
        u64ToF64(writeString(readCString(Number(f64ToU64(sF))).trim())),
      __l4_contains: (hF, nF) =>
        readCString(Number(f64ToU64(hF))).includes(
          readCString(Number(f64ToU64(nF))),
        )
          ? 1
          : 0,
      __l4_starts_with: (hF, nF) =>
        readCString(Number(f64ToU64(hF))).startsWith(
          readCString(Number(f64ToU64(nF))),
        )
          ? 1
          : 0,
      __l4_ends_with: (hF, nF) =>
        readCString(Number(f64ToU64(hF))).endsWith(
          readCString(Number(f64ToU64(nF))),
        )
          ? 1
          : 0,
      __l4_index_of: (hF, nF) =>
        ratFromInt(
          readCString(Number(f64ToU64(hF))).indexOf(
            readCString(Number(f64ToU64(nF))),
          ),
        ),
      __l4_char_at: (sF, iH) =>
        u64ToF64(
          writeString(
            readCString(Number(f64ToU64(sF))).charAt(
              Number(numToInt(iH, "CHARAT")),
            ),
          ),
        ),
      __l4_substring: (sF, iH, jH) =>
        u64ToF64(
          writeString(
            readCString(Number(f64ToU64(sF))).substring(
              Number(numToInt(iH, "SUBSTRING")),
              Number(numToInt(jH, "SUBSTRING")),
            ),
          ),
        ),
      __l4_replace: (sF, aF, bF) => {
        const s = readCString(Number(f64ToU64(sF)));
        const a = readCString(Number(f64ToU64(aF)));
        const b = readCString(Number(f64ToU64(bF)));
        return u64ToF64(writeString(s.split(a).join(b)));
      },
      // TONUMBER: STRING → MAYBE NUMBER. Parse the string text exactly into
      // a rational (no JS Number detour) so "0.1" → 1/10, matching how
      // jl4-core would parse a numeric literal.
      __l4_to_number: (sF) => {
        const s = readCString(Number(f64ToU64(sF)));
        try {
          const r = ratFromDecimalString(s);
          return mbJustF64(ratBox(r));
        } catch {
          return mbNothing();
        }
      },
      // Nullary temporal — per typechecker, TODAY: DATE, NOW: DATETIME,
      // CURRENTTIME: TIME — all *non-NUMBER*, so they stay raw f64 / pointer.
      __l4_today_serial: () =>
        Math.floor(Date.now() / 86400000) + DATE_EPOCH_SHIFT,
      __l4_now_serial: () => Date.now() / 1000,
      __l4_current_time: () => {
        const d = new Date();
        return (
          d.getUTCHours() * 3600 + d.getUTCMinutes() * 60 + d.getUTCSeconds()
        );
      },
      __l4_timezone_name: () => u64ToF64(writeString("UTC")),
      // JSON: jl4-core's JSON_ENCODE on a NUMBER renders integers exact and
      // non-integers as the shortest-round-trip Double — same rule as the
      // top-level JSON marshaller (ratToJSONValue).
      __l4_json_encode: (vF) =>
        u64ToF64(writeString(JSON.stringify(ratToJSONValue(ratUnbox(vF))))),
      // M5 slice 2B — trace ABI. The instrumented `<fn>$trace` codegen
      // passes nodeId / kind as plain f64 *values* (the lowering emits
      // `arith.constant 0.0 : f64`, etc.), not as f64-encoded bit patterns
      // — so we read them back with `Number`, not `f64ToU64`. Result boxes
      // stay raw f64 (could be a rational handle, a boolean 0/1, a pointer
      // bit-pattern) and the serialiser decides how to render based on
      // the kind byte the metadata + lowering agreed on.
      __l4_trace_enter: (nodeIdF) => {
        tracePool.enter(Number(nodeIdF) | 0);
      },
      __l4_trace_exit: (resultRawF, kindF) => {
        tracePool.exit(resultRawF, Number(kindF) | 0);
      },
      // M5 slice 4A — per-function context so node IDs resolve against
      // the right `traceMeta.nodes` table. Each `<fn>$trace` calls
      // `enter_fn` at start (with a wasm-symbol string pointer) and
      // `exit_fn` at return.
      __l4_trace_enter_fn: (symbolPtrF) => {
        const sym = readCString(Number(f64ToU64(symbolPtrF)));
        tracePool.pushFn(sym);
      },
      __l4_trace_exit_fn: () => {
        tracePool.popFn();
      },
      // M5 slice 4E — record that the wasm body just forced
      // `field` on the record at `wasmPtr`. The marshaler registered
      // each compound JSON arg's wasm address in `valueByPtr`; we
      // look up the JS object and attach `__forced.add(field)` so the
      // renderer can read it later. Helper calls share pointers with
      // their callers (the wasm passes the same address), so this
      // works without any translation layer.
      __l4_mark_forced: (valuePtrF, fieldPtrF) => {
        const wp = Number(f64ToU64(valuePtrF));
        const entry = valueByPtr.get(wp);
        const field = readCString(Number(f64ToU64(fieldPtrF)));
        if (!entry || !entry.value || !field) return;
        if (!entry.value.__forced) entry.value.__forced = new Set();
        entry.value.__forced.add(field);
      },
      // Slice-4D push/pop builtins kept as no-ops (declared in
      // Runtime.Builtins, the wasm module still imports them while
      // Lower.hs is mid-migration) so older bundles don't trap.
      __l4_trace_push_arg_path: () => {},
      __l4_trace_pop_arg_paths: () => {},
      __l4_json_decode: (sF) => {
        try {
          const txt = readCString(Number(f64ToU64(sF)));
          const v = JSON.parse(txt);
          // For a number, parse the *text* form (not the JS Number) so we
          // preserve the same exactness guarantee as TONUMBER.
          if (typeof v === "number") {
            const m = /-?\d+(?:\.\d+)?(?:[eE][+-]?\d+)?/.exec(txt);
            return mbJustF64(
              ratBox(ratFromDecimalString(m ? m[0] : String(v))),
            );
          }
          return mbNothing();
        } catch {
          return mbNothing();
        }
      },
    };
    return {
      env: new Proxy(env, {
        get(target, prop) {
          if (prop in target) return target[prop];
          return () => {
            throw new Error(
              "l4: unresolved runtime symbol '" +
                String(prop) +
                "' (higher-order / unsupported library feature)",
            );
          };
        },
      }),
    };
  }

  // ---- High-level helper: marshal args, call an exported function,
  //      decode the return per the schema's return type. ----
  function invokeFunction(instance, meta, args) {
    resetHeap();
    // M6 — deontic functions never invoke the wasm body. The JS
    // interpreter walks 'meta.deonticContract' against the request's
    // 'startTime' + 'events' fields and returns a wire value the
    // envelope wrapper can drop into 'SimpleResponse'.
    if (meta.isDeontic) {
      return invokeDeontic(meta, args);
    }
    const props = meta.parameters.properties || {};
    const order = meta.paramOrder || Object.keys(props);
    const required = new Set(meta.parameters.required || order);

    const marshaled = order.map((pn) => {
      const fs_ = props[pn] || {};
      const v = args[pn];
      const isMaybe = !required.has(pn);
      const isEnum = !!(fs_.enum && fs_.enum.length > 0);
      let m;
      if (isMaybe) m = marshalMaybe(v, fs_);
      else m = marshalArg(v, fs_);
      if (isMaybe || isPointerSchema(fs_)) return u64ToF64(m);
      if (fs_.type === "number" || isEnum) return Number(m);
      return Number(m);
    });
    const fn = instance.exports[meta.wasmSymbol];
    const raw = fn(...marshaled);
    return unmarshalResult(raw, meta.returnType);
  }

  // ---- M6: deontic dispatch -------------------------------------
  // The args may carry @startTime@ and @events@ either at the top
  // level (jl4-service shape) or nested under @arguments@; the HTTP
  // wrapper hands us the request body's @arguments@ field already,
  // which conventionally only carries the GIVEN parameters — so
  // 'startTime' / 'events' live on the *envelope*. To keep
  // 'invokeFunction' callable from both shapes, look for the fields
  // on @args@ first.
  function invokeDeontic(meta, args) {
    if (!meta.deonticContract) {
      throw new DeonticInputError(
        "deontic function has no compiled contract metadata",
      );
    }
    const startTime = args.startTime;
    const events = args.events;
    if (startTime === undefined || startTime === null) {
      throw new DeonticInputError(
        "startTime is required for functions returning DEONTIC",
      );
    }
    if (events === undefined || events === null) {
      throw new DeonticInputError(
        "events is required for functions returning DEONTIC",
      );
    }
    // Pass the full args bag so 'runDeontic' can resolve 'DEParam'
    // references inside the contract (party / action / IF guard)
    // against the request's GIVEN parameter values. Also forward
    // 'meta' so the interpreter can look up param types (e.g.
    // 'x-l4-type') for record-typed party serialization.
    return runDeontic(meta.deonticContract, startTime, events, args, meta);
  }

  // ---- M5 slice 2B: trace-mode invocation via the instrumented `<fn>$trace`.
  //
  // If the wasm module exports a `<fn>$trace` symbol we route to it: the
  // compiled body calls `__l4_trace_enter/_exit` at every traceable
  // subexpression, and the runtime's `tracePool` ends up holding the body's
  // Reasoning tree. We then wrap it with the synthesised top + fn-value
  // nodes (using `traceMeta.fnName`) to match jl4-service's
  // `traceToReasoning` shape.
  //
  // If `<fn>$trace` doesn't exist (older bundles, unsupported fn), fall
  // back to the slice-1/2A degenerate synthesised one-node Reasoning. That
  // way the wasm-server keeps returning a TraceResponse envelope even when
  // we can't actually trace.
  // M5 slice 4A — node-table-by-fn-symbol, populated from the schema
  // bundle so the runtime can resolve per-frame node lookups even when
  // `<fn>$trace` recurses into a different function. Set via
  // `setBundleFunctions`; defaults to empty (single-function callers).
  let nodesByFn = {};
  function setBundleFunctions(fnsByName) {
    nodesByFn = {};
    for (const [name, m] of Object.entries(fnsByName || {})) {
      const sym = m.wasmSymbol || name;
      const ns = (m.traceMeta && m.traceMeta.nodes) || [];
      // Match Lower.hs's @__l4_trace_enter_fn@ which always interns
      // @<funcName>__<postClosureArity>@. For exports the arity =
      // paramOrder length (minus the two deontic extras when
      // applicable). For ASSUME-derived extra args the lowering
      // appends to the parameter list before computing length, but
      // we keep the simpler 'paramOrder' length here since exports
      // never overload on the schema side.
      const order = m.paramOrder || [];
      const deonticExtras = m.isDeontic ? 2 : 0;
      const arity = Math.max(0, order.length - deonticExtras);
      nodesByFn[sym + "__" + arity] = ns;
    }
  }
  // M5 — helpers (non-@export'd Decides) get @<fn>$trace@ variants
  // too, and their trace_enter calls reference their own node tables.
  // Without this map, nested calls into a helper resolve node IDs
  // against the outer function's table and render garbage.
  function setBundleHelperTraceMeta(helpersByWasmSymbol) {
    for (const [sym, tm] of Object.entries(helpersByWasmSymbol || {})) {
      nodesByFn[sym] = (tm && tm.nodes) || [];
    }
  }

  function invokeFunctionWithReasoning(instance, meta, args) {
    // M6 — deontic functions skip wasm entirely; the JS interpreter
    // owns the value and the reasoning. For now we don't emit a
    // per-event trace tree (jl4-service's deontic responses are
    // SimpleResponse without reasoning), so 'reasoning' is empty.
    if (meta.isDeontic) {
      return { value: invokeDeontic(meta, args), reasoning: null };
    }
    const traceSymbol = (meta.wasmSymbol || "") + "$trace";
    if (typeof instance.exports[traceSymbol] === "function") {
      const value = invokeFunctionRaw(instance, meta, args, traceSymbol);
      // M5 slice 4A — the `<fn>$trace` codegen pushes one frame per
      // top-level event (fn-value + body root), so we hand all roots
      // to the renderer instead of just the last one.
      const traceRoots = tracePool.roots;
      const reasoning = buildReasoningFromTrace(meta, args, value, traceRoots);
      return { value, reasoning };
    }
    const value = invokeFunction(instance, meta, args);
    const reasoning = synthesizeReasoning(meta, args, value);
    return { value, reasoning };
  }

  // `invokeFunction` factored to take an explicit wasm symbol so we can
  // route to `<fn>$trace` without duplicating the marshalling pipeline.
  function invokeFunctionRaw(instance, meta, args, wasmSymbol) {
    resetHeap();
    const props = meta.parameters.properties || {};
    const order = meta.paramOrder || Object.keys(props);
    const required = new Set(meta.parameters.required || order);
    const marshaled = order.map((pn) => {
      const fs_ = props[pn] || {};
      const v = args[pn];
      const isMaybe = !required.has(pn);
      const isEnum = !!(fs_.enum && fs_.enum.length > 0);
      let m;
      if (isMaybe) m = marshalMaybe(v, fs_);
      else m = marshalArg(v, fs_);
      if (isMaybe || isPointerSchema(fs_)) return u64ToF64(m);
      if (fs_.type === "number" || isEnum) return Number(m);
      return Number(m);
    });
    const fn = instance.exports[wasmSymbol];
    const raw = fn(...marshaled);
    return unmarshalResult(raw, meta.returnType);
  }

  // Turn a `tracePool.root` frame (whose nodes carry raw result boxes and
  // a kindByte) into a `Reasoning` value byte-compatible with
  // jl4-service's `traceToReasoning` for the corresponding L4 expression.
  // The compile-time `traceMeta.nodes[]` array supplies `exampleCode` per
  // node; the result kind comes from `traceMeta.nodes[id].resultKind` so
  // the runtime can render bool/integer/double-rational/string/maybe the
  // same way `Print.prettyLayout NF` does.
  function buildReasoningFromTrace(meta, args, value, traceRoots) {
    // M5 slice 4A — node-lookup must be PER-FRAME: the frame's `fn`
    // field (set when `__l4_trace_enter_fn` fired) tells us whose
    // `traceMeta.nodes` to consult. Without this, a frame pushed inside
    // a recursive `is_eligible$trace` call would resolve its node ID
    // against `calculate-bonus`'s table and render the wrong text.
    const lookupNode = (frame) => {
      const fnNodes =
        (frame.fn && nodesByFn[frame.fn]) ||
        (meta.traceMeta && meta.traceMeta.nodes) ||
        [];
      return fnNodes[frame.nodeId] || {};
    };
    // Render one trace frame into a Reasoning node. For nodes whose
    // compile-time metadata has `special` set (M5 slice 4A), we patch
    // the children list to match what `traceToReasoning` produces:
    //
    //   * short-circuit filter — drop the rhs sibling when the prelude
    //     would have skipped it (AND with FALSE lhs, OR with TRUE lhs);
    //     our wasm codegen is eager so both frames exist in the pool.
    //   * synthetic IF sub-tree — `__AND__ a b` desugars to
    //     @IF a THEN b ELSE FALSE@; jl4-service's trace shows this IF as
    //     an additional child of the AND node, with `a`/`b` Var leaves.
    //     Mirror the same shape here.
    // M5 — second arg is the label inherited from an enclosing
    // WHERE/LET binding wrapper. jl4-core's `traceToReasoning`
    // propagates the wrapper's label down through the LAST
    // (expr, kids) pair of each Trace via its recursive rewrite —
    // visible in the wire shape as @["income", "max OF 0, …"]@,
    // @["income", "IF …"]@, @["income", "x"]@ etc. all nested under
    // an @income@ binding. Mirror it here: a frame inherits the
    // current label, then passes the label only to the LAST child
    // (the "body" position in the LAST (expr, kids) pair).
    const renderFrame = (frame, inheritedLabel) => {
      const node = lookupNode(frame);
      const exampleCode = node.exampleCode || "";
      // The schema's per-node `resultKind` is the primary source of truth,
      // but for `App _ _ []` (Var) leaves the schema can't always pin down
      // the right kind when the helper's params are un-annotated (e.g.
      // prelude's @go acc l MEANS …@) — it falls back to kind 3. The
      // Lowering refines those cases at @emitTraceExit@ time using the
      // binding's MLIR type; honor the refined kind here when the schema
      // emitted the default 3.
      const wasmKind =
        frame.result && typeof frame.result.kind === "number"
          ? frame.result.kind
          : undefined;
      const effectiveKind =
        node.resultKind === 3 && wasmKind !== undefined && wasmKind !== 3
          ? wasmKind
          : node.resultKind;
      const resultText = renderTraceResult(
        frame.result,
        effectiveKind,
        node.returnSchema,
      );
      // M5 slice 4G — Proj desugars into a 4-level sub-tree
      //   record's field
      //     └── App-form: field OF record
      //           ├── (record's arg-eval — this frame's actual kids)
      //           ├── field (fn-value, Result: <function>)
      //           └── CONSIDER <Ty> WHEN <Ty> <fields> THEN field
      //                 └── field (bound-var, same Result as outer)
      // The kids from the wasm-side instrumentation (the record's
      // evaluation sub-tree, e.g. the inner Proj for nested cases)
      // move INSIDE the App-form's children, matching the order
      // jl4-core's `traceToReasoning` produces.
      if (node.special === "PROJ" && node.proj) {
        const proj = node.proj;
        const argEvalChildren = frame.children.map((c) => renderFrame(c));
        return {
          exampleCode: [exampleCode],
          explanation: ["Result: " + resultText],
          children: [
            {
              exampleCode: [proj.appForm],
              explanation: ["Result: " + resultText],
              children: [
                ...argEvalChildren,
                {
                  exampleCode: [proj.fieldName],
                  explanation: ["Result: <function>"],
                  children: [],
                },
                {
                  exampleCode: [proj.considerEx],
                  explanation: ["Result: " + resultText],
                  children: [
                    {
                      exampleCode: [proj.fieldName],
                      explanation: ["Result: " + resultText],
                      children: [],
                    },
                  ],
                },
              ],
            },
          ],
        };
      }
      let kidFrames = frame.children;
      let extras = [];
      if (node.special === "AND" || node.special === "OR") {
        const filtered = filterShortCircuitChildren(frame, node, lookupNode);
        kidFrames = filtered.kids;
        extras = synthesizeBoolDesugar(node, filtered, resultText, lookupNode);
      } else if (node.special === "NOT") {
        extras = synthesizeNotDesugar(frame, node, resultText, lookupNode);
      }
      // Effective label for THIS frame: the schema's bindingLabel if
      // the frame is a wrapper, else whatever the parent passed down.
      // The label gets prepended to this frame's exampleCode.
      const effectiveLabel = node.bindingLabel || inheritedLabel || null;
      const ec = effectiveLabel ? [effectiveLabel, exampleCode] : [exampleCode];
      // Pass the label down to the LAST child only — mirroring the
      // recursive rewrite in jl4-core's @traceToReasoning@ that puts
      // a fresh labeled Trace at the tail of the @kids@ list. fn-value
      // / argument-eval children sit at earlier indices and don't
      // inherit; the body trace sits at the end and does.
      const renderedKids = kidFrames.map((c, i) =>
        renderFrame(c, i === kidFrames.length - 1 ? effectiveLabel : null),
      );
      return {
        exampleCode: ec,
        explanation: ["Result: " + resultText],
        children: [...renderedKids, ...extras],
      };
    };
    // Synthesised top: `<fn name> OF <args>` Result: …. Children list
    // mirrors jl4-service's `traceToReasoning` shape for a function
    // application: [arg-eval-1, …, arg-eval-N, fn-value, body]. The
    // arg-eval subtrees come from `synthesizeArgEvalTree` (compound
    // args only — primitives correspond to dropped `Lit` traces). The
    // fn-value + body frames come straight from `tracePool.roots` —
    // since slice 4A, the `<fn>$trace` codegen emits the fn-value
    // frame itself, so we no longer synthesise one here.
    const fnLabel =
      (meta.traceMeta && meta.traceMeta.fnName) ||
      meta.apiName ||
      meta.name ||
      "";
    const argsPretty = renderArgList(meta, args);
    const topExample =
      argsPretty.length > 0 ? `${fnLabel} OF ${argsPretty}` : fnLabel;
    const props = (meta.parameters && meta.parameters.properties) || {};
    const order = meta.paramOrder || Object.keys(props);
    // M5 slice 4E — arg-eval renderer reads `__forced` directly off
    // each JS object (attached by the marshaler + populated by the
    // wasm body's __l4_mark_forced calls). No path/forced opts needed.
    const argEvalTrees = [];
    for (const pn of order) {
      const tree = synthesizeArgEvalTree(args[pn], props[pn] || {});
      if (tree) argEvalTrees.push(tree);
    }
    // Defensive: if the wasm didn't actually emit any trace frames
    // (e.g. older bundle without slice-4A fn-value), fall back to a
    // synthesised one-node body frame.
    const rootFrames =
      traceRoots && traceRoots.length > 0
        ? traceRoots.map((f) => renderFrame(f))
        : [
            {
              exampleCode: [fnLabel],
              explanation: ["Result: <function>"],
              children: [],
            },
            {
              exampleCode: [(meta.traceMeta && meta.traceMeta.body) || ""],
              explanation: [
                "Result: " + prettyResultText(value, meta.returnType),
              ],
              children: [],
            },
          ];
    return {
      exampleCode: [topExample],
      explanation: ["Result: " + prettyResultText(value, meta.returnType)],
      children: [...argEvalTrees, ...rootFrames],
    };
  }

  // M5 slice 4A — read a boolean truth value from a frame's raw result
  // box, taking the metadata's `resultKind` into account so we can tell
  // an AND-truth from, say, a NUMBER handle that happens to have a 0/1
  // bit-pattern. Returns null for "couldn't determine", which the
  // short-circuit filter treats conservatively (no filter applied).
  function frameTruth(frame, lookupNode) {
    if (!frame || !frame.result) return null;
    const fnode = lookupNode(frame);
    const kind = typeof fnode.resultKind === "number" ? fnode.resultKind : 3;
    if (kind === 1) return Number(frame.result.raw) !== 0; // BOOLEAN
    // For non-bool kinds we can't reason about short-circuit; let the
    // child stay in the trace.
    return null;
  }

  // M5 slice 4A — drop the rhs frame when the prelude would have
  // short-circuited. Our wasm codegen evaluates AND/OR eagerly so both
  // arg frames exist in the pool; this filter brings the trace shape
  // back in line with jl4-service's lazy evaluation order.
  function filterShortCircuitChildren(frame, node, lookupNode) {
    const kids = frame.children;
    if (kids.length < 1) return { kids, dropped: false, lhsTruth: null };
    // M5 slice 4D — always read lhs from the FIRST child if present.
    // With slice-4D's short-circuit AND/OR codegen, the rhs frame is
    // absent (only 1 child); we still need lhsTruth to drive the IF
    // sub-tree's taken-branch leaf (FALSE / TRUE / b).
    const lhsTruth = frameTruth(kids[0], lookupNode);
    if (kids.length < 2) return { kids, dropped: false, lhsTruth };
    let kept = kids;
    let dropped = false;
    if (node.special === "AND" && lhsTruth === false) {
      kept = kids.slice(0, 1);
      dropped = true;
    } else if (node.special === "OR" && lhsTruth === true) {
      kept = kids.slice(0, 1);
      dropped = true;
    }
    return { kids: kept, dropped, lhsTruth };
  }

  // M5 slice 4A — append the synthetic IF sub-tree the prelude desugar
  // for @__AND__@ / @__OR__@ produces in jl4-service's trace.
  //
  //   __AND__ a b ⟶ IF a THEN b ELSE FALSE
  //   __OR__  a b ⟶ IF a THEN TRUE ELSE b
  //
  // The IF sub-tree's "a" and "b" leaves use the *evaluated* truths
  // from the AND/OR's sibling sub-traces — the lambda body sees @a@
  // and @b@ already reduced to WHNF. When short-circuit dropped the
  // rhs, we drop the corresponding child from the IF sub-tree too.
  function synthesizeBoolDesugar(node, filtered, parentResultText, lookupNode) {
    const kids = filtered.kids;
    if (kids.length < 1) return [];
    const lhsTruth = filtered.lhsTruth;
    const rhsTruth = kids.length >= 2 ? frameTruth(kids[1], lookupNode) : null;
    const aText =
      lhsTruth === true ? "TRUE" : lhsTruth === false ? "FALSE" : "";
    const bText =
      rhsTruth === true ? "TRUE" : rhsTruth === false ? "FALSE" : "";
    // IF sub-tree's exampleCode literal differs by operator. AND emits
    // `IF a THEN b ELSE FALSE`; OR emits `IF a THEN TRUE ELSE b`.
    const ifText =
      node.special === "AND"
        ? "IF a THEN b ELSE FALSE"
        : "IF a THEN TRUE ELSE b";
    const ifChildren = [
      { exampleCode: ["a"], explanation: ["Result: " + aText], children: [] },
    ];
    // The second IF child is the *taken* branch:
    //   AND, lhs TRUE  → `b` (the rhs eval)            ⇒ Result = b's truth
    //   AND, lhs FALSE → `FALSE` (the ELSE literal)    ⇒ Result = FALSE
    //   OR,  lhs TRUE  → `TRUE`  (the THEN literal)    ⇒ Result = TRUE
    //   OR,  lhs FALSE → `b` (the rhs eval)            ⇒ Result = b's truth
    // jl4-service traces the taken branch as a leaf node with the
    // branch expression as exampleCode and its NF result as the line.
    if (node.special === "AND") {
      if (lhsTruth === true && kids.length >= 2) {
        ifChildren.push({
          exampleCode: ["b"],
          explanation: ["Result: " + bText],
          children: [],
        });
      } else if (lhsTruth === false) {
        ifChildren.push({
          exampleCode: ["FALSE"],
          explanation: ["Result: FALSE"],
          children: [],
        });
      }
    } else {
      // OR
      if (lhsTruth === true) {
        ifChildren.push({
          exampleCode: ["TRUE"],
          explanation: ["Result: TRUE"],
          children: [],
        });
      } else if (lhsTruth === false && kids.length >= 2) {
        ifChildren.push({
          exampleCode: ["b"],
          explanation: ["Result: " + bText],
          children: [],
        });
      }
    }
    return [
      {
        exampleCode: [ifText],
        explanation: ["Result: " + parentResultText],
        children: ifChildren,
      },
    ];
  }

  // M5 slice 4A — synthetic IF sub-tree for `__NOT__ a` ⟶
  // `IF a THEN FALSE ELSE TRUE`. Single arg, no short-circuit.
  function synthesizeNotDesugar(frame, node, parentResultText, lookupNode) {
    if (frame.children.length < 1) return [];
    const argTruth = frameTruth(frame.children[0], lookupNode);
    const aText =
      argTruth === true ? "TRUE" : argTruth === false ? "FALSE" : "";
    // Mirror 'synthesizeBoolDesugar' for AND/OR — the IF sub-tree
    // includes a 'taken-branch' leaf after the @a@ input leaf:
    //   argTruth === true  → @FALSE@ (the THEN literal)
    //   argTruth === false → @TRUE@  (the ELSE literal)
    const ifChildren = [
      { exampleCode: ["a"], explanation: ["Result: " + aText], children: [] },
    ];
    if (argTruth === true) {
      ifChildren.push({
        exampleCode: ["FALSE"],
        explanation: ["Result: FALSE"],
        children: [],
      });
    } else if (argTruth === false) {
      ifChildren.push({
        exampleCode: ["TRUE"],
        explanation: ["Result: TRUE"],
        children: [],
      });
    }
    return [
      {
        exampleCode: ["IF a THEN FALSE ELSE TRUE"],
        explanation: ["Result: " + parentResultText],
        children: ifChildren,
      },
    ];
  }

  // M5 — walk a wasm-allocated compound value at `raw` using the
  // compile-time 'RetSchema' the schema attached to this trace node.
  // Returns @{ ok: true, value, schema }@ where @value@ is a JS object
  // shaped for 'prettyL4Value' (and @schema@ is the matching Parameter
  // shape), or @{ ok: false }@ if the schema/layout don't decode
  // cleanly (e.g. a record field's schema is absent, or a string
  // pointer reads past the heap).
  //
  // Layout must match what marshalArg / marshalStruct / marshalList /
  // marshalMaybe emit:
  //   * RSScalar   — the raw f64 IS the value.
  //   * RSMaybe    — `raw` is f64-encoded pointer to 16 bytes:
  //                  tag (f64, 0 = NOTHING) + payload (8B; decode per inner).
  //   * RSList     — `raw` is f64-encoded pointer to cons-cell head:
  //                  { item (8B), tail-ptr (8B) }, 0 = end of list.
  //   * RSRecord   — `raw` is f64-encoded pointer to a struct: 8 bytes
  //                  per field in `fieldOrder`, decoded per the per-field
  //                  schema entry in `fields`.
  function walkWasmValue(raw, schema) {
    if (!schema) return { ok: false };
    switch (schema.kind) {
      case "scalar":
        return walkScalar(raw, schema.type);
      case "enum": {
        // Enum tags are stored as the integer constructor index
        // (mirroring 'lookupEnumTag' in Lower.hs). Read the index off
        // the raw f64 directly and look up the constructor's source
        // name from the schema's @values@ array; prettyL4Value
        // renders an enum as a bare name when it sees @enum: […]@.
        const idx = Math.trunc(Number(raw));
        const values = schema.values || [];
        return {
          ok: true,
          value: values[idx] != null ? values[idx] : String(idx),
          schema: { type: "string", enum: values },
        };
      }
      case "maybe":
        return walkMaybe(raw, schema.inner);
      case "list":
        return walkList(raw, schema.items);
      case "record":
        return walkRecord(raw, schema);
      default:
        return { ok: false };
    }
  }

  function walkScalar(raw, type) {
    // Most scalar trace results never reach walkWasmValue because the
    // outer dispatch on kindByte handles them (kind 0/1/2 paths). This
    // branch covers a record/MAYBE/list field whose payload is scalar.
    if (type === "number") {
      try {
        const r = ratUnbox(raw);
        // ratToAesonValue keeps integers as JS numbers and tags
        // non-integers via aesonDouble — matches prettyL4Value's
        // expectation for record fields.
        return {
          ok: true,
          value: ratToAesonValue(r),
          schema: { type: "number" },
        };
      } catch {
        return { ok: false };
      }
    }
    if (type === "boolean") {
      return {
        ok: true,
        value: Number(raw) !== 0,
        schema: { type: "boolean" },
      };
    }
    if (type === "string") {
      const p = Number(f64ToU64(raw));
      if (!p) return { ok: false };
      return { ok: true, value: readCString(p), schema: { type: "string" } };
    }
    return { ok: false };
  }

  function walkMaybe(raw, innerSchema) {
    const p = Number(f64ToU64(raw));
    if (!p) {
      return { ok: true, value: "NOTHING", schema: undefined };
    }
    const tag = memView.getFloat64(p, true);
    if (tag === 0.0) {
      return { ok: true, value: "NOTHING", schema: undefined };
    }
    // Decode the payload slot using the inner schema's layout. Scalars
    // and MAYBE-of-scalar account for everything in the test corpus;
    // records/lists are handled by reading the slot as a pointer.
    let inner;
    if (!innerSchema) return { ok: false };
    if (innerSchema.kind === "scalar") {
      if (innerSchema.type === "number") {
        const handleF = memView.getFloat64(p + 8, true);
        try {
          inner = ratToAesonValue(ratUnbox(handleF));
        } catch {
          return { ok: false };
        }
      } else if (innerSchema.type === "boolean") {
        inner = memView.getFloat64(p + 8, true) !== 0;
      } else if (innerSchema.type === "string") {
        const sp = Number(memView.getBigUint64(p + 8, true));
        inner = sp ? readCString(sp) : "";
      } else {
        inner = memView.getFloat64(p + 8, true);
      }
    } else if (
      innerSchema.kind === "record" ||
      innerSchema.kind === "list" ||
      innerSchema.kind === "maybe"
    ) {
      const innerPtrF = memView.getFloat64(p + 8, true);
      const innerWalked = walkWasmValue(innerPtrF, innerSchema);
      if (!innerWalked.ok) return { ok: false };
      inner = innerWalked.value;
    } else {
      return { ok: false };
    }
    return { ok: true, value: { JUST: [inner] }, schema: undefined };
  }

  function walkList(raw, itemSchema) {
    let head = Number(f64ToU64(raw));
    const items = [];
    const itemSchemaForRender = retSchemaToParameter(itemSchema);
    let guard = 0;
    while (head && guard++ < 1_000_000) {
      let item;
      if (itemSchema && itemSchema.kind === "scalar") {
        if (itemSchema.type === "number") {
          const handleF = memView.getFloat64(head, true);
          try {
            item = ratToAesonValue(ratUnbox(handleF));
          } catch {
            return { ok: false };
          }
        } else if (itemSchema.type === "boolean") {
          item = memView.getFloat64(head, true) !== 0;
        } else if (itemSchema.type === "string") {
          const sp = Number(memView.getBigUint64(head, true));
          item = sp ? readCString(sp) : "";
        } else {
          item = memView.getFloat64(head, true);
        }
      } else {
        // record / list / maybe: the slot is itself a pointer-shaped f64
        const subF = memView.getFloat64(head, true);
        const sub = walkWasmValue(subF, itemSchema);
        if (!sub.ok) return { ok: false };
        item = sub.value;
      }
      items.push(item);
      head = Number(memView.getBigUint64(head + 8, true));
    }
    return {
      ok: true,
      value: items,
      schema: { type: "array", items: itemSchemaForRender },
    };
  }

  function walkRecord(raw, schema) {
    const ptr = Number(f64ToU64(raw));
    if (!ptr) return { ok: false };
    const order = schema.fieldOrder || [];
    const fields = schema.fields || {};
    const value = {};
    for (let i = 0; i < order.length; i++) {
      const fname = order[i];
      const fschema = fields[fname];
      if (!fschema) return { ok: false };
      const slot = ptr + i * 8;
      if (fschema.kind === "scalar") {
        if (fschema.type === "number") {
          const handleF = memView.getFloat64(slot, true);
          try {
            value[fname] = ratToAesonValue(ratUnbox(handleF));
          } catch {
            return { ok: false };
          }
        } else if (fschema.type === "boolean") {
          value[fname] = memView.getFloat64(slot, true) !== 0;
        } else if (fschema.type === "string") {
          const sp = Number(memView.getBigUint64(slot, true));
          value[fname] = sp ? readCString(sp) : "";
        } else {
          value[fname] = memView.getFloat64(slot, true);
        }
      } else if (fschema.kind === "enum") {
        // Enum values are stored as the integer tag.
        const idx = Math.trunc(memView.getFloat64(slot, true));
        const vs = fschema.values || [];
        value[fname] = vs[idx] != null ? vs[idx] : idx;
      } else if (fschema.kind === "maybe") {
        const innerWalked = walkMaybe(
          memView.getFloat64(slot, true),
          fschema.inner,
        );
        if (!innerWalked.ok) return { ok: false };
        value[fname] = innerWalked.value;
      } else if (fschema.kind === "list") {
        const sub = walkList(memView.getFloat64(slot, true), fschema.items);
        if (!sub.ok) return { ok: false };
        value[fname] = sub.value;
      } else if (fschema.kind === "record") {
        const sub = walkWasmValue(memView.getFloat64(slot, true), fschema);
        if (!sub.ok) return { ok: false };
        value[fname] = sub.value;
      } else {
        return { ok: false };
      }
    }
    // Mark only the first field as "forced" — body-allocated records
    // (e.g. constructor helpers like @bracket 10%@) have no wasm-side
    // @__forced@ tracking, so 'prettyL4Value' would render all fields
    // verbatim. jl4-core's lazy NF rendering for records observed via
    // a LIST element shows only the head field's value and elides the
    // rest as @(...)@ — mirror that here so the trace text matches
    // the reference byte-for-byte. Marshaled-arg records keep their
    // own (richer) @__forced@ marking and don't go through this path.
    if (order.length > 0) {
      Object.defineProperty(value, "__forced", {
        value: new Set([order[0]]),
        enumerable: false,
        writable: true,
        configurable: true,
      });
    }
    return { ok: true, value, schema: retSchemaToParameter(schema) };
  }

  // Convert a RetSchema (used for layout decoding) to the looser
  // Parameter-shaped schema 'prettyL4Value' consumes. The two shapes
  // overlap on `type`/`properties`/`items` but differ in how MAYBE
  // is encoded (RetSchema wraps; Parameter strips). Records carry
  // their x-l4-type so the prelude prints the constructor name.
  function retSchemaToParameter(s) {
    if (!s) return undefined;
    switch (s.kind) {
      case "scalar":
        return { type: s.type };
      case "enum":
        return { type: s.type, enum: s.values || [] };
      case "maybe":
        return retSchemaToParameter(s.inner);
      case "list":
        return { type: "array", items: retSchemaToParameter(s.items) };
      case "record": {
        const order = s.fieldOrder || [];
        const fields = s.fields || {};
        const properties = {};
        for (const n of order) properties[n] = retSchemaToParameter(fields[n]);
        return {
          type: "object",
          properties,
          propertyOrder: order,
          required: order,
          "x-l4-type": s.name,
        };
      }
      default:
        return undefined;
    }
  }

  // Pretty-print one trace frame's result box according to the kindByte
  // the lowering passed to `__l4_trace_exit`. Falls back to the box's
  // unrenderable bit pattern when the kind doesn't apply (e.g. record
  // pointers we don't yet know how to walk — slice 3 territory).
  // Kind constants here MUST match the compile-time encoding in Lower.hs:
  //   0 = NUMBER (rational handle) ; 1 = BOOLEAN (0/1) ;
  //   2 = STRING (f64-boxed pointer) ; 3 = OTHER (compound — record /
  //   list / MAYBE) ; 4 = "<function>" (M5 slice 4A — synthetic fn-value
  //   frame).
  //
  // M5 — when `kind === 3` and the compile-time metadata carries a
  // 'returnSchema' (RSScalar / RSMaybe / RSList / RSRecord), we walk
  // wasm linear memory at the result pointer to reconstruct the JS
  // value, then pretty-print via 'prettyL4Value'. This avoids the
  // raw-pointer leak (e.g. @5.494e-321@) when the body allocated the
  // compound inline rather than from a marshaled arg.
  function renderTraceResult(result, kind, returnSchema) {
    if (!result) return "";
    const { raw } = result;
    const k = typeof kind === "number" ? kind : Number(kind || 0);
    if (k === 4) return "<function>";
    // Final-cleanup — compound results (records, lists, MAYBE-wrapped
    // records) come back as raw wasm pointers. Look them up in
    // `valueByPtr` (populated by the marshaler) and walk via
    // `prettyL4Value` so the Result line matches jl4-service. Falls
    // through to the kind-specific renderers when the pointer doesn't
    // correspond to a marshaled compound (intermediate wasm-side
    // allocations, primitives whose bit-pattern happens to collide).
    if (k === 3) {
      const wp = Number(f64ToU64(raw));
      const entry = valueByPtr.get(wp);
      if (entry) {
        return prettyL4Value(entry.value, entry.schema, {
          inResult: true,
          depth: 0,
        }).text;
      }
      if (returnSchema) {
        const walked = walkWasmValue(raw, returnSchema);
        if (walked.ok) {
          return prettyL4Value(walked.value, walked.schema, {
            inResult: true,
            depth: 0,
          }).text;
        }
      }
    }
    if (k === 1) return Number(raw) !== 0 ? "TRUE" : "FALSE";
    if (k === 2) return readCString(Number(f64ToU64(raw)));
    if (k === 0) {
      // NUMBER handle → rational → jl4-core's @prettyRatio@:
      // 'formatScientific Fixed' for non-integers (always decimal),
      // plain digits for integers. Never scientific — matching
      // jl4-service's trace text where @0.016583333333333332@ stays
      // decimal even though the Aeson-encoded value-level wire form
      // is @1.6583333333333332e-2@.
      try {
        const r = ratUnbox(raw);
        if (r.den === 1n) {
          if (r.num <= _MAX_SAFE && r.num >= _MIN_SAFE) return String(r.num);
          return r.num.toString();
        }
        return formatFixedDouble(ratToDouble(r));
      } catch {
        return String(raw);
      }
    }
    return String(raw);
  }

  // Render the args portion of the top-level "<fn> OF <args>" trace.
  // Each arg is rendered via `prettyL4Value`, then wrapped in parens
  // when compound (records / JUST), so the header matches jl4-service's
  // `Print.prettyLayout` byte-for-byte:
  //   `is eligible` OF 6, 4
  //   `denial reason` OF (LoanRequest OF (CreditProfile OF 650, …), 50000, 60, (JUST OF 100000))
  function renderArgList(meta, args) {
    const props = (meta.parameters && meta.parameters.properties) || {};
    const order = meta.paramOrder || Object.keys(props);
    if (order.length === 0) return "";
    return order
      .map((pn) => {
        const fs_ = props[pn] || {};
        const v = args[pn];
        return prettyL4ValueWrapped(v, fs_);
      })
      .join(", ");
  }

  return {
    // Lifecycle
    attachMemory,
    resetHeap,
    allocBytes,
    // M7 — memory cap + peak-allocation telemetry
    getPeakHeapBytes,
    getHeapBytes,
    getMaxHeapBytes,
    // Type reinterpret
    u64ToF64,
    f64ToU64,
    // Memory I/O
    readCString,
    writeString,
    // Marshaling
    marshalArg,
    marshalMaybe,
    marshalStruct,
    marshalList,
    unmarshalResult,
    unmarshalScalar,
    unmarshalMaybe,
    isPointerSchema,
    // High-level
    makeImports,
    invokeFunction,
    invokeFunctionWithReasoning,
    setBundleFunctions,
    setBundleHelperTraceMeta,
    // M5 slice 4D — expose for buildReasoningFromTrace's renderer.
    getForcedFields: () => forcedFields,
  };
}

// ---------------------------------------------------------------------------
// M5 slice 1 — trace envelope assembly.
//
// `synthesizeReasoning` produces a Reasoning value that matches the JL4
// SHAPE (children/exampleCode/explanation) but with the slice-1 degenerate
// content: one node naming the function and showing the final result. The
// parity harness's trace column will read every cell as "trace-differs"
// until later slices replace this with a real instrumented trace.
//
// `wrapEvaluationEnvelope({value, reasoning})` builds the
// `{contents: {result: {value}, reasoning?}, tag}` envelope `jl4-service`
// returns, with `tag = TraceResponse` whenever `reasoning` is non-empty
// (matching `responseTag` in jl4-service's Api.hs).
// ---------------------------------------------------------------------------

const EMPTY_REASONING = {
  exampleCode: [],
  explanation: [],
  children: [],
};

export function isEmptyReasoning(r) {
  return (
    r &&
    Array.isArray(r.exampleCode) &&
    r.exampleCode.length === 0 &&
    Array.isArray(r.explanation) &&
    r.explanation.length === 0 &&
    Array.isArray(r.children) &&
    r.children.length === 0
  );
}

// Render a JSON value the way `Print.prettyLayout NF` would, for inclusion
// in a Reasoning node's "Result: …" explanation line. The optional
// `returnType` (e.g. `"NUMBER"`, `"BOOLEAN"`, `"STRING"`, `"MAYBE STRING"`)
// disambiguates strings that should be JSON-quoted (real `STRING` values)
// from bare strings (`"NOTHING"` sentinel, enum tags). When omitted, we
// keep the slice-1 behaviour of bare strings.
export function prettyResultText(value, returnType) {
  if (value === null || value === undefined) return "NOTHING";
  if (typeof value === "boolean") return value ? "TRUE" : "FALSE";
  if (typeof value === "string") {
    // If the return type says STRING, jl4-service renders the value
    // with double quotes (`Print.prettyLayout` on `ValString` uses
    // `show`). Otherwise the bare form covers enum tags and the
    // `"NOTHING"` MAYBE sentinel.
    if (returnType === "STRING") return JSON.stringify(value);
    return value;
  }
  if (typeof value === "number") {
    if (Number.isInteger(value) && Number.isFinite(value)) return String(value);
    return formatFixedDouble(value);
  }
  if (typeof value === "bigint") return value.toString();
  if (isAesonDouble(value)) return formatFixedDouble(value[AESON_DOUBLE_TAG]);
  if (value && typeof value === "object") {
    if ("__l4_raw_json" in value) return value.__l4_raw_json;
    // MAYBE JUST: {JUST: [payload]}. Strip a leading `MAYBE ` from
    // returnType so the recursive call sees the inner type — that
    // way a `MAYBE STRING` payload renders with quotes.
    if (Array.isArray(value.JUST)) {
      const innerType =
        typeof returnType === "string" && returnType.startsWith("MAYBE ")
          ? returnType.slice(6)
          : undefined;
      return "JUST OF " + prettyResultText(value.JUST[0], innerType);
    }
    // Best-effort fallback for richer shapes.
    return aesonStringify(value);
  }
  return String(value);
}

// Synthetic Reasoning: one-node tree with the function name as example code
// and "Result: <pretty value>" as the explanation. The shape matches
// jl4-service's `Reasoning`, but the content is degenerate.
//
// Slice 2A: when the schema carries `traceMeta` (the compile-time
// `Print.prettyLayout` of the function head), use the backticked source
// spelling — `` `is eligible` `` — instead of the sanitized API form
// `is-eligible`. The shape stays a one-node stub; slices 2B+ replace this
// with a real instrumented trace tree.
export function synthesizeReasoning(meta, _args, value) {
  const label =
    (meta.traceMeta && meta.traceMeta.fnName) ||
    meta.apiName ||
    meta.name ||
    "";
  return {
    exampleCode: [label],
    explanation: ["Result: " + prettyResultText(value, meta.returnType)],
    children: [],
  };
}

// M5 slice 3 — pretty-print a runtime JS value the way Haskell's
// `Print.prettyLayout NF` would. Driven by the parameter `schema` we
// emit alongside each function (the JSON-Schema-ish `Parameter` shape
// from jl4-core's `FunctionSchema`), and the L4 user-declared type name
// stashed in `x-l4-type`.
//
// Returns `{ text, compound }` — `compound` is true for forms whose
// pretty layout would need parens when used as a positional argument
// (records / `JUST OF …`); the caller decides via `prettyL4ValueWrapped`.
//
// Coverage (slice 3):
//   * primitives: NUMBER / BOOLEAN / STRING / DATE / TIME / DATETIME
//   * enum tags (bare constructor name)
//   * MAYBE (NOTHING / JUST OF …)
//   * records (`<TypeName> OF <field1>, <field2>, …` in propertyOrder)
//   * arrays best-effort (`[a, b, c]`); not fully spec'd until we see a
//     real list trace, but unblocks compile and keeps progress visible
export function prettyL4Value(value, schema, opts) {
  // M5 slice 4B + 4D — `opts.inResult` toggles jl4-core's lazy NF
  // rendering (the `Result: …` line). When true, record fields are
  // rendered as VALUE if `opts.forced` says they were forced at trace
  // time (via the `path.field` key in the set), as `(...)` otherwise.
  // `opts.path` is the current location string (e.g. `req.applicant`)
  // — empty/undefined means we're not inside a record-positional
  // context and the (...) rule doesn't apply.
  const inResult = !!(opts && opts.inResult);
  const depth = opts && typeof opts.depth === "number" ? opts.depth : 0;
  const path = opts && typeof opts.path === "string" ? opts.path : "";
  const forced = (opts && opts.forced) || null;
  // MAYBE NOTHING — unmarshalMaybe returns the literal string "NOTHING",
  // and `null`/`undefined` is treated the same way for safety.
  if (value === null || value === undefined || value === "NOTHING") {
    return { text: "NOTHING", compound: false };
  }
  // MAYBE JUST — `{JUST: [payload]}` from unmarshalMaybe.
  if (value && typeof value === "object" && Array.isArray(value.JUST)) {
    const inner = prettyL4Value(value.JUST[0], schema, {
      inResult,
      depth,
      path,
      forced,
    });
    const innerText = inner.compound ? "(" + inner.text + ")" : inner.text;
    return { text: "JUST OF " + innerText, compound: true };
  }
  // Schema-driven dispatch.
  const ty = schema && schema.type;
  const fmt = schema && schema.format;
  if (schema && Array.isArray(schema.enum) && schema.enum.length > 0) {
    // Enum tag: the value comes as the bare constructor name.
    return { text: String(value), compound: false };
  }
  if (fmt === "date" && ty === "string") {
    return { text: String(value), compound: false };
  }
  if (fmt === "time" && ty === "string") {
    return { text: String(value), compound: false };
  }
  if (ty === "boolean") {
    return { text: value ? "TRUE" : "FALSE", compound: false };
  }
  if (ty === "number") {
    // Two distinct fractional renderings:
    //  * inResult (trace's "Result: …" line) — jl4-core's 'prettyRatio'
    //    uses @formatScientific Fixed@, always decimal.
    //  * Non-inResult (exampleCode / arg pretty) — jl4-service's
    //    @prettyLayout@ goes through Aeson which uses Ryu-shortest, so
    //    @0.016583333333333332@ renders as @1.6583333333333332e-2@.
    const formatFractional = inResult ? formatFixedDouble : formatAesonDouble;
    if (typeof value === "number") {
      return {
        text: Number.isInteger(value) ? String(value) : formatFractional(value),
        compound: false,
      };
    }
    if (typeof value === "bigint") {
      return { text: value.toString(), compound: false };
    }
    // 'ratToAesonValue' tags fractions as @{__l4_aeson_double: x}@ and
    // very-big integers as @{__l4_raw_json: "…"}@ — unwrap both so the
    // trace text renders the underlying value, not @[object Object]@.
    if (value && typeof value === "object") {
      if (typeof value.__l4_aeson_double === "number") {
        return {
          text: formatFractional(value.__l4_aeson_double),
          compound: false,
        };
      }
      if (typeof value.__l4_raw_json === "string") {
        return { text: value.__l4_raw_json, compound: false };
      }
    }
    return { text: String(value), compound: false };
  }
  if (ty === "string") {
    // jl4-core renders strings with double quotes (matching the source
    // literal); `JSON.stringify` on a string gives the same double-quoted
    // form, with the same backslash escapes we'd see in a Haskell `Show`.
    return { text: JSON.stringify(String(value)), compound: false };
  }
  if (ty === "array") {
    // M5 slice 4C — jl4-core renders lists with a `LIST` keyword.
    //   * exampleCode form: `LIST (e1), (e2), ...` — compound elements
    //     wrapped in parens.
    //   * Result form (lazy NF): `LIST <e1-NF>, <e2-NF>, ...` — no parens,
    //     each element rendered in NF (inResult mode); empty list as
    //     `EMPTY` (no `LIST` prefix at all).
    const items = Array.isArray(value) ? value : [];
    const itemSchema = (schema && schema.items) || {};
    if (inResult && items.length === 0) {
      return { text: "EMPTY", compound: true };
    }
    if (items.length === 0) {
      // exampleCode for empty list — `LIST ` (trailing space) is how
      // jl4-service renders the Cons tail's exampleCode literal.
      return { text: "LIST ", compound: true };
    }
    // List elements don't have a meaningful per-index path (jl4-core
    // doesn't track them individually); pass an empty path so children
    // render via their own depth-based rule.
    const parts = items.map((v) =>
      prettyL4Value(v, itemSchema, { inResult, depth, path: "", forced }),
    );
    if (inResult) {
      // jl4-core's lazy NF collapses a Cons whose head and tail were
      // both Omitted to a single @LIST ...@ (three ASCII dots, via
      // @goList Omitted Omitted = "..."@ in Print.hs). We approximate
      // that here: when every element is a compound that had nothing
      // forced (records with `__forced.size === 0`), the head's NF
      // would be Omitted and the tail recurses into the same shape,
      // so jl4-core collapses to the literal @LIST ...@ sentinel.
      const allUnforced =
        items.length > 0 &&
        items.every((it) => {
          if (!it || typeof it !== "object") return false;
          if (Array.isArray(it)) return false;
          if (Array.isArray(it.JUST)) return false;
          return it.__forced && it.__forced.size === 0;
        });
      if (allUnforced) return { text: "LIST ...", compound: true };
      // NF form: no parens around items, just comma-separated NFs.
      return {
        text: "LIST " + parts.map((p) => p.text).join(", "),
        compound: true,
      };
    }
    // exampleCode form: parens around each compound element.
    const inner = parts
      .map((p) => (p.compound ? "(" + p.text + ")" : p.text))
      .join(", ");
    return { text: "LIST " + inner, compound: true };
  }
  if (ty === "object" && schema && schema.properties) {
    const typeName = schema["x-l4-type"] || "";
    const order = schema.propertyOrder || Object.keys(schema.properties);
    // Fields not in `required` are MAYBE-typed at the L4 level — jl4-core
    // wraps them in `JUST` when present and renders `NOTHING` when null,
    // even though the JSON payload omits the wrapper. Mirror that here so
    // the trace's Result line matches jl4-service byte-for-byte.
    const required = new Set(schema.required || order);
    // M5 slice 4F — lazy-NF placeholder rule (simplified). At ALL
    // depths, consult `value.__forced`: forced fields render as
    // values, unforced as `(...)`. The earlier slice-4B
    // `hasNestedRecord` depth-0 exception was based on misreading
    // a test capture; with the slice-4E value-pointer markers
    // populating `__forced` accurately, the rule generalises.
    const objForced =
      inResult && value && typeof value === "object" ? value.__forced : null;
    const useForcedFilter = inResult;
    const parts = order.map((name) => {
      const fs = schema.properties[name] || {};
      const v = (value || {})[name];
      const isMaybe = !required.has(name);
      // (...) iff we're filtering by __forced AND this field wasn't
      // forced (or __forced isn't attached, which shouldn't happen
      // for marshaled args but does for synthetic test contexts —
      // those use no filter at all because `useForcedFilter` only
      // triggers in Result mode).
      const unforcedInResult =
        useForcedFilter && objForced && !objForced.has(name);
      if (unforcedInResult) {
        return { text: "(...)", compound: false };
      }
      const childOpts = { inResult, depth: depth + 1 };
      if (isMaybe) {
        if (v === null || v === undefined) {
          return { text: "NOTHING", compound: false };
        }
        const inner = prettyL4Value(v, fs, childOpts);
        const innerText = inner.compound ? "(" + inner.text + ")" : inner.text;
        return { text: "JUST OF " + innerText, compound: true };
      }
      return prettyL4Value(v, fs, childOpts);
    });
    const inner = parts
      .map((p) => (p.compound ? "(" + p.text + ")" : p.text))
      .join(", ");
    // jl4-service emits `<TypeName> OF <fields>` for a record value; if
    // the type name is missing (defensive — shouldn't happen for a
    // well-formed DECLARE), drop the prefix rather than emit "OF …".
    const text = typeName ? typeName + " OF " + inner : inner;
    return { text, compound: !!typeName };
  }
  // Fallback — best-effort. Slice 4 may refine for shapes we haven't
  // seen in fixtures yet.
  return { text: aesonStringify(value), compound: false };
}

// Convenience for callers that always want to wrap compound forms.
export function prettyL4ValueWrapped(value, schema) {
  const { text, compound } = prettyL4Value(value, schema);
  return compound ? "(" + text + ")" : text;
}

// M5 slice 4C — build the Cons-style sub-tree jl4-core emits for a list
// arg-eval. Returns a single LIST-node Reasoning whose children are a
// CONS chain. Empty list → null (no LIST sub-tree at the parent level,
// matching `Lit/Var` pruning for trivial cases).
export function buildConsListChildren(values, itemSchema) {
  if (!Array.isArray(values) || values.length === 0) return null;
  // LIST node wraps the whole list. exampleCode + Result use the
  // value-level prettyprinter; children = [single CONS node].
  const exText = prettyL4Value(values, {
    type: "array",
    items: itemSchema,
  }).text;
  const nfText = prettyL4Value(
    values,
    { type: "array", items: itemSchema },
    { inResult: true, depth: 0 },
  ).text;
  return {
    exampleCode: [exText],
    explanation: ["Result: " + nfText],
    children: [buildConsNodeReasoning(values, itemSchema)],
  };
}

function buildConsNodeReasoning(values, itemSchema) {
  // Pre-condition: values is non-empty. Renders one CONS frame with
  // [head, tail-list] as children; tail-list is a recursive LIST node
  // (or an empty LIST leaf).
  const head = values[0];
  const rest = values.slice(1);
  const headPretty = prettyL4Value(head, itemSchema);
  // jl4-core's prettyLayout for Cons reads `(head) FOLLOWED BY (tail)`
  // with exactly ONE level of parens around each side. We use the raw
  // head text (not the already-wrapped form) and wrap once here.
  const tailListEx = prettyL4Value(rest, {
    type: "array",
    items: itemSchema,
  }).text;
  const consExampleCode =
    "(" + headPretty.text + ") FOLLOWED BY (" + tailListEx + ")";
  const nfText = prettyL4Value(
    values,
    { type: "array", items: itemSchema },
    { inResult: true, depth: 0 },
  ).text;
  // Head sub-tree: skip entirely when the head is an unforced compound
  // (matches jl4-core's lazy NF rendering — if no field of the head was
  // ever forced beyond WHNF, the trace just leaves it as text inside
  // the cons's exampleCode without a child frame). The full sub-tree
  // is still emitted whenever any field has been forced.
  const headChildren = [];
  const headForced = head && typeof head === "object" && head.__forced;
  const headIsCompound = headPretty.compound;
  if (headIsCompound && (!headForced || headForced.size > 0)) {
    const headTree = synthesizeArgEvalTree(head, itemSchema);
    if (headTree) headChildren.push(headTree);
  } else if (!headIsCompound) {
    headChildren.push({
      exampleCode: [headPretty.text],
      explanation: [
        "Result: " +
          prettyL4Value(head, itemSchema, { inResult: true, depth: 0 }).text,
      ],
      children: [],
    });
  }
  // Tail: recursive LIST (with another CONS) or the empty leaf.
  const tailTree =
    rest.length === 0
      ? {
          exampleCode: ["LIST "],
          explanation: ["Result: EMPTY"],
          children: [],
        }
      : {
          exampleCode: [
            prettyL4Value(rest, { type: "array", items: itemSchema }).text,
          ],
          explanation: [
            "Result: " +
              prettyL4Value(
                rest,
                { type: "array", items: itemSchema },
                { inResult: true, depth: 0 },
              ).text,
          ],
          children: [buildConsNodeReasoning(rest, itemSchema)],
        };
  return {
    exampleCode: [consExampleCode],
    explanation: ["Result: " + nfText],
    children: [...headChildren, tailTree],
  };
}

// M5 slice 3 — synthesise a Reasoning subtree for the evaluation of one
// argument *value*, the way jl4-service's interpreter does when it
// reduces the synthesised @EVAL fn arg1 arg2@ wrapper at trace=full
// time. Each compound arg (record / JUST) becomes a node; primitives
// don't appear (they correspond to `Lit` traces which jl4-service drops
// via `simplifyEvalTrace`). Returns `null` for primitives so the caller
// can omit them from the parent's `children` list.
export function synthesizeArgEvalTree(value, schema, _opts) {
  // M5 slice 4E — no path/forced opts: the lazy-NF info lives on the
  // value's own `__forced` Set, attached by the marshaler + populated
  // by `__l4_mark_forced` during the wasm body's Proj evaluations.
  // Primitive → no subtree (matches the Lit-pruning in simplifyEvalTrace).
  const { text, compound } = prettyL4Value(value, schema);
  if (!compound) return null;
  // Arrays: route to the Cons-list builder; the renderer reads
  // `__forced` on each element directly.
  if (schema && schema.type === "array" && Array.isArray(value)) {
    return buildConsListChildren(value, (schema && schema.items) || {});
  }
  // The Result line uses jl4-core's lazy NF — `prettyL4Value` with
  // `inResult: true` reads `value.__forced` to choose value vs (...)
  // per field, with the slice-4B `hasNestedRecord` heuristic at depth 0.
  const resultText = prettyL4Value(value, schema, {
    inResult: true,
    depth: 0,
  }).text;
  // Compound: recurse into the structure, gathering subtrees for each
  // compound sub-value. JUST recurses into its payload; objects recurse
  // through propertyOrder. Same shape jl4-service produces inside the
  // arg-eval frame on the wire.
  const children = [];
  if (value && typeof value === "object" && Array.isArray(value.JUST)) {
    const sub = synthesizeArgEvalTree(value.JUST[0], schema);
    if (sub) children.push(sub);
  } else if (schema && schema.type === "object" && schema.properties) {
    const order = schema.propertyOrder || Object.keys(schema.properties);
    const required = new Set(schema.required || order);
    // M5 slice 4E — only push a child for forced compound fields.
    // `value.__forced` is the Set populated by `__l4_mark_forced`
    // during the wasm body's Proj evaluations. Missing __forced
    // (synth contexts) ⇒ treat all as forced.
    const objForced =
      value && typeof value === "object" && value.__forced
        ? value.__forced
        : null;
    for (const name of order) {
      const fs = schema.properties[name] || {};
      const v = (value || {})[name];
      const isMaybe = !required.has(name);
      const isForced = !objForced || objForced.has(name);
      if (isMaybe) {
        if (!isForced) continue;
        if (v === null || v === undefined) continue;
        // Synthesise a JUST OF … child with the inner value.
        const innerPretty = prettyL4Value(v, fs);
        const innerText = innerPretty.compound
          ? "(" + innerPretty.text + ")"
          : innerPretty.text;
        const justText = "JUST OF " + innerText;
        const innerNF = prettyL4Value(v, fs, { inResult: true, depth: 0 }).text;
        const innerSub = synthesizeArgEvalTree(v, fs);
        const innerChildren = innerSub ? [innerSub] : [];
        children.push({
          exampleCode: [justText],
          explanation: ["Result: JUST OF " + innerNF],
          children: innerChildren,
        });
        continue;
      }
      if (!isForced) continue;
      const sub = synthesizeArgEvalTree(v, fs);
      if (sub) children.push(sub);
    }
  }
  // (arrays handled by the early-return up top, before the field walk)
  return {
    exampleCode: [text],
    explanation: ["Result: " + resultText],
    children,
  };
}

// Build the wire envelope matching jl4-service's `SimpleResponse` ToJSON
// instance: `{tag, contents}` where `contents` is `ResponseWithReason` (so
// `{result, reasoning?}`). The tag flips to "TraceResponse" whenever
// `reasoning` is non-empty (= `responseTag` in Backend/Api.hs).
export function wrapEvaluationEnvelope({ value, reasoning }) {
  const result = { value };
  const contents =
    reasoning && !isEmptyReasoning(reasoning)
      ? { result, reasoning }
      : { result };
  const tag =
    reasoning && !isEmptyReasoning(reasoning)
      ? "TraceResponse"
      : "SimpleResponse";
  return { contents, tag };
}
