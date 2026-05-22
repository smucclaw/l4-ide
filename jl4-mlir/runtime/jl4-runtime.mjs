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

// Parse a decimal literal exactly (no Double round-trip): "123", "1.5",
// "-0.001", "2.5e-3". This is how both numeric literals (lowering) and JSON
// number inputs must become rationals so 0.1 is exactly 1/10, matching
// jl4-core's `Scientific -> toRational` (not the Double 0.1000…0055).
export function ratFromDecimalString(s) {
  const str = String(s).trim();
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

export function createRuntime() {
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
  const allocBytes = (n) => {
    const nn = Number(n);
    const p = heapPtr;
    heapPtr += (nn + 7) & ~7;
    return p;
  };
  // Per-call exact-rational pool (M4). NUMBER values are handles into this.
  const ratPool = makeRationalPool();
  const resetHeap = () => {
    heapPtr = 1024;
    ratPool.reset();
  };
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
        return Number(value);
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
    if (innerSchema && innerSchema.type === "number")
      memView.setFloat64(p + 8, Number(inner), true);
    else memView.setBigUint64(p + 8, BigInt(inner), true);
    return p;
  }

  function marshalStruct(value, schema) {
    if (!schema || !schema.properties) return 0;
    const order = schema.propertyOrder || Object.keys(schema.properties);
    const required = new Set(schema.required || order);
    const p = allocBytes(order.length * 8);
    order.forEach((name, idx) => {
      const fs_ = schema.properties[name] || {};
      const v = (value || {})[name];
      const isMaybe = !required.has(name);
      const isEnum = !!(fs_.enum && fs_.enum.length > 0);
      if (isMaybe) {
        memView.setBigUint64(p + idx * 8, BigInt(marshalMaybe(v, fs_)), true);
      } else if (fs_.type === "number" || isEnum) {
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
    return head;
  }

  function unmarshalScalar(raw, type) {
    if (type === "BOOLEAN") return Number(raw) !== 0;
    if (type === "NUMBER") return Number(raw);
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
    if (innerType === "NUMBER") payload = memView.getFloat64(ptr + 8, true);
    else if (innerType === "BOOLEAN")
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
    const env = {
      __l4_pow: Math.pow,
      __l4_min: Math.min,
      __l4_max: Math.max,
      __l4_abs: Math.abs,
      __l4_floor: Math.floor,
      __l4_ceil: Math.ceil,
      __l4_round: Math.round,
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
      // NUMBER → STRING. Matches jl4-core's rendering: integer-valued
      // numbers print with no decimal point, others as a shortest decimal.
      __l4_to_string: (vF) => u64ToF64(writeString(String(Number(vF)))),
      // ---- Exact-rational NUMBER ABI (M4). Args/results are f64-boxed
      //      handles into ratPool; resets each call. Dormant until the
      //      lowering (slice 2b) emits these instead of arith.* float ops. ----
      __l4_rat_parse: (sPtrF) =>
        ratBox(ratFromDecimalString(readCString(Number(f64ToU64(sPtrF))))),
      __l4_rat_from_int: (f) => ratBox(fromInt(BigInt(Math.trunc(Number(f))))),
      __l4_rat_add: (a, b) => ratBox(ratAdd(ratUnbox(a), ratUnbox(b))),
      __l4_rat_sub: (a, b) => ratBox(ratSub(ratUnbox(a), ratUnbox(b))),
      __l4_rat_mul: (a, b) => ratBox(ratMul(ratUnbox(a), ratUnbox(b))),
      __l4_rat_div: (a, b) => ratBox(ratDiv(ratUnbox(a), ratUnbox(b))),
      __l4_rat_neg: (a) => ratBox(ratNeg(ratUnbox(a))),
      // comparison returns a plain numeric -1/0/1 (for branching), not a handle
      __l4_rat_cmp: (a, b) => ratCmp(ratUnbox(a), ratUnbox(b)),
      // shims for inherently-inexact builtins (sqrt/ln/sin/pow…): drop to f64
      __l4_rat_to_f64: (a) => ratToDouble(ratUnbox(a)),
      __l4_f64_to_rat: (f) => ratBox(ratFromNumber(Number(f))),
      __l4_list_count: (ptrF) => {
        let n = 0,
          p = Number(f64ToU64(ptrF));
        while (p !== 0 && n < 100000) {
          p = Number(memView.getBigUint64(p + 8, true));
          n++;
        }
        return n;
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
      // DATE
      __l4_date_from_dmy: (d, m, y) =>
        dateFromDMY(Number(d), Number(m), Number(y)),
      __l4_date_from_serial: (n) => Number(n),
      __l4_date_serial: (d) => Number(d),
      __l4_date_day: (d) => new Date(dateToUnixDays(d) * 86400000).getUTCDate(),
      __l4_date_month: (d) =>
        new Date(dateToUnixDays(d) * 86400000).getUTCMonth() + 1,
      __l4_date_year: (d) =>
        new Date(dateToUnixDays(d) * 86400000).getUTCFullYear(),
      __l4_datevalue: (sPtrF) => {
        const v = parseDateStr(readCString(Number(f64ToU64(sPtrF))));
        return Number.isFinite(v) ? mbJustF64(v) : mbNothing();
      },
      __l4_to_date: (sPtrF) => {
        const v = parseDateStr(readCString(Number(f64ToU64(sPtrF))));
        return Number.isFinite(v) ? mbJustF64(v) : mbNothing();
      },
      // TIME
      __l4_time_from_hms: (h, m, s) =>
        timeFromHMS(Number(h), Number(m), Number(s)),
      __l4_time_from_serial: (n) => Number(n),
      __l4_time_serial: (t) => Number(t),
      __l4_time_hour: (t) => Math.floor(Number(t) / 3600),
      __l4_time_minute: (t) => Math.floor((Number(t) % 3600) / 60),
      __l4_time_second: (t) => Number(t) % 60,
      __l4_to_time: (sPtrF) => {
        const v = parseTimeStr(readCString(Number(f64ToU64(sPtrF))));
        return Number.isFinite(v) ? mbJustF64(v) : mbNothing();
      },
      // DATETIME
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
        if (!p) return 0;
        return memView.getFloat64(p, true);
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
      // Extended numeric intrinsics
      __l4_sqrt: Math.sqrt,
      __l4_ln: Math.log,
      __l4_log10: Math.log10,
      __l4_sin: Math.sin,
      __l4_cos: Math.cos,
      __l4_tan: Math.tan,
      __l4_asin: Math.asin,
      __l4_acos: Math.acos,
      __l4_atan: Math.atan,
      __l4_trunc: (x, d) => {
        const m = Math.pow(10, Number(d));
        return Math.trunc(Number(x) * m) / m;
      },
      __l4_is_integer: (x) => (Number.isInteger(Number(x)) ? 1 : 0),
      // String intrinsics (all string args arrive as f64-bitcast pointers)
      __l4_string_length: (sF) => readCString(Number(f64ToU64(sF))).length,
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
        readCString(Number(f64ToU64(hF))).indexOf(
          readCString(Number(f64ToU64(nF))),
        ),
      __l4_char_at: (sF, iF) =>
        u64ToF64(
          writeString(readCString(Number(f64ToU64(sF))).charAt(Number(iF))),
        ),
      __l4_substring: (sF, iF, jF) =>
        u64ToF64(
          writeString(
            readCString(Number(f64ToU64(sF))).substring(Number(iF), Number(jF)),
          ),
        ),
      __l4_replace: (sF, aF, bF) => {
        const s = readCString(Number(f64ToU64(sF)));
        const a = readCString(Number(f64ToU64(aF)));
        const b = readCString(Number(f64ToU64(bF)));
        return u64ToF64(writeString(s.split(a).join(b)));
      },
      __l4_to_number: (sF) => {
        const n = Number(readCString(Number(f64ToU64(sF))));
        return Number.isNaN(n) ? mbNothing() : mbJustF64(n);
      },
      // Nullary temporal
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
      // JSON
      __l4_json_encode: (vF) => u64ToF64(writeString(String(Number(vF)))),
      __l4_json_decode: (sF) => {
        try {
          const v = JSON.parse(readCString(Number(f64ToU64(sF))));
          return typeof v === "number" ? mbJustF64(v) : mbNothing();
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

  return {
    // Lifecycle
    attachMemory,
    resetHeap,
    allocBytes,
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
  };
}
