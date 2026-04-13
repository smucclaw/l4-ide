// Lightweight Node HTTP wrapper around a jl4-mlir-compiled .wasm bundle.
//
// Routes are shaped like jl4-service:
//   POST /deployments/<any>/functions/<fn>/evaluation
//   Body: { "arguments": { ... } }
//   Returns: { "tag": "SimpleResponse", "contents": { "result": { "value": X } } }
//
// Same wire format as jl4-service — drop this in front of a .wasm + schema
// bundle when you want to benchmark / serve the mlir backend over HTTP.
//
// Usage: node wasm-server.mjs <schema.json> <wasm> [port]
import fs from "node:fs";
import http from "node:http";

const [, , schemaPath, wasmPath, portArg] = process.argv;
if (!schemaPath || !wasmPath) {
  console.error("usage: node wasm-server.mjs <schema.json> <wasm> [port]");
  process.exit(1);
}
const port = parseInt(portArg || "8766", 10);

const schema = JSON.parse(fs.readFileSync(schemaPath, "utf8"));
const wasmBuf = fs.readFileSync(wasmPath);

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

let memView, memU8;
let heapPtr = 1024;
const allocBytes = (n) => {
  const nn = Number(n);
  const p = heapPtr;
  heapPtr += (nn + 7) & ~7;
  return p;
};
const resetHeap = () => {
  heapPtr = 1024;
};

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

// ---- DATE / TIME / DATETIME helpers (match generated Node runner) ----
const DATE_EPOCH_SHIFT = 4; // 1969-12-28 (Sun) to 1970-01-01 (Thu)
function dateFromDMY(d, m, y) {
  return Math.floor(Date.UTC(y, m - 1, d) / 86400000) + DATE_EPOCH_SHIFT;
}
function dateToUnixDays(x) {
  return Number(x) - DATE_EPOCH_SHIFT;
}
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
function timeFromHMS(h, m, s) {
  return h * 3600 + m * 60 + s;
}
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
  if (schema.format === "date" || schema.format === "time") return false;
  return (
    schema.type === "string" ||
    schema.type === "array" ||
    schema.type === "object"
  );
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

const imports = {
  env: new Proxy(
    {
      __l4_pow: Math.pow,
      __l4_min: Math.min,
      __l4_max: Math.max,
      __l4_abs: Math.abs,
      __l4_floor: Math.floor,
      __l4_ceil: Math.ceil,
      __l4_round: Math.round,
      __l4_str_concat: (a) => a,
      __l4_str_eq: (a, b) => (a === b ? 1 : 0),
      __l4_str_len: () => 0,
      __l4_to_string: () => 0,
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
    },
    {
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
    },
  ),
};

const { instance } = await WebAssembly.instantiate(wasmBuf, imports);
memView = new DataView(instance.exports.memory.buffer);
memU8 = new Uint8Array(instance.exports.memory.buffer);

const fnByName = {};
for (const [sanitized, meta] of Object.entries(schema.functions)) {
  fnByName[sanitized] = meta;
  fnByName[sanitized.replace(/-/g, " ")] = meta;
}

function invokeFunction(meta, args) {
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

const server = http.createServer((req, res) => {
  if (req.method !== "POST") {
    res.writeHead(405);
    res.end();
    return;
  }
  const m = req.url.match(
    /\/deployments\/[^/]+\/functions\/([^/]+)\/evaluation/,
  );
  if (!m) {
    res.writeHead(404);
    res.end("not found");
    return;
  }
  const fnName = decodeURIComponent(m[1]);
  const meta = fnByName[fnName] || fnByName[fnName.replace(/ /g, "-")];
  if (!meta) {
    res.writeHead(404, { "content-type": "application/json" });
    res.end(JSON.stringify({ error: "Unknown function: " + fnName }));
    return;
  }
  let body = "";
  req.on("data", (c) => (body += c));
  req.on("end", () => {
    try {
      const parsed = body ? JSON.parse(body) : {};
      const result = invokeFunction(meta, parsed.arguments || {});
      res.writeHead(200, { "content-type": "application/json" });
      res.end(
        JSON.stringify({
          tag: "SimpleResponse",
          contents: { result: { value: result } },
        }),
      );
    } catch (err) {
      res.writeHead(500, { "content-type": "application/json" });
      res.end(JSON.stringify({ error: String(err.message || err) }));
    }
  });
});
server.listen(port, "127.0.0.1", () => {
  console.error("wasm HTTP wrapper listening on http://127.0.0.1:" + port);
});
