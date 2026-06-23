// Tests for the exact rational core (M4 slice 1) and the rational ABI imports
// (slice 2a). Pure JS, no toolchain.
//
//   node runtime/rational.test.mjs

import {
  createRuntime,
  makeRat,
  fromInt,
  ratAdd,
  ratSub,
  ratMul,
  ratDiv,
  ratNeg,
  ratCmp,
  ratToDouble,
  ratToJSONValue,
  ratToAesonValue,
  ratFromDecimalString,
  ratMod,
  ratAbs,
  ratFloor,
  ratCeil,
  ratRound,
  ratTrunc,
  ratMin,
  ratMax,
  isInteger,
  formatAesonDouble,
  aesonStringify,
  wrapEvaluationEnvelope,
  synthesizeReasoning,
  prettyResultText,
  isEmptyReasoning,
  prettyL4Value,
  prettyL4ValueWrapped,
  synthesizeArgEvalTree,
} from "./jl4-runtime.mjs";

let pass = 0;
let fail = 0;
const ok = (name, cond, detail = "") => {
  if (cond) pass++;
  else {
    fail++;
    console.log(`FAIL ${name} ${detail}`);
  }
};
const eq = (name, got, want) =>
  ok(name, got === want, `got ${got} want ${want}`);

// ---- exact arithmetic: the whole point is f64 drift disappears ----
// 1/10 + 2/10 == 3/10 exactly (f64 gives 0.30000000000000004)
{
  const r = ratAdd(makeRat(1n, 10n), makeRat(2n, 10n));
  ok("1/10+2/10 == 3/10 exactly", r.num === 3n && r.den === 10n);
  eq("ratToDouble(3/10) == 0.3", ratToDouble(r), 0.3);
  ok("…and 0.3 !== f64 0.1+0.2", ratToDouble(r) !== 0.1 + 0.2);
}
// (1/3)*3 == 1 exactly
{
  const r = ratMul(makeRat(1n, 3n), fromInt(3));
  ok("(1/3)*3 == 1 exactly", r.num === 1n && r.den === 1n);
}
// sum of 1/3 three times == 1
{
  let r = fromInt(0);
  for (let i = 0; i < 3; i++) r = ratAdd(r, makeRat(1n, 3n));
  ok("1/3+1/3+1/3 == 1", r.num === 1n && r.den === 1n);
}
eq("ratCmp 1/2 vs 2/3", ratCmp(makeRat(1n, 2n), makeRat(2n, 3n)), -1);
eq("ratCmp equal", ratCmp(makeRat(2n, 4n), makeRat(1n, 2n)), 0);
{
  const r = ratSub(makeRat(7n, 6n), makeRat(1n, 6n));
  ok("7/6 - 1/6 == 1", r.num === 1n && r.den === 1n);
}
eq("ratNeg", ratToDouble(ratNeg(makeRat(7n, 4n))), -1.75);
ok(
  "ratDiv by zero throws",
  (() => {
    try {
      ratDiv(fromInt(1), fromInt(0));
      return false;
    } catch {
      return true;
    }
  })(),
);

// ---- ratToDouble: exact-decimal and simple cases ----
eq("1/3", ratToDouble(makeRat(1n, 3n)), 0.3333333333333333);
eq("1/10", ratToDouble(makeRat(1n, 10n)), 0.1);
eq("2/1", ratToDouble(fromInt(2)), 2);
eq("0", ratToDouble(fromInt(0)), 0);
eq("-7/4", ratToDouble(makeRat(-7n, 4n)), -1.75);
eq("10/4", ratToDouble(makeRat(10n, 4n)), 2.5);

// ---- ratToDouble: the M0 ulp-differs case ----
// total-interest-paid expected 0.016583333333333332 (jl4-service, exact).
// 199/12000 = 0.016583333333333... ; correctly-rounded must give …332, the
// value f64 accumulation got wrong (…335).
eq(
  "199/12000 == 0.016583333333333332",
  ratToDouble(makeRat(199n, 12000n)),
  0.016583333333333332,
);

// ---- ratToJSONValue: integer vs fractional path ----
eq("JSON integer 25000", ratToJSONValue(fromInt(25000)), 25000);
eq("JSON fractional", ratToJSONValue(makeRat(1n, 4n)), 0.25);
eq(
  "JSON big integer -> string",
  ratToJSONValue(makeRat(123456789012345678901234567890n, 1n)),
  "123456789012345678901234567890",
);

// ---- ties-to-even ----
// A value exactly halfway between two doubles must round to the even mantissa.
// 2^53 + 1 is not representable; (2^53+1)/1 rounds to 2^53 (even).
eq("2^53+1 ties to even (down)", ratToDouble(fromInt(2n ** 53n + 1n)), 2 ** 53);
eq(
  "2^53+3 ties to even (up)",
  ratToDouble(fromInt(2n ** 53n + 3n)),
  2 ** 53 + 4,
);

// ---- ratFromDecimalString: exact decimal parsing (no Double round-trip) ----
{
  const r = ratFromDecimalString("0.1");
  ok("0.1 -> 1/10 exactly", r.num === 1n && r.den === 10n);
}
{
  const r = ratFromDecimalString("-0.001");
  ok("-0.001 -> -1/1000", r.num === -1n && r.den === 1000n);
}
{
  const r = ratFromDecimalString("2.5e-3");
  ok("2.5e-3 -> 1/400", r.num === 1n && r.den === 400n);
}
{
  const r = ratFromDecimalString("42");
  ok("42 -> 42/1", r.num === 42n && r.den === 1n);
}
{
  const r = ratFromDecimalString("1.50");
  ok("1.50 -> 3/2", r.num === 3n && r.den === 2n);
}
ok(
  "0.1+0.2 via decimal parse == 3/10 (not f64)",
  (() => {
    const s = ratAdd(ratFromDecimalString("0.1"), ratFromDecimalString("0.2"));
    return s.num === 3n && s.den === 10n;
  })(),
);
ok(
  "bad decimal throws",
  (() => {
    try {
      ratFromDecimalString("1.2.3");
      return false;
    } catch {
      return true;
    }
  })(),
);

// ---- random oracle: for operands < 2^53, Number(n)/Number(d) is itself
//      correctly-rounded RNE, so it's an exact reference. ----
{
  let mism = 0;
  let seed = 0x2545f4914f6cdd1dn;
  const rnd = () => {
    // xorshift64* -> a BigInt in [1, 2^52)
    seed ^= seed >> 12n;
    seed ^= (seed << 25n) & 0xffffffffffffffffn;
    seed ^= seed >> 27n;
    return (
      (((seed * 0x2545f4914f6cdd1dn) & 0xffffffffffffffffn) % (1n << 52n)) + 1n
    );
  };
  const N = 20000;
  for (let i = 0; i < N; i++) {
    const n = rnd();
    const d = rnd();
    const sign = i % 2 ? -1n : 1n;
    const mine = ratToDouble(makeRat(sign * n, d));
    const oracle = Number(sign * n) / Number(d);
    if (mine !== oracle) {
      mism++;
      if (mism <= 3)
        console.log(
          `  oracle mismatch: ${sign * n}/${d} mine=${mine} oracle=${oracle}`,
        );
    }
  }
  ok(
    `random oracle: ${N} rationals correctly rounded`,
    mism === 0,
    `(${mism} mismatches)`,
  );
}

// ---- slice 2a: the __l4_rat_* runtime imports (handles boxed across ABI) ----
{
  const rt = createRuntime();
  rt.attachMemory(new WebAssembly.Memory({ initial: 2 }));
  const { env } = rt.makeImports();
  // intern a decimal literal string in linear memory and box its pointer
  const litF = (s) => rt.u64ToF64(rt.writeString(s));
  const parse = (s) => env.__l4_rat_parse(litF(s));

  // 0.1 + 0.2 == 0.3 exactly (the canonical f64 failure)
  const sum = env.__l4_rat_add(parse("0.1"), parse("0.2"));
  eq("ABI: 0.1+0.2 -> 0.3", env.__l4_rat_to_f64(sum), 0.3);

  // (1/3) computed as 1 / 3, then * 3 == 1
  const third = env.__l4_rat_div(
    env.__l4_rat_from_int(1),
    env.__l4_rat_from_int(3),
  );
  const back = env.__l4_rat_mul(third, env.__l4_rat_from_int(3));
  eq("ABI: (1/3)*3 -> 1", env.__l4_rat_to_f64(back), 1);

  eq("ABI: cmp 0.1 vs 0.2", env.__l4_rat_cmp(parse("0.1"), parse("0.2")), -1);
  eq(
    "ABI: sub 0.3-0.1 -> 0.2",
    env.__l4_rat_to_f64(env.__l4_rat_sub(parse("0.3"), parse("0.1"))),
    0.2,
  );
  eq("ABI: neg", env.__l4_rat_to_f64(env.__l4_rat_neg(parse("1.75"))), -1.75);
  // f64<->rat shim round-trips
  eq(
    "ABI: f64->rat->f64",
    env.__l4_rat_to_f64(env.__l4_f64_to_rat(1.25)),
    1.25,
  );
  // handles survive a resetHeap (new call) — pool cleared, fresh handles work
  rt.resetHeap();
  eq("ABI: after reset, parse works", env.__l4_rat_to_f64(parse("2.5")), 2.5);
}

// ---- M4 slice 2b helpers: ratMod / ratAbs / ratFloor / ratCeil / ratRound /
// ratTrunc / ratMin / ratMax — must match jl4-core semantics tested
// empirically against `l4 run`.
{
  // ratFromDecimalString "n/d" form (used by the MLIR lowering for literals)
  const r = ratFromDecimalString("199/12000");
  ok("n/d form parses", r.num === 199n && r.den === 12000n);

  // abs
  ok("|−7/3| == 7/3", ratAbs(makeRat(-7n, 3n)).num === 7n);

  // floor / ceil / round on rationals
  ok("floor(7/3) = 2", ratFloor(makeRat(7n, 3n)).num === 2n);
  ok("floor(-7/3) = -3", ratFloor(makeRat(-7n, 3n)).num === -3n);
  ok("ceil(7/3) = 3", ratCeil(makeRat(7n, 3n)).num === 3n);
  ok("ceil(-7/3) = -2", ratCeil(makeRat(-7n, 3n)).num === -2n);
  // banker's: 2.5 → 2, 3.5 → 4 (ties to even)
  ok("round(5/2) = 2 (even)", ratRound(makeRat(5n, 2n)).num === 2n);
  ok("round(7/2) = 4 (even)", ratRound(makeRat(7n, 2n)).num === 4n);
  ok("round(3/2) = 2 (even)", ratRound(makeRat(3n, 2n)).num === 2n);
  ok("round(-3/2) = -2", ratRound(makeRat(-3n, 2n)).num === -2n);
  // truncate is toward zero
  ok("trunc(7/3) = 2", ratTrunc(makeRat(7n, 3n)).num === 2n);
  ok("trunc(-7/3) = -2", ratTrunc(makeRat(-7n, 3n)).num === -2n);

  // ratMod — Haskell `mod`, integer-only. Validated against l4 run earlier.
  eq("10 mod 3 = 1", ratMod(fromInt(10), fromInt(3)).num, 1n);
  eq("(-7) mod 3 = 2", ratMod(fromInt(-7), fromInt(3)).num, 2n);
  eq("(-7) mod (-3) = -1", ratMod(fromInt(-7), fromInt(-3)).num, -1n);
  eq("7 mod (-3) = -2", ratMod(fromInt(7), fromInt(-3)).num, -2n);
  // non-integer error matches jl4-core message
  try {
    ratMod(makeRat(11n, 2n), fromInt(3));
    ok("mod: 5.5 mod 3 throws", false, "no throw");
  } catch (e) {
    ok(
      "mod: 5.5 mod 3 message matches jl4-core",
      String(e.message).includes(
        "Expected an Integer but got the fractional number",
      ) && String(e.message).includes("MODULO"),
      e.message,
    );
  }
  // divide-by-zero matches jl4-core
  try {
    ratMod(fromInt(7), fromInt(0));
    ok("mod: 7 mod 0 throws", false, "no throw");
  } catch (e) {
    ok(
      "mod: 7 mod 0 message matches jl4-core",
      String(e.message).includes("Division by zero") &&
        String(e.message).includes("MODULO"),
      e.message,
    );
  }

  // min / max
  ok(
    "min(2/3, 3/4) = 2/3",
    ratCmp(ratMin(makeRat(2n, 3n), makeRat(3n, 4n)), makeRat(2n, 3n)) === 0,
  );
  ok(
    "max(2/3, 3/4) = 3/4",
    ratCmp(ratMax(makeRat(2n, 3n), makeRat(3n, 4n)), makeRat(3n, 4n)) === 0,
  );

  // isInteger
  ok("isInteger(7/1)", isInteger(fromInt(7)));
  ok("!isInteger(7/2)", !isInteger(makeRat(7n, 2n)));
}

// ---- ABI imports for the new ops (slice 2b lowering will emit these) ----
{
  const rt = createRuntime();
  rt.attachMemory(new WebAssembly.Memory({ initial: 2 }));
  const { env } = rt.makeImports();
  const litF = (s) => rt.u64ToF64(rt.writeString(s));
  const parse = (s) => env.__l4_rat_parse(litF(s));

  // n/d literal through __l4_rat_parse
  eq(
    "ABI: parse 199/12000 -> 199/12000",
    env.__l4_rat_to_f64(parse("199/12000")),
    199 / 12000,
  );

  // MODULO via __l4_rat_mod
  eq(
    "ABI: 10 mod 3 -> 1",
    env.__l4_rat_to_f64(env.__l4_rat_mod(parse("10"), parse("3"))),
    1,
  );
  eq(
    "ABI: -7 mod 3 -> 2 (Euclidean)",
    env.__l4_rat_to_f64(env.__l4_rat_mod(parse("-7"), parse("3"))),
    2,
  );

  // FLOOR / CEILING / ROUND / ABS go through __l4_floor / etc. (rewritten in
  // slice 2b to consume/produce handles).
  eq(
    "ABI: floor(7/3) -> 2",
    env.__l4_rat_to_f64(env.__l4_floor(parse("7/3"))),
    2,
  );
  eq(
    "ABI: ceil(-7/3) -> -2",
    env.__l4_rat_to_f64(env.__l4_ceil(parse("-7/3"))),
    -2,
  );
  eq(
    "ABI: round(5/2) -> 2 (banker)",
    env.__l4_rat_to_f64(env.__l4_round(parse("5/2"))),
    2,
  );
  eq(
    "ABI: abs(-1.5) -> 1.5",
    env.__l4_rat_to_f64(env.__l4_abs(parse("-1.5"))),
    1.5,
  );

  // MIN/MAX
  eq(
    "ABI: min(0.1, 0.2) -> 0.1",
    env.__l4_rat_to_f64(env.__l4_min(parse("0.1"), parse("0.2"))),
    0.1,
  );
  eq(
    "ABI: max(0.1, 0.2) -> 0.2",
    env.__l4_rat_to_f64(env.__l4_max(parse("0.1"), parse("0.2"))),
    0.2,
  );

  // TRUNC(value, digits) — jl4-core semantics, validated against:
  //   1.2345 trunc 2 == 1.23 ; 1.2345 trunc 0 == 1 ; -1.55 trunc 1 == -1.5
  eq(
    "ABI: 1.2345 trunc 2 -> 1.23",
    env.__l4_rat_to_f64(env.__l4_trunc(parse("1.2345"), parse("2"))),
    1.23,
  );
  eq(
    "ABI: 1.2345 trunc 0 -> 1",
    env.__l4_rat_to_f64(env.__l4_trunc(parse("1.2345"), parse("0"))),
    1,
  );
  eq(
    "ABI: -1.55 trunc 1 -> -1.5",
    env.__l4_rat_to_f64(env.__l4_trunc(parse("-1.55"), parse("1"))),
    -1.5,
  );

  // IS INTEGER
  eq("ABI: is_integer(7) -> 1", env.__l4_is_integer(parse("7")), 1);
  eq("ABI: is_integer(7/2) -> 0", env.__l4_is_integer(parse("7/2")), 0);
}

// ---- formatAesonDouble: matches jl4-service's wire format byte-for-byte ----
// Aeson uses bytestring's `doubleDec` (formatDouble FStandard) — scientific
// notation when e_ryu < -3 or k > 7, otherwise fixed-point with an explicit
// ".0" for integer-valued Doubles. Differs from JS's Number.toString.
{
  // The M0 case we care about: total-interest-paid renders as scientific.
  eq(
    "aeson: 0.016583333333333332 → scientific",
    formatAesonDouble(0.016583333333333332),
    "1.6583333333333332e-2",
  );
  // Fixed-point branch.
  eq("aeson: 0.5 → 0.5", formatAesonDouble(0.5), "0.5");
  eq("aeson: 1.5 → 1.5", formatAesonDouble(1.5), "1.5");
  eq("aeson: 0.1 → 0.1", formatAesonDouble(0.1), "0.1");
  // Haskell rule on k (floatToDigits exponent): scientific when k < 0 or
  // k > 7. 0.001 has k=-2 → scientific; 1.0165833333333334 has k=1 → fixed
  // (the M0 regression that nudged us off the `e_ryu < -3` rule).
  eq("aeson: 0.001 → 1.0e-3", formatAesonDouble(0.001), "1.0e-3");
  eq(
    "aeson: 1.0165833333333334 stays fixed (k=1)",
    formatAesonDouble(1.0165833333333334),
    "1.0165833333333334",
  );
}
eq("aeson: 0.0001 → scientific", formatAesonDouble(0.0001), "1.0e-4");
// Integer-valued Doubles take an explicit ".0".
eq("aeson: 5.0 → 5.0", formatAesonDouble(5.0), "5.0");
eq("aeson: 25000 → 25000.0", formatAesonDouble(25000.0), "25000.0");
eq("aeson: 1e7 → 1.0e7", formatAesonDouble(1e7), "1.0e7");
eq("aeson: 9999999.0 fixed", formatAesonDouble(9999999.0), "9999999.0");
// Negative / sign-only / zero.
eq("aeson: -1.5", formatAesonDouble(-1.5), "-1.5");
eq("aeson: -0.0", formatAesonDouble(-0.0), "-0.0");
eq("aeson: 0", formatAesonDouble(0), "0.0");
eq("aeson: NaN", formatAesonDouble(NaN), "NaN");
eq("aeson: Infinity", formatAesonDouble(Infinity), "Infinity");
eq("aeson: -Infinity", formatAesonDouble(-Infinity), "-Infinity");

// ---- aesonStringify: AesonDouble tag wins over JS Number formatting ----
{
  // Integer rationals serialise as bare digits; non-integer rationals come
  // out tagged from ratToAesonValue and render in scientific where needed.
  eq(
    "stringify: 25000 integer",
    aesonStringify(ratToAesonValue(fromInt(25000))),
    "25000",
  );
  eq(
    "stringify: 199/12000 → scientific",
    aesonStringify(ratToAesonValue(makeRat(199n, 12000n))),
    "1.6583333333333332e-2",
  );
  // The full envelope shape used by both the parity harness and wasm-server.
  const envelope = {
    contents: { result: { value: ratToAesonValue(makeRat(199n, 12000n)) } },
    tag: "SimpleResponse",
  };
  eq(
    "stringify: full SimpleResponse envelope byte-identical",
    aesonStringify(envelope),
    '{"contents":{"result":{"value":1.6583333333333332e-2}},"tag":"SimpleResponse"}',
  );
  // JSON.stringify would give the wrong bytes here — sanity check.
  ok(
    "JSON.stringify on the same envelope produces a *different* string",
    aesonStringify(envelope) !==
      JSON.stringify({
        contents: { result: { value: ratToDouble(makeRat(199n, 12000n)) } },
        tag: "SimpleResponse",
      }),
  );
}

// ---- M5 slice 1: envelope + synthetic Reasoning ----
{
  // SimpleResponse path: no reasoning, no tag flip — must produce the exact
  // bytes the M4 parity harness was already byte-identical on.
  const noTrace = aesonStringify(
    wrapEvaluationEnvelope({ value: ratToAesonValue(makeRat(199n, 12000n)) }),
  );
  eq(
    "envelope: SimpleResponse byte-identical to M4 baseline",
    noTrace,
    '{"contents":{"result":{"value":1.6583333333333332e-2}},"tag":"SimpleResponse"}',
  );
  // TraceResponse path: non-empty reasoning flips the tag and inserts the
  // reasoning key (alphabetically before "result", per Aeson's key order).
  const reasoning = {
    exampleCode: ["is-eligible"],
    explanation: ["Result: TRUE"],
    children: [],
  };
  const traced = aesonStringify(
    wrapEvaluationEnvelope({ value: true, reasoning }),
  );
  eq(
    "envelope: TraceResponse shape matches Aeson key order",
    traced,
    '{"contents":{"reasoning":{"children":[],"exampleCode":["is-eligible"],"explanation":["Result: TRUE"]},"result":{"value":true}},"tag":"TraceResponse"}',
  );
  // Empty reasoning collapses back to SimpleResponse — same rule as
  // `responseTag` in jl4-service's Backend/Api.hs.
  ok(
    "envelope: empty reasoning ⇒ SimpleResponse",
    wrapEvaluationEnvelope({
      value: 1,
      reasoning: { exampleCode: [], explanation: [], children: [] },
    }).tag === "SimpleResponse",
  );
  ok(
    "isEmptyReasoning matches",
    isEmptyReasoning({ exampleCode: [], explanation: [], children: [] }),
  );
}

// ---- synthesizeReasoning produces the slice-1 stub shape ----
{
  const meta = { apiName: "is-eligible", name: "is-eligible" };
  const r = synthesizeReasoning(meta, {}, true);
  eq("synth: exampleCode is function name", r.exampleCode[0], "is-eligible");
  eq("synth: explanation has Result line", r.explanation[0], "Result: TRUE");
  ok("synth: no children in slice 1", r.children.length === 0);
}

// ---- slice 2A: traceMeta from schema overrides the synthesized label ----
{
  // When the schema carries the compile-time `Print.prettyLayout` of the
  // function head, the runtime renders the backticked source spelling
  // (`is eligible`) instead of the sanitized API form (`is-eligible`).
  // That's the first content the trace harness's firstDiffByte burns
  // through, so this is the cheapest visible byte-id win on the way to
  // full instrumented traces.
  const meta = {
    apiName: "is-eligible",
    traceMeta: {
      fnName: "`is eligible`",
      body: "`years of service` AT LEAST 3 AND `performance rating` AT LEAST 4",
      params: ["years of service", "performance rating"],
    },
  };
  const r = synthesizeReasoning(meta, {}, true);
  eq(
    "synth: uses traceMeta.fnName when present",
    r.exampleCode[0],
    "`is eligible`",
  );
}

// ---- M5 slice 3: prettyL4Value covers Print.prettyLayout NF shapes ----
// Used by the trace harness's top-level `<fn> OF <args>` header to match
// `jl4-service` byte-for-byte on record-typed parameters. Coverage here
// is the same set the parity harness exercises against real wasm.
{
  const schemaBool = { type: "boolean" };
  const schemaNum = { type: "number" };
  const schemaStr = { type: "string" };
  const schemaDate = { type: "string", format: "date" };
  const schemaEnum = { type: "string", enum: ["Single", "Married"] };
  const schemaRecord = {
    type: "object",
    "x-l4-type": "CreditProfile",
    propertyOrder: [
      "credit score",
      "monthly income",
      "monthly debts",
      "employment months",
      "bankruptcy history",
    ],
    properties: {
      "credit score": schemaNum,
      "monthly income": schemaNum,
      "monthly debts": schemaNum,
      "employment months": schemaNum,
      "bankruptcy history": schemaBool,
    },
  };
  // Primitives — no parens (compound=false).
  eq("L4: boolean TRUE", prettyL4Value(true, schemaBool).text, "TRUE");
  eq("L4: boolean FALSE", prettyL4Value(false, schemaBool).text, "FALSE");
  eq("L4: integer", prettyL4Value(650, schemaNum).text, "650");
  eq(
    "L4: fractional number via Aeson",
    prettyL4Value(0.016583333333333332, schemaNum).text,
    "1.6583333333333332e-2",
  );
  eq("L4: string is double-quoted", prettyL4Value("x", schemaStr).text, '"x"');
  eq("L4: enum tag bare", prettyL4Value("Single", schemaEnum).text, "Single");
  eq(
    "L4: DATE pass-through",
    prettyL4Value("2025-01-01", schemaDate).text,
    "2025-01-01",
  );
  // MAYBE
  eq(
    "L4: MAYBE NOTHING (null)",
    prettyL4Value(null, schemaNum).text,
    "NOTHING",
  );
  eq(
    "L4: MAYBE NOTHING (literal)",
    prettyL4Value("NOTHING", schemaNum).text,
    "NOTHING",
  );
  eq(
    "L4: MAYBE JUST of integer",
    prettyL4Value({ JUST: [100000] }, schemaNum).text,
    "JUST OF 100000",
  );
  // Record — uses x-l4-type as the constructor name, propertyOrder for field order.
  {
    const credit = {
      "credit score": 650,
      "monthly income": 5000,
      "monthly debts": 500,
      "employment months": 24,
      "bankruptcy history": false,
    };
    const out = prettyL4Value(credit, schemaRecord);
    eq(
      "L4: record renders as <Ty> OF <fields>",
      out.text,
      "CreditProfile OF 650, 5000, 500, 24, FALSE",
    );
    ok("L4: record is compound (parens-when-arg)", out.compound === true);
    // prettyL4ValueWrapped wraps compounds in parens.
    eq(
      "L4: wrapped record gets parens",
      prettyL4ValueWrapped(credit, schemaRecord),
      "(CreditProfile OF 650, 5000, 500, 24, FALSE)",
    );
  }
  // Nested record — LoanRequest containing CreditProfile + JUST. This is
  // the `denial-reason` arg the parity harness exercises.
  {
    const loanSchema = {
      type: "object",
      "x-l4-type": "LoanRequest",
      propertyOrder: [
        "applicant",
        "loan amount",
        "term in months",
        "collateral value",
      ],
      properties: {
        applicant: schemaRecord,
        "loan amount": schemaNum,
        "term in months": schemaNum,
        "collateral value": { ...schemaNum }, // MAYBE NUMBER, but at this level untagged
      },
    };
    const req = {
      applicant: {
        "credit score": 650,
        "monthly income": 5000,
        "monthly debts": 500,
        "employment months": 24,
        "bankruptcy history": false,
      },
      "loan amount": 50000,
      "term in months": 60,
      "collateral value": { JUST: [100000] },
    };
    eq(
      "L4: nested record matches jl4-service prettyLayout",
      prettyL4Value(req, loanSchema).text,
      "LoanRequest OF (CreditProfile OF 650, 5000, 500, 24, FALSE), 50000, 60, (JUST OF 100000)",
    );
  }
  // prettyL4ValueWrapped: primitives don't wrap.
  eq(
    "L4: wrapped primitive no parens",
    prettyL4ValueWrapped(5, schemaNum),
    "5",
  );

  // ---- synthesizeArgEvalTree: arg-eval subtrees jl4-service emits before the body trace ----
  // Primitives: no subtree.
  ok("argEval: integer ⇒ null", synthesizeArgEvalTree(5, schemaNum) === null);
  ok("argEval: bool ⇒ null", synthesizeArgEvalTree(true, schemaBool) === null);
  ok(
    "argEval: NOTHING ⇒ null",
    synthesizeArgEvalTree(null, schemaNum) === null,
  );
  // Compound record: subtree with no children (fields all primitive).
  {
    const credit = {
      "credit score": 650,
      "monthly income": 5000,
      "monthly debts": 500,
      "employment months": 24,
      "bankruptcy history": false,
    };
    const tree = synthesizeArgEvalTree(credit, schemaRecord);
    ok("argEval: record yields tree", tree !== null);
    eq(
      "argEval: record exampleCode",
      tree.exampleCode[0],
      "CreditProfile OF 650, 5000, 500, 24, FALSE",
    );
    // Without a `forced` set in opts (slice 4D), all fields are treated
    // as forced → render as values, and compound sub-fields all produce
    // children. With a real wasm trace + __l4_mark_forced markers, the
    // set narrows to only the fields the body actually accessed.
    eq(
      "argEval: record explanation (no forced set ⇒ values)",
      tree.explanation[0],
      "Result: CreditProfile OF 650, 5000, 500, 24, FALSE",
    );
    eq(
      "argEval: record children empty (all primitive fields)",
      tree.children.length,
      0,
    );
  }
  // Nested record: outer's children include only non-MAYBE compounds.
  // MAYBE-typed fields are rendered in the parent's exampleCode (as
  // `JUST OF …`) but don't get their own arg-eval child — jl4-service
  // only pushes a child when the body actually forces the field, and
  // we can't tell that statically (slice 4 will refine).
  {
    const loanSchema = {
      type: "object",
      "x-l4-type": "LoanRequest",
      propertyOrder: [
        "applicant",
        "loan amount",
        "term in months",
        "collateral value",
      ],
      // `collateral value` is MAYBE NUMBER — absent from `required`.
      required: ["applicant", "loan amount", "term in months"],
      properties: {
        applicant: schemaRecord,
        "loan amount": schemaNum,
        "term in months": schemaNum,
        "collateral value": { ...schemaNum },
      },
    };
    const req = {
      applicant: {
        "credit score": 650,
        "monthly income": 5000,
        "monthly debts": 500,
        "employment months": 24,
        "bankruptcy history": false,
      },
      "loan amount": 50000,
      "term in months": 60,
      "collateral value": 100000, // present JSON value → renders as JUST OF 100000
    };
    const tree = synthesizeArgEvalTree(req, loanSchema);
    eq(
      "argEval: nested record exampleCode shows JUST wrapper for MAYBE field",
      tree.exampleCode[0],
      "LoanRequest OF (CreditProfile OF 650, 5000, 500, 24, FALSE), 50000, 60, (JUST OF 100000)",
    );
    // With no `forced` set, every compound sub-field gets a child:
    // the inner CreditProfile + the JUST OF 100000 wrapper.
    eq(
      "argEval: nested children count (default = treat all as forced)",
      tree.children.length,
      2,
    );
    eq(
      "argEval: first child is the inner record",
      tree.children[0].exampleCode[0],
      "CreditProfile OF 650, 5000, 500, 24, FALSE",
    );
    eq(
      "argEval: second child is the JUST wrapper",
      tree.children[1].exampleCode[0],
      "JUST OF 100000",
    );
  }
  // M5 slice 4E — value-object `__forced` Set drives lazy-NF.
  // The marshaler attaches `value.__forced` to compounds the wasm
  // body actually projected into; the renderer reads it directly,
  // so the path/forcedSet args are no longer needed.
  {
    const credit = {
      "credit score": 650,
      "monthly income": 5000,
      "monthly debts": 500,
      "employment months": 24,
      "bankruptcy history": false,
    };
    // Simulate the body forcing only `bankruptcy history`.
    credit.__forced = new Set(["bankruptcy history"]);
    const tree = synthesizeArgEvalTree(credit, schemaRecord);
    eq(
      "argEval: __forced Set drives Result line (only bankruptcy)",
      tree.explanation[0],
      "Result: CreditProfile OF (...), (...), (...), (...), FALSE",
    );
    delete credit.__forced;
  }

  // prettyL4Value handles MAYBE wrapping for the rendered text too.
  {
    const schemaParent = {
      type: "object",
      "x-l4-type": "Wrapper",
      propertyOrder: ["v"],
      required: [], // v is MAYBE
      properties: { v: schemaNum },
    };
    eq(
      "L4: MAYBE present in record renders as JUST OF inner",
      prettyL4Value({ v: 5 }, schemaParent).text,
      "Wrapper OF (JUST OF 5)",
    );
    eq(
      "L4: MAYBE absent in record renders as NOTHING",
      prettyL4Value({}, schemaParent).text,
      "Wrapper OF NOTHING",
    );
  }
}

// ---- prettyResultText covers slice-1 value kinds ----
eq("prettyResult: bool TRUE", prettyResultText(true), "TRUE");
eq("prettyResult: bool FALSE", prettyResultText(false), "FALSE");
eq("prettyResult: integer 25000", prettyResultText(25000), "25000");
// The trace's "Result: …" line uses jl4-core's @prettyRatio@, which
// formats fractions via @formatScientific Fixed@ — always decimal,
// never scientific. Distinct from the value-level Aeson encoding
// (Ryu-shortest) covered by 'aesonDouble' tests above.
eq(
  "prettyResult: fractional double via Fixed rules",
  prettyResultText(0.016583333333333332),
  "0.016583333333333332",
);
eq("prettyResult: NOTHING via null", prettyResultText(null), "NOTHING");
eq(
  "prettyResult: NOTHING via 'NOTHING' string",
  prettyResultText("NOTHING"),
  "NOTHING",
);
eq("prettyResult: JUST", prettyResultText({ JUST: [42] }), "JUST OF 42");

console.log(`\n${pass} passed, ${fail} failed`);
process.exit(fail ? 1 : 0);
