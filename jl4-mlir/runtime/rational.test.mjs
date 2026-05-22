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
  ratFromDecimalString,
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

console.log(`\n${pass} passed, ${fail} failed`);
process.exit(fail ? 1 : 0);
