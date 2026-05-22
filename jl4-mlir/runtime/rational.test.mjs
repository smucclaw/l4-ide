// Tests for the exact rational core (M4 slice 1). Pure JS, no toolchain.
//
//   node runtime/rational.test.mjs

import {
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
} from "./rational.mjs";

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

console.log(`\n${pass} passed, ${fail} failed`);
process.exit(fail ? 1 : 0);
