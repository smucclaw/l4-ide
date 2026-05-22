// Exact rational arithmetic for the L4 NUMBER type (M4).
//
// jl4-core evaluates NUMBER as an arbitrary-precision `Rational` and only
// rounds to a Double at the JSON boundary (non-integer results) — integers are
// emitted exactly. To match it byte-for-byte the WASM runtime must do the same:
// compute exactly in BigInt rationals, then render with correctly-rounded
// (round-half-to-even) rational→Double conversion, exactly like GHC's
// `fromRational` / CPython's `float(Fraction)`.
//
// This module is pure JS (no wasm/toolchain needed) so it can be unit-tested in
// isolation — see rational.test.mjs.

// ---- core: normalised {num, den} with den > 0, gcd(num,den) == 1 ----------

function bgcd(a, b) {
  if (a < 0n) a = -a;
  if (b < 0n) b = -b;
  while (b) {
    [a, b] = [b, a % b];
  }
  return a;
}

// Build a normalised rational from BigInt numerator/denominator.
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

// ---- correctly-rounded (round-half-to-even) rational -> IEEE 754 double ----
//
// Scale so the quotient mantissa has 53 significant bits, keep the EXACT
// remainder (which subsumes the guard + sticky bits), then round half-to-even
// using `2*rem vs den`. Provably correctly-rounded; validated against
// Number(n)/Number(d) (itself correctly-rounded RNE) for operands < 2^53.

const bitLen = (x) => (x === 0n ? 0 : x.toString(2).length);
const TWO52 = 1n << 52n;
const TWO53 = 1n << 53n;

export function ratToDouble(r) {
  let { num: n, den: d } = r;
  if (n === 0n) return 0;
  let sign = 1;
  if (n < 0n) {
    sign = -1;
    n = -n;
  }
  // value = n/d. Pick e so that mant = round(n/d / 2^e) lands in [2^52, 2^53).
  let e = bitLen(n) - bitLen(d) - 53;
  const scale = () => {
    if (e >= 0) return [n, d << BigInt(e)];
    return [n << BigInt(-e), d];
  };
  let [num, den] = scale();
  let mant = num / den;
  let rem = num % den;
  // The bit-length estimate can be off by one — fix up to [2^52, 2^53).
  if (mant >= TWO53) {
    e += 1;
    [num, den] = scale();
    mant = num / den;
    rem = num % den;
  } else if (mant < TWO52) {
    e -= 1;
    [num, den] = scale();
    mant = num / den;
    rem = num % den;
  }
  // round half-to-even using the exact remainder
  const twice = rem * 2n;
  if (twice > den || (twice === den && (mant & 1n) === 1n)) {
    mant += 1n;
    if (mant === TWO53) {
      mant = TWO52;
      e += 1;
    }
  }
  // mant < 2^53 is exact in a double; 2^e is an exact power of two (normal
  // range). Subnormal/overflow extremes fall back to IEEE behaviour of the
  // product — adequate for the legal/financial number range.
  return sign * Number(mant) * Math.pow(2, e);
}

// ---- the JSON value jl4-core would emit for this rational ------------------
// Integer (den == 1) -> the exact integer (number if it fits in a JS safe
// integer, otherwise its decimal string to avoid precision loss). Non-integer
// -> the correctly-rounded Double, which JS then serialises shortest-round-trip
// (matching Aeson's Ryu output).
const MAX_SAFE = BigInt(Number.MAX_SAFE_INTEGER);
const MIN_SAFE = BigInt(Number.MIN_SAFE_INTEGER);
export function ratToJSONValue(r) {
  if (r.den === 1n) {
    if (r.num <= MAX_SAFE && r.num >= MIN_SAFE) return Number(r.num);
    return r.num.toString(); // exact, but out of safe-integer range
  }
  return ratToDouble(r);
}

// ---- per-call handle pool (for the ABI: NUMBER = handle index) -------------
// Slice 2 boxes a handle as the f64 ABI value; the pool resets each call.
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
