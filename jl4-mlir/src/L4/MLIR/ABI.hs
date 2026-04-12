{-# LANGUAGE OverloadedStrings #-}

-- | Uniform calling convention for compiled L4 functions.
--
-- == The problem it solves
--
-- An L4 module exposes functions of many different types — one might
-- take a 'NUMBER' and return a 'BOOLEAN', another a record and return a
-- list, another a 'MAYBE T' etc. A naive lowering uses the native MLIR
-- type that best matches each L4 type. That falls apart as soon as a
-- polymorphic helper (e.g. @id :: a -> a@ from the prelude) gets called
-- from multiple monomorphic sites, or an L4 function returns a value
-- that flows through another function with a slightly different
-- statically-inferred type: MLIR rejects the SSA type mismatch at every
-- boundary.
--
-- == The fix: a single boundary type ('l4Value' = 'f64')
--
-- Every value crossing a function boundary is an 'f64'. Internally a
-- function may freely use @i1@, @i32@, @!llvm.struct@ etc. for local
-- computation — but at the boundary (arg, return, call-site) everything
-- is boxed into an 'f64'. Call sites box their args before calling and
-- unbox the return afterwards; function bodies unbox their args on
-- entry and box their return value on exit.
--
-- == Encoding
--
-- The encoding is deliberately *not* NaN-boxing — it's simpler:
--
--   * __NUMBER__ — the 'f64' value directly (identity)
--   * __BOOLEAN__ — 0.0 for @FALSE@, 1.0 for @TRUE@
--   * __ENUM tag (i32)__ — bit-preserving cast through i64
--   * __Pointers__ (STRING, record, list, MAYBE) — the WASM 32-bit
--     pointer zero-extended to i64 then 'arith.bitcast' to 'f64'. The
--     reinterpretation is bit-preserving: encoded pointers look like
--     subnormal/denormal 'f64' values, but we never do arithmetic on
--     them — only box/unbox roundtrips.
--
-- This mirrors how JavaScript engines handle WASM ↔ JS interop when
-- reference types aren't available, and it gives us three properties
-- the native-type lowering lacks:
--
--   1. __Polymorphism Just Works.__ A function of type @a -> a@ takes
--      an 'f64' and returns the same 'f64'. Bit-preserving roundtrip
--      is guaranteed.
--   2. __No cross-module type fragility.__ A function that returns a
--      record defined in a dependency can be called from any other
--      module without knowing the record's layout.
--   3. __Uniform JS runtime.__ The Node-side caller has a single box
--      / unbox rule per L4 type, driven by the schema.
module L4.MLIR.ABI
  ( l4Value
  , isBoundaryType
  , BoxKind (..)
  , boxKindFor
  ) where

import L4.MLIR.IR

-- | The single type every L4 function boundary uses.
-- 'f64' was chosen because the vast majority of L4 values are numbers,
-- so most boundary crossings become no-ops.
l4Value :: MLIRType
l4Value = FloatType 64

-- | Is this already the ABI type? If so, no boxing is needed.
isBoundaryType :: MLIRType -> Bool
isBoundaryType (FloatType 64) = True
isBoundaryType _              = False

-- | How to box/unbox a given native MLIR type across the boundary.
-- The lowering dispatches on this tag to choose the right sequence
-- of bitcast / extui / ptrtoint / inttoptr / load-store ops.
data BoxKind
  = BoxNoop
    -- ^ Already an 'f64' — no coercion needed.
  | BoxFromBool
    -- ^ @i1 → f64@: @arith.uitofp@ (produces 0.0 or 1.0)
  | BoxFromIntTrunc Int
    -- ^ Integer of N bits → f64: extui to i64 then bitcast. N in {1,8,16,32}.
  | BoxFromInt64
    -- ^ Already i64: bitcast.
  | BoxFromPtr
    -- ^ Pointer → f64: ptrtoint to i64, bitcast to f64.
  | BoxFromStruct
    -- ^ Struct value → f64: take its address (alloca + store + ptrtoint + bitcast).
    --   Caller is expected to produce a pointer to the struct first.
  deriving (Eq, Show)

-- | Pick the right boxing strategy for a given native MLIR type.
boxKindFor :: MLIRType -> BoxKind
boxKindFor = \case
  FloatType 64     -> BoxNoop
  IntegerType 1    -> BoxFromBool
  IntegerType 64   -> BoxFromInt64
  IntegerType n    -> BoxFromIntTrunc n
  PointerType      -> BoxFromPtr
  StructType _ _   -> BoxFromStruct
  _                -> BoxNoop  -- best-effort: treat unknown types as already-f64
