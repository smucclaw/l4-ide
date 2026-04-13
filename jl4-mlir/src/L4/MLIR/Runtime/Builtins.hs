-- | Runtime builtin function declarations for compiled L4 WASM modules.
--
-- L4 programs may use operations that can't be expressed purely in
-- MLIR arithmetic (string concatenation, toString, pow, list operations,
-- min/max, etc.). These are compiled as calls to runtime functions that
-- must be provided by the WASM host environment or linked from a
-- runtime support library.
--
-- This module emits @func.func private@ declarations for each runtime
-- function so MLIR can type-check the calls. The actual implementations
-- are linked at the WASM level.
module L4.MLIR.Runtime.Builtins
  ( builtinDeclarations
  , runtimeFunctions
  ) where

import L4.MLIR.IR
import L4.MLIR.Types (l4NumberType)

-- | All runtime function declarations. These appear at the top of the
-- MLIR module as @func.func private @name(...)@ declarations.
builtinDeclarations :: [Operation]
builtinDeclarations = map mkExternDecl runtimeFunctions

-- | Registry of runtime functions under the uniform f64 ABI.
-- All args and results are 'l4NumberType' (f64) — the Node/Wasmtime
-- host unboxes pointers internally using the same bitcast convention
-- the compiler uses for boxing (see "L4.MLIR.ABI").
--
-- @__l4_alloc@ is special: it takes an i64 size and returns an
-- @!llvm.ptr@ because it's called by the compiler's @allocSlots@
-- helper *before* the pointer is boxed into the ABI.
runtimeFunctions :: [(Text, [MLIRType], [MLIRType])]
runtimeFunctions =
  [ ("__l4_pow",         [l4NumberType, l4NumberType], [l4NumberType])
  , ("__l4_min",         [l4NumberType, l4NumberType], [l4NumberType])
  , ("__l4_max",         [l4NumberType, l4NumberType], [l4NumberType])
  , ("__l4_abs",         [l4NumberType],               [l4NumberType])
  , ("__l4_floor",       [l4NumberType],               [l4NumberType])
  , ("__l4_ceil",        [l4NumberType],               [l4NumberType])
  , ("__l4_round",       [l4NumberType],               [l4NumberType])
  , ("__l4_str_concat",  [l4NumberType, l4NumberType], [l4NumberType])
  , ("__l4_str_eq",      [l4NumberType, l4NumberType], [l4NumberType])
  , ("__l4_str_len",     [l4NumberType],               [l4NumberType])
  , ("__l4_to_string",   [l4NumberType],               [l4NumberType])
  , ("__l4_list_count",  [l4NumberType],               [l4NumberType])
  , ("__l4_list_empty",  [],                           [l4NumberType])
  , ("__l4_alloc",       [IntegerType 64],             [PointerType])  -- special
  , ("__l4_free",        [l4NumberType],               [])
    -- DATE primitives. DATE is represented as f64 days-since-Unix-epoch
    -- (1970-01-01). Arithmetic on dates (PLUS/MINUS NUMBER) is plain
    -- arith.addf / arith.subf; only construction, parsing and field
    -- extraction need runtime help.
  , ("__l4_date_from_dmy",    [l4NumberType, l4NumberType, l4NumberType], [l4NumberType])
  , ("__l4_date_from_serial", [l4NumberType], [l4NumberType])
  , ("__l4_date_serial",      [l4NumberType], [l4NumberType])
  , ("__l4_date_day",         [l4NumberType], [l4NumberType])
  , ("__l4_date_month",       [l4NumberType], [l4NumberType])
  , ("__l4_date_year",        [l4NumberType], [l4NumberType])
  , ("__l4_datevalue",        [l4NumberType], [l4NumberType]) -- STRING -> Either STRING DATE (returned as MAYBE-like ptr)
  , ("__l4_to_date",          [l4NumberType], [l4NumberType]) -- STRING -> MAYBE DATE
    -- TIME primitives. TIME is represented as f64 seconds-since-midnight
    -- (non-negative; fractional for sub-second precision).
  , ("__l4_time_from_hms",    [l4NumberType, l4NumberType, l4NumberType], [l4NumberType])
  , ("__l4_time_from_serial", [l4NumberType], [l4NumberType])
  , ("__l4_time_serial",      [l4NumberType], [l4NumberType])
  , ("__l4_time_hour",        [l4NumberType], [l4NumberType])
  , ("__l4_time_minute",      [l4NumberType], [l4NumberType])
  , ("__l4_time_second",      [l4NumberType], [l4NumberType])
  , ("__l4_to_time",          [l4NumberType], [l4NumberType]) -- STRING -> MAYBE TIME
    -- DATETIME primitives. DATETIME is represented as a pointer to a
    -- 2-slot record: slot 0 = UTC Unix seconds (f64), slot 1 = IANA
    -- timezone name (STRING pointer).
  , ("__l4_datetime_from_dtz", [l4NumberType, l4NumberType, l4NumberType], [l4NumberType])
  , ("__l4_datetime_date",     [l4NumberType], [l4NumberType])
  , ("__l4_datetime_time",     [l4NumberType], [l4NumberType])
  , ("__l4_datetime_tz",       [l4NumberType], [l4NumberType])
  , ("__l4_datetime_serial",   [l4NumberType], [l4NumberType])
  , ("__l4_to_datetime",       [l4NumberType], [l4NumberType]) -- STRING -> MAYBE DATETIME
    -- Extended numeric intrinsics.
  , ("__l4_sqrt",     [l4NumberType], [l4NumberType])
  , ("__l4_ln",       [l4NumberType], [l4NumberType])
  , ("__l4_log10",    [l4NumberType], [l4NumberType])
  , ("__l4_sin",      [l4NumberType], [l4NumberType])
  , ("__l4_cos",      [l4NumberType], [l4NumberType])
  , ("__l4_tan",      [l4NumberType], [l4NumberType])
  , ("__l4_asin",     [l4NumberType], [l4NumberType])
  , ("__l4_acos",     [l4NumberType], [l4NumberType])
  , ("__l4_atan",     [l4NumberType], [l4NumberType])
  , ("__l4_trunc",    [l4NumberType, l4NumberType], [l4NumberType])  -- digits
  , ("__l4_is_integer", [l4NumberType], [l4NumberType])
    -- String unary.
  , ("__l4_string_length", [l4NumberType], [l4NumberType])
  , ("__l4_to_upper",      [l4NumberType], [l4NumberType])
  , ("__l4_to_lower",      [l4NumberType], [l4NumberType])
  , ("__l4_trim",          [l4NumberType], [l4NumberType])
    -- String binary / search.
  , ("__l4_contains",     [l4NumberType, l4NumberType], [l4NumberType])
  , ("__l4_starts_with",  [l4NumberType, l4NumberType], [l4NumberType])
  , ("__l4_ends_with",    [l4NumberType, l4NumberType], [l4NumberType])
  , ("__l4_index_of",     [l4NumberType, l4NumberType], [l4NumberType])
  , ("__l4_char_at",      [l4NumberType, l4NumberType], [l4NumberType])
    -- String ternary.
  , ("__l4_substring", [l4NumberType, l4NumberType, l4NumberType], [l4NumberType])
  , ("__l4_replace",   [l4NumberType, l4NumberType, l4NumberType], [l4NumberType])
    -- Conversion.
  , ("__l4_to_number", [l4NumberType], [l4NumberType])
    -- Nullary temporal.
  , ("__l4_today_serial",  [], [l4NumberType])
  , ("__l4_now_serial",    [], [l4NumberType])
  , ("__l4_current_time",  [], [l4NumberType])
  , ("__l4_timezone_name", [], [l4NumberType])
    -- JSON.
  , ("__l4_json_encode", [l4NumberType], [l4NumberType])
  , ("__l4_json_decode", [l4NumberType], [l4NumberType])
  ]

-- | Build an extern function declaration (func.func private).
mkExternDecl :: (Text, [MLIRType], [MLIRType]) -> Operation
mkExternDecl (name, argTys, retTys) = Operation
  { opResults = []
  , opName = "func.func"
  , opOperands = []
  , opAttributes =
      [ NamedAttribute "sym_name" (StringAttr name)
      , NamedAttribute "function_type" (TypeAttr (FunctionType argTys retTys))
      , NamedAttribute "sym_visibility" (StringAttr "private")
      ]
  , opResultTypes = []
  , opOperandTypes = []
  , opRegions = []  -- No body = external declaration
  , opSuccessors = []
  }
