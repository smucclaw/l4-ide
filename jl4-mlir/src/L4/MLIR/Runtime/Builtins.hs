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
  [ -- Exact-rational NUMBER ABI (M4). Args/results are f64-boxed handles
    -- into the per-call rational pool; the JS adapter does box/unbox.
    -- @__l4_rat_parse@ takes a string-pool pointer (boxed as f64) and
    -- builds the rational. @__l4_rat_cmp@ returns -1.0\/0.0\/1.0 as f64.
    ("__l4_rat_parse",    [l4NumberType], [l4NumberType])
  , ("__l4_rat_from_int", [l4NumberType], [l4NumberType])
  , ("__l4_rat_add",      [l4NumberType, l4NumberType], [l4NumberType])
  , ("__l4_rat_sub",      [l4NumberType, l4NumberType], [l4NumberType])
  , ("__l4_rat_mul",      [l4NumberType, l4NumberType], [l4NumberType])
  , ("__l4_rat_div",      [l4NumberType, l4NumberType], [l4NumberType])
  , ("__l4_rat_mod",      [l4NumberType, l4NumberType], [l4NumberType])
  , ("__l4_rat_neg",      [l4NumberType], [l4NumberType])
  , ("__l4_rat_cmp",      [l4NumberType, l4NumberType], [l4NumberType])
  , ("__l4_rat_to_f64",   [l4NumberType], [l4NumberType])
  , ("__l4_f64_to_rat",   [l4NumberType], [l4NumberType])
  , ("__l4_pow",         [l4NumberType, l4NumberType], [l4NumberType])
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
    -- M5 slice 2B — trace ABI. Both calls are no-ops in the untraced
    -- `<fn>` variant; the instrumented `<fn>$trace` variant brackets each
    -- traceable subexpression with these. `__l4_trace_enter` records the
    -- compile-time-assigned node ID; `__l4_trace_exit` captures the result
    -- box plus a kind byte (0=NUMBER, 1=BOOLEAN, 2=STRING, 3=OTHER) the
    -- runtime serialiser uses to render `Result: …` lines.
  , ("__l4_trace_enter", [l4NumberType], [])
  , ("__l4_trace_exit",  [l4NumberType, l4NumberType], [])
    -- M5 slice 4A — each `<fn>$trace` brackets its body with
    -- `__l4_trace_enter_fn(fnSymbolPtr)` / `__l4_trace_exit_fn()` so
    -- the runtime knows whose `traceMeta.nodes` table to resolve
    -- node IDs against when the function recurses into another
    -- function's `$trace`. The arg is the f64-boxed pointer to a
    -- NUL-terminated wasm-symbol string in linear memory.
  , ("__l4_trace_enter_fn", [l4NumberType], [])
  , ("__l4_trace_exit_fn",  [],             [])
    -- M5 slice 4D — at each property selector (@record's field@) in
    -- `<fn>$trace` mode, the lowering emits one of these calls with
    -- the static path of the record (e.g. @\"req.applicant\"@) and
    -- the field's source name (e.g. @\"bankruptcy history\"@). The
    -- runtime accumulates a set of forced @path.field@ keys so the
    -- arg-eval renderer can show forced fields as values and
    -- unforced ones as @(...)@ — jl4-core's lazy-NF behaviour.
  , ("__l4_mark_forced", [l4NumberType, l4NumberType], [])
    -- M5 slice 4D — when a `<fn>$trace` body calls another L4
    -- function with a known static path (e.g. @`annual interest rate`
    -- (req's applicant)@), the caller pushes one binding per compound
    -- arg before the call:
    --   `__l4_trace_push_arg_path(helperParamName, callerPath)`
    -- then a single `__l4_trace_pop_arg_paths()` after the call.
    -- The runtime applies the top binding when rewriting marker paths
    -- (so the helper's `profile.bankruptcy history` becomes
    -- `req.applicant.bankruptcy history`).
  , ("__l4_trace_push_arg_path", [l4NumberType, l4NumberType], [])
  , ("__l4_trace_pop_arg_paths", [], [])
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
