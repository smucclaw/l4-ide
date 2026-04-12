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
