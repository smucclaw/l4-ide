-- | L4 type system → MLIR type mapping.
--
-- Defines how L4 types (NUMBER, BOOLEAN, STRING, records, enums,
-- MAYBE, LIST) are represented in MLIR for WASM compilation.
--
-- Design choices for WASM performance:
--   * NUMBER  → f64 (fast FPU ops, no arbitrary precision)
--   * BOOLEAN → i1
--   * STRING  → !llvm.ptr (pointer to null-terminated UTF-8)
--   * Record  → !llvm.struct<"RecordName", (field types...)>
--   * Enum    → i32 tag (variants without fields) or tagged union
--   * MAYBE T → !llvm.struct<"maybe", (i1, T)> where i1=has_value
--   * LIST T  → !llvm.ptr to linked list node struct
module L4.MLIR.Types
  ( l4TypeToMLIR
  , l4NumberType
  , l4BoolType
  , l4StringType
  , l4MaybeType
  , l4ListNodeType
  , l4UnitType
  , TypeEnv (..)
  , emptyTypeEnv
  , registerRecord
  , registerEnum
  , lookupRecordFields
  , lookupEnumVariants
  ) where

import L4.MLIR.IR

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import L4.Syntax (Type', Resolved)

-- | L4 NUMBER → f64 (64-bit IEEE 754 double).
-- L4 uses rationals but for WASM perf we compile to f64.
l4NumberType :: MLIRType
l4NumberType = FloatType 64

-- | L4 BOOLEAN → i1.
l4BoolType :: MLIRType
l4BoolType = IntegerType 1

-- | L4 STRING → pointer to UTF-8 data.
l4StringType :: MLIRType
l4StringType = PointerType

-- | L4 MAYBE T → struct { i1 has_value, T value }.
l4MaybeType :: MLIRType -> MLIRType
l4MaybeType inner = StructType "l4.maybe" [IntegerType 1, inner]

-- | L4 LIST T → pointer to linked list node.
-- Node layout: struct { T value, ptr next }
l4ListNodeType :: MLIRType -> MLIRType
l4ListNodeType elemTy = StructType "l4.list_node" [elemTy, PointerType]

-- | L4 Unit/Void → none type.
l4UnitType :: MLIRType
l4UnitType = NoneType

-- | Record field information: ordered list of (field_name, field_type).
type RecordInfo = [(Text, MLIRType)]

-- | Enum variant information: (variant_name, tag_value).
type EnumInfo = [(Text, Integer)]

-- | Type environment built during lowering.
-- Tracks record/enum declarations so we can look up field layouts.
data TypeEnv = TypeEnv
  { records :: Map Text RecordInfo
  , enums   :: Map Text EnumInfo
  }
  deriving stock (Show)

emptyTypeEnv :: TypeEnv
emptyTypeEnv = TypeEnv Map.empty Map.empty

registerRecord :: Text -> RecordInfo -> TypeEnv -> TypeEnv
registerRecord name fields env = env { records = Map.insert name fields env.records }

registerEnum :: Text -> EnumInfo -> TypeEnv -> TypeEnv
registerEnum name variants env = env { enums = Map.insert name variants env.enums }

lookupRecordFields :: Text -> TypeEnv -> Maybe RecordInfo
lookupRecordFields name env = Map.lookup name env.records

lookupEnumVariants :: Text -> TypeEnv -> Maybe EnumInfo
lookupEnumVariants name env = Map.lookup name env.enums

-- | Convert an L4 type to its MLIR representation.
--
-- == Uniform @f64@ representation
--
-- Every L4 type maps to 'FloatType 64' ('l4Value'). That's the single
-- representation used for every SSA value in the generated WASM. See
-- "L4.MLIR.ABI" for the encoding details and rationale.
--
-- Operations that conceptually require other MLIR types (@arith.cmpf@
-- produces @i1@, @llvm.extractvalue@ needs a struct, etc.) do their
-- work on native types internally but immediately box the result back
-- to f64. See 'L4.MLIR.Lower' for the boxing conventions.
--
-- This function is kept in case a caller genuinely needs the conceptual
-- L4 type (not the SSA representation) — for example, schema emission
-- in "L4.MLIR.Schema" still distinguishes NUMBER from BOOLEAN so clients
-- know how to marshal JSON. But for SSA-producing operations always use
-- 'l4Value' / 'l4NumberType' directly instead of calling this.
l4TypeToMLIR :: TypeEnv -> Type' Resolved -> MLIRType
l4TypeToMLIR _env _ty = l4NumberType  -- FloatType 64 — the sole SSA type

