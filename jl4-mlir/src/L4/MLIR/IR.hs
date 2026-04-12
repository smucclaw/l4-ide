-- | Core MLIR intermediate representation types.
--
-- Models the essential structure of MLIR: modules contain functions,
-- functions contain regions, regions contain blocks, blocks contain
-- operations that produce and consume SSA values.
--
-- This is a self-contained Haskell representation of MLIR IR that can
-- be pretty-printed to MLIR textual format and consumed by mlir-opt.
module L4.MLIR.IR
  ( -- * Top-level
    MLIRModule (..)
    -- * SSA values
  , Value (..)
  , ValueId
    -- * Types
  , MLIRType (..)
    -- * Operations
  , Operation (..)
  , Attribute (..)
  , NamedAttribute (..)
    -- * Blocks and regions
  , Block (..)
  , BlockId
  , Region (..)
    -- * Helpers
  , freshValue
  , freshBlock
  , mkOp
    -- * Re-exported types
  , Text
  ) where

import Data.Text (Text)

-- | Unique identifier for an SSA value (%0, %1, ...)
type ValueId = Int

-- | Unique identifier for a block (^bb0, ^bb1, ...)
type BlockId = Int

-- | An SSA value reference.
data Value
  = SSAValue ValueId         -- ^ %N
  | BlockArg BlockId Int     -- ^ Block argument reference
  | ConstantRef Text         -- ^ Named constant (@name)
  deriving stock (Eq, Ord, Show)

-- | MLIR types used in the L4 compilation pipeline.
data MLIRType
  = IntegerType Int              -- ^ iN (i1, i32, i64)
  | IndexType                    -- ^ index
  | FloatType Int                -- ^ f32, f64
  | NoneType                     -- ^ none
  | FunctionType [MLIRType] [MLIRType]  -- ^ (inputs) -> (outputs)
  | MemRefType [Maybe Int] MLIRType     -- ^ memref<shape x element>
  | UnrankedMemRefType MLIRType         -- ^ memref<* x element>
  | TupleType [MLIRType]         -- ^ tuple<types...>
  | StructType Text [MLIRType]   -- ^ !llvm.struct<"name", (types...)>
  | PointerType                  -- ^ !llvm.ptr
  | ArrayType Int MLIRType       -- ^ !llvm.array<N x type>
  | NamedType Text               -- ^ !dialect.typename
  deriving stock (Eq, Ord, Show)

-- | MLIR attributes (constant data attached to operations).
data Attribute
  = IntegerAttr MLIRType Integer        -- ^ 42 : i64
  | FloatAttr MLIRType Double           -- ^ 3.14 : f64
  | StringAttr Text                     -- ^ "hello"
  | BoolAttr Bool                       -- ^ true/false
  | TypeAttr MLIRType                   -- ^ type attribute
  | ArrayAttr [Attribute]               -- ^ [attr1, attr2, ...]
  | DenseAttr MLIRType [Attribute]      -- ^ dense<[values]> : type
  | UnitAttr                            -- ^ unit
  | FlatSymbolRefAttr Text              -- ^ @symbol_name
  | SymbolNameAttr Text                 -- ^ sym_name = "name"
  | DictAttr [NamedAttribute]           -- ^ {key = value, ...}
  deriving stock (Eq, Ord, Show)

-- | A named attribute (key = value).
data NamedAttribute = NamedAttribute
  { attrName  :: Text
  , attrValue :: Attribute
  }
  deriving stock (Eq, Ord, Show)

-- | An MLIR operation (the fundamental unit of computation).
--
-- @
--   %result = "dialect.op"(%arg0, %arg1) {attrs} : (i64, i64) -> i64
-- @
data Operation = Operation
  { opResults    :: [ValueId]           -- ^ SSA values produced
  , opName       :: Text                -- ^ Qualified operation name (e.g. "arith.addf")
  , opOperands   :: [Value]             -- ^ Input SSA values
  , opAttributes :: [NamedAttribute]    -- ^ Named attributes
  , opResultTypes :: [MLIRType]         -- ^ Types of results
  , opOperandTypes :: [MLIRType]        -- ^ Types of operands (for signature)
  , opRegions    :: [Region]            -- ^ Nested regions (for func, scf.if, etc.)
  , opSuccessors :: [BlockId]           -- ^ Successor blocks (for terminators)
  }
  deriving stock (Eq, Ord, Show)

-- | A basic block: a label, arguments, and a sequence of operations.
data Block = Block
  { blockId   :: BlockId
  , blockArgs :: [(ValueId, MLIRType)]  -- ^ Block arguments with types
  , blockOps  :: [Operation]            -- ^ Operations in order
  }
  deriving stock (Eq, Ord, Show)

-- | A region is a list of basic blocks (the first is the entry block).
data Region = Region
  { regionBlocks :: [Block]
  }
  deriving stock (Eq, Ord, Show)

-- | Top-level MLIR module.
data MLIRModule = MLIRModule
  { moduleOps :: [Operation]  -- ^ Top-level operations (typically func.func definitions)
  }
  deriving stock (Eq, Ord, Show)

-- | Create a fresh SSA value id from a counter.
freshValue :: ValueId -> (Value, ValueId)
freshValue n = (SSAValue n, n + 1)

-- | Create a fresh block id from a counter.
freshBlock :: BlockId -> (BlockId, BlockId)
freshBlock n = (n, n + 1)

-- | Convenience: build an operation with no regions or successors.
mkOp :: [ValueId] -> Text -> [Value] -> [NamedAttribute] -> [MLIRType] -> [MLIRType] -> Operation
mkOp results name operands attrs resultTys operandTys = Operation
  { opResults = results
  , opName = name
  , opOperands = operands
  , opAttributes = attrs
  , opResultTypes = resultTys
  , opOperandTypes = operandTys
  , opRegions = []
  , opSuccessors = []
  }
