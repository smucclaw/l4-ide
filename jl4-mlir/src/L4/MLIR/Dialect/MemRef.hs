-- | MLIR @memref@ dialect operations.
--
-- Used for L4 record allocation, list node storage, and string buffers.
-- See: https://mlir.llvm.org/docs/Dialects/MemRef/
module L4.MLIR.Dialect.MemRef
  ( memrefAlloc
  , memrefAlloca
  , memrefLoad
  , memrefStore
  , memrefDealloc
  , memrefGetGlobal
  , memrefGlobal
  ) where

import L4.MLIR.IR

-- | @memref.alloc@ — heap allocation.
memrefAlloc :: ValueId -> MLIRType -> [Value] -> Operation
memrefAlloc result ty dynamicSizes = mkOp [result] "memref.alloc" dynamicSizes [] [ty] []

-- | @memref.alloca@ — stack allocation (preferred for small, short-lived values).
memrefAlloca :: ValueId -> MLIRType -> [Value] -> Operation
memrefAlloca result ty dynamicSizes = mkOp [result] "memref.alloca" dynamicSizes [] [ty] []

-- | @memref.load@ — load a value from a memref at given indices.
memrefLoad :: ValueId -> Value -> [Value] -> MLIRType -> MLIRType -> Operation
memrefLoad result memref indices elemTy memrefTy =
  mkOp [result] "memref.load" (memref : indices) [] [elemTy] (memrefTy : replicate (length indices) IndexType)

-- | @memref.store@ — store a value to a memref at given indices.
memrefStore :: Value -> Value -> [Value] -> MLIRType -> MLIRType -> Operation
memrefStore val memref indices elemTy memrefTy =
  mkOp [] "memref.store" (val : memref : indices) [] [] (elemTy : memrefTy : replicate (length indices) IndexType)

-- | @memref.dealloc@ — free heap-allocated memref.
memrefDealloc :: Value -> MLIRType -> Operation
memrefDealloc memref ty = mkOp [] "memref.dealloc" [memref] [] [] [ty]

-- | @memref.get_global@ — get reference to a global memref.
memrefGetGlobal :: ValueId -> Text -> MLIRType -> Operation
memrefGetGlobal result name ty = mkOp [result] "memref.get_global"  []
  [NamedAttribute "name" (FlatSymbolRefAttr name)] [ty] []

-- | @memref.global@ — declare a global memref (for string constants, etc.).
memrefGlobal :: Text -> MLIRType -> Attribute -> Operation
memrefGlobal name ty initial = Operation
  { opResults = []
  , opName = "memref.global"
  , opOperands = []
  , opAttributes =
      [ NamedAttribute "sym_name" (StringAttr name)
      , NamedAttribute "type" (TypeAttr ty)
      , NamedAttribute "initial_value" initial
      ]
  , opResultTypes = []
  , opOperandTypes = []
  , opRegions = []
  , opSuccessors = []
  }
