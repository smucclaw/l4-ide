-- | MLIR @llvm@ dialect operations.
--
-- Used in the final lowering stage before emitting LLVM IR for WASM.
-- Provides struct operations for L4 records and pointer ops for lists.
-- See: https://mlir.llvm.org/docs/Dialects/LLVM/
module L4.MLIR.Dialect.LLVM
  ( -- * Struct operations (L4 records)
    llvmInsertvalue
  , llvmExtractvalue
    -- * Pointer operations (L4 lists, MAYBE)
  , llvmAlloca
  , llvmLoad
  , llvmStore
  , llvmNull
  , llvmIcmp
    -- * GEP
  , llvmGetelementptr
    -- * Global strings
  , llvmGlobalString
    -- * Misc
  , llvmUndefOp
  , llvmCall
  ) where

import L4.MLIR.IR

-- | @llvm.insertvalue@ — insert a value into a struct field.
llvmInsertvalue :: ValueId -> Value -> Value -> [Int] -> MLIRType -> Operation
llvmInsertvalue result container val indices structTy = mkOp [result] "llvm.insertvalue"
  [container, val]
  [NamedAttribute "position" (ArrayAttr (map (IntegerAttr (IntegerType 64) . fromIntegral) indices))]
  [structTy] [structTy]

-- | @llvm.extractvalue@ — extract a value from a struct field.
llvmExtractvalue :: ValueId -> Value -> [Int] -> MLIRType -> MLIRType -> Operation
llvmExtractvalue result container indices resultTy structTy = mkOp [result] "llvm.extractvalue"
  [container]
  [NamedAttribute "position" (ArrayAttr (map (IntegerAttr (IntegerType 64) . fromIntegral) indices))]
  [resultTy] [structTy]

-- | @llvm.alloca@ — allocate stack space.
llvmAlloca :: ValueId -> Value -> MLIRType -> Operation
llvmAlloca result size elemTy = mkOp [result] "llvm.alloca" [size]
  [NamedAttribute "elem_type" (TypeAttr elemTy)]
  [PointerType] [IntegerType 64]

-- | @llvm.load@ — load through a pointer.
llvmLoad :: ValueId -> Value -> MLIRType -> Operation
llvmLoad result ptr ty = mkOp [result] "llvm.load" [ptr] [] [ty] [PointerType]

-- | @llvm.store@ — store through a pointer.
llvmStore :: Value -> Value -> MLIRType -> Operation
llvmStore val ptr valTy = mkOp [] "llvm.store" [val, ptr] [] [] [valTy, PointerType]

-- | @llvm.mlir.null@ — null pointer constant.
llvmNull :: ValueId -> Operation
llvmNull result = mkOp [result] "llvm.mlir.null" [] [] [PointerType] []

-- | llvm.icmp with two operands.
llvmIcmp :: ValueId -> Text -> Value -> Value -> MLIRType -> Operation
llvmIcmp result pred_ lhs rhs ty = mkOp [result] "llvm.icmp" [lhs, rhs]
  [NamedAttribute "predicate" (StringAttr pred_)]
  [IntegerType 1] [ty, ty]

-- | @llvm.getelementptr@ — compute pointer to struct field / array element.
llvmGetelementptr :: ValueId -> Value -> [Value] -> MLIRType -> Operation
llvmGetelementptr result base indices elemTy = mkOp [result] "llvm.getelementptr"
  (base : indices)
  [NamedAttribute "elem_type" (TypeAttr elemTy)]
  [PointerType] (PointerType : replicate (length indices) (IntegerType 64))

-- | @llvm.mlir.global@ — string constant in data section.
llvmGlobalString :: Text -> Text -> Operation
llvmGlobalString name val = Operation
  { opResults = []
  , opName = "llvm.mlir.global"
  , opOperands = []
  , opAttributes =
      [ NamedAttribute "sym_name" (StringAttr name)
      , NamedAttribute "constant" UnitAttr
      , NamedAttribute "value" (StringAttr val)
      , NamedAttribute "linkage" (IntegerAttr (IntegerType 64) 0) -- private
      ]
  , opResultTypes = []
  , opOperandTypes = []
  , opRegions = []
  , opSuccessors = []
  }

-- | @llvm.mlir.undef@ — undefined value (used to initialize structs).
llvmUndefOp :: ValueId -> MLIRType -> Operation
llvmUndefOp result ty = mkOp [result] "llvm.mlir.undef" [] [] [ty] []

-- | @llvm.call@ — call an external / runtime function.
llvmCall :: [ValueId] -> Text -> [Value] -> [MLIRType] -> [MLIRType] -> Operation
llvmCall results callee args argTys resultTys = Operation
  { opResults = results
  , opName = "llvm.call"
  , opOperands = args
  , opAttributes = [NamedAttribute "callee" (FlatSymbolRefAttr callee)]
  , opResultTypes = resultTys
  , opOperandTypes = argTys
  , opRegions = []
  , opSuccessors = []
  }
