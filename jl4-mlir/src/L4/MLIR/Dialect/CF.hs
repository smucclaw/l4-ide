-- | MLIR @cf@ dialect operations (unstructured control flow).
--
-- Used when L4 pattern matching (CONSIDER/WHEN) requires multi-way
-- branching that doesn't fit @scf.if@ nesting.
-- See: https://mlir.llvm.org/docs/Dialects/ControlFlowDialect/
module L4.MLIR.Dialect.CF
  ( cfBr
  , cfCondBr
  , cfSwitch
  ) where

import L4.MLIR.IR

-- | @cf.br@ — unconditional branch to a block.
cfBr :: BlockId -> [Value] -> [MLIRType] -> Operation
cfBr dest args tys = Operation
  { opResults = []
  , opName = "cf.br"
  , opOperands = args
  , opAttributes = []
  , opResultTypes = []
  , opOperandTypes = tys
  , opRegions = []
  , opSuccessors = [dest]
  }

-- | @cf.cond_br@ — conditional branch.
cfCondBr :: Value -> BlockId -> [Value] -> BlockId -> [Value] -> [MLIRType] -> [MLIRType] -> Operation
cfCondBr cond trueDest trueArgs falseDest falseArgs trueTys falseTys = Operation
  { opResults = []
  , opName = "cf.cond_br"
  , opOperands = cond : trueArgs ++ falseArgs
  , opAttributes = []
  , opResultTypes = []
  , opOperandTypes = IntegerType 1 : trueTys ++ falseTys
  , opRegions = []
  , opSuccessors = [trueDest, falseDest]
  }

-- | @cf.switch@ — multi-way branch (for enum pattern matching).
--
-- Switches on an integer tag value, branching to the default or one of
-- the case blocks.
cfSwitch :: Value -> MLIRType -> BlockId -> [Value] -> [(Integer, BlockId, [Value])] -> Operation
cfSwitch flag flagTy defaultDest defaultArgs cases = Operation
  { opResults = []
  , opName = "cf.switch"
  , opOperands = flag : defaultArgs ++ concatMap (\(_, _, as) -> as) cases
  , opAttributes =
      [ NamedAttribute "case_values" (DenseAttr flagTy (map (\(v, _, _) -> IntegerAttr flagTy v) cases))
      ]
  , opResultTypes = []
  , opOperandTypes = [flagTy]
  , opRegions = []
  , opSuccessors = defaultDest : map (\(_, b, _) -> b) cases
  }
