-- | MLIR @scf@ dialect operations (structured control flow).
--
-- Used for L4's IF/THEN/ELSE and CONSIDER/WHEN pattern matching.
-- See: https://mlir.llvm.org/docs/Dialects/SCFDialect/
module L4.MLIR.Dialect.SCF
  ( scfIf
  , scfYield
  , scfWhile
  , scfCondition
  ) where

import L4.MLIR.IR

-- | Build an @scf.if@ operation (structured if/then/else).
--
-- @
--   %result = scf.if %cond -> f64 {
--     scf.yield %then_val : f64
--   } else {
--     scf.yield %else_val : f64
--   }
-- @
scfIf :: [ValueId] -> Value -> Region -> Region -> [MLIRType] -> Operation
scfIf results cond thenRegion elseRegion resultTys = Operation
  { opResults = results
  , opName = "scf.if"
  , opOperands = [cond]
  , opAttributes = []
  , opResultTypes = resultTys
  , opOperandTypes = [IntegerType 1]
  , opRegions = [thenRegion, elseRegion]
  , opSuccessors = []
  }

-- | Build an @scf.yield@ operation (region terminator that yields values).
scfYield :: [Value] -> [MLIRType] -> Operation
scfYield vals tys = mkOp [] "scf.yield" vals [] [] tys

-- | Build an @scf.while@ operation (structured while loop).
--
-- Used for recursive L4 list processing compiled to iterative form.
scfWhile :: [ValueId] -> [Value] -> [MLIRType] -> Region -> Region -> Operation
scfWhile results inits resultTys beforeRegion afterRegion = Operation
  { opResults = results
  , opName = "scf.while"
  , opOperands = inits
  , opAttributes = []
  , opResultTypes = resultTys
  , opOperandTypes = resultTys
  , opRegions = [beforeRegion, afterRegion]
  , opSuccessors = []
  }

-- | Build an @scf.condition@ (terminator for the "before" region of scf.while).
scfCondition :: Value -> [Value] -> [MLIRType] -> Operation
scfCondition cond args tys = mkOp [] "scf.condition" (cond : args) [] [] (IntegerType 1 : tys)
