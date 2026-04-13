-- | MLIR @func@ dialect operations.
--
-- Provides builders for function definitions, calls, and returns.
-- See: https://mlir.llvm.org/docs/Dialects/Func/
module L4.MLIR.Dialect.Func
  ( funcFunc
  , funcCall
  , funcReturn
  ) where

import L4.MLIR.IR

-- | Build a @func.func@ operation (function definition).
--
-- @
--   func.func @name(%arg0: i64, %arg1: f64) -> i64 {
--     ...
--   }
-- @
funcFunc :: Text -> [(ValueId, MLIRType)] -> [MLIRType] -> Region -> Operation
funcFunc name args resultTys body = Operation
  { opResults = []
  , opName = "func.func"
  , opOperands = []
  , opAttributes =
      [ NamedAttribute "sym_name" (StringAttr name)
      , NamedAttribute "function_type" (TypeAttr (FunctionType (map snd args) resultTys))
      ]
  , opResultTypes = []
  , opOperandTypes = []
  , opRegions = [body]
  , opSuccessors = []
  }

-- | Build a @func.call@ operation (function call).
--
-- @
--   %0 = func.call @callee(%arg0, %arg1) : (i64, i64) -> i64
-- @
funcCall :: [ValueId] -> Text -> [Value] -> [MLIRType] -> [MLIRType] -> Operation
funcCall results callee args argTys resultTys = Operation
  { opResults = results
  , opName = "func.call"
  , opOperands = args
  , opAttributes =
      [ NamedAttribute "callee" (FlatSymbolRefAttr callee)
      ]
  , opResultTypes = resultTys
  , opOperandTypes = argTys
  , opRegions = []
  , opSuccessors = []
  }

-- | Build a @func.return@ operation (function terminator).
--
-- @
--   func.return %0 : i64
-- @
funcReturn :: [Value] -> [MLIRType] -> Operation
funcReturn vals tys = mkOp [] "func.return" vals [] [] tys
