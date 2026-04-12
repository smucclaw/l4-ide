-- | MLIR @arith@ dialect operations.
--
-- Provides builders for arithmetic and comparison operations on
-- integers and floats. L4 NUMBER maps to f64 for WASM performance.
-- See: https://mlir.llvm.org/docs/Dialects/ArithOps/
module L4.MLIR.Dialect.Arith
  ( -- * Constants
    arithConstant
  , arithConstantFloat
  , arithConstantInt
  , arithConstantBool
    -- * Float arithmetic (L4 NUMBER = f64)
  , arithAddf
  , arithSubf
  , arithMulf
  , arithDivf
  , arithRemf
  , arithNegf
    -- * Integer arithmetic
  , arithAddi
  , arithSubi
  , arithMuli
  , arithDivsi
  , arithRemsi
    -- * Float comparisons
  , arithCmpf
  , CmpfPredicate (..)
    -- * Integer comparisons
  , arithCmpi
  , CmpiPredicate (..)
    -- * Casts
  , arithSitofp
  , arithFptosi
  , arithExtui
  , arithTrunci
    -- * Boolean logic
  , arithAndi
  , arithOri
  , arithXori
    -- * Select
  , arithSelect
  ) where

import L4.MLIR.IR

-- | Float comparison predicates.
data CmpfPredicate
  = OEQ | OGT | OGE | OLT | OLE | ONE | ORD | UEQ | UGT | UGE | ULT | ULE | UNE | UNO
  deriving stock (Eq, Ord, Show)

-- | Integer comparison predicates.
data CmpiPredicate
  = EQ_ | NE | SLT | SLE | SGT | SGE | ULT_ | ULE_ | UGT_ | UGE_
  deriving stock (Eq, Ord, Show)

cmpfPredicateAttr :: CmpfPredicate -> Attribute
cmpfPredicateAttr p = IntegerAttr (IntegerType 64) $ case p of
  OEQ -> 1; OGT -> 2; OGE -> 3; OLT -> 4; OLE -> 5; ONE -> 6
  ORD -> 7; UEQ -> 8; UGT -> 9; UGE -> 10; ULT -> 11; ULE -> 12
  UNE -> 13; UNO -> 14

cmpiPredicateAttr :: CmpiPredicate -> Attribute
cmpiPredicateAttr p = IntegerAttr (IntegerType 64) $ case p of
  EQ_ -> 0; NE -> 1; SLT -> 2; SLE -> 3; SGT -> 4; SGE -> 5
  ULT_ -> 6; ULE_ -> 7; UGT_ -> 8; UGE_ -> 9

-- | @arith.constant@ — generic constant.
arithConstant :: ValueId -> Attribute -> MLIRType -> Operation
arithConstant result attr ty = mkOp [result] "arith.constant" []
  [NamedAttribute "value" attr] [ty] []

-- | @arith.constant@ — f64 literal (L4 NUMBER).
arithConstantFloat :: ValueId -> Double -> Operation
arithConstantFloat result val =
  arithConstant result (FloatAttr (FloatType 64) val) (FloatType 64)

-- | @arith.constant@ — i64 literal.
arithConstantInt :: ValueId -> Integer -> Operation
arithConstantInt result val =
  arithConstant result (IntegerAttr (IntegerType 64) val) (IntegerType 64)

-- | @arith.constant@ — i1 boolean literal.
arithConstantBool :: ValueId -> Bool -> Operation
arithConstantBool result val =
  arithConstant result (IntegerAttr (IntegerType 1) (if val then 1 else 0)) (IntegerType 1)

-- Binary float ops
binopF :: Text -> ValueId -> Value -> Value -> Operation
binopF name result lhs rhs =
  mkOp [result] name [lhs, rhs] [] [FloatType 64] [FloatType 64, FloatType 64]

arithAddf, arithSubf, arithMulf, arithDivf, arithRemf :: ValueId -> Value -> Value -> Operation
arithAddf = binopF "arith.addf"
arithSubf = binopF "arith.subf"
arithMulf = binopF "arith.mulf"
arithDivf = binopF "arith.divf"
arithRemf = binopF "arith.remf"

-- | @arith.negf@ — negate a float.
arithNegf :: ValueId -> Value -> Operation
arithNegf result val =
  mkOp [result] "arith.negf" [val] [] [FloatType 64] [FloatType 64]

-- Binary integer ops
binopI :: Text -> ValueId -> Value -> Value -> Operation
binopI name result lhs rhs =
  mkOp [result] name [lhs, rhs] [] [IntegerType 64] [IntegerType 64, IntegerType 64]

arithAddi, arithSubi, arithMuli, arithDivsi, arithRemsi :: ValueId -> Value -> Value -> Operation
arithAddi = binopI "arith.addi"
arithSubi = binopI "arith.subi"
arithMuli = binopI "arith.muli"
arithDivsi = binopI "arith.divsi"
arithRemsi = binopI "arith.remsi"

-- | @arith.cmpf@ — float comparison.
arithCmpf :: ValueId -> CmpfPredicate -> Value -> Value -> Operation
arithCmpf result pred_ lhs rhs = mkOp [result] "arith.cmpf" [lhs, rhs]
  [NamedAttribute "predicate" (cmpfPredicateAttr pred_)]
  [IntegerType 1] [FloatType 64, FloatType 64]

-- | @arith.cmpi@ — integer comparison.
arithCmpi :: ValueId -> CmpiPredicate -> Value -> Value -> Operation
arithCmpi result pred_ lhs rhs = mkOp [result] "arith.cmpi" [lhs, rhs]
  [NamedAttribute "predicate" (cmpiPredicateAttr pred_)]
  [IntegerType 1] [IntegerType 64, IntegerType 64]

-- | @arith.sitofp@ — signed integer to float.
arithSitofp :: ValueId -> Value -> MLIRType -> MLIRType -> Operation
arithSitofp result val fromTy toTy = mkOp [result] "arith.sitofp" [val] [] [toTy] [fromTy]

-- | @arith.fptosi@ — float to signed integer.
arithFptosi :: ValueId -> Value -> MLIRType -> MLIRType -> Operation
arithFptosi result val fromTy toTy = mkOp [result] "arith.fptosi" [val] [] [toTy] [fromTy]

-- | @arith.extui@ — unsigned integer extension.
arithExtui :: ValueId -> Value -> MLIRType -> MLIRType -> Operation
arithExtui result val fromTy toTy = mkOp [result] "arith.extui" [val] [] [toTy] [fromTy]

-- | @arith.trunci@ — integer truncation.
arithTrunci :: ValueId -> Value -> MLIRType -> MLIRType -> Operation
arithTrunci result val fromTy toTy = mkOp [result] "arith.trunci" [val] [] [toTy] [fromTy]

-- Boolean logic (on i1)
boolop :: Text -> ValueId -> Value -> Value -> Operation
boolop name result lhs rhs =
  mkOp [result] name [lhs, rhs] [] [IntegerType 1] [IntegerType 1, IntegerType 1]

arithAndi, arithOri, arithXori :: ValueId -> Value -> Value -> Operation
arithAndi = boolop "arith.andi"
arithOri  = boolop "arith.ori"
arithXori = boolop "arith.xori"

-- | @arith.select@ — conditional select (ternary).
arithSelect :: ValueId -> Value -> Value -> Value -> MLIRType -> Operation
arithSelect result cond trueVal falseVal ty =
  mkOp [result] "arith.select" [cond, trueVal, falseVal] [] [ty] [IntegerType 1, ty, ty]
