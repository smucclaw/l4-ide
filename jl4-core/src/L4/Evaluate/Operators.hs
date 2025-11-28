module L4.Evaluate.Operators where
import Base

data BinOp =
    BinOpPlus
  | BinOpMinus
  | BinOpTimes
  | BinOpDividedBy
  | BinOpModulo
  | BinOpCons
  | BinOpEquals
  | BinOpLeq
  | BinOpGeq
  | BinOpLt
  | BinOpGt
  -- String binary operations
  | BinOpContains        -- STRING → STRING → BOOLEAN
  | BinOpStartsWith      -- STRING → STRING → BOOLEAN
  | BinOpEndsWith        -- STRING → STRING → BOOLEAN
  | BinOpIndexOf         -- STRING → STRING → NUMBER
  | BinOpSplit           -- STRING → STRING → LIST OF STRING
  | BinOpCharAt          -- STRING → NUMBER → STRING
  deriving stock (Show, Generic)
  deriving anyclass (NFData)
