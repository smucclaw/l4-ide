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
  deriving stock (Show, Generic)
  deriving anyclass (NFData)
