module L4.Evaluate.Operators where
import Base
import L4.Print

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

instance LayoutPrinter BinOp where
  printWithLayout = \case
    BinOpPlus -> "PLUS"
    BinOpMinus -> "MINUS"
    BinOpTimes -> "TIMES"
    BinOpDividedBy -> "DIVIDED"
    BinOpModulo -> "MODULO"
    BinOpCons -> "FOLLOWED BY"
    BinOpEquals -> "EQUALS"
    BinOpLeq -> "AT MOST"
    BinOpGeq -> "AT LEAST"
    BinOpLt -> "LESS THAN"
    BinOpGt -> "GREATER THAN"
