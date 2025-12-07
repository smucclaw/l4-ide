module L4.Evaluate.ValueLazy where

import Base
import Control.Concurrent (ThreadId)
import Data.Time (Day)
import L4.Syntax
import L4.Evaluate.Operators (BinOp)

-- | Public addresses. These must be globally unique, therefore
-- we for now include the URI.
--
data Address = MkAddress !NormalizedUri !Int
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass NFData

-- | A reference pairs a public address with a pointer to a thunk.
--
data Reference =
  MkReference
    { address :: !Address
    , pointer :: !(IORef Thunk)
    }
  deriving stock Generic
  deriving anyclass NFData

instance Show Reference where
  show reference = show reference.address

data Thunk =
    Unevaluated !(Set ThreadId) (Expr Resolved) !Environment
  | WHNF WHNF
  deriving stock Show

type Environment = Map Unique Reference

type WHNF = Value Reference

data NF = MkNF (Value NF) | Omitted
  deriving stock (Generic, Show)
  deriving anyclass NFData

data Value a =
    ValNumber Rational
  | ValString Text
  | ValDate Day
  | ValNil
  | ValCons a a
  | ValClosure (GivenSig Resolved) (Expr Resolved) Environment
  | ValObligation Environment (Either RExpr (Value a)) (RAction Resolved) (Either (Maybe RExpr) (Value a)) RExpr (Maybe RExpr)
  | ValROp Environment RBinOp (Either RExpr (Value a)) (Either RExpr (Value a))
  | ValNullaryBuiltinFun NullaryBuiltinFun
  | ValUnaryBuiltinFun UnaryBuiltinFun
  | ValBinaryBuiltinFun BinOp
  | ValTernaryBuiltinFun TernaryBuiltinFun
  | ValPartialTernary TernaryBuiltinFun a             -- Ternary with 1 arg applied
  | ValPartialTernary2 TernaryBuiltinFun a a          -- Ternary with 2 args applied
  | ValUnappliedConstructor Resolved
  | ValConstructor Resolved [a]
  | ValAssumed Resolved
  | ValEnvironment Environment
  | ValBreached (ReasonForBreach a)
  deriving stock (Show, Functor, Foldable, Traversable)

data RBinOp = ValROr | ValRAnd
  deriving stock Show

instance NFData RBinOp where
  rnf ValROr = ()
  rnf ValRAnd = ()

data ReasonForBreach a = DeadlineMissed a a Rational a (RAction Resolved) Rational
  deriving stock (Generic, Show, Functor, Foldable, Traversable)
  deriving anyclass NFData

data NullaryBuiltinFun
  = NullaryTodaySerial
  | NullaryNowSerial
  deriving stock (Show)

data UnaryBuiltinFun
  = UnaryIsInteger
  | UnaryRound
  | UnaryCeiling
  | UnaryFloor
  | UnaryPercent
  | UnarySqrt            -- NUMBER → NUMBER (square root)
  -- String unary functions
  | UnaryStringLength    -- STRING → NUMBER
  | UnaryToUpper         -- STRING → STRING
  | UnaryToLower         -- STRING → STRING
  | UnaryTrim            -- STRING → STRING
  -- IO/JSON functions from main
  | UnaryFetch
  | UnaryEnv
  | UnaryJsonEncode
  | UnaryJsonDecode
  | UnaryDateValue
  | UnaryTimeValue
  | UnaryToString        -- α → STRING (runtime-restricted to supported types)
  | UnaryToNumber        -- STRING → MAYBE NUMBER
  | UnaryToDate          -- STRING → MAYBE DATE (uses runtime type info)
  deriving stock (Show)

data TernaryBuiltinFun
  = TernarySubstring     -- STRING → NUMBER → NUMBER → STRING
  | TernaryReplace       -- STRING → STRING → STRING → STRING
  | TernaryPost          -- from main
  deriving stock (Show)

-- | This is a non-standard instance because environments can be recursive, hence we must
-- not actually force the environments ...
instance NFData a => NFData (Value a) where
  rnf :: Value a -> ()
  rnf (ValNumber i)               = rnf i
  rnf (ValDate d)                 = rnf d
  rnf (ValROp env op a b)     = env `seq` op `deepseq` a `deepseq` b `deepseq` ()
  rnf (ValString t)               = rnf t
  rnf ValNil                      = ()
  rnf (ValCons r1 r2)             = rnf r1 `seq` rnf r2
  rnf (ValClosure given expr env) = env `seq` rnf given `seq` rnf expr
  rnf (ValNullaryBuiltinFun r)    = rnf r
  rnf (ValUnaryBuiltinFun r)      = rnf r
  rnf (ValBinaryBuiltinFun r)     = rnf r
  rnf (ValTernaryBuiltinFun r)    = rnf r
  rnf (ValPartialTernary r a)     = rnf r `seq` rnf a
  rnf (ValPartialTernary2 r a b)  = rnf r `seq` rnf a `seq` rnf b
  rnf (ValUnappliedConstructor r) = rnf r
  rnf (ValConstructor r vs)       = rnf r `seq` rnf vs
  rnf (ValAssumed r)              = rnf r
  rnf (ValEnvironment env)        = env `seq` ()
  rnf (ValBreached ev)            = rnf ev `seq` ()
  rnf (ValObligation env p a t f l) = env `seq` p `deepseq` a `deepseq` t `deepseq` f `deepseq` l `deepseq` ()

type MaybeEvaluated = MaybeEvaluated' RExpr

type MaybeEvaluated' a = Either a WHNF

type RExpr = Expr Resolved

instance NFData NullaryBuiltinFun where
  rnf :: NullaryBuiltinFun -> ()
  rnf NullaryTodaySerial = ()
  rnf NullaryNowSerial = ()

instance NFData UnaryBuiltinFun where
  rnf :: UnaryBuiltinFun -> ()
  rnf UnaryIsInteger = ()
  rnf UnaryRound = ()
  rnf UnaryCeiling = ()
  rnf UnaryFloor = ()
  rnf UnaryPercent = ()
  rnf UnarySqrt = ()
  rnf UnaryStringLength = ()
  rnf UnaryToUpper = ()
  rnf UnaryToLower = ()
  rnf UnaryTrim = ()
  rnf UnaryToString = ()
  rnf UnaryToNumber = ()
  rnf UnaryToDate = ()
  rnf UnaryFetch = ()
  rnf UnaryEnv = ()
  rnf UnaryJsonEncode = ()
  rnf UnaryJsonDecode = ()
  rnf UnaryDateValue = ()
  rnf UnaryTimeValue = ()

instance NFData TernaryBuiltinFun where
  rnf :: TernaryBuiltinFun -> ()
  rnf TernarySubstring = ()
  rnf TernaryReplace = ()
  rnf TernaryPost = ()
