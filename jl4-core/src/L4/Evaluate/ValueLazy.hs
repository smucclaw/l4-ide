module L4.Evaluate.ValueLazy where

import Base
import Control.Concurrent (ThreadId)
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
  | ValNil
  | ValCons a a
  | ValClosure (GivenSig Resolved) (Expr Resolved) Environment
  | ValObligation Environment (Either RExpr (Value a)) (RAction Resolved) (Either (Maybe RExpr) (Value a)) RExpr (Maybe RExpr)
  | ValROp Environment RBinOp (Either RExpr (Value a)) (Either RExpr (Value a))
  | ValUnaryBuiltinFun UnaryBuiltinFun
  | ValBinaryBuiltinFun BinOp
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

data UnaryBuiltinFun
  = UnaryIsInteger
  | UnaryRound
  | UnaryCeiling
  | UnaryFloor
  | UnaryPercent
  | UnaryFetch
  | UnaryJsonEncode
  deriving stock (Show)

data TernaryBuiltinFun
  = TernaryPost
  deriving stock (Show)

-- | This is a non-standard instance because environments can be recursive, hence we must
-- not actually force the environments ...
instance NFData a => NFData (Value a) where
  rnf :: Value a -> ()
  rnf (ValNumber i)               = rnf i
  rnf (ValROp env op a b)     = env `seq` op `deepseq` a `deepseq` b `deepseq` ()
  rnf (ValString t)               = rnf t
  rnf ValNil                      = ()
  rnf (ValCons r1 r2)             = rnf r1 `seq` rnf r2
  rnf (ValClosure given expr env) = env `seq` rnf given `seq` rnf expr
  rnf (ValUnaryBuiltinFun r)      = rnf r
  rnf (ValBinaryBuiltinFun r)     = rnf r
  rnf (ValUnappliedConstructor r) = rnf r
  rnf (ValConstructor r vs)       = rnf r `seq` rnf vs
  rnf (ValAssumed r)              = rnf r
  rnf (ValEnvironment env)        = env `seq` ()
  rnf (ValBreached ev)            = rnf ev `seq` ()
  rnf (ValObligation env p a t f l) = env `seq` p `deepseq` a `deepseq` t `deepseq` f `deepseq` l `deepseq` ()

type MaybeEvaluated = MaybeEvaluated' RExpr

type MaybeEvaluated' a = Either a WHNF

type RExpr = Expr Resolved

instance NFData UnaryBuiltinFun where
  rnf :: UnaryBuiltinFun -> ()
  rnf UnaryIsInteger = ()
  rnf UnaryRound = ()
  rnf UnaryCeiling = ()
  rnf UnaryFloor = ()
  rnf UnaryPercent = ()
  rnf UnaryFetch = ()
  rnf UnaryJsonEncode = ()

instance NFData TernaryBuiltinFun where
  rnf :: TernaryBuiltinFun -> ()
  rnf TernaryPost = ()
